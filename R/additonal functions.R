




gw_percentile_map <- function(data){

  ## map
  details <- data$details
  return(well_map(details))

}


gw_percentile_class_table <- function(data, gt = TRUE){

  t <- well_table_status(data$w_perc, perc_values, data$window)

  # Get footnotes needed
  missing_dates <- stringr::str_subset(names(t), "\\*") %>% stringr::str_remove("\\*")
  missing_data <- any(t == "" | is.na(t))

  t <- t %>%
    dplyr::rename_with(~stringr::str_remove(., "\\*"))

  if (gt) {
    t <- t %>%
      gt::gt(rowname_col = "class") %>%
      gt_perc_colours(perc_col = "stub") %>%
      gt::cols_align("center") %>%
      gt::tab_spanner(label = "Current Year", columns = 1 + !!data$range1) %>%
      gt::tab_spanner(label = "Last Year", columns = 1 + !!data$range2) %>%
      #footnotes_below_normal(missing_dates, missing_data, n_days = n_days) %>%
      gt_bcgwl_style()
  }
  return(t)
}

gw_wells_below_normal_table <- function(data,
                                        which = c("totals", "hydraulic_connectivity", "type")[1],
                                        gt = TRUE){

  t <- well_table_below_norm(data$w_perc, data$window, which = which)

  # Get footnotes needed
  missing_dates <- stringr::str_subset(names(t), "\\*") %>% stringr::str_remove("\\*")
  missing_data <- any(t == "" | is.na(t))

  t <- t %>%
    dplyr::rename_with(~stringr::str_remove(., "\\*"))

  if (gt) {
    t <- t %>%
      gt::gt() %>%
      gt::tab_spanner(label = "Current Year", columns = ifelse(which == "totals", !!data$range1, 1 + !!data$range1)) %>%
      gt::tab_spanner(label = "Last Year", columns = ifelse(which == "totals", !!data$range2, 1 + !!data$range2)) %>%
      footnotes_below_normal(missing_dates, missing_data, n_days = data$n_days) %>%
      gt_bcgwl_style()
  }
  return(t)
}

gw_percentiles_details_table <- function(data,
                                         gt = TRUE){

  foot1 <- "n is the number of years included in the percentile calculation."
  foot2 <- "Values with an Approval Status of ‘Working’"

  details <- data$details

  max_dates <- data$w_full %>%
    dplyr::group_by(ow) %>%
    dplyr::summarise(date2 = max(Date))
  details <- dplyr::left_join(details, max_dates, by = "ow")
  details <- dplyr::mutate(details, date = date2)
  details <- dplyr::select(details, -date2)

  if (!gt) {
    details <- details %>%
      dplyr::select(Well = ow, Region = region, Area = area, Location = location,
                    Hydraulic_Connection = hydraulic_connectivity, Aquifer_Type = aquifer_type,
                    Latest_Date = date,Approval = approval, Percentile = percentile,
                    Class = class, n_Years = n_years)
  } else {
    details <- details %>%
      dplyr::mutate(percentile = dplyr::if_else(
        !is.na(percentile),
        glue::glue("{class} ({percentile}%)\n(n = {n_years})"),
        glue::glue("Not Available\n(n = {n_years})"))) %>%
      dplyr::select(area, location, ow, hydraulic_connectivity,
                    aquifer_type, date, percentile, class, approval) %>%
      dplyr::left_join(data$remarks, by = "ow") %>%
      dplyr::group_by(.data$area) %>%
      gt::gt() %>%
      gt::cols_hide(c("approval", "class")) %>%
      gt::cols_label("location" = "Location",
                     "hydraulic_connectivity" = "Hydraulic Connection",
                     "aquifer_type" = "Aquifer Type",
                     "date" = "Latest Date" ,
                     "percentile" = "Percentile",
                     "ow" = "Obs. Well",
                     "remarks" = "Remarks") %>%
      gt::cols_align("center", columns = "ow") %>%
      gt_perc_colours() %>%
      gt::fmt_missing(everything(), missing_text = "") %>%
      gt::tab_footnote(foot1, locations = gt::cells_column_labels("percentile")) %>%
      gt::tab_footnote(foot2, locations = gt::cells_body(columns = "date",
                                                         rows = approval == "Working")) %>%
      gt::opt_footnote_marks(marks = c("a", "**")) %>%
      gt::cols_align("left", columns = "remarks") %>%
      gt_bcgwl_style()
  }
  return(details)
}

gw_percentiles_plot <- function(data, ows = NA){

 # if (nrow(data$w_dates) > 0) {

    latest_date <- data$w_dates %>%
      dplyr::group_by(ow) %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::filter(Date == max(Date))

    if (all(!is.na(ows))) {

      if (any(!ows %in% unique(latest_date$ow)))
        stop(paste0("ows wells not in original list: ", list(ows[!ows %in% unique(latest_date$ow)])), call. = FALSE)

      latest_date <- latest_date %>%
        dplyr::filter(ow %in% ows)
    }
  #}

  p <- list()
  for (ow in unique(data$w_full$ow)) {
    full <- dplyr::filter(data$w_full, ow == !!ow)
    full_all <- dplyr::filter(data$w_full_all, ow == !!ow)
    hist <- dplyr::filter(data$w_hist, ow == !!ow)
    date <- dplyr::filter(latest_date, ow == !!ow)

    min_daily_date <- dplyr::case_when(min(full_all$WaterYear) != min(full$WaterYear) ~ min(full$Date, na.rm = TRUE),
                                       TRUE ~ NA_real_)
    p1 <- well_plot_perc(full, hist, date, years_min, water_year = data$wy, info = paste0(ow, " "))

    pnames <- names(p)
    p <- append(p,
                list(ow = p1))
    names(p) <- c(pnames, paste0(ow))
  }
  return(p)
}

gw_historic_data_plot <- function(data, ows = NA){

  latest_date <- data$w_dates %>%
    dplyr::group_by(ow) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Date == max(Date))

  if (all(!is.na(ows))) {

    if (any(!ows %in% unique(latest_date$ow)))
      stop(paste0("ows wells not in original list: ", list(ows[!ows %in% unique(latest_date$ow)])), call. = FALSE)

    latest_date <- latest_date %>%
      dplyr::filter(ow %in% ows)
  }

  p <- list()
  for (ow in unique(latest_date$ow)) {
    full <- dplyr::filter(data$w_full, ow == !!ow)
    full_all <- dplyr::filter(data$w_full_all, ow == !!ow)
    hist <- dplyr::filter(data$w_hist, ow == !!ow)
    date <- dplyr::filter(latest_date, ow == !!ow)

    min_daily_date <- dplyr::case_when(min(full_all$WaterYear) != min(full$WaterYear) ~ min(full$Date, na.rm = TRUE),
                                       TRUE ~ NA_real_)
    p1 <- well_plot_hist(full_all, hist, vline_date = min_daily_date, info = paste0(ow, " "))

    pnames <- names(p)
    p <- append(p,
                list(ow = p1))
    names(p) <- c(pnames, paste0(ow))
  }
  return(p)
}

gw_both_plots <- function(data, ows = NA){

  latest_date <- data$w_dates %>%
    dplyr::group_by(ow) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Date == max(Date))

  if (all(!is.na(ows))) {

    if (any(!ows %in% unique(latest_date$ow)))
      stop(paste0("ows wells not in original list: ", list(ows[!ows %in% unique(latest_date$ow)])), call. = FALSE)

    latest_date <- latest_date %>%
      dplyr::filter(ow %in% ows)
  }

  p <- list()
  for (ow in unique(latest_date$ow)) {
    full <- dplyr::filter(data$w_full, ow == !!ow)
    full_all <- dplyr::filter(data$w_full_all, ow == !!ow)
    hist <- dplyr::filter(data$w_hist, ow == !!ow)
    date <- dplyr::filter(latest_date, ow == !!ow)

    min_daily_date <- dplyr::case_when(min(full_all$WaterYear) != min(full$WaterYear) ~ min(full$Date, na.rm = TRUE),
                                       TRUE ~ NA_real_)
    p1 <- well_plot_perc(full, hist, date, years_min, water_year = data$wy, info = paste0(ow, " "))
    p2 <- well_plot_hist(full_all, hist, vline_date = min_daily_date, info = paste0(ow, " "))

    p_both <- p1 / p2


    pnames <- names(p)
    p <- append(p,
                list(ow = p_both))
    names(p) <- c(pnames, paste0(ow))
  }
  return(p)

}

gw_precip_plots <- function(){

}

get_obs_in_area <- function(nr_area = c("North Natural Resource Area",
                                        "South Coast Region",
                                        "South Natural Resource Area",
                                        "West Coast Region"),
                            rm_well = NA){
  read.csv("data-raw/obswell_locations.csv") %>%
    dplyr::filter(Area %in% nr_area,
                  !Well %in% rm_well) %>%
    dplyr::pull(Well)
}


# gw_percentile_map <- function(ows,
#                               #name = "report",
#                               report_dates = c(Sys.Date() - lubridate::weeks(2),
#                                                Sys.Date() - lubridate::weeks(4)),
#                               # title = NULL,
#                               # description = NULL,
#                               # remarks = NULL,
#                               n_days = 13,
#                               years_min = 5,
#                               #out_dir = ".",
#                               cache_age = 7){
#
#   data <- data_prep(ows,
#                     name = "report",
#                     report_dates = report_dates,
#                     title = NULL,
#                     description = NULL,
#                     remarks = NULL,
#                     n_days = n_days,
#                     years_min = years_min,
#                     out_dir = ".",
#                     cache_age = cache_age)
#
#
#   ## map
#   details <- well_table_summary(data$w_dates, data$w_hist, data$perc_values, data$full_window)
#   return(well_map(details))
#
# }
