# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#' Create a percentiles map with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#'
#' @export

gw_percentile_map <- function(data){

  ## map
  details <- data$details
  return(well_map(details))

}

#' Create a percentiles class table with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param gt Make the table gt format (TRUE) or a regular data.frame (FALSE)
#'
#' @export
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

#' Create a wells below normal table with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param which Which group to filter by: "totals" for all, 'hydraulic_connectivity' or 'type' for aquifer type.
#' @param gt Make the table gt format (TRUE) or a regular data.frame (FALSE)
#'
#' @export
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

#' Create a wells percentiles details with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param gt Make the table gt format (TRUE) or a regular data.frame (FALSE)
#'
#' @export
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
      gt::sub_missing(dplyr::everything(), missing_text = "") %>%
      # gt::fmt_missing(dplyr::everything(), missing_text = "") %>%
      gt::tab_footnote(foot1, locations = gt::cells_column_labels("percentile")) %>%
      gt::tab_footnote(foot2, locations = gt::cells_body(columns = "date",
                                                         rows = approval == "Working")) %>%
      gt::opt_footnote_marks(marks = c("a", "**")) %>%
      gt::cols_align("left", columns = "remarks") %>%
      gt_bcgwl_style()
  }
  return(details)
}

#' Create an annual hydrograph with percentiles plot with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param ows List a specific well to plot from original listed wells (exports just this well)
#'
#' @export
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

#' Create historical data plot with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param ows List a specific well to plot from original listed wells (exports just this well)
#'
#' @export
gw_historic_data_plot <- function(data, ows = NA){

  well_list <- data$ows
  # latest_date <- data$w_dates %>%
  #   dplyr::group_by(ow) %>%
  #   dplyr::filter(!is.na(Value)) %>%
  #   dplyr::filter(Date == max(Date))

  if (all(!is.na(ows))) {

    # if (any(!ows %in% unique(latest_date$ow)))
    if (any(!ows %in% well_list))
      stop(paste0("ows wells not in original list: ", list(ows[!ows %in% unique(latest_date$ow)])), call. = FALSE)

    # latest_date <- latest_date %>%
    #   dplyr::filter(ow %in% ows)
    well_list <- well_list[well_list %in% ows]

  }

  p <- list()
  #  for (ow in unique(latest_date$ow)) {
  for (ow in well_list) {
    full <- dplyr::filter(data$w_full, ow == !!ow)
    full_all <- dplyr::filter(data$w_full_all, ow == !!ow)
    hist <- dplyr::filter(data$w_hist, ow == !!ow)
    #date <- dplyr::filter(latest_date, ow == !!ow)

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

#' Create both percentiles and historical plot with selected observation wells
#' @param data Data object created from `gw_data_prep()` function.
#' @param ows List a specific well to plot from original listed wells (exports just this well)
#'
#' @export
gw_both_plots <- function(data, ows = NA, water_year_start = 10){

  well_list <- data$ows
  latest_date <- data$w_dates_latest %>%
    dplyr::group_by(ow) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Date == max(Date))

  if (all(!is.na(ows))) {

    # if (any(!ows %in% unique(latest_date$ow)))
    if (any(!ows %in% well_list))
      stop(paste0("ows wells not in original list: ", list(ows[!ows %in% unique(latest_date$ow)])), call. = FALSE)

    # latest_date <- latest_date %>%
    #   dplyr::filter(ow %in% ows)
    well_list <- well_list[well_list %in% ows]
  }

  p <- list()
  #  for (ow in unique(latest_date$ow)) {
  for (ow in well_list) {
    full <- dplyr::filter(data$w_full, ow == !!ow)
    full_all <- dplyr::filter(data$w_full_all, ow == !!ow)
    hist <- dplyr::filter(data$w_hist, ow == !!ow)
    date <- dplyr::filter(latest_date, ow == !!ow)

    min_daily_date <- dplyr::case_when(min(full_all$WaterYear) != min(full$WaterYear) ~ min(full$Date, na.rm = TRUE),
                                       TRUE ~ NA)
    p1 <- well_plot_perc(full, hist, date, years_min, water_year = data$wy, info = paste0(ow, " "), water_year_start = water_year_start)
    p2 <- well_plot_hist(full_all, hist, vline_date = min_daily_date, info = paste0(ow, " "))

    p_both <- p1 / p2


    pnames <- names(p)
    p <- append(p,
                list(ow = p_both))
    names(p) <- c(pnames, paste0(ow))
  }
  return(p)

}

#' Get a list of all wells from a natural resource region or area.
#' @param nr_area List one or multiple of c("North Natural Resource Area", "South Coast Region",
#'    "South Natural Resource Area", "West Coast Region")
#' @param rm_well Exclude specific wells.
#'
#' @export
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


#'
#'
#' #' Generate and save plots and a csv percentiles file from all wells in obswell_locations for drought
#' #' @param plots_dir Location to save all plots
#' #' @param table_dir Location to save csv table with percentiles
#' #'
#' #' @export
#' generate_well_percentiles <- function(#region,
#'   plots_dir = ".",
#'   table_dir = "."){
#'
#'   well_list_table <- read.csv("data-raw/obswell_locations.csv", na.strings = "")# %>%
#'   #  dplyr::filter(Region_Drought == "Thompson-Okanagan")
#'
#'   well_list <- well_list_table %>%
#'     dplyr::filter(!is.na(Region_Drought)) %>%
#'     dplyr::pull(Well)
#'
#'   gw_data <- gw_data_prep(ows = well_list,
#'                           n_days = 14,report_dates = Sys.Date(),years_min = 5,cache_age = 7)
#'
#'   map <- gw_percentile_map(gw_data)
#'   htmlwidgets::saveWidget(map, file = paste0(plots_dir, "province_map.html"))
#'
#'   summary <- gw_percentile_class_table(gw_data, gt = TRUE)
#'   gt::gtsave(summary, filename = paste0(plots_dir, "province_class_summary.png"))
#'
#'
#'   table <- gw_percentiles_details_table(gw_data, gt = FALSE) %>%
#'     dplyr::select(-Area, -Location)
#'   wells_sf <- data_load("wells_sf") %>%
#'     dplyr::rename(Well = ow) %>%
#'     dplyr::filter(Well %in% well_list)#%>%
#'   # dplyr::mutate(Latitude = sf::st_coordinates(.)[,1],
#'   #               Longitude = sf::st_coordinates(.)[,2])
#'
#'
#'   table_save <- wells_sf %>%
#'     dplyr::left_join(table, by = "Well") %>%
#'     dplyr::left_join(well_list_table, by = "Well") %>%
#'     dplyr::mutate(Latitude = NA,
#'                   Longitude = NA,
#'                   Hydrograph_url = NA,
#'                   Updated_Time = Sys.time()) %>%
#'     dplyr::select(Well, Location = Location_Long, Latitude, Longitude, Class, Percentile, n_Years, Latest_Date, Approval,
#'                   Aquifer = aquifer_id, Aquifer_Type, Hydraulic_Connection, Hydrograph_url, Updated_Time)
#'
#'
#'   write.csv(table_save, paste0(table_dir, "groundwater_percentiles.csv"), row.names = FALSE, na = "")
#'   # sf::st_write(table_save, "groundwater_percentiles.shp", delete_layer = TRUE)
#'
#'
#'   plots <- gw_both_plots(gw_data)
#'
#'
#'   for (well in names(plots)) {
#'     message(paste0("Saving plot for ", well, " (", match(well, names(plots)), "/", length(names(plots)),")"))
#'
#'     ggplot2::ggsave(plot = plots[[well]],
#'                     filename = paste0(plots_dir, well, ".png"),
#'                     height = 9, width = 9.5)
#'
#'   }
#'
#'   #return(plots)
#'
#' }
#'
#'
#' #' Generate and save regional plots and reports grouped by region in obswell_locations for drought
#' #' @param region Region to create reports/plot. One of c("Skeena", "Northeast", "Omineca", "South Coast",
#' #'  "Thompson-Okanagan", "Kootenay-Boundary", "Cariboo", "West Coast")
#' #' @param write_report Logical, choose to save reports
#' #' @param write_plots Logical, choose to save plots
#' #' @param report_dir Location to save regional reports
#' #' @param plots_dir Location to save regional plots and tables
#' #'
#' #' @export
#' generate_regional_reports <- function(region,
#'                                       write_report = TRUE,
#'                                       write_plots = TRUE,
#'                                       report_dir = ".",
#'                                       plots_dir = "."){
#'
#'
#'   well_list <- read.csv("data-raw/obswell_locations.csv", na.strings = "") %>%
#'     dplyr::filter(Region_Drought == region) %>%
#'     dplyr::pull(Well)
#'
#'   if (write_report){
#'
#'     well_report(ows = well_list,
#'                 report_dates = c(Sys.Date()),
#'                 title = paste0(region, " Natural Resource Region Groundwater Level Conditions"),
#'                 description = paste0("The following provides an overview of groundwater (GW) conditions in the ",
#'                                      region, " Natural Resource Region as of ",
#'                                      format(Sys.Date(), format = "%B %d, %Y"), "."),
#'                 n_days = 14,
#'                 years_min = 5,
#'                 out_dir = report_dir,
#'                 cache_age = 7,
#'                 name = paste0(region))
#'
#'   }
#'
#'   if (write_plots) {
#'
#'     gw_data <- gw_data_prep(ows = well_list,
#'                             n_days = 14,report_dates = Sys.Date(),years_min = 5,cache_age = 7)
#'
#'
#'     map <- gw_percentile_map(gw_data)
#'     htmlwidgets::saveWidget(map, file = paste0(plots_dir,  region, "_map.html"))
#'
#'     summary <- gw_percentile_class_table(gw_data, gt = TRUE)
#'     gt::gtsave(summary, filename = paste0(plots_dir,  region, "_class_summary.png"))
#'
#'     bnorm_tot <- gw_wells_below_normal_table(gw_data, which = "totals", gt = TRUE)
#'     gt::gtsave(bnorm_tot, filename = paste0(plots_dir,  region, "_belownorm_total.png"))
#'
#'     bnorm_hyd <- gw_wells_below_normal_table(gw_data, which = "hydraulic_connectivity", gt = TRUE)
#'     gt::gtsave(bnorm_hyd, filename = paste0(plots_dir,  region, "_belownorm_hydcond.png"))
#'
#'     bnorm_type <- gw_wells_below_normal_table(gw_data, which = "type", gt = TRUE)
#'     gt::gtsave(bnorm_type, filename = paste0(plots_dir,  region, "_belownorm_aqtype.png"))
#'
#'     details <- gw_percentiles_details_table(gw_data, gt = TRUE)
#'     gt::gtsave(details, filename = paste0(plots_dir,  region, "_welldetails.png"))
#'
#'   }
#'
#' }
#'
#'
#'
#'
#' #' Generate and save regional plots and reports grouped by region in obswell_locations for drought
#' #' @param region Region to create reports/plot. One of c("Skeena", "Northeast", "Omineca", "South Coast",
#' #'  "Thompson-Okanagan", "Kootenay-Boundary", "Cariboo", "West Coast")
#' #' @param write_report Logical, choose to save reports
#' #' @param write_plots Logical, choose to save plots
#' #' @param report_dir Location to save regional reports
#' #' @param plots_dir Location to save regional plots and tables
#' #'
#' #' @export
#' generate_drought_reports <- function(region = NA,
#'                                      write_report = TRUE,
#'                                      write_plots = TRUE,
#'                                      report_dir = "",
#'                                      reg_plots_dir = "",
#'                                      well_plots_dir = "",
#'                                      table_dir = ""){
#'
#'   # prepare region variable
#'   if (all(is.na(region))) {
#'     region <- read.csv("data-raw/obswell_locations.csv", na.strings = "") %>%
#'       dplyr::filter(!is.na(Region_Drought)) %>%
#'       dplyr::pull(Region_Drought) %>%
#'       unique()
#'   } else {
#'     # make sure listed regions are in obswell_locations.csv file, stop error/message
#'   }
#'
#'   # download and filter obswell_locations data and filter for regions
#'   well_list_table <- read.csv("data-raw/obswell_locations.csv", na.strings = "") %>%
#'     dplyr::filter(Region_Drought %in% region)
#'
#'
#'
#'   combined_table <- dplyr::tibble()
#'
#'   # loop through each region
#'   for (rgn in region) {
#'
#'     message(paste0("----  Starting region: " , rgn, " (", match(rgn, region), "/", length(region),")"))
#'
#'
#'     well_list <- well_list_table %>%
#'       dplyr::filter(Region_Drought == rgn) %>%
#'       dplyr::pull(Well)
#'
#'     rgn_label <- ifelse(rgn == "North", "Natural Resource Area", "Natural Resource Region")
#'
#'     if (write_report){
#'
#'       well_report(ows = well_list,
#'                   report_dates = c(Sys.Date()),
#'                   title = paste0(rgn, " ", rgn_label," Groundwater Level Conditions"),
#'                   description = paste0("The following provides an overview of groundwater (GW) conditions in the ",
#'                                        rgn, " ", rgn_label," as of ",
#'                                        format(Sys.Date(), format = "%B %d, %Y"), "."),
#'                   n_days = 14,
#'                   years_min = 5,
#'                   out_dir = report_dir,
#'                   cache_age = 7,
#'                   name = paste0(rgn))
#'
#'     }
#'
#'     if (write_plots) {
#'
#'       message(paste0("----  Starting to save maps and tables"))
#'       gw_data <- gw_data_prep(ows = well_list,
#'                               n_days = 14,report_dates = Sys.Date(),years_min = 5,cache_age = 7)
#'
#'
#'       map <- gw_percentile_map(gw_data)
#'       htmlwidgets::saveWidget(map, file = paste0(reg_plots_dir,  rgn, "_map.html"))
#'
#'       summary <- gw_percentile_class_table(gw_data, gt = TRUE)
#'       gt::gtsave(summary, filename = paste0(reg_plots_dir,  rgn, "_class_summary.png"))
#'
#'       bnorm_tot <- gw_wells_below_normal_table(gw_data, which = "totals", gt = TRUE)
#'       gt::gtsave(bnorm_tot, filename = paste0(reg_plots_dir,  rgn, "_belownorm_total.png"))
#'
#'       bnorm_hyd <- gw_wells_below_normal_table(gw_data, which = "hydraulic_connectivity", gt = TRUE)
#'       gt::gtsave(bnorm_hyd, filename = paste0(reg_plots_dir,  rgn, "_belownorm_hydcond.png"))
#'
#'       bnorm_type <- gw_wells_below_normal_table(gw_data, which = "type", gt = TRUE)
#'       gt::gtsave(bnorm_type, filename = paste0(reg_plots_dir,  rgn, "_belownorm_aqtype.png"))
#'
#'       details <- gw_percentiles_details_table(gw_data, gt = TRUE)
#'       gt::gtsave(details, filename = paste0(reg_plots_dir,  rgn, "_welldetails.png"))
#'
#'       details_table <- gw_percentiles_details_table(gw_data, gt = FALSE)
#'       combined_table <- dplyr::bind_rows(combined_table, details_table)
#'
#'       message(paste0("----  Starting to create plots...."))
#'       plots <- gw_both_plots(gw_data)
#'       message(paste0("----  ....starting to save plots"))
#'       for (well in names(plots)) {
#'         message(paste0("Saving plot for ", well, " (", match(well, names(plots)), "/", length(names(plots)),")"))
#'
#'         ggplot2::ggsave(plot = plots[[well]],
#'                         filename = paste0(well_plots_dir, well, ".png"),
#'                         height = 9, width = 9.5)
#'
#'       }
#'
#'     }
#'   }
#'
#'   write.csv(combined_table, paste0(table_dir, "groundwater_percentiles_new.csv"), row.names = FALSE, na = "")
#'
#' }
#'
#' # gw_percentile_map <- function(ows,
#' #                               #name = "report",
#' #                               report_dates = c(Sys.Date() - lubridate::weeks(2),
#' #                                                Sys.Date() - lubridate::weeks(4)),
#' #                               # title = NULL,
#' #                               # description = NULL,
#' #                               # remarks = NULL,
#' #                               n_days = 13,
#' #                               years_min = 5,
#' #                               #out_dir = ".",
#' #                               cache_age = 7){
#' #
#' #   data <- data_prep(ows,
#' #                     name = "report",
#' #                     report_dates = report_dates,
#' #                     title = NULL,
#' #                     description = NULL,
#' #                     remarks = NULL,
#' #                     n_days = n_days,
#' #                     years_min = years_min,
#' #                     out_dir = ".",
#' #                     cache_age = cache_age)
#' #
#' #
#' #   ## map
#' #   details <- well_table_summary(data$w_dates, data$w_hist, data$perc_values, data$full_window)
#' #   return(well_map(details))
#' #
#' # }
