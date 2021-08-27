# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

bcgwl_style <- function(kable_input, ...) {
  kable_classic(kable_input,
                lightable_options = "hover", full_width = FALSE,
                html_font = "\"Arial\", \"Source Sans Pro\", sans-serif",
                ...)
}

well_map <- function(details) {

 locs <- data_load("wells_sf") %>%
   dplyr::right_join(dplyr::select(details, "ow", "bg_colour",
                                   "Percentile Class"), by = "ow") %>%
   dplyr::mutate(`Percentile Class` = stringr::str_extract(
     `Percentile Class`, "^[[:alpha:] ]+ \\([[:digit:]]+\\%\\)"),
     `Percentile Class` = tidyr::replace_na(`Percentile Class`, "No current data")) %>%
   sf::st_transform(4326) %>%
   dplyr::left_join(region_names, by = "ow") %>%
   dplyr::mutate(tooltip = glue::glue(
     "<strong>Well</strong>: {.data$ow}<br>",
     "<strong>Region</strong>: {.data$region}<br>",
     "<strong>Location</strong>: {.data$location}<br>",
     "<strong>Current percentile</strong>: {.data$`Percentile Class`}"),
     tooltip = purrr::map(.data$tooltip, htmltools::HTML))

 regions <- data_load("regions") %>%
   sf::st_transform(4326)

 leaflet::leaflet(data = locs) %>%
   leaflet::addProviderTiles("Stamen.Terrain") %>%
   leaflet::addCircleMarkers(color = "black",
                             fillColor = ~as.vector(bg_colour), weight = 1,
                             fillOpacity = 1, radius = 7,
                             label = ~tooltip) %>%
   leaflet::addLegend("topright",
                      colors = perc_values$colour,
                      labels = perc_values$nice,
                      values = ~as.vector(bg_colour))
}


well_plots_base <- function(title = "", legend = "right", caption = NA) {
  p_values <- dplyr::filter(perc_values, !.data$class %in% c("p_max", "p_min"))
  g <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = legend,
                   legend.margin = ggplot2::margin(),
                   legend.box.spacing = ggplot2::unit(5, "pt"),
                   legend.spacing = ggplot2::unit(1, "pt")) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::scale_size_manual(
      values = stats::setNames(plot_values$size, plot_values$type)) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(p_values$colour, p_values$nice)) +
    ggplot2::labs(x = "Day of Year", y = "Water Level (mbgs)", subtitle = title)

  if(!is.na(caption)) g <- g + ggplot2::labs(caption = caption)
  g
}

well_plot_perc <- function(full, hist, latest_date = NULL,
                           years_min, legend = "right") {

  year <- full %>%
    dplyr::filter(!is.na(.data$Value)) %>%
    dplyr::count(.data$WaterYear) %>%
    dplyr::filter(.data$n >= 7) %>%   # Must have at least one week of data
    dplyr::slice(dplyr::n()) %>%
    dplyr::pull(.data$WaterYear)

  # Get current and previous year
  recent <- dplyr::filter(full, .data$WaterYear == !!year)

  lastyear <- dplyr::filter(full, .data$WaterYear == !!year - 1) %>%
    # Fake the year to plot on top
    dplyr::mutate(Date = dplyr::if_else(.data$WaterYear == !!year - 1,
                                        .data$Date + lubridate::years(1),
                                        .data$Date))

  if(nrow(recent) > 0) {
    origin <- min(recent$Date[recent$DayofYear == 1] - lubridate::days(1))
    hist <- hist %>%
      dplyr::mutate(Date = lubridate::as_date(.data$DayofYear, origin = !!origin),
                    Approval = "Median",
                    q = purrr::map(.data$v, well_quantiles, minmax = FALSE)) %>%
      tidyr::unnest(q)

    data <- dplyr::select(recent, "Date", "Approval", "Value") %>%
      dplyr::bind_rows(dplyr::select(hist, "Date", "Approval",
                                     "Value" = "median")) %>%
      dplyr::mutate(Approval = factor(
        .data$Approval, levels = c("Median", "Approved", "Working")))

    title <- glue_collapse(unique(recent$CalendarYear), ' - ')
    title <- glue::glue("Water Year {title}")

    range_name <- glue::glue("Historical range of min & max ",
                             "({hist$start_year[1]} - {hist$end_year[1]})")

    caption <- NA_character_
    if(nrow(hist) == 0) {
      caption <- "Not enough non-missing data to calculate percentiles"
    } else if(any(hist$quality_hist == "poor")) {
      caption <- glue::glue("Percentiles skipped for some dates with ",
                            "less than {years_min} years of data.")
    }


    p <- well_plots_base(title = title, legend = legend, caption = caption) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 14))

    if(nrow(hist) > 0) {
      p <- p +
        ggplot2::geom_ribbon(data = hist, alpha = 0.5,
                             ggplot2::aes_string(x = "Date", ymin = "q_low",
                                                 ymax = "q_high", fill = "nice"))
    }
    p <- p +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes_string(x = "Date", y = "Value",
                            colour = "Approval", size = "Approval"),
        na.rm = TRUE) +
      ggplot2::geom_line(
        data = lastyear,
        ggplot2::aes_string(x = "Date", y = "Value", linetype = "'Previous Year'",
                            colour = "Approval", size = "Approval"),
        na.rm = TRUE) +
      ggplot2::scale_linetype_manual(values = "dotted")

    if(!is.null(latest_date) &&
       nrow(latest_date) > 0 &&
       latest_date$Date %in% data$Date) {
      p <- p +
        ggplot2::geom_point(
          data = latest_date, size = 3,
          ggplot2::aes_string(x = "Date", y = "Value", shape = "'Latest Report Date'"),
          fill = "black")
    }
    p +
      ggplot2::scale_colour_manual(
        values = stats::setNames(plot_values$colour, plot_values$type)) +
      ggplot2::scale_shape_manual(values = 21) +
      ggplot2::guides(shape = guide_legend(order = 1),
                      fill = guide_legend(order = 2),
                      colour = guide_legend(order = 3),
                      linetype = guide_legend(order = 4), size = "none")

  } else {
    well_plots_base() +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank()) +
      ggplot2::annotate(geom = "text", x = Sys.Date(), y = 1, label = "No Data")
  }
}

well_plot_hist <- function(full, hist, legend = "right") {

  well_plots_base(title = "Historical Record", legend = legend) +
    ggplot2::geom_line(data = full,
                       ggplot2::aes_string(x = "Date", y = "Value",
                                           colour = "Approval"),
                       na.rm = TRUE) +
    ggplot2::geom_point(data = full,
                        ggplot2::aes_string(x = "Date", y = "Value",
                                            colour = "Approval"),
                        na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(plot_values$colour[1:2], plot_values$type[1:2]))
}


well_table_overview <- function(w_dates, format = "html") {
  w_dates %>%
    well_meta() %>%
    dplyr::mutate(
      ow = ow_link(.data$ow, format = format),
      Value = as.character(round(.data$Value, 2)),
      Value = dplyr::if_else(.data$Date != .data$report_dates & !is.na(.data$Value),
                             as.character(glue::glue("{.data$Value}*")),
                             .data$Value)) %>%
    dplyr::arrange(.data$area, .data$location, .data$ow, dplyr::desc(.data$Date)) %>%
    dplyr::select("region", "Area" = "area", "Location Name" = location,
                  "Obs.\nWell" = "ow", "Value", "report_dates") %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "Value")
}

well_table_below_norm <- function(w_perc, window, which = "totals") {

  if(which == "totals") {
    r <- w_perc %>%
      dplyr::select(-"subtype") %>%
      dplyr::filter(stringr::str_detect(.data$class, "low")) %>%
      dplyr::group_by(.data$report_dates) %>%
      dplyr::summarize(t = unique(.data$n_total_date),
                       n = sum(.data$n_class_subtype),
                       p = round(.data$n / .data$t * 100),
                       p = dplyr::if_else(is.nan(.data$p), 0, .data$p),
                       .groups = "drop") %>%
      dplyr::mutate(text = glue::glue("{p}% ({n}/{t})"),
                    text = dplyr::if_else(.data$t == 0, glue::glue(""), .data$text)) %>%
      dplyr::select("report_dates", "text") %>%
      dplyr::arrange(dplyr::desc(.data$report_dates))
  } else {
    r <- w_perc %>%
      dplyr::filter(stringr::str_detect(.data$class, "low")) %>%
      dplyr::left_join(type_values, by = "subtype") %>%
      dplyr::group_by(.data$report_dates, .data[[which]], .data$subtype) %>%
      dplyr::summarize(t = unique(.data$n_total_subtype),
                       n = sum(.data$n_class_subtype), .groups = "drop_last") %>%
      dplyr::summarize(t = sum(.data$t),
                       n = sum(.data$n),
                       p = round(.data$n / .data$t * 100),
                       p = dplyr::if_else(is.nan(.data$p), 0, .data$p),
                       .groups = "drop") %>%
      dplyr::mutate(text = glue::glue("{p}% ({n}/{t})"),
                    text = dplyr::if_else(t == 0, glue::glue(""), .data$text)) %>%
      dplyr::select("report_dates", !!which, "text")
  }

  r <- r %>%
    dplyr::arrange(dplyr::desc(.data$report_dates)) %>%
    dplyr::mutate(report_dates = dplyr::if_else(
      .data$report_dates %in% !!window,
      as.character(glue::glue("{.data$report_dates}*")),
      as.character(.data$report_dates))) %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "text")

  if(which == "type") {
    r <- dplyr::select(r, "Aquifer Type" = "type", dplyr::everything())
  } else if(which == "hydraulic_connectivity") {
    r <- dplyr::select(r, "Hydraulic connectivity" = "hydraulic_connectivity",
                       dplyr::everything())
  }
  r
}

well_table_status <- function(w_perc, perc_values, window) {
  w_perc %>%
    dplyr::select("class", "report_dates", "n_total_class", "n_total_date") %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$report_dates)) %>%
    tidyr::pivot_longer(cols = dplyr::contains("total"), names_to = "total",
                        values_to = "n") %>%
    dplyr::mutate(class = dplyr::if_else(stringr::str_detect(.data$total, "date"),
                                         "Across all classes", .data$class),
                  class = factor(.data$class, levels = c(!!perc_values$nice,
                                                         "Across all classes"))) %>%
    dplyr::select(-"total") %>%
    dplyr::distinct() %>%
    dplyr::mutate(report_dates =
                    dplyr::if_else(.data$report_dates %in% !!window,
                                   as.character(glue::glue("{report_dates}*")),
                                   as.character(.data$report_dates))) %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "n") %>%
    dplyr::arrange(.data$class) %>%
    dplyr::select("class", dplyr::everything())
}

well_table_summary <- function(w_dates, w_hist, perc_values, format = "html") {

  t <- well_hist_compare(w_dates, w_hist)

  last_year <- t %>%
    dplyr::filter(!.data$CurrentYear) %>%
    dplyr::select("ow", "value_last_year" = "Value", "report_dates") %>%
    dplyr::mutate(report_dates = .data$report_dates + lubridate::years(1))

  if(format == "pdf") percent <- "\\%" else percent <- "%"

  t %>%
    well_meta() %>%
    dplyr::arrange(.data$ow, dplyr::desc(.data$Date)) %>%
    dplyr::group_by(.data$ow) %>%
    dplyr::filter(.data$CurrentYear) %>%
    dplyr::mutate(keep = dplyr::if_else(all(is.na(.data$Value)),
                                        .data$Date[1],
                                        .data$Date[!is.na(.data$Value)][1])) %>%
    dplyr::filter(.data$Date == .data$keep) %>%
    dplyr::left_join(last_year, by = c("ow", "report_dates")) %>%
    dplyr::mutate(
      ow_link = ow_link(.data$ow, format = format),
      class = purrr::map_chr(.data$percentile, perc_match, cols = "nice"),
      Value = as.character(round(.data$Value, 2)),
      Value = dplyr::if_else(.data$Approval == "Working" & !is.na(.data$Value),
                             as.character(glue::glue("{Value}*")),
                             .data$Value),
      value_last_year = round(.data$value_last_year, 2),
      class = tidyr::replace_na(.data$class, ""),
      percentile = dplyr::if_else(
        is.na(.data$percentile),
        glue::glue(""),
        glue::glue(" ({round((1 - percentile) * 100)}{percent})")),
      percentile = glue::glue("{class}{percentile}"),
      n_years = dplyr::case_when(
        .data$percentile == "" ~ glue::glue(""),
        TRUE ~ glue::glue(.data$n_years)),
      median = round(.data$median, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$region, .data$area, .data$location, .data$ow) %>%
    dplyr::mutate(
      bg_colour = rlang::set_names(!!perc_values$colour, !!perc_values$nice)[.data$class],
      bg_colour = tidyr::replace_na(.data$bg_colour, "white"),
      txt_colour = rlang::set_names(!!perc_values$txt_colour, !!perc_values$nice)[.data$class],
      txt_colour = tidyr::replace_na(.data$txt_colour, "black"),
      percentile = glue::glue("{percentile}<br><small>(n = {n_years})</small>"),
      percentile = dplyr::if_else(is.na(Value), glue::glue(""), percentile)) %>%
    dplyr::select(
      "bg_colour", "txt_colour", "ow",
      "region",
      "Area" = "area", "Location Name" = "location",
      "Obs.\nWell" = "ow_link", "Aquifer Type" = "type",
      "Latest\nDate" = "Date", "Latest\nValue" = "Value",
      "Percentile Class" = "percentile",
      "Previous Year's\nValue" = "value_last_year")
}

appendix_dates <- function(w_dates, format = "html") {
  w_dates %>%
    well_meta() %>%
    dplyr::mutate(
      ow = ow_link(.data$ow, format = format),
      Value = as.character(round(.data$Value, 2)),
      Date = dplyr::if_else(is.na(.data$Value),
                            lubridate::as_date(NA),
                            .data$Date)) %>%
    dplyr::arrange(.data$region, .data$area, .data$location,
                   .data$ow, dplyr::desc(.data$report_dates)) %>%
    dplyr::select("region", "Area" = "area", "Location Name" = "location",
                  "Obs.\nWell" = "ow", "report_dates", "Date") %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "Date")
}
