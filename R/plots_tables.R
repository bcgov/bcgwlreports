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


well_plots_base <- function(title = "", legend = "right", caption = NA) {
  p_values <- dplyr::filter(perc_values, !.data$class %in% c("p_max", "p_min"))
  g <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = legend,
                   legend.margin = ggplot2::margin(),
                   legend.box.spacing = ggplot2::unit(5, "pt"),
                   legend.spacing = ggplot2::unit(1, "pt")) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::scale_size_manual(values = stats::setNames(plot_values$size, plot_values$type)) +
    ggplot2::scale_colour_manual(values = stats::setNames(plot_values$colour, plot_values$type)) +
    ggplot2::scale_fill_manual(values = stats::setNames(p_values$colour, p_values$nice)) +
    ggplot2::labs(x = "Day of Year", y = "DBG (m)", subtitle = title)

  if(!is.na(caption)) g <- g + ggplot2::labs(caption = caption)
  g
}

well_plot_perc <- function(full, hist, latest_date = NULL,
                           years_min, years_max, legend = "right") {

  year <- full %>%
    dplyr::filter(!is.na(.data$Value)) %>%
    dplyr::count(.data$WaterYear) %>%
    dplyr::filter(.data$n >= 7) %>%   # Must have at least one week of data
    dplyr::slice(dplyr::n()) %>%
    dplyr::pull(.data$WaterYear)

  recent <- dplyr::filter(full, .data$WaterYear == !!year)

  if(nrow(recent) > 0) {
    origin <- recent$Date[recent$DayofYear == 1] - lubridate::days(1)
    hist <- hist %>%
      dplyr::mutate(Date = lubridate::as_date(.data$DayofYear, origin = !!origin),
                    Approval = "Median",
                    q = purrr::map(.data$v, well_quantiles, minmax = FALSE)) %>%
      tidyr::unnest(q)

    data <- dplyr::select(recent, "Date", "Approval", "Value") %>%
      dplyr::bind_rows(dplyr::select(hist, "Date", "Approval", "Value" = "median")) %>%
      dplyr::mutate(Approval = factor(.data$Approval,
                                      levels = c("Median", "Approved", "Working")))

    title <- glue::glue("Water Year {glue_collapse(unique(recent$CalendarYear), ' - ')}")
    range_name <- glue::glue("Historical range of min & max ({hist$start_year[1]} - ",
                             "{hist$end_year[1]})")

    caption <- dplyr::case_when(
      nrow(hist) == 0 ~ "Not enough non-missing data to calculate percentiles",
      any(hist$quality_hist != "good") ~
        as.character(
          glue::glue("Data quality for percentiles are indicated above the figure\n",
                     "Black = 'Fair' ({years_min} <= years of data < {years_max}); ",
                     "Red = 'Poor' (years of data < {years_min})")),
      TRUE ~ NA_character_)

    p <- well_plots_base(title = title, legend = legend, caption = caption) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 14))

    if(nrow(hist) > 0) {
      # Get position for percentile quality points
      y <- hist %>%
        dplyr::summarize(min = min(.data$min),
                         max = max(.data$max),
                         range = .data$max - .data$min,
                         y = .data$min - (.data$range * 0.1)) %>%
        dplyr::pull(.data$y)

      p <- p +
        ggplot2::geom_ribbon(data = hist, alpha = 0.35,
                             ggplot2::aes_string(x = "Date", ymin = "q_low",
                                                 ymax = "q_high", fill = "nice"))

      if(nrow(fair <- dplyr::filter(hist, .data$quality_hist == "fair")) > 0) {
        p <- p +
          ggplot2::annotate(geom = "point", x = fair$Date, y = y, size = 0.01)
      }
      if(nrow(poor <- dplyr::filter(hist, .data$quality_hist == "poor")) > 0) {
        p <- p +
          ggplot2::annotate(geom = "point", x = poor$Date, y = y, size = 0.01,
                            colour = "red")
      }
    }
    p <- p +
      ggplot2::geom_line(data = data,
                         ggplot2::aes_string(x = "Date", y = "Value",
                                             colour = "Approval", size = "Approval"),
                         na.rm = TRUE)

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
      ggplot2::scale_shape_manual(values = 21)

  } else {
    well_plots_base() +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank()) +
      ggplot2::annotate(geom = "text", x = Sys.Date(), y = 1, label = "No Data")
  }
}

well_plot_hist <- function(full, hist, date_range, latest_date = NULL,
                           legend = "right", wrap_year = FALSE) {

  data <- full %>%
    dplyr::select("Date", "Approval", "Value") %>%
    dplyr::filter(.data$Date >= !!date_range[1], .data$Date < !!date_range[2]) %>%
    tidyr::complete(Date = seq(!!date_range[1], !!date_range[2]-1, "1 day"),
                    Approval = c("Working", "Approved"),
                    fill = list(Value = NA)) %>%
    fasstr::add_date_variables(water_year_start = full$water_year_start[1]) %>%
    dplyr::left_join(dplyr::select(hist, "DayofYear", "Median" = "median"),
                     by = "DayofYear") %>%
    tidyr::pivot_wider(names_from = "Approval", values_from = "Value") %>%
    tidyr::pivot_longer(cols = c("Approved", "Working", "Median"),
                        names_to = "Approval", values_to = "Value") %>%
    dplyr::mutate(Approval = forcats::fct_relevel(.data$Approval, "Median"))

  yr_bin <- unique(data$WaterYear)
  yr_bin <- yr_bin[c(1, length(yr_bin)/2, length(yr_bin))]

  data <- dplyr::mutate(data, yr_bin = dplyr::if_else(
    .data$WaterYear <= !!yr_bin[2],
    glue::glue("Water Year {yr_bin[1]} - {yr_bin[2]}"),
    glue::glue("Water Year {yr_bin[2]+1} - {yr_bin[3]}")))

  p <- well_plots_base(title = "Historical Record", legend = legend) +
    ggplot2::geom_line(data = data,
                       ggplot2::aes_string(x = "Date", y = "Value",
                                           colour = "Approval", size = "Approval"),
                       na.rm = TRUE) +
    ggplot2::facet_wrap(~ yr_bin, ncol = 1, scales = "free_x")

  if(!is.null(latest_date) &&
     nrow(latest_date) > 0 &&
     latest_date$Date %in% data$Date
     && latest_date$CurrentYear) {
    latest_date$yr_bin <- data$yr_bin[nrow(data)]
    p <- p +
      ggplot2::geom_point(
        data = latest_date, size = 4,
        ggplot2::aes_string(x = "Date", y = "Value",
                            shape = "'Latest Report Date'")) +
      ggplot2::guides(shape = ggplot2::guide_legend(
        override.aes = list(colour = "black",
                            fill = "black",
                            shape = 21)))
  }

  p
}


well_table_overview <- function(w_dates, format = "html") {
  w_dates %>%
    well_meta() %>%
    dplyr::mutate(
      ow = ow_link(.data$ow, format = format),
      area_name = stringr::str_remove_all(.data$area_name,
                                          "( Natural Resource )|(Region)|(Area)"),
      district_name = stringr::str_remove(.data$district_name,
                                          " Natural Resource District"),
      Value = as.character(round(.data$Value, 2)),
      Value = dplyr::if_else(.data$Date != .data$report_dates & !is.na(.data$Value),
                             as.character(glue::glue("{.data$Value}*")),
                             .data$Value)) %>%
    dplyr::arrange(.data$area_name, .data$district_name, .data$ow, dplyr::desc(.data$Date)) %>%
    dplyr::select("area_name", "district_name", "ow", "Value", "report_dates") %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "Value") %>%
    dplyr::rename("Region" = "area_name", "Geographic\nArea" = "district_name", "Obs.\nWell" = "ow")
}

well_table_below_norm <- function(w_perc, window) {

  totals <- w_perc %>%
    dplyr::select(-"type") %>%
    dplyr::filter(stringr::str_detect(.data$class, "low")) %>%
    dplyr::group_by(.data$report_dates) %>%
    dplyr::summarize(t = unique(.data$n_total_date),
                     n = sum(.data$n_class_type),
                     p = round(.data$n / .data$t * 100),
                     p = dplyr::if_else(is.nan(.data$p), 0, .data$p),
                     .groups = "drop") %>%
    dplyr::mutate(text = glue::glue("{p}% ({n}/{t})"),
                  text = dplyr::if_else(.data$t == 0, glue::glue(""), .data$text)) %>%
    dplyr::select("report_dates", "text") %>%
    dplyr::arrange(dplyr::desc(.data$report_dates)) %>%
    dplyr::mutate(type = "Across all types")

  w_perc %>%
    dplyr::filter(stringr::str_detect(.data$class, "low")) %>%
    dplyr::group_by(.data$report_dates, .data$type) %>%
    dplyr::summarize(t = unique(.data$n_total_type), n = sum(.data$n_class_type),
                     p = round(.data$n / .data$t * 100),
                     p = dplyr::if_else(is.nan(.data$p), 0, .data$p),
                     .groups = "drop") %>%
    dplyr::mutate(text = glue::glue("{p}% ({n}/{t})"),
                  text = dplyr::if_else(t == 0, glue::glue(""), .data$text)) %>%
    dplyr::select("report_dates", "type", "text") %>%
    dplyr::arrange(dplyr::desc(.data$report_dates)) %>%
    dplyr::bind_rows(totals) %>%
    dplyr::mutate(report_dates = dplyr::if_else(
      .data$report_dates %in% !!window,
      as.character(glue::glue("{.data$report_dates}*")),
      as.character(.data$report_dates))) %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "text") %>%
    dplyr::select("type", dplyr::everything()) %>%
    dplyr::rename("Aquifer Type" = "type")
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
      ow = ow_link(.data$ow, format = format),
      class = purrr::map_chr(.data$percentile, perc_match, cols = "nice"),
      Name = "",
      area_name = stringr::str_remove_all(.data$area_name,
                                          "( Natural Resource )|(Region)|(Area)"),
      district_name = stringr::str_remove(.data$district_name,
                                          "Natural Resource District"),
      Value = as.character(round(.data$Value, 2)),
      Value = dplyr::if_else(.data$Approval == "Working" & !is.na(.data$Value),
                             as.character(glue::glue("{Value}*")),
                             .data$Value),
      value_last_year = round(.data$value_last_year, 2),
      class = tidyr::replace_na(.data$class, ""),
      percentile = dplyr::if_else(is.na(.data$percentile),
                                  glue::glue(""),
                                  glue::glue(" ({round((1 - percentile) * 100)}{percent})")),
      percentile = glue::glue("{class}{percentile}"),
      n_years = dplyr::case_when(
        .data$percentile == "" ~ glue::glue(""),
        .data$quality_hist == "fair" ~ glue::glue("{n_years}**"),
        TRUE ~ glue::glue(.data$n_years)),
      median = round(.data$median, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$area_name, .data$district_name, .data$ow) %>%
    dplyr::mutate(
      bg_colour = rlang::set_names(!!perc_values$colour, !!perc_values$nice)[.data$class],
      bg_colour = tidyr::replace_na(.data$bg_colour, "white"),
      txt_colour = rlang::set_names(!!perc_values$txt_colour, !!perc_values$nice)[.data$class],
      txt_colour = tidyr::replace_na(.data$txt_colour, "black")) %>%
    dplyr::select(
      "bg_colour", "txt_colour",
      "Region" = "area_name", "Geographic\nArea" = "district_name", "Name",
      "Obs.\nWell" = "ow", "Aquifer Type" = "type",
      "Latest\nDate" = "Date", "Latest\nValue" = "Value",
      "Percentile Class" = "percentile", "Perc.\nYears"= "n_years",
      "Last Year's\nValue" = "value_last_year", "Median" = "median")
}

appendix_dates <- function(w_dates, format = "html") {
  w_dates %>%
    well_meta() %>%
    dplyr::mutate(
      ow = ow_link(.data$ow, format = format),
      area_name = stringr::str_remove_all(
        .data$area_name, "( Natural Resource )|(Region)|(Area)"),
      district_name = stringr::str_remove(.data$district_name,
                                          " Natural Resource District"),
      Value = as.character(round(.data$Value, 2)),
      Date = dplyr::if_else(is.na(.data$Value),
                            lubridate::as_date(NA),
                            .data$Date)) %>%
    dplyr::arrange(.data$area_name, .data$district_name,
                   .data$ow, dplyr::desc(.data$report_dates)) %>%
    dplyr::select("area_name", "district_name", "ow", "report_dates", "Date") %>%
    tidyr::pivot_wider(names_from = "report_dates", values_from = "Date") %>%
    dplyr::rename("Region" = "area_name", "Geographic\nArea" = "district_name",
                  "Obs.\nWell" = "ow")
}
