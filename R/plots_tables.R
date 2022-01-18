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

gt_perc_colours <- function(gt, perc_col = "percentile") {

  if(perc_col == "stub") {
    for(i in seq_len(nrow(perc_values)))
      gt <- gt %>%
        tab_style(style = list(cell_fill(perc_values$colour[i]),
                               cell_text(perc_values$txt_colour[i])),
                  locations = cells_stub(rows = i))
  } else {
    for(i in seq_len(nrow(perc_values))) {
      gt <- gt %>%
        tab_style(style = list(cell_fill(perc_values$colour[i]),
                               cell_text(perc_values$txt_colour[i])),
                  locations = cells_body(rows = class == perc_values$nice[i],
                                         columns = perc_col))
    }
  }
  gt
}


gt_bcgwl_style <- function(gt) {
  gt %>%
    tab_style(style = cell_text(weight = "bold", size = "large"),
              locations = cells_column_labels()) %>%
    tab_style(style = cell_text(weight = "bold", size = "large"),
              locations = cells_column_spanners()) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) %>%
    tab_style(style = cell_text(color = "#666666"),
              locations = cells_footnotes()) %>%
    tab_options(table_body.hlines.width = 0,
                data_row.padding = px(5),
                footnotes.padding = px(2),
                table.border.top.color = "black",
                table.border.top.width = px(3),
                table.border.bottom.color = "black",
                table_body.border.top.color = "black",
                table_body.border.bottom.color = "black",
                stub.border.width = 0)
}

footnotes_below_normal <- function(gt, missing_dates, missing_data, n_days = NULL) {
  foot1 <- glue("(X/Y) indicates X wells with low values out of Y wells ",
                "total for that date")
  foot2 <- "Blank cells indicate no data"
  foot3 <- glue("Not all reporting dates had data. Includes values ",
                "obtained from a {n_days*2 + 1}-day window centred on the ",
                "reporting date")

  # First foot
  gt <- gt::tab_footnote(gt, footnote = foot1,
                         locations = gt::cells_column_spanners(1))
  marks <- ""

  # Optional extras
  if(missing_data) {
    gt <- gt::tab_footnote(gt, footnote = foot2,
                           locations = gt::cells_column_spanners(2))
    marks <- c(marks, "")
  }

  if(length(missing_dates) > 0) {
    gt <- gt::tab_footnote(
      gt, footnote = foot3,
      locations = gt::cells_column_labels(all_of(missing_dates)))
    marks <- c(marks, "\u2731")
  }

  # Needs at least two to use custom marks
  if(length(marks) == 1) marks <- c(marks, "")

  opt_footnote_marks(gt, marks = marks)
}


well_map <- function(details, format = "html") {

 locs <- data_load("wells_sf") %>%
   dplyr::right_join(dplyr::select(details, "ow", "class", "percentile"),
                     by = "ow") %>%
   dplyr::mutate(
     percentile = if_else(is.na(percentile),
                          glue("No current data"),
                          glue("{percentile}%"))) %>%
   sf::st_transform(4326) %>%
   dplyr::left_join(region_names, by = "ow") %>%
   dplyr::mutate(tooltip = glue::glue(
     "<strong>Well</strong>: {ow_link(ow_fish(.data$ow), format = format)}<br>",
     "<strong>Region</strong>: {.data$region}<br>",
     "<strong>Location</strong>: {.data$location}<br>",
     "<strong>Current percentile</strong>: {.data$percentile}"),
     tooltip = purrr::map(.data$tooltip, htmltools::HTML),
     class = factor(class, levels = perc_values$nice))

 perc_pal <- leaflet::colorFactor(perc_values$colour, levels = perc_values$nice)

 regions <- data_load("regions") %>%
   sf::st_transform(4326)

 leaflet::leaflet(data = locs) %>%
   leaflet::addProviderTiles("Stamen.Terrain") %>%
   leaflet::addCircleMarkers(color = "black",
                             fillColor = ~perc_pal(class), weight = 1,
                             fillOpacity = 1, radius = 7,
                             popup = ~tooltip, label = ~ow,
                             labelOptions = leaflet::labelOptions(
                               noHide = TRUE, textOnly = TRUE, direction = "top",
                               style = list("font-weight" = "bold",
                                            "font-size" = "12px"))) %>%
   leaflet::addLegend("topright", title = "Groundwater Levels",
                      colors = c(perc_values$colour, "#808080"),
                      labels = c(perc_values$nice, "No current data"))
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
    ggplot2::scale_fill_manual(
      values = stats::setNames(p_values$colour, p_values$nice)) +
    ggplot2::labs(x = "Day of Year",
                  y = "Water Level Below Ground (metres)", subtitle = title)

  if(!is.na(caption)) g <- g + ggplot2::labs(caption = caption)
  g
}

well_plot_perc <- function(full, hist, latest_date = NULL,
                           years_min, water_year, legend = "right") {

  # Change names for approval
  full <- full %>%
    dplyr::mutate(Approval = factor(
      .data$Approval,
      levels = c("Median", "Approved", "Working"),
      labels = c("Median", "Current Year Approved", "Current Year Working")))

  # Get current and previous year
  recent <- dplyr::filter(full, .data$WaterYear == !!water_year)

  lastyear <- dplyr::filter(full, .data$WaterYear == !!water_year - 1) %>%
    # Fake the year to plot on top
    dplyr::mutate(Date = dplyr::if_else(.data$WaterYear == !!water_year - 1,
                                        .data$Date + lubridate::years(1),
                                        .data$Date))

  title <- glue::glue("Annual Hydrograph – Water Year ",
                      "[{water_year - 1} - {water_year}]")

  if(nrow(recent) > 0) {
    origin <- min(recent$Date[recent$DayofYear == 1] - lubridate::days(1))
    hist <- hist %>%
      dplyr::mutate(Date = lubridate::as_date(.data$DayofYear, origin = !!origin),
                    Approval = "Median",
                    q = purrr::map(.data$v, well_quantiles, minmax = FALSE)) %>%
      tidyr::unnest(q)

    data <- dplyr::select(recent, "Date", "Approval", "Value") %>%
      dplyr::bind_rows(dplyr::select(hist, "Date", "Approval",
                                     "Value" = "median"))

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
      ggplot2::scale_size_manual(
        values = stats::setNames(plot_values$size, plot_values$type_current)) +
      ggplot2::scale_colour_manual(
        values = stats::setNames(plot_values$colour, plot_values$type_current)) +
      ggplot2::scale_shape_manual(values = 21) +
      ggplot2::guides(shape = guide_legend(order = 1),
                      fill = guide_legend(order = 2),
                      colour = guide_legend(order = 3),
                      linetype = guide_legend(order = 4), size = "none")

  } else {
    well_plots_base(title = title) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank()) +
      ggplot2::annotate(geom = "text", x = Sys.Date(), y = 1, label = "No Data")
  }
}

well_plot_hist <- function(full, hist, legend = "right") {

  title <- dplyr::filter(full, !is.na(Value)) %>%
    dplyr::summarize(min = min(Date), max = max(Date)) %>%
    glue::glue_data("Historical Record - {min} to {max}")

  well_plots_base(title = title, legend = legend) +
    ggplot2::geom_point(data = full,
                        ggplot2::aes_string(x = "Date", y = "Value",
                                            colour = "Approval"),
                        na.rm = TRUE) +
    ggplot2::scale_size_manual(
      values = stats::setNames(plot_values$size[1:2], plot_values$type[1:2])) +
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
      dplyr::filter(stringr::str_detect(.data$class, "low|Min")) %>%
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
      dplyr::filter(stringr::str_detect(.data$class, "low|Min")) %>%
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
    dplyr::left_join(select(perc_values, nice, low_show, high_show), by = c("class" = "nice")) %>%
    dplyr::mutate(class = dplyr::if_else(
      !stringr::str_detect(class, "Max|Min|Across"),
      as.character(glue::glue("{class} ({low_show*100}% - {high_show*100}%)")),
      class)) %>%
    dplyr::select("class", dplyr::everything(), -"low_show", -"high_show")
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
    dplyr::mutate(recent_diff = Value[1] - Value[2],
                  keep = dplyr::if_else(all(is.na(.data$Value)),
                                        .data$Date[1],
                                        .data$Date[!is.na(.data$Value)][1])) %>%
    dplyr::filter(.data$Date == .data$keep) %>%
    dplyr::left_join(last_year, by = c("ow", "report_dates")) %>%
    dplyr::mutate(dplyr::across(
      .cols = c("Value", "value_last_year", "recent_diff"),
      ~if_else(is.na(.), NA_character_, sprintf(., fmt = "%#.2f")))) %>%
    dplyr::mutate(
      class = purrr::map_chr(.data$percentile, perc_match, cols = "nice"),
      percentile = round((1 - percentile) * 100)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$region, .data$area, .data$location, .data$ow) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::select(
      "ow", "region", "area", "location",
      "hydraulic_connectivity", "aquifer_type" = "type",
      "date", "class", "percentile", "n_years", "approval",
      "value", "value_last_year",
      "recent_diff")
}
