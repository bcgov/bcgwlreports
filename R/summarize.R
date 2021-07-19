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

dates_check <- function(report_dates) {

  report_dates <- suppressWarnings(lubridate::as_date(report_dates))
  if(any(is.na(report_dates))) {
    stop("report_dates must be valid dates YYYY-MM-DD", call. = FALSE)
  } else if (any(report_dates > Sys.Date())) {
    stop("Cannot calculate reports for future dates", call. = FALSE)
  }

  sort(c(report_dates, report_dates - lubridate::years(1)), decreasing = TRUE)
}


well_prep <- function(ows, water_year_start, report_dates) {
  w <- ow_update(ows) %>%
    dplyr::mutate(data = purrr::map(.data$data, well_clean,
                                    water_year_start = !!water_year_start,
                                    report_dates = !!report_dates)) %>%
    tidyr::unnest(.data$data) %>%
    dplyr::select(-"file")

  wy <- unique(w$WaterYear[w$Date == max(report_dates)])

  dplyr::mutate(w, CurrentYear = .data$WaterYear == !!wy)
}

well_clean <- function(w, water_year_start, report_dates) {
  w %>%
    dplyr::select(-"myLocation") %>%
    dplyr::mutate(Date = lubridate::as_date(.data$Time)) %>%
    dplyr::group_by(.data$Date) %>%
    # makes a day "Working" if any hour is "Working"
    dplyr::mutate(Approval = dplyr::case_when(
      any(.data$Approval == "Working") ~ "Working",
      TRUE ~ "Approved")) %>%
    # Create daily averages, keeping dates and approvals
    dplyr::group_by(.data$Date, .data$Approval) %>%
    dplyr::summarise(Value = mean(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    find_continuous() %>%
    tidyr::complete(Date = !!report_dates) %>%
    # Fill in dates with missing values with NA and add various date columns
    fasstr::fill_missing_dates(water_year_start = water_year_start) %>%
    fasstr::add_date_variables(water_year_start = water_year_start) %>%
    # If filled with NA, make "Working" and categorize data for historic/recent
    dplyr::mutate(Approval = dplyr::if_else(is.na(.data$Approval), "Working", .data$Approval),
                  water_year_start = !!water_year_start) %>%
    dplyr::filter(.data$DayofYear != 366)
}


well_meta <- function(w) {
  w %>%
    dplyr::left_join(well_regions(unique(w$ow)), by = "ow") %>%
    dplyr::left_join(data_load("aquifers"), by = c("aquifer_id", "ow"))
}

well_hist <- function(w_full, years_min, years_max) {
  current_year <- unique(w_full$WaterYear[w_full$CurrentYear])

  w_full %>%
    dplyr::filter(.data$WaterYear >= !!current_year - !!years_max,
                  !.data$CurrentYear,
                  .data$Approval == "Approved",
                  !is.na(.data$Value)) %>%
    dplyr::group_by(.data$ow, .data$DayofYear) %>%
    dplyr::summarize(min = min(.data$Value, na.rm = TRUE),
                     max = max(.data$Value, na.rm = TRUE),
                     median = stats::median(.data$Value, na.rm = TRUE),
                     mean = mean(.data$Value, na.rm = TRUE),
                     n_years = length(unique(.data$WaterYear)),
                     start_year = min(.data$WaterYear),
                     end_year = max(.data$WaterYear),
                     v = list(.data$Value),
                     p = list(stats::ecdf(.data$Value)), .groups = "drop") %>%
    dplyr::mutate(
      quality_hist = dplyr::case_when(.data$n_years == !!years_max ~ "good",
                                      .data$n_years >= !!years_min ~ "fair",
                                      TRUE ~ "poor"),
      quality_hist = factor(.data$quality_hist, levels = c("poor", "fair", "good")),
      v = dplyr::if_else(.data$quality_hist == "poor", list(NA), .data$v),
      p = dplyr::if_else(.data$quality_hist == "poor", list(NA), .data$p))
}

well_dates <- function(w_full, w_hist, report_dates, within) {

  r <- dplyr::tibble(report_dates = report_dates) %>%
    dplyr::mutate(Date = purrr::map(.data$report_dates,
                                    ~seq(. - lubridate::days(!!within),
                                         . + lubridate::days(!!within),
                                         by = "1 day"))) %>%
    tidyr::unnest(.data$Date)

  w_full %>%
    dplyr::right_join(r, by = "Date") %>%
    dplyr::left_join(
      dplyr::select(w_hist, "ow", "DayofYear", "quality_hist", "n_years"),
      by = c("ow", "DayofYear")) %>%
    dplyr::group_by(.data$ow, .data$report_dates) %>%
    dplyr::arrange(dplyr::desc(.data$quality_hist),
                   abs(.data$Date - .data$report_dates),
                   .by_group = TRUE) %>%
    dplyr::mutate(keep = dplyr::if_else(all(is.na(.data$Value)),
                                        .data$Date[1],
                                        .data$Date[!is.na(.data$Value)][1])) %>%
    dplyr::filter(.data$Date == .data$keep) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"keep")
}

well_hist_compare <- function(w_dates, w_hist) {
  w_dates %>%
    dplyr::left_join(w_hist, by = c("ow", "DayofYear", "n_years", "quality_hist")) %>%
    dplyr::mutate(percentile = purrr::map2_dbl(
      .data$p, .data$Value,
      ~{if(!is.null(.x)) .x(.y) else NA_real_}))
}

well_percentiles <- function(w_comp) {
  w_comp %>%
    well_meta() %>%
    dplyr::select("ow", "Date", "report_dates", "percentile", "type") %>%
    dplyr::filter(!is.na(.data$percentile)) %>%
    dplyr::mutate(class = purrr::map_chr(.data$percentile, perc_match, cols = "nice")) %>%
    dplyr::select(-"percentile") %>%
    dplyr::mutate(window = .data$Date != .data$report_dates) %>%
    dplyr::group_by(.data$report_dates, .data$class, .data$type) %>%
    dplyr::summarize(n_class_type = dplyr::n(),
                     window = any(.data$window),
                     .groups = "drop") %>%
    tidyr::complete(report_dates = unique(w_comp$report_dates),
                    type = unique(type_values$type),
                    class = perc_values$nice, fill = list(n_class_type = 0)) %>%
    dplyr::group_by(.data$report_dates, .data$type) %>%
    dplyr::mutate(n_total_type = sum(.data$n_class_type)) %>%
    dplyr::group_by(.data$report_dates, .data$class) %>%
    dplyr::mutate(n_total_class = sum(.data$n_class_type)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$report_dates) %>%
    dplyr::mutate(n_total_date = sum(.data$n_class_type),
           window = any(.data$window, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

well_quantiles <- function(values, minmax = TRUE) {
  p_values <- perc_values
  if(!minmax) p_values <- dplyr::filter(p_values,
                                        !.data$class %in% c("p_max", "p_min"))

  p_values %>%
    dplyr::mutate(
      q_low = stats::quantile(!!values, !!p_values$low, na.rm = TRUE),
      q_high = stats::quantile(!!values, !!p_values$high, na.rm = TRUE)) %>%
    dplyr::select("nice", "q_low", "q_high") %>%
    dplyr::mutate(nice = factor(.data$nice, levels = !!p_values$nice))
}


well_regions <- function(ows) {
  data_load("wells_sf") %>%
    dplyr::filter(.data$ow %in% ows) %>%
    sf::st_join(data_load("regions")) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(area_name = dplyr::if_else(
      .data$area_name == "Coast Natural Resource Area",
      .data$region_name, .data$area_name)) %>%
    dplyr::select(-"region_name")
}
