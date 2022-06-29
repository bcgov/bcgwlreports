# Copyright 2022 Province of British Columbia
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

#' Prepare well data for creating table and plots
#'
#' @inheritParams well_report
#' @details `remarks` can be a file path to a TSV (tab-separated) text file or
#'   Excel file contain columns 'ow' and 'remarks', or it can be a
#'   `data.frame()`/`tibble()` (see examples) containing the same. Note that CSV
#'   is not permitted as ',' is used for separating variables which can make it
#'   difficult to write out complete, complex remarks.
#'
#' @examples
#'
#' \dontrun{
#'
#' wells <- c('OW400')
#'
#' gw_data <- gw_data_prep(ows = wells)
#'
#' }
#'
#' @export
#'
gw_data_prep <- function(ows,
                         report_dates = Sys.Date(),
                         remarks = NULL,
                         n_days = 14,
                         years_min = 5,
                         cache_age = 7) {

  check_numeric(n_days, type = "n_days", lower = 0)
  check_numeric(years_min, type = "years_min", lower = 1)
  check_numeric(cache_age, type = "cache_age", lower = 0)

  report_dates <- check_dates(report_dates, n_days)

  remarks <- check_remarks(remarks, ows)

  # Update the local data if cache out of date
  data_update(cache_age)

  # Format obs wells just in case
  ows <- toupper(ows)
  ows <- stringr::str_trim(ows)
  if(any(o <- !stringr::str_detect(ows, "OW[0-9]{3}"))) {
    stop(glue::glue("Some obs wells are not valid IDs (OW000): ",
                    {glue::glue_collapse(ows[o], sep = ',')}),
         call. = FALSE)
  }
  ows <- sort(ows)

  message(glue::glue("- Fetching/cleaning obs well data ({length(ows)} wells)"))

  w_full_all <- well_prep(ows, water_year_start = 10, report_dates,
                          exclude_non_continuous = FALSE)

  f <- function(x) { rev(cumsum(!is.na(rev(x)))) != 0 }
  w_full <- dplyr::filter(w_full_all, .data$continuous_data) %>%
    dplyr::group_by(ow) %>%
    dplyr::filter(f(Value)) %>%
    dplyr::ungroup()

  message("- Summarizing historical statistics")
  w_hist <- well_hist(w_full, years_min)

  message("- Calculating best report dates")
  w_dates <- well_dates(w_full, w_hist, report_dates, n_days)
  # return(w_dates)

  message("- Comparing current to historical data")
  w_comp <- well_hist_compare(w_dates, w_hist)
  #return(w_comp)
  message("- Summarizing percentiles")
  w_perc <- well_percentiles(w_comp)



  message("- Creating list of objects")


  window <- w_perc %>%
    dplyr::filter(window) %>%
    dplyr::pull(report_dates) %>%
    unique()

  max_report_date <- max(report_dates, na.rm = TRUE)
  full_window <- seq.Date(from = max_report_date - lubridate::days(n_days),
                          to = max_report_date + lubridate::days(n_days),
                          by = "day")

  range1 <- length(report_dates)/2
  range2 <- (range1 + 1):(range1 * 2)
  range1 <- 1:range1

  wy <- ifelse(lubridate::month(max_report_date) < 10,
               lubridate::year(max_report_date),
               lubridate::year(max_report_date) + 1)

  remarks <- dplyr::tibble(ow = NA_character_, remarks = NA_character_, .rows = 0)
  if(!is.null(remarks)) remarks <- remarks

  # Create table
  details <- well_table_summary(w_dates, w_hist, perc_values, full_window)


  return(list("ows" = ows,
              "remarks" = remarks,
              "w_full_all" = w_full_all,
              "w_full" = w_full,
              "w_hist" = w_hist,
              "w_dates" = w_dates,
              "w_comp" = w_comp,
              "w_perc" = w_perc,
              "details" = details,
              "report_dates" = report_dates,
              "n_days" = n_days,
              "years_min" = years_min,
              "window" = window,
              "full_window" = full_window,
              "range1" = range1,
              "range2" = range2,
              "wy" = wy))

}
