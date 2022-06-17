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

#' Compile report
#'
#' @param ows Character vector. Observation well numbers (e.g, "OW000")
#' @param name Character string. Short text to name the file. Will become
#'   `name_YYYY_MM_DD.html`
#' @param report_dates Character vector. Two current dates to explore. By
#'   default a date 2 week ago and 4 weeks before that are used.
#' @param title Character. Title of the report.
#' @param description Character. Descriptive paragraph to place at the start.
#' @param remarks Character / data frame. Path to file OR data frame containing
#'   remarks on specific observation wells to be included in the main summary
#'   table (see Details).
#' @param n_days Numeric. If there is no data on the report date chosen, this is
#'   the range of days over which to look for alternative dates with data.
#'   Defaults to 2 weeks, meaning 2 weeks before and 2 weeks after a given
#'   report date, for a total window of 4 weeks.
#' @param years_min Numeric. Minimum number of years required to to calculate a
#'   percentiles
#' @param out_dir Character. Location of output report. Defaults to working
#'   directory.
#' @param cache_age Logical. Maximum age in days of cached datasets (not obs well
#'   data, but metadata related to regional maps, aquifer and wells).
#' @param cache_report Logical. Whether or not to use a cache for the report plots.
#'   Permits faster runs when tweaking details.
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
#' well_report(ows = c("OW008", "OW217", "OW377", "OW197"))
#'
#' # If short, easiest to add remarks in script:
#'
#' library(dplyr)
#'
#' remarks <- tribble(~ow,     ~remarks,
#'                    "OW377", "Construction in the area disrupting measurements",
#'                    "OW008", "No problems")
#'
#' well_report(ows = c("OW008", "OW217", "OW377", "OW197"),
#'             remarks = remarks)
#'
#' # Or load from a file
#' library(readr)
#' write_tsv(remarks, "remarks.txt")
#' check_remarks(remarks = "remarks.txt")
#' }
#'
#' @export
#'
gw_data_prep <- function(ows,
                         name = "report",
                         report_dates = Sys.Date(),
                         title = NULL,
                         description = NULL,
                         remarks = NULL,
                         n_days = 13,
                         years_min = 5,
                         out_dir = ".",
                         cache_age = 7) {

  check_numeric(n_days, type = "n_days", lower = 0)
  check_numeric(years_min, type = "years_min", lower = 1)
  check_numeric(cache_age, type = "cache_age", lower = 0)
  check_out_dir(out_dir)
  check_name(name)

  check_title(title)
  description <- check_description(description)
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
              "title" = title,
              "description" = description,
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
