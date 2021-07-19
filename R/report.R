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
#' @param report_dates Character vector. Two current dates to explore
#' @param within Numeric. Number of days within which to get alternative dates
#' @param years_min Numeric. Minimum number of years required to to calculate a
#'   percentiles
#' @param years_max Numeric. Maximum number of years used to to calculate a
#'   percentiles (starting with previous year).
#' @param out_dir Character. Location of output report. Defaults to working directory.
#' @param cache_age Logical. Maxmum age in days of cached datasets (not obs well
#'   data, but metadata related to regional maps, aquifer and wells).
#'
#' @examples
#'
#' \dontrun{
#'
#' well_report(ows = c("OW008", "OW217", "OW377", "OW197"),
#'             report_dates = c(Sys.Date(), Sys.Date() - lubridate::weeks(2)))
#'
#' }
#'
#' @export
#'
well_report <- function(ows, report_dates = Sys.Date(), within = 7,
                        years_min = 5, years_max = 10,
                        out_dir = ".", cache_age = 7) {

  report_dates <- dates_check(report_dates)


  data_update()

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
  w_full <- well_prep(ows, water_year_start = 10, report_dates)
  message("- Summarizing historical statistics")
  w_hist <- well_hist(w_full, years_min, years_max)

  message("- Calculating best report dates")
  w_dates <- well_dates(w_full, w_hist, report_dates, within)

  message("- Comparing current to historical data")
  w_comp <- well_hist_compare(w_dates, w_hist)

  message("- Summarizing percentiles")
  w_perc <- well_percentiles(w_comp)

  message("- Writing report")
  rmarkdown::render(system.file("rmd_report", "report.Rmd", package = "bcgwlreports"),
                    params = list("w_full" = w_full, "w_hist" = w_hist,
                                  "w_comp" = w_hist, "w_perc" = w_perc,
                                  "w_dates" = w_dates, "report_dates" = report_dates,
                                  "within" = within,
                                  "years_min" = years_min, "years_max" = years_max),
                    output_dir = out_dir,
                    output_file = glue::glue("report_{Sys.Date()}.pdf"))
}
