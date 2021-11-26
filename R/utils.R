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

ow_n <- function(ows, numeric = TRUE) {
  o <- stringr::str_extract(ows, "[0-9]{3}")
  if(numeric) o <- as.numeric(o)
  o
}

ow_c <- function(ows) {
  glue::glue("OW{stringr::str_pad(ows, '3', pad = 0)}") %>%
    as.character()
}

ow_link <- function(ow, format) {
  ow_ref <- stringr::str_extract(ow, "OW[0-9]{3}") %>%
    tolower()
  if(format == "pdf") ow <- glue::glue("\\hyperref[{ow_ref}]{{{ow}}}")
  if(format == "html") ow <- glue::glue("<a href = '#{ow_ref}'>{ow}</a>")
}

ow_fish <- function(ow) {
  well_meta(ow) %>%
    dplyr::mutate(ow = if_else(.data$hydraulic_connectivity == "Likely",
                               as.character(paste(.data$ow, emo::ji("fish"))),
                               .data$ow)) %>%
    dplyr::pull(ow)
}

find_continuous <- function(w) {
  first_date <- w %>%
    dplyr::mutate(month = lubridate::floor_date(.data$Date, "month")) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 25) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$month)

  dplyr::mutate(w, continuous_data = .data$Date > !!first_date)
}

perc_match <- function(x, cols = "class") {
  perc_values[[cols]][x >= perc_values$low][sum(x >= perc_values$low)]
}

check_title <- function(title) {
  if(!is.null(title) && (!is.character(title) & !is.numeric(title))) {
    stop("'title' must be text or numbers", call. = FALSE)
  }
}

check_description <- function(description) {
  if(!is.null(description)) {
    if(!is.character(description) & !is.numeric(description)) {
      stop("'description' must be text/numbers (a description in text, ",
           "or the path to a text file)", call. = FALSE)
    }

    d <- try(readr::read_lines(description), silent = TRUE)
    if("try-error" %in% class(d)) {
      d <- description
      message("Can't find 'description' as file ('",
              stringr::str_trunc("title.txt", 20),
              "'), assuming text, not file")
    }
  } else d <- NULL
  d
}

check_numeric <- function(x, type, lower) {
  if(!is.numeric(x) || x < lower) {
    stop("'", type, "' must be numeric, ", lower, " or greater",
         call. = FALSE)
  }
}

check_dates <- function(report_dates, n_days) {

  report_dates <- suppressWarnings(lubridate::as_date(report_dates))
  if(any(is.na(report_dates))) {
    stop("report_dates must be valid dates YYYY-MM-DD", call. = FALSE)
  } else if (any(report_dates > Sys.Date())) {
    stop("Cannot calculate reports for future dates", call. = FALSE)
  } else if (length(report_dates) > 2) {
    stop("Can only use two or fewer current dates (for now)", call. = FALSE)
  } else if(length(report_dates) > 1 &&
            (min(report_dates) + lubridate::days(n_days) >= max(report_dates))){
    stop("Range over which to look for data ('n_days') cannot overlap both ",
         "report dates.\nEither make 'n_days' smaller or select two report dates ",
         "farther apart.", call. = FALSE)
  }

  sort(c(report_dates, report_dates - lubridate::years(1)), decreasing = TRUE)
}

check_out_dir <- function(out_dir) {
  if(!is.character(out_dir)) {
    stop("'out_dir' must be a text string indicating the output ",
         "folder for the report", call. = FALSE)
  }
  if(!dir.exists(out_dir)) {
    stop("'out_dir' does not exist, please create it first", call. = FALSE)
  }
}


check_remarks <- function(remarks, ows) {

  if(is.null(remarks)) return(remarks)

  # Data frame or file?
  if(!is.data.frame(remarks)) {
    if(length(remarks) > 1 ||
       !stringr::str_detect(tolower(remarks), ".[a-z]{3,4}$")) {
      stop("'remarks' must either be a data frame or a path to a ",
           "TSV or Excel file", call. = FALSE)
    }

    if(file.exists(remarks)) {
      stop("Cannot find the 'remarks' file:\n", normalizePath(remarks),
           call. = FALSE)
    }

    ext <- stringr::str_extract(tolower(remarks), "[a-z]{3,4}$")

    if(stringr::str_detect(ext, "xls|xlxs")) {
      read <- readxl::read_excel
    } else read <- readr::read_tsv
    remarks <- read(remarks, show_col_types = FALSE)
  }

  remarks <- dplyr::rename_with(remarks, tolower)

  if(any(!c("ow", "remarks") %in% names(remarks))) {
    stop("'remarks' data must have columns 'ow' and 'remarks'", call. = FALSE)
  }

  if(!any(ows %in% remarks$ow)) {
    warning("None of the Obs wells in 'remarks' are in the set of observation ",
            "wells for the report ('ows')")
  }

  remarks
}

check_name <- function(name) {
  if(!is.character(name)) stop("'name' should be text", call. = FALSE)
  if(stringr::str_length(name) > 100) {
    stop("'name' is too long (longer than 100 characters)", call. = FALSE)
  }
}

