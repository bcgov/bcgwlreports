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


data_update <- function(cache_age = 7, type = NULL) {

  d <- data_details(cache_age)

  if(!is.null(type)) d <- dplyr::filter(d, .data$type %in% !!type)

  message("Checking/Updating data...")
  for(i in seq_len(nrow(d))) {
    if(d$update[i]) {
      get(glue::glue("data_{d$type[i]}"))()
    } else message(glue::glue("- Skipping {d$type[i]} data"))
  }
}

data_details <- function(cache_age) {

  if(!dir.exists(data_cache())) {
    # Ask for permission to save data
    cont <- utils::askYesNo(
      paste0("bcgwlreports would like to store data for the reports ",
             "in: \n", data_cache(), "\nIs that okay? ",
             "(You can always use clean_cache() to remove it)"))

    if(!cont) {
      message("Can't store data. Stopping.")
      return(invisible())
    } else {
      dir.create(file.path(data_cache(), "ow"), recursive = TRUE)
      dir.create(file.path(data_cache(), "datasets"), recursive = TRUE)
    }
  }


  dplyr::tibble(f = list.files(file.path(data_cache(), "datasets"), ".rds"),
                date = stringr::str_extract(.data$f, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
                type = stringr::str_remove_all(
                  .data$f, glue::glue("(_{date})|(.csv)|(.rds)"))) %>%
    dplyr::mutate(type = factor(.data$type, levels = data_types)) %>%
    dplyr::mutate(update = dplyr::if_else(Sys.Date() - lubridate::days(!!cache_age) > date,
                                          TRUE, FALSE)) %>%
    tidyr::complete(.data$type, fill = list(update = TRUE)) %>%
    dplyr::arrange(.data$type)
}


data_cache <- function() {
  rappdirs::user_data_dir("bcgwlreports")
}

data_loc <- function(type = NULL, ext = "rds") {
  if(!is.null(type)) {
    file.path(data_cache(), "datasets", glue::glue("{type}_{Sys.Date()}.{ext}"))
  } else {
    file.path(data_cache(), "datasets")
  }
}

#' Clean cache
#'
#' Removes data cache
#'
#' @examples
#'
#' # clean_cache()
#'
#' @export

clean_cache <- function() {
  unlink(rappdirs::user_data_dir("bcgwlreports"), recursive = TRUE)
}


data_wells_sf <- function() {
  message("Downloading Spatial Observation well data")
  data_remove("wells_sf")
  bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
    dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
    dplyr::collect() %>%
    dplyr::mutate(ow = ow_c(.data$OBSERVATION_WELL_NUMBER)) %>%
    dplyr::select("ow", "aquifer_id" = "AQUIFER_ID") %>%
    readr::write_rds(data_loc("wells_sf"))
}

data_aquifers <- function() {

  message("Downloading Aquifer Data")
  data_remove("aquifers")
  httr::GET("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv",
            httr::write_disk(data_loc("aquifers", "csv"), overwrite = TRUE),
            httr::progress())

  data_load("aquifers") %>%
    dplyr::select("aquifer_id", "subtype") %>%
    dplyr::mutate(subtype = stringr::str_extract(.data$subtype,
                                                 "^[1-6]{1}[a-c]{0,1}")) %>%
    dplyr::left_join(type_values, by = "subtype") %>%
    dplyr::right_join(data_load("wells_sf") %>% sf::st_drop_geometry(),
                      by = "aquifer_id") %>%
    readr::write_rds(data_loc("aquifers"))
  unlink(data_loc("aquifers", "csv"))
}
#
# data_gwells <- function() {
#   # Link from https://apps.nrs.gov.bc.ca/gwells/
#   message("Downloading GWELLS")
#   data_remove("well")
#   data_remove("lithology")
#   GET("https://s3.ca-central-1.amazonaws.com/gwells-export/export/gwells.zip",
#       write_disk("./data/gwells.zip", overwrite = TRUE), progress())
#   unzip("./data/gwells.zip", exdir = "./data/",
#         files = c("well.csv", "lithology.csv"), overwrite = TRUE)
#   unlink("./data/gwells.zip")
#
#   data_load("well") %>%
#     filter(!is.na(observation_well_number)) %>%
#     mutate(ow = ow_c(as.numeric(observation_well_number))) %>%
#     write_csv("data/well.csv")
#
#   file.rename(f <- c("./data/well.csv","./data/lithology.csv"),
#               str_replace_all(f, ".csv", glue("_{Sys.Date()}.csv")))
# }

data_regions <- function() {
  message("Downloading Regional maps and coallating")
  data_remove("regions")

  a <- bcmaps::nr_areas(ask = FALSE) %>%
    dplyr::select("area_name" = "AREA_NAME")

  r <- bcmaps::nr_regions(ask = FALSE) %>%
    dplyr::select("region_name" = "REGION_NAME") %>%
    sf::st_join(a, largest = TRUE) %>%
    suppressWarnings()

  d <- bcmaps::nr_districts(ask = FALSE) %>%
    dplyr::select("district_name" = "DISTRICT_NAME") %>%
    sf::st_join(r, largest = TRUE) %>%
    suppressWarnings()

  readr::write_rds(d, data_loc("regions"))
}

data_load <- function(type) {
  f <- list.files(data_loc(), type, full.names = TRUE)
  if(length(f) > 1) stop("More than one file to load", call. = FALSE)
  if(stringr::str_detect(f, "rds$")) d <- readr::read_rds(f)
  if(stringr::str_detect(f, "csv$")) {
    d <- readr::read_csv(f, col_types = readr::cols(), guess_max = 150000)
  }
  d
}

data_remove <- function(type) {
  type <- glue::glue("{type}_[0-9]{{4}}-[0-9]{{2}}-[0-9]{{2}}.(rds|csv)")
  f <- list.files(data_loc(), type, full.names = TRUE)
  if(length(f) == 1) {
    unlink(f)
  } else if (length(f) > 1) {
    stop("More than one file to remove", call. = FALSE)
  }
}

ow_details <- function(ows = NULL) {
  ow_cache <- dplyr::tibble(
    file_local = list.files(glue::glue("{data_cache()}/ow/"), full.names = TRUE),
    ow = stringr::str_extract(basename(.data$file_local), "OW[0-9]{3}"),
    date = stringr::str_extract(basename(.data$file_local),
                                "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
    dplyr::mutate(date = lubridate::as_date(.data$date))
  if(!is.null(ows)) ow_cache <- dplyr::filter(ow_cache,
                                              .data$ow %in% !!ows,
                                              .data$date == Sys.Date())
  ow_cache
}

ow_update <- function(ows) {
  ow_clean()

  dplyr::tibble(ow = ows,
                file_url = glue::glue("http://www.env.gov.bc.ca/wsd/data_searches/",
                                      "obswell/map/data/{.data$ow}-data.csv")) %>%
    dplyr::left_join(ow_details(ows), by = "ow") %>%
    dplyr::mutate(file = dplyr::if_else(!is.na(.data$file_local),
                                        glue::as_glue(.data$file_local),
                                        .data$file_url)) %>%
    dplyr::select(-"file_local", -"file_url", -"date") %>%
    dplyr::mutate(data = purrr::map2(.data$file, .data$ow, ow_read))
}

ow_clean <- function() {
  ow_details() %>%
    dplyr::filter(.data$date != Sys.Date()) %>%
    dplyr::pull(.data$file_local) %>%
    unlink()
}

ow_read <- function(file, ow) {

  well_loc <- dplyr::if_else(stringr::str_detect(file, 'http'), 'online', 'local')
  message(glue::glue("   - {ow} ({well_loc})"))

  d <- try(readr::read_csv(file, col_types = "Tncc", progress = FALSE), silent = TRUE)

  # Try again if errors
  if("try-error" %in% class(d)){
    d <- try(readr::read_csv(file, col_types = "Tncc", progress = FALSE), silent = TRUE)
    if("try-error" %in% class(d)){
      stop("Problem downloading well ", ow, ". Please try again!", call. = FALSE)
    }
  }

  if(well_loc == "online") {
    readr::write_csv(d, glue::glue("{data_cache()}/ow/{ow}_{Sys.Date()}.csv"))
  }
  d
}
