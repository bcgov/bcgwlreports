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

data_types <- c("wells_sf", "aquifers", "regions")

data_update <- function(cache_age = 7, type = NULL) {
  d <- data_retrieve(cache_age)
  if(!is.null(type)) d <- filter(type %in% !!type)

  for(i in seq_len(nrow(d))) {
    if(d$update[i]) {
      get(glue("data_{d$type[i]}"))()
    } else message(glue("Skipping {d$type[i]} data"))
  }
}

data_wells_sf <- function() {
  message("Downloading Spatial Observation well data")
  data_remove("wells_sf")
  bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
    filter(!is.na(OBSERVATION_WELL_NUMBER)) %>%
    collect() %>%
    mutate(ow = ow_c(OBSERVATION_WELL_NUMBER)) %>%
    select(ow, aquifer_id = AQUIFER_ID) %>%
    write_rds(glue("data/wells_sf_{Sys.Date()}.rds"))
}

data_aquifers <- function() {

  message("Downloading Aquifer Data")
  data_remove("aquifers")
  GET("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv",
      write_disk(glue("./data/aquifers.csv"), overwrite = TRUE),
      progress())

  data_load("aquifers") %>%
    select(aquifer_id, subtype) %>%
    mutate(subtype = str_extract(subtype, "^[1-6]{1}[a-c]{0,1}")) %>%
    left_join(type_values, by = c("subtype" = "code")) %>%
    select(-subtype) %>%
    right_join(data_load("wells_sf") %>% st_drop_geometry(), by = "aquifer_id") %>%
    write_rds(glue("data/aquifers_{Sys.Date()}.rds"))
  unlink("data/aquifers.csv")
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
  a <- nr_areas(ask = FALSE) %>%
    select(area_name = AREA_NAME)

  r <- nr_regions(ask = FALSE) %>%
    select(region_name = REGION_NAME) %>%
    st_join(a, largest = TRUE)

  d <- nr_districts(ask = FALSE) %>%
    select(district_name = DISTRICT_NAME) %>%
    st_join(r, largest = TRUE)
  write_rds(d, glue("data/regions_{Sys.Date()}.rds"))
}




data_retrieve <- function(cache_age) {
  tibble(f = list.files("data"),
         date = str_extract(f, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
         type = str_remove_all(f, glue("(_{date})|(.csv)|(.rds)"))) %>%
    mutate(type = factor(type, levels = data_types)) %>%
    mutate(update = if_else(Sys.Date() - days(cache_age) > date, TRUE, FALSE)) %>%
    complete(type, fill = list(update = TRUE)) %>%
    arrange(type)
}

data_load <- function(type) {
  f <- list.files("data", type, full.names = TRUE)
  if(str_detect(f, "rds$")) d <- read_rds(f)
  if(str_detect(f, "csv$")) d <- read_csv(f, col_types = cols(), guess_max = 150000)
  d
}

data_remove <- function(type) {
  type <- glue("{type}_[0-9]{{4}}-[0-9]{{2}}-[0-9]{{2}}.(rds|csv)")
  f <- list.files("data", type, full.names = TRUE)
  if(length(f) == 1) {
    unlink(f)
  } else if (length(f) > 1) {
    stop("More than one file to remove", call. = FALSE)
  }
}
