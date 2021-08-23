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

#scales::show_col(viridis::viridis(n = 7, option = "A"), ncol = 1) # --> No good
#scales::show_col(viridis::viridis(n = 7, option = "B", end = 0.8), ncol = 1) # Okay
#scales::show_col(viridis::viridis(n = 7, option = "C"), ncol = 1) # --> No good
#scales::show_col(viridis::viridis(n = 7, option = "D"), ncol = 1) # Use this one

perc_colours <- viridis::viridis(n = 7, option = "D")

perc_values <- dplyr::tribble(
  ~class,      ~nice,               ~txt_colour, ~low,    ~high,   ~low_show, ~high_show,
  "p_max",     "Maximum",           "white",     0,       0.00001, 1,         1,
  "p_v_high",  "Much Above Normal", "white",     0.00001, 0.10,    0.9,       1,
  "p_m_high",  "Above Normal",      "white",     0.10,    0.25,    0.75,      0.9,
  "p_n",       "Normal",            "black",     0.25,    0.75,    0.25,      0.75,
  "p_m_low",   "Below Normal",      "black",     0.75,    0.9,     0.10,      0.25,
  "p_v_low",   "Much Below Normal", "black",     0.9,     0.99999, 0,         0.10,
  "p_min",     "Minimum",           "black",     0.99999, 1,       0,         0) %>%
  dplyr::mutate(colour = !!perc_colours)

plot_values <- dplyr::tribble(~ type,     ~ size, ~ colour,
                              "Working",  0.75,      "red",
                              "Approved", 0.75,     "black",
                              "Median",   0.5,      "grey50")

type_values <- dplyr::tribble(~code, ~type,
                              "5a",  "Sedimentary",
                              "5b",  "Sedimentary",
                              "4b",  "Confined sand and gravel",
                              "4c",  "Confined sand and gravel",
                              "6a",  "Crystalline bedrock",
                              "6b",  "Crystalline bedrock",
                              "1a",  "Unconfined sand and gravel",
                              "1b",  "Unconfined sand and gravel",
                              "1c",  "Unconfined sand and gravel",
                              "2",   "Unconfined sand and gravel",
                              "3",   "Unconfined sand and gravel",
                              "4a",  "Unconfined sand and gravel")


locs <- readr::read_csv("data-raw/obswell_locations.csv") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::rename(ow = well, region = area, area = subarea) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              ~ stringr::str_remove_all(., "\xa0") %>%
                                stringr::str_trim()))


usethis::use_data(data_types, perc_values, plot_values, type_values, locs,
                  internal = TRUE, overwrite = TRUE)
