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

perc_values <- dplyr::tribble(
  ~class,      ~nice,               ~colour,  ~txt_colour, ~low,    ~high,   ~low_show, ~high_show,
  "p_max",     "Maximum",           "black",  "white",     0,       0.00001, 1,         1,
  "p_v_high",  "Much Above Normal", "blue",   "white",     0.00001, 0.10,    0.9,       1,
  "p_m_high",  "Above Normal",      "cyan",   "black",     0.10,    0.25,    0.75,      0.9,
  "p_n",       "Normal",            "green",  "black",     0.25,    0.75,    0.25,      0.75,
  "p_m_low",   "Below Normal",      "yellow", "black",     0.75,    0.9,     0.10,      0.25,
  "p_v_low",   "Much Below Normal", "red",    "black",     0.9,     0.99999, 0,         0.10,
  "p_min",     "Minimum",           "orange", "black",     0.99999, 1,       0,         0)

plot_values <- dplyr::tribble(~ type,     ~ size, ~ colour,
                              "Working",  0.5,      "red",
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


usethis::use_data(data_types, perc_values, plot_values, type_values,
                  internal = TRUE, overwrite = TRUE)
