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
  o <- str_extract(ows, "[0-9]{3}")
  if(numeric) o <- as.numeric(o)
  o
}

ow_c <- function(ows) {
  glue("OW{str_pad(ows, '3', pad = 0)}") %>%
    as.character()
}

ow_link <- function(ow) glue("\\hyperref[{tolower(ow)}]{{{ow}}}")
