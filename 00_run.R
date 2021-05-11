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

source("00_setup.R")

ows <- c("OW008", "OW217", "OW377", "OW197", "OW312", "OW373", "OW430", "OW074",
         "OW236", "OW302", "OW259", "OW211", "OW002", "OW272", "OW075", "OW203",
         "OW299", "OW301",
         "OW337", "OW390", "OW432", "OW262", "OW413", "OW442")

#ows <- c("OW008", "OW217", "OW377", "OW197")

# Check problems
# 201, 008, 301,

# Remove duplicates obs wells

# Default dates are today (Sys.Date()), and two weeks ago (Sys.Date() - weeks(2)).
report_dates <- c(Sys.Date(), Sys.Date() - weeks(2))
report_dates # e.g., "2021-03-23" "2021-03-09"

# Run report including all the above wells
well_report(ows = ows, report_dates = report_dates, within = 7)

# The script finds the dates within 7 days (within) with the most data (i.e the
# most obs wells with data) and uses those.
#
# To get exact dates, use within = 0
#
# All dates are compared to dates from 1 year ago as well.
#
# Therefore the final report will always have 2x as many dates as specified in
# report_dates


