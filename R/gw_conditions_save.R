# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#' Calculate percentile conditions and save data and plots  in directories
#' @param ows Character vector. Observation well numbers (e.g, "OW000"). Set to NA for all wells (default).
#' @param report_dates Character vector. Only one date for this function. Default Sys.Date().
#' @param n_days Numeric. If there is no data on the report date chosen, this is
#'   the range of days over which to look for alternative dates with data.
#'   Defaults to 2 weeks, meaning 2 weeks before and 2 weeks after a given
#'   report date, for a total window of 4 weeks.
#' @param years_min Numeric. Minimum number of years required to to calculate a
#'   percentiles
#' @param cache_age Logical. Maximum age in days of cached datasets (not obs well
#'   data, but metadata related to regional maps, aquifer and wells).
#' @param csv_dir Folder to save the exported csv file with all gw percentiles
#' @param plots_dir Folder to save all the static plots
#'
#' @export

gw_conditions_save <- function(ows = NA,
                               report_dates = Sys.Date(),
                               n_days = 14,
                               years_min = 5,
                               cache_age = 7,
                               csv_dir = "",
                               plots_dir = ""){

  # ows = NA
  # report_dates = Sys.Date()
  # n_days = 14
  # years_min = 5
  # cache_age = 7
  # Get well latitude and longitude from spatial data
  wells_sf <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
    dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
    dplyr::collect() %>%
    dplyr::mutate(ow = paste0("OW", OBSERVATION_WELL_NUMBER)) %>%
    sf::st_transform(., crs = 4326)%>%
    dplyr::mutate(Latitude = sf::st_coordinates(.)[,1],
                  Longitude = sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(ow, Latitude, Longitude)

  # Get well location information
  data_info_raw <- read.csv("data-raw/obswell_locations.csv", na.strings = "")

  # Get list of wells to sumamrize (must have "Region_Drought" columns in file)
  well_list <- data_info_raw %>%
    dplyr::filter(!is.na(Region_Drought)) %>%
    dplyr::pull(Well)
  if (!is.na(ows[1])) {
    well_list <- well_list[well_list %in% ows]
  }

    # get and calculate percentiles
  gw_data <- gw_data_prep(ows = well_list,
                          n_days = n_days,
                          report_dates = report_dates[1],
                          years_min = years_min,
                          cache_age = cache_age)

  # Merge information and tidy into final form
  details <- gw_data$details_latest %>%
    dplyr::mutate(class = ifelse(is.na(class), "Not Available", class)) %>%
    dplyr::left_join(data_info_raw %>% dplyr::rename(ow = Well), by = "ow") %>%
    dplyr::left_join(wells_sf, by = "ow") %>%
    dplyr::mutate(region_report1 = paste0("http://bcrfc.env.gov.bc.ca/Real-time_Data/Drought_regional_statistics/",
                                          gsub(" ", "_", Region_Drought),"_Groundwater_Report.html"),
                  region_report2 = ifelse(Region_Drought != Region_Drought,
                                          paste0("http://bcrfc.env.gov.bc.ca/Real-time_Data/Drought_regional_statistics/",
                                                 gsub(" ", "_", Region_Drought2),"_Groundwater_Report.html"),
                                          NA),
                  region_report3 = ifelse(!is.na(region_report2),
                                          paste0(region_report1, ", ", region_report2),
                                          region_report1)) %>%
    dplyr::mutate("Interactive Hydrograph" = "insert url",
                  "Static Hydrograph" = "insert url",
                  "Update Time" = Sys.time()) %>%
    dplyr::select("Well ID" = ow, "Well Location" = Location_Long, Latitude, Longitude,
                  Date = date, Depth = value, Percentile = percentile, "Percentile Class" = class, Approval = approval,
                  "Aquifer Type" = aquifer_type, "Hydraulic Connectivity" = hydraulic_connectivity,
                  "Interactive Hydrograph", "Static Hydrograph",
                  "Regional Report" = region_report3, `Update Time`)

  message("- Saving .csv files")
  write.csv(details, file = paste0(csv_dir, "groundwater_conditions.csv"), row.names = FALSE)
  write.csv(details, file = paste0(csv_dir, "groundwater_conditions_", Sys.Date(), ".csv"), row.names = FALSE)


  message("- Creating plots")
  plots <- gw_both_plots(gw_data)

  message("- Saving plots")
  for (p in names(plots)) {
    plot <- plots[[p]]
    ggplot2::ggsave(paste0(plots_dir, p, "_figure.png"),
                    plot = plot,
                    width = 10, height = 9)
  }

  # message("- Creating plots")
  # plots_hist <- gw_percentiles_plot(gw_data)

  # message("- Saving plots")
  # for (p in names(plots)) {
  #   plot <- plots[[p]]
  #   ggplot2::ggsave(paste0(plots_dir, p, "_figure.png"),
  #                   plot = plot,
  #                   width = 10, height = 10)
  # }

  return(list("details" = details,
              "plots" = plots))
}

