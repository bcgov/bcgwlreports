# Load the package

time1 <- Sys.time()

# Get the well point data from BC data warehouse
# ----------

wells_spatial <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
  dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
  dplyr::collect() %>%
  dplyr::mutate(ow = ow_c(.data$OBSERVATION_WELL_NUMBER),
                Longitude = unlist(purrr::map(.data$geometry,1)),
                Latitude = unlist(purrr::map(.data$geometry,2))) %>%
  dplyr::select(Well_Number = "ow", Well_Status = OBSERVATION_WELL_STATUS, Aquifer_ID = AQUIFER_ID, Longitude, Latitude)

# Get the well nr areas and location descriptions for reporting wells
# ----------

wells_locations <- read.csv("data-raw/obswell_locations.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(Well_Number = Well, NR_Area = Area, NR_Subarea = Subarea,
                Location = Location, Location_Long = Location_Long) %>%
  dplyr::mutate(Report = TRUE) %>%
  dplyr::arrange(Well_Number)

# Get the squifer information from GWELLS
# ----------

aquifers <- read.csv("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv") %>%
  dplyr::select(Aquifer_ID = "aquifer_id", Aquifer_Subtype = "subtype") %>%
  dplyr::mutate(Aquifer_Subtype = stringr::str_extract(.data$Aquifer_Subtype,  "^[1-6]{1}[a-c]{0,1}")) %>%
  dplyr::left_join(dplyr::tribble(
    ~Aquifer_Subtype, ~Aquifer_Type,                        ~Hydraulic_Connectivity,
    "1a",     "Unconfined sand and gravel", "Likely",
    "1b",     "Unconfined sand and gravel", "Likely",
    "1c",     "Unconfined sand and gravel", "Likely",
    "2",      "Unconfined sand and gravel", "Likely",
    "3",      "Unconfined sand and gravel", "Likely",
    "4a",     "Unconfined sand and gravel", "Likely",
    "4b",     "Confined sand and gravel",   "Not Likely",
    "4c",     "Confined sand and gravel",   "Not Likely",
    "5a",     "Sedimentary",                "Not Likely",
    "5b",     "Sedimentary",                "Likely",
    "6a",     "Crystalline bedrock",        "Not Likely",
    "6b",     "Crystalline bedrock",        "Not Likely",
    "UNK",    "Unknown",                    "Unknown"
  ), by = "Aquifer_Subtype")



# Download all daily data to get start/end dates
# ----------

# Download data
well_data_all <- read.csv("https://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/ObservationWellDataAll_DailyMean.csv")%>%
  dplyr::select(Well_Number = myLocation, Date = QualifiedTime, Value)

# Add dates and determine if continuous or not
well_data_all_more <- well_data_all %>%
  fasstr::fill_missing_dates(water_year_start = 10, groups = Well_Number) %>%
  fasstr::add_date_variables(water_year_start = 10)
well_data_all_more <- bind_rows(lapply(unique(well_data_all_more$Well_Number), function(ow){
  dplyr::filter(well_data_all_more, Well_Number == ow) %>%
    find_continuous()
}))

# Determine dates for all, monthly, and continuous periods
well_all_dates <- well_data_all_more %>%
  dplyr::filter(complete.cases(Value)) %>%
  dplyr::group_by(Well_Number) %>%
  dplyr::summarise(Start_Year = min(CalendarYear, na.rm = TRUE),
                   End_Year = max(CalendarYear, na.rm = TRUE),
                   N_Years = length(unique(CalendarYear)),
                   Start_WaterYear = min(WaterYear, na.rm = TRUE),
                   End_WaterYear = max(WaterYear, na.rm = TRUE),
                   N_WaterYears = length(unique(WaterYear)))

well_daily_dates <- well_data_all_more %>%
  dplyr::filter(continuous_data, complete.cases(Value)) %>%
  dplyr::group_by(Well_Number) %>%
  dplyr::summarise(Start_Year_Continuous = min(CalendarYear, na.rm = TRUE),
                   End_Year_Continuous = max(CalendarYear, na.rm = TRUE),
                   N_Years_Continuous = length(unique(CalendarYear)),
                   Start_WaterYear_Continuous = min(WaterYear, na.rm = TRUE),
                   End_WaterYear_Continuous = max(WaterYear, na.rm = TRUE),
                   N_WaterYears_Continuous = length(unique(WaterYear)))

well_Monthly_dates <- well_data_all_more %>%
  dplyr::filter(!continuous_data, complete.cases(Value)) %>%
  dplyr::group_by(Well_Number) %>%
  dplyr::summarise(Start_Year_Monthly = min(CalendarYear, na.rm = TRUE),
                   End_Year_Monthly = max(CalendarYear, na.rm = TRUE),
                   N_Years_Monthly = length(unique(CalendarYear)),
                   Start_WaterYear_Monthly = min(WaterYear, na.rm = TRUE),
                   End_WaterYear_Monthly = max(WaterYear, na.rm = TRUE),
                   N_WaterYears_Monthly = length(unique(WaterYear)))

well_dates <- dplyr::left_join(well_all_dates, well_Monthly_dates, by = "Well_Number") %>%
  dplyr::left_join(well_daily_dates, by = "Well_Number")


# Join all the info together
# ----------

obswell_info_table_sf <- dplyr::left_join(wells_spatial, wells_locations, by = "Well_Number") %>%
  dplyr::mutate(Report = ifelse(is.na(Report), FALSE, TRUE)) %>%
  dplyr::left_join(aquifers, by = "Aquifer_ID") %>%
  dplyr::left_join(well_dates, by = "Well_Number") %>%
  dplyr::select(Well_Number, Well_Status, NR_Area, NR_Subarea, Location, Location_Long,
                Latitude, Longitude,
                Aquifer_ID, Aquifer_Subtype, Aquifer_Type, Hydraulic_Connectivity,
                dplyr::everything())

obswell_info_table <- obswell_info_table_sf %>%
  sf::st_drop_geometry()

# Save the data into "data-raw"
# ----------

saveRDS(obswell_info_table_sf, "data-raw/obswell_info.rds")
write.csv(obswell_info_table, "data-raw/obswell_info_table.csv", row.names = FALSE)

# -----

time2 <- Sys.time()
time_length <- time2 - time1
time_length
