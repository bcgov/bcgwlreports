

## Create the folder to save the files if it doesnt exist
dir.create("reports/province_reports", showWarnings = FALSE)


# region_id <- "South Natural Resource Area"
# region_id <- "South Coast Region"
# region_id <- "North Natural Resource Area"
# region_id <- "West Coast Region"
region_id <- c("North Natural Resource Area",
               "South Coast Region",
               "South Natural Resource Area",
               "West Coast Region")


report_title <- paste0(ifelse(length(region_id) == 4, "BC", region_id), " Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions in the ",
                             ifelse(length(region_id) == 4, "Province of BC", region_id),
                             " as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

ows <- sort(bcgwlreports::get_obs_in_area(region_id))

bcgwlreports::well_report(ows,
                          report_dates = c(Sys.Date()),
                          title = report_title,
                          description = report_description,
                          n_days = 14,
                          years_min = 5,
                          out_dir = "reports/province_reports",
                          cache_age = 7,
                          name = paste0( ifelse(length(region_id) == 4, "BC", region_id),
                                         "_", Sys.Date()))
