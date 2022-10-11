

## Create the folder to save the files if it doesnt exist
dir.create("reports/north_area_reports", showWarnings = FALSE)


#### Regional Reports

north_wells <- bcgwlreports::get_obs_in_area(nr_area = "North Natural Resource Area")

report_title <- paste0("North Area Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the North Natural Resource Area as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = north_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/north_area_reports",
                           cache_age = 7,
                           name = paste0("north_area_", Sys.Date()))


