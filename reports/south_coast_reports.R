

library(bcgwlreports)
dir.create("reports/south_coast_reports")


#### Regional Reports

southc_wells <- get_obs_in_area(nr_area = "South Coast Region")

report_title <- paste0("South Coast Region Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the South Coast Region as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

well_report (ows = southc_wells,
             report_dates = c(Sys.Date()),
             title = report_title,
             description = report_description,
             n_days = 14,
             years_min = 5,
             out_dir = "reports/south_coast_reports",
             cache_age = 7,
             name = "south_coast_region")
