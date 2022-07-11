
### GAP FILL SMALL GAPS???
####### test 400 and 296
### add precipitation plots for version 2
### add custom hydraulic connectivity in raw_table ()



## Create the folder to save the files if it doesnt exist
dir.create("reports/south_area_reports", showWarnings = FALSE)


#### OBWB Water Support Seminar (list provided)

south_wells <- c("OW217",
                 "OW306", "OW074", "OW291", "OW309", "OW362", "OW363", "OW468", "OW444",
                 "OW185", "OW405",
                 "OW296", "OW381", "OW400", "OW180", "OW294", "OW409",
                 "OW236", "OW356", "OW442", "OW282", "OW403", "OW407", "OW075",
                 "OW172","OW402","OW412")



report_title <- paste0("South Area Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the South Area as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = south_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/south_area_reports",
                           cache_age = 7,
                           name = "south_area_OBWB")




#### Regional Reports (list provided)

south_wells <- c('OW400', 'OW344', 'OW296', 'OW185', 'OW381', 'OW422', 'OW180',
                 'OW409', 'OW294', 'OW384', 'OW356', 'OW442', 'OW236', 'OW203',
                 'OW262', 'OW282', 'OW403', 'OW407', 'OW075', 'OW074', 'OW291',
                 'OW468', 'OW309', 'OW362', 'OW363', 'OW306', 'OW444', 'OW217')

report_title <- paste0("South Area Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the South Area as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = south_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/south_area_reports",
                           cache_age = 7,
                           name = "south_area_regional")



## Kootenay-Boundary Wells (filtered for stations)

# kb_wells <- c("OW074", # Central Kootenay
#               "OW185", "OW302", "OW309", "OW365", "OW381", "OW464", #Columbia-Shuswap
#               "OW291", "OW362", "OW363", "OW468", #East Kootenay
#               "OW217", "OW306", "OW444" #Kootenay Boundary
# )

kb_wells <- c('OW074', 'OW291', 'OW468', 'OW309', 'OW362', 'OW363', #Kootenay
              'OW306', 'OW444', 'OW217') # Boundary


report_title <- paste0("Kootenay-Boundary Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the Kootenay-Boundary as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = kb_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/south_area_reports",
                           cache_age = 7,
                           name = "Kootenay-Boundary_drought_reports")


## Thompson-Okanagan Wells (filtered for stations)

# to_wells <- c('OW115', 'OW172', 'OW236', 'OW262', 'OW356', 'OW410', 'OW411', 'OW413', 'OW442', #Central Okanagan
#               'OW047', 'OW117', 'OW118', 'OW122', 'OW180', 'OW294', 'OW311', 'OW384', 'OW409', 'OW487', #North Okanagan
#               'OW075', 'OW154', 'OW203', 'OW264', 'OW282', 'OW332', 'OW387', 'OW401', #Similkameen
#               'OW402', 'OW403', 'OW404', 'OW405', 'OW407', 'OW412', 'OW467', #Similkameen
#               'OW035', 'OW045', 'OW080', 'OW296', 'OW344', 'OW375', 'OW399', 'OW400', 'OW422', 'OW423', 'OW494' #Thompson-Nicola
# )

to_wells <- c('OW400', 'OW344', 'OW296', 'OW185', 'OW381', 'OW422', #Thompson
              'OW180', 'OW409', 'OW294', 'OW384', #North Okanagan
              'OW356', 'OW442', 'OW236', # Central Okanagan
              'OW203', 'OW262', 'OW282', 'OW403', 'OW407',
              'OW075') # Similkameen


report_title <- paste0("Thompson-Okanagan Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the Thompson-Okanagan as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = to_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/south_area_reports",
                           cache_age = 7,
                           name = "Thompson-Okanagan_drought_reports")

