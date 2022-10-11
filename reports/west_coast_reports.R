

## Create the folder to save the files if it doesnt exist
dir.create("reports/west_coast_reports", showWarnings = FALSE)


#### Regional Reports

westc_wells <- bcgwlreports::get_obs_in_area(nr_area = "West Coast Region")

report_title <- paste0("West Coast Region Groundwater Level Conditions")

report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                             "in the West Coast Region as of ", format(Sys.Date(), format = "%B %d, %Y"), ".")

bcgwlreports::well_report (ows = westc_wells,
                           report_dates = c(Sys.Date()),
                           title = report_title,
                           description = report_description,
                           n_days = 14,
                           years_min = 5,
                           out_dir = "reports/west_coast_reports",
                           cache_age = 7,
                           name = paste0("west_coast_region_", Sys.Date()))




#
# westc_wells <- c('OW058', 'OW060', 'OW071', 'OW125', 'OW128', 'OW196', 'OW197',
#                  'OW201', 'OW204', 'OW211', 'OW232', 'OW233', 'OW240', 'OW258',
#                  'OW265', 'OW268', 'OW281', 'OW283', 'OW284', 'OW287', 'OW288',
#                  'OW290', 'OW295', 'OW303', 'OW304', 'OW310', 'OW312', 'OW314',
#                  'OW316', 'OW319', 'OW320', 'OW321', 'OW327', 'OW329', 'OW337',
#                  'OW338', 'OW340', 'OW343', 'OW345', 'OW351', 'OW355', 'OW369',
#                  'OW371', 'OW372', 'OW373', 'OW383', 'OW385', 'OW388', 'OW389',
#                  'OW390', 'OW391', 'OW392', 'OW393', 'OW394', 'OW395', 'OW396',
#                  'OW397', 'OW398', 'OW424', 'OW425', 'OW426', 'OW428', 'OW429',
#                  'OW430', 'OW431', 'OW432', 'OW433', 'OW434', 'OW435', 'OW436',
#                  'OW437')
