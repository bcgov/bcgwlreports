# Copyright 2022 Province of British Columbia
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


#' Launch shiny app
#'
#' @export

library(shiny)
library(shinydashboard)



run_shiny <- function() {

  # Setup -------------------------------------
  options(
    spinner.color = "#003366", spinner.type = 5, spinner.size = 0.5, # Spinners
    scipen=999,                                              # No sci notations
    shiny.launch.browser = .rs.invokeShinyWindowExternal)

  css <- system.file("shiny_app", "www", "bcgov.css", package = "bcgwlreports")
  if(css == "" ) {
    if(file.exists("../../shiny_app/www/bcgov.css")) {
      css <- "../../shiny_app/www/bcgov.css"
    } else css <- ""
  }

  # Get table information -----------------

  wells_locations <- read.csv("data-raw/obswell_locations.csv", stringsAsFactors = FALSE) %>%
    dplyr::arrange(Well)
  #
  # aquifers <- read.csv("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv") %>%
  #   dplyr::select("aquifer_id", "subtype") %>%
  #   dplyr::mutate(subtype = stringr::str_extract(.data$subtype,  "^[1-6]{1}[a-c]{0,1}")) %>%
  #   dplyr::left_join(dplyr::tribble(
  #     ~subtype, ~type,                        ~hydraulic_connectivity,
  #     "1a",     "Unconfined sand and gravel", "Likely",
  #     "1b",     "Unconfined sand and gravel", "Likely",
  #     "1c",     "Unconfined sand and gravel", "Likely",
  #     "2",      "Unconfined sand and gravel", "Likely",
  #     "3",      "Unconfined sand and gravel", "Likely",
  #     "4a",     "Unconfined sand and gravel", "Likely",
  #     "4b",     "Confined sand and gravel",   "Not Likely",
  #     "4c",     "Confined sand and gravel",   "Not Likely",
  #     "5a",     "Sedimentary",                "Not Likely",
  #     "5b",     "Sedimentary",                "Likely",
  #     "6a",     "Crystalline bedrock",        "Not Likely",
  #     "6b",     "Crystalline bedrock",        "Not Likely",
  #     "UNK",    "Unknown",                    "Unknown"
  #   ), by = "subtype")
  #
  # wells_sf <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
  #   dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
  #   dplyr::collect() %>%
  #   dplyr::mutate(ow = ow_c(.data$OBSERVATION_WELL_NUMBER)) %>%
  #   dplyr::select(Well = "ow", "aquifer_id" = "AQUIFER_ID")%>%
  #   dplyr::mutate(Longitude = unlist(purrr::map(.data$geometry,1)),
  #                 Latitude = unlist(purrr::map(.data$geometry,2)))
  #
  # wells_table_sf <- dplyr::right_join(wells_sf, wells_locations, by = "Well") %>%
  #   dplyr::left_join(aquifers, by = "aquifer_id") %>%
  #   dplyr::mutate(type = as.factor(type), subtype = as.factor(subtype),
  #                 hydraulic_connectivity = as.factor(hydraulic_connectivity)) %>%
  #   dplyr::select(Well, Area, Subarea,
  #                 Location,
  #                 Aquifer_Type = type,
  #                 Aquifer_Subtype = subtype, Hydraulic_Connectivity = hydraulic_connectivity,
  #                 Aquifer_ID = aquifer_id,
  #                 Location_Long,#Latitude, Longitude,
  #   )
  # wells_table <- sf::st_drop_geometry(wells_table_sf) %>%
  #   dplyr::mutate(Well = as.character(Well),
  #                 Location_Long = as.character(Location_Long),
  #                 Remarks = NA) %>%
  #   dplyr::arrange(Well)

  wells_table_sf <- readRDS("data-raw/obswell_info.rds") %>%
    dplyr::filter(Well_Number %in% wells_locations$Well) %>%
    dplyr::select(-Report) %>%
    dplyr::rename(Well = Well_Number) %>%
    dplyr::mutate(Aquifer_Type = as.factor(Aquifer_Type),
                  Aquifer_Subtype = as.factor(Aquifer_Subtype),
                  Hydraulic_Connectivity = as.factor(Hydraulic_Connectivity))
  wells_table <- sf::st_drop_geometry(wells_table_sf) %>%
    dplyr::mutate(Well = as.character(Well),
                  Location_Long = as.character(Location_Long),
                  Remarks = NA) %>%
    dplyr::arrange(Well)

  # Other setup -----------------

  maps_points <- list(
    "None" = "None",
    "Aquifer Type" = "Aquifer_Type",
    "Aquifer Subtype" = "Aquifer_Subtype",
    "Hydraulic Connection" = "Hydraulic_Connectivity")


  # ui ------
  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "bcgwlreports"),
      dashboardSidebar(disable = TRUE#,
                       # sidebarMenu(
                       #   id = "menu",
                       #   menuItem("Build Report", tabName = "report", icon = icon("home")),
                       #   menuItem("Data", tabName = "data", icon = icon("table"),
                       #            menuSubItem("Tab 1", tabName = "Sub1"),
                       #            menuSubItem("Tab 2", tabName = "Sub2")))
      ),
      dashboardBody(
        # fluidPage(
        if(css != "") includeCSS(css),
        #    titlePanel("bcgwlreports"),
        #  tabItems(
        #    tabItem("report",
        #     mainPanel(
        fluidRow(
          column(
            width = 12, br(),
            box(
              width = 3,
              helpText("insert text"),
              helpText("can copy paste cells from excel into wells"),
              "fix preview where have to load build tab UI first",
              hr(),
              h4(strong("Wells")),
              uiOutput("wells_selectize"),
              # fluidRow(column(width = 9, uiOutput("region_wells")),
              #          column(width = 3, br(), actionButton("add_reg_wells","Add"))),
              # fluidRow(column(width = 9, uiOutput("subregion_wells")),
              #          column(width = 3, br(), actionButton("add_subreg_wells","Add"))),
              fluidRow(column(width=12, actionButton("clear_all_wells","Clear all wells from list",
                                                     icon = icon("minus", lib = "glyphicon")))),
              br(),
              fluidRow(column(width=12, actionButton("add_table_wells","Add filtered wells from table",
                                                     icon = icon("plus", lib = "glyphicon")))),
              br(),
              fluidRow(column(width=12, actionButton("add_tablesel_wells","Add selected wells from table",
                                                     icon = icon("plus", lib = "glyphicon")))),
              br(),
              fluidRow(column(width=12, actionButton("download_data","Download well water level data",
                                                     icon = icon("download-alt", lib = "glyphicon")))),
              hr(),
              #  h4(strong("Map Options")),
              h4(strong(shinyWidgets::materialSwitch(inputId = "show_map_options",
                                                     label = "Map Options",
                                                     status = "primary",
                                                     value = FALSE))),
              # div(id = "map_options",
              conditionalPanel(
                "input.show_map_options",
                fluidRow(column(width=6,selectInput("point_colour", label = "Well Map Colours:", choices = maps_points))),
                hr()),
              #  h4(strong("Percentile Reporting Options")),
              # h4(strong("Percentile Reporting Options")),
              h4(strong(shinyWidgets::materialSwitch(inputId = "show_reporting_options",
                                                     label = "Percentile Reporting Options",
                                                     status = "primary",
                                                     value = FALSE))),
              conditionalPanel(
                "input.show_reporting_options",
                fluidRow(column(width = 6, dateInput("date_select", "Reporting date:", max = Sys.Date())),
                         column(width = 6, numericInput("window_days", "Date window (+/- days):", value = 14, min = 0, max = 100))),
                h5(verbatimTextOutput("window_dates")),
                checkboxInput("two_weeks", "Compare values to 2 weeks ago", FALSE),
                numericInput("min_years", "Minimum number of years:", value = 5, min = 5, max = 100),
              ),
              hr()

            ),
            tabBox(
              width = 9,

              ## Main Table ---------------------
              tabPanel(
                title = "Table",
                h4("map above, not showing percentiles colours, but other options"),
                h4("LIST WELLS NOT IN OUTPUT"),
                #   leaflet::leafletOutput("wells_map"),
                DT::dataTableOutput("locations"),
                verbatimTextOutput("test")
              ),
              tabPanel(
                title = "Well Summary",
                h4("plotly plots of well data, map with aquifer?")
              ),

              ## Report Prevview ---------------------
              tabPanel(
                title = "Preview Report",
              #  checkboxInput("inc_plots", "Preview plots? (unchecking may save time if viewing just tables)", value = TRUE),
               # actionButton("gen_preview_button", "Preview Report",
              #               icon = icon("eye-open", lib = "glyphicon")),
                br(),
                h2(textOutput("prev_title")),
                textOutput("prev_desc"),
                br(),
                "This report was generated on ", format(Sys.Date(), format = '%B %d, %Y'),".",
                hr(),
                h3("Background"),
                "The province maintains a network of groundwater observation wells to monitor water levels in priority aquifers. These observation wells (OW) record water level fluctuations which allow for improved understanding of how aquifers respond to changes in climate, precipitation, and effects from pumping. Many of the observation wells are equipped with satellite telemetry to provide real time information on water levels.",
                br(),br(),
                "The following summaries compare recent groundwater levels to all historical continuous daily records to determine percentile classes, with a minimum of `r years_min` years of data. Historical monthly water level samples (before ~2004) are not included. A percentile is on a scale of 100 and indicates the percent of a distribution that is equal to or below it. For example, a groundwater level at the 10th percentile is equal to or greater than 10% of the water level values recorded on this day of the year during all previous years of data.",
                br(),br(),
                "In general, a groundwater level value that is:",
                tags$ul(
                  tags$li("the highest ever measured for the day of year is considered **High**"),
                  tags$li("greater than the 90th percentile is considered **Much Above Normal**"),
                  tags$li("between 75th percentile and 90th percentile is considered **Above Normal**"),
                  tags$li("between 25th and 75th percentiles is considered **Normal**"),
                  tags$li("less than the 25 percentile is considered **Below Normal**"),
                  tags$li("less than 10 percentile is considered **Much Below Normal**"),
                  tags$li("the lowest ever measured for the day of year is considered **Low**")
                ),

                hr(),h3("Groundwater Level Conditions"),
                #map
                gt::gt_output("ptile_class_table"),

                hr(),h3("Wells Below Normal"),
                "This section reports on the number and total proportion of wells below normal or lower (i.e. 25th percentile or lower) on a given reporting date and one year prior for comparison. Hydraulic Connectivity and Aquifer Type categories are inferred based on aquifer subtype. Hydraulic connectivity is not field verified.",
                br(),br(),
                tabsetPanel(tabPanel("All Wells", br(),
                                     gt::gt_output("belnorm_all")),
                            tabPanel("By Hydraulic Connectivity", br(),
                                     gt::gt_output("belnorm_hc")),
                            tabPanel("By Aquifer Type", br(),
                                     gt::gt_output("belnorm_aq"))),

                hr(),h3("Latest Details"),
                gt::gt_output("latest_details"),

                hr(),h3("Historical Water Level Plots"),
                "Annual hydrographs and historical records for the observation wells summarized above can be found in this section.",
                br(),br(),
                "Current conditions for provincial groundwater observation wells can be accessed any time through the Groundwater Level Data Interactive Map.",
                br(),br(),
                "Note: ‘Working’ data are preliminary and have not yet been finalized as ‘Approved’ data with approved corrections and data grades for quality assurance. Quality assurance procedures may result in differences between what is displayed as ‘Working’ and what will become the official record.",
                br(),
                uiOutput("well_plot_selected"),
                helpText("put the meta info here"),
                plotOutput("well_plot_ptile_preview"),
                plotOutput("well_plot_record_preview")
              ),
              ## Test ---------------------
              tabPanel(
                title = "Build HTML Report",
                fluidRow(column(width = 6,
                                h4(strong("Report Details")),
                                uiOutput("report_title"),
                                uiOutput("report_description")),
                         column(width = 6,
                                #  actionButton("out_dir2", "Build Report"),
                                h4(strong("Report File")),
                                uiOutput("report_name"),
                                shinyFiles::shinyDirButton('out_dir', 'Select location to save', 'Please select a folder', FALSE),br(),
                                h5("Location:"),
                                verbatimTextOutput("out_dir_print"))),
                br(),
                fluidRow(column(width = 8, actionButton("gen_report_button", "Build and save HTML file",
                                                        icon = icon("floppy-disk", lib = "glyphicon")))),
                br(),br()
              )
            )
          )
        ))
      # tabItem("Sub1", h4("Test")),
      #  tabItem("Sub2", h4("Test2"))
      # )

    ),
    tags$footer(
      div(
        a(href="https://www2.gov.bc.ca/gov/content/home", "Home"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer",
          "Disclaimer"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/accessibility",
          "Accessibility"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/copyright",
          "Copyright"),
        " | ",
        a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html",
          "Contact"), class = "bcgov-footer")

    )
  )


  # server ------
  server <- function(input, output, session) {




    # output$test <- renderText({
    #   data_aquifers()
    # })
    output$test <- renderText({
      # areas_list2()
      # input$well_plot_selected
      #gw_data_wells()$data$details$ow
      input$preview_confirmation
    })

    # Main UI Objects ----------------

    observe(shinyjs::toggle("map_options", condition = input$show_map_options))
    observe(shinyjs::toggle("reporting_options", condition = input$show_reporting_options))

    output$wells_selectize <- renderUI(
      selectizeInput(inputId = "wells_selectize",
                     label = "List of wells to include in report:",
                     choices = wells_locations$Well,
                     multiple = TRUE,
                     options = list(delimiter = " ", create = T))
    )

    output$region_wells <- renderUI(
      selectizeInput(inputId = "region_wells",
                     label = "Region wells to add:",
                     choices = unique(wells_locations$Area),
                     multiple = TRUE)
    )

    observeEvent(input$add_reg_wells, {
      wells_filt <- wells_locations %>%
        dplyr::filter(Area %in% input$region_wells) %>%
        dplyr::pull(Well)
      updateSelectizeInput(session, "wells_selectize",
                           selected = wells_filt)
      updateSelectizeInput(session, "subregion_wells",
                           selected = '')
    })

    observeEvent(input$add_table_wells, {
      updateSelectizeInput(session, "wells_selectize",
                           selected = wells_locations$Well[input$locations_rows_all])
    })
    observeEvent(input$add_tablesel_wells, {
      updateSelectizeInput(session, "wells_selectize",
                           selected = wells_locations$Well[input$locations_rows_selected])
    })
    observeEvent(input$clear_all_wells, {
      updateSelectizeInput(session, "wells_selectize",
                           selected = '')
    })

    output$subregion_wells <- renderUI(
      selectizeInput(inputId = "subregion_wells",
                     label = "Sub Region wells to add:",
                     choices = unique(wells_locations$Subarea),
                     multiple = TRUE)
    )

    observeEvent(input$add_subreg_wells, {
      wells_filt <- wells_locations %>%
        dplyr::filter(Subarea %in% input$subregion_wells) %>%
        dplyr::pull(Well)
      updateSelectizeInput(session, "wells_selectize",
                           selected = wells_filt)
      updateSelectizeInput(session, "region_wells",
                           selected = '')
    })



    observeEvent(input$download_data, {
      shinyWidgets::ask_confirmation(
        inputId = "download_confirmation",
        title = NULL,
        text = tags$b(
          # icon("file"),
          "This may take several minutes. Do you want to continue?",
          style = "color: #00BFFF;"
        ),
        btn_labels = c("No", "Yes"),
        btn_colors = c("#FE2E2E", "#00BFFF"),
        html = TRUE
      )
    })
    gw_download <- observeEvent(input$download_confirmation, {
       req(input$wells_selectize)
      if(input$download_confirmation) {
        showModal(modalDialog("Downloading well water level data. Please be patient, this may take several minutes...", footer=NULL))
        # data <- gw_data_prep(ows = input$wells_selectize,
        #                      report_dates = ,
        #                      n_days = input$window_days,
        #                      years_min = input$min_years,
        #                      cache_age = 7)
        data <-  ow_update(input$wells_selectize)
        removeModal()
      }
    })

    # Main Table----------------

    output$locations <- DT::renderDataTable({
      wells_table %>%
        dplyr::mutate(NR_Area = as.factor(NR_Area), NR_Subarea = as.factor(NR_Subarea)) %>%
        dplyr::select(Well, NR_Area, NR_Subarea, Location,
                      Aquifer_Type, Hydraulic_Connectivity, Remarks, Aquifer_Subtype, Aquifer_ID, Location_Long,
                      Latitude, Longitude, Well_Status,
                      dplyr::everything()) %>%
        DT::datatable(rownames = FALSE,
                      filter = 'top',
                      extensions = c("Scroller", "Buttons"),
                      selection = "multiple",
                      editable = list(target = "column", disable = list(columns = c(0:4))),
                      options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                     deferRender = TRUE, dom = 'Brtip',
                                     buttons = list(list(extend = 'copy', title = NULL),
                                                    'csv', 'excel')))
    })

    # Report Objects----------------

    areas_list <- reactive({
      # levels(areas_list())[unique(wells_locations$Area[input$locations_rows_all])]
      #as.character(unique(wells_locations$Area[input$locations_rows_all]))[1]#[unique(wells_locations$Area[input$locations_rows_all])]
      #  as.numeric(unique(wells_locations$Area[input$locations_rows_all]))
      #  as.character(unique(wells_locations$Area[input$locations_rows_all]))[as.integer(unique(wells_locations$Area[input$locations_rows_all]))]
      unique(as.character(wells_table$NR_Area[input$locations_rows_all]))
      #  input$locations_rows_all

    })

    output$report_title <- renderUI({
      # if (is.null(input$region_wells)) {
      #   title <- "Provide title, ex. Groundwater Level Conditions"
      # } else if (length(input$region_wells) == 1) {
      #   title <- paste0(input$region_wells, " Groundwater Level Conditions")
      # } else {
      #   title <- "BC Groundwater Level Conditions"
      # }
      if (length(areas_list()) == 4) {
        title <- "BC Groundwater Level Conditions"
      } else if (length(areas_list()) == 1) {
        title <- paste0(areas_list(), " Groundwater Level Conditions")
      } else {
        title <- "BC Groundwater Level Conditions"
      }

      textInput("report_title",
                "Report title:",
                value =  title)
    })
    output$report_description <- renderUI({
      # if (is.null(input$region_wells)) {
      #   report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
      #                                "in various observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      # } else if (length(input$region_wells) == 1) {
      #   report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
      #                                "in the ", input$region_wells," as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      # } else {
      #   report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
      #                                "in various observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      # }

      if (length(unique(wells_table$NR_Subarea[input$locations_rows_all])) == 1) {
        Subarea <- paste0(unique(wells_table$NR_Subarea[input$locations_rows_all]), " region of the ")
      } else {
        Subarea <- ""
      }

      if (length(areas_list()) == 4) {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in BC observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      } else if (length(areas_list()) == 1) {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in the ", Subarea, areas_list()," as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      } else {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in BC observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      }

      textAreaInput("report_description",
                    "Report description:",
                    value =  report_description, rows = 2)
    })
    output$report_name <- renderUI({
      # if (is.null(input$region_wells)) {
      #   title <- "Provide title, ex. Groundwater Level Conditions"
      # } else if (length(input$region_wells) == 1) {
      #   title <- paste0(input$region_wells, " Groundwater Level Conditions")
      # } else {
      #   title <- "BC Groundwater Level Conditions"
      # }
      if (length(areas_list()) == 4) {
        title <- paste0("report_", Sys.Date())
      } else if (length(areas_list()) == 1) {
        title <- paste0(areas_list(), "_", Sys.Date())
      } else {
        title <- paste0("report_", Sys.Date())
      }

      textInput("report_name",
                "HTML report file name:",
                value =  title)
    })

    output$window_dates <- renderPrint({

      if (input$date_select + input$window_days > Sys.Date()) {
        max_date <- Sys.Date()
      } else {
        max_date <- input$date_select + input$window_days
      }

      if (input$window_days > 0) {
        glue::glue("Date Range: {format(input$date_select - input$window_days, format = '%B %d, %Y')} to {format(max_date, format = '%B %d, %Y')}")
      } else {
        glue::glue("Date Range: {format(input$date_select, format = '%B %d, %Y')}")
      }
    })

    # volumes <- shinyFiles::getVolumes()
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    #  shinyFiles::shinyDirChoose(input, 'out_dir', roots=volumes, filetypes=c('', 'txt')) #c(wd='.')
    shinyFiles::shinyDirChoose(input, "out_dir", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
    # observe({
    #   shinyFiles::shinyDirChoose(input, "out_dir", roots = volumes, session = session)
    #   if(!is.null(input$out_dir)){
    #     myOutput1 <- shinyFiles::parseFilePaths(c(home = '~'),input$out_dir)
    #     myblue <- path.expand(myOutput1) #myblue isthen my file path that I can use in my function
    #   }
    # })

    output$prev_title <- renderText({
      input$report_title
    })
    output$prev_desc <- renderText({
      input$report_description
    })

    output$out_dir_print <- renderPrint({
      #  paste0(out_dir(),"/",input$report_name,".html")
      out_dir()
    })

    out_dir <- reactive({
      if (is.integer(input$out_dir)) {
        cat("No directory has been selected.")
      } else {
        shinyFiles::parseDirPath(volumes, input$out_dir)
      }
    })

    observeEvent(input$gen_report_button, {
      shinyWidgets::ask_confirmation(
        inputId = "build_confirmation",
        title = NULL,
        text = tags$b(
          # icon("file"),
          "This may take several minutes. Do you want to continue?",
          style = "color: #00BFFF;"
        ),
        btn_labels = c("No", "Yes"),
        btn_colors = c("#FE2E2E", "#00BFFF"),
        html = TRUE
      )
    })

    observeEvent(input$build_confirmation, {
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'Thank you for clicking')
      req(input$build_confirmation, input$wells_selectize, input$out_dir)
      if(input$build_confirmation) {
        showModal(modalDialog("Building HTML report file. Please be patient, this may take several minutes...", footer=NULL))
        well_report(ows = input$wells_selectize,
                    report_dates = input$date_select,
                    title = input$report_title,
                    description = input$report_description,
                    n_days = input$window_days,
                    years_min = input$min_years,
                    out_dir = out_dir(),
                    cache_age = 7,
                    name = input$report_name)
        removeModal()
        showModal(modalDialog(HTML(paste0("Finished! Your report can be found here:.<br>",
                                          out_dir(),"/",input$report_name,".html"))))
      }
    })


    # Preview Objects----------------

    observeEvent(input$gen_preview_button, {
      shinyWidgets::ask_confirmation(
        inputId = "preview_confirmation",
        title = NULL,
        text = tags$b(
          # icon("file"),
          "This may take several minutes. Do you want to continue?",
          style = "color: #00BFFF;"
        ),
        btn_labels = c("No", "Yes"),
        btn_colors = c("#FE2E2E", "#00BFFF"),
        html = TRUE
      )
    })


    gw_data_wells <- eventReactive(input$preview_confirmation, {
       req(input$wells_selectize)
      if(input$preview_confirmation) {
        showModal(modalDialog("Building tables and plots. Please be patient, this may take several minutes...", footer=NULL))
        data <- gw_data_prep(ows = input$wells_selectize,
                             report_dates = input$date_select,
                             n_days = input$window_days,
                             years_min = input$min_years,
                             cache_age = 7)

        if (input$inc_plots) {
        data_all <- list( gw_data_prep = data,
                          ptile_class_table = gw_percentile_class_table(data, gt = TRUE),
                          belnorm_all = gw_wells_below_normal_table(data, gt = TRUE, which = "totals"),
                          belnorm_hc = gw_wells_below_normal_table(data, gt = TRUE, which = "hydraulic_connectivity"),
                          belnorm_aq = gw_wells_below_normal_table(data, gt = TRUE, which = "type"),
                          latest_details = gw_percentiles_details_table(data, gt = TRUE),
                          well_plot_ptile_preview = gw_percentiles_plot(data),
                          well_plot_record_preview = gw_historic_data_plot(data))
        } else {
          data_all <- list( gw_data_prep = data,
                            ptile_class_table = gw_percentile_class_table(data, gt = TRUE),
                            belnorm_all = gw_wells_below_normal_table(data, gt = TRUE, which = "totals"),
                            belnorm_hc = gw_wells_below_normal_table(data, gt = TRUE, which = "hydraulic_connectivity"),
                            belnorm_aq = gw_wells_below_normal_table(data, gt = TRUE, which = "type"),
                            latest_details = gw_percentiles_details_table(data, gt = TRUE),
                            well_plot_ptile_preview = NULL,
                            well_plot_record_preview = NULL)
        }

        removeModal()
        data_all
      }
    })

    output$ptile_class_table <- gt::render_gt({
      # gw_percentile_class_table(gw_data_wells(), gt = TRUE)
      req()
      gw_data_wells()$ptile_class_table
    })

    output$belnorm_all <- gt::render_gt({
      #  gw_wells_below_normal_table(gw_data_wells(), gt = TRUE, which = "totals")
      gw_data_wells()$belnorm_all
    })
    output$belnorm_hc <- gt::render_gt({
      # gw_wells_below_normal_table(gw_data_wells(), gt = TRUE, which = "hydraulic_connectivity")
      gw_data_wells()$belnorm_hc
    })
    output$belnorm_aq <- gt::render_gt({
      #gw_wells_below_normal_table(gw_data_wells(), gt = TRUE, which = "type")
      gw_data_wells()$belnorm_aq
    })
    output$latest_details <- gt::render_gt({
      #gw_percentiles_details_table(gw_data_wells(), gt = TRUE)
      gw_data_wells()$latest_details
    })


    output$well_plot_selected <- renderUI({
      selectInput("well_plot_selected",
                  "Well to Plot:",
                  choices = sort(gw_data_wells()$gw_data_prep$details$ow))
    })

    #  plot_ptile_preview <- reactive({
    #   gw_percentiles_plot(gw_data_wells())
    #})
    output$well_plot_ptile_preview <- renderPlot({
      req(input$well_plot_selected)
      #    plot_ptile_preview()[[input$well_plot_selected]]
      gw_data_wells()$well_plot_ptile_preview[[input$well_plot_selected]]
    })

    #  plot_record_preview <- reactive({
    #    gw_historic_data_plot(gw_data_wells())
    #  })
    output$well_plot_record_preview <- renderPlot({
      req(input$well_plot_selected)
      #    plot_record_preview()[[input$well_plot_selected]]
      # gw_historic_data_plot(gw_data_wells(), ows = input$well_plot_selected)[[1]]
      gw_data_wells()$well_plot_record_preview[[input$well_plot_selected]]
    })

  }

  # output$remarks <- renderTable({
  #  wells_locations$Area[input$locations_rows_all]
  # })


  # output$wells_map <- leaflet::renderLeaflet({
  #   # req(input$point_colour)
  #
  #   l <- leaflet::leaflet() %>%
  #
  #     # base maps
  #     leaflet::addTiles(group = "OpenStreetMap") %>%
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Stamen.Terrain, group = "Stamen (Terrain)") %>%
  #
  #     # Map panes for vertical sorting
  #     leaflet::addMapPane("points", zIndex = 430) %>%
  #     leaflet::addMapPane("polygons", zIndex = 410) %>%
  #
  #     # Stations
  #     leaflet::addCircleMarkers(data = wells_table_sf,
  #                               #group = "points",
  #                              # options = leaflet::pathOptions(pane = "points"),
  #                               lng = ~Longitude, lat = ~Latitude,
  #                               layerId = ~Well,
  #                               radius = 6, fillOpacity = 1, stroke = TRUE,
  #                               #fillColor = fill,
  #                               color = "black", opacity = 1, weight = 1,
  #                               label = ~purrr::map(glue::glue(
  #                                 "<strong>{stringr::str_to_title(Location_long)}</strong><br>",
  #                                 "<strong>Station ID</strong>: {Well}<br>"), HTML)) %>%
  #     # add_markers(data = wells_table_sf) %>%
  #     #  add_markers(data = wells_table_sf, variable = input$point_colour) %>%
  #     #  add_markers(data = wells_table_sf, variable = "selected") %>%
  #     #   leaflet::addLegend(
  #     #     "bottomright", pal = leaflet::colorFactor("viridis", domain = wells_table_sf[[input$point_colour]]),
  #     #     values = wells_table_sf[[input$point_colour]],
  #     #     title = names(maps_points[maps_points == input$point_colour])) %>%
  #     #
  #     # Controls
  #     leaflet::addLayersControl(
  #       baseGroups = c("Stamen (Terrain)", "OpenStreetMap"),
  #       # overlayGroups = bc_maps_labs$group,
  #       options = leaflet::layersControlOptions(collapsed = FALSE)
  #     )
  #
  #
  #   # Hide all polygons
  #   #  for(i in bc_maps_labs$group) l <- leaflet::hideGroup(l, i)
  #
  #   # for(i in seq_along(bc_maps_layers)) {
  #   #   l <- l %>%
  #   #     leaflet::addPolygons(
  #   #       options = leaflet::pathOptions(pane = "polygons"),
  #   #       data = bc_maps_layers[[i]], group = bc_maps_labs$group[[i]],
  #   #       stroke = 0.5, opacity = 1, weight = 1,
  #   #       fillOpacity = 0.15, fillColor = "black", color = "black",
  #   #       label = bc_maps_labs$label[[i]])
  #   # }
  #
  #   # if (watershed_exists()) {
  #   #   l <- l %>% leaflet::addPolygons(data = watershed())
  #   # }
  #   # map_ready(TRUE)
  #   l
  # })
  # observe({
  #  # req(input$point_colour)
  #   leaflet::leafletProxy("hydat_map") %>%
  #     leaflet::clearGroup("points") %>%
  #     add_markers(data = stations_sub(), variable = input$point_colour) %>%
  #     add_markers(data = stations_sub(), variable = "selected")
  # }) %>%
  #   bindEvent(stations_sub())

  shinyApp(ui = ui, server = server)
}
