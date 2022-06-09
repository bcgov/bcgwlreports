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

  wells_locations <- read.csv("data-raw/obswell_locations.csv", stringsAsFactors = TRUE) %>%
    dplyr::arrange(Well)

  aquifers <- read.csv("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv") %>%
    dplyr::select("aquifer_id", "subtype") %>%
    dplyr::mutate(subtype = stringr::str_extract(.data$subtype,  "^[1-6]{1}[a-c]{0,1}")) %>%
    dplyr::left_join(dplyr::tribble(
      ~subtype, ~type,                        ~hydraulic_connectivity,
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
    ), by = "subtype")

  wells_sf <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
    dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
    dplyr::collect() %>%
    dplyr::mutate(ow = ow_c(.data$OBSERVATION_WELL_NUMBER)) %>%
    dplyr::select(Well = "ow", "aquifer_id" = "AQUIFER_ID")%>%
    dplyr::mutate(Longitude = unlist(purrr::map(.data$geometry,1)),
                  Latitude = unlist(purrr::map(.data$geometry,2)))

  wells_table_sf <- dplyr::right_join(wells_sf, wells_locations, by = "Well") %>%
    dplyr::left_join(aquifers, by = "aquifer_id") %>%
    dplyr::mutate(type = as.factor(type), subtype = as.factor(subtype),
                  hydraulic_connectivity = as.factor(hydraulic_connectivity)) %>%
    dplyr::select(Well, Area, Subarea,
                  Location,
                  Aquifer_Type = type,
                  Aquifer_Subtype = subtype, Hydraulic_Connection = hydraulic_connectivity,
                  Aquifer_ID = aquifer_id,
                  Location_Long#,Latitude, Longitude
    )
  wells_table <- sf::st_drop_geometry(wells_table_sf)

  # Other setup -----------------

  maps_points <- list(
    "None" = "None",
    "Aquifer Type" = "Aquifer_Type",
    "Aquifer Subtype" = "Aquifer_Subtype",
    "Hydraulic Connection" = "Hydraulic_Connection")




  # Define UI for application that draws a histogram
  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "bcgwlreports"),
      dashboardSidebar(sidebarMenu(
        id = "menu",
        menuItem("Build Report", tabName = "report", icon = icon("home")),
        menuItem("Data", tabName = "data", icon = icon("table"),
                 menuSubItem("Tab 1", tabName = "Sub1"),
                 menuSubItem("Tab 2", tabName = "Sub2")))),
      dashboardBody(
        if(css != "") includeCSS(css),
        tabItems(
          tabItem("report",
                  fluidRow(
                    column(
                      width = 12, h2("Title 1"),
                      box(
                        width = 3,
                        helpText("insert text"),
                        hr(),
                        h4(strong("Wells")),
                        uiOutput("wells_selectize"),
                        # fluidRow(column(width = 9, uiOutput("region_wells")),
                        #          column(width = 3, br(), actionButton("add_reg_wells","Add"))),
                        # fluidRow(column(width = 9, uiOutput("subregion_wells")),
                        #          column(width = 3, br(), actionButton("add_subreg_wells","Add"))),
                        actionButton("add_table_wells","Add Wells from Table"),
                        actionButton("clear_all_wells","Remove all Wells"),
                        selectInput("point_colour", label = "Well Map Colours:",
                                    choices = maps_points),
                        hr(),
                        h4(strong("Options")),
                        fluidRow(column(width = 6, dateInput("date_select", "Reporting date:", max = Sys.Date())),
                                 column(width = 6, numericInput("window_days", "Date window (+/- days):", value = 13, min = 0, max = 100))),
                        h5(textOutput("window_dates")),
                        checkboxInput("two_weeks", "Compare values to 2 weeks ago", FALSE),
                        numericInput("min_years", "Minimum number of years:", value = 5, min = 5, max = 100),

                        hr(),
                        h4(strong("Report Details")),
                        uiOutput("report_title"),
                        uiOutput("report_description"),
                        #  actionButton("out_dir2", "Build Report"),
                        shinyFiles::shinyDirButton('out_dir', 'Location to save', 'Please select a folder', FALSE),
                        actionButton("gen_report_button", "Build Report")



                        # ows,
                        # name = "report",
                        # report_dates = Sys.Date(),
                        # title = NULL,
                        # description = NULL,
                        # remarks = NULL,
                        # n_days = 13,
                        # years_min = 5,
                        # out_dir = ".",
                        # cache_age = 7
                      ),
                      tabBox(
                        width = 9,

                        ## Plot ---------------------
                        tabPanel(
                          title = "Table",
                          h4("map above, not showing percentiles colours, but other options"),
                       #   leaflet::leafletOutput("wells_map"),
                          DT::dataTableOutput("locations"),
                          verbatimTextOutput("test")
                        )
                      )
                    )
                  )),
          tabItem("Sub1", h4("Test")),
          tabItem("Sub2", h4("Test2"))
        )
      )
    ),
    tags$footer(
      div(
        div(style = "position:absolute; right: 7px; bottom: 7px",
            bookmarkButton(label = "Bookmark")),
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


  # Define server logic required to draw a histogram
  server <- function(input, output, session) {




    # output$test <- renderText({
    #   data_aquifers()
    # })
    output$test <- renderText({
      # unique(wells_locations$Area[input$locations_rows_all])
      # req(input$out_dir)
      # myblue
    })

    output$wells_selectize <- renderUI(
      selectizeInput(inputId = "wells_selectize",
                     label = "Wells to include:",
                     choices = wells_locations$Well,
                     multiple = TRUE)
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



    # Report ---------
    output$report_title <- renderUI({
      # if (is.null(input$region_wells)) {
      #   title <- "Provide title, ex. Groundwater Level Conditions"
      # } else if (length(input$region_wells) == 1) {
      #   title <- paste0(input$region_wells, " Groundwater Level Conditions")
      # } else {
      #   title <- "BC Groundwater Level Conditions"
      # }
      if (length(unique(wells_locations$Area[input$locations_rows_all])) == 4) {
        title <- "BC Groundwater Level Conditions"
      } else if (length(unique(wells_locations$Area[input$locations_rows_all])) == 1) {
        title <- paste0(unique(wells_locations$Area[input$locations_rows_all]), " Groundwater Level Conditions")
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

      if (length(unique(wells_locations$Subarea[input$locations_rows_all])) == 1) {
        Subarea <- paste0(unique(wells_locations$Subarea[input$locations_rows_all]), " region of the ")
      } else {
        Subarea <- ""
      }

      if (length(unique(wells_locations$Area[input$locations_rows_all])) == 4) {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in BC observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      } else if (length(unique(wells_locations$Area[input$locations_rows_all])) == 1) {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in the ", Subarea, unique(wells_locations$Area[input$locations_rows_all])," as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      } else {
        report_description <- paste0("The following provides an overview of groundwater (GW) conditions ",
                                     "in BC observation wells as of ", format(input$date_select, format = "%B %d, %Y"), ".")
      }

      textAreaInput("report_description",
                    "Report description:",
                    value =  report_description, rows = 2)
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

    volumes <- shinyFiles::getVolumes()
    shinyFiles::shinyDirChoose(input, 'out_dir', roots=volumes, filetypes=c('', 'txt')) #c(wd='.')

    # observe({
    #   shinyFiles::shinyDirChoose(input, "out_dir", roots = volumes, session = session)
    #   if(!is.null(input$out_dir)){
    #     myOutput1 <- shinyFiles::parseFilePaths(c(home = '~'),input$out_dir)
    #     myblue <- path.expand(myOutput1) #myblue isthen my file path that I can use in my function
    #   }
    # })


    output$locations <- DT::renderDataTable({
      wells_table %>%
        dplyr::mutate(Well = as.character(Well),
                      Location_Long = as.character(Location_Long),
                      Remarks = NA) %>%
        #  dplyr::filter(Well %in% input$wells_selectize) %>%
        dplyr::arrange(Well) %>%
        DT::datatable(rownames = FALSE,
                      filter = 'top',
                      extensions = c("Scroller", "Buttons"),
                      selection = "single",
                      editable = list(target = "column", disable = list(columns = c(0:4))),
                      options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                     deferRender = TRUE, dom = 'Brtip',
                                     buttons = list(list(extend = 'copy', title = NULL),
                                                    'csv', 'excel')))
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
