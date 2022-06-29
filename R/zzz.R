.onLoad <-
  function(libname = find.package("bcgwlreports"),
           pkgname = "bcgwlreports") {
    # CRAN Note avoidance
    if (getRversion() >= "2.15.1")
      utils::globalVariables(
        # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
        ## This is getting ridiculous
        c("Value",
          "%m-%",
          "read.csv",
          "Area",
          "Well",
          "Date",
          "years_min",
          "ow",
          "date2",
          "region",
          "location",
          "area",
          "hydraulic_connectivity",
          "aquifer_type",
          "approval",
          "percentile",
          "n_years",
          ".rs.invokeShinyWindowExternal",
          "Well_Number",
          "Report",
          "Aquifer_Type",
          "Aquifer_Subtype",
          "Hydraulic_Connectivity",
          "Location_Long",
          "NR_Area",
          "NR_Subarea",
          "Location",
          "Remarks",
          "Aquifer_ID",
          "Latitude",
          "Longitude",
          "Well_Status",
          "myLocation",
          "QualifiedTime",
          "DayofYear")
      )
    invisible()
  }
