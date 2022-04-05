
test_that("Basic run", {
  expect_message(well_report(ows = c("OW008", "OW217", "OW377",
                                     "OW197", "OW432"))) %>%
    suppressMessages()
  expect_true(file.exists(paste0("report_", Sys.Date(), ".html")))
  unlink(paste0("report_", Sys.Date(), ".html"))
})

test_that("Title/desciption run", {
  well_report(
    ows = c("OW008", "OW217", "OW377", "OW197", "OW432"),
    title = "Summer 2021 Drought Report",
    description = paste0("This report covers the period of **2020 to 2021** ",
                         "and indicates levels of severe drought in much of ",
                         "southern British Columbia.")) %>%
    expect_message() %>%
    suppressMessages()
  expect_true(file.exists(paste0("report_", Sys.Date(), ".html")))
  unlink(paste0("report_", Sys.Date(), ".html"))
})

test_that("Title/desciption from file run", {
  readr::write_lines(paste0(
    "This report covers the period of **2020 to 2021** and indicates levels ",
    "of severe drought in much of southern British Columbia."),
    file = "test.txt")

  well_report(ows = c("OW008", "OW217", "OW377", "OW197", "OW432"),
              title = "Summer 2021 Drought Report",
              description = "test.txt") %>%
    expect_message() %>%
    suppressMessages()

  expect_true(file.exists(paste0("report_", Sys.Date(), ".html")))
  unlink(paste0("report_", Sys.Date(), ".html"))
  unlink("test.txt")
})
