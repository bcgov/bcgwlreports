test_that("well_meta", {
  ow <- c("OW355", "OW320")
  m <- expect_silent(well_meta(ow)) %>%
    expect_s3_class("data.frame") %>%
    expect_named(c("ow", "aquifer_id", "region", "area",
                   "location", "location_long",
                   "subtype", "type", "hydraulic_connectivity"))

  expect_equal(m$ow, sort(ow)) # NOTE: well_meta() sorts output
  expect_equal(m$location, c("Cobble Hill", "Chemainus"))
  expect_equal(m$hydraulic_connectivity, c("Not Likely", "Likely"))
  expect_equal(m$region, rep("West Coast Region", 2))
})


test_that("ow_fish() has correct order and info", {

  # OW355 is likely connected, the others are not
  ow <- c("OW355", "OW320", "OW397", "OW396", "OW394", "OW200")

  expect_silent(f <- ow_fish(ow)) %>%
    expect_type("character") %>%
    expect_length(length(ow))

  expect_equal(ow[-1], f[-1])
  expect_equal(ow, stringr::str_extract(f, "OW[0-9]{3}"))

  expect_equal(well_meta(ow[1])$hydraulic_connectivity, "Likely")
  expect_true(all(well_meta(ow[-1])$hydraulic_connectivity == "Not Likely"))

})
