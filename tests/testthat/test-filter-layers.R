test_that("filter_county_poly correctly filters county poly layers", {
  portage_cty <- filter_county_poly("portage")
  central_wi_counties <- filter_county_poly("portage", "marathon", "waupaca")
  expect_true(nrow(portage_cty) == 1)
  expect_true(nrow(central_wi_counties) == 3)
})
