current_sections <- c(
  "AM_WARP_MAP", "DG_HiCap", "DG_Viewer", "DG_Well_Driller", "DW_Map_Cached",
  "DW_Map_Dynamic", "ER_Biotics", "ER_Bird_Breeding_Atlas", "FM_Trout",
  "LF_DML", "PR_TRAILS", "RR_Sites_Map", "WM_CWD", "WM_LMS", "WM_VPA",
  "WT_Condition_Viewer", "WT_SWDV", "WT_TMDL", "WY_Lakes_AIS", "WY_PRESTO",
  "CS_Offices", "FN_FLEET", "FR_IFFRS", "FR_IFFRS_TRN", "FR_WIS_BURN",
  "LF_Master_Planning", "OGW_Beach_Monitoring", "TS_AGOL_STAGING_SERVICES",
  "WA_Waste", "WM_APIP", "WY_Wastewater", "WY_WETLAND", "DW_Elevation",
  "DW_Image", "DW_Land_Cover", "FR_URBAN_FORESTRY"
)
trout_services <- c(
  "FM_TROUT_ANNO_WTM_Ext", "FM_TROUT_COUNTY_REGS_WTM",
  "FM_TROUT_HAB_SITES_WTM_Ext", "FM_TROUT_NONDNR_EASEMENTS_WTM_Ext",
  "FM_TROUT_REGS_WTM_Ext"
)


test_that("list_sections works appropriately", {
  sections <- list_sections()
  expect_length(sections, 36)
  expect_equal(sections, current_sections)
})

test_that("list_services works appropriately", {
  services <- list_services(section = "FM_Trout")
  expect_length(services, 5)
  expect_equal(services, trout_services)
})

test_that("list_layers works appropriately", {
  trout_anno_layers <- c(
    "X10330.FM_TROUT_LABELS_AN_500K",
    "Annotation Class 1"
  )
  trout_anno_layers_df <- data.frame(
    section = "FM_Trout",
    service = "FM_TROUT_ANNO_WTM_Ext",
    layer = c("X10330.FM_TROUT_LABELS_AN_500K", "Annotation Class 1"),
    stringsAsFactors = FALSE
  )
  layers <- list_layers(services = trout_services[1])
  layers_df <- list_layers(services = trout_services[1], pull = FALSE)
  expect_length(layers, 2)
  expect_equal(layers, trout_anno_layers)
  expect_equal(layers_df, trout_anno_layers_df)
  expect_warning(list_layers())
})

test_that("list_urls works appropriately", {
  trout_anno_urls <- list_urls(services = trout_services[1])
  trout_anno2_url <- paste0(
    "https://dnrmaps.wi.gov/arcgis/rest/services/FM_Trout/",
    "FM_TROUT_COUNTY_REGS_WTM/MapServer/0"
  )
  expect_length(trout_anno_urls, 2)
  expect_equal(list_urls(services = trout_services[2]), trout_anno2_url)
  expect_true(nrow(list_urls(services = trout_services[1], pull = FALSE)) == 2)
  expect_true(ncol(list_urls(services = trout_services[1], pull = FALSE)) == 5)
})

test_that("match_sections works appropriately", {
  expect_equal(match_sections("trout"), "FM_Trout")
  expect_length(match_sections("WT"), 3)
  expect_equal(match_sections("FR_IFFRS", exact = TRUE), "FR_IFFRS")
})

test_that("match_services works appropriately", {
  expect_length(match_services("Fish"), 2)
  expect_equal(
    match_services("Fish", sections = match_sections("WT")),
    "WT_Fisheries_Waters_WTM_Ext"
  )
  lc_service <- "EN_Land_Cover2_Lev4"
  expect_equal(match_services(lc_service, exact = TRUE), lc_service)
})

test_that("match_layers works appropriately", {
  expect_length(match_layers("fish"), 38)
  expect_length(match_layers("fish|trout"), 51)
  expect_equal(match_layers("stream", sections = match_sections("Trout")),
               "Trout Stream Regulations 2018-2019")
  expect_equal(match_layers("Wild Rice", exact = TRUE), "Wild Rice")
})

test_that("argument checking functions return correct warnings", {
  a <- NULL
  b <- NULL
  c <- 3
  d <- 4
  expect_error(check_layer_args(a, b))
  expect_null(check_layer_args(a, b, c))
  expect_error(avoid_duplicate_sf_args(a, b, c, d))
  expect_null(avoid_duplicate_sf_args(a, b, c))
  expect_equal(deparse_arg_names(a, b, c), c("a", "b", "c"))
})

test_that("standardize_county_names works correctly", {
  expect_equal(standardize_county_names("Waushara"), "waushara")
  expect_equal(standardize_county_names("St. Croix"), "st_croix")
  expect_equal(standardize_county_names("Fond du Lac"), "fond_du_lac")
  expect_equal(
    standardize_county_names("Fond du Lac", "ST. CROIX", "wAuShARA"),
    c("fond_du_lac", "st_croix", "waushara")
  )

})
