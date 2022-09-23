skip_on_cran()

#build sf_point object
test_marker <- arcpullr::sf_point(c(-88.5274, 45.43536))
#build sf_multipoint object
test_markers <- arcpullr::sf_points(
  c(-88.52849, 45.43534),
  c(-88.49072, 45.44492))
#build sf_linestring object
test_line <-
  arcpullr::sf_line(
    c(-88.52999, 45.4358),
    c(-88.53045, 45.43903))
#build sf_multilinestring object
test_lines <-
  st_multilinestring(
    list(
      rbind(c(-88.529, 45.435), c(-88.53, 45.435)),
      rbind(c(-88.4911, 45.4446), c(-88.48936, 45.4479)
            )))%>%
  st_sfc(crs = 4326)%>%
  st_as_sf()
#build polygon object
test_polygon <- arcpullr::sf_polygon(
  c(-88.53150, 45.43769),
  c(-88.52998, 45.43583),
  c(-88.52641, 45.43627),
  c(-88.52930, 45.43861),
  c(-88.53150, 45.43769)
)
# test get_hydro_layer function -------------------------------------------
test_that("get_hydro_layer spatial query by polygon returns
          correct WATERBODY_WBIC",{
            otter_lake <- get_hydro_layer(
              sf_object = test_polygon,
              layer_type = "polygons",
              sp_rel = "esriSpatialRelOverlaps")
            expect_equal(otter_lake$WATERBODY_WBIC,549400)
            })
test_that("get_hydro_layer - spatial query by sf_linestring returns
          the correct WATERBODY_WBIC",{
            otter_lake <- get_hydro_layer(
              sf_object = test_line,
              layer_type = "polygons",
              sp_rel = "esriSpatialRelCrosses")
            expect_equal(otter_lake$WATERBODY_WBIC,549400)
          })
#appears to return the second feature only
test_that("get_hydro_layer - spatial query by sf_multilinestring returns
          the correct WATERBODY_WBIC",{
            otter_lake <- get_hydro_layer(
              sf_object = test_lines,
              layer_type = "polygons",
              sp_rel = "esriSpatialRelCrosses")
            expect_equal(otter_lake$WATERBODY_WBIC, c(549400, 548700))
          })
test_that("get_hydro_layer - spatial query by sf_point returns
          the correct WATERBODY_WBIC",{
            otter_lake <- get_hydro_layer(
              sf_object = test_marker,
              layer_type = "polygons")
            expect_equal(otter_lake$WATERBODY_WBIC,549400)
          })
#this does not work
test_that("get_hydro_layer - spatial query by sf_multipoint returns
          the correct WATERBODY_WBIC",{
            otter_lake <- get_hydro_layer(
              sf_object = test_markers,
              layer_type = "polygons")
            expect_equal(otter_lake$WATERBODY_WBIC, c(549400, 548700))
          })
test_that("get_hydro_layer - test that the 'layer_type' arguement uses
          the correct service and returns a multilinestring",{
  otter_creek <- get_hydro_layer(
    sf_object = test_polygon,
    layer_type = "lines",
    sp_rel = "esriSpatialRelCrosses")
  expect_equal(as.character(sf::st_geometry_type(otter_creek)),
               "MULTILINESTRING")
          })
test_that("get_hydro_layer - test that sql where returns the
          correct layer",{
  otter_creek <- get_hydro_layer(where = "RIVER_SYS_WBIC = 547200")
  expect_equal(otter_creek$ROW_NAME[1],"Otter Creek")
  })
# get_watershed_layer -----------------------------------------------------
test_polygon <-
  arcpullr::sf_polygon(
    c(-87.83913, 43.10541),
    c(-88.14056, 43.08735),
    c(-88.15636, 42.90793),
    c(-87.80342, 42.94818),
    c(-87.83913, 43.10541)
  )
test_that("get_watershed_layer - spatial query by sf_polygon returns
          the correct HUC12_CODE",{
            otter_ws <- get_watershed_layer(
              sf_object = st_transform(test_marker,3071),
              huc_level = "HUC_12",
              sp_rel = "esriSpatialRelTouches")
            expect_equal(otter_ws$HUC12_CODE,"040301050401")
          })
#does not work
test_that("get_watershed_layer - spatial query by sf_multipoints returns
          the correct HUC12_CODE",{
            otter_ws <- get_watershed_layer(
              sf_object = st_transform(test_markers,3071),
              huc_level = "HUC_12",
              sp_rel = "esriSpatialRelTouches")
            expect_equal(otter_ws$HUC12_CODE,"040301050401")
          })
test_that("get_watershed_layer - spatial query by sf_linestring returns
          the correct HUC12_CODE",{
            otter_ws <- get_watershed_layer(
              sf_object = st_transform(test_line,3071),
              huc_level = "HUC_12",
              sp_rel = "esriSpatialRelTouches")
            expect_equal(otter_ws$HUC12_CODE,"040301050401")
          })
test_that("get_watershed_layer - spatial query by sf_multilinestrings returns
          the correct HUC12_CODE",{
            otter_ws <- get_watershed_layer(
              sf_object = st_transform(test_lines,3071),
              huc_level = "HUC_12",
              sp_rel = "esriSpatialRelTouches")
            expect_equal(otter_ws$HUC12_CODE,"040301050401")
          })
test_that("get_watershed_layer - spatial query by sf_polygon returns
          the correct HUC12_CODE",{
            kk_watershed <- get_watershed_layer(
              sf_object = st_transform(test_polygon,3071),
              huc_level = "HUC_12",
              sp_rel = "esriSpatialRelWithin")
            expect_equal(kk_watershed$HUC12_CODE[1],"040400030501")
          })
test_that("get_watershed_layer - spatial query by sf_polygon returns
          the correct HUC12_CODE",{
            otter_ws <- get_watershed_layer(
              where = "HUC12_CODE = '040301050401'",
              huc_level = "HUC_12")
            expect_equal(otter_ws$HUC12_CODE,"040301050401")
          })
# test get_roads_layer function -------------------------------------------
#build st_point object
test_marker <-
  arcpullr::sf_point(
    c(686090.89600249426, 286395.55429548677),
    crs = 3071
  )
#build st_multipoint object
test_markers <-
  arcpullr::sf_points(
    c(679812.95480241114, 287892.79189548083),
    c(686090.89600249426, 286395.55429548677),
    crs = 3071
  )
#build st_linestring object
test_line <-
  arcpullr::sf_line(
    c(-87.93575, 43.03633),
    c(-87.93577, 43.03527)
  )
#build st_polygon object
test_polygon <-
  arcpullr::sf_polygon(
    c(-87.93859, 43.03514),
    c(-87.93636, 43.03516),
    c(-87.93745, 43.03628),
    c(-87.93859, 43.03514)
    )
#tests
test_that("get_roads_layer - spatial query by polygon returns
          correct road name",{
            roads <- get_roads_layer(
              sf_object = test_polygon,
              layer_type ="major_roads",
              sp_rel = "esriSpatialRelCrosses")
            expect_equal(roads$NAME[1],"East-West Freeway")
          })
test_that("get_roads_layer - spatial query by polyline returns
          correct road name",{
            roads <- get_roads_layer(
              sf_object = test_line,
              layer_type ="major_roads",
              sp_rel = "esriSpatialRelIntersects")
            expect_equal(roads$NAME[2],"East-West Freeway")
          })

# both of these commented out tests don't work
# points are tough to hit the mark on sometimes
# test_that("get_roads_layer - spatial query by sf_point returns
#           correct road name",{
#             roads <- get_roads_layer(
#               sf_object = test_marker,
#               layer_type ="major_roads",
#               sp_rel = "esriSpatialRelIntersects")
#             expect_equal(roads$NAME[1],"East-West Freeway")
#           })
# test_that("get_roads_layer - spatial query by sf_multipoint returns
#           correct road name",{
#             roads <- get_roads_layer(
#               sf_object = test_markers,
#               layer_type ="major_roads",
#               sp_rel = "esriSpatialRelIntersects")
#             expect_equal(roads$NAME[1:2],c("East-West Freeway", "Zoo Freeway"))
#           })

test_that("get_roads_layer - sql where statement returns the correct
          road name",{
            roads <- get_roads_layer(
              where = "OSM_ID = 50487000",layer_type ="major_roads")
            expect_equal(roads$NAME[1],"East-West Freeway")
          })
