#' Retrieve a watershed polygon
#'
#' This function will retrieve a watershed boundary from WDNR's ArcGIS Rest
#' Services. A subbasin (HUC8), watershed (HUC 10), or subwatershed (HUC 12) can
#' be retrieved by passing the HUC code or name as a character string.
#' See watershed_lookup for a full list of HUC codes and names.
#' Use filter_huc() to see watersheds by county or
#' classification level.
#'
#' A function to retrieve a watershed boundary from WDNR's
#' subbasin (HUC8), watershed (HUC 10), or subwatershed (HUC 12) spatial layers.
#' Use 'watershed_lookup' to see a full list of available HUC codes and names.
#'
#' @param watershed_code A character object specifying the HUC code for a watershed
#' @param watershed_name A character object specifying the HUC name for a watershed
#' @param county A character object specifying a county
#' @param sf_object Any sf object
#' @param huc_level "HUC_8","HUC_10", or "HUC_12"
#' @param where A SQL statement
#' @param ... Additional parameters that are passed to
#' \code{\link[arcpullr]{get_spatial_layer}}
#'
#' @return A sf polygon object
#' @export
#'
#' @examples
#' \dontrun{
#' get_watershed_layer(watershed_code = "07070006")
#' get_watershed_layer(watershed_name = "Kickapoo")
#' get_watershed_layer(county = "forest",huc_level = "HUC_12")
#' }
get_watershed_layer <- function(watershed_code = NULL,
                                watershed_name = NULL,
                                county = NULL,
                                sf_object = NULL,
                                huc_level = NULL,
                                where = NULL,
                                ...) {
  #error catching
  check_layer_args(watershed_code, watershed_name, county, sf_object, where)

  #user specific both a huc_code and watershed_name
  avoid_duplicate_sf_args(watershed_name, watershed_code, county, sf_object)

  #setting parameters based on watershed_name or huc_code
  #identify which HUC url to use within spatial query
  if (!is.null(watershed_code)) {
    huc_level <-
      wdnr.gis::watershed_lookup %>%
      dplyr::filter(.data$huc_codes %in% watershed_code) %>%
      dplyr::pull(huc_level) %>%
      unique()
    if (length(huc_level) == 0) {
      stop("There are no watershed codes that match those entered.")
    } else if (length(huc_level) > 1) {
      stop(
        "You've specified watersheds across different HUC levels.\n",
        "Please specify watersheds with similar HUC values",
        "(i.e. HUC8, HUC12, etc.)"
      )
    }
  } else if (!is.null(watershed_name)) {
    huc_level <-
      wdnr.gis::watershed_lookup %>%
      dplyr::filter(.data$huc_names %in% watershed_name) %>%
      dplyr::pull(huc_level) %>%
      unique()
    if (length(huc_level) == 0) {
      stop("There are no watershed names that match those entered.")
    } else if (length(huc_level) > 1) {
      stop(
        "You've specified watersheds across different HUC levels.\n",
        "Please specify watersheds with similar HUC values",
        "(i.e. HUC8, HUC12, etc.)"
      )
    }
  }

  # user did not specify huc_level when it is required
  if (is.null(huc_level)) {
    stop("You did not specify a huc_level (i.e. HUC_8, HUC_10, HUC_12)")
  }

  #find the correct polygon for a spatial query
  if (!is.null(sf_object)) {
    input_geometry <- sf_object
  } else if (!is.null(county)) {
    input_geometry <- filter_county_poly(county)
  } else {
    input_geometry <- ""
  }

  url <- switch(
    huc_level,
    "HUC_8" = list_urls(layers = "8-digit HUCs (Subbasins)"),
    "HUC_10" = list_urls(layers = "10-digit HUCs (Watersheds)"),
    "HUC_12" = list_urls(layers = "12-digit HUCs (Subwatersheds)")
  )

  #format premade where statements
  if (any(!is.null(where),
          !is.null(watershed_name),
          !is.null(watershed_code))) {
    if (!is.null(where)) {
      query <- where
    } else if (!is.null(watershed_name)) {
      query <- switch(
        huc_level,
        "HUC_8" = arcpullr::sql_where(HUC8_NAME = watershed_name),
        "HUC_10" = arcpullr::sql_where(HUC10_NAME = watershed_name),
        "HUC_12" = arcpullr::sql_where(HUC12_NAME = watershed_name)
      )
    }
    else if (!is.null(watershed_code)) {
      query <- switch(
        huc_level,
        "HUC_8" = arcpullr::sql_where(HUC8_CODE = watershed_code),
        "HUC_10" = arcpullr::sql_where(HUC10_CODE = watershed_code),
        "HUC_12" = arcpullr::sql_where(HUC12_CODE = watershed_code)
      )
    }
  } else {
    query <- ""
  }

  # pull the data using the appropriate function (spatial query or not)
  out <- find_layer_query(url, query, input_geometry)

  return(out)
}

#' Match a watershed's name based on one or more regex
#'
#' This function will match the names of a HUC_8 or a HUC_12 watershed found in
#' the \code{watershed_lookup} data set.
#'
#' @param ... One or more regex passed as character string
#' @param pull Logical. Pull the unique values or
#'
#' @return A character string with full watershed names if pull = TRUE, or a
#' data.frame with the number of rows equal to the number of matches otherwise
#' @export
#'
#' @examples
#' match_watershed_name("rainbow")
match_watershed_name <- function(..., pull = TRUE) {
  args <- list(...)
  out <- lapply(args, function(x) {
    return(dplyr::filter(wdnr.gis::watershed_lookup,
                         grepl(x, .data$huc_names, ignore.case = TRUE)))
  }) %>% do.call("rbind", .)
  if (nrow(out) == 0) {
    stop("No watershed names matched this string")
  }
  if (pull) {
    return(out$huc_names)
  } else {
    return(out)
  }
}

#' Retrieve WDNR's HYDRO spatial layer
#'
#' A function that can be used to retrieve WDNR's 24k Hydrography (HYDRO) layer.
#' Either the"24K Hydrography Streams and Rivers" or the
#' "24K Hydrography Lakes and Open Water" can be queried by setting 'layer_type'
#' to 'lines' or 'polygons' respectively.
#' A spatial query can be performed to limit the output of the function by
#' supplying a county name, watershed code, watershed name, or custom sf polygon
#' object. Use the 'watershed_lookup' to find valid watershed codes and names.
#' WBIC's can also be provided in order to return features for
#' specific waterbodies. The 'where' arguement can be used to run
#' custom SQL queries.
#'
#' This function will retrieve WDNR's hydro layer.  A county, watershed code,
#' watershed_name, or custom sf polygon can be specifie to filter the layer.
#' The layer type can be specified to query either the polylines or polygons
#' hydro spatial layers.
#'
#'
#' @param county A character object specifying a county name
#' @param watershed_code A character object specifying the HUC code for a
#' watershed
#' @param watershed_name A character object specifying the HUC name for a
#' watershed
#' @param sf_object Any sf polygon object
#' @param wbic A character object or string of WBIC's
#' @param where SQL statement
#' @param layer_type "lines", "polygons", or "flowlines"
#' @param ... Additional parameters to pass to
#' \code{\link[arcpullr]{get_spatial_layer}}
#'
#' @return An sf object of class polylines of polygons
#' @export
#'
#' @examples
#' \dontrun{
#' get_hydro_layer(county = "milwaukee", layer_type = "lines")
#' get_hydro_layer(watershed_code = "07070006", layer_type = "polygons")
#' get_hydro_layer(wbic = c("549400", "15000"), layer_type = "polygons")
#' get_hydro_layer(county = "milwaukee", where = "HYDROTYPE = '508'")
#' }
get_hydro_layer <- function (county = NULL,
                             watershed_code = NULL,
                             watershed_name = NULL,
                             sf_object = NULL,
                             wbic = NULL,
                             where = NULL,
                             layer_type = "lines",
                             ...) {
  # error catching
  if (is.null(layer_type) |
      !layer_type %in% c("lines", "polygons", "flow", "flowlines")) {
    stop(
      "'layer_type' must be equal to either 'lines', 'polygons'",
      "'flow', or 'flowlines'"
    )
  }

  check_layer_args(
    county, watershed_code, watershed_name, sf_object, wbic, where
  )

  avoid_duplicate_sf_args(
    county, watershed_name, watershed_code, sf_object
  )

  #format premade where statements
  if (!is.null(where) && !is.null(wbic)) {
    stop("You cannot specify both wbic and a where statement.")
  } else if (!is.null(where)){
    query <- where
  } else if (!is.null(wbic)) {
    if (layer_type == "lines"){
      query <- arcpullr::sql_where(RIVER_SYS_WBIC = wbic)
    } else if(layer_type == "polygons"){
      query <- arcpullr::sql_where(WATERBODY_WBIC = wbic)
    }
  } else {
    query <- ""
  }

  # find the correct polygon for a spatial query
  if (!is.null(sf_object)) {
    input_geometry <- sf_object
  } else if (!is.null(county)) {
    input_geometry <- filter_county_poly(county)
  } else if (!is.null(watershed_code)) {
    input_geometry <- get_watershed_layer(watershed_code = watershed_code)
  } else if (!is.null(watershed_name)) {
    input_geometry <- get_watershed_layer(watershed_name = watershed_name)
  } else {
    input_geometry <- ""
  }


  # get the appropriate url based on what is desired
  if (layer_type == "lines") {
    url <- list_urls(layers = "24K Hydrography Streams and Rivers")
  } else if (layer_type == "flow" || layer_type == "flowlines") {
    url <- list_urls(layers = "24K Flowlines")
  } else if (layer_type == "polygons") {
    url <- list_urls(layers = "24K Hydrography Lakes and Open Water")
  }

  # pull the data using the appropriate function (spatial query or not)
  out <- find_layer_query(url, query, input_geometry, ...)

  return(out)
}

#' Retrieve WDNR's FMDB Site spatial layer
#'
#' A function that can be used to retrieve the WDNR's Fish Management Database's
#' (FMDB) monitoring site spatial layer.
#' A spatial query can be performed to limit the output of the function by
#' supplying a county name, watershed code, watershed name, or custom sf polygon
#' object. Use the 'watershed_lookup' to find valid watershed codes and names.
#' FMDB site sequance numbers (site_seq) or SWIMS (swims_site_seq) site sequance
#' numbers can be provided to return specific sites. The 'where'
#' arguement can be used to run custom SQL queries.
#'
#'
#' @param county A character object specifying a county name
#' @param watershed_code A character object specifying the HUC code for a
#' watershed
#' @param watershed_name A character object specifying the HUC name for a
#' watershed
#' @param sf_object Any sf object
#' @param site_seq A character object or string
#' @param swims_site_seq A character object or string
#' @param where A SQL statement
#' @param layer_type Character. Retrieve point stations, polygon stations, or
#' both.
#' @param ... Additional parameters to pass to
#' \code{\link[arcpullr]{get_spatial_layer}}
#'
#' @return A sf object of class multipoints
#' @export
#'
#' @examples
#' \dontrun{
#' get_fmdb_site_layer(county = "milwaukee")
#' get_fmdb_site_layer(watershed_code = "07070006")
#' get_fmdb_site_layer(site_seq = c(7511,10175131,128290))
#' get_fmdb_site_layer(county = "waukesha",
#'                     where = "STATION_TYPE_CODE = 'LAKE'")
#' }
get_fmdb_site_layer <- function (county = NULL,
                                 watershed_code = NULL,
                                 watershed_name = NULL,
                                 sf_object = NULL,
                                 site_seq = NULL,
                                 swims_site_seq = NULL,
                                 where = NULL, layer_type = "points", ...) {

  # error catching
  # add both back in here once you fix both query below
  if (is.null(layer_type) |
      !layer_type %in% c("points", "polygons")) {
    stop("layer_type must be one of the following:\n",
         paste("    *", c("points", "polygons"),
               collapse = "\n"))
  }

  check_layer_args(
    county, watershed_code, watershed_name, sf_object,
    site_seq, swims_site_seq, where
  )

  avoid_duplicate_sf_args(county, watershed_name, watershed_code, sf_object)

  #format premade where statements
  if(!is.null(where)) {
    query <- where
  } else if(!is.null(site_seq)){
    query <- arcpullr::sql_where(FMDB_SITE_SEQ_NO = site_seq)
  } else if(!is.null(swims_site_seq)){
    query <- arcpullr::sql_where(SWIMS_STATION_ID = swims_site_seq)
  } else {
    query = ""
  }


  # find the correct polygon for a spatial query
  if (!is.null(sf_object)) {
    input_geometry <- sf_object
  } else if (!is.null(county)) {
    input_geometry <- filter_county_poly(county)
  } else if (!is.null(watershed_code)) {
    input_geometry <- get_watershed_layer(watershed_code = watershed_code)
  } else if (!is.null(watershed_name)) {
    input_geometry <- get_watershed_layer(watershed_name = watershed_name)
  } else {
    input_geometry <- ""
  }

  fmdb_poly_url <- list_urls(layers = "FMDB Polygon Stations")
  fmdb_point_url <- list_urls(layers = "FMDB Point Stations")

  # # need to figure out a better way to combine both points and polygons in
  # # single query
  # if (layer_type == "both") {
  #   fmdb_poly <- find_layer_query(fmdb_poly_url, query, input_geometry, ...)
  #   fmdb_points <- find_layer_query(fmdb_point_url, query, input_geometry, ...)
  #   fmdb_poly <-
  #     fmdb_poly %>%
  #     sf::st_transform(crs = 3071) %>%
  #     sf::st_centroid() %>%
  #     sf::st_transform(crs = 4326) %>%
  #     dplyr::mutate(shape.AREA = NA) %>%
  #     dplyr::select(-.data$shape.AREA) %>%
  #     suppressWarnings()
  #   out <- rbind(fmdb_points, fmdb_poly)
  # } else
  if (layer_type == "points") {
    out <- find_layer_query(fmdb_point_url, query, input_geometry, ...)
  } else if (layer_type == "polygons") {
    out <- find_layer_query(fmdb_poly_url, query, input_geometry, ...)
  } else {
    stop("layer_type must be specified as 'points', 'polygons', or 'both'")
  }
  return(out)
}

#' Retrieve WDNR's roads spatial layer
#'
#' A function to retrieve WDNR's roads spatial layers. "layer_type" can be set
#' to "major_roads" or "minor_roads" to query the Major Roads or County and
#' Local Roads respectively.
#' A spatial query can be performed to limit the output of the function by
#' supplying a county name, watershed code, watershed name, or custom sf polygon
#' object. Use the 'watershed_lookup' to find valid watershed codes and names.
#' The "where" argument can be used to run custom SQL queries.
#'
#'
#' @param county A character object specifying a county name
#' @param watershed_code A character object specifying the HUC code for a
#' watershed
#' @param watershed_name A character object specifying the HUC name for a
#' watershed
#' @param sf_object Any sf object
#' @param where A SQL statement
#' @param layer_type "major_roads" or "minor_roads"
#' @param ... Additional parameters to pass to
#' \code{\link[arcpullr]{get_spatial_layer}}
#'
#' @return A sf object of class polylines
#' @export
#'
#' @examples
#' \dontrun{
#' get_roads_layer(county = "washington", layer_type = "major_roads")
#' get_roads_layer(watershed_code = "07070006", layer_type = "minor_roads")
#' get_roads_layer(where ="HWY_NUM = '43'",layer_type = "major_roads")
#' }
get_roads_layer <- function (county = NULL,
                             watershed_code = NULL,
                             watershed_name = NULL,
                             sf_object = NULL,
                             where = NULL,
                             layer_type = "all", ...){
  #error catching
  #Stop the function if the layer_type is not properly specified
  accept_layer_type <- c("all", "all_roads", "major_roads", "minor_roads")
  if (!(layer_type %in% accept_layer_type)) {
    stop("layer_type must be one of the following:\n",
         paste("    *", c("major_roads", "minor_roads", "all"),
               collapse = "\n"))
  }

  check_layer_args(
    county, watershed_code, watershed_name, sf_object, where
  )

  avoid_duplicate_sf_args(county, watershed_name, watershed_code, sf_object)


  #format premade where statements
  if (!is.null(where)) {
    query <- where
  } else {
    query <- ""
  }

  # find the correct polygon for a spatial query
  if (!is.null(sf_object)) {
    input_geometry <- sf_object
  } else if (!is.null(county)) {
    input_geometry <- filter_county_poly(county)
  } else if (!is.null(watershed_code)) {
    input_geometry <- get_watershed_layer(watershed_code = watershed_code)
  } else if (!is.null(watershed_name)) {
    input_geometry <- get_watershed_layer(watershed_name = watershed_name)
  } else {
    input_geometry <- ""
  }

  #Query the REST API and return the object
  #Run the query on the major road layer
  major_url <- list_urls(layers = "Major Roads")[1]
  minor_url <- list_urls(layers = "County and Local Roads")[1]

  if (layer_type %in% c("all_roads", "all")) {
    majors <- find_layer_query(major_url, query, input_geometry, ...)
    minors <- find_layer_query(minor_url, query, input_geometry, ...)
    out <- rbind(majors, minors)
  } else if (layer_type == "major_roads") {
    out <- find_layer_query(major_url, query, input_geometry, ...)
  } else if (layer_type == "minor_roads") {
    out <- find_layer_query(minor_url, query, input_geometry, ...)
  } else {
    stop("layer_type must be one of the following:\n",
         paste("    *", c("major_roads", "minor_roads", "all"),
               collapse = "\n"))
  }
  return(out)
}
