#' wdnr.gis
#'
#' A package to pull spatial layers from the Wisconsin DNR ArcGIS
#' REST API
#'
#' \if{html}{\figure{logo.png}{options: alt='logo' width=120}}
#' \if{latex}{\figure{logo.png}{options: width=0.5in}}
#'
#' The wdnr.gis package provides shortcut functions for working with various
#' spatial layers on the WDNR ArcGIS REST API. Currently, these include:
#' get_hydro_layer, get_watershed_layer, get_roads_layer, get_fmdb_site_layer
#'
#' @section get_*_layer functions:
#' These functions retrieve spatial layers that are noted by the middle term in
#' the function name. For example, the get_hydro_layer function retrieve's
#' spatial data from Wisconsin's 24K Rivers and Streams Hydrography layer (or
#' lakes if specified). These functions generally have the same arguments and
#' can be queried by county, sf_object, watershed, or a SQL where statement.
#'
#' @aliases wdnr.gis
#'
#' @name wdnr.gis-package
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
