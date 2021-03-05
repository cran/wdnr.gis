#' Retrieve county polygon layer
#'
#' Return specific county polygon layer from \code{wi_counties} sf object
#'
#' @param ... Any Wisconsin counties provided as character strings, separated
#' by commas
#'
#' @return An sf data.frame with the appropriate counties
#' @export
#'
#' @examples
#' \dontrun{
#' plot(filter_county_poly("door"))
#' plot_layer(filter_county_poly("portage"))
#' }
filter_county_poly <- function(...) {
  input <- unlist(list(...))
  counties <-
    input %>%
    standardize_county_names() %>%
    lapply(function(x) {
      if (x %in% wdnr.gis::wi_counties$county) {
        wdnr.gis::wi_counties[wdnr.gis::wi_counties$county == x, ]
      } else {
        return(NULL)
      }
    })
  county_chk <- unlist(lapply(counties, is.null))
  if (any(county_chk)) {
    if (any(!county_chk)) {
      warning("One or more counties may be missing")
      return(do.call("rbind", counties))
    } else {
      stop("Are you sure that's a county in Wisconsin?")
    }
  } else {
    return(do.call("rbind", counties))
  }
}

