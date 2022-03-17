
#' List available sections, services, layers, and URLs in the WDNR GIS REST API
#'
#' These functions can take sections, services, and layers specified as
#' character strings and return either the section, service, layer or url as
#' available in the WDNR GIS REST API
#'
#' @return A vector of matching sections, services, layers, or URLs depending
#' on the function called
#' @export
#'
#' @name list_funs
#'
#' @examples
#' list_sections()
#' list_services(sections = "WT_TMDL")
#' list_layers(services = match_services("Invasive"))
#' list_urls(sections = match_sections("WT"),
#'           services = match_services("inland"))
list_sections <- function() {
  return(unique(wdnr.gis::service_urls$section))
}

#' @rdname list_funs
#' @param sections A character vector of available sections to subset by
#' @param pull Logical. Pull unique values (TRUE, default) or show the matching
#' rows in the service_urls data.frame
#' @export
list_services <- function(sections = NULL, pull = TRUE) {
  if (!is.null(sections)) {
    sections <- match_sections(sections)
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$section %in% sections)
    if (pull) {
      out <-
        out %>%
        dplyr::pull(.data$service) %>%
        unique()
    } else {
      out <-
        out %>%
        dplyr::select(.data$section, .data$service) %>%
        dplyr::distinct()
    }
  } else {
    out <- unique(wdnr.gis::service_urls$service)
  }
  return(out)
}

#' @rdname list_funs
#' @param services A character vector of available services to subset by
#' @export
list_layers <- function(sections = NULL, services = NULL, pull = TRUE) {
  return(
    list_layer_url(
      type = "layer",
      sections = sections,
      services = services,
      pull = pull
    )
  )
}

#' @rdname list_funs
#' @param layers A character vector of available layers to subset by
#' @export
list_urls <- function(layers = NULL, sections = NULL, services = NULL,
                      pull = TRUE) {
  if (!is.null(layers)) {
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$layer %in% layers)
    if (pull) {
      out <-
        out %>%
        dplyr::pull(.data$url) %>%
        unique()
    }
    return(out)
  } else {
    return(
      list_layer_url(
        type = "url",
        sections = sections,
        services = services,
        pull = pull
      )
    )
  }
}

#' Helper function to re-create \code{\link{list_layers}} and
#' \code{\link{list_urls}}
#'
#' @param type Character. The column of data to retrieve from service_urls
#' @param sections See \code{\link{list_funs}}
#' @param services See \code{\link{list_funs}}
#' @param pull See \code{\link{list_funs}}
#'
#' @return A vector of available layers or URLs; depending on \code{type}
list_layer_url <- function(type = "layer", sections = NULL, services = NULL,
                           pull = TRUE) {
  if (!is.null(sections) & !is.null(services)) {
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$section %in% sections, .data$service %in% services)
  } else if (!is.null(sections) & is.null(services)) {
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$section %in% sections)
  } else if (is.null(sections) & !is.null(services)) {
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$service %in% services)
    if (length(out) > 20 & length(services) > 1) {
      warning(
        paste(
          sprintf(
            "There are %s %s available.",
            length(out),
            paste0(type, "s")
          ),
          "You may want to specify a section as well."
        )
      )
    }
  } else {
    out <- wdnr.gis::service_urls
    warning(
      paste(
        sprintf(
          "There are %s %s available.",
          nrow(wdnr.gis::service_urls),
          paste0(type, "s")
        ),
        "Please specify a section and/or service"
      )
    )
  }
  if (pull) {
    out <- unique(out[, type])
  } else {
    if (type == "layer") {
      out <- dplyr::select(out, .data$section, .data$service, .data$layer)
    }
  }
  return(out)
}

#' Find available sections, services, or layers using a regular expression
#'
#' These functions allow you to search for sections, services, or layers that
#' are available in the WDNR ArcGIS REST API using a regular expression. This
#' is useful when you don't know the full name of a section, service, or
#' layer but want to search based on keywords
#'
#' @param ... Character vector or regular expression to match on
#' @param exact Logical stating whether to match objects in \code{...} exactly
#' or loosely
#'
#' @return A character vector of all matching sections, services, or layers
#' appropriate to the called function
#' @export
#'
#' @name match_funs
#'
#' @examples
#' match_sections("WT")
#' match_services("Fish", sections = match_sections("WT"))
#' match_layers("Fish", sections = match_sections("WT"))
match_sections <- function(..., exact = FALSE) {
  x <- unlist(list(...))
  if (exact) {
    x <- paste0("^", x, "$")
  }
  out <-
    lapply(x, function(y) {
      grep(y, unique(wdnr.gis::service_urls$section),
           ignore.case = TRUE, value = TRUE)
    })
  return(unlist(out))
}

#' @rdname match_funs
#' @param sections A character vector of available sections to subset by
#' @param pull Logical. Pull unique values (TRUE, default) or show the matching
#' rows in the service_urls data.frame
#' @export
match_services <- function(..., sections = NULL, pull = TRUE, exact = FALSE) {
  x <- unlist(list(...))
  if (exact) {
    x <- paste0("^", x, "$")
  }
  out <-
    lapply(x, function(y) {
      if (!is.null(sections)) {
        sections <- match_sections(sections)
        tmp <-
          wdnr.gis::service_urls %>%
          dplyr::filter(.data$section %in% sections) %>%
          dplyr::pull(.data$service) %>%
          unique()
      } else {
        tmp <- unique(wdnr.gis::service_urls$service)
      }
      return(grep(y, tmp, ignore.case = TRUE, value = TRUE))
    })
  if (!pull) {
    tmp <- unlist(out)
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$service %in% tmp) %>%
      dplyr::select(.data$section,
                    .data$service,
                    .data$layer,
                    .data$layer_type)
    return(out)
  } else {
    return(unlist(out))
  }
}

#' @rdname match_funs
#' @param services A character vector of available services to subset by
#' @export
match_layers <- function(..., sections = NULL, services = NULL,
                         pull = TRUE, exact = FALSE) {
  x <- unlist(list(...))
  if (exact) {
    x <- paste0("^", x, "$")
  }
  out <-
    lapply(x, function(y) {
      if (!is.null(sections) & !is.null(services)) {
        sections <- match_sections(sections)
        services <- match_services(services)
        tmp <-
          wdnr.gis::service_urls %>%
          dplyr::filter(.data$section %in% sections,
                        .data$services %in% services) %>%
          dplyr::pull(.data$layer) %>%
          unique()
      } else if (!is.null(sections) & is.null(services)) {
        sections <- match_sections(sections)
        tmp <-
          wdnr.gis::service_urls %>%
          dplyr::filter(.data$section %in% sections) %>%
          dplyr::pull(.data$layer) %>%
          unique()
      } else if (is.null(sections) & !is.null(services)) {
        services <- match_services(services)
        tmp <-
          wdnr.gis::service_urls %>%
          dplyr::filter(.data$service %in% services) %>%
          dplyr::pull(.data$layer) %>%
          unique()
      } else {
        tmp <- unique(wdnr.gis::service_urls$layer)
      }
      return(grep(y, tmp, ignore.case = TRUE, value = TRUE))
    })
  if (!pull) {
    tmp <- unlist(out)
    out <-
      wdnr.gis::service_urls %>%
      dplyr::filter(.data$layer %in% tmp) %>%
      dplyr::select(.data$section, .data$service, .data$layer)
    return(out)
  } else {
    return(unlist(out))
  }
}


#' Helper functions to aid in checking arguments to get_*_layer functions
#'
#' \code{check_layer_args} simply looks at the arguments that is passed to it
#' and checks to make sure that at least one is not NULL.
#' \code{avoid_duplicate_sf_args} ensures the presence of only one argument that
#' would result in a downstream spatial query (i.e. only a single sf object
#' can be used in a spatial query -- this function ensures that only one will
#' be).
#' \code{deparse_arg_names} is just a helper for the above two functions to
#' format argument names in a useful way
#'
#' @param ... Any number of objects to be checked
#'
#' @return If any of \code{...} are not NULL, returns nothing. Otherwise stops
#' function execution.
#'
#' @name check_args
#'
#' @examples
#' \dontrun{
#' a <- NULL
#' b <- NULL
#' check_layer_args(a, b)
#' }
check_layer_args <- function(...) {
  chk <- unlist(lapply(list(...), is.null))
  if (all(chk)) {
    arg_names <- deparse_arg_names(...)
    msg <- paste0(
      "Please provide one of the following arguments:\n",
      paste("    *", unlist(arg_names), collapse = "\n")
    )
    return(stop(msg))
  }
}

#' @rdname check_args
avoid_duplicate_sf_args <- function(...) {
  null_poly_list <- lapply(list(...), is.null)
  if (length(null_poly_list[!unlist(null_poly_list)]) > 1) {
    arg_names <- deparse_arg_names(...)
    msg <- paste0(
      "Please provide one and only one of the following arguments:\n",
      paste("    *", unlist(arg_names), collapse = "\n")
    )
    return(stop(msg))
  }
}

# find_sf_query_object <- function(...) {
#   args <- list(...)
#   if (!is.null(args$watershed_name)) {
#     sf_poly <- get_watershed_layer(watershed_name = args$watershed_name)
#   } else if (!is.null(args$watershed_code)) {
#     sf_poly <- get_watershed_layer(watershed_code = args$watershed_code)
#   } else if (!is.null(args$county)) {
#     sf_poly <- filter_county_poly(args$county)
#   } else if (!is.null(args$sf_object)) {
#     sf_poly <- sf_object
#   } else {
#     sf_poly <- wdnr.gis::wi_poly
#   }
#   return(sf_poly)
# }

#' @rdname check_args
deparse_arg_names <- function(...) {
  arg_names <- gsub("list\\(|\\)", "", deparse(substitute(list(...))))
  arg_names <- trimws(strsplit(paste(arg_names, collapse = ""),
                               split = ",", )[[1]])
  return(arg_names)
}

find_layer_query <- function(url, query, input_geometry, ...) {

  # first check the input geometry type
  if (inherits(input_geometry, "sf")) {
    input_geom_type <- sf::st_geometry_type(input_geometry)
  } else if (inherits(input_geometry, "bbox")) {
    input_geom_type <- "bbox"
  } else {
    input_geom_type <- NULL
  }

  if (!inherits(input_geometry, "sf")) {
    out <- arcpullr::get_spatial_layer(
      url = url, where = query, ...
    )
  } else if (all(grepl("POLYGON", input_geom_type))) {
    out <- arcpullr::get_layer_by_poly(
      url = url, geometry = input_geometry, where = query, ...
    )
  } else if (all(grepl("LINE", input_geom_type))) {
    out <- arcpullr::get_layer_by_line(
      url = url, geometry = input_geometry, where = query, ...
    )
  } else if (all(input_geom_type == "POINT")) {
    out <- arcpullr::get_layer_by_point(
      url = url, geometry = input_geometry, where = query, ...
    )
  } else if (all(input_geom_type == "MULTIPOINT")) {
    out <- arcpullr::get_layer_by_point(
      url = url, geometry = input_geometry, where = query, ...
    )
  } else if (all(input_geom_type == "bbox")) {
    out <- arcpullr::get_layer_by_envelope(
      url = url, geometry = input_geometry, where = query, ...
    )
  } else {
    stop("Sorry, something went wrong with your query. ",
         "Check your arguments to make sure you didn't miss something.")
  }
  return(out)
}


#' Standardize county names
#'
#' This function alters string text of county names to a standardized format of
#' lower-cased, no punctuation (i.e. st instead of st.), and underscore instead
#' of spaces
#'
#'
#' @param ... One or more county names in quotations, or a character vector of
#' county names
#'
#' @return A character vector the same length as \code{name}, but tidied up
#' for easier and standard viewing
standardize_county_names <- function(...) {
  names <- unlist(list(...))
  out <-
    names %>%
    tolower() %>%
    trimws() %>%
    gsub("\\s+", "_", .) %>%
    gsub("\\.", "", .)
  return(out)
}
