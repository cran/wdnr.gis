% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{check_args}
\alias{check_args}
\alias{check_layer_args}
\alias{avoid_duplicate_sf_args}
\alias{deparse_arg_names}
\title{Helper functions to aid in checking arguments to get_*_layer functions}
\usage{
check_layer_args(...)

avoid_duplicate_sf_args(...)

deparse_arg_names(...)
}
\arguments{
\item{...}{Any number of objects to be checked}
}
\value{
If any of \code{...} are not NULL, returns nothing. Otherwise stops
function execution.
}
\description{
\code{check_layer_args} simply looks at the arguments that is passed to it
and checks to make sure that at least one is not NULL.
\code{avoid_duplicate_sf_args} ensures the presence of only one argument that
would result in a downstream spatial query (i.e. only a single sf object
can be used in a spatial query -- this function ensures that only one will
be).
\code{deparse_arg_names} is just a helper for the above two functions to
format argument names in a useful way
}
\examples{
\dontrun{
a <- NULL
b <- NULL
check_layer_args(a, b)
}
}
