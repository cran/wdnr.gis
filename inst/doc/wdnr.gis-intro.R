## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, eval = TRUE-------------------------------------------------------
library(wdnr.gis)

## ---- warning = FALSE---------------------------------------------------------
#  mke_cty_streams <- get_hydro_layer(county = "milwaukee")

## ---- warning = FALSE---------------------------------------------------------
#  pt <- sf_point(c(-90.8, 43.4))
#  watershed <- get_watershed_layer(sf_object = pt, huc_level = "HUC_8")
#  streams <- get_hydro_layer(sf_object = watershed)

## -----------------------------------------------------------------------------
#  portage_lc <- get_wis_landcover(county = "portage")
#  plot_layer(portage_lc)

## -----------------------------------------------------------------------------
#  portage_imagery <- get_wis_imagery(county = "portage")
#  plot_layer(portage_imagery)

## ---- eval = TRUE-------------------------------------------------------------
match_sections("trout")
match_services("trout")
match_layers("trout stream")

## ---- eval = TRUE-------------------------------------------------------------
list_services("FM_Trout")

## ---- eval = TRUE-------------------------------------------------------------
list_layers(services = match_services("trout.*stream"))
list_urls(layers = match_layers("trout.*stream"))

