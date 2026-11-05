# setup script for processing functions

#Install (if necessary) and load all required packages ----------------

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# vector of packages to load
packages <- c(
  "akima",
  "ambient",
  "arcgisutils",
  "arrow",
  "broom",
  "cowplot",
  "curl",
  "data.table",
  "dataRetrieval",
  "devtools",
  "dtplyr",
  "elevatr",
  "exactextractr",
  "flextable",
  "furrr",
  "geojsonsf",
  "geosphere",
  "ggdist",
  "ggforce",
  "ggfortify",
  "ggpubr",
  "ggrepel",
  # "ggseas",
  "ggthemes",
  "glue",
  "grDevices",
  "gridExtra",
  "here",
  "httr",
  "igraph",
  "janitor",
  "jsonlite",
  "kableExtra",
  "knitr",
  "leafem",
  "leaflet",
  "leafsync",
  "lwgeom",
  "maptiles",
  "mapview",
  "Metrics",
  "ncdf4",
  "nhdplusTools",
  "nngeo",
  "padr",
  "patchwork",
  "plotly",
  "progress",
  "RColorBrewer",
  "RCurl",
  "readxl",
  "remotes",
  "rvest",
  "scales",
  "sf",
  "shinytest2",  
  "soilDB",
  "SPEI",
  "stars",
  "terra",
  "testthat",
  "tigris",
  "tmap",
  "trend",
  "units",
  "viridis",
  "whitebox",
  "xml2",
  "zoo"
)

packages <- c(packages, "tidyverse")


packageLoad(packages)

## GitHub package installs ----------------

# suggested install of github version for climateR
# remotes::install_github("mikejohnson51/AOI") # suggested! But I don't think we need it..
if (!"climateR" %in% installed.packages()) {
  remotes::install_github("mikejohnson51/climateR")
}
library(climateR)


# suggested install of Rnoaa - 
if (!"rnoaa" %in% installed.packages()) {
  remotes::install_github("ropensci/rnoaa")
}
library(rnoaa)


# if (!"cft" %in% installed.packages()) {
#   devtools::install_github("earthlab/cft")
# }
# library(cft)

# source all functions --------------------------

if (!dir.exists("src")) {
  stop("src/ not found. Run this script from the nps_water_rights project root.")
}

purrr::map(list.files(
  path = "src/",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
),
source)

