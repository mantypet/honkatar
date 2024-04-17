library(dplyr)
library(purrr)
library(sf)
library(httr)
library(ows4R)

grid_wfs <- "https://avoin.metsakeskus.fi/rajapinnat/v1/gridcell/ows"

# function this
url <- parse_url(grid_wfs)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # facultative
                  request = "GetCapabilities"
)
request <- build_url(url)
request
#

grid_client <- WFSClient$new(grid_wfs, 
                                   serviceVersion = "2.0.0")

grid_client$getFeatureTypes(pretty = TRUE)

url <- parse_url(grid_wfs)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = "v1:gridcell",
                  srsName = "EPSG:3067",
                  bbox = "434726.2816,7007375.8560,441155.2157,7013078.3720"
)
request <- build_url(url)

gridcells <- read_sf(request)

###


