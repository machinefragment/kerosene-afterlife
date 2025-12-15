source("Rstartup.R")
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# load these guys in 
fuel60 <- st_read("data/shapefiles/analysisready/heatingfuel1960.shp")
fuel70 <- st_read("data/shapefiles/analysisready/fuel70ctt_tracts1970.shp")

# we need longitude and latitude
fuel60 <- st_transform(fuel60, 4326)
fuel70 <- st_transform(fuel70, 4326)
fuel60 <- st_make_valid(fuel60)
fuel70 <- st_make_valid(fuel70)

# bbox setting
seattle_bbox <- st_bbox(c(
  xmin = -122.45,
  xmax = -122.20,
  ymin = 47.50,
  ymax = 47.75
), crs = st_crs(4326))

# Mapping 
tmap_options(check.and.fix = TRUE)
tmap_mode("view")   # static first

tm_shape(fuel60,bbox = seattle_bbox) +
  tm_polygons(
    col = "pctker",
    title = "% Households using Kerosene",
    id = "AREANAME",
    popup.vars = c(
      "Households using Kerosene" = "kerosene",
      "Percent Kerosene" = "pctker",
      "Total Households" = "total"
    )
  )


