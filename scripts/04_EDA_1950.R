source("Rstartup.R")

# Examining Data Coverage for 1940/1950 - create shapefiles then export to 
# GIS viewer of choice to zoom around and whatnot

data_1940 <- read_csv("data/forties/nhgis0041_ds76_1940_tract.csv")
shape_1940 <- st_read("data/shapefiles/US_tract_1940.shp")

data_1950 <- read_csv("data/fifties/nhgis0040_ds82_1950_tract.csv")
shape_1950 <- st_read("data/shapefiles/US_tract_1950.shp")

shape_1940 <- shape_1940 %>%
  left_join(data_1940, by = "GISJOIN")

shape_1950 <- shape_1950 %>%
  left_join(data_1950, by = "GISJOIN")


st_write(shape_1940, "data/shapefiles/processing/shape_1940_joined.shp")
st_write(shape_1950, "data/shapefiles/processing/shape_1950_joined.shp")
