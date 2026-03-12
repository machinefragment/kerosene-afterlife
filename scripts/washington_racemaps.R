source("Rstartup.R")

# Looking at race and nativity in Washington state, 1910 and 1920, by county. 
# Data from NHGIS

data_1910 <- read_csv("data/washington_race/washington_1910/nhgis0036_csv/nhgis0036_ds37_1910_county.csv")
shape_1910 <- st_read("data/washington_race/washington_1910/nhgis0036_shapefile_tl2008_us_county_1910/US_county_1910_conflated.shp")
data_1920 <- read_csv("data/washington_race/washington_1920/nhgis0037_csv/nhgis0037_ds43_1920_county.csv")
data_1920_2 <- read_csv("data/washington_race/washington_1920/nhgis0038_csv/nhgis0038_ds43_1920_county.csv")
shape_1920 <- st_read("data/washington_race/washington_1920/nhgis0037_shapefile_tl2008_us_county_1920/US_county_1920_conflated.shp")

# first, we have to sum race by sex totals, then add the total population of indians chinese etc
# first, subset all data to washington state
data_1910 <- data_1910 %>% 
  filter(STATE == "Washington")

data_1920 <- data_1920 %>% 
  filter(STATE == "Washington")

data_1920_2 <- data_1920_2 %>% 
  filter(STATE == "Washington")

shape_1910 <- shape_1910 %>%
  filter(STATENAM == "Washington")

shape_1920 <- shape_1920 %>%
  filter(STATENAM == "Washington")

# join only necessary columns 
shape_1910 <- shape_1910 %>%
  left_join(
    data_1910 %>%
      select(GISJOIN, A5B001, A5B002, A5B003, A5B004, A30003, A30004, A46001),
    by = "GISJOIN"
  )

shape_1920 <- shape_1920 %>%
  left_join(
    data_1920 %>%
      select(GISJOIN, A8L001, A8L002, A8L003, A8L004, A8L005, A8L006),
    by = "GISJOIN"
  )
shape_1920 <- shape_1920 %>%
  left_join(
    data_1920_2 %>%
      select(GISJOIN, A87001),
    by = "GISJOIN"
  )

# now we can start creating some more human readable and legible columns
shape_1910 <- shape_1910 %>%
  mutate(
    total = A5B001 + A5B002 + A5B003 + A5B004 + A30003 + A30004 + A46001
  )

shape_1910 <- shape_1910 %>%
  mutate(
    nativYT = A5B001 + A5B002 + A5B003,
    frgnYT = A5B004,
    ngro = A30003 + A30004,
    othr = A46001 #this is chinese, japanese, indian, and all other
  
  )

shape_1920 <- shape_1920 %>%
  mutate(
    total = A8L001 + A8L002 + A8L003 + A8L004 + A8L005 + A8L006 + A87001,
    nativYT = A8L001 + A8L002,
    frgnYT = A8L003 + A8L004,
    ngro = A8L005 + A8L006,
    othr = A87001 #this is chinese, japanese, indian, and all other
  )

# now lets compute percentages 
shape_1910 <- shape_1910 %>%
  mutate(
    pctN8v = nativYT / total,
    pctFRG = frgnYT / total,
    pctN = ngro / total,
    pctO = othr/total
    
    )

shape_1920 <- shape_1920 %>%
  mutate(
    pctN8v = nativYT / total,
    pctFRG = frgnYT / total,
    pctN = ngro / total,
    pctO = othr/total
    
  )

# Now lets map this out, perhaps with our old HTML 
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(tmap)

# don't worry about this
tmap_options(check.and.fix = TRUE)
tmap_mode("view")

map1910 <- tm_shape(shape_1910) +
  tm_polygons(
    col = "pctN8v",
    title = "% Native Born White",
    id = "NHGISNAM",
    popup.vars = c(
      "Total" = "total",
      "Native Whites" = "nativYT",
      "Foreign Whites" = "frgnYT",
      "Negroes" = "ngro",
      "Chinese, Japanese, Indian, Other" = "othr"
    )
  )

tmap_save(map1910, "outputs/map1910.html", selfcontained = TRUE)

map1920 <- tm_shape(shape_1920) +
  tm_polygons(
    col = "pctN8v",
    title = "% Native Born White",
    id = "NHGISNAM",
    popup.vars = c(
      "Total" = "total",
      "Native Whites" = "nativYT",
      "Foreign Whites" = "frgnYT",
      "Negroes" = "ngro",
      "Chinese, Japanese, Indian, Other" = "othr"
    )
  )
tmap_save(map1920, "outputs/map1920.html", selfcontained = TRUE)
