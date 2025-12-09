# Creating shapefiles for inspection in external GIS software
# Get our regular startup sequence running
source("Rstartup.R")

# create a new directory to put ready shapefiles in 
dir.create("data/shapefiles/analysisready")
# first, our 1960s data
fuel60 <- read_csv(here("data/fuel_csv", "fuel60clean.csv"))
tracts60 <- st_read(here("data/shapefiles", "US_tract_1960.shp"))


# lets do an inner join to keep only matched columns
merged60 <- tracts60 %>%
  inner_join(fuel60, by = "GISJOIN")
# now lets write it to memory
st_write(
  merged60,
  here("data", "shapefiles/analysisready/heatingfuel1960.shp"),
  delete_dsn = TRUE # this lets us overwrite later
)

# Ok - that was pretty easy, and we will be exploring it in a GIS interface.
# Next up is to loop through all of our 70s data, creating a shapefile for each
tracts70 <- st_read((here("data/shapefiles/US_tract_1970.shp")))

#list file stems
fuel_files <- c(
  "fuel70ct0",
  "fuel70ctp",
  "fuel70ctr",
  "fuel70cts",
  "fuel70ctt",
  "fuel70ctv",
  "fuel70ctw",
  "fuel70ctx",
  "fuel70ctz"
  )

# Loop through each file, join, export 
purrr::walk(fuel_files, function(stem){
  # read csv
  fuel_df <- read_csv(here("data", "fuel_csv", paste0(stem, ".csv")))
  
  # merge
  merged <- tracts70 %>% inner_join(fuel_df, by = "GISJOIN")
  
  # output path
  out_path <- here("data", "shapefiles", "analysisready", paste0(stem, "_tracts1970.shp"))
  
  # write
  st_write(merged, out_path, delete_dsn = TRUE)
})

