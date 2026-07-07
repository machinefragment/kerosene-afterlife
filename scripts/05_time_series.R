# Time Series of Centralized VS not centralized heating
source("Rstartup.R")


# states / counties of interest

target_states   <- c("California", "Washington")
target_counties <- c("Alameda", "San Francisco", "King")

# Load data for each decade
files <- list(
  "1940" = "data/forties/nhgis0041_ds76_1940_tract.csv",
  "1950" = "data/fifties/nhgis0040_ds82_1950_tract.csv",
  "1960" = "data/fuel_csv/fuel60.csv",
  "1970" = "data/fuel_csv/fuel70.csv",
  "1980" = "data/eighties/nhgis0041_ds107_1980_tract.csv"
)

# apply filter
data_list <- lapply(files, \(f) {
  read_csv(f) |>
    filter(STATE %in% target_states, COUNTY %in% target_counties)
})

# get each DF back out to manipulate
list2env(setNames(data_list, paste0("data_", names(data_list))), envir = .GlobalEnv)

# start manipulating into county shape
county_list <- lapply(data_list, \(df) {
  df |>
    group_by(STATE, COUNTY) |>
    summarise(across(where(is.numeric) & !any_of(c("GISJOIN", "YEAR", "STATEA", "COUNTYA", "PRETRACTA", "TRACTA")), 
                     sum, na.rm = TRUE), 
              .groups = "drop")
})

all_counties <- bind_rows(county_list, .id = "year")

king          <- filter(all_counties, COUNTY == "King")
alameda       <- filter(all_counties, COUNTY == "Alameda")
san_francisco <- filter(all_counties, COUNTY == "San Francisco")

# create output folder
dir.create("data/county", recursive = TRUE)

# write data, clean further in excel
write_csv(king,          "data/county/king.csv")
write_csv(alameda,       "data/county/alameda.csv")
write_csv(san_francisco, "data/county/san_francisco.csv")
