source("Rstartup.R")

# Load CSVs of fuel data for 1960s

fuel60 <- read_csv(file.path(data_dir, "fuel_csv/fuel60.csv"))

# Rename for readability - 60s dataset
fuel60 <- fuel60 %>%
  rename(
    coal        = B73001,
    wood                = B73002,
    utgas         = B73003,
    botgas   = B73004,
    elec         = B73005,
    kerosene   = B73006,
    otherfuel          = B73007,
    nofuel     = B73008
  )

# Create total, for percentages 
fuel60 <- fuel60 %>%
  mutate(
    total = coal + wood + utgas + botgas + elec + kerosene + otherfuel + nofuel
  )

# Create percentages 
fuel60 <- fuel60 %>%
  mutate(
    pctcoal = (coal / total) * 100,
    pctwood = (wood / total) * 100,
    pctut = (utgas / total) * 100,
    pctbot = (botgas / total) * 100,
    pctelec = (elec / total) * 100,
    pctker = (kerosene / total) * 100,
    pctoth = (otherfuel / total) * 100,
    pctno = (nofuel / total) *100
  )


# Load CSVs of fuel data for 1970
fuel70 <- read_csv(file.path(data_dir, "fuel_csv/fuel70.csv"))

# We need to disag these because they are like 9 tables
# Step 1 to do so is select columns to keep for each 
geo_cols <- c(
  "GISJOIN", "YEAR", "REGIONA", "DIVISIONA", "STATE", "STATEA", "COUNTY", "COUNTYA",
  "CTY_SUBA", "PLACEA", "TRACTA", "SCSAA", "SMSAA", "URB_AREAA",
  "AREANAME", "CENCNTY", "CBD", "SEA"
)

# Create one tibble per table, selecting its relevant NHGIS code range
fuel70_ctp <- fuel70 %>% select(all_of(geo_cols), starts_with("CTP")) # CTP = Cooking fuel in occupied units
fuel70_ctr <- fuel70 %>% select(all_of(geo_cols), starts_with("CTR")) # CTR = Cooking fuel in negro occupied units
fuel70_cts <- fuel70 %>% select(all_of(geo_cols), starts_with("CTS")) # CTS = Cooking fuel in Spanish American occupied units
fuel70_ctt <- fuel70 %>% select(all_of(geo_cols), starts_with("CTT")) # CTT = House heating fuel in occupied units
fuel70_ctv <- fuel70 %>% select(all_of(geo_cols), starts_with("CTV")) # CTV = House heating fuel in negro occupied units
fuel70_ctw <- fuel70 %>% select(all_of(geo_cols), starts_with("CTW")) # CTW = House heating fuel in Spanish American occupied units
fuel70_ctx <- fuel70 %>% select(all_of(geo_cols), starts_with("CTX")) # CTX = Water heating fuel in occupied units
fuel70_ctz <- fuel70 %>% select(all_of(geo_cols), starts_with("CTZ")) # CTZ = Water heating fuel in negro occupied units
fuel70_ct0 <- fuel70 %>% select(all_of(geo_cols), starts_with("CT0")) # CT0 = Water heating fuel in Spanish American occupied units


# For all of the above 
# 1 = utility gas
# 2 = bottled gas
# 3 = electricity
# 4 = kerosene 
# 5 = coal 
# 6 = wood
# 7 = other
# 8 = none

# Calculate totals per category

