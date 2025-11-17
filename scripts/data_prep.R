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

# Remove NAs - but only if they are NAs of the above columns. B/C some places naturally do not have 'urban area name', but dont need removal
fuel60 <- fuel60 %>%
  filter(!if_any(c(coal, wood, utgas, botgas, elec, kerosene, otherfuel, nofuel), is.na))

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

# Save cleaned and manipulated CSV for further analysis
write_csv(fuel60, file.path(data_dir, "fuel_csv/fuel60clean.csv"))

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

# We now have to filter these 
# Our first dataframe, CTP, for cooking fuel in occupied units overall, drops from 34647 to 33695 when filtering NAs
fuel70_ctp <- fuel70_ctp %>%
  filter(!if_any(c(CTP001, CTP002, CTP003, CTP004, CTP005, CTP006, CTP007, CTP008), is.na))

#CTR, for negro occupied cooking fuel, drops from 34647 to 7471
fuel70_ctr <- fuel70_ctr %>%
  filter(!if_any(c(CTR001, CTR002, CTR003, CTR004, CTR005, CTR006, CTR007, CTR008), is.na))

#CTS, for spanish occupied cooking fuel, drops from 34647 to 4413
fuel70_cts <- fuel70_cts %>%
  filter(!if_any(c(CTS001, CTS002, CTS003, CTS004, CTS005, CTS006, CTS007, CTS008), is.na))

#CTT, for overall occupied units heating fuel drops from 34647 to 33695
fuel70_ctt <- fuel70_ctt %>%
  filter(!if_any(c(CTT001, CTT002, CTT003, CTT004, CTT005, CTT006, CTT007, CTT008), is.na))

#CTV, for negro occupied units heating fuel drops from 34647 to 7471
fuel70_ctv <- fuel70_ctv %>%
  filter(!if_any(c(CTV001, CTV002, CTV003, CTV004, CTV005, CTV006, CTV007, CTV008), is.na))

#CTW for spanish occupied units heating fuel drops from 34647 to 4413
fuel70_ctw <- fuel70_ctw %>%
  filter(!if_any(c(CTW001, CTW002, CTW003, CTW004, CTW005, CTW006, CTW007, CTW008), is.na))

# CTX for all occupied units water heating fuel drops from 34647 to 33695
fuel70_ctx <- fuel70_ctx %>%
  filter(!if_any(c(CTX001, CTX002, CTX003, CTX004, CTX005, CTX006, CTX007, CTX008), is.na))

# CTZ for negro occupied units water heating fuel drops from 34647 to 7471
fuel70_ctz <- fuel70_ctz %>%
  filter(!if_any(c(CTZ001, CTZ002, CTZ003, CTZ004, CTZ005, CTZ006, CTZ007, CTZ008), is.na))

# CT0 for spanish occupied units water heating fuel drops from 34647 to 4413
fuel70_ct0 <- fuel70_ct0 %>%
  filter(!if_any(c(CT0001, CT0002, CT0003, CT0004, CT0005, CT0006, CT0007, CT0008), is.na))

# Save cleaned and manipulated CSVs for further analysis
write_csv(fuel70_ct0, file.path(data_dir, "fuel_csv/fuel70ct0.csv"))

write_csv(fuel70_ctp, file.path(data_dir, "fuel_csv/fuel70ctp.csv"))

write_csv(fuel70_ctr, file.path(data_dir, "fuel_csv/fuel70ctr.csv"))

write_csv(fuel70_cts, file.path(data_dir, "fuel_csv/fuel70cts.csv"))

write_csv(fuel70_ctt, file.path(data_dir, "fuel_csv/fuel70ctt.csv"))

write_csv(fuel70_ctv, file.path(data_dir, "fuel_csv/fuel70ctv.csv"))

write_csv(fuel70_ctw, file.path(data_dir, "fuel_csv/fuel70ctw.csv"))

write_csv(fuel70_ctx, file.path(data_dir, "fuel_csv/fuel70ctx.csv"))

write_csv(fuel70_ctz, file.path(data_dir, "fuel_csv/fuel70ctz.csv"))
