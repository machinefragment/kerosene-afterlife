source("Rstartup.R")

# Comparing Bottled Gas

fuel60clean <- read_csv("data/fuel_csv/fuel60clean.csv")
fuel70cleanCTT <- read_csv("data/fuel_csv/fuel70ctt.csv")

# Lets start just with raw counts by state
# In fuel 60 clean we are looking at 'botgas', in fuel 70 clean it is 'CTT002'

fuel60_by_state <- fuel60clean %>%
  group_by(STATE) %>%
  summarise(
    total_botgas = sum(botgas, na.rm = TRUE),
    .groups = "drop"
  )

fuel70_by_state <- fuel70cleanCTT %>%
  group_by(STATE) %>%
  summarise(
    total_CTT002 = sum(CTT002, na.rm = TRUE),
    .groups = "drop"
  )
