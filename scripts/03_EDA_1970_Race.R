source("Rstartup.R")
# Some analysis for the 1970s, along ethnic lines, for comparison

# Lets start with data about african americans in the 1970s, for heating fuels
analysis70_CTV <- read_csv(file.path(data_dir, "fuel_csv/fuel70CTV.csv"))

state_summary70CTV <- analysis70_CTV %>%
  group_by(STATE) %>%
  summarise(
    CTV001 = sum(CTV001, na.rm = TRUE),
    CTV002 = sum(CTV002, na.rm = TRUE),
    CTV003 = sum(CTV003, na.rm = TRUE),
    CTV004 = sum(CTV004, na.rm = TRUE),
    CTV005 = sum(CTV005, na.rm = TRUE),
    CTV006 = sum(CTV006, na.rm = TRUE),
    CTV007 = sum(CTV007, na.rm = TRUE),
    CTV008 = sum(CTV008, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(
    pctut = 100 * CTV001 / total,
    pctbot = 100 * CTV002 / total,
    pctelec   = 100 * CTV003 / total,
    pctker  = 100 * CTV004 / total,
    pctcoal = 100 * CTV005 / total,
    pctwood  = 100 * CTV006 / total,
    pctoth  = 100 * CTV007 / total,
    pctno   = 100 * CTV008 / total
  )


# And now Latino 
analysis70_CTW <- read_csv(file.path(data_dir, "fuel_csv/fuel70CTW.csv"))

state_summary70CTW <- analysis70_CTW %>%
  group_by(STATE) %>%
  summarise(
    CTW001 = sum(CTW001, na.rm = TRUE),
    CTW002 = sum(CTW002, na.rm = TRUE),
    CTW003 = sum(CTW003, na.rm = TRUE),
    CTW004 = sum(CTW004, na.rm = TRUE),
    CTW005 = sum(CTW005, na.rm = TRUE),
    CTW006 = sum(CTW006, na.rm = TRUE),
    CTW007 = sum(CTW007, na.rm = TRUE),
    CTW008 = sum(CTW008, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(
    pctut = 100 * CTW001 / total,
    pctbot = 100 * CTW002 / total,
    pctelec   = 100 * CTW003 / total,
    pctker  = 100 * CTW004 / total,
    pctcoal = 100 * CTW005 / total,
    pctwood  = 100 * CTW006 / total,
    pctoth  = 100 * CTW007 / total,
    pctno   = 100 * CTW008 / total
  )
