source("Rstartup.R")
#First, lets repeat our national analysis from last time
analysis70_CTT <- read_csv(file.path(data_dir, "fuel_csv/fuel70ctt.csv"))

state_summary70 <- analysis70_CTT %>%
  group_by(STATE) %>%
  summarise(
    CTT001 = sum(CTT001, na.rm = TRUE),
    CTT002 = sum(CTT002, na.rm = TRUE),
    CTT003 = sum(CTT003, na.rm = TRUE),
    CTT004 = sum(CTT004, na.rm = TRUE),
    CTT005 = sum(CTT005, na.rm = TRUE),
    CTT006 = sum(CTT006, na.rm = TRUE),
    CTT007 = sum(CTT007, na.rm = TRUE),
    CTT008 = sum(CTT008, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(
    pctut = 100 * CTT001 / total,
    pctbot = 100 * CTT002 / total,
    pctelec   = 100 * CTT003 / total,
    pctker  = 100 * CTT004 / total,
    pctcoal = 100 * CTT005 / total,
    pctwood  = 100 * CTT006 / total,
    pctoth  = 100 * CTT007 / total,
    pctno   = 100 * CTT008 / total
  )

# summarise() collapses each group into one row
# across() tells R to apply the same function to multiple columns once
# starts_with() grabs just the pct columns
# mean computes average of each column per state
# na. rm = True ignores missing values

p70 <- state_summary70 %>% 
  pivot_longer(cols = starts_with("pct"), names_to = "source", values_to = "percent") %>% # Reshape from wide (many columns) to long (two columns - source and percent)
  mutate(source = recode(source, # Here we are making our labels human readable
                         pctbot = "Bottled Gas",
                         pctcoal = "Coal or Coke",
                         pctelec = "Electricity",
                         pctker = "Kerosene or Fuel Oil",
                         pctno = "None",
                         pctoth = "Other",
                         pctut = "Utility Gas",
                         pctwood = "Wood"
  )) %>%
  ggplot(aes(x = STATE, y = percent, fill = source)) + # Starts ggplot object telling R how to map visually
  geom_bar(stat = "identity", position = "stack") + # adds bar geometry, stat = identity tells it to use actual values not counts, position = stack stacks the bars by fuel type 
  coord_flip() + # Flip for readability, stats run vertically so thats easier to read
  labs(
    title = "Heating Fuels Used in Occupied Units, 1970",
    x = "State",
    y = "Percent",
    fill = "Fuel Type"
  )

# Interesting, lets export
# dir.create("outputs/charts")
ggsave("outputs/charts/1970_statebreakdown.png", plot = p70, width = 10, height = 7, dpi = 300)


# Now since we have cooking fuel data and water heating fuel - lets see what that looks like

analysis70_CTP <- read_csv(file.path(data_dir, "fuel_csv/fuel70ctp.csv"))

state_summary70cook <- analysis70_CTP %>%
  group_by(STATE) %>%
  summarise(
    CTP001 = sum(CTP001, na.rm = TRUE),
    CTP002 = sum(CTP002, na.rm = TRUE),
    CTP003 = sum(CTP003, na.rm = TRUE),
    CTP004 = sum(CTP004, na.rm = TRUE),
    CTP005 = sum(CTP005, na.rm = TRUE),
    CTP006 = sum(CTP006, na.rm = TRUE),
    CTP007 = sum(CTP007, na.rm = TRUE),
    CTP008 = sum(CTP008, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(
    pctut = 100 * CTP001 / total,
    pctbot = 100 * CTP002 / total,
    pctelec   = 100 * CTP003 / total,
    pctker  = 100 * CTP004 / total,
    pctcoal = 100 * CTP005 / total,
    pctwood  = 100 * CTP006 / total,
    pctoth  = 100 * CTP007 / total,
    pctno   = 100 * CTP008 / total
  )

p70cook <- state_summary70cook %>% 
  pivot_longer(cols = starts_with("pct"), names_to = "source", values_to = "percent") %>% # Reshape from wide (many columns) to long (two columns - source and percent)
  mutate(source = recode(source, # Here we are making our labels human readable
                         pctbot = "Bottled Gas",
                         pctcoal = "Coal or Coke",
                         pctelec = "Electricity",
                         pctker = "Kerosene or Fuel Oil",
                         pctno = "None",
                         pctoth = "Other",
                         pctut = "Utility Gas",
                         pctwood = "Wood"
  )) %>%
  ggplot(aes(x = STATE, y = percent, fill = source)) + # Starts ggplot object telling R how to map visually
  geom_bar(stat = "identity", position = "stack") + # adds bar geometry, stat = identity tells it to use actual values not counts, position = stack stacks the bars by fuel type 
  coord_flip() + # Flip for readability, stats run vertically so thats easier to read
  labs(
    title = "Cooking Fuels Used in Occupied Units, 1970",
    x = "State",
    y = "Percent",
    fill = "Fuel Type"
  )

ggsave("outputs/charts/1970_statebreakdown_cook.png", plot = p70cook, width = 10, height = 7, dpi = 300)
