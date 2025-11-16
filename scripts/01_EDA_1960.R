# Let's do some exploration of our 1960s data, now properly cleaned
source("Rstartup.R")

analysis60 <- read_csv(file.path(data_dir, "fuel_csv/fuel60clean.csv"))

# Let's first see what states have what energy mixes - in the below code we are adding up total counts 
state_summary <- analysis60 %>%
  group_by(STATE) %>%
  summarise(
    coal = sum(coal, na.rm = TRUE),
    wood = sum(wood, na.rm = TRUE),
    utgas = sum(utgas, na.rm = TRUE),
    botgas = sum(botgas, na.rm = TRUE),
    elec = sum(elec, na.rm = TRUE),
    kerosene = sum(kerosene, na.rm = TRUE),
    otherfuel = sum(otherfuel, na.rm = TRUE),
    nofuel = sum(nofuel, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(
    pctcoal = 100 * coal / total,
    pctwood = 100 * wood / total,
    pctut   = 100 * utgas / total,
    pctbot  = 100 * botgas / total,
    pctelec = 100 * elec / total,
    pctker  = 100 * kerosene / total,
    pctoth  = 100 * otherfuel / total,
    pctno   = 100 * nofuel / total
  )

# summarise() collapses each group into one row
# across() tells R to apply the same function to multiple columns once
# starts_with() grabs just the pct columns
# mean computes average of each column per state
# na. rm = True ignores missing values

p60 <- state_summary %>% 
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
    title = "Heating Fuels Used in Occupied Units, 1960",
    x = "State",
    y = "Percent",
    fill = "Fuel Type"
  )

# Interesting, lets export
# dir.create("outputs/charts")
ggsave("outputs/charts/1960_statebreakdown.png", plot = p60, width = 10, height = 7, dpi = 300)

