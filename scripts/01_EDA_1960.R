# Let's do some exploration of our 1960s data, now properly cleaned
source("Rstartup.R")

analysis60 <- read_csv(file.path(data_dir, "fuel_csv/fuel60clean.csv"))

# Let's first see what states have what energy mixes
state_summary <- analysis60 %>% # Starts pipeline with dataframe called analysis 60, sends forward to create new dataframe, 'state_summary"
  group_by(STATE) %>% # Tells R to treat rows within save value of STATE as belonging to same group
  summarise(across(starts_with("pct"), mean, na.rm = TRUE))
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
dir.create("outputs/charts")
ggsave("outputs/charts/1960_statebreakdown.png", plot = p60, width = 10, height = 7, dpi = 300)

# Of immediate curiosity is whether this captures within-state variation 
# Lets see within state histograms, doing 10 each 
# get sorted list of unique states
state_list <- analysis60 %>%
  distinct(STATE) %>%
  arrange(STATE) %>%
  pull(STATE)

# split into chunks of 10
state_chunks <- split(state_list, ceiling(seq_along(state_list) / 10))

# check it
length(state_chunks)  # how many groups of 10
state_chunks[[1]]     # the first 10

for (i in seq_along(state_chunks)) {
  
  subset_states <- state_chunks[[i]]
  group_label <- paste(range(subset_states), collapse = " – ")
  
  p <- analysis60 %>%
    filter(STATE %in% subset_states) %>%
    pivot_longer(cols = starts_with("pct"), names_to = "source", values_to = "percent") %>%
    mutate(source = recode(source,
                           pctbot = "Bottled Gas",
                           pctcoal = "Coal or Coke",
                           pctelec = "Electricity",
                           pctker = "Kerosene or Fuel Oil",
                           pctno = "None",
                           pctoth = "Other",
                           pctut = "Utility Gas",
                           pctwood = "Wood")) %>%
    ggplot(aes(x = STATE, y = percent, fill = source)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Heating Fuel Shares Within States (1960):", group_label),
      x = "State",
      y = "Percent of Occupied Units",
      fill = "Fuel Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 8)
    )
  
  ggsave(paste0("outputs/charts/1960_boxplots_", i, ".png"),
         plot = p, width = 8, height = 12, dpi = 300)
  
  message("Saved boxplot ", i, " for states: ", paste(subset_states, collapse = ", "))
}
