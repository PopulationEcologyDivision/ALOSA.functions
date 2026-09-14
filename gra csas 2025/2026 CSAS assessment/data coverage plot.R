#Data coverage plot
#adapted from Logan's Code for the Res Doc
data_years <- 1964:2026
data_vars <- c("Landings", "Escapement", "Biological characteristics")

data_grid <- expand.grid(year = data_years, vars = data_vars) 

# These are the vectors storing the years for which we have data ~~~~~~~~~~~~~~~

# Age
# bio_years <- age_df |> filter(!is.na(CURRENT_AGE)) |> distinct(YEAR) |> arrange(YEAR) |> pull(YEAR)

bio_years <- c(1982:1984,1997:2003,2016,2018,2019,2021:2026)

# Catch
catch_years <- 1964:2026

# Count
# count_years <- esc |> distinct(YEAR) |> arrange(YEAR) |> pull(YEAR)
count_years <- c(1970,1982:1984,1995,1997:2007,2009,2012,2013,2015:2019,2021:2026)

# Mass/Length
#mass_years <- bio |> filter(!is.na(WEIGHT)) |> distinct(YEAR) |> arrange(YEAR) |> pull(YEAR)

# Create a data frame that contains all the data years ~~~~~~~~~~~~~~~~~~~~~~~~~
data_coverage <- data_grid |> 
  mutate(has_data = case_when(
    vars == "Landings" & year %in% catch_years ~ TRUE,
    vars == "Escapement" & year %in% count_years ~ TRUE,
    vars == "Biological characteristics" & year %in% bio_years ~ TRUE,
    vars == "Biological characteristics" & year %in% c(1982:1984) ~ TRUE
  )) |> 
  mutate(has_data = if_else(is.na(has_data), FALSE, has_data)) |> 
  # Add a column to flag some of the data that have different types by year
  mutate(
    fill_colour = case_when(
      # Catch - darker data are "better"
      vars == "Landings" & year %in% 1964:2006 ~ "#2A5783",
      vars == "Landings" & year %in% 2012:2026 ~ "#6495BF",
      vars == "Landings" & year %in% 2008:2011 ~ "#B9DDF1",
      vars == "Landings" & year == 2007 ~ "#E7CA46",
      # Counts - darker data are "better"
      vars == "Escapement" & year %in% c(1970,1982:1984,1995,1997:2003) ~ "#2A5783",# Full count
      vars == "Escapement" & year %in% c(2015:2019, 2021:2026) ~ "#6495BF", # Estimate, two way stratified
      vars == "Escapement" & year == 2013 ~ "#9FCAE6", # Estiamte, two-way stratified, days only
      vars == "Escapement" & year %in% c(2004:2007, 2009, 2012) ~ "#B9DDF1", # Estimate, one-way stratified
      # Biocharacteristics
      vars == "Biological characteristics" & year %in% c(1997:2002) ~ "#2A5783", # proportional sampling
      vars == "Biological characteristics" & year %in% c(2016, 2018, 2019, 2021:2026) ~ "#6495BF", # approximate proportional sampling
      vars == "Biological characteristics" & year %in% c(1982:1984) ~ "#B9DDF1", # twice-per-week sampling
      TRUE ~ "grey90"
    )
  )

data_coverage |> 
  mutate(vars = factor(vars, levels = c("Landings", "Escapement", "Biological characteristics"))) |> 
  ggplot(aes(x = as.factor(year), y = vars)) +
  geom_point(
    aes(fill = fill_colour),        # interior color
    #alpha = 0.9,
    shape = 22,                  # filled square
    color = "black",             # outline color
    size = 7,                    # size of the square
    stroke = 0.5 # thickness of the outline
  ) +
  scale_fill_identity() +
  #scale_fill_manual(values = c("TRUE" = "#41B7C4", "FALSE" = "grey90")) +
  scale_x_discrete(breaks = as.character(seq(1965, 2026, by = 5))) +
  labs(x = "", y = "", fill = "Data available: ") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    legend.position = "top",
    legend.justification = "left",
    axis.ticks.x = element_line()  ) +
  coord_fixed(ratio = 1.8)

#ggsave("data_coverage.png", plot = last_plot(), width = 9, height = 6, units = "in", dpi = 300, bg = "white")



