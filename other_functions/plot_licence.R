plot_licence <- function(licence_number = NULL) {
  
  load("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/Old pulls/marfis_pull.Rdata")
  
  df <- marfis_error_cleaner(catch, years = "all", convert_to_kgs = TRUE, tidy_data = TRUE, correct_eel_pot = TRUE, unknown_rivers_are_na = TRUE, unknown_gear_are_na = TRUE)
  
  if (nrow(df |> filter(LICENCE_ID == licence_number)) == 0) {
    message("Licence number ", licence_number, " not found in data.")
    message("Make sure the licence is a six digit number (e.g. 120118)")
    return(NULL)
  }
  
  df |> 
    filter(LICENCE_ID == licence_number) |> 
    select(RIVERNAME_CLEANED, COUNTY) -> river_county
  
  rivers <- unique(river_county$RIVERNAME_CLEANED)
  counties <- unique(river_county$COUNTY)
  
  df |> 
    mutate(YEAR = year(FV_DATE_FISHED)) |> 
    filter(LICENCE_ID == licence_number) |> 
    group_by(YEAR, GEAR_DESCRIPTION) |> 
    summarise(TOTAL_WEIGHT = sum(FV_WEIGHT)) -> df
  
  # Refactor the GEAR_DESCRIPTION so that the gear with largest catches are the
  # base of the bar plot
  df |> 
    ungroup() |> 
    mutate(GEAR_DESCRIPTION = fct_reorder(GEAR_DESCRIPTION, TOTAL_WEIGHT, .fun = sum)) -> df
  
  df |> 
    ggplot(aes(YEAR, TOTAL_WEIGHT, fill = GEAR_DESCRIPTION)) +
    #geom_col(fill = "#63C3BF", colour = "grey30") +
    geom_col(colour = "grey30") +
    labs(
      title = paste("Reported landings for licence number", licence_number),
      subtitle = paste("River(s):", paste(stringr::str_to_title(rivers), collapse = ", "), "\nCounty(ies):", paste(stringr::str_to_title(counties), collapse = ", ")),
      y = "Total landings (kg)",
      x = "",
      fill = "Gear Type:"
      ) +
    scale_x_continuous(
      breaks = seq(min(df$YEAR, na.rm = TRUE), max(df$YEAR, na.rm = TRUE), by = 1)
    ) +
    scale_y_continuous(
      limits = c(0, max(df$TOTAL_WEIGHT, na.rm = TRUE)),
      breaks = scales::breaks_pretty(n = 10),
      labels = scales::comma
    ) +
    guides(fill = guide_legend(reverse = TRUE, label.position = "left")) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 16),
      panel.grid.minor = element_blank(),
      legend.background = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.background = element_rect(fill = "grey95"),
      text = element_text(family = "Arial")
    )
  
}