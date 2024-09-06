#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# in_season_assessment_tusket.R
# 
# This function is meant to be run in season to generate estimates of escapement
# at the fish ladders for Vaughan Dam and the Powerhouse on the Tusket River.
# The season setup section only needs to be run the first time to generate the
# files that will be used throughout the season.
# The plotting sections in this script are meant to be catered to the data as
# the season develops. They might work as is, but will likely need tinkering
# to look any good.
#
# Arguments:
#
# year = integer; the year that you wish to analyse; defaults to current year
#
# powerhouse = Boolean; whether or not you wish to include observations from the
# ladder at the Powerhouse. This can only be TRUE for years >= 2022 as no
# observations were made prior to this.
#
# output_folder = character; folder where the plots should go
# 
# new_season = Boolean; whether or not you wish to create the blank CSVs that 
# hold the count observations. We only do this at the beginning of the season.
#
# Created: 2024-08-28 by Logan Gray
# 
# Current issues:  ¯\_(ツ)_/¯
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

in_season_assessment_tusket <- function(
  year = as.integer(format(Sys.Date(), "%Y")),
  powerhouse = FALSE,
  output_folder = path.expand("~")
  ){

  # Setup ####
  library(tidyverse, quietly = TRUE)
  library(scales, quietly = TRUE)
  library(ROracle, quietly = TRUE)
  
  # This sources the functions used to calculate escapement
  source("~/git/ALOSA.functions/functions/sourcery.R")
  sourcery()
  
  # Set up channel connection
  channel = dbConnect(
    DBI::dbDriver("Oracle"),
    username = "GASPEREA",
    password = "_REMOVED",
    dbname = "PTRAN",
    believeNRows = FALSE
  )
  
  # Estimate escapement ####
  
  # Get folder path with count CSVs
  message("This is to avoid a bug, please click cancel")
  choose.files(caption = "This is to avoid a bug, you can close this window")
  message("Select folder with count CSVs")
  data_path <- choose.dir(default = "", caption = "Select a folder with count CSVs")
  data_path <- gsub("\\\\", "/", data_path)
  setwd(data_path)
  
  ## Lake Vaughan Dam (VD) ####
  message("Estimating escapement at Vaughan Dam")
  
  vaughan_file <- paste0("Vaughan ", year, " count data.csv")
  
  while (sink.number() > 0) {sink()}
  sink(tempfile()) # this prevents info from being printed to console
  on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
  counts <- onespecies.river.escapement(
    vaughan_file,
    fixtime = TRUE,
    downstream.migration = FALSE,
    database = FALSE,#when false, arguments below are not used
    year = year,#not used
    site = 2,#not used
    channel = channel#not used
  )
  counts <- round(counts)
  write.csv(counts, file = "vd_in_season_summary.csv", row.names = F)
  counts$location <- "Lake Vaughan"
  
  ## The Powerhouse (PH) ####
  if (powerhouse == TRUE) {
    message("Estimating escapement at Powerhouse")
    
    while (sink.number() > 0) {sink()}
    sink(tempfile()) # this prevents info from being printed to console
    on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
    powerhouse_file <- paste0("Powerhouse ", year, " count data.csv")
    ph_counts <- onespecies.river.escapement(
      powerhouse_file,
      fixtime = TRUE,
      downstream.migration = FALSE,
      database = FALSE,#when false, arguments below are not used
      year = year,#not used
      site = 14,#not used
      channel = channel#not used
    )

    ph_counts <- round(ph_counts)
    write.csv(ph_counts, file = "ph_in_season_summary.csv", row.names = F)
    ph_counts$location <- "Powerhouse"
    counts <- rbind(counts, ph_counts)
  }
  
  ## Clean up counts data ####
  # Clean up NAs and NaNs in counts
  counts <- counts %>%
    mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))


  # Plot: escapement (VD) ####
  message("Making plots")
  
  counts <- counts |> mutate(date = make_date(year, mon, day))
  
  in_season_vd_escapement_plot <- counts |>
    filter(location == "Lake Vaughan") |>
    ggplot(aes(date, total)) +
    geom_path(colour = "#7ECDBB",
              alpha = 0.9,
              linewidth = 1.25) +
    geom_ribbon(
      aes(ymin = clow, ymax = chigh),
      fill = "#7ECDBB",
      colour = "grey90",
      alpha = 0.2
    ) +
    theme_bw() +
    labs(
      title = paste0("Gaspereau escapement for Lake Vaughan Dam for ", year),
      x = "Date",
      y = "fish / day"
      ) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
    scale_y_continuous(
      limits = c(0, max(counts$chigh)),
      breaks = seq(0, max(counts$chigh), by = 10000),
      labels = scales::comma
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.title.x = element_blank()
    )
  
  plot_path <- paste0(output_folder, "/in_season_vd_escapement_plot.png")
  
  ggsave(
    plot_path,
    plot = in_season_vd_escapement_plot,
    width = 6,
    height = 4,
    dpi = 300
  )
  
  # Plot: escapement (VD multi-year) ####
  
  get_VD_multiple_years <- function() {
    
    counts_df <- data.frame()
    
    get_years <- seq(year-5, year-1)
    
    for (i in get_years) {
      tryCatch({
        year_data <- onespecies.river.escapement(
          filename = "dummy_file_name",
          fixtime = TRUE,
          downstream.migration = TRUE,
          database = TRUE,
          year = i,
          site = 2,
          channel = channel
        )
        
        year_data$year <- i
        
        counts_df <- rbind(counts_df, year_data)
        
      }, error = function(e) {
        #message("An error occured: ", conditionMessage(e))
        
        return(counts_df)
        
      })
      
    }
    
    return(counts_df)
    
  }
  
  multi_year_counts <- get_VD_multiple_years()
  counts_for_multi <- counts |> filter(location == "Lake Vaughan")  
  counts_for_multi$year <- year
  counts_for_multi <- counts_for_multi |> select(-date, -location)
  
  multi_year_counts <- rbind(multi_year_counts, counts_for_multi)
  
  # Make sure to filter before creating dates because this screws them up ¯\_(ツ)_/¯
  multi_year_counts <- multi_year_counts %>%
    mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  # You make these all have the same year in their date so they plot nicely
  multi_year_counts <- multi_year_counts |>
    mutate(date = make_date(as.character(!!year), mon, day))
  
  multi_year_counts$year <- as.character(multi_year_counts$year)
  
  # I use the %>% pipe here to use "." to filter data ¯\_(ツ)_/¯
  in_season_multi_year_counts_plot <- multi_year_counts %>%
    group_by(year) %>% 
    ggplot(aes(date, total)) +
    geom_path(
      data = . %>% filter(year != !!year),
      aes(colour = year),
      alpha = 0.25,
      linewidth = 1,
      na.rm = TRUE
    ) +
    geom_path(
      data = . %>% filter(year == !!year),
      aes(colour = year),
      alpha = 0.9,
      linewidth = 1.5,
      na.rm = TRUE
    ) +
    geom_ribbon(
      data = . %>% filter(year == !!year),
      aes(ymin = clow, ymax = chigh),
      fill = year,
      alpha = 0.5
    ) +
    theme_bw() +
    labs(
      title = "Gaspereau escapement for Lake Vaughan Dam",
      x = "Date",
      y = "Fish / day",
      colour = "Year",
      linetype = "Location"
    ) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
    scale_y_continuous(
      limits = c(0, max(multi_year_counts$total)),
      breaks = seq(0, max(multi_year_counts$total), by = 10000),
      labels = scales::comma
    ) +    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_path <- paste0(output_folder, "/in_season_multi_year_counts_plot.png")
  
  ggsave(
    plot_path,
    plot = in_season_multi_year_counts_plot,
    width = 6,
    height = 4,
    dpi = 300
  )

  # Plot: escapement (VD vs PH) ####
  if (powerhouse == TRUE) {
    
    counts <- counts |> mutate(date = make_date(year, mon, day))
    
    in_season_ph_vd_escapement_plot <- counts |>
      ggplot(aes(date, total)) +
      geom_path(
        data = counts |> filter(location == "Lake Vaughan"),
        aes(colour = location),
        alpha = 0.9,
        linewidth = 1.25
      ) +
      geom_ribbon(
        data = counts |> filter(location == "Lake Vaughan"),
        aes(ymin = clow,
            ymax = chigh,
            fill = location),
        alpha = 0.2
      ) +
      geom_path(
        data = counts |> filter(location == "Powerhouse"),
        aes(colour = location),
        alpha = 0.9,
        linewidth = 1.25
      ) +
      geom_ribbon(
        data = counts |> filter(location == "Powerhouse"),
        aes(ymin = clow,
            ymax = chigh,
            fill = location),
        alpha = 0.5
      ) +
      theme_bw() +
      labs(
        title = "Escapement estimates for gaspereau on the Tusket River",
        x = "Date",
        y = "fish / day",
        colour = "Location",
        fill = "Location"
      ) +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
      scale_y_continuous(
        limits = c(0, max(counts$chigh)),
        breaks = seq(0, max(counts$chigh), by = 10000),
        labels = scales::comma
      ) +
      theme(
        legend.position = c(0.850, 0.870),
        legend.background = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        ),
        axis.title.x = element_blank()
      )
    
    plot_path <- paste0(output_folder, "/in_season_ph_vd_escapement_plot.png")
    
    ggsave(
      plot_path,
      plot = in_season_ph_vd_escapement_plot,
      width = 6,
      height = 4,
      dpi = 300
    )
    
  # Plot: escapement (PH multi-year) ####
    # The PH data are troublesome for the earlier years, so we need to tinker
    # with the arguments and functions to get these to work. These are what
    # worked in the past. If these data are cleaned and re-upload to the db
    # in the future, we can re-write this section to be cleaner.
    # 
    # NOTE: I currently have these set to use local files because data on
    # the database needs to be cleaned to get it to work, so I will just use
    # local files until we solve this
    sink(tempfile()) # this prevents info from being printed to console
    on.exit(sink())  # this prevents info from being printed to console
    ph_counts_2022 <- onespecies.partial.river.escapement(
      "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets/Counts/Powerhouse Count Sheet Cleaned 2022.csv",
      fixtime = FALSE,
      database = FALSE,
      2022,
      14,
      channel
    )
    on.exit(NULL) # this prevents info from being printed to console
    ph_counts_2022$year <- as.character(2022)
    ph_counts_2022$location <- "Powerhouse"
    
    sink(tempfile()) # this prevents info from being printed to console
    on.exit(sink())  # this prevents info from being printed to console
    ph_counts_2023 <- onespecies.river.escapement(
      "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/Data Sheets/Powerhouse 2023 count data1.csv",
      fixtime = FALSE,
      downstream.migration = FALSE,
      database = FALSE,
      2023,
      14,
      channel
    )
    on.exit(NULL) # this prevents info from being printed to console
    ph_counts_2023$year <- as.character(2023)
    ph_counts_2023$location <- "Powerhouse"
    
    # Combine with counts dataframe that was previously created and which should
    # have the PH counts from the in season year. This can be probably be done a
    # better way, but where's the fun in that?
    counts_for_multi_ph <- counts |> filter(location == "Powerhouse")
    counts_for_multi_ph$year <- year
    counts_for_multi_ph <- counts_for_multi_ph |> select(-date)
    counts_for_multi_ph <- rbind(counts_for_multi_ph, ph_counts_2022, ph_counts_2023)
    counts_for_multi_ph <- counts_for_multi_ph |> select(-location)
    
    # Make sure to filter before creating dates because this screws them up ¯\_(ツ)_/¯
    counts_for_multi_ph <- counts_for_multi_ph %>%
      mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    # You make these all have the same year in their date so they plot nicely
    counts_for_multi_ph <- counts_for_multi_ph |>
      mutate(date = make_date(as.character(!!year), mon, day))
    
    counts_for_multi_ph$year <- as.character(counts_for_multi_ph$year)
    
    counts_for_multi_ph_plot <- counts_for_multi_ph %>%
      group_by(year) %>% 
      ggplot(aes(date, total)) +
      geom_path(
        data = . %>% filter(year != !!year),
        aes(colour = year),
        alpha = 0.25,
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_path(
        data = . %>% filter(year == !!year),
        aes(colour = year),
        alpha = 0.9,
        linewidth = 1.5,
        na.rm = TRUE
      ) +
      geom_ribbon(
        data = . %>% filter(year == !!year),
        aes(ymin = clow, ymax = chigh),
        fill = year,
        alpha = 0.5
      ) +
      theme_bw() +
      labs(
        title = "Gaspereau escapement for the Powerhouse",
        x = "Date",
        y = "Fish / day",
        colour = "Year"
      ) +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
      scale_y_continuous(
        limits = c(0, max(counts_for_multi_ph$total)),
        breaks = seq(0, max(counts_for_multi_ph$total), by = 1000),
        labels = scales::comma
      ) +    theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    plot_path <- paste0(output_folder, "/counts_for_multi_ph_plot.png")
    
    ggsave(
      plot_path,
      plot = counts_for_multi_ph_plot,
      width = 6,
      height = 4,
      dpi = 300
    )
    
  }
  
}
