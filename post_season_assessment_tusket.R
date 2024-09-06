#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# post_season_assessment_tusket.R
#
# This is a re-design and streamlining of the Assessment Script Tusket code.
# It aims to be used as something we run post-season once we have the counts
# finalized and uploaded to the GASPEREA database. This simplifies the code
# instead of needing to make exceptions for when we have local files.
#
# Arguments:
#
# channel = database connection
#
# year = integer; the year that you wish to analyse; defaults to current year
#
# powerhouse = Boolean; whether or not you wish to include observations from the
# ladder at the Powerhouse. This can only be TRUE for years >= 2022 as no
# observations were made prior to this.
#
# output_folder = character; folder where the plots should go
#
# Created: 2024-08-27 by Logan Gray
#
# Current issues:  ¯\_(ツ)_/¯
# - When year = 2023, the species plot has limited range of days - I think it is
# something to do with the date where the calculation starts?
# - When year = 2024, the data are not uploaded yet so it don-wanna-werk
# - When powerhouse = TRUE, need to use local data as the db ones are impure
#
# To do:
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

post_season_assessment_tusket <- function(
  channel,
  year,
  powerhouse,
  output_folder = path.expand("~")
) {
  
  # Libraries ####
  library(tidyverse, quietly = TRUE)
  options(dplyr.summarise.inform = FALSE)
  library(scales, quietly = TRUE)
  library(ROracle, quietly = TRUE)
  source("~/git/ALOSA.functions/functions/sourcery.R") # Functions we need
  sourcery() # Load functions

  # Haul count data ####
  message(paste0("Getting count data for Vaughan Dam for ", year))
  
  countdata <- dbReadTable(channel, "ALOSA_VIDEO_COUNT_DATA")
  vd_file <- countdata |> filter(YEAR == year & SITE_ID == 2)
  
  if (powerhouse == TRUE) {
    message(paste0("Getting count data for Powerhouse for ", year))
    ph_file <- countdata |> filter(YEAR == year & SITE_ID == 14)
  }
  
  ### Estimate escapement VD ####
  message("Estimating escapement at Vaughan Dam")
  
  while (sink.number() > 0) {sink()}
  sink(tempfile()) # this prevents info from being printed to console
  on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
  counts <- onespecies.river.escapement(
    vd_file,
    fixtime = TRUE,
    downstream.migration = FALSE,
    database = TRUE,
    year = year,
    site = 2,
    channel = channel
  )
  counts$year <- as.character(year)
  counts$location <- "Lake Vaughan"
  
  ## Estimate escapement PH ####
  # The PH data are troublesome for the earlier years, so we need to tinker
  # with the arguments and functions to get these to work. These are what
  # worked in the past. If these data are cleaned and re-upload to the db
  # in the future, we can re-write this section to be cleaner.
  #
  # NOTE: I currently have these set to use local files because data on
  # the database needs to be cleaned to get it to work, so I will just use
  # local files until we solve this
  message("Estimating escapement at Powerhouse")
  
  if (powerhouse == TRUE) {
    
    if (year == 2022) {
      while (sink.number() > 0) {sink()}
      sink(tempfile()) # this prevents info from being printed to console
      on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
      ph_counts <- onespecies.partial.river.escapement(
        "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets/Counts/Powerhouse Count Sheet Cleaned 2022.csv",
        fixtime = FALSE,
        database = FALSE,
        2022,
        14,
        channel
      )
      ph_counts$year <- as.character(year)
      ph_counts$location <- "Powerhouse"
    }
    
    if (year == 2023) {
      while (sink.number() > 0) {sink()}
      sink(tempfile()) # this prevents info from being printed to console
      on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
      ph_counts <- onespecies.river.escapement(
        "R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/Data Sheets/Powerhouse 2023 count data1.csv",
        fixtime = FALSE,
        downstream.migration = FALSE,
        database = FALSE,
        2023,
        14,
        channel
      )
      ph_counts$year <- as.character(year)
      ph_counts$location <- "Powerhouse"
    }
    
    if (year >= 2024) {
      while (sink.number() > 0) {sink()}
      sink(tempfile()) # this prevents info from being printed to console
      on.exit(sink(), add = TRUE)  # this prevents info from being printed to console
      ph_counts <- onespecies.river.escapement(
        ph_file,
        fixtime = FALSE,
        downstream.migration = FALSE,
        database = TRUE,
        year = year,
        site = 2,
        channel = channel
      )
      ph_counts$year <- as.character(year)
      ph_counts$location <- "Powerhouse"
    }
    
    counts <- rbind(counts, ph_counts)
    
  }
  
  # Clean count data
  # The escapement estimates have NAs in some of the columns and we want to deal
  # with these ahead of time so they don't screw with the plots below.
  # Note: I use the %>% pipe here so I can use the "."
  counts <- counts %>%
    mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  # Calculate species proportions
  ## Since species were not checked at PH, we want to combine escapement estimates
  ## for both VD and PH before splitting the estimate. We could change this in
  ## the future if we check for species at PH or if we want to assume that the
  ## species proportions at PH are similar to VD.
  message("Calculating proportion of alewives and bluebacks")
  proportions <- split_species(
    year = year,
    siteID = 2,
    channel = channel
  )
  
  # If we are including PH estimates for escapement, combine them with the
  # estimates for VD so we have a single estimate.
  if (powerhouse == TRUE) {
    counts_species <- counts |>
      group_by(year, mon, day) |>
      summarise(
        total = sum(total),
        sd = sum(sd),
        clow = sum(clow),
        chigh = sum(chigh)
      ) |>
      mutate(date = make_date(year, mon, day)) |>
      ungroup() |>
      select(date, total, clow, chigh) |>
      inner_join(proportions, by = "date") |>
      mutate(B = total * BB_prop, A = total - B) |>
      select(date, A, B) |>
      pivot_longer(cols = c(A, B),
                   names_to = "species",
                   values_to = "total") |>
      mutate(species = str_replace(species, "A", "Alewives")) |>
      mutate(species = str_replace(species, "B", "Bluebacks")) |>
      ungroup()
    
  }
  
  if (powerhouse == FALSE) {
    counts_species <- counts |>
      mutate(date = make_date(year, mon, day)) |>
      select(date, total, clow, chigh) |>
      inner_join(proportions, by = "date") |>
      mutate(B = total * BB_prop, A = total - B) |>
      select(date, A, B) |>
      pivot_longer(cols = c(A, B),
                   names_to = "species",
                   values_to = "total") |>
      mutate(species = str_replace(species, "A", "Alewives")) |>
      mutate(species = str_replace(species, "B", "Bluebacks")) |>
      ungroup()
    
  }
  
  # Plots ####
  message("Creating plots")
  
  ## Escapement (PH vs VD) ####
  if (powerhouse == TRUE) {
    counts <- counts |> mutate(date = make_date(year, mon, day))
    
    ph_vd_escapement_plot <- counts |>
      ggplot(aes(date, total)) +
      geom_path(
        data = counts |> filter(location == "Lake Vaughan"),
        aes(colour = location),
        alpha = 0.9,
        linewidth = 1.25
      ) +
      geom_ribbon(
        data = counts |> filter(location == "Lake Vaughan"),
        aes(
          ymin = clow,
          ymax = chigh,
          fill = location
        ),
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
        aes(
          ymin = clow,
          ymax = chigh,
          fill = location
        ),
        alpha = 0.5
      ) +
      theme_bw() +
      labs(
        title =
          paste0(
            "Escapement estimates for gaspereau on the Tusket River \nfor ",
            year
          ),
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
    
    plot_path <-
      paste0(output_folder, "/ph_vd_escapement_plot.png")
    
    ggsave(
      plot_path,
      plot = ph_vd_escapement_plot,
      width = 6,
      height = 4,
      dpi = 300
    )
    
  }
  
  ## Escapement (VD) ####
  if (powerhouse == FALSE) {
    counts <- counts |>
      mutate(date = make_date(year, mon, day)) |>
      select(-location)
    
    vd_escapement_plot <- counts |>
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
        title = paste0(
          "Escapement estimates for gaspereau at Lake Vaughan Dam \nfor ",
          year
        ),
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
        legend.position = c(0.850, 0.870),
        legend.background = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        ),
        axis.title.x = element_blank()
      )
    
    plot_path <- paste0(output_folder, "/vd_escapement_plot.png")
    
    ggsave(
      plot_path,
      plot = vd_escapement_plot,
      width = 6,
      height = 4,
      dpi = 300
    )
    
  }
  
  ## Escapement multi-year (VD) ####
  # We can look back five years to see how the escapement from the current year
  # compares. Only doing this with VD counts so as not to overwhelm the viewer.
  
  ### Get data ####
  get_VD_multiple_years <- function() {
    counts_df <- data.frame()
    
    get_years <- seq(year - 5, year)
    
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
  
  # Make sure to filter before creating dates because this screws them up ¯\_(ツ)_/¯
  multi_year_counts <- multi_year_counts %>%
    mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  # You make these all have the same year in their date so they plot nicely
  multi_year_counts <- multi_year_counts |>
    mutate(date = make_date(as.character(!!year), mon, day))
  
  multi_year_counts$year <- as.character(multi_year_counts$year)
  
  # I use the %>% pipe here to use "." to filter data ¯\_(ツ)_/¯
  multi_year_counts_plot <- multi_year_counts %>%
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
      alpha = 0.1
    ) +
    theme_bw() +
    labs(
      title = paste0(
        "Estimated escapement for gaspereau at Lake Vaughan Dam \nfor ",
        year
      ),
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
  
  
  # Obviously one has to tinker here to make this supremely beautiful
  plot_path <-
    paste0(output_folder, "/multi_year_counts_plot.png")
  ggsave(
    plot_path,
    plot = multi_year_counts_plot,
    width = 6,
    height = 4,
    dpi = 300
  )
  
  ## Species escapement ####
  # if (year >= 2022) {
  #   species_escapement_plot <- counts_species |>
  #     group_by(species) |>
  #     ggplot(aes(date, total)) +
  #     geom_path(aes(colour = species, fill = species), size = 1) +
  #     theme_bw() +
  #     labs(
  #       title = paste0(
  #         "Total daily escapement estimates for gaspereau by species \nfor ",
  #         year
  #       ),
  #       x = "Date",
  #       y = "fish / day",
  #       colour = "species",
  #       fill = "species"
  #     ) +
  #     scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  #     scale_y_continuous(
  #       limits = c(0, max(counts_species$total)),
  #       breaks = seq(0, max(counts_species$total), by = 10000),
  #       labels = scales::comma
  #     ) +
  #     theme(axis.text.x = element_text(
  #       angle = 45,
  #       vjust = 1,
  #       hjust = 1
  #     ),
  #     axis.title.x = element_blank())
  #   
  #   # Obviously one has to tinker here to make this supremely beautiful
  #   plot_path <-
  #     paste0(output_folder, "/species_escapement_plot.png")
  #   ggsave(
  #     plot_path,
  #     plot = species_escapement_plot,
  #     width = 6,
  #     height = 4,
  #     dpi = 300
  #   )
  # }
}
