#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# split_species.R
# 
# This function takes species observations (from CSV or db) and calculates the
# proportion of each species for each day. The dataframe of proportions and
# dates that it returns can be used to split the escapement estimates for a site
# into Alewives (A) and Bluebacks (BB).
#
# Arguments:
#
# database = logical; whether or not the data you want are on the database
# 
# biodata_file = character; file path for CSV of biodata
# 
# accessory_data = logical; are there accessory data (i.e. additional species
# IDs) that should be added to the biological data? This is only used when 
# database is FALSE as the accessory data are automatically used from the 
# database if the year is >= 2022.
# 
# accessory_file = character; file path for CSV of accessory biodata
# 
# site_id = integer; ID corresponding to SITENAME in the GASPEREA database; 
# if you do not know the ID that corresponds to the site you want, just
# run the function i.forgot.the.siteIDs() on your database connection to get a
# list of IDs
# 
# year = integer; if you are using the database, you need to indicate which 
# year you would like the data to come from
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

split_species <- function(
  database = FALSE,
  biodata_file = NA,
  accessory_data = FALSE,
  accessory_file = NA,
  site_id = NA,
  year = NA
) {
  
  library(tidyverse)
  
  # Database = FALSE ####
  if (database == FALSE) {
    biodata <- read.csv(biodata_file)
    colnames(biodata) <- toupper(colnames(biodata))
    
    # These are some commonly used column names for the three main variables we
    # need to do the analysis below. We can use these to rename the column names
    # so that the selection of columns doesn't break when the column names are 
    # just a bit different year to year. This is not a problem when hauling the
    # data from the database as the column names there are obviously consistent.
    common_colnames_mon <- c("MONTH", "MON", "M")
    common_colnames_day <- c("DAY", "D")
    common_colnames_species <- c("SPECIES", "SPECIES_ID", "ID", "SPECIES.ID")
    
    new_mon <- data.frame(MON = biodata[, colnames(biodata) %in% common_colnames_mon])
    new_day <- data.frame(DAY = biodata[, colnames(biodata) %in% common_colnames_day])
    new_species <- data.frame(SPECIES_ID = biodata[, colnames(biodata) %in% common_colnames_species])
    biodata <- cbind(new_mon, new_day, new_species)
    
    if (accessory_data == TRUE) {
      # cat("Select your accessory data file (should be CSV)\n")
      # accessory <- read.csv(choose.files(caption = "Select your accessory data file (should be CSV)"))
      accessory <- read.csv(accessory_file)
      colnames(accessory) <- toupper(colnames(accessory))
      
      # Need to do the same re-naming as above
      new_mon <- data.frame(MON = accessory[, colnames(accessory) %in% common_colnames_mon])
      new_day <- data.frame(DAY = accessory[, colnames(accessory) %in% common_colnames_day])
      new_species <- data.frame(SPECIES_ID = accessory[, colnames(accessory) %in% common_colnames_species])
      accessory <- cbind(new_mon, new_day, new_species)
      
      # We we want to add on the accessory data to the biodata
      biodata <- rbind(biodata, accessory)
    }
  }
  
  # Database = TRUE ####
  if (database == TRUE) {
    
    if (is.na(site_id)) {
      cat("No site id selected. You must enter a value for site_id when using the database.\n")
      return()
    }
    
    if (is.na(year)) {
      cat("No year selected. You must enter a value for year when using the database.\n")
      return()
    }
    
    # Database connection
    # These values should already entered in your .Rprofile
    library(ROracle)
    channel <- dbConnect(
      DBI::dbDriver("Oracle"),
      oracle.username.GASP,
      oracle.password.GASP,
      "PTRAN" ,
      believeNRows = FALSE
    )
    
    biodata <- dbReadTable(channel, "ALOSA_FISH_BIO_DATA") |> 
      filter(SITE_ID == site_id & YEAR == year) |>
      select(DAY, MON, SPECIES_ID)
    
    # Only available for years >= 2022
    if (year >= 2022) {
      accessory <- dbReadTable(channel, "ALOSA_FISH_ACCESSORY_BIODATA") |> 
        filter(SITE_ID == site_id & YEAR == year) |>
        select(DAY, MON, SPECIES_ID)
      biodata <- rbind(biodata, accessory)
    }
  }
  
  # Rename the values under SPECIES_ID so they are consistent
  alewife_names <- 
    c(
      "Alewife",
      "alewife",
      "A",
      "a",
      "ALEWIFE",
      3501
    )
  
  blueback_names <-
    c(
      "Blueback",
      "blueback",
      "B",
      "b",
      "BB",
      "BH",
      "bb",
      "bh",
      "Blueback Herring",
      "Blueback herring",
      "blueback herring",
      3502
    )
  
  biodata$SPECIES_ID[biodata$SPECIES_ID %in% alewife_names]   <- "A"
  biodata$SPECIES_ID[biodata$SPECIES_ID %in% blueback_names]  <- "B"
  
  # Proportions ####
  # Calculate the proportion of the escapement estimate for gaspereau that are
  # bluebacks based on the proportions of bluebacks we observed on each date
  proportions <- biodata |>
    mutate(date = make_date(year, MON, DAY)) |>
    group_by(date) |>
    summarize(
      total = n(),
      A = sum(SPECIES_ID == "A"),
      BB = sum(SPECIES_ID == "B"),
      BB_prop = BB / total
    ) |>
    select(date, A, BB, BB_prop)
  
  return(proportions)
  
}
