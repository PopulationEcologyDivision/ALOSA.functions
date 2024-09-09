#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# split_species.R
# 
# This function takes species observations and calculates the proportion of 
# each species for each day. The dataframe of proportions and dates that it 
# returns can be used to split the escapement estimates for a site
# into Alewives (A) and Bluebacks (BB). Some of this was harvested from Mark's
# split.spp.R.
#
# Arguments:
#
# year = integer; if you are using the database, you need to indicate which 
# year you would like the data to come from
# 
# site_id = integer; ID corresponding to SITENAME in the GASPEREA database; 
# if you do not know the ID that corresponds to the site you want, just
# run the function i.forgot.the.siteIDs() on your database connection to get a
# list of IDs
# 
# channel = the database connection
# 
# biodata = character; a path to your biodata CSV that contains accessory data
# appended on the end if that is applicable.
# 
# Functions: format.BIODATA.onesite
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

split_species <- function(year, siteID, channel, biodata_CSV = NULL) {
  
  library(tidyverse)
  
  # CSV route ####
  if (!is.null(biodata_CSV)) {
    
    # Your biodata CSV should have your accessory data (observations that are
    # just for species ID) attached on the end i.e. a bunch of rows of species
    # identifications without any weight, length, or sex data.
    
    # We want to format the column names so they are standardised.
    biodata <- format.BIODATA.onesite(biodata_CSV)
    
    # Get columns we want and make sure the column names are correct
    biodata <- biodata[, c("DAY", "MON", "SPECIES_ID")]
    goodnames <- c("DAY", "MON", "SPECIES_ID")
    name_check <- colnames(biodata) == goodnames
    if(any(name_check == FALSE)){
      stop("The column names in your biodata do not match DAY, MON, SPECIES_ID format")
    }
  
  # Database route ####
  } else {
    
    # This gets biodata from the database.
    biodataA <- get.bio.data(year, siteID, sppID = 3501, channel)
    biodataB <- get.bio.data(year, siteID, sppID = 3502, channel)
    biodata <- rbind(biodataA, biodataB)
    
    # Accessory data have been added to the main biodata file by giving them a
    # FISH_ID starting at 10,000. However, in 2019 there are some FISH_IDs that
    # are greater than 10,000 but they do not represent additional data. Rather,
    # these observations are records of mistakes. The following code will filter
    # the data to exclude this mistakes as using them would introduce duplicates
    # into the data. Even though there are only 12 entries like this, we should
    # still remove them. The weird start at 281001, so we will filter them out.
    biodata <- biodata |> filter(FISH_ID < 281001)
    
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
      total_fish = n(),
      A = sum(SPECIES_ID == "A"),
      BB = sum(SPECIES_ID == "B"),
      BB_prop = BB / total_fish
    ) |>
    select(date, total_fish, A, BB, BB_prop)
  
  return(proportions)
  
}

