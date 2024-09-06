#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# split_species.R
# 
# This function takes species observations and calculates the proportion of 
# each species for each day. The dataframe of proportions and dates that it 
# returns can be used to split the escapement estimates for a site
# into Alewives (A) and Bluebacks (BB).
#
# Arguments:
#
# channel = the database connection
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

split_species <- function(year, siteID, channel) {
  
  library(tidyverse)
  
  biodataA <- get.bio.data(year, siteID, sppID = 3501, channel)
  
  biodataB <- get.bio.data(year, siteID, sppID = 3502, channel)
  
  biodataALL <- rbind(biodataA, biodataB)
  
  # Accessory data have been added to the main biodata file by giving them a
  # FISH_ID starting at 10,000. However, in 2019 there are some FISH_IDs that
  # are greater than 10,000 but they do not represent additional data. Rather,
  # these observations are records of mistakes. The following code will filter
  # the data to exclude this mistakes as using them would introduce duplicates
  # into the data. Even though there are only 12 entries like this, we should
  # still remove them. The weird start at 281001, so we will filter them out.
  biodataALL <- biodataALL |> filter(FISH_ID < 281001)
  
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
  
  biodataALL$SPECIES_ID[biodataALL$SPECIES_ID %in% alewife_names]   <- "A"
  biodataALL$SPECIES_ID[biodataALL$SPECIES_ID %in% blueback_names]  <- "B"
  
  # Proportions ####
  # Calculate the proportion of the escapement estimate for gaspereau that are
  # bluebacks based on the proportions of bluebacks we observed on each date
  proportions <- biodataALL |>
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

