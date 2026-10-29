##script to summarize landings data for Yarmouth county
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#bring in previous pull
setwd("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI")
load("29OCT2026 MARFIS PULL.Rdata")

#something's messed up with the note columns this will fix it
catch$NOTES<-iconv(catch$NOTES, from = "latin1", to = "UTF-8", sub = "byte")

#to clean the data, run this. it takes awhile.
source("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/MARFIS_error_cleaner.R")

df <- marfis_error_cleaner(
  catch = catch,
  years = "all",
  convert_to_kgs = TRUE,
  tidy_data = FALSE,
  correct_eel_pot = TRUE,
  unknown_rivers_are_na = TRUE,
  unknown_gear_are_na = TRUE
)

df<-df[df$COUNTY=="YARMOUTH COUNTY",]

#at this point we should be ready to check the data or make corrections
#You can use the function present.MARFIS.data to organize it so it matches the logs for easy checking
#eg
catch.sub<-present.MARFIS.data(licence = 120029,year = 2014,catch = df,write=F)
