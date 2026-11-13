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
  convert_to_kgs = FALSE,
  tidy_data = FALSE,
  correct_eel_pot = TRUE,
  unknown_rivers_are_na = TRUE,
  unknown_gear_are_na = TRUE
)

df<-df[df$COUNTY=="YARMOUTH COUNTY",]

#at this point we should be ready to check the data or make corrections
#You can use the function present.MARFIS.data to organize it so it matches the logs for easy checking
#eg
catch.sub<-present.MARFIS.data(licence = 120608,year = 2016,catch = df,write=F)
catch.sub$numbah <- catch.sub[catch.sub$MEASUREMENT_UNIT == "KILOGRAMS", "FV_WEIGHT"]/.24
catch.sub <- catch.sub |> arrange(FV_DATE_FISHED)
# library(devtools)
# install_github('Maritimes/Mar.utils')

unique(df$LICENCE_ID[df$YEAR==2017])




#licence report card
licences |> 
  select(
    LICENCE_ID = `Licence Id`,
    LICENCE_TYPE = `Licence Type Description (English)`,
    COUNTY = `County Name 1`,
    PROVINCE = `Province Abbrev (English)`,
    GEAR_DESCRIPTION = `Gear Description (English)`,
    RIVERNAME = `RIVER CONFIRMED`
  ) |> 
  filter(COUNTY %in% c("YARMOUTH")) |> 
  distinct(LICENCE_ID, COUNTY) |> 
  arrange(LICENCE_ID) -> yar_licences

rep.card.YAR<-rep.card[rep.card$LICENCE_ID %in% cb_licences$LICENCE_ID,]

years<-2008:2026
rep.sum<-data.frame(years=years,
                    Catch=NA,
                    DNF=NA,
                    No.Report=NA)
for(yr in 1:length(years))
{
  rep.sum[yr,2]<-table(rep.card.YAR[,yr+1])[1]
  rep.sum[yr,3]<-table(rep.card.YAR[,yr+1])[2]
  rep.sum[yr,4]<-table(rep.card.YAR[,yr+1])[3]
}