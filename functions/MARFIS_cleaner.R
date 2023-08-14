# A Data cleaner function to catch issues with landsings data from MARFIS
# Requires input of:
# Catch dataframe from MARFIS_all in one.R
#
# This function is used by:
#     - MARFIS_all in one.R
#
# This function uses:
#     - dplyr

MARFIS_cleaner<-function(catch)
{

library(dplyr)

##Do any entries have different units within a single year for a single licence?
catch.lbs<-catch[catch$MEASUREMENT_UNIT=="POUNDS",]
catch.kgs<-catch[catch$MEASUREMENT_UNIT=="KILOGRAMS",]

clbs<-unique(catch.lbs[c("LICENCE_ID","YEAR")])
ckgs<-unique(catch.kgs[c("LICENCE_ID","YEAR")])

double.units<-inner_join(clbs,ckgs)

##Flag any licences with a change in location
#Make a df of licence, year, rivername cleaned
licence.year.river<-catch[,c(1,3,15,16)]
licence.year.river<-licence.year.river[!duplicated(licence.year.river),]
licence.year.river<-licence.year.river[order(licence.year.river$LICENCE_ID,licence.year.river$YEAR),]

##Flag any licences with only a single entry in a year
single.year<-catch[,c(1,3,15)]
single.year<-as.data.frame(table(single.year))
single.year<-single.year[single.year$Freq==1,]

##Should we check for gear code missing? hours fished missing?

##output section
assign("double.units", double.units, envir = .GlobalEnv)
assign("licence.year.river",licence.year.river, envir = .GlobalEnv)
assign("single.year",single.year, envir = .GlobalEnv)

}