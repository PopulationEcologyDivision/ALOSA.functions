# outputs a dataframe landings data with columns ordered to match the paper logs
# Requires input of:
#                   licence=integer, vector, or range of licences
#                       eg. 120146
#                   year= integer, vector, or range of years
#                   catch= catch dataframe from MARFIS_all in one.R
#                   write=switch to output a csv
#
# Requirements: the "catch" object produced from MARFIS_all in one.R
#
# This function is used by:
#     - no other functions
#
# This function uses:
#     - no other functions

present.MARFIS.data<-function(licence,year,catch,write=F)
{
  catch.sub<-catch[catch$LICENCE_ID %in% licence,]
  catch.sub<-catch.sub[catch.sub$YEAR %in% year,]
  col.order<-c("LICENCE_ID","YEAR","MONTH","DAY","FV_DATE_FISHED","RIVERNAME_LOGBOOK",
               "RIVERNAME_CLEANED","COUNTY","PROVINCE","FV_GEAR_CODE","GEAR_DESCRIPTION",
               "FV_HOURS_FISHED","FV_WEIGHT","MEASUREMENT_UNIT")
  catch.reorder<-catch.sub[,col.order]
  if(write==T)
  {write.csv(catch.reorder,file="MARFIS freshwater logs.csv",row.names=F)}
  return(catch.reorder)
}