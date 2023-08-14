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
  catch.reorder<-catch.sub[,c(1,3,10,11,2,15,16,17,5,6,4,8,9)]
  if(write==T)
  {write.csv(catch.reorder,file="MARFIS freshwater logs.csv",row.names=F)}
  return(catch.reorder)
}