#
#Description: creates new column in DF with all catch from MARFIS is converted
#             into KGS instead of a mix of pounds and kilograms.
#
# Inputs: 
#        - data: df of catch from MARFIS. Needs to have columns labelled 'KGS' and "FV_WEIGHT'

#
# This function is used by:
#       - RIVER.summary

# This functions uses:
#     - no user defined functions are used by this function


convert.KGS<-function(data){
  data$KGS[data$MEASUREMENT_UNIT=="KILOGRAMS"]<-
    data$FV_WEIGHT[data$MEASUREMENT_UNIT=="KILOGRAMS"]
  
  data$KGS[data$MEASUREMENT_UNIT=="POUNDS"]<-
    data$FV_WEIGHT[data$MEASUREMENT_UNIT=="POUNDS"]/2.205
  return(data)
}  