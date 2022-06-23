#
# Calculates the percent of fish in each strata
#
# Inputs:
#        - strata as a single number. Does not work with vectors
#
# This function is used by:
#     - onespecies.river.escapement

# This functions uses:
#     - no other user defined functions used.

strata.percent=function(strata,data){
  if (!strata==1){
    stop ("More than one strata entered. Please change.")
  }
  total.s=sum(data$total[data$strata==strata])
  total.all=sum(data$total,na.rm=TRUE)
  return(paste(total.s," (",round(total.s/total.all*100,digits=3),"%)",
               sep=""))
}