#Day of count based on first day that fish were observed and counted
# Requires input of:
#                   data=dataframe
#                   timetype=column name of time units to extract from 
#                            (DOY, day of run, etc.)
#                   year= year (can be used in loop with multiple years)
#                   condition=column name with conditional info
#                             e.g. total count, age, length

# Requirements: The data set must have a column named 'year'
#
# This function is used by:
#     - onespecies.river.escapement
#
# This function uses:
#     - no other functions


first.last.day<-function(data,timetype,year,condition.name){
  if(!("year" %in% names(data))){
    df.name <- deparse(substitute(data))
    stop(paste("No column named 'year' found in ",df.name,sep=" "))
  }
  z=data[data$year==year,timetype]
  selecttype=data[,names(data)==condition.name]
  first=min(z[selecttype>0], na.rm=TRUE)
  last=max(z[selecttype>0], na.rm=TRUE)
  return(c(first,last))
}