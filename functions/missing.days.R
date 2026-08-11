# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description: Find missing sampling days
#
# Inputs: biocharacteristics data with columns for day, month, and year
#
# This function is used by:
# 
# This functions uses:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


missing.days<-function(biodata){
  
  # Make the column names all lower case so we can use this on data with different
  # styles of column names
  names(biodata) <- tolower(names(biodata))
  
  biodata$date = as.Date(paste(biodata$day, biodata$mon, biodata$year, sep = "-"), format = "%d-%m-%Y")
  
  #dayofyear uses "strftime" to evaluate which day of the year each date aligns with
  biodata$dayofyear = as.numeric(strftime(biodata$date, format = "%j"))
  
  #calculates the dayofyear which the first sample was taken
  startofrun = min(biodata$dayofyear, na.rm = T)
  
  #calculates dayofyear which last sample was taken
  endofrun = max(biodata$dayofyear, na.rm = T)
  
  #length of the run is categorized as the start to the end
  alldays = startofrun:endofrun
  
  #missing days are any days within alldays(length of the run) which have no reported samples 
  missingdays = alldays[!(alldays %in% biodata$dayofyear)]
  
  #ensures the dates are printed in date format
  #missingdates=as.numeric(strftime(missingdays,format="%j"))
  missingdates = as.Date(missingdays, origin = paste((unique(max(biodata$year)) - 1), 12, 31, sep = "-"))
  
  if(length(missingdates > 0)) {
    cat("Missing days:", "\n")
    cat(as.character(missingdates))
    cat("\n", "\n")
    
  } else {
    cat("No missing sampling dates", "\n", "\n")
  }
  
  return(missingdays)
  
}

