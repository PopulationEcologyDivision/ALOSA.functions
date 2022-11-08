########
# run through onespecies.river.escapement.R for all years and paste together daily.summary to new large dataframe with same format as old excel sheet.
gimme.the.counts <- function(years,
                             site){
  
  daily.summary <- list()
  for(i in 1:length(years)){
    
    test <- onespecies.river.escapement(year = years[i], site = site, channel = channel)
    test$year <- years[i]
    
    daily.summary[[i]] <- test
    #all.counts <- rbind(all.counts, daily.summary)
    
  }
  all.counts <- do.call("rbind", daily.summary)
  return(all.counts)
}
  
# out <- gimme.the.counts(years = c(2015:2019), site = 3) #
