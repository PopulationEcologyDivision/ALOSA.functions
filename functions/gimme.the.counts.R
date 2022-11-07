########
# run through onespecies.river.escapement.R for all years and paste together daily.summary to new large dataframe with same format as old excel sheet.
gimme.the.counts <- function(years,
                             site){
  all.counts <- NULL
  for(i in length(years)){
    
    daily.summary <- onespecies.river.escapement(year = years[i], site = site, channel = channel)
    all.counts <- rbind(all.counts, daily.summary)
    
  }
  return(all.counts)
}
  
test <- gimme.the.counts(years = c(2013, 2022), site = 2)
