# Description:
#
# Inputs:
# - countdata = the daily.summary object from escapement script for site
#   produced by the onespecies.river.escapement.R function
# - biodata = the csv containing the physical measurements for the site
# - weekly = whether or not you want to use weeks of the year to do the weighing
#   or not; useful for when weekends were not sampled i.e. 2024
# - year = the year of the observations
# - seed = any number, it allows the sampling to be pseudo-random
# - nsamples = the number of scale samples we want to look at (arbitrary)
#
# This function is used by: NA
#
# This functions uses: NA

ageing.selection.test <- function(
    countdata, 
    biodata,
    weekly = FALSE,
    year,
    seed = 42069,
    nsamples = 500,
    species = "both"
){
  
  library(lubridate)
  
  biodata$date <- make_date(year = biodata$year, month = biodata$mon, day = biodata$day)
  countdata$date <- make_date(year = year, month = countdata$mon, day = countdata$day)
  
  if(weekly == TRUE){
    
    biodata$weekofyear <- week(biodata$date)
    
    countdata$weekofyear <- week(countdata$date)
    
  }
  
  if(weekly == FALSE){
    
    biodata$dayofyear <- yday(biodata$date)
    
    countdata$dayofyear <- yday(countdata$date)
    
  }
  
  # only sample data with scale samples
  scaledata <- biodata[biodata$scale == "Y", ]
  
  # take out the species you want if determined
  if(species == "A"){
    
    A.names = c("Alewife", "Alewife ", "A", "a", "alewife", "ale", "Ale", "ALE", "AL")
    scaledata <- scaledata %>% filter(species %in% A.names)
    print("Only sampling Alewives")
    
  } else if (species == "B"){
    
    B.names = c("Blueback Herring", "blueback herring", "Blueback", "blueback", "BBH", "b", "B", "bbh", "Bbh", "Bb", "BB", "bb")
    scaledata <- scaledata %>% filter(species %in% B.names)
    print("Only sampling Bluebacks")
    
  } else {print("Sampling both species")}
  
  # see how many samples were collected for each week
  
  if(weekly == TRUE){
    
    n.sampled <- aggregate(scaledata$weekofyear, by = list(scaledata$weekofyear), FUN = function(x){length(x[!is.na(x)])})
    colnames(n.sampled) = c("weekofyear", "n.sampled")
    biodata.with.weights <- merge(biodata, n.sampled, by = "weekofyear", all.x = T)
    mergedcountdata = aggregate(countdata$total, by = list(countdata$weekofyear), FUN = sum)
    colnames(mergedcountdata) = c("weekofyear", "merged.number.up")
    biodata.with.weights <- merge(biodata.with.weights, mergedcountdata[ , c("weekofyear", "merged.number.up")], by = "weekofyear", all.x = T)
    
  }
  
  if(weekly == FALSE){
    
    n.sampled <- aggregate(scaledata$dayofyear, by = list(scaledata$dayofyear), FUN = function(x){length(x[!is.na(x)])})
    colnames(n.sampled) = c("dayofyear", "n.sampled")
    biodata.with.weights <- merge(biodata, n.sampled, by = "dayofyear", all.x = T)
    mergedcountdata = aggregate(countdata$total, by = list(countdata$dayofyear), FUN = sum)
    colnames(mergedcountdata) = c("dayofyear", "merged.number.up")
    biodata.with.weights <- merge(biodata.with.weights, mergedcountdata[ , c("dayofyear", "merged.number.up")], by = "dayofyear", all.x = T)
    
  }
  
  biodata.with.weights$weighting <- biodata.with.weights$merged.number.up / biodata.with.weights$n.sampled
  
  # Remove infinite values from the weighting column
  biodata.with.weights$weighting[biodata.with.weights$weighting == Inf] <- 0
  
  # sample
  set.seed(seed)
  
  ladd.sample <- data.frame(
    sample(
      biodata.with.weights$sample[biodata.with.weights$scale == 'Y'],
      nsamples,
      replace = F,
      prob = biodata.with.weights$weighting[biodata.with.weights$scale == 'Y']
    )
  )
  
  colnames(ladd.sample) <- ("sample")
  
  ladd.sample$to.be.aged <- "Y"
  
  ladd.sample <- ladd.sample[with(ladd.sample, order(sample)), ]
  
  out <- merge(biodata.with.weights, ladd.sample, by = "sample", all.y = T)
  
  if(species == "A"){out$species <- "A"}
  
  if(species == "B"){out$species <- "B"}
  
  if(species == "both"){out$species <- NA}
  
  out$current.age = NA
  
  out$age.at.first.spawn = NA
  
  out$age.structure.sample = "T"
  
  out <- out[ , c("year", "sample", "species", "current.age", "age.at.first.spawn", "notes", "age.structure.sample")]
  
  names(out) <- c("year", "sample", "species", "current.age", "age.at.first.spawn", "notes", "age.structure.sample")
  
  out$replacementfor.which.sample = NA
  
  print("Writing scales to be aged to csv file in working directory")
  
  write.csv(
    out,
    file = paste("scales_to_age_", year, ".csv", sep = ""),
    row.names = F,
    na = ""
  )
  
}
