#
#Description: Function to weight biological characteristics data
#
# Inputs:
#biodata from a year
#count data from a year
#
# This function is used by:
# 

# This functions uses:
#missing.days.R

weighting.biodata<-function(bio.data,
                            count.data,
                            missingdays,
                            mergedays,
                            method=c("daily","weekly"))
{
  library(lubridate)
  
  bio.data$date <- make_date(year = bio.data$YEAR, month = bio.data$MON, day = bio.data$DAY)
  count.data$date <- make_date(year = count.data$year, month = count.data$mon, day = count.data$day)
  
  if(method=="weekly")
  {
    bio.data$weekofyear <- epiweek(bio.data$date) #needs to be epiweek so it starts on sunday
    #filter out fish sampled for species Id, they have no measurements and aren't really samples. only used for getting better count proportions
    # bio.data<-rbind(bio.data[!complete.cases(bio.data$NOTES),], #This criteria gives us all rows with NAs in notes
    #                          bio.data[complete.cases(bio.data$NOTES) & bio.data$NOTES!="species ID only",]) #and this criteria gives us rows with notes other than species ID only
    bio.data<-bio.data[!is.na(bio.data$FORK_LENGTH),] #FL generally taken from every fish so use this to exclude species ID only fish
    count.data$weekofyear <- epiweek(count.data$date)
    n.sampled <- aggregate(bio.data$weekofyear, by = list(bio.data$weekofyear), FUN = function(x){length(x[!is.na(x)])})
    colnames(n.sampled) = c("weekofyear", "n.sampled")
    bio.data.with.weights <- merge(bio.data, n.sampled, by = "weekofyear", all.x = T)
    merged.count.data = aggregate(count.data$total, by = list(count.data$weekofyear), FUN = sum)
    colnames(merged.count.data) = c("weekofyear", "merged.total")
    bio.data.with.weights <- merge(bio.data.with.weights, merged.count.data[, c("weekofyear", "merged.total")], by = "weekofyear", all.x = T)
  }
  if(method=="daily")
  {
    bio.data$dayofyear <- yday(bio.data$date)
    #filter out fish sampled for species Id, they have no measurements and aren't really samples. only used for getting better count proportions
    # bio.data<-rbind(bio.data[!complete.cases(bio.data$NOTES),], #This criteria gives us all rows with NAs in notes
                    # bio.data[complete.cases(bio.data$NOTES) & bio.data$NOTES!="species ID only",]) #and this criteria gives us rows with notes other than species ID only
    bio.data<-bio.data[!is.na(bio.data$FORK_LENGTH),] #FL generally taken from every fish so use this to exclude species ID only fish
    count.data$dayofyear <- yday(count.data$date)
    n.sampled <- aggregate(bio.data$dayofyear, by = list(bio.data$dayofyear), FUN = function(x){length(x[!is.na(x)])})
    colnames(n.sampled) = c("dayofyear", "n.sampled")
    bio.data.with.weights <- merge(bio.data, n.sampled, by = "dayofyear", all.x = T)
    #deal with missing days. these need to be manually deteremined outside of this function
    #on days with no sampling, it assigns the count to the next closest day, so that
    #the weightings reflect all fish enumerated. eg, no sampling sunday, all fish smapled on saturday
    #are representative of the fish counted on saturday and sunday.
    for(i in 1:length(missingdays)){
      count.data$dayofyear[count.data$dayofyear==missingdays[i]] <- mergedays[i]
    }
    merged.count.data = aggregate(count.data$total, by = list(count.data$dayofyear), FUN = sum)
    colnames(merged.count.data) = c("dayofyear", "merged.total")
    bio.data.with.weights <- merge(bio.data.with.weights, merged.count.data[, c("dayofyear", "merged.total")], by = "dayofyear", all.x = T)
  }

  bio.data.with.weights$weighting<- bio.data.with.weights$merged.total/bio.data.with.weights$n.sampled
  # Remove infinite values from the weighting column
  bio.data.with.weights$weighting[bio.data.with.weights$weighting == Inf] <- 0
  bio.data<-bio.data.with.weights
  
  return(bio.data)
}


