##Prepare mean weights by year for calculating SSB
#...............................................................................
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
#...............................................................................
#Set account name, password, and server
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 

####data from old assessments####
mw.old.old<-data.frame(year=1982:1984,
                       weight=c(293.5,257.6,271.9))

#numbers pulled from McIntyre et al. 2007
mw.old<-data.frame(year=1997:2002,
                   weight=c(236.6,226.5,208.5,245.4,216.3,238.9))
#data from 2003-2006 not used due to unreconcilable difference in methodology
mw.new<-data.frame(year=c(2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026),
                   weight=NA)
####2016####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2016,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2016,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
#no missing days in 2016

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2016]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2018####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2018,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2018,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#I tested this without, done like this, and done randomly - changes less than 0.1g
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1,missingdays[2]+2,missingdays[3]+1,missingdays[4]-1,missingdays[5]+1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2018]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2019####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2019,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2019,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1,missingdays[2]-2,missingdays[3]+2,missingdays[4]+1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2019]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2021####
count.data<-onespecies.river.escapement(fixtime = F, #needs to be F for 2021
                                        downstream.migration = F,
                                        year = 2021,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2021,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#doing this increases mean weight by 0.3g
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1,missingdays[2]+1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2021]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2022####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2022,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2022,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#doing this decreases mean weight by 0.1g
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1,missingdays[2]+1,missingdays[3]-1,missingdays[4]-1,missingdays[5]-1,missingdays[6]+1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2022]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2023####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2023,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2023,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#no days missed in 2023 per missingdays, however, sampling stopped and an extra ~150k fish went up over 3 weeks
#none of which were sampled. 
mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2023]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2024####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2024,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2024,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#doing this decreases mean weight by 0.07g
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1,missingdays[2]-1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2024]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2025####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2025,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2025,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})
colnames(n.sampled)=c("dayofyear","n.sampled")
biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
# incase a sampling day was missed - must be done manually
#doing this decreases mean weight by 0.065g
missingdays<-missing.days(bio.data)
#merge days selected somewhat arbitrarily - combine counts to nearest day with samples
mergedays<-c(missingdays[1]-1)
for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)
biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2025]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)

####2026####
count.data<-onespecies.river.escapement(fixtime = T,
                                        downstream.migration = F,
                                        year = 2026,
                                        database = T,
                                        site = 3,
                                        channel = channel
)

bio.data<-get.bio.data(2026,3,3501,channel)
bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")
count.data$date=as.Date(paste(count.data$day,count.data$mon,2026,sep="-"),
                      format="%d-%m-%Y")

#no sampling on weekends in 2026, so we do weekyl weights instead
library(lubridate)
bio.data$weekofyear <- epiweek(bio.data$date)
count.data$weekofyear <- epiweek(count.data$date)
n.sampled <- aggregate(bio.data$weekofyear, by = list(bio.data$weekofyear), FUN = function(x){length(x[!is.na(x)])})
colnames(n.sampled) = c("weekofyear", "n.sampled")
biodata.with.weights <- merge(bio.data, n.sampled, by = "weekofyear", all.x = T)
#no need for mergedays here
mergedcountdata = aggregate(count.data$total, by = list(count.data$weekofyear), FUN = sum)
colnames(mergedcountdata) = c("weekofyear", "merged.total")
biodata.with.weights <- merge(biodata.with.weights, mergedcountdata[, c("weekofyear", "merged.total")], by = "weekofyear", all.x = T)

biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
mw.new$weight[mw.new$year==2026]<-weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T)
####cleanup####
rm(bio.data,biodata.with.weights,count.data,mergedcountdata,n.sampled,summary_data_check)
