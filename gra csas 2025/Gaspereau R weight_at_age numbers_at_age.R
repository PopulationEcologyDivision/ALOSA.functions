require(ROracle)
#-------------------------------------------------------------------------------
#...............................................................................
#...............................................................................
#

source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#...............................................................................
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 

count.data<-onespecies.river.escapement(fixtime=T,database=T,year=2024,site=3,channel=channel)
#bio.data<-get.bio.data(year=2023,siteID = 3,sppID=3501, channel)
age.data<-get.age.data(year=2024,siteID = 3,sppID=3501, AgeStructure = T, PrimaryAger="Y", channel)

age.data$PREVIOUS_SPAWNS<-age.data$CURRENT_AGE-age.data$AGE_AT_FIRST_SPAWN
age.datam<-age.data[age.data$SEX_ID==1,]
age.dataf<-age.data[age.data$SEX_ID==2,]
for(i in unique(age.data$CURRENT_AGE))
{
  print(paste0("Age",i))
  print(paste0("Males",round(mean(age.datam$WEIGHT[age.datam$CURRENT_AGE==i],na.rm=T),1)))
  print(paste0("Females",round(mean(age.dataf$WEIGHT[age.dataf$CURRENT_AGE==i],na.rm=T),1)))
}

table(age.datam$CURRENT_AGE,age.datam$PREVIOUS_SPAWNS)


##2025
age.data<-get.age.data(2025,siteID = 3,sppID = 3501,AgeStructure = T,PrimaryAger = "Y",channel)
# table(age25$CURRENT_AGE,age25$AGE_AT_FIRST_SPAWN,age25$SEX_ID) #males = 1, females = 2

#weight-at-age by sex
age.data$PREVIOUS_SPAWNS<-age.data$CURRENT_AGE-age.data$AGE_AT_FIRST_SPAWN
age.datam<-age.data[age.data$SEX_ID==1,]
age.dataf<-age.data[age.data$SEX_ID==2,]
for(i in unique(age.data$CURRENT_AGE))
{
  print(paste0("Age",i))
  print(paste0("Males",round(mean(age.datam$WEIGHT[age.datam$CURRENT_AGE==i],na.rm=T),1)))
  print(paste0("Females",round(mean(age.dataf$WEIGHT[age.dataf$CURRENT_AGE==i],na.rm=T),1)))
}

table(age.datam$CURRENT_AGE,age.datam$PREVIOUS_SPAWNS)
table(age.dataf$CURRENT_AGE,age.dataf$PREVIOUS_SPAWNS)

##look at mean weight by year two ways
year<-2021

count.data<-onespecies.river.escapement(fixtime = F,
                                        downstream.migration = F,
                                        year = year,
                                        database = T,
                                        site = 3,
                                        channel = channel
)


bio.data<-get.bio.data(year,3,3501,channel)

bio.data$date=as.Date(paste(bio.data$DAY,bio.data$MON,bio.data$YEAR,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
bio.data$dayofyear=as.numeric(strftime(bio.data$date, format="%j"))


# scaledata=biodata[biodata$SCALE=="Y",]
n.sampled<-aggregate(bio.data$dayofyear,by=list(bio.data$dayofyear),FUN=function(x){length(x[!is.na(x)])})

colnames(n.sampled)=c("dayofyear","n.sampled")

biodata.with.weights<-merge(bio.data,n.sampled,by="dayofyear",all.x=T)
missingdays<-missing.days(bio.data)
mergedays<-missingdays-1

for(i in 1:length(missingdays)){
  count.data$dayofyear[count.data$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(count.data$total,by=list(count.data$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)


biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled

print(weighted.mean(biodata.with.weights$WEIGHT,biodata.with.weights$weighting,na.rm=T))

age.data<-get.age.data(year=2021,siteID = 3,sppID=3501, AgeStructure = T, PrimaryAger="Y", channel)
print(mean(age.data$WEIGHT,na.rm=T))


all.fish.weight<-data.frame(year=c(2016,2017,2018,2019,2020,2021,2022,2023,2024),
                            weight=c(217.9,NA,222.2,210.8,212.0,NA,230.0,204.7,230.1),
                            weight.aged=c(214.7,NA,222.7,210.4,211.5,NA,231.5,204.6,230.5))