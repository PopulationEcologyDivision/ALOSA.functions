##subsampling biodata
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 

library(lubridate)
library(nnet)

countdata<-onespecies.river.escapement(
  fixtime = TRUE,
  downstream.migration = FALSE,
  database = TRUE,
  year = 2024,
  site = 3,
  channel = channel)
  


dat<-get.bio.data(2024,3,3501,channel)
dat<-dat[order(dat$FISH_ID),]
dat$DATE<-as.Date(paste(dat$DAY,dat$MON,dat$YEAR,sep="-"),format="%d-%m-%Y")
dat$DOY<-yday(dat$DATE)


#weight data
n.sampled <- aggregate(dat$DOY, by = list(dat$DOY), FUN = function(x){length(x[!is.na(x)])})
colnames(n.sampled) = c("DOY", "n.sampled")
dat <- merge(dat, n.sampled, by = "DOY", all.x = T)
mergedcountdata = aggregate(countdata$total, by = list(countdata$dayofyear), FUN = sum)
colnames(mergedcountdata) = c("DOY", "total")
dat<-merge(dat, mergedcountdata[,c("DOY","total")], by="DOY", all.x = T)

dat$weighting <- dat$total / dat$n.sampled
# Remove infinite values from the weighting column
dat$weighting[dat$weighting == Inf] <- 0

#weighted mean
all.wwm<-weighted.mean(dat$WEIGHT,dat$weighting)

#weekends
dat$DOW<-dat$DOY%%7+1
dat$weekend<-ifelse(dat$DOW %in% c(1,3,5),"Y","N")

summary(glm(WEIGHT~weekend,weights=weighting,data=dat))





#agedata
# agedat<-get.age.data(2021,3,3501,AgeStructure = "N",channel=channel)
agedat$DATE<-as.Date(paste(agedat$DAY,agedat$MON,agedat$YEAR,sep="-"),format="%d-%m-%Y")
agedat$DOY<-yday(agedat$DATE)
agedat$DOW<-agedat$DOY%%7+1


#we want to cycle through all "possible" weekends
out<-list()
for(i in 1:7)
{
  if(i==7)
  {agedat$weekend<-ifelse(agedat$DOW %in% c(i,1),"Y","N")}#to get the combo of the 1st and 7th day
  else{agedat$weekend<-ifelse(agedat$DOW %in% c(i,i+1),"Y","N")}
  x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
  print(x)
  out[[i]]<-chisq.test(x,simulate.p.value=T)
}

##3 days a week
agedat$weekend<-ifelse(agedat$DOW %in% c(1,3,5),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(2,4,6),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(3,5,7),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(4,6,1),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(5,7,2),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(6,1,3),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)
agedat$weekend<-ifelse(agedat$DOW %in% c(7,2,4),"Y","N")
x<-table(agedat$AGE_AT_FIRST_SPAWN,agedat$weekend)
chisq.test(x,simulate.p.value=T)


fit<-multinom(AGE_AT_FIRST_SPAWN~DOW,data=agedat)
z <- summary(fit)$coefficients/summary(fit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
