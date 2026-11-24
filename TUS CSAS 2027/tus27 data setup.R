# TUSKET CSAS 2027 Data Setup
#this script sources in all the data required for the csas
####Setup####
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE)
library(lubridate)
####Count data####
#2018
#no sppecies ID, total count
#not in database FIX ME
# vdcount18<-onespecies.river.escapement(fixtime=T,
#                                        downstream.migration = F,
#                                        database = T,
#                                        year=2018,
#                                        site=2,
#                                        channel=channel)
# vdcount18$year<-2018
#2019
#samples were taken every day, so no need for interpolation
pp19<-split_species(2019,2,channel)
vdcount19<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,
                                       database = T,
                                       year=2019,
                                       site=2,
                                       channel=channel,
                                       species.split=pp19)
awcount19<-vdcount19[[1]]
awcount19$year<-2019
awcount19$date<-make_date(awcount19$year,awcount19$mon,awcount19$day)
awcount19$clow<-ifelse(is.na(awcount19$clow),0,awcount19$clow) #set NAs to 0s in CLs
awcount19$chigh<-ifelse(is.na(awcount19$chigh),0,awcount19$chigh) #set NAs to 0s in CLs
bbcount19<-vdcount19[[2]]
bbcount19$year<-2019
bbcount19$date<-make_date(bbcount19$year,bbcount19$mon,bbcount19$day)
bbcount19$clow<-ifelse(is.na(bbcount19$clow),0,bbcount19$clow) #set NAs to 0s in CLs
bbcount19$chigh<-ifelse(is.na(bbcount19$chigh),0,bbcount19$chigh) #set NAs to 0s in CLs
#2021
# some days missing, but none during bb run
pp21<-split_species(2021,2,channel)
vdcount21<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,
                                       database = T,
                                       year=2021,
                                       site=2,
                                       channel=channel,
                                       species.split=pp21)
awcount21<-vdcount21[[1]]
awcount21$year<-2021
awcount21$date<-make_date(awcount21$year,awcount21$mon,awcount21$day)
awcount21$clow<-ifelse(is.na(awcount21$clow),0,awcount21$clow) #set NAs to 0s in CLs
awcount21$chigh<-ifelse(is.na(awcount21$chigh),0,awcount21$chigh) #set NAs to 0s in CLs
bbcount21<-vdcount21[[2]]
bbcount21$year<-2021
bbcount21$date<-make_date(bbcount21$year,bbcount21$mon,bbcount21$day)
bbcount21$clow<-ifelse(is.na(bbcount21$clow),0,bbcount21$clow) #set NAs to 0s in CLs
bbcount21$chigh<-ifelse(is.na(bbcount21$chigh),0,bbcount21$chigh) #set NAs to 0s in CLs
#2022
# missing one day in bb run, June 17, third last day of the run
# we have counts at powerhouse now
pp22<-split_species(2022,2,channel)
vdcount22<-twospecies.river.escapement(fixtime=T,
                                     downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                     database = T,
                                     year=2022,
                                     site=2,
                                     channel=channel,
                                     species.split=pp22)

phcount22<-onespecies.partial.river.escapement(fixtime=T,
                                               database = T,
                                               year=2022,
                                               site=14,
                                               channel=channel)


awcount22<-vdcount22[[1]]
#add in PH partial counts to total where data exist
for(i in 1:nrow(awcount22))
{
  possiblephcount<-phcount22[[1]]$total[phcount22[[1]]$dayofyear==awcount22$dayofyear[i]]
  if(length(possiblephcount)>0)
  {
    awcount22$total[i]<-awcount22$total[i]+possiblephcount
  }
}
awcount22$year<-2022
awcount22$date<-make_date(awcount22$year,awcount22$mon,awcount22$day)
awcount22$clow<-ifelse(is.na(awcount22$clow),0,awcount22$clow) #set NAs to 0s in CLs
awcount22$chigh<-ifelse(is.na(awcount22$chigh),0,awcount22$chigh) #set NAs to 0s in CLs
#at most 78 BBH would have passed at PH during the counted time so simply ignore them
bbcount22<-vdcount22[[2]]
bbcount22$year<-2022
bbcount22$date<-make_date(bbcount22$year,bbcount22$mon,bbcount22$day)
bbcount22$clow<-ifelse(is.na(bbcount22$clow),0,bbcount22$clow) #set NAs to 0s in CLs
bbcount22$chigh<-ifelse(is.na(bbcount22$chigh),0,bbcount22$chigh) #set NAs to 0s in CLs

#2023
# counts at PH
# lots of missing species ID, need to fill in
pp23<-split_species(2023,2,channel) #this gives us our starting data for fitting a model to BBH proportions
pp23$doy<-yday(pp23$date)
#need to make the dataframe longer, one row for every observed fish
templist<-list()
for(i in 1:nrow(pp23))
{
  templist[[i]]<-data.frame(doy=rep(pp23$doy[i],pp23$total_fish[i]),
                            sp=c(rep(0,pp23$A[i]),rep(1,pp23$BB[i]))
                            )          
}
pp23long<-do.call("rbind",templist)

glmfit <- glm(sp ~ doy, data = pp23long, family = "binomial")

#expand df for all days
doy.df<-data.frame(doy=min(pp23$doy):max(pp23$doy),all=rep(1,length(min(pp23$doy):max(pp23$doy))))
pp23<-merge(pp23,doy.df,all.y=T,by="doy")

#get predictions
ginv <- glmfit$family$linkinv
pp23$pred.prop<-ginv(predict(glmfit,newdata=doy.df,type="link"))

#apply the predictions to the gaps
pp23$BBprop<-ifelse(is.na(pp23$BBprop),pp23$pred.prop,pp23$BBprop)

#fill in the date and day and mon cols for merging
pp23$date<-as.Date(pp23$doy-1,format="%j",origin="1.1.2023") #-1 on doy as R indexes to 0
pp23$day<-day(pp23$date)
pp23$mon<-month(pp23$date)

vdcount23<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = T,
                                       year=2023,
                                       site=2,
                                       channel=channel,
                                       species.split=pp23)

#the 2023 PH counts have a lot of gaps in March and April that makes the script crash
#to avoid this, we will pull the data here, trim before April 25,
#and feed that object directly into the escapement script
ph23.count.data<-get.count.data(2023, 14, channel)
names(ph23.count.data)[1]<-"year"
names(ph23.count.data)[2]<-"mon"
names(ph23.count.data)[3]<-"day"
names(ph23.count.data)[4]<-"strata"
names(ph23.count.data)[5]<-"time"
names(ph23.count.data)[6]<-"count.upstream"
names(ph23.count.data)[7]<-"count.downstream"
names(ph23.count.data)[8]<-"minutes"
names(ph23.count.data)[9]<-"seconds"
ph23.count.data$date<-make_date(ph23.count.data$year,ph23.count.data$mon,ph23.count.data$day)
ph23.count.data<-ph23.count.data[ph23.count.data$date>=as.Date("2023-04-25"),]
ph23.count.data$date<-NULL #remove col so it won't throw an error in the escapement function

phcount23<-twospecies.river.escapement(ph23.count.data,
                                       fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = F, #see note above
                                       year=2023,
                                       site=14,
                                       channel=channel,
                                       species.split=pp23)

#need to properly combine counts from both locations
#alewife
a1<-merge(vdcount23[[1]],phcount23[[1]],by=c("mon","day","dayofyear"),all.x=T)
a1<-a1[sort(a1$dayofyear),]
a1$total<-a1$total.x+a1$total.y
a1$CI.x<-a1$chigh.x-a1$clow.x
a1$CI.y<-a1$chigh.y-a1$clow.y
a1$CI<-sqrt(a1$CI.x^2+a1$CI.y^2)
#fill in gaps. no PH counts early, so jsut use VD counts
a1$total<-ifelse(is.na(a1$total),a1$total.x,a1$total)
a1$CI<-ifelse(is.na(a1$CI),a1$CI.x,a1$CI)
a1$clow<-a1$total-a1$CI/2
a1$chigh<-a1$total+a1$CI/2
#some small numbers are making the CI's weird,
a1$clow<-ifelse(a1$clow<0,0,a1$clow)
a1$chigh<-a1$total+a1$CI/2

awcount23<-a1[,c("mon","day","dayofyear","total","clow","chigh")]
awcount23$sd<-NA #for consistency with other count dfs
awcount23$year<-2023
awcount23$date<-make_date(awcount23$year,awcount23$mon,awcount23$day)
awcount23$clow<-ifelse(is.na(awcount23$clow),0,awcount23$clow) #set NAs to 0s in CLs
awcount23$chigh<-ifelse(is.na(awcount23$chigh),0,awcount23$chigh) #set NAs to 0s in CLs


b1<-merge(vdcount23[[2]],phcount23[[2]],by=c("mon","day","dayofyear"),all.x=T)
b1<-b1[sort(b1$dayofyear),]
b1$total<-b1$total.x+b1$total.y
b1$CI.x<-b1$chigh.x-b1$clow.x
b1$CI.y<-b1$chigh.y-b1$clow.y
b1$CI<-sqrt(b1$CI.x^2+b1$CI.y^2)
#fill in gaps. no PH counts early, so jsut use VD counts
b1$total<-ifelse(is.na(b1$total),b1$total.x,b1$total)
b1$CI<-ifelse(is.na(b1$CI),b1$CI.x,b1$CI)
b1$clow<-b1$total-b1$CI/2
#some small numbers are making the CI's weird,
b1$clow<-ifelse(b1$clow<0,0,b1$clow)
b1$chigh<-b1$total+b1$CI/2

bbcount23<-b1[,c("mon","day","dayofyear","total","clow","chigh")]
bbcount23$sd<-NA #for consistency with other count dfs
bbcount23$year<-2023
bbcount23$date<-make_date(bbcount23$year,bbcount23$mon,bbcount23$day)
bbcount23$clow<-ifelse(is.na(bbcount23$clow),0,bbcount23$clow) #set NAs to 0s in CLs
bbcount23$chigh<-ifelse(is.na(bbcount23$chigh),0,bbcount23$chigh) #set NAs to 0s in CLs

#2024
# counts at PH and VD again.
# some missing species ID to fill in
pp24<-split_species(2024,2,channel) #this gives us our starting data for fitting a model to BBH proportions
pp24$doy<-yday(pp24$date)
#need to make the dataframe longer, one row for every observed fish
templist<-list()
for(i in 1:nrow(pp24))
{
  templist[[i]]<-data.frame(doy=rep(pp24$doy[i],pp24$total_fish[i]),
                            sp=c(rep(0,pp24$A[i]),rep(1,pp24$BB[i]))
  )          
}
pp24long<-do.call("rbind",templist)

glmfit <- glm(sp ~ doy, data = pp24long, family = "binomial")

#expand df for all days
doy.df<-data.frame(doy=min(pp24$doy):max(pp24$doy),all=rep(1,length(min(pp24$doy):max(pp24$doy))))
pp24<-merge(pp24,doy.df,all.y=T,by="doy")

#get predictions
ginv <- glmfit$family$linkinv
pp24$pred.prop<-ginv(predict(glmfit,newdata=doy.df,type="link"))

#apply the predictions to the gaps
pp24$BBprop<-ifelse(is.na(pp24$BBprop),pp24$pred.prop,pp24$BBprop)

#fill in the date and day and mon cols for merging
pp24$date<-as.Date(pp24$doy-1,format="%j",origin="1.1.2024") #-1 on doy as R indexes to 0
pp24$day<-day(pp24$date)
pp24$mon<-month(pp24$date)

vdcount24<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = T,
                                       year=2024,
                                       site=2,
                                       channel=channel,
                                       species.split=pp24)

phcount24<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = T, #see note above
                                       year=2024,
                                       site=14,
                                       channel=channel,
                                       species.split=pp24)

#need to properly combine counts from both locations
#alewife
a1<-merge(vdcount24[[1]],phcount24[[1]],by=c("mon","day","dayofyear"),all.x=T)
a1$total<-a1$total.x+a1$total.y
a1$CI.x<-a1$chigh.x-a1$clow.x
a1$CI.y<-a1$chigh.y-a1$clow.y
a1$CI<-sqrt(a1$CI.x^2+a1$CI.y^2)
#fill in gaps. no PH counts early, so jsut use VD counts
a1$total<-ifelse(is.na(a1$total),a1$total.x,a1$total)
a1$CI<-ifelse(is.na(a1$CI),a1$CI.x,a1$CI)
a1$clow<-a1$total-a1$CI/2
a1$chigh<-a1$total+a1$CI/2
#some small numbers are making the CI's weird,
a1$clow<-ifelse(a1$clow<0,0,a1$clow)
a1$chigh<-a1$total+a1$CI/2

awcount24<-a1[,c("mon","day","dayofyear","total","clow","chigh")]
awcount24$sd<-NA #for consistency with other count dfs
awcount24$year<-2024
awcount24$date<-make_date(awcount24$year,awcount24$mon,awcount24$day)
awcount24$clow<-ifelse(is.na(awcount24$clow),0,awcount24$clow) #set NAs to 0s in CLs
awcount24$chigh<-ifelse(is.na(awcount24$chigh),0,awcount24$chigh) #set NAs to 0s in CLs

b1<-merge(vdcount24[[2]],phcount24[[2]],by=c("mon","day","dayofyear"),all.x=T)
b1$total<-b1$total.x+b1$total.y
b1$CI.x<-b1$chigh.x-b1$clow.x
b1$CI.y<-b1$chigh.y-b1$clow.y
b1$CI<-sqrt(b1$CI.x^2+b1$CI.y^2)
#fill in gaps. no PH counts early, so jsut use VD counts
b1$total<-ifelse(is.na(b1$total),b1$total.x,b1$total)
b1$CI<-ifelse(is.na(b1$CI),b1$CI.x,b1$CI)
b1$clow<-b1$total-b1$CI/2
#some small numbers are making the CI's weird,
b1$clow<-ifelse(b1$clow<0,0,b1$clow)
b1$chigh<-b1$total+b1$CI/2

bbcount24<-b1[,c("mon","day","dayofyear","total","clow","chigh")]
bbcount24$sd<-NA #for consistency with other count dfs
bbcount24$year<-2024
bbcount24$date<-make_date(bbcount24$year,bbcount24$mon,bbcount24$day)
bbcount24$clow<-ifelse(is.na(bbcount24$clow),0,bbcount24$clow) #set NAs to 0s in CLs
bbcount24$chigh<-ifelse(is.na(bbcount24$chigh),0,bbcount24$chigh) #set NAs to 0s in CLs

#2025
# counts at VD only PH sucked
# no missing species ID data to fill in
pp25<-split_species(2025,2,channel) #this gives us our starting data for fitting a model to BBH proportions

vdcount25<-twospecies.river.escapement(fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = T,
                                       year=2025,
                                       site=2,
                                       channel=channel,
                                       species.split=pp25)



#alewife
awcount25<-vdcount25[[1]]
awcount25$year<-2025
awcount25$date<-make_date(awcount25$year,awcount25$mon,awcount25$day)
awcount25$clow<-ifelse(is.na(awcount25$clow),0,awcount25$clow) #set NAs to 0s in CLs
awcount25$chigh<-ifelse(is.na(awcount25$chigh),0,awcount25$chigh) #set NAs to 0s in CLs

bbcount25<-vdcount25[[2]]
bbcount25$year<-2025
bbcount25$date<-make_date(bbcount25$year,bbcount25$mon,bbcount25$day)
bbcount25$clow<-ifelse(is.na(bbcount25$clow),0,bbcount25$clow) #set NAs to 0s in CLs
bbcount25$chigh<-ifelse(is.na(bbcount25$chigh),0,bbcount25$chigh) #set NAs to 0s in CLs

#2026
# counts at VD only PH sucked
#no species ID info
vdcount26<-onespecies.river.escapement(fixtime=T,
                                       downstream.migration = F,#for one of these, we implemented downstream migration because counts at vaughan were messing it up. figuer out where it needs to be
                                       database = T,
                                       year=2026,
                                       site=2,
                                       channel=channel)
vdcount26$year<-2026
vdcount26$date<-make_date(vdcount26$year,vdcount26$mon,vdcount26$day)
vdcount26$clow<-ifelse(is.na(vdcount26$clow),0,vdcount26$clow) #set NAs to 0s in CLs
vdcount26$chigh<-ifelse(is.na(vdcount26$chigh),0,vdcount26$chigh) #set NAs to 0s in CLs

####Bio data####
#alewife
#need to do missing days and merge days here since it is so arbitrary
awbiodata19<-get.bio.data(year = 2019, siteID = 2, sppID = 3501, channel = channel)
missingdays<-missing.days(awbiodata19)
mergedays<-missingdays-c(1,-1,1,-1,1,1,1,1,-1)#change the missing day to the next closest day with samples
awbiodata19<-weighting.biodata(awbiodata19,awcount19,missingdays,mergedays,method="daily")

awbiodata21<-get.bio.data(year = 2021, siteID = 2, sppID = 3501, channel = channel)
missingdays<-missing.days(awbiodata21)
mergedays<-missingdays-c(1,-1,1,-1,1,-1,1,-1)#change the missing day to the next closest day with samples
awbiodata21<-weighting.biodata(awbiodata21,awcount21,missingdays,mergedays,method="daily")

awbiodata22<-get.bio.data(year = 2022, siteID = 2, sppID = 3501, channel = channel)
missingdays<-missing.days(awbiodata22)
mergedays<-missingdays-1#change the missing day to the next closest day with samples
awbiodata22<-weighting.biodata(awbiodata22,awcount22,missingdays,mergedays,method="daily")
#2023 is just species ID data, only used in splitting counts

awbiodata24<-get.bio.data(year = 2024, siteID = 2, sppID = 3501, channel = channel)
#no sampling on weekends at all so we'll use weekly weightings
awbiodata24<-weighting.biodata(awbiodata24,awcount24,missingdays=NULL,mergedays=NULL,method="weekly")

awbiodata25<-get.bio.data(year = 2025, siteID = 2, sppID = 3501, channel = channel)
#no sampling on weekends at all so we'll use weekly weightings
awbiodata25<-weighting.biodata(awbiodata25,awcount25,missingdays,mergedays,method="weekly")

awbiodata19$dayofyear<-awbiodata21$dayofyear<-awbiodata22$dayofyear<-NULL
awbiodata24$weekofyear<-awbiodata25$weekofyear<-NULL
awbiodata<-rbind(awbiodata19,awbiodata21,awbiodata22,awbiodata24,awbiodata25)
rm(list = c("awbiodata19","awbiodata21","awbiodata22","awbiodata24","awbiodata25"))

#bluebacks
bbbiodata19<-get.bio.data(year = 2019, siteID = 2, sppID = 3502, channel = channel)
missingdays<-missing.days(bbbiodata19)
#missing days are the same as awbiodata19, no sampling done on those days in 2019
mergedays<-missingdays-c(1,-1,1,-1,1,1,1,1,-1)#change the missing day to the next closest day with samples
bbbiodata19<-weighting.biodata(bbbiodata19,bbcount19,missingdays,mergedays,method="daily")
#one sample is missing a weighting due to a 0 count that day, give it previous day's weighting
bbbiodata19$weighting[which(is.na(bbbiodata19$weighting))]<-bbbiodata19$weighting[which(is.na(bbbiodata19$weighting))-1]

bbbiodata21<-get.bio.data(year = 2021, siteID = 2, sppID = 3502, channel = channel)
#no missing days, sampling was done and no BBH caught (and therefore not counted) on 
#2021-05-31 2021-06-01 2021-06-02 2021-06-04 2021-06-14
bbbiodata21<-weighting.biodata(bbbiodata21,bbcount21,missingdays=NULL,mergedays=NULL,method="daily")

bbbiodata22<-get.bio.data(year = 2022, siteID = 2, sppID = 3502, channel = channel)
##no missing days, same as 2021
bbbiodata22<-weighting.biodata(bbbiodata22,bbcount22,missingdays=NULL,mergedays=NULL,method="daily")

bbbiodata24<-get.bio.data(year = 2024, siteID = 2, sppID = 3502, channel = channel)
#no sampling on weekends at all so we'll use weekly weightings
bbbiodata24<-weighting.biodata(bbbiodata24,bbcount24,missingdays=NULL,mergedays=NULL,method="weekly")

bbbiodata25<-get.bio.data(year = 2025, siteID = 2, sppID = 3502, channel = channel)
#no sampling on weekends at all so we'll use weekly weightings
bbbiodata25<-weighting.biodata(bbbiodata25,bbcount25,missingdays=NULL,mergedays=NULL,method="weekly")

bbbiodata19$dayofyear<-bbbiodata21$dayofyear<-bbbiodata22$dayofyear<-NULL
bbbiodata24$weekofyear<-bbbiodata25$weekofyear<-NULL
bbbiodata<-rbind(bbbiodata19,bbbiodata21,bbbiodata22,bbbiodata24,bbbiodata25)
rm(list = c("bbbiodata19","bbbiodata21","bbbiodata22","bbbiodata24","bbbiodata25"))

####Age data####
#alewife
agedata19<-get.age.data(year = 2019, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata21<-get.age.data(year = 2021, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata22<-get.age.data(year = 2022, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata22$CURRENT_AGE[agedata22$CURRENT_AGE==2]<-3
agedata22$AGE_AT_FIRST_SPAWN[agedata22$AGE_AT_FIRST_SPAWN==2]<-3
agedata24<-get.age.data(year = 2024, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata25<-get.age.data(year = 2025, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata<-rbind(agedata19,agedata21,agedata22,agedata24,agedata25)
rm(list = c("agedata19","agedata21","agedata22","agedata24","agedata25"))

#blueback
#no point in showing blueback age data not enough

#cleanup
rm(list=c("a1","b1","doy.df","glmfit","ph23.count.data","phcount22","phcount23","phcount24",
          "pp19","pp21","pp22","pp23","pp23long","pp24","pp24long","pp25","summary_data_check",
          "templist","vdcount19","vdcount21","vdcount22","vdcount23","vdcount24","vdcount25"))
