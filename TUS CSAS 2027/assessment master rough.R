##starting off a master script for tusket 2027 assessment
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE)
library(lme4)
##catch curve play around
#try multi year
#make df
min.age<-3
max.age<-10
min.sp<-1
max.sp<-5
age<-min.age:max.age
spawns<-min.sp:max.sp
year<-c(2019,2021,2022,2024,2025)
df<-expand.grid(age,spawns,year)
colnames(df)<-c("age","spawns","year")
df$age.mat<-df$age-df$spawns+1
df<-df[df$age.mat>1 & df$age.mat<7,] #filter to categories where fish matured at ages 2-6

#connect to database and pull age data
agedata19<-get.age.data(year = 2019, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata21<-get.age.data(year = 2021, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata22<-get.age.data(year = 2022, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata22$CURRENT_AGE[agedata22$CURRENT_AGE==2]<-3
agedata22$AGE_AT_FIRST_SPAWN[agedata22$AGE_AT_FIRST_SPAWN==2]<-3
agedata24<-get.age.data(year = 2024, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)
agedata25<-get.age.data(year = 2025, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)

agedata<-rbind(agedata19,agedata21,agedata22,agedata24,agedata25)

agedata$spawns<-agedata$CURRENT_AGE-agedata$AGE_AT_FIRST_SPAWN+1
agedata<-subset(agedata,select=c(spawns,AGE_AT_FIRST_SPAWN,YEAR))

#summarize age data to counts
library(dplyr)
age.table<-count(agedata,spawns,AGE_AT_FIRST_SPAWN,YEAR)
colnames(age.table)<-c("spawns","age.mat","year","n.fish")


ages.table.expanded<-merge(df,age.table,by=c("spawns","age.mat","year"),all.x=T)
ages.table.expanded$n.fish[is.na(ages.table.expanded$n.fish)]<-0 #add 0s for unobserved age categories in sample

#fit model
fit<-glm(n.fish~as.factor(age.mat)+spawns+as.factor(year),family="poisson",data=ages.table.expanded)
summary(fit)


fit2<-glmer(n.fish~as.factor(age.mat)+spawns+(1|year),family="poisson",data=ages.table.expanded)
summary(fit2)

library(glmmTMB)
fit3<-glmmTMB(n.fish~as.factor(age.mat)+spawns+(0|year),family="poisson",data=ages.table.expanded)
summary(fit3)


#testing mixed effects
ages.table.expanded$yearcat<-ifelse(ages.table.expanded$year==2019,"A",
                                   ifelse(ages.table.expanded$year==2021,"B",
                                          ifelse(ages.table.expanded$year==2022,"C",
                                                 ifelse(ages.table.expanded$year==2024,"D","E"))))
fit4<-glmer(n.fish~as.factor(age.mat)+spawns+(1|yearcat),family="poisson",data=ages.table.expanded)
summary(fit4)
