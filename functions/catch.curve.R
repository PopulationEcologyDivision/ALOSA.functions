#
#Description:
#
# Inputs:
#channel needs to be called
#
# This function is used by:
#All river assessment RMD files
# Catch curve function

# This functions uses:
#get.age.data

catch.curve<-function()
{}
library(dplyr)


min.age<-2
max.age<-9
min.sp<-1
max.sp<-7
age<-min.age:max.age
spawns<-min.sp:max.sp
df<-expand.grid(age,spawns)
colnames(df)<-c("age","spawns")
df$age.mat<-df$age-df$spawns+1
df<-df[df$age.mat>1 & df$age.mat<7,]


channel=dbConnect(DBI::dbDriver("Oracle"), "GASPEREA", "_REMOVED", "PTRAN" , 
                  believeNRows=FALSE) 
agedat<-get.age.data(year = 2021, siteID = 2, sppID = 3501, AgeStructure = T, channel = channel)


agedata$spawns<-agedata$CURRENT_AGE-agedata$AGE_AT_FIRST_SPAWN+1
agedat<-subset(agedata,select=c(spawns,AGE_AT_FIRST_SPAWN))


age.table<-count(agedat,spawns,AGE_AT_FIRST_SPAWN)
colnames(age.table)<-c("spawns","age.mat","n.fish")

j<-merge(df,age.table,by=c("spawns","age.mat"),all.x=T)
j$n.fish[is.na(j$n.fish)]<-0
fit<-glm(n.fish~as.factor(age.mat)+spawns,family="poisson",data=age.table)
summary(fit)
Z<--fit$coef["spawns"]

fit1<-glm(n.fish~as.factor(age.mat)+spawns,family="poisson",data=j)
summary(fit1)
Z<--fit$coef["spawns"]

return()

}