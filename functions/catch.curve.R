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

catch.curve<-function(year,siteID,sppID,channel,modelfit=F)
{
#setup dataframe of possible age categories
min.age<-2
max.age<-10
min.sp<-1
max.sp<-5
age<-min.age:max.age
spawns<-min.sp:max.sp
df<-expand.grid(age,spawns)
colnames(df)<-c("age","spawns")
df$age.mat<-df$age-df$spawns+1
df<-df[df$age.mat>1 & df$age.mat<7,] #filter to categories where fish matured at ages 2-6

#connect to database and pull age data
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
agedata<-get.age.data(year = year, siteID = siteID, sppID = sppID, AgeStructure = T, channel = channel)

#extract comparable age information (# spawns, age at maturity)
agedata$spawns<-agedata$CURRENT_AGE-agedata$AGE_AT_FIRST_SPAWN+1
agedata<-subset(agedata,select=c(spawns,AGE_AT_FIRST_SPAWN))

#summarize age data to counts
age.table<-count(agedata,spawns,AGE_AT_FIRST_SPAWN)
colnames(age.table)<-c("spawns","age.mat","n.fish")

#combine data with template dataframe
ages.table.expanded<-merge(df,age.table,by=c("spawns","age.mat"),all.x=T) 
ages.table.expanded$n.fish[is.na(ages.table.expanded$n.fish)]<-0 #add 0s for unobserved age categories in sample

#fit model
fit<-glm(n.fish~as.factor(age.mat)+spawns,family="poisson",data=ages.table.expanded)
Z<--fit$coef["spawns"]

#output
if(modelfit==F)
{
  return(Z)
} else
{
  return(summary(fit))
}
}