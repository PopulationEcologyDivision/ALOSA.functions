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
age.data<-get.age.data(year=2024,siteID = 3,sppID=3501, AgeStructure = T, channel)

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
