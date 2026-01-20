##check 2025 GR age data bias

require(ROracle)

source("~/git/ALOSA.functions/functions/sourcery.R")

sourcery()

#Set account name, password, and server
channel = dbConnect(
  DBI::dbDriver("Oracle"),
  oracle.username.GASP,
  oracle.password.GASP,
  "PTRAN",
  believeNRows = FALSE
) 

#ladder
lad.pri<-get.age.data(2025,siteID = 3,sppID = 3501,AgeStructure = T,PrimaryAger = "Y",channel)
lad.sec<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2025/Ageing/WR re-aged_LILY.csv")

for(i in 1:nrow(lad.sec))
{
  lad.sec$current.age2[i]<-lad.pri$CURRENT_AGE[lad.pri$FISH_ID==lad.sec$sample[i]]
  lad.sec$age.at.first.spawn2[i]<-lad.pri$AGE_AT_FIRST_SPAWN[lad.pri$FISH_ID==lad.sec$sample[i]]
  
  #ape
  mean.age<-mean(c(lad.sec$current.age[i],lad.sec$current.age2[i]))
  lad.sec$ape[i]<-((abs(lad.sec$current.age[i]-mean.age)/mean.age)+(abs(lad.sec$current.age2[i]-mean.age)/mean.age))/2
  
  #CV
  lad.sec$cv[i]<-sqrt((lad.sec$current.age[i]-mean.age)^2+(lad.sec$current.age2[i]-mean.age)^2)/mean.age
}

mean(lad.sec$ape)
mean(lad.sec$cv)

#fishery
