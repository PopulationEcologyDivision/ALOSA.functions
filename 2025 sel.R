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

wrl25<-get.age.data(2025,siteID = 3,sppID = 3501,AgeStructure = T,PrimaryAger = "Y",channel)
library(lubridate)
wrl25$DATE<-make_date(year = wrl25$YEAR, month = wrl25$MON, day = wrl25$DAY)
wrl25$DOY<-yday(wrl25$DATE)
wrl25$SEX<-ifelse(wrl25$SEX_ID==1,"M","F")

#fix sample 1310
wrl25$CURRENT_AGE[wrl25$FISH_ID==1310]<-5
wrl25$AGE_AT_FIRST_SPAWN[wrl25$FISH_ID==1310]<-4

table(wrl25$CURRENT_AGE,wrl25$AGE_AT_FIRST_SPAWN)
#create dataframe to put numbers-at-age in
wrl.age25<-data.frame(CURRENT_AGE=rep(c(3,4,4,5,5,5,6,6,6,6,7),2),
                      AGE_AT_FIRST_SPAWN=rep(c(3,3,4,3,4,5,3,4,5,6,4),2),
                      SEX=c(rep("M",11),rep("F",11)),
                      NUMBER_OF_FISH=rep(NA,22))
for(i in 1:nrow(wrl.age25))
{
  wrl.age25$NUMBER_OF_FISH[i]<-nrow(wrl25[wrl25$CURRENT_AGE==wrl.age25$CURRENT_AGE[i] &
                                          wrl25$AGE_AT_FIRST_SPAWN==wrl.age25$AGE_AT_FIRST_SPAWN[i] &
                                          wrl25$SEX==wrl.age25$SEX[i],])
}


wr25count<-858041 #from running escapement function in Gaspereau_2025_Assessment.Rmd
ladd.corr<-wr25count/500

#multiply the numbers-at-age from the combined three fishing stands to get the
#total numbers-at-age for the entire fishery (assuming these three are representative)
wrl.age25$NUMBER_OF_FISH<-wrl.age25$NUMBER_OF_FISH*ladd.corr

#get fishery age and bio data
grf25<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2025/Ageing/GR fishery samples aged_LILY.csv")
grf25<-grf25[-c(4,7,8,9)]
colnames(grf25)<-c("YEAR","FISH_ID","SITE_ID","CURRENT_AGE","AGE_AT_FIRST_SPAWN")

grfbio25<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2025/Gaspereau_fishingsites_biocharacteristics.csv")
grfbio25<-grfbio25[c(2,3,4,5,7,8,9,11)]
colnames(grfbio25)<-c("YEAR","DAY","MON","FISH_ID","SEX_ID","FORK_LENGTH","WEIGHT","SITE_ID")

#merge ages and bio dataframe to get day and month
grf25<-merge(grf25,grfbio25,by=c("YEAR","FISH_ID","SITE_ID"))

#weight fishery ages
grf.landings25<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2025/Gaspereau River 2025 fishery landings.csv")
colnames(grf.landings25)<-c("YEAR","MON","DAY","SITE_ID","LANDINGS","UNIT")
grf.landings25$LANDINGSFISH<-ifelse(grf.landings25$UNIT=="lbs",grf.landings25$LANDINGS*2,grf.landings25$LANDINGS/0.227)

grf.landings25$DATE<-make_date(year = grf.landings25$YEAR, month = grf.landings25$MON, day = grf.landings25$DAY)
grf.landings25$DOY<-yday(grf.landings25$DATE)

grf25<-merge(grf25,grf.landings25,by=c("YEAR","MON","DAY","SITE_ID"))


# grf25$DATE<-make_date(year = grf25$YEAR, month = grf25$MON, day = grf25$DAY)
# grf25$DOY<-yday(grf25$DATE)
#we will scale the aged sample up to the get the total number of aged a,ps fish at each site
#then scale it back down to the sample size, in numbers-at-age,ps

#treat fishery as single point
#daily weights
n.sampled <- aggregate(grf25$DOY, by = list(grf25$DOY), FUN = function(x){length(x[!is.na(x)])})
#fish caught
n.caught <- aggregate(grf.landings25$LANDINGSFISH, by = list(grf.landings25$DOY), FUN = "sum")
weights.df<-merge(n.sampled,n.caught,by="Group.1")
colnames(weights.df)<-c("DOY","sampled","caught")
weights.df$WEIGHTS<-weights.df$caught/weights.df$sampled

#merge back with all data to get numbers-at-age
grf25<-merge(grf25,weights.df[,c(1,4)],by="DOY")

#create dataframe to put numbers-at-age in
grf.age25<-data.frame(CURRENT_AGE=rep(c(3,4,4,5,5,5,6,6,6,6),2),
                      AGE_AT_FIRST_SPAWN=rep(c(3,3,4,3,4,5,3,4,5,6),2),
                      SEX=c(rep("M",10),rep("F",10)),
                      NUMBER_OF_FISH=rep(NA,20))
for(i in 1:nrow(grf.age25))
{
  grf.age25$NUMBER_OF_FISH[i]<-sum(grf25$WEIGHTS[grf25$CURRENT_AGE==grf.age25$CURRENT_AGE[i] &
                                                 grf25$AGE_AT_FIRST_SPAWN==grf.age25$AGE_AT_FIRST_SPAWN[i] &
                                                 grf25$SEX==grf.age25$SEX[i]],na.rm=T)
}

gr25catch<-336873*2 #number of pounds reported by Chelsea in email 2025-06-02
catch.corr<-gr25catch/sum(grf.landings25$LANDINGSFISH)

#multiply the numbers-at-age from the combined three fishing stands to get the
#total numbers-at-age for the entire fishery (assuming these three are representative)
grf.age25$NUMBER_OF_FISH<-grf.age25$NUMBER_OF_FISH*catch.corr

####calc sel####
fishery.age<-aggregate(grf.age25$NUMBER_OF_FISH, by = list(grf.age25$CURRENT_AGE), FUN = "sum")
ladder.age<-aggregate(wrl.age25$NUMBER_OF_FISH, by = list(wrl.age25$CURRENT_AGE), FUN = "sum")
#sum 6 and 7 into plus group for ladder
ladder.age$x[4]<-ladder.age$x[4]+ladder.age$x[5]
ladder.age<-ladder.age[-5,]

sel.df<-merge(fishery.age,ladder.age,by="Group.1")
colnames(sel.df)<-c("age","n.catch","n.esc")
sel.df$u.at.age<-sel.df$n.catch/(sel.df$n.catch+sel.df$n.esc)
sel.df$f.at.age<-(-log(1-sel.df$u.at.age)) #age 5 is max
sel.df$sel<-sel.df$f.at.age/sel.df$f.at.age[3]
