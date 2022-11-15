##Tusket season closure sim##
#Using escapement data paired with daily landings data from 2021 and 2022
#I will see what effect closing various days will have on escapement,
#by assuming all fish not caught ascend the ladder the same day

####SETUP####
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#...............................................................................
#Set account name, password, and server
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 


####Escapement####
##2021
daily.summary.2021<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2021/Data Analysis/TUSKET_2021_ESCAPEMENT_daily summary.csv")

##2022
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets")

accessory.datafile="TUSKET_2022_VAUGHAN_accessory_data.csv"
species.split<-split.spp(year=2022,site=2,channel,accessory.datafile)

out<-twospecies.river.escapement(year=2022,
                                 site=2,
                                 channel=channel,
                                 species.split=species.split)
##so few bluebacks will just use Alewife for simplicity
daily.summary.2022<-out[[172]] ##This still has dayofyear out of order
daily.summary.2022$dayofyear<-as.integer(levels(daily.summary.2022$dayofyear))

##Add in date column
daily.summary.2021$date<-as.POSIXct(paste("2021",daily.summary.2021$mon,daily.summary.2021$day,sep="-"))
daily.summary.2022$date<-as.POSIXct(paste("2022",daily.summary.2022$mon,daily.summary.2022$day,sep="-"))
####LANDINGS####
##Licence numbers from Brady and Jarrad
g2021.lic<-c(120103,120118,120169,120336,120757)
d2021.lic<-c(120410,120461,120529,120560,303395)
d2022.lic<-c(120410,120461,120514,120529,120608)
g2022.lic<-c(120029,120072,120159,120336,120441)

##Get Landings by manually running MARFIS_all in one.R

##First let's see if we can pull out a dataframe for each of the above vectors
d2022<-subset(catch,LICENCE_ID %in% d2022.lic & YEAR==2022)
d2022<-convert.KGS(d2022)
d2022$NUM_FISH<-d2022$KGS/0.24
sum(d2022$KGS)

g2022<-subset(catch,LICENCE_ID %in% g2022.lic & YEAR==2022)
g2022<-convert.KGS(g2022)
g2022$NUM_FISH<-g2022$KGS/0.24
sum(g2022$KGS)

d2021<-subset(catch,LICENCE_ID %in% d2021.lic & YEAR==2021)
d2021<-convert.KGS(d2021)
d2021$NUM_FISH<-d2021$KGS/0.24
sum(d2021$KGS)

g2021<-subset(catch,LICENCE_ID %in% g2021.lic & YEAR==2021)
g2021<-convert.KGS(g2021)
g2021$NUM_FISH<-g2021$KGS/0.24
sum(g2021$KGS)

##What proportion of the catch do these 5 licences represent?
catch.t22<-subset(catch,RIVERNAME_CLEANED=="TUSKET" & YEAR==2022)
catch.t22<-convert.KGS(catch.t22)
catch.t21<-subset(catch,RIVERNAME_CLEANED=="TUSKET" & YEAR==2021)
catch.t21<-convert.KGS(catch.t21)
sum(catch.t22$KGS)
sum(catch.t21$KGS)

g21.prop<-sum(g2021$KGS)/sum(catch.t21$KGS)
d21.prop<-sum(d2021$KGS)/sum(catch.t21$KGS)
g22.prop<-sum(g2022$KGS)/sum(catch.t22$KGS) ##likely lower than it should be
d22.prop<-sum(d2022$KGS)/sum(catch.t22$KGS) ##likely lower than it should be


