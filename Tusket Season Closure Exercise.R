##Tusket season closure sim##
#Using escapement data paired with daily landings data from 2021 and 2022
#I will see what effect closing various days will have on escapement,
#by assuming all fish not caught ascend the ladder the same day

####SETUP####
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
library(lubridate)
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
daily.summary.2022$dayofyear<-as.numeric(levels(daily.summary.2022$dayofyear))

##Add in date column
daily.summary.2021$date<-as.POSIXct(paste("2021",daily.summary.2021$mon,daily.summary.2021$day,sep="-"))
daily.summary.2022$date<-as.POSIXct(paste("2022",daily.summary.2022$mon,daily.summary.2022$day,sep="-"))
daily.summary.2021$dayofyear<-yday(daily.summary.2021$date)
####LANDINGS####
##Licence numbers from Brady and Jarrad
g2021.lic<-c(120103,120118,120169,120336,120757)
d2021.lic<-c(120410,120461,120529,120560,303395)
# d2022.lic<-c(120410,120461,120514,120529,120608)
# g2022.lic<-c(120029,120072,120159,120336,120441)
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
# 
# g21.prop<-sum(g2021$KGS)/sum(catch.t21$KGS)
# d21.prop<-sum(d2021$KGS)/sum(catch.t21$KGS)
# g22.prop<-sum(g2022$KGS)/sum(catch.t22$KGS) ##likely lower than it should be
# d22.prop<-sum(d2022$KGS)/sum(catch.t22$KGS) ##likely lower than it should be
prop21<-(sum(g2021$KGS)+sum(d2021$KGS))/sum(catch.t21$KGS)
prop22<-(sum(g2022$KGS)+sum(d2022$KGS))/sum(catch.t22$KGS)##likely lower than it should be


##make a summarized catch dataframe for each year
#2021
junk<-unique(c(unique(g2021$FV_DATE_FISHED),(unique(d2021$FV_DATE_FISHED))))
catch.df21<-data.frame(date=junk,
                       gill5.num=rep(NA,length(junk)),
                       dip5.num=rep(NA,length(junk)))
catch.df21<-catch.df21[order(catch.df21$date),]
for(i in 1:nrow(catch.df21))
{
  catch.df21$gill5.num[i]<-sum(g2021$NUM_FISH[g2021$FV_DATE_FISHED==catch.df21$date[i]])
  catch.df21$dip5.num[i]<-sum(d2021$NUM_FISH[d2021$FV_DATE_FISHED==catch.df21$date[i]])
}
# catch.df21$gill.num<-round(catch.df21$gill5.num/g21.prop,0)
# catch.df21$dip.num<-round(catch.df21$dip5.num/d21.prop,0)
catch.df21$all.num<-round((catch.df21$gill5.num+catch.df21$dip5.num)/prop21,0)
catch.df21$dayofyear<-yday(catch.df21$date)

#2022
junk<-unique(c(unique(g2022$FV_DATE_FISHED),(unique(d2022$FV_DATE_FISHED))))
catch.df22<-data.frame(date=junk,
                       gill5.num=rep(NA,length(junk)),
                       dip5.num=rep(NA,length(junk)))
catch.df22<-catch.df22[order(catch.df22$date),]
for(i in 1:nrow(catch.df22))
{
  catch.df22$gill5.num[i]<-sum(g2022$NUM_FISH[g2022$FV_DATE_FISHED==catch.df22$date[i]])
  catch.df22$dip5.num[i]<-sum(d2022$NUM_FISH[d2022$FV_DATE_FISHED==catch.df22$date[i]])
}
# catch.df22$gill.num<-round(catch.df22$gill5.num/g22.prop,0)
# catch.df22$dip.num<-round(catch.df22$dip5.num/d22.prop,0)
catch.df22$all.num<-round((catch.df22$gill5.num+catch.df22$dip5.num)/prop22,0)
catch.df22$dayofyear<-yday(catch.df22$date)

##Visualize
plot(catch.df21$date,catch.df21$all.num,type="l")
plot(catch.df22$date,catch.df22$all.num,type="l")

####Season Closure Scenarios####
fishing.doy21<-c(92,93,94,99,100,101,106,107,108,113,114,115,120,121,122,127,128,129,134,135,136,141,142,143,148,149,150)
wkend21<-list(wk1=c(92,93,94),
              wk2=c(99,100,101),
              wk3=c(106,107,108),
              wk4=c(113,114,115),
              wk5=c(120,121,122),
              wk6=c(127,128,129),
              wk7=c(134,135,136),
              wk8=c(141,142,143),
              wk9=c(148,149,150)
              )
fishing.doy22<-c(92,93,98,99,100,105,106,107,112,113,114,119,120,121,126,127,128,133,134,135,140,141,142)
wkend22<-list(wk1=c(92,93),
              wk2=c(98,99,100),
              wk3=c(105,106,107),
              wk4=c(112,113,114),
              wk5=c(119,120,121),
              wk6=c(126,127,128),
              wk7=c(133,134,135),
              wk8=c(140,141,142)
              )
##full close
##2021
daily.summary.2021<-merge(daily.summary.2021,catch.df21,by="dayofyear",all.x=T)
daily.summary.2021$all.num[is.na(daily.summary.2021$all.num)]<-0
daily.summary.2021$combined<-daily.summary.2021$total+daily.summary.2021$all.num

fishing.doy<-fishing.doy21
yaxis<-data.frame(a=seq(0,200000,by=20000),
                  b=seq(0,200,by=20))
fs<-c(92,99,106,113,120,127,134,141,148)##fishing start
fe<-c(94,101,108,115,122,129,136,143,150)##fishing end
plot(daily.summary.2021$dayofyear,daily.summary.2021$combined,type="l",xaxt="n",yaxt="n",xlab="Date",ylab="Thousands of Fish")
rect(fs,0,fe,max(daily.summary.2021$combined,na.rm=T),col="grey",density=30)
lines(daily.summary.2021$dayofyear,daily.summary.2021$total,col="red")
points(daily.summary.2021$dayofyear,daily.summary.2021$all.num)
axis(1,at=c(90,105,121,135,152,167),labels=c("April 1","April 15","May 01","May 15","June 01","June 15"))
axis(2,at=yaxis$a,yaxis$b,las=2)
text(166,160000,"2021",cex=2)
##2022
daily.summary.2022<-merge(daily.summary.2022,catch.df22,by="dayofyear",all.x=T)
daily.summary.2022$all.num[is.na(daily.summary.2022$all.num)]<-0
daily.summary.2022$combined<-daily.summary.2022$total+daily.summary.2022$all.num

fishing.doy<-fishing.doy22
yaxis<-data.frame(a=seq(0,220000,by=20000),
                  b=seq(0,220,by=20))
fs<-c(92,98,105,112,119,126,133,140)##fishing start
fe<-c(93,100,107,114,121,128,135,142)##fishing end
plot(daily.summary.2022$dayofyear,daily.summary.2022$combined,type="l",xaxt="n",yaxt="n",xlab="Date",ylab="Thousands of Fish")
rect(fs,0,fe,max(daily.summary.2022$combined,na.rm=T),col="grey",density=30)
rect(147,0,149,max(daily.summary.2022$combined,na.rm=T),col="yellow",density=30)
lines(daily.summary.2022$dayofyear,daily.summary.2022$total,col="red")
points(daily.summary.2022$dayofyear,daily.summary.2022$all.num)
axis(1,at=c(90,105,121,135,152,167),labels=c("April 1","April 15","May 01","May 15","June 01","June 15"))
axis(2,at=yaxis$a,yaxis$b,las=2)
text(166,160000,"2022",cex=2)


