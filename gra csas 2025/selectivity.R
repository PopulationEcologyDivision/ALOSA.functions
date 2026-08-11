#calculate selectivity from age data collected only at the fish ladder
library(lubridate)
library(miscTools)
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

##function to calculate selectivity using one set of age data####
##takes year, location, fishery closure date, delay time, total catch, total escapement
##requires sourcery, channel to be setup
calc.sel<-function(year,location=3,close.date="May 31",delay.time=1,catch,escape)
{
  #read in age data from database
  x<-get.age.data(year = year, siteID = location, sppID = 3501, channel = channel)
  x$date<-make_date(x$YEAR,x$MON,x$DAY)
  
  #format the close date
  close.date<-as.Date(close.date,format="%b %d")
  year(close.date)<-year #change the year from current to specified by argument
  
  #generate truncated age.dataframe
  y<-x[x$date<=close.date-delay.time,]
  
  #x=ladder, y=fishery (from truncated ladder data)
  
  #calculate selectivity
  xnaa<-as.numeric(table(x$CURRENT_AGE))[1:4]
  ynaa<-as.numeric(table(y$CURRENT_AGE))[1:4]
  xpaa<-xnaa/sum(xnaa)
  ypaa<-ynaa/sum(ynaa)
  
  xnaa<-xpaa*escape
  ynaa<-ypaa*catch
  
  uaa<-ynaa/(ynaa+xnaa)
  faa<--log(1-uaa)
  
  selaa<-faa/max(faa)
  
  #calculate proportions at age
  z<-table(x$CURRENT_AGE)
  #props for ages 3:6
  propatage<-c(z[1]/sum(z),z[2]/sum(z),z[3]/sum(z),sum(z[4:length(z)])/sum(z)) 
  
  return(c(selaa,max(faa),propatage))
}

#stole from data setup.R in gra
abun.df<-data.frame(year=1979:2025,
                    catch=c(1066800,622300,243840,254068,150408,212966,217170,1171956,1461770,1154049,
                            1572260,975233,357632,421640,747522,1018794,954960,761873,611520,372400,
                            698600,754585,119348,391278,416335,268820,219173,292589,332264,371940,
                            342884,581998,431497,384803,387333,439000,705500,769133,605900,903655,
                            784152,1202604,1231005,1562900,1431500,1265348,673746),
                    ccr=c(0.31,0.31,0.31,0.20,0.76,0.52,0.31,0.31,0.31,0.31,
                          0.31,0.31,0.31,0.31,0.31,0.31,0.13,0.31,0.16,0.46,
                          0.12,0.13,2.00,0.93,1.05,0.65,1.21,0.71,1.41,0.93,
                          1.16,0.93,0.93,0.93,0.39,0.93,0.62,0.59,1.84,1.17,
                          1.30,0.93,0.80,0.56,0.92,0.47,1.27)
)

esc.df<-data.frame(year=c(1982,1983,1984,1995,1997,1998,1999,2000,2001,2002,
                          2003,2004,2005,2006,2007,2009,2012,2013,2015,2016,
                          2017,2018,2019,2021,2022,2023,2024,2025),
                   esc=c(50400,114800,111100,126933,95443,171639,81326,98883,238842,310746,
                         435842,222662,299910,242078,470356,398807,158387,149682,438874,454800,
                         1114450,1061688,1021186,984094,874862,1312700,588540,859000)
)

sel97<-calc.sel(1997,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1997],esc.df$esc[esc.df$year==1997])
sel98<-calc.sel(1998,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1998],esc.df$esc[esc.df$year==1998])
sel99<-calc.sel(1999,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1999],esc.df$esc[esc.df$year==1999])
sel00<-calc.sel(2000,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2000],esc.df$esc[esc.df$year==2000])
sel01<-calc.sel(2001,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2001],esc.df$esc[esc.df$year==2001])
sel02<-calc.sel(2002,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2002],esc.df$esc[esc.df$year==2002])

sel16<-calc.sel(2016,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2016],esc.df$esc[esc.df$year==2016])
sel18<-calc.sel(2018,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2018],esc.df$esc[esc.df$year==2018])
sel19<-calc.sel(2019,3,"May 31",delay.time=2,abun.df$catch[abun.df$year==2019],esc.df$esc[esc.df$year==2019])
sel21<-calc.sel(2021,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2021],esc.df$esc[esc.df$year==2021])
sel22<-calc.sel(2022,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2022],esc.df$esc[esc.df$year==2022])
sel23<-calc.sel(2023,3,"May 19",delay.time=3,abun.df$catch[abun.df$year==2023],esc.df$esc[esc.df$year==2023])
sel24<-calc.sel(2024,3,"May 21",delay.time=3,abun.df$catch[abun.df$year==2024],esc.df$esc[esc.df$year==2024])
sel25<-calc.sel(2025,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2025],esc.df$esc[esc.df$year==2025])

# sel97<-calc.sel(1997,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1997],esc.df$esc[esc.df$year==1997])
# sel98<-calc.sel(1998,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1998],esc.df$esc[esc.df$year==1998])
# sel99<-calc.sel(1999,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==1999],esc.df$esc[esc.df$year==1999])
# sel00<-calc.sel(2000,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2000],esc.df$esc[esc.df$year==2000])
# sel01<-calc.sel(2001,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2001],esc.df$esc[esc.df$year==2001])
# sel02<-calc.sel(2002,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2002],esc.df$esc[esc.df$year==2002])
# 
# sel16<-calc.sel(2016,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2016],esc.df$esc[esc.df$year==2016])
# sel18<-calc.sel(2018,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2018],esc.df$esc[esc.df$year==2018])
# sel19<-calc.sel(2019,3,"May 31",delay.time=2,abun.df$catch[abun.df$year==2019],esc.df$esc[esc.df$year==2019])
# sel21<-calc.sel(2021,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2021],esc.df$esc[esc.df$year==2021])
# sel22<-calc.sel(2022,3,"May 31",delay.time=3,abun.df$catch[abun.df$year==2022],esc.df$esc[esc.df$year==2022])
# sel23<-calc.sel(2023,3,"May 19",delay.time=3,abun.df$catch[abun.df$year==2023],esc.df$esc[esc.df$year==2023])
# sel24<-calc.sel(2024,3,"May 21",delay.time=3,abun.df$catch[abun.df$year==2024],esc.df$esc[esc.df$year==2024])

selall<-data.frame(rbind(sel97,sel98,sel99,sel00,sel01,sel02,sel16,sel18,sel19,sel21,sel22,sel23,sel24,sel25))
colnames(selall)<-c("sel3","sel4","sel5","sel6","ffully","prop3","prop4","prop5","prop6+")
selall$years<-c(1997:2002,2016,2018,2019,2021:2025)
means<-colMeans(selall) #change to medians if desired. note that median props-at-age don't sum to 1
selall<-rbind(selall,means)
write.csv(selall,"calcedsel25.csv",row.names=F)
#random diagnostic plots
# age<-3:6
# 
# plot(age,sel16[1:4],type="l",ylim=c(0,1))
# lines(age,sel18[1:4],col="red")
# lines(age,sel19[1:4],col="orange")
# lines(age,sel21[1:4],col="green")
# lines(age,sel22[1:4],col="blue")
# lines(age,sel23[1:4],col="purple")
# lines(age,sel24[1:4],col="brown")
# 
# ccr.sub<-c(0.59,1.17,1.30,0.80,0.56,0.92,0.47)
# u<-1/(1+ccr.sub)
# f<--log(1-u)
# fmax<-c(sel16[5],sel18[5],sel19[5],sel21[5],sel22[5],sel23[5],sel24[5])
# plot(f,fmax)
# abline(0,1)
# text(f,fmax,labels=c(2016,2018,2019,2021,2022,2023,2024))



# #other stuff####
# fishery.sel<-c(0.504,0.653,0.834,1)
# 
# age.dat.2016<-get.age.data(2016,3,3501,T,"Y",channel)
# age.dat.2016$date<-as.Date(ISOdate(age.dat.2016$YEAR,age.dat.2016$MON,age.dat.2016$DAY))
# snaa16<-as.numeric(table(age.dat.2016$CURRENT_AGE))[1:4]
# 
# age.dat.2016.trunc<-age.dat.2016[age.dat.2016$MON<=5,] #excldue all june
# snaat16<-as.numeric(table(age.dat.2016.trunc$CURRENT_AGE))[1:4]
# 
# paa16<-snaa16/sum(snaa16)
# paat16<-snaat16/sum(snaat16)
# 
# cat16<-769133
# esc16<-454800
# 
# naa16<-paa16*esc16
# cnaa16<-paat16*cat16
# 
# uaa16<-cnaa16/(cnaa16+naa16)
# faa16<--log(1-uaa16)
# 
# selaa16<-faa16/max(faa16)
# 
# #let's try generating a range of sels, using a range of truncations
# 
# out.sel<-list()
# for(i in max(age.dat.2016.trunc$DAY):min(age.dat.2016.trunc$DAY))
# {
#   print(i)
#   ad16t<-age.dat.2016.trunc[age.dat.2016.trunc$DAY<=i,]
#   snaat16<-as.numeric(table(ad16t$CURRENT_AGE))[1:4]
#   print(snaat16)
#   paa16<-snaa16/sum(snaa16)
#   paat16<-snaat16/sum(snaat16)
#   
#   cat16<-769133
#   esc16<-454800
#   
#   naa16<-paa16*esc16
#   cnaa16<-paat16*cat16
#   
#   uaa16<-cnaa16/(cnaa16+naa16)
#   faa16<--log(1-uaa16)
#   
#   selaa16<-faa16/max(faa16)
#   out.sel[[i]]<-selaa16
# }
# 
# out.sel<-out.sel[9:31] #trim NAs
# out.sel<-rev(out.sel) #reverse list
# plot.cols<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
# plot(3:6,fishery.sel,type="l",lwd=2,ylim=c(0,1))
# for(i in 1:length(out.sel))
# {
#   lines(3:6,out.sel[[i]],col=plot.cols(length(out.sel))[i])
# }
# legend(2.9,0.05,legend=9:20,lty=1,col=plot.cols(23)[1:11],horiz=T,cex=0.5)
# legend(2.9,0.11,legend=21:31,lty=1,col=plot.cols(23)[12:23],horiz=T,cex=0.5)
# 
# #look at cumulative age numbers over time for both locations
# #bring in agedata from fishery
# comm16<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2016/Biological characteristics datasheet commercial 05302016 Master withage.csv")
# comm16<-comm16[!is.na(comm16$age),]
# comm16$year<-2016
# comm16$date<-as.Date(ISOdate(comm16$year,comm16$month,comm16$day))
# 
# #f=fishery, l=ladder
# f3<-subset(comm16,age==3,select=c(age,date))
# f3$cumul<-cumsum(f3$age)/3#/sum(f3$age)
# f4<-subset(comm16,age==4,select=c(age,date))
# f4$cumul<-cumsum(f4$age)/4#/sum(f4$age)
# f5<-subset(comm16,age==5,select=c(age,date))
# f5$cumul<-cumsum(f5$age)/5#/sum(f5$age)
# f6<-subset(comm16,age>=6,select=c(age,date))
# f6$cumul<-cumsum(f6$age)/6#/sum(f6$age)
# 
# l3<-subset(age.dat.2016,CURRENT_AGE==3,select=c(CURRENT_AGE,date))
# l3$cumul<-cumsum(l3$CURRENT_AGE)/3#/sum(l3$CURRENT_AGE)
# l4<-subset(age.dat.2016,CURRENT_AGE==4,select=c(CURRENT_AGE,date))
# l4$cumul<-cumsum(l4$CURRENT_AGE)/4#/sum(l4$CURRENT_AGE)
# l5<-subset(age.dat.2016,CURRENT_AGE==5,select=c(CURRENT_AGE,date))
# l5$cumul<-cumsum(l5$CURRENT_AGE)/5#/sum(l5$CURRENT_AGE)
# l6<-subset(age.dat.2016,CURRENT_AGE>=6,select=c(CURRENT_AGE,date))
# l6$cumul<-cumsum(l6$CURRENT_AGE)/6#/sum(l6$CURRENT_AGE)
# 
# xlims=c(min(c(f3$date,f4$date,f5$date,f6$date,l3$date,l4$date,l5$date,l6$date)),
#         max(c(f3$date,f4$date,f5$date,f6$date,l3$date,l4$date,l5$date,l6$date)))
# plot.cols<-viridis(4)
# plot(l3$date,l3$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[1],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(l4$date,l4$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[2],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(l5$date,l5$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[3],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(l6$date,l6$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[4],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(f3$date,f3$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[1],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(f4$date,f4$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[2],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(f5$date,f5$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[3],lwd=3,xlab="",ylab="")
# par(new=TRUE,xpd=TRUE)
# plot(f6$date,f6$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[4],lwd=3,xlab="",ylab="")
# box()
# axis(1,labels=c("May 01","May 15","June 01"),at=c(16922,16937,16953))
# axis(2)
# 
# 
# 
# #attempt 2 - weighting ladder ages with fishery catch
# catch2016<-data.frame(day = c(19,25,26,28,29,02,03,05,06,09,10,12,13,16,17,19,20,23,24,26,27,30,31),
#                       month=c(04,04,04,04,04,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05),
#                       catch=c(7,23,7,8,2,190,2094,1333,395,1142,380,571,395,1333,12,258,190,95,85,95,83,163,5))
# catch2016$year<-2016
# catch2016$date<-as.Date(ISOdate(catch2016$year,catch2016$month,catch2016$day))
# 
# total.run.time<-data.frame(date=seq(min(c(age.dat.2016$date,catch2016$date)),
#                                     max(c(age.dat.2016$date,catch2016$date)),by=1),
#                            dor=NA)
# total.run.time$dor<-1:nrow(total.run.time)
# 
# catch2016<-merge(catch2016,total.run.time)
# age.dat.2016<-merge(age.dat.2016,total.run.time)
# 
# 
# migration.time<-1
# age.dat.2016$dor.corr<-age.dat.2016$dor-migration.time
# 
