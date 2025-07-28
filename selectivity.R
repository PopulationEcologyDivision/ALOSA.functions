# get.bio.data(year = 2025, siteID = 2, sppID = 3501, channel = channel)
# 
# 
# years<-2014:2025
# tot<-c(590380,191428,NA,NA,NA,29928,NA,15394,8463,17988,10112,1813)
fishery.sel<-c(0.504,0.653,0.834,1)

age.dat.2016<-get.age.data(2016,3,3501,T,"Y",channel)
age.dat.2016$date<-as.Date(ISOdate(age.dat.2016$YEAR,age.dat.2016$MON,age.dat.2016$DAY))
snaa16<-as.numeric(table(age.dat.2016$CURRENT_AGE))[1:4]

age.dat.2016.trunc<-age.dat.2016[age.dat.2016$MON<=5,] #excldue all june
snaat16<-as.numeric(table(age.dat.2016.trunc$CURRENT_AGE))[1:4]

paa16<-snaa16/sum(snaa16)
paat16<-snaat16/sum(snaat16)

cat16<-769133
esc16<-454800

naa16<-paa16*esc16
cnaa16<-paat16*cat16

uaa16<-cnaa16/(cnaa16+naa16)
faa16<--log(1-uaa16)

selaa16<-faa16/max(faa16)

#let's try generating a range of sels, using a range of truncations

out.sel<-list()
for(i in max(age.dat.2016.trunc$DAY):min(age.dat.2016.trunc$DAY))
{
  print(i)
  ad16t<-age.dat.2016.trunc[age.dat.2016.trunc$DAY<=i,]
  snaat16<-as.numeric(table(ad16t$CURRENT_AGE))[1:4]
  print(snaat16)
  paa16<-snaa16/sum(snaa16)
  paat16<-snaat16/sum(snaat16)
  
  cat16<-769133
  esc16<-454800
  
  naa16<-paa16*esc16
  cnaa16<-paat16*cat16
  
  uaa16<-cnaa16/(cnaa16+naa16)
  faa16<--log(1-uaa16)
  
  selaa16<-faa16/max(faa16)
  out.sel[[i]]<-selaa16
}

out.sel<-out.sel[9:31] #trim NAs
out.sel<-rev(out.sel) #reverse list
plot.cols<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
plot(3:6,fishery.sel,type="l",lwd=2,ylim=c(0,1))
for(i in 1:length(out.sel))
{
  lines(3:6,out.sel[[i]],col=plot.cols(length(out.sel))[i])
}
legend(2.9,0.05,legend=9:20,lty=1,col=plot.cols(23)[1:11],horiz=T,cex=0.5)
legend(2.9,0.11,legend=21:31,lty=1,col=plot.cols(23)[12:23],horiz=T,cex=0.5)

#look at cumulative age numbers over time for both locations
#bring in agedata from fishery
comm16<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2016/Biological characteristics datasheet commercial 05302016 Master withage.csv")
comm16<-comm16[!is.na(comm16$age),]
comm16$year<-2016
comm16$date<-as.Date(ISOdate(comm16$year,comm16$month,comm16$day))

#f=fishery, l=ladder
f3<-subset(comm16,age==3,select=c(age,date))
f3$cumul<-cumsum(f3$age)/3#/sum(f3$age)
f4<-subset(comm16,age==4,select=c(age,date))
f4$cumul<-cumsum(f4$age)/4#/sum(f4$age)
f5<-subset(comm16,age==5,select=c(age,date))
f5$cumul<-cumsum(f5$age)/5#/sum(f5$age)
f6<-subset(comm16,age>=6,select=c(age,date))
f6$cumul<-cumsum(f6$age)/6#/sum(f6$age)

l3<-subset(age.dat.2016,CURRENT_AGE==3,select=c(CURRENT_AGE,date))
l3$cumul<-cumsum(l3$CURRENT_AGE)/3#/sum(l3$CURRENT_AGE)
l4<-subset(age.dat.2016,CURRENT_AGE==4,select=c(CURRENT_AGE,date))
l4$cumul<-cumsum(l4$CURRENT_AGE)/4#/sum(l4$CURRENT_AGE)
l5<-subset(age.dat.2016,CURRENT_AGE==5,select=c(CURRENT_AGE,date))
l5$cumul<-cumsum(l5$CURRENT_AGE)/5#/sum(l5$CURRENT_AGE)
l6<-subset(age.dat.2016,CURRENT_AGE>=6,select=c(CURRENT_AGE,date))
l6$cumul<-cumsum(l6$CURRENT_AGE)/6#/sum(l6$CURRENT_AGE)

xlims=c(min(c(f3$date,f4$date,f5$date,f6$date,l3$date,l4$date,l5$date,l6$date)),
        max(c(f3$date,f4$date,f5$date,f6$date,l3$date,l4$date,l5$date,l6$date)))
plot.cols<-viridis(4)
plot(l3$date,l3$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[1],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(l4$date,l4$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[2],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(l5$date,l5$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[3],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(l6$date,l6$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=1,col=plot.cols[4],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(f3$date,f3$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[1],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(f4$date,f4$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[2],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(f5$date,f5$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[3],lwd=3,xlab="",ylab="")
par(new=TRUE,xpd=TRUE)
plot(f6$date,f6$cumul,type="s",axes=0,xlim=xlims,ylim=c(0,300),lty=3,col=plot.cols[4],lwd=3,xlab="",ylab="")
box()
axis(1,labels=c("May 01","May 15","June 01"),at=c(16922,16937,16953))
axis(2)



#attempt 2 - weighting ladder ages with fishery catch
catch2016<-data.frame(day = c(19,25,26,28,29,02,03,05,06,09,10,12,13,16,17,19,20,23,24,26,27,30,31),
                      month=c(04,04,04,04,04,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05,05),
                      catch=c(7,23,7,8,2,190,2094,1333,395,1142,380,571,395,1333,12,258,190,95,85,95,83,163,5))
catch2016$year<-2016
catch2016$date<-as.Date(ISOdate(catch2016$year,catch2016$month,catch2016$day))

total.run.time<-data.frame(date=seq(min(c(age.dat.2016$date,catch2016$date)),
                                    max(c(age.dat.2016$date,catch2016$date)),by=1),
                           dor=NA)
total.run.time$dor<-1:nrow(total.run.time)

catch2016<-merge(catch2016,total.run.time)
age.dat.2016<-merge(age.dat.2016,total.run.time)


migration.time<-1
age.dat.2016$dor.corr<-age.dat.2016$dor-migration.time

