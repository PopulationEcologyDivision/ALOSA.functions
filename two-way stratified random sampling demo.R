##Simualtion and figures for counting presentation
#setup
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
library(ROracle)
channel <- dbConnect(
  DBI::dbDriver("Oracle"),
  oracle.username.GASP,
  oracle.password.GASP,
  "PTRAN",
  believeNRows = FALSE
)
library(readxl)
library(hms)
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/data from NSPI may 25 2015")
c02<-read_excel("2002 alewife counts.xls",skip=1,sheet="Counts")
#file is a mess, clean it up and pull out what we want
#we want camera count (second setof number/daily/cumulative columns)
#we want date and time and number
c02<-c02[,c(1,2,6)]
colnames(c02)<-c("date","time","total")
c02$time<-as_hms(c02$time)
c02$date<-as.Date(c02$date)

#setup strata
strata.breakpoints<-as_hms(c("06:00:00","12:00:00","16:00:00","20:00:00"))
c02$strata<-ifelse(c02$time<strata.breakpoints[1],1,
                   ifelse(c02$time>=strata.breakpoints[1]&c02$time<strata.breakpoints[2],2,
                          ifelse(c02$time>=strata.breakpoints[2]&c02$time<strata.breakpoints[3],3,
                                 ifelse(c02$time>=strata.breakpoints[3]&c02$time<strata.breakpoints[4],4,5))))

#select a day for demonstration. may 24 has the highest count
#a bunch of counts are missing from 2002
test<-aggregate(na.omit(c02$total),by=list(c02$date[!is.na(c02$total)]),FUN="length")

strata.sum<-aggregate(c02$total,by=list(c02$date,c02$strata),FUN="sum",na.rm=T)

#may 13 has almost all counted, fill in missing one
count1<-c02[c02$date=="2002-05-13",]
count1$total[c(49,50)]<-round(count1$total[50]/2,0) #take the next count and split it into two to fill in the missing count
count1$total[96]<-0 #last count at night gets 0 like other ones
#may 15 has a count of ~10k and all times counted
count2<-c02[c02$date=="2002-05-15",]

####make plot for pres####
# par(mfrow=c(2,2),cex=1.2)
# p1<-barplot(count1$total,ylim=c(0,775),xlab="Time of Day",ylab="Count")
# abline(v=p1[25])
# abline(v=p1[49])
# abline(v=p1[65])
# abline(v=p1[81])
# axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000),cex=2)
# text(p1[10],600,sum(count1$total))
# mtext("Day 1",3,line=1,cex=1.2)
# box()
# p2<-barplot(count2$total,ylim=c(0,775),xlab="Time of Day",ylab="Count")
# abline(v=p2[25])
# abline(v=p2[49])
# abline(v=p2[65])
# abline(v=p2[81])
# axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000))
# text(p2[10],600,sum(count2$total))
# mtext("Day 2",3,line=1,cex=1.2)
# box()
# p3<-barplot(strata.sum$x[strata.sum$Group.1=="2002-05-13"],ylim=c(0,8500),xlab="Strata",ylab="Count")
# axis(1,at=p3,labels=1:5)
# text(p3[1],7000,sum(count1$total))
# box()
# p4<-barplot(strata.sum$x[strata.sum$Group.1=="2002-05-15"],ylim=c(0,8500),xlab="Strata",ylab="Count")
# axis(1,at=p4,labels=1:5)
# text(p4[1],7000,sum(count2$total))
# box()

#now that we have our full day of counts, we can replicate subsampling

####SRS####
countSRS<-rbind(count1,count2)
#draw 30 random counts
samplesSRS <- countSRS[sample(1:nrow(countSRS), 30, FALSE),]
SRS.daily<-round(c(mean(samplesSRS$total[samplesSRS$date=="2002-05-13"])*nrow(countSRS[countSRS$date=="2002-05-13",]),
             mean(samplesSRS$total[samplesSRS$date=="2002-05-15"])*nrow(countSRS[countSRS$date=="2002-05-15",])),0)
SRS.total<-round(mean(samplesSRS$total)*nrow(countSRS),0)

#visualize
#plot all counts with selected counts highlighted?
count1SRS<-countSRS[countSRS$date=="2002-05-13",]
count1SRS$selected<-ifelse(count1SRS$time%in%samplesSRS$time[samplesSRS$date=="2002-05-13"],"red","gray")
count2SRS<-countSRS[countSRS$date=="2002-05-15",]
count2SRS$selected<-ifelse(count2SRS$time%in%samplesSRS$time[samplesSRS$date=="2002-05-15"],"red","gray")

plotcols <- c("gray", "red")
par(mfrow=c(2,2),xpd=T)
p1<-barplot(count1SRS$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count1SRS$selected)
axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000),cex=2)
text(p1[10],600,paste0("Estimated count ",SRS.daily[1]))
mtext("Day 1",3,line=1,cex=1.2)
box()

p2<-barplot(count2SRS$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count2SRS$selected)
axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000),cex=2)
text(p1[10],600,paste0("Estimated count ",SRS.daily[2]))
mtext("Day 2",3,line=1,cex=1.2)
box()
obs.total<-sum(strata.sum$x[strata.sum$Group.1=="2002-05-13" | strata.sum$Group.1=="2002-05-15"])
p3<-barplot(c(obs.total,SRS.total),ylab="Count"
            ,col=c("gray", "red"),ylim=c(0,30000))
axis(1,at=p3[1:2],labels=c("Observed", "Predicted"),cex=2)
box()
legend("topleft",inset=c(0,-0.3),
       legend=c(paste0("Observed ",sum(obs.total)),paste0("Estimated ",round(sum(SRS.daily),0))),
       fill=plotcols,
       bty="n",
       horiz=T)
#repeat and report mean and CI
SRSrep.counts<-c()
for(i in 1:50)
{
  samplesSRS <- countSRS[sample(1:nrow(countSRS), 30, FALSE),]
  SRS.daily<-round(c(mean(samplesSRS$total[samplesSRS$date=="2002-05-13"])*nrow(countSRS[countSRS$date=="2002-05-13",]),
                     mean(samplesSRS$total[samplesSRS$date=="2002-05-15"])*nrow(countSRS[countSRS$date=="2002-05-15",])),0)
  SRS.total<-round(mean(samplesSRS$total)*nrow(countSRS),0)
  SRSrep.counts[i]<-SRS.total
}

####1wayStratified####
#draw 15 15 minute counts from each day
countS1WRS<-rbind(count1,count2)

sp <- split(countS1WRS, list(countS1WRS$date))

samples <- lapply(sp, function(x) x[sample(1:nrow(x), 15, FALSE),])

dataS1WRS<-do.call(rbind,samples)
S1WRS.daily<-round(c(mean(dataS1WRS$total[dataS1WRS$date=="2002-05-13"])*nrow(countS1WRS[countS1WRS$date=="2002-05-13",]),
                   mean(dataS1WRS$total[dataS1WRS$date=="2002-05-15"])*nrow(countS1WRS[countS1WRS$date=="2002-05-15",])),0)
# S1WRS.total<-sum(S1WRS.daily)

#visualize
count1S1WRS<-countS1WRS[countS1WRS$date=="2002-05-13",]
count1S1WRS$selected<-ifelse(count1S1WRS$time%in%dataS1WRS$time[dataS1WRS$date=="2002-05-13"],"red","gray")
count2S1WRS<-countS1WRS[countS1WRS$date=="2002-05-15",]
count2S1WRS$selected<-ifelse(count2S1WRS$time%in%dataS1WRS$time[dataS1WRS$date=="2002-05-15"],"red","gray")

plotcols <- c("gray", "red")
par(mfrow=c(2,2),xpd=T)
p1<-barplot(count1S1WRS$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count1S1WRS$selected)
axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000),cex=2)
text(p1[10],600,paste0("Estimated count ",S1WRS.daily[1]))
mtext("Day 1",3,line=1,cex=1.2)
box()

p2<-barplot(count2S1WRS$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count2S1WRS$selected)
axis(1,at=p1[c(25,49,65,81)],labels=c(0600, 1200, 1600, 2000),cex=2)
text(p1[10],600,paste0("Estimated count ",S1WRS.daily[2]))
mtext("Day 2",3,line=1,cex=1.2)
box()

p3<-barplot(c(sum(strata.sum$x[strata.sum$Group.1=="2002-05-13"]),S1WRS.daily[1])
            ,col=c("gray", "red"),ylim=c(0,30000))
axis(1,at=p3[1:2],labels=c("Observed", "Predicted"),cex=2)
box()
legend("topleft",inset=c(0,-0.3),
       legend=c(paste0("Observed ",sum(strata.sum$x[strata.sum$Group.1=="2002-05-13"])),paste0("Estimated ",round(S1WRS.daily[1],0))),
       fill=plotcols,
       bty="n",
       horiz=T)

p4<-barplot(c(sum(strata.sum$x[strata.sum$Group.1=="2002-05-15"]),S1WRS.daily[2])
            ,col=c("gray", "red"),ylim=c(0,30000))
axis(1,at=p3[1:2],labels=c("Observed", "Predicted"),cex=2)
box()
legend("topleft",inset=c(0,-0.3),
       legend=c(paste0("Observed ",sum(strata.sum$x[strata.sum$Group.1=="2002-05-15"])),paste0("Estimated ",round(S1WRS.daily[2],0))),
       fill=plotcols,
       bty="n",
       horiz=T)

#repeat sampling
S1WRSrep.counts<-c()
for(i in 1:50)
{
  samples <- lapply(sp, function(x) x[sample(1:nrow(x), 15, FALSE),])
  
  dataS1WRS<-do.call(rbind,samples)
  S1WRS.daily<-round(c(mean(dataS1WRS$total[dataS1WRS$date=="2002-05-13"])*nrow(countS1WRS[countS1WRS$date=="2002-05-13",]),
                       mean(dataS1WRS$total[dataS1WRS$date=="2002-05-15"])*nrow(countS1WRS[countS1WRS$date=="2002-05-15",])),0)
  S1WRSrep.counts[i]<-sum(S1WRS.daily)
}
####2wayStratified####
#draw 3 15 minute counts from each strata
#there are 24 15-minute blocks in strata 1 and 2 and 16 in strata 3-5
countS2WRS<-rbind(count1,count2)

sp <- split(countS2WRS, list(countS2WRS$strata,countS2WRS$date))

samples <- lapply(sp, function(x) x[sample(1:nrow(x), 3, FALSE),])

dataS2WRS<-do.call(rbind,samples)

#use escapement script code
strata.means<-aggregate(dataS2WRS$total,by=list(dataS2WRS$date,dataS2WRS$strata),FUN="mean",na.rm=T)
colnames(strata.means)=c("date","strata","mean")

##Compile into dataframe 
junk2<-data.frame(strata=strata.means$strata,
                  date=strata.means$date,
                  mean=strata.means$mean)

min15.periods<-data.frame(strata=c(1,2,3,4,5),n.periods=c(24,24,16,16,16))
#merge and order by strata, after all missing counts have been filled in
summary.data<-merge(junk2,min15.periods,by="strata") 
summary.data$total<-summary.data$mean*summary.data$n.periods 

#visualize
#plot all counts with selected counts highlighted?
count1<-countS2WRS[countS2WRS$date=="2002-05-13",]
count1$selected<-ifelse(count1$time%in%dataS2WRS$time[dataS2WRS$date=="2002-05-13"],"red","gray")
count2<-countS2WRS[countS2WRS$date=="2002-05-15",]
count2$selected<-ifelse(count2$time%in%dataS2WRS$time[dataS2WRS$date=="2002-05-15"],"red","gray")

par(mfrow=c(2,2),xpd=F)
p1<-barplot(count1$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count1$selected)
abline(v=p1[25]-0.6)
abline(v=p1[49]-0.6)
abline(v=p1[65]-0.6)
abline(v=p1[81]-0.6)
axis(1,at=p1[c(25,49,65,81)]-0.6,labels=c(0600, 1200, 1600, 2000),cex=2)
mtext("Day 1",3,line=1,cex=1.2)
box()
p2<-barplot(count2$total,ylim=c(0,775),xlab="Time of Day",ylab="Count",col=count2$selected)
abline(v=p1[25]-0.6)
abline(v=p1[49]-0.6)
abline(v=p1[65]-0.6)
abline(v=p1[81]-0.6)
axis(1,at=p1[c(25,49,65,81)]-0.6,labels=c(0600, 1200, 1600, 2000),cex=2)
mtext("Day 2",3,line=1,cex=1.2)
box()

datatoplot1<-t(matrix(c(strata.sum$x[strata.sum$Group.1=="2002-05-13"],
                        summary.data$total[summary.data$date=="2002-05-13"]),ncol=2))

datatoplot2<-t(matrix(c(strata.sum$x[strata.sum$Group.1=="2002-05-15"],
                       summary.data$total[summary.data$date=="2002-05-15"]),ncol=2))

plotcols <- c("gray", "red")
barplot(datatoplot1,beside=T,space=c(0,0.1),ylim=c(0,max(datatoplot1[1,]*1.2)),col=plotcols,xlab="Strata",ylab="Count")
legend("topleft",
       legend=c(paste0("Observed ",sum(datatoplot1[1,])),paste0("Estimated ",round(sum(datatoplot1[2,]),0))),
       fill=plotcols,
       bty="n")
axis(1,at=seq(1.1,10.2,by=2.1),labels=1:5)
box()
barplot(datatoplot2,beside=T,space=c(0,0.1),ylim=c(0,max(datatoplot2[1,]*1.2)),col=plotcols,xlab="Strata",ylab="Count")
legend("topleft",
       legend=c(paste0("Observed ",sum(datatoplot2[1,])),paste0("Estimated ",round(sum(datatoplot2[2,]),0))),
       fill=plotcols,
       bty="n")
axis(1,at=seq(1.1,10.2,by=2.1),labels=1:5)
box()


#if we repeated this 50 times, whats the average count
S2WRSrep.counts<-c()
for(i in 1:50)
{
  sp <- split(countS2WRS, list(countS2WRS$strata,countS2WRS$date))
  
  samples <- lapply(sp, function(x) x[sample(1:nrow(x), 3, FALSE),])
  
  dataS2WRS<-do.call(rbind,samples)
  
  #use escapement script code
  strata.means<-aggregate(dataS2WRS$total,by=list(dataS2WRS$date,dataS2WRS$strata),FUN="mean",na.rm=T)
  colnames(strata.means)=c("date","strata","mean")
  
  ##Compile into dataframe 
  junk2<-data.frame(strata=strata.means$strata,
                    date=strata.means$date,
                    mean=strata.means$mean)
  
  min15.periods<-data.frame(strata=c(1,2,3,4,5),n.periods=c(24,24,16,16,16))
  #merge and order by strata, after all missing counts have been filled in
  summary.data<-merge(junk2,min15.periods,by="strata") 
  summary.data$total<-summary.data$mean*summary.data$n.periods 
  
  
  S2WRSrep.counts[i]<-sum(summary.data$total)
}

####final comparison plot####
# x<-data.frame(`Simple Random`=SRSrep.counts,
#               `One-Way Stratified`=S1WRSrep.counts,
#               `Two-Way Stratified`=S2WRSrep.counts)
# par(mfrow=c(1,1))
# boxplot(x,xlab="Sampling Design",ylab="Total Count")
# abline(h=obs.total)
