dat<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Annis 2025/Annis2025sampling.csv")
#setup datetime
dat$date<-as.Date(paste(dat$day,dat$month,dat$year,sep="-"),format="%d-%m-%Y")
dat$hour<-ifelse(nchar(dat$local.time..24hr.)==3,substr(dat$local.time..24hr.,1,1),substr(dat$local.time..24hr.,1,2))
dat$minute<-ifelse(nchar(dat$local.time..24hr.)==3,substr(dat$local.time..24hr.,2,3),substr(dat$local.time..24hr.,3,4))
dat$datetime<-as.POSIXct(paste(dat$date,paste(dat$hour,dat$minute,sep=":"),sep=" "),format="%Y-%m-%d %H:%M")

#remove fall back data
dat<-dat[-which(dat$notes=="AARON                    Fall out "),]

#remove non-observations, where fishing time was set to 0
dat<-dat[dat$fishing.time..minutes.>0,]

#remove NA rows
dat<-dat[!is.na(dat$year),]

#daily catch rate
# dat$catch.rate<-dat$number.caught/dat$fishing.time..minutes.
# 
# dat$a.catch.rate<-dat$number.alewife/dat$fishing.time..minutes.
# dat$b.catch.rate<-dat$number.blueback/dat$fishing.time..minutes.
# 
# par(mar=c(3,3,1,1),mfrow=c(2,1))
# plot(dat$datetime,dat$a.catch.rate)
# plot(dat$datetime,dat$b.catch.rate)
# 
#plot species proportion over time, scaled to number sampled
date.xaxis<-as.POSIXct(c("2025-05-10 00:00","2025-05-12 00:00","2025-05-14 00:00","2025-05-16 00:00","2025-05-18 00:00",
                         "2025-05-20 00:00","2025-05-22 00:00","2025-05-24 00:00","2025-05-26 00:00"))
dat$prop<-dat$number.blueback/(dat$number.blueback+dat$number.alewife)

png("Annisspeciesprop.png",width=16,height=9,units = "in",res=100)
par(omi=c(0.5,0.5,0.5,0.2),mai=c(.75,1.25,.25,.2),cex=1.5)

plot(dat$datetime,dat$prop,cex=sqrt(dat$number.caught/pi)*0.5,pch=16,xlab="",ylab="",xaxt="n",yaxt="n")
axis(1,at=date.xaxis,labels=c("May 10", "May 12", "May 14", "May 16", "May 18"," May 20", "May 22", "May 24", "May 26"))
axis(2,las=2)
points(date.xaxis[7],0.5,cex=sqrt(5/pi)*0.5)
points(date.xaxis[7],0.6,cex=sqrt(50/pi)*0.5)
points(date.xaxis[7],0.7,cex=sqrt(500/pi)*0.5)
text(c(date.xaxis[7],date.xaxis[7],date.xaxis[7]),c(0.5,0.6,0.7),labels=c("5","50","500"),pos=4,offset=1.5)
mtext("Proportion",2,line=3,cex=1.5)
mtext("Date",1,line=3,cex=1.5)

dev.off()

#mean and variance of daily catch rate
#standardize to 5 minutes
dat$a.catch.5<-dat$number.alewife/dat$fishing.time..minutes.*5
dat$b.catch.5<-dat$number.blueback/dat$fishing.time..minutes.*5

#summary statistics
daily.mean.a <- aggregate(dat$a.catch.5,by = list(dat$date),FUN = "mean",na.rm = T)
daily.mean.b <- aggregate(dat$b.catch.5,by = list(dat$date),FUN = "mean",na.rm = T)

daily.sd.a= aggregate(dat$a.catch.5,by = list(dat$date),FUN = "sd",na.rm = T)
daily.sd.b= aggregate(dat$b.catch.5,by = list(dat$date),FUN = "sd",na.rm = T)

user.se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}
daily.se.a= aggregate(dat$a.catch.5,by = list(dat$date),FUN = user.se)
daily.se.b= aggregate(dat$b.catch.5,by = list(dat$date),FUN = user.se)

daily.n.samples<-aggregate(dat$fishing.time..minutes.,by = list(dat$date),FUN = "length")

#merge datafrmaes alewife
summary.a<-merge(daily.mean.a,daily.sd.a,by="Group.1")
summary.a<-merge(summary.a,daily.se.a,by="Group.1")
summary.a<-merge(summary.a,daily.n.samples,by="Group.1")
colnames(summary.a)<-c("date","mean","sd","se","n.samples")

#merge datafrmaes blueback
summary.b<-merge(daily.mean.b,daily.sd.b,by="Group.1")
summary.b<-merge(summary.b,daily.se.b,by="Group.1")
summary.b<-merge(summary.b,daily.n.samples,by="Group.1")
colnames(summary.b)<-c("date","mean","sd","se","n.samples")


summary.b$tot<-summary.b$mean*288
summary.a$tot<-summary.a$mean*288

#total variance
tot.var.b<-sum(288*(288-summary.b$n.samples)*summary.b$sd^2/summary.b$n.samples)
tot.var.a<-sum(288*(288-summary.a$n.samples)*summary.a$sd^2/summary.a$n.samples)

df.func<-function(N,n,s2)
{
a=((N*(N-n))/n)
(sum(a*s2)^2)/(sum(((a*s2)^2)/(n-1)))
}
df.b<-df.func(288,summary.b$n.samples,summary.b$sd^2)
crit.b <- qt(1 - 0.05 / 2, df.b)
b.CI.5<-crit.b*sqrt(tot.var.b)

df.a<-df.func(288,summary.a$n.samples,summary.a$sd^2)
crit.a <- qt(1 - 0.05 / 2, df.a)
a.CI.5<-crit.a*sqrt(tot.var.a)

print(paste(round(sum(summary.a$tot),0),"+/-",round(a.CI.5,0),"alewife"))

print(paste(round(sum(summary.b$tot),0),"+/-",round(b.CI.5,0),"blueback herring"))

#make date time object in summary dfs
summary.a$datetime<-as.POSIXct(paste0(summary.a$date, "00:00",sep= " "),format="%Y-%m-%d %H:%M")
summary.b$datetime<-as.POSIXct(paste0(summary.b$date, "00:00",sep= " "),format="%Y-%m-%d %H:%M")

#plot standardized catch
png("Anniscatchdata.png",width=16,height=9,units = "in",res=100)
par(omi=c(0.5,0.5,0.5,0.2),mai=c(.75,1.25,.25,.2), mfrow=c(1,2),cex=1.5)

plot(dat$datetime,dat$a.catch.5,ylim=c(0,450),xlab="",ylab="",xaxt="n",yaxt="n")
points(summary.a$datetime,summary.a$mean,pch=15)
axis(1,at=date.xaxis,labels=c("May 10", "May 12", "May 14", "May 16", "May 18"," May 20", "May 22", "May 24", "May 26"))
axis(2,las=2)
mtext("Number of fish",2,line=3,las=3,cex=1.5)
mtext("Date",1,line=3,cex=1.5)
mtext("Alewife",3,adj=0,cex=1.5)

plot(dat$datetime,dat$b.catch.5,ylim=c(0,450),xlab="",ylab="",xaxt="n",yaxt="n")
points(summary.b$datetime,summary.b$mean,pch=15)
axis(1,at=date.xaxis,labels=c("May 10", "May 12", "May 14", "May 16", "May 18"," May 20", "May 22", "May 24", "May 26"))
axis(2,las=2)
mtext("Number of fish",2,line=3,las=3,cex=1.5)
mtext("Date",1,line=3,cex=1.5)
mtext("Blueback herring",3,adj=0,cex=1.5)
dev.off()

#daily variance
#not sure about poission CIs

# for(i in 1:nrow(summary.a))
# {
#   summary.a$df[i]<-df.func(288,summary.a$n.samples[i],summary.a$sd[i]^2)
#   summary.b$df[i]<-df.func(288,summary.b$n.samples[i],summary.b$sd[i]^2)
# }
# # summary.a$clow<-(summary.a$mean-qt(1-0.2/2,summary.a$df)*summary.a$sd)*288
# # summary.a$chigh<-(summary.a$mean+qt(1-0.2/2,summary.a$df)*summary.a$sd)*288
# 
# summary.a$clow<-(0.5*qchisq(0.975,summary.a$mean*2))*288
# summary.a$chigh<-(0.5*qchisq(0.025,(summary.a$mean+1)*2))*288
# 
# summary.b$clow<-(0.5*qchisq(0.975,summary.b$mean*2))*288
# summary.b$chigh<-(0.5*qchisq(0.025,(summary.b$mean+1)*2))*288
# 
# plot(summary.b$date,summary.b$tot,pch=19,ylim=c(0,max(summary.b$tot)*1.2))
# for(i in 1:nrow(summary.b))
# {
#   segments(summary.b$date[i],summary.b$clow[i],summary.b$date[i],summary.b$chigh[i])
# }
# 
# plot(summary.b$date,summary.b$mean)
# for(i in 1:nrow(summary.b))
# {
#   segments(summary.b$date[i],summary.b$mean[i]+summary.b$se[i],summary.b$date[i],summary.b$mean[i]-summary.b$se[i])
# }