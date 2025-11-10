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
# dat$prop<-dat$number.blueback/(dat$number.blueback+dat$number.alewife)
# plot(dat$datetime,dat$prop)

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
crit.b*sqrt(tot.var.b)

df.a<-df.func(288,summary.a$n.samples,summary.a$sd^2)
crit.a <- qt(1 - 0.05 / 2, df.a)
crit.a*sqrt(tot.var.a)



#daily variance
#this all wrong, calc CI's then scale up I think

for(i in 1:nrow(summary.a))
{
  summary.a$df[i]<-df.func(288,summary.a$n.samples[i],summary.a$sd[i]^2)
  summary.b$df[i]<-df.func(288,summary.b$n.samples[i],summary.b$sd[i]^2)
}
# summary.a$clow<-(summary.a$mean-qt(1-0.2/2,summary.a$df)*summary.a$sd)*288
# summary.a$chigh<-(summary.a$mean+qt(1-0.2/2,summary.a$df)*summary.a$sd)*288

summary.a$clow<-(0.5*qchisq(0.975,summary.a$mean*2))*288
summary.a$chigh<-(0.5*qchisq(0.025,(summary.a$mean+1)*2))*288

summary.b$clow<-(0.5*qchisq(0.975,summary.b$mean*2))*288
summary.b$chigh<-(0.5*qchisq(0.025,(summary.b$mean+1)*2))*288

plot(summary.b$date,summary.b$tot,pch=19,ylim=c(0,max(summary.b$tot)*1.2))
for(i in 1:nrow(summary.b))
{
  segments(summary.b$date[i],summary.b$clow[i],summary.b$date[i],summary.b$chigh[i])
}

