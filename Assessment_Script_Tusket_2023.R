#...............................................................................
#...............................................................................
#
# This script is designed to run all of the functions needed to calculate
#
# Escapement, run timing, and biological characteristics.
# 
# Each site and will have to be assessed separately:
#
#...............................................................................
#...............................................................................
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
#...............................................................................
#
#i.forgot.the.siteIDs(channel)
#...............................................................................
#### Season setup ####
#Only run at beginning of season!
# blank.datasheets(seed=112,startmonth=3,endmonth=6,startday=1,rivername="Vaughan",
#                  year=2023,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
# blank.datasheets(seed=113,startmonth=3,endmonth=6,startday=15,rivername="Powerhouse",
#                  year=2023,recordtime=T,speciesID=T,strata=6,samplesperstrata=4)
# make.count.filename.textfile("Powerhouse 2023 count data.csv","Powerhouse",2023)
#...............................................................................
#### In Season Count ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/Data Sheets")

x<-onespecies.river.escapement("Vaughan 2023 count data.csv",fixtime=T,downstream.migration=T,database=F,2023,2,channel)

x<-round(x)
n<-dim(x)[1]
x<-x[1:n-1,]
# x$dayofyear<-as.numeric(as.character(x$dayofyear))

print(paste0("Total escapement as of ",x$mon[n],"-",x$day[n]," is ",sum(x$total),sep=""))

write.csv(x,file="inseasonsummary.csv",row.names=F)

#### powerhouse ####
y<-onespecies.partial.river.escapement("Powerhouse 2023 count data1.csv",fixtime=T,database=F,2023,14,channel)
y<-onespecies.river.escapement("Powerhouse 2023 count data1.csv",fixtime=T,downstream.migration=T,database=F,2023,14,channel)

##innovasea counts
# j<-data.frame(total.IS=c(2337,3204,2317,4586,6388,5952,4653,6641,4196,2585,3933,5153,3808,4942),
#               dayofyear=122:135)
is<-read.csv("Tusket Powerhouse Fishladder_Daily Counts_April1-June15.csv")
is<-is[1:76,] #get rid of totals at bottom of sheet
is$Date<-as.Date(is$Date)
is$dayofyear=as.numeric(strftime(is$Date, format="%j"))
is$count<-as.numeric(gsub("\\*","",is$Daily.Count.NSP.camera)) #remove asterisks from values and convert to numeric

plot(y$dayofyear,y$total,type="l",lwd=2)
lines(y$dayofyear,y$clow,lty=3)
lines(y$dayofyear,y$chigh,lty=3)
lines(is$dayofyear,is$count,col="red",lwd=2)

y1<-merge(y,is,by="dayofyear",all.x=T)
y1$diff<-y1$total-y1$count
y1$per.diff<-(y1$count-y1$total)/y1$total

test.df<-y1[y1$total>250 & !is.na(y1$count),]

t.test(test.df$total,test.df$count,paired = TRUE)


#why the discrepancy?
#half of it due to May 6 - may 10 (14.4k)
#look at time of counts
ph<-read.csv("Powerhouse 2023 count data1.csv")
#date conversion amalgamates month and year columns into one format
ph$date=as.Date(paste(ph$day,ph$mon,ph$year,sep="-"),
                        format="%d-%m-%Y")
#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
ph$dayofyear=as.numeric(strftime(ph$date, format="%j"))

ph<-ph[ph$dayofyear<172,]

# plot(ph$time,ph$dayofyear)
ggplot(ph,aes(time,dayofyear,size=count.upstream)) +
  geom_point() +
  geom_vline(xintercept=c(500,1045,1345,1645,2100))

#### plot ####
old.data<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/data for multi year tusket plot.csv")

old.data$date=as.Date(paste(old.data$day,old.data$mon,2023,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
old.data$dayofyear=as.numeric(strftime(old.data$date, format="%j"))

plot(x$dayofyear,x$total,type="l", xlim=c(min(x$dayofyear),min(x$dayofyear)+80), ylim=c(0,120000),lwd=2,
     ylab="Number of Fish",xlab="Day of Year")
lines(old.data$dayofyear,old.data$Total2014,type="l",col="red")
lines(old.data$dayofyear,old.data$Total2015,type="l",col="orange")
lines(old.data$dayofyear,old.data$Total2018,type="l",col="yellow")
lines(old.data$dayofyear,old.data$Total2019,type="l",col="green")
lines(old.data$dayofyear,old.data$Total2021,type="l",col="blue")
lines(old.data$dayofyear,old.data$Total2022,type="l",col="purple")
segments(119,0,119,120000)
segments(133,0,133,120000)
legend(100, 100000, legend=c("2014", "2015", "2018", "2019", "2021", "2022", "2023"),
       col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), lty=1, cex=0.8)

#...............................................................................
#### Post season ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/Data Sheets")
year<-year
site<-sitenumber # Main ones are 3=Gaspereau River at White Rock, 
#                  1=Carleton and 2=Vaughan. Use 'i.forgot.the.siteIDs(channel) for other locations
sppID<-sppID #Either 3501 for Alewife or 3502 for BB
seed=seed #Seed used for scale selection. 
nsamples=500  #Number of scale selected to be aged

# If needed:
# species.split<-split.spp(year,site,channel,"accessory_data.csv")
##since no biodata exists, just manually create the output of the spp.split function
species.split<-read.csv("Tusket 2023 Vaughan accessory data.csv")
extra.days<-data.frame(day=23:25,
                       mon=c(6,6,6),
                       all=c(NA,NA,NA),
                       BB=c(NA,NA,NA),
                       BBprop=c(NA,NA,NA))
species.split<-rbind(species.split,extra.days)

species.split$day.int<-1:nrow(species.split)
species.split$BBprop[29]<-NA#get rid of last day of sampling with only 2 fish

dat<-data.frame(x=species.split$day.int,y=species.split$BBprop)
dat<-dat[complete.cases(dat),]


glmfit1<-glm(BB~day.int,data=species.split,offset=log(all),family="poisson")

out<-data.frame(day.int=1:32,
                all=rep(1,32)) ##all is set to 1 to give proportions

# out.pred<-predict(glmfit,newdata=out,type="response") #outputs fitted values for day/int in out
##gets overwritten by below. learned below from https://stackoverflow.com/questions/40985366/prediction-of-poisson-regression
out.predci<-predict(glmfit1,newdata=out,type="link",se.fit=T) #list of 3
ginv <- glmfit1$family$linkinv
out.pred<-ginv(out.predci[[1]])
out.predlo<-ginv(out.predci[[1]]-1.96*out.predci[[2]])
out.predhi<-ginv(out.predci[[1]]+1.96*out.predci[[2]])

##fill species split back in with fitted values
##use predict values and lo/hi CI for the three scenarios
species.split1<-species.split
species.split2<-species.split
species.split3<-species.split

for(i in 1:nrow(species.split))
{
  if(is.na(species.split$BBprop[i])==T)
  {
    species.split1$BBprop[i]<-out.pred[i]
    species.split2$BBprop[i]<-out.predlo[i]
    species.split3$BBprop[i]<-out.predhi[i]
  }
}


#counts run off uploaded count data. The secies split scenario with the most likely
#predicted blueback proportion values (rather than high/low confidence interval values)
#are used. Downstream migration correction is applied due to the apparent net downstream
#movement occurring between May 25 and June 3rd at night time.
#other species splti scenarios can be run by changing the species split argument
daily.count<-twospecies.river.escapement(fixtime=T,
                                           downstream.migration=T,
                                           database=T,
                                           year=2023,
                                           site = 2,
                                           channel=channel,
                                           species.split=species.split1)
#assign species specific counts to objects and clean foramt
va<-daily.count[[1]]
vb<-daily.count[[2]]
va$dayofyear<-as.integer(levels(va$dayofyear))
vb$dayofyear<-as.integer(levels(vb$dayofyear))
###IMPORTANT! powerhouse counts are uploaded to database, but need some processing
### before the escapement script will run, due to large gaps in March and April.
### trim so counts fun from April 25 to end, June 19. or run from csv on R drive
ph1<-twospecies.river.escapement("Powerhouse 2023 count data1.csv",
                                 fixtime=T,
                                 downstream.migration=T,
                                 database=F,
                                 2023,
                                 14,
                                 channel,
                                 species.split=species.split1)
pa<-ph1[[1]]
pb<-ph1[[2]]
pa$dayofyear<-as.integer(levels(pa$dayofyear))
pb$dayofyear<-as.integer(levels(pb$dayofyear))

##combined counts
data=read.csv("//ent.dfo-mpo.ca/atlshares/Science/Population Ecology Division/DFD/Alosa/Assessment Results/daily_counts_all years_all rivers.csv",
              header=T,stringsAsFactors = F)
data$doy=as.Date(paste(data$day,data$mon,data$year,sep="-"),format="%d-%m-%Y")
data$doy=as.numeric(strftime(data$doy, format="%j"))
TRdata=data[data$river=="tusket",]
combtotalA<-TRdata[TRdata$year==2023 & TRdata$species=="A",]
combtotalB<-TRdata[TRdata$year==2023 & TRdata$species=="B",]

####PLOT FOR AC MEETING####
par(oma=c(7,3,3,0.5),mfrow=c(3,1),mar=c(0,5,0,3))
  #plot one
  plot(1,1, pch=" ",xlab="", ylab="",ylim=c(0,(max(va$total*1.2/1000))),xlim=c(96,177),xaxt="n",yaxt='n',cex.axis=2)
  rect(91, -5, 121, (max(va$total)*1.2/1000)+5, border = NA,col="gray90")
  rect(152, -5, 182, (max(va$total)*1.2/1000)+5, border = NA,col="gray90")
  box()
  lines(va$dayofyear,va$total/1000,lty=1,lwd=1.5)
  lines(va$dayofyear,va$clow/1000,lty=2,lwd=1.5)
  lines(va$dayofyear,va$chigh/1000,lty=2,lwd=1.5)
  lines(vb$dayofyear,vb$total/1000,lty=1,lwd=1.5,col="blue")
  lines(vb$dayofyear,vb$clow/1000,lty=2,lwd=1.5)
  lines(vb$dayofyear,vb$chigh/1000,lty=2,lwd=1.5)
  text(94,max(va$total)/1000,"Vaughan",cex=2,pos=4)
  # text(171,max(TRdata$number[TRdata$year==plotyear]/1000),plotyear,cex=2)
  # text(94,max(TRdata$number[TRdata$year==plotyear]/1000),paste("Total     =", TR$escape[!is.na(TR$year==plotyear)& TR$year==plotyear & TR$species=="All"],sep=" "),cex=1.5,pos=4)
  # text(94,max(TRdata$number[TRdata$year==plotyear]/1000)*0.8,paste("Total A =", TR$escape[!is.na(TR$year==plotyear)& TR$year==plotyear & TR$species=="A"],sep=" "),cex=1.5,pos=4)
  # text(94,max(TRdata$number[TRdata$year==plotyear]/1000)*0.6,paste("Total B =", TR$escape[!is.na(TR$year==plotyear)& TR$year==plotyear & TR$species=="B"],sep=" "),cex=1.5,pos=4)
  axis(2,cex.axis=1.5, at=c(10,20,30,40,50,60,70,80,90,100,110,120))
  
  plot(1,1, pch=" ",xlab="", ylab="",ylim=c(0,(max(pa$total*1.5/1000))),xlim=c(96,177),xaxt="n",yaxt='n',cex.axis=2)
  rect(91, -5, 121, (max(va$total)*1.2/1000)+5, border = NA,col="gray90")
  rect(152, -5, 182, (max(va$total)*1.2/1000)+5, border = NA,col="gray90")
  box()
  lines(pa$dayofyear,pa$total/1000,lty=1,lwd=1.5)
  lines(pa$dayofyear,pa$clow/1000,lty=2,lwd=1.5)
  lines(pa$dayofyear,pa$chigh/1000,lty=2,lwd=1.5)
  lines(pb$dayofyear,pb$total/1000,lty=1,lwd=1.5,col="blue")
  lines(pb$dayofyear,pb$clow/1000,lty=2,lwd=1.5)
  lines(pb$dayofyear,pb$chigh/1000,lty=2,lwd=1.5)
  text(94,max(pa$total)/1000,"Powerhouse",cex=2,pos=4)
  axis(2,cex.axis=1.5, at=c(2,4,6,8,10,12,14))

  
  plot(1,1, pch=" ",xlab="", ylab="",ylim=c(0,(max(combtotalA$number*1.2/1000))),xlim=c(96,177),xaxt="n",yaxt='n',cex.axis=2)
  rect(91, -5, 121, (max(combtotalA$number)*1.2/1000)+5, border = NA,col="gray90")
  rect(152, -5, 182, (max(combtotalA$number)*1.2/1000)+5, border = NA,col="gray90")
  box()
  lines(combtotalA$doy,combtotalA$number/1000,lty=1,lwd=1.5)
  lines(combtotalB$doy,combtotalB$number/1000,lty=1,lwd=1.5,col="blue")
  text(94,max(combtotalA$number)/1000,"Combined",cex=2,pos=4)
  axis(2,cex.axis=1.5, at=c(10,20,30,40,50,60,70,80,90,100,110,120))
axis(1,cex.axis=1.5, at=c(91,100,110,121,130,140,152,161,171), 
     labels=c("1","10","20","1","10","20","1","10","20"))

mtext("April",1,at=105,cex=1.5,line=3)
mtext("May",1,at=135,cex=1.5,line=3)
mtext("June",1,at=166,cex=1.5,line=3)
mtext("Date",1,at=135,cex=1.5,line=5.5)
mtext("Number of Fish (1000's)",2, outer=T,cex=1.5,las=3)
####Other stuff not important####
# 
# reference.point.plot(400000 ,232400 ,0.53,0.35,rivername="Gaspereau River",
#                      pointsX=TR$escape[TR$species=="A"],pointsY=TR$er[TR$species=="A"],
#                      label="",plotchar = "")
# 
# ssnmsy=3098547
# Fupper=0.53
# 
# series82.84=GR[GR$year>=1982 & GR$year<=1984,]
# series97.06=GR[GR$year>=1997 & GR$year<=2006,]
# series15.19=GR[GR$year>=2015 & GR$year<=2019,]
# series21.present=GR[GR$year>=2021,]
# 
# points(series82.84$escape/ssnmsy,series82.84$er/Fupper,pch=1)
# lines(series82.84$escape/ssnmsy,series82.84$er/Fupper)
# 
# points(series97.06$escape/ssnmsy,series97.06$er/Fupper,pch=4)
# lines(series97.06$escape/ssnmsy,series97.06$er/Fupper)
# 
# points(series15.19$escape/ssnmsy,series15.19$er/Fupper,pch=16)
# lines(series15.19$escape/ssnmsy,series15.19$er/Fupper)
# 
# points(series21.present$escape/ssnmsy,series21.present$er/Fupper,pch=19)
# lines(series21.present$escape/ssnmsy,series21.present$er/Fupper)
# 
# 
# text(0.07,1.2,"1982-\n84",col="gray35",cex=0.85)
# text(0.23,1.63,"1997",pos=4,col="gray35",cex=0.85)
# text(0.6,0.62,"2001",pos=1,col="gray35",cex=0.85)
# text(0.8,1.47,"2013",col="gray35",cex=0.85)
# text(1.1,1.23,"2015-16",pos=4,col="gray35",cex=0.85)
# text(2.8,0.67,"2017",pos=1,col="gray35",cex=0.85)
# text(2.8,0.85,"2018",pos=3,col="gray35",cex=0.85)
# text(2.43,0.81,"2019",pos=3,col="gray35",cex=0.85)
# text(2.5,1.05,"2021",pos=3,col="red",cex=0.85)
# arrows(0.65,1.45,0.43,1.37,length=0.1,col="gray35",cex=0.85)
# 
# 
# 
# 
# 
# ### NOT CHECKED BELOW THIS POINT
# 
# 
# onespecies.river.escapement("TUSKET_test.csv",database=F)
# 
# 
# 
# 
# 
# "UPDATE ALOSA_VIDEO_COUNT_DATA
#     SET ALOSA_VIDEO_COUNT_DATA.COUNT_DOWN=0
#     WHERE ALOSA_VIDEO_COUNT_DATA.COUNT_ID=747 AND
#           ALOSA_VIDEO_COUNT_DATA.SITE_ID=3 AND
#           ALOSA_VIDEO_COUNT_DATA.YEAR=2021 AND
#           ALOSA_VIDEO_COUNT_DATA.TIME=2020"
# 
# x=onespecies.river.escapement(year=2021,
#                               site=3,
#                               channel=channel)
# 
# x=onespecies.river.escapement(filename="gasp_count_2021.csv",
#                               year=2021,
#                               site=3,
#                               channel=channel,
#                               database=F)
# 
# 
# 
# agedata=dbGetQuery(channel, "SELECT * FROM ALOSA_FISH_AGE_DATA
#                    LEFT JOIN ALOSA_FISH_BIO_DATA ON
#                       ALOSA_FISH_BIO_DATA.FISH_ID= ALOSA_FISH_AGE_DATA.FISH_ID AND
#                       ALOSA_FISH_BIO_DATA.SITE_ID= ALOSA_FISH_AGE_DATA.SITE_ID AND
#                       ALOSA_FISH_BIO_DATA.YEAR= ALOSA_FISH_AGE_DATA.YEAR
#                    
#                    WHERE ALOSA_FISH_BIO_DATA.SITE_ID=2 AND
#                          ALOSA_FISH_BIO_DATA.YEAR=2021 AND
#                          ALOSA_FISH_BIO_DATA.SPECIES_ID=3502")
# 
# agedata=agedata.B
# 
# mean.age.all=mean(agedata$CURRENT_AGE)
# mean.age.r=mean(agedata$CURRENT_AGE[agedata$CURRENT_AGE>agedata$AGE_AT_FIRST_SPAWN])
# 
# 
# mean.firstspawnage.all=mean(agedata$AGE_AT_FIRST_SPAWN)
# mean.firstspawnage.r=mean(agedata$AGE_AT_FIRST_SPAWN[agedata$CURRENT_AGE>agedata$AGE_AT_FIRST_SPAWN])
# 
# 
# 
# 
#   
#   age.prop.matrix=matrix(rep(0,20),nrow=4, ncol=5,
#                          dimnames=list(c("First","Second","Third","Fourth"),
#                                        c("Age3","Age4","Age5","Age6","Age7")))
#   for (i in 3:7){
#     CurrentAge=agedata[agedata$CURRENT_AGE==i,c("CURRENT_AGE","AGE_AT_FIRST_SPAWN")]
#     for (j in i:3){
#       spawngroup=CurrentAge[CurrentAge$AGE_AT_FIRST_SPAWN==j,]
#       proportion=dim(spawngroup)[1]/(dim(agedata)[1])
#       #proportion.weighted=sum(spawngroup$age.weight)/sum(age.dat$age.weight)
#       if(proportion>0)age.prop.matrix[(i-(j-1)),(i-2)]=proportion
#       #if(proportion.weighted>0)age.prop.matrix.weighted[(i-(j-1)),(i-2)]=proportion.weighted
#     }
#   }
#   age.num.matrix=15394*age.prop.matrix/1000
#  
#    1941411
#   15394
#   
#   # Plot 1a: Proportion of popualtion by age and spawning history:
#   jpeg(width=7,height=5, file=paste("age.plot.prop.text.2021 for", spp,".jpeg", sep="")
#        ,units='in',res=200)
#   #x11(width=7,height=5)
#   barplot(age.prop.matrix[1:4,],ylim=c(0,(max(age.prop.matrix)+0.05)),
#           xlab="Age (Years)",ylab="Proportion",cex.lab=1.5,
#           col=c("#E69F00","#56B4E9","#009E73","#0072B2"))
#   legend(4.75,0.5,legend=c("First","Second","Third","Fourth"),
#          fill=c("#E69F00","#56B4E9","#009E73","#0072B2"),bty='n',
#          title="Number of\nSpawnings",title.adj=1)
#   dev.off()
#   # Plot 1b: Number of fish by age and spawning history:
#   jpeg(width=7,height=5, file=paste("age.plot.number.text.2021 for", spp,".jpeg", sep="")
#        ,units='in',res=200)
#   #x11(width=7,height=5)
#   barplot(age.num.matrix[1:4,],ylim=c(0,(8)),
#           xlab="Age (Years)",ylab="Thousands of Fish",cex.lab=1.5,
#           col=c("#E69F00","#56B4E9","#009E73","#0072B2"))
#   legend(4.75,7,legend=c("First","Second","Third","Fourth"),
#          fill=c("#E69F00","#56B4E9","#009E73","#0072B2"),bty='n',
#          title="Number of\nSpawnings",title.adj=1)
#   dev.off()
#   # # Plot 1c: Proportion of popualtion by age and spawning history (weighted):
#   # jpeg(width=7,height=5, file="age.plot.age.text.w.2018.jpeg",units='in',res=200)
#   # #x11(width=7,height=5)
#   # barplot(age.prop.matrix.weighted[1:4,],ylim=c(0,0.6),
#   # 								xlab="Age (Years)",ylab="Proportion",cex.lab=1.5,
#   # 								col=c("grey15","grey35","grey55","grey75"))
#   # legend(4.75,0.5,legend=c("First","Second","Third","Fourth"),
#   # 							fill=c("grey15","grey35","grey55","grey75"),bty='n',
#   # 							title="Number of\nSpawnings",title.adj=1)
#   # dev.off()
#   # # Plot 1d: Number of fish by age and spawning history:
#   jpeg(width=7,height=5, file="age.plot.num.text.2018.w.jpeg",units='in',res=200)
#   #x11(width=7,height=5)
#   barplot(age.num.matrix[1:4,],ylim=c(0,1600),
#   								xlab="Age (Years)",ylab="Thousands of Fish",cex.lab=1.5,
#   								col=c("grey15","grey35","grey55","grey75"))
#   legend(4.75,1500,legend=c("First","Second","Third","Fourth"),
#   							fill=c("grey15","grey35","grey55","grey75"),bty='n',
#   							title="Number of\nSpawnings",title.adj=1)
#   dev.off()
#   #----------------------------------------------------------------------
#   # Plot 2: Proportion of repeat spawners
#   jpeg(width=7,height=5, file=paste("Repeat spawners 2019 for", spp,".jpeg", sep="")
#        ,units='in',res=200)
#   barplot(c(sum(age.prop.matrix[1,]),
#             sum(age.prop.matrix[2,]),
#             sum(age.prop.matrix[3,]),
#             sum(age.prop.matrix[4,])),ylim=c(0,1),ylab="Proportion",
#           xlab="Spawning Event",cex.lab=1.5,
#           width=0.1,names.arg=c("First","Second","Third","Fourth"))
#   dev.off()
#   
#   boxplot(weight~current.age,data=speciesdata)
#   boxplot(fork.length~current.age,data=speciesdata)
#   with(speciesdata,plot(weight,fork.length))
# }
# 
# 
# 
