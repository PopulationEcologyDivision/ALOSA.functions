#...............................................................................
#...............................................................................
#
# This script is designed to run all of the functions needed to calculate
#
# Escapement, run timing, and biological characteristics.
# 
# Each site will have to be assessed separately:
#
#...............................................................................
#...............................................................................
require(ROracle)
#-------------------------------------------------------------------------------
# setwd(choose.dir(caption = "Navigate to Desired WORKING DIRECTORY"))
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
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets")
year<-2022
site<-2 # Main ones are 3=Gaspereau River at White Rock, 1=Carleton and 2=Vaughan
nspp<-2 # Either 1 or 2
sppID<-sppID #Either 3501 for Alewife or 3502 for BB
seed=117 #Seed used for scale selection. 
nsamples=500  #Number of scale selected to be aged
accessory.datafile="TUSKET_2022_VAUGHAN_accessory_data.csv"
#---...---...---...---...---...---...---...---...---...---...---...---...---...
# Species split

species.split<-split.spp(year,site,channel,accessory.datafile)

if(nspp==1){
  daily.count<-onespecies.river.escapement(year=year,
                              site=site,
                              channel=channel) 
  }
if(nspp==2){
  out<-twospecies.river.escapement(year=year,
                                  site=site,
                                  channel=channel,
                                  species.split=species.split)  
}

##the list outputs as 172 long because of all the print objects. if the stuff 
##printed in the function changes, the below will not work
daily.summary.B<-out[[170]]
daily.summary.A<-out[[172]] ##This still has dayofyear out of order
#Get bio data from DB
bio.data.A<-get.bio.data(year=year,siteID = site, sppID=3501, channel)
bio.data.B<-get.bio.data(year=year,siteID = site, sppID=3502, channel)

missingdays<-missing.days(bio.data.A)

# mergedays<- c(117,120,120,123,127,130,134,137) # For missing sample days, we merge the counts from two days
                # and use that in the weighting calculation. 
                # For example, ff day 112 is missing then decide if you want to merge the counts
                # with day 111 or 113. Do this for all the missing dates and provide the
                # replacement days in this vector. Length(mergedays)==Length(missingdays)
mergedays<-NULL
missingdays<-NULL
####Only run this once! Completed July####            
# ageing.selection(daily.summary.A,bio.data.A,missingdays,mergedays,seed,nsamples)

##data cleanup
daily.summary.A$dayofyear<-as.integer(levels(daily.summary.A$dayofyear))
daily.summary.B$dayofyear<-as.integer(levels(daily.summary.B$dayofyear))

daily.summary.A$total<-round(daily.summary.A$total,0)
daily.summary.A$sd<-round(daily.summary.A$sd,0)
daily.summary.A$clow<-round(daily.summary.A$clow,0)
daily.summary.A$chigh<-round(daily.summary.A$chigh,0)

daily.summary.B$total<-round(daily.summary.B$total,0)
daily.summary.B$sd<-round(daily.summary.B$sd,0)
daily.summary.B$clow<-round(daily.summary.B$clow,0)
daily.summary.B$chigh<-round(daily.summary.B$chigh,0)

##OK now gotta do the stupid messed up counts at Carleton and Powerhouse.
##both have entire missed days. Total count is meaningless, so output
##should be table of daily escapements

#### Carleton ####
##The count file has counts in it that are bad - ie days where mass downstream
##migration was occuring. Students were told not to count times when there was
##a lot moving down, but they did count when there was a lot moving up.
##the large numbers moving up are likely downstream migrants going back and
##forth. those entire days should be removed from the count. we will do this
## here, and write out a csv that can then be read into the escapement script
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets/Counts")
#### Run to get orignal count file cleaned - Completed July 27 2022 ####
# filename="Carleton Count sheet 2022 - Sheet1.csv"
# count.data=read.csv(filename,header=T,stringsAsFactors = F)
# ##manually delete counts based on their comments
# count.data$count.upstream[470:491]<-NA ##SH video partially obstructed by grate, count could be inaccurate
# count.data$count.downstream[470:491]<-NA ##SH video partially obstructed by grate, count could be inaccurate
# count.data$count.upstream[1060]<-NA #screen partially covered by spider 
# count.data$count.downstream[1060]<-NA #screen partially covered by spider 
# count.data$count.upstream[1075]<-NA #screen partially covered by spider 
# count.data$count.downstream[1075]<-NA #screen partially covered by spider 
# count.data$count.upstream[1181]<-NA #screen partially covered by spider 
# count.data$count.downstream[1181]<-NA #screen partially covered by spider 
# count.data$count.upstream[1254]<-NA #screen partially covered by spider 
# count.data$count.downstream[1254]<-NA #screen partially covered by spider 
# ##downstream migration begins May 23 evening.
# count.data$count.upstream[1266:1268]<-NA #downstream movement
# count.data$count.downstream[1266:1268]<-NA #downstream movement
# ##entire day of 24th, 25th, and 26th should be removed due to downstream migration
# count.data$count.upstream[1273:1344]<-NA #downstream movement
# count.data$count.downstream[1273:1344]<-NA #downstream movement
# ##Spidertime
# count.data$count.upstream[1376:1381]<-NA #SH left side of camera obstructed (spider)
# count.data$count.downstream[1376:1381]<-NA #SH left side of camera obstructed (spider)
# count.data$count.upstream[1393:1394]<-NA #SH left side of camera obstructed (spider)
# count.data$count.downstream[1393:1394]<-NA #SH left side of camera obstructed (spider)
# count.data$count.upstream[1410:1414]<-NA #Spider and fallback
# count.data$count.downstream[1410:1414]<-NA #Spider and fallback
# ##All fallback from May 31 afternoon onward
# ##keep counts from all strata on May 31, NA for fallbacks
# count.data$count.upstream[1456:1464]<-NA #Spider and fallback
# count.data$count.downstream[1456:1464]<-NA #Spider and fallback
# count.data<-count.data[1:1464,]
# 
# write.csv(count.data,"Carleton Count Sheet Cleaned 2022.csv",row.names=F,na="")
#### Get Carleton Count summary from file ####
carleton.summary<-onespecies.partial.river.escapement(filename="Carleton Count Sheet Cleaned 2022.csv",
                                                      fixtime=T,
                                                      database=F)
##Re dd gaps for days with no counts
carleton.summary$dayofyear<-as.integer(levels(carleton.summary$dayofyear))
carleton.dayofyear.key<-as.data.frame(c(min(carleton.summary$dayofyear):max(carleton.summary$dayofyear)))
names(carleton.dayofyear.key)<-"dayofyear"
carleton.summary<-merge(carleton.summary,carleton.dayofyear.key,by="dayofyear",all.y=T)

##Clean up data
carleton.summary$total<-round(carleton.summary$total,0)
carleton.summary$sd<-round(carleton.summary$sd,0)
carleton.summary$clow<-round(carleton.summary$clow,0)
carleton.summary$chigh<-round(carleton.summary$chigh,0)


#### Powerhouse Ladder ####
# filename="Secret Ladder 2022 - Sheet1.csv"
# count.data=read.csv(filename,header=T,stringsAsFactors = F)
# count.data$count.upstream[596]<-NA #JB - a lot are comming down hard to see
# count.data$count.downstream[596]<-NA #JB - a lot are comming down hard to see
# count.data$count.upstream[726]<-NA #BR - Video obstructed 
# count.data$count.downstream[726]<-NA #BR - Video obstructed 
# ##Strata 5 of June 6th is likely downstream migration
# ##Could be due to changes in flow? flow appears same to June 5th though
# ##Entire day of june 6th has relatively large number of downstream counts
# ##Remove entire day after analysis?
# count.data$count.upstream[913:946]<-NA #BR - Video obstructed 
# count.data$count.downstream[913:946]<-NA #BR - Video obstructed
# write.csv(count.data,"Powerhouse Count Sheet Cleaned 2022.csv",row.names=F,na="")

powerhouse.summary<-onespecies.partial.river.escapement(filename="Powerhouse Count Sheet Cleaned 2022.csv",
                                                        fixtime=F,
                                                        database=F)

##Re dd gaps for days with no counts
powerhouse.summary$dayofyear<-as.integer(levels(powerhouse.summary$dayofyear))
powerhouse.dayofyear.key<-as.data.frame(c(min(powerhouse.summary$dayofyear):max(powerhouse.summary$dayofyear)))
names(powerhouse.dayofyear.key)<-"dayofyear"
powerhouse.summary<-merge(powerhouse.summary,powerhouse.dayofyear.key,by="dayofyear",all.y=T)

##Clean up data
powerhouse.summary$total<-round(powerhouse.summary$total,0)
powerhouse.summary$sd<-round(powerhouse.summary$sd,0)
powerhouse.summary$clow<-round(powerhouse.summary$clow,0)
powerhouse.summary$chigh<-round(powerhouse.summary$chigh,0)





stop()

reference.point.plot(400000 ,232400 ,0.53,0.35,rivername="Gaspereau River",
                     pointsX=TR$escape[TR$species=="A"],pointsY=TR$er[TR$species=="A"],
                     label="",plotchar = "")

ssnmsy=3098547
Fupper=0.53

series82.84=GR[GR$year>=1982 & GR$year<=1984,]
series97.06=GR[GR$year>=1997 & GR$year<=2006,]
series15.19=GR[GR$year>=2015 & GR$year<=2019,]
series21.present=GR[GR$year>=2021,]

points(series82.84$escape/ssnmsy,series82.84$er/Fupper,pch=1)
lines(series82.84$escape/ssnmsy,series82.84$er/Fupper)

points(series97.06$escape/ssnmsy,series97.06$er/Fupper,pch=4)
lines(series97.06$escape/ssnmsy,series97.06$er/Fupper)

points(series15.19$escape/ssnmsy,series15.19$er/Fupper,pch=16)
lines(series15.19$escape/ssnmsy,series15.19$er/Fupper)

points(series21.present$escape/ssnmsy,series21.present$er/Fupper,pch=19)
lines(series21.present$escape/ssnmsy,series21.present$er/Fupper)


text(0.07,1.2,"1982-\n84",col="gray35",cex=0.85)
text(0.23,1.63,"1997",pos=4,col="gray35",cex=0.85)
text(0.6,0.62,"2001",pos=1,col="gray35",cex=0.85)
text(0.8,1.47,"2013",col="gray35",cex=0.85)
text(1.1,1.23,"2015-16",pos=4,col="gray35",cex=0.85)
text(2.8,0.67,"2017",pos=1,col="gray35",cex=0.85)
text(2.8,0.85,"2018",pos=3,col="gray35",cex=0.85)
text(2.43,0.81,"2019",pos=3,col="gray35",cex=0.85)
text(2.5,1.05,"2021",pos=3,col="red",cex=0.85)
arrows(0.65,1.45,0.43,1.37,length=0.1,col="gray35",cex=0.85)















### NOT CHECKED BELOW THIS POINT


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



