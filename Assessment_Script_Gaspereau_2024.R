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
# Only run at beginning of season!
# blank.datasheets(seed=115,startmonth=4,endmonth=6,startday=15,rivername="White Rock",
#                  year=2024,recordtime=T,speciesID=T,strata=5,samplesperstrata=5)
# make.count.filename.textfile("White Rock 2024 count data.csv","WRock",2024)
# 
# blank.datasheets(seed=116,startmonth=4,endmonth=6,startday=15,rivername="Lanes Mills",
#                  year=2024,recordtime=T,speciesID=T,strata=5,samplesperstrata=5)
# make.count.filename.textfile("Lanes Mills 2024 count data.csv","Lanes Mills",2024)
# 
# blank.datasheets(seed=117,startmonth=4,endmonth=6,startday=15,rivername="Below Lanes Mills",
#                  year=2024,recordtime=T,speciesID=T,strata=5,samplesperstrata=5)
# make.count.filename.textfile("Below Lanes Mills 2024 count data.csv","Below Lanes Mills",2024)
#...............................................................................
#### In Season Count ####
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/Gaspereau 2024")

# filename="White Rock 2024 count data.csv"
# fixtime=T
# database=F
# year=2024
# site=3
# channel=channel

x<-onespecies.river.escapement("White Rock 2024 count data.csv",fixtime=T,database=F,downstream.migration = F,2024,3,channel)

# x<-onespecies.partial.river.escapement("White Rock Counts - Sheet1.csv",fixtime=T,database=F,2023,3,channel)

x<-round(x)
n<-dim(x)[1]
x<-x[1:n-1,]

print(paste0("Total escapement as of ",x$mon[n],"-",x$day[n]," is ",sum(x$total),sep=""))

write.csv(x,file="inseasonsummary.csv",row.names=F) # for in season emails

#plot for in season emails
old.data<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Gaspereau River/data for multi year gaspereau plot.csv")

old.data$date=as.Date(paste(old.data$day,old.data$mon,2024,sep="-"),
                      format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
old.data$dayofyear=as.numeric(strftime(old.data$date, format="%j"))

plot(x$dayofyear,x$total,type="l", xlim=c(min(x$dayofyear)-4,min(x$dayofyear)+65), ylim=c(0,120000),lwd=2,
     ylab="Number of Fish",xlab="Day of Year")
# lines(old.data$dayofyear,old.data$Total2015,type="l",col="red")
# lines(old.data$dayofyear,old.data$Total2016,type="l",col="orange")
# lines(old.data$dayofyear,old.data$Total2017,type="l",col="yellow")
# lines(old.data$dayofyear,old.data$Total2018,type="l",col="green")
# lines(old.data$dayofyear,old.data$Total2019,type="l",col="blue")
# lines(old.data$dayofyear,old.data$Total2021,type="l",col="purple")
# lines(old.data$dayofyear,old.data$Total2022,type="l",col="brown")
# lines(old.data$dayofyear,old.data$Total2023,type="l",col="brown")

lines(old.data$dayofyear,old.data$Total2017,type="l",col="red")
lines(old.data$dayofyear,old.data$Total2018,type="l",col="orange")
lines(old.data$dayofyear,old.data$Total2019,type="l",col="yellow")
lines(old.data$dayofyear,old.data$Total2021,type="l",col="green")
lines(old.data$dayofyear,old.data$Total2022,type="l",col="blue")
lines(old.data$dayofyear,old.data$Total2023,type="l",col="purple")

legend(110, 100000, legend=c("2017", "2018", "2019", "2021", "2022", "2023", "2024"),
       col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), lty=1, cex=0.8)

#### Post season ####
seed=667 #Seed used for scale selection. 
nsamples=500  #Number of scale selected to be aged

bio.data<-read.csv("White Rock 2024 biocharacteristics data.csv")
# bio.data$mon<-ifelse(bio.data$mon=="April",4,ifelse(bio.data$mon=="May",5,ifelse(bio.data$mon=="May ",5,6)))
write.csv(bio.data,"WRbiodata corrected.csv")

#Check Bio data
checkers("White Rock 2024 biocharacteristics data.csv") # this file has the correct column names, same data as WE 2023 bio data.csv

countdata<-daily.count<-x

bio.data<-bio.data[,1:11]
colnames(bio.data)<-c("SITE_ID", "YEAR", "DAY", "MON","FISH_ID", "SPECIES_ID",
                                       "SEX_ID","FORK_LENGTH","WEIGHT","SCALE","NOTES") 
biodata<-bio.data
missingdays<-missing.days(bio.data)
mergedays<-missingdays-1 #in both cases the two previous days are most similar in count
# For missing sample days, we merge the counts from two days
# and use that in the weighting calculation. 
# For example, ff day 112 is missing then decide if you want to merge the counts
# with day 111 or 113. Do this for all the missing dates and provide the
# replacement days in this vector. Length(mergedays)==Length(missingdays)


#ran June 24th 2024 to get ages for student work      
ageing.selection(daily.count,bio.data,missingdays,mergedays,seed,nsamples)
scale.age<-read.csv("to be aged_rename this file.csv")

#### I forget what this stuff is####
biodata<-bio.data
biodata$date=as.Date(paste(biodata$DAY,biodata$MON,biodata$YEAR,sep="-"),
                     format="%d-%m-%Y")

#dayofyear uses "strftime" to evaluate which day of the year each date aligns with
biodata$dayofyear=as.numeric(strftime(biodata$date, format="%j"))


scaledata=biodata[biodata$SCALE=="Y",]
n.sampled<-aggregate(scaledata$dayofyear,by=list(scaledata$dayofyear),FUN=function(x){length(x[!is.na(x)])})

colnames(n.sampled)=c("dayofyear","n.sampled")

biodata.with.weights<-merge(biodata,n.sampled,by="dayofyear",all.x=T)

for(i in 1:length(missingdays)){
  countdata$dayofyear[countdata$dayofyear==missingdays[i]]<-mergedays[i]
}

mergedcountdata=aggregate(countdata$total,by=list(countdata$dayofyear),FUN=sum)
colnames(mergedcountdata)=c("dayofyear","merged.total")

biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                            by="dayofyear",all.x=T)


biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled

hghghg<-biodata.with.weights[-c(1508,1564),]

####recount of WR to get percent error####
#sim code
set.seed(seed)
counts<-read.csv("White Rock 2024 count data.csv")
counts$number.up<-counts$count.upstream-counts$count.downstream
counts$dev<-rnorm(nrow(counts),1,0.1)
counts$recount<-round(counts$number.up*counts$dev,0)
counts.sub<-counts[counts$number.up!=0,]
counts.sub<-counts.sub[complete.cases(counts.sub),]
counts.sample<-sample(1:nrow(counts.sub),200,replace=F) #this will draw 200 random counts to redo
counts.sub<-counts.sub[counts.sample,]
counts.sub$per.diff<-(counts.sub$number.up-counts.sub$recount)/((counts.sub$number.up+counts.sub$recount)/2)

#we can use this to make a count sheet to redo these randomly selected counts
counts.sub1<-counts.sub[,c(1:5,9)]
names(counts.sub1)[6]<-"previous counter"
counts.sub1<-counts.sub1[order(counts.sub1$mon,counts.sub1$day,counts.sub1$time),]
counts.sub1$count.upstream<-NA
counts.sub1$count.downstream<-NA
counts.sub1$number.up<-NA
counts.sub1$camera.desc<-NA
counts.sub1$minutes<-NA
counts.sub1$seconds<-NA

write.csv(counts.sub1,"White Rock 2024 RECOUNTS.csv",row.names=F,na="")


##testing ageing selection
names(scale.age)[names(scale.age)=='sample']<-'FISH_ID'
scale.age1<-merge(scale.age,biodata.with.weights,by.x="FISH_ID")

n.selected<-aggregate(scale.age1$weighting,by=list(scale.age1$weighting),FUN=function(x){length(x[!is.na(x)])})
