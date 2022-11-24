catch.g<-catch[catch$RIVERNAME_CLEANED=="GASPEREAU",]
catch.g<-catch.g[complete.cases(catch.g[,'LICENCE_ID']),] 

##from combing through logbooks
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120146]<-"KILOGRAMS"
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120242]<-"KILOGRAMS"
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120381]<-"KILOGRAMS"

catch.g<-convert.KGS(catch.g)

sum(catch.g$KGS[catch.g$YEAR==2019])

catch.sub<-catch.g[catch.g$GEAR_DESCRIPTION!="SQUARE NET",]
catch.ind<-catch.g[catch.g$LICENCE_ID==120315,]

daykey<-c(4,5,9,12,16,19,22,23,25,26,29)

catch.ind17<-subset(catch.ind,DAY %in% daykey & YEAR==2018)

x<-sum(catch.ind17$FV_WEIGHT)+200+100+250+250

x1=aggregate(catch.ind$KGS, by=list(YEAR=catch.ind$YEAR),FUN=sum)
