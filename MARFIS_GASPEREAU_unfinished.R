catch.g<-catch[catch$RIVERNAME_CLEANED=="GASPEREAU",]

##from combing through logbooks
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120146]<-"KILOGRAMS"
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120242]<-"KILOGRAMS"
catch.g$MEASUREMENT_UNIT[catch.g$YEAR==2019 & catch.g$LICENCE_ID==120381]<-"KILOGRAMS"

catch.g<-convert.KGS(catch.g)

sum(catch.g$KGS[catch.g$YEAR==2019])

catch.sub<-catch.g[catch.g$GEAR_DESCRIPTION!="SQUARE NET",]
catch.ind<-catch.g[catch.g$LICENCE_ID==120124,]
