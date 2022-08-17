###IN PRogress
##pulling together a brief summary of gill vs trap net catches in SJR for RM
##Needs a MARFIS pull in the environment to work, specifically the catch obj


library(dplyr)


catch.sj<-catch[catch$RIVERNAME_CLEANED=="SAINT JOHN",]
catch.sj<-catch.sj[complete.cases(catch.sj[,'LICENCE_ID']),]



catch.sj<-convert.KGS(catch.sj)

gill<-catch.sj[catch.sj$FV_GEAR_CODE==41 | catch.sj$FV_GEAR_CODE==42,]
trap<-catch.sj[catch.sj$FV_GEAR_CODE==62,]

table(trap$LICENCE_ID,trap$YEAR)
table(gill$LICENCE_ID,gill$YEAR)


# gill.ind<-gill[gill$LICENCE_ID==120121,]
# trap.ind<-trap[trap$LICENCE_ID==120760,]
# gill.121<-aggregate(gill.ind$KGS,by=list(YEAR=gill.ind$YEAR),FUN="sum")
# trap.121<-aggregate(trap.ind$KGS,by=list(YEAR=trap.ind$YEAR),FUN="sum")

# plot(trap.121$YEAR,trap.121$x)
# lines(gill.121$YEAR,gill.121$x)

gill.sum<-aggregate(gill$KGS,by=list(gill$LICENCE_ID,gill$YEAR),FUN="sum")
trap.sum<-aggregate(trap$KGS,by=list(trap$LICENCE_ID,trap$YEAR),FUN="sum")

plot(trap.sum$Group.2,trap.sum$x)
points(gill.sum$Group.2,gill.sum$x,col="red")

plot(jitter(as.numeric(trap.sum$Group.2)),trap.sum$x)
points(jitter(as.numeric(gill.sum$Group.2)),gill.sum$x,col="red")


##model
gill.sum$gear<-"gill"
trap.sum$gear<-"trap"
df<-rbind(gill.sum,trap.sum)
names(df)<-c("Licence", "Year", "Catch","Gear")

#Trim off bad data
##no 2008 or 2022 for starters
df<-df[df$Year>2008 & df$Year<2022,]

fit<-lm(Catch~Gear+Year,data=df)
summary(fit)

##plot
boxplot(Catch~Gear+Year,data=df,col=c("red","blue"),
        names=c("2009","","2010","","2011","","2012","","2013","","2014","",
                "2015","","2016","","2017","","2018","","2019","","2020","","2021",""
                ),
        )
legend("topright", fill = c("red","blue"), legend = c("Gill", "Trap"), horiz = T)


mean(df$Catch[df$Gear=="trap"])
mean(df$Catch[df$Gear=="gill"])

sub<-df[df$Year==2019 & df$Gear=="gill",]


####Only Gill net catch####
gill.sub<-gill[gill$YEAR==2017 | gill$YEAR==2018 | gill$YEAR==2019,]
gill.sub<-gill.sub[complete.cases(gill.sub[,'LICENCE_ID']),]

gill.sub$KGS[gill.sub$LICENCE_ID==120121]<-gill.sub$KGS[gill.sub$LICENCE_ID==120121]/2.2

gill.sub.sum<-aggregate(gill.sub$KGS,by=list(gill.sub$LICENCE_ID,gill.sub$YEAR),FUN="sum")

##add in some missing data manually
gill.sub.sum[nrow(gill.sub.sum)+1,]<-c("120274","2017",9420+8723+2664)
gill.sub.sum[nrow(gill.sub.sum)+1,]<-c("120274","2018",11352+2264)
gill.sub.sum$x<-as.numeric(gill.sub.sum$x)
##potential metrics for "typical" catch

plot(gill.sub.sum$x)
boxplot(gill.sub.sum$x)
mean(gill.sub.sum$x)
median(gill.sub.sum$x)

mean(gill.sub.sum$x[gill.sub.sum$x>500])
median(gill.sub.sum$x[gill.sub.sum$x>500])

x<-tail(sort(gill.sub.sum$x),5)
mean(x)
median(x)

x<-tail(sort(gill.sub.sum$x),10)
mean(x)
median(x)

gill.sub.sum$x<-round(gill.sub.sum$x,0)
names(gill.sub.sum)<-c("Licence ID", "Year", "Catch (kg)")
write.csv(gill.sub.sum,file="SJR gillnet catch 2017_2019.csv",row.names = F)

###indv licences
##2017 120121 - Number vs weight in kg issues
##2017 120288 - catch high, logbook listed as kg but in MARFIS as pounds (if this is an error, catch would be even higher!)
##2019 120274 - set 180 fa of gear code 41 AND 42 - double a typical licence????
##2017 120052 - no apparent errors, 150 fathoms of net
##2017 120285 - Wrong unit for most entries, log book indicates pounds. CORRECTED ABOVE


catch.sub<-catch[catch$LICENCE_ID==120088,]

catch.gasp<-catch[catch$RIVERNAME_CLEANED=="GASPEREAU",]
