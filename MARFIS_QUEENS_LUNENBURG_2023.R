ql23.clean<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/QLcatch2023forR.csv")
ql23.clean$NUMBER.OF.FISH<-as.integer(ql23.clean$NUMBER.OF.FISH)
qllics<-c(120028,120036,120037,120067,120069,120135,120144,120180,120301,120304,
120312,120316,120325,120344,120345,120358,120367,120388,120389,120412,120417,
120418,120440,120447,120468,120499,120503,120519,120534,120559,120567,120570,
120584,120590,120625,120626,120632,120633,120634,120635,120638,120641,120644,
120647,120650,120659,120663,142394,300209,302396,303398,341717,343808,344408,
344651,344652,351949,355469,359744,359844,359845,363569,363889,363968,363986,
366835,366836) #I'm lazy and copied this in from Jarrad's excel file

qllics<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/Queens and Lunenburg licences.csv")

x<-ql23.clean
tab<-table(x$LICENCE_ID,x$RIVERNAME_CLEANED)
tab.df<-as.data.frame.matrix(tab)

#unique river names
rivs<-unique(x$RIVERNAME_CLEANED)
rivs.lics<-data.frame(rivs=rivs,
                      nlics=rep(NA,length(rivs)),
                      lics=rep(NA,length(rivs)))

for(i in 1:length(rivs))
{
  temp<-unique(x$LICENCE_ID[x$RIVERNAME_CLEANED==rivs.lics$rivs[i]])
  temp<-temp[!is.na(temp)]
  print(temp)
  rivs.lics$nlics[i]<-length(temp)
  rivs.lics$lics[i]<-paste(temp, collapse=" ")
}

#reporting rate
##adapted from yarshel23 script, generalized a bit. should make into function
qllics<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/Queens and Lunenburg licences.csv")
lics<-qllics
names(lics)<-c("LICENCE_ID","COUNTY")
lics$COUNTY[lics$COUNTY=="QUEENS (NS)   "]<-"QUEENS"
lics$COUNTY[lics$COUNTY=="LUNENBURG   "]<-"LUNENBURG"
lics$COUNTY[lics$COUNTY=="QUEENS (NS) LUNENBURG  "]<-"QUEENS AND LUNENBURG"
lics$FATE<-rep(NA,nrow(lics))

for(i in 1:nrow(lics))
{
  if(lics$LICENCE_ID[i] %in% x$LICENCE_ID)
  {
    lics$FATE[i]<-"CATCH"
  }
  else if(lics$LICENCE_ID[i] %in% didnotfish$LICENCE_ID[didnotfish$YEAR==2023 & didnotfish$NIL_REPORT_FLAG=="Y"])
  {
    lics$FATE[i]<-"DNF"
  }
}
#manually check all NAs in lics$FATE
lics$FATE[lics$LICENCE_ID==120301]<-"LOG, NO MARFIS" #add to total, single line item
lics$FATE[lics$LICENCE_ID==120344]<-"MARFIS, NO LOG" #add to total, single line item
lics$FATE[lics$LICENCE_ID==120659]<-"LOG, NO MARFIS" #add to total, single line item
lics$FATE[lics$LICENCE_ID==303398]<-"MARFIS, NO LOG" #add to total, single line item

lics$FATE[is.na(lics$FATE)]<-"DID NOT REPORT"

#double check did not reports
badlics<-lics$LICENCE_ID[lics$FATE=="DID NOT REPORT"]
check<-catch[catch$YEAR==2023 & catch$LICENCE_ID %in% badlics,] #got 303398 submitting

#now lets add missed logs into x
#!!!!CAREFUL NOT TO RUN THIS MULTIPLE TIMES AND KEEP ADDING ROWS!!!!
# lic120344<-catch[catch$LICENCE_ID==120344 & catch$YEAR==2023,]
# x[nrow(x) + 1,]<-c(120344,"QUEENS (NS)   ",2023,5,1,"MERSY","MERSEY","QUEENS COUNTY","NS",41,"GILL NET (SET OR FIXED)",NA,
#                           sum(lic120344$FV_WEIGHT),NA,"KILOGRAMS",NA,NA,round(sum(lic120344$FV_WEIGHT)/0.24,0),"SUM OF LICENCE ENTERED IN R")
# lic303398<-catch[catch$LICENCE_ID==303398 & catch$YEAR==2023,]
# x[nrow(x) + 1,]<-c(303398,"QUEENS (NS)   ",2023,5,1,"MEDWAY","MEDWAY","QUEENS COUNTY","NS",70,"DIP NET",NA,
#                    sum(lic303398$FV_WEIGHT),NA,"KILOGRAMS",NA,NA,round(sum(lic303398$FV_WEIGHT)/0.24,0),"SUM OF LICENCE ENTERED IN R")
# #NOW LOGS WITH NO marfis
# x[nrow(x) + 1,]<-c(120301,"QUEENS (NS)   ",2023,5,1,"MILTON","MERSEY","QUEENS COUNTY","NS",70,"DIP NET",NA,
#                    8635/0.24,NA,"KILOGRAMS",NA,NA,8635,"SUM OF LICENCE ENTERED IN R FROM SCAN")
# x[nrow(x) + 1,]<-c(120659,"LUNENBURG   ",2023,5,1,"LAHAVE","LAHAVE","LUNENBURG COUNTY","NS",70,"DIP NET",NA,
#                    1250,NA,"POUNDS",NA,NA,1250*2,"SUM OF LICENCE ENTERED IN R FROM SCAN")

z<-table(lics$COUNTY,lics$FATE)
#handcopied this into onenote and condensed

#catch totals
x$NUMBER.OF.FISH<-as.integer(x$NUMBER.OF.FISH)
sum(x$NUMBER.OF.FISH[x$VLOOKUP.COUNTIES=="LUNENBURG   "])
sum(x$NUMBER.OF.FISH[x$VLOOKUP.COUNTIES!="LUNENBURG   "])
