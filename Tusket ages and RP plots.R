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

years<-c(2019,2021,2022,2024,2025)
age.data.ls<-list()
for(yr in 1:length(years))
{
  age.data<-get.age.data(year=years[yr],siteID = 2,sppID=3501, AgeStructure = T, PrimaryAger="Y", channel)
  
  age.data$PREVIOUS_SPAWNS<-age.data$CURRENT_AGE-age.data$AGE_AT_FIRST_SPAWN

  age.data.ls[[yr]]<-age.data
}

names(age.data.ls)<-c("age2019","age2021","age2022","age2024","age2025")
esc<-c(397709,1956804,1670471,2262181,1372982)

#plot
par(mfrow=c(1,5))
for(yr in 1:length(years))
{
  agedata<-age.data.ls[[yr]]
  age.prop.matrix=matrix(rep(0,20),nrow=4, ncol=5,
                         dimnames=list(c("First","Second","Third","Fourth"),
                                       c("Age3","Age4","Age5","Age6","Age7")))
  for (i in 3:7){
    CurrentAge=agedata[agedata$CURRENT_AGE==i,c("CURRENT_AGE","AGE_AT_FIRST_SPAWN")]
    for (j in i:3){
      spawngroup=CurrentAge[CurrentAge$AGE_AT_FIRST_SPAWN==j,]
      proportion=dim(spawngroup)[1]/(dim(agedata)[1])
      if(proportion>0)age.prop.matrix[(i-(j-1)),(i-2)]=proportion
    }
  }
  
  age.matrix<-age.prop.matrix*esc[yr]
  
  barplot(age.matrix[1:4,],
          # ylim=c(0,max(sum(age.prop.matrix[,2],na.rm=T)*1.2,sum(age.prop.matrix[,3],na.rm=T)*1.2)),
          # ylim=c(0,0.8),
          ylim=c(0,1500000),
          xlab="",ylab="Thousands of Fish",cex.lab=1.5,
          col=c("#E69F00","#56B4E9","#009E73","#0072B2")) 
}

####habitat based reference point plot####
#92swkm habitat from 2016 assessment
#Mark estimated 18.86 sqkm above great barren, 1.5 above mink dam not included in that total
sqkm<-seq(0:(92+18.86+1.5))
#to get from sqkm to numbers of fish, do 51mt/km2 * 94.7% (SSB0) * 14.85% (SSBmsy) *1000kg/mt * 0.213kg/fish
#10% of SSB0 for LRP
USR.conv<-51*0.947*0.1485*1000/0.213
USR<-USR.conv*sqkm/1000000 #divide by 1 million to get millions of fish
LRP.conv<-51*0.947*0.1*1000/0.213
LRP<-LRP.conv*sqkm/1000000 #divide by 1 million to get millions of fish

png("HabitatBasedRPs.png",width=1200,height=1200,units="px",pointsize=12)
par(cex=2)
plot(sqkm,USR,type="l",xlab="Square Kilometres of Habitat",ylab="Millions of Fish",yaxt="n")
axis(2,las=2)
lines(sqkm,LRP)
abline(v=92)
text(92-2,1,label="Current Accessible Habitat",srt=90)
abline(v=111,lty=3)#adding Great Barren
text(111-2,1,label="Current + Great Barren",srt=90)
abline(v=113,lty=3) #adding great barren and mink lake
text(113+1,1,label="Current + Great Barren and Mink Lake",srt=90)
abline(v=92*0.7,lty=2) #removing 30% correspodning to uppstream of Carleton
text(92*0.7-2,1,label="Current - Carleton",srt=90)
text(40,USR[43],label="USR",srt=45)
text(45,LRP[49],label="LRP",srt=45*0.1/0.1485)

dev.off()


sqkm<-seq(0:(92+18.86+1.5))
#to get from sqkm to numbers of fish, do 51mt/km2 * 94.7% (SSB0) * 14.85% (SSBmsy) *1000kg/mt * 0.213kg/fish
#10% of SSB0 for LRP
SSB0.conv<-51*0.947*1000/0.213
SSB0<-SSB0.conv*sqkm/1000000 #divide by 1 million to get millions of fish
USR.conv<-51*0.947*0.1485*1000/0.213
USR<-USR.conv*sqkm/1000000 #divide by 1 million to get millions of fish
LRP.conv<-51*0.947*0.1*1000/0.213
LRP<-LRP.conv*sqkm/1000000 #divide by 1 million to get millions of fish

png("HabitatBasedRPs2.png",width=1200,height=1200,units="px",pointsize=12)
par(cex=2)
plot(sqkm,SSB0,type="l",xlab="Square Kilometres of Habitat",ylab="Millions of Fish",yaxt="n")
axis(2,las=2)
lines(sqkm,USR)
lines(sqkm,LRP)
abline(v=92)
text(92-2,8,label="Current Accessible Habitat",srt=90)
abline(v=111,lty=3)#adding Great Barren
text(111-2,8,label="Current + Great Barren",srt=90)
abline(v=113,lty=3) #adding great barren and mink lake
text(113+1,8,label="Current + Great Barren and Mink Lake",srt=90)
abline(v=92*0.7,lty=2) #removing 30% correspodning to uppstream of Carleton
text(92*0.7-2,8,label="Current - Carleton",srt=90)
text(40,SSB0[43],label="SSB0",srt=45)
text(45,USR[45]+0.5,label="USR",srt=45*0.1485)
text(80,LRP[80]-0.5,label="LRP",srt=45*0.1)

dev.off()