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
catch<-c(361754,1265893,1863153,1747021,1240938)
tot<-esc+catch


#plot numbers at age
par(mfrow=c(1,7))
age.matrix.ls<-list()
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
  
  age.matrix<-age.prop.matrix*tot[yr]
  age.matrix.ls[[yr]]<-age.matrix
  #numbers at age by year
  barplot(age.matrix[1:4,]/1000,
          ylim=c(0,2500),
          xlab="Age",ylab="Thousands of Fish",cex.lab=1.5,
          col=c("#E69F00","#56B4E9","#009E73","#0072B2"))
  mtext(years[yr],3)
  if(yr==1 | yr==3){plot.new()} #puts gaps in for 2020 and 2023 when no age data available
  if(yr==5)
  {
    legend("topright",legend=c("First","Second","Third","Fourth"),
           fill=c("#E69F00","#56B4E9","#009E73","#0072B2"),bty='n',
           title="Number of\nSpawnings",title.adj=0,xpd=TRUE,inset = c(0.05,0.1))
  }
}

temp.df<-data.frame(Age=rep(3:7,each=5),Spawns=seq(1,5,1))
temp.df<-temp.df[temp.df$Spawns<5,]
# temp.df$diff<-temp.df$Age-temp.df$Spawns
# temp.df<-temp.df[temp.df$diff>1,]
# temp.df$diff<-NULL
temp.df$Year<-NA
temp.df$Cohort<-NA
temp.df$Total<-NA

age.df.ls<-list()
for(yr in 1:length(years))
{
  temp.df$Year<-years[yr]
  temp.df$Cohort<-temp.df$Year-temp.df$Age
  temp.df$Total<-c(age.matrix.ls[[yr]])
  age.df.ls[[yr]]<-temp.df
}
age.df<-do.call(rbind,age.df.ls)

cohorts<-sort(unique(age.df$Cohort))
par(mfrow=c(2,length(cohorts)/2))
for(coh in 1:length(cohorts))
{
  if(coh==1){next()}#skip 2012 only 1 year old no visual info
  plot.df<-age.df[age.df$Cohort==cohorts[coh],]
  #convert to a matrix for carplot
  plot.matrix.trunc<-tapply(plot.df$Total,plot.df[1:2],FUN=mean)
  plot.matrix.trunc<-t(plot.matrix.trunc) #transpose
  plot.matrix<-matrix(rep(0,20),nrow=4, ncol=5, dimnames=list(c(1:4),c(3:7)))
  ind<-cbind(rownames(plot.matrix.trunc)[row(plot.matrix.trunc)],colnames(plot.matrix.trunc)[col(plot.matrix.trunc)])
  plot.matrix[ind]<-plot.matrix[ind]+plot.matrix.trunc[ind]
  barplot(plot.matrix[1:4,]/1000,
          ylim=c(0,2500),
          xlab="Age",ylab="Thousands of Fish",cex.lab=1.5,
          col=c("#E69F00","#56B4E9","#009E73","#0072B2"))
  mtext(paste("Cohort Year ",cohorts[coh]),3)
  if(cohorts[coh]==2015){mtext("Tusket Numbers-at-age by Cohort Year",side=3,line=3)}
  if(cohorts[coh]==2022)
    {
      legend("topright",legend=c("First","Second","Third","Fourth"),
             fill=c("#E69F00","#56B4E9","#009E73","#0072B2"),bty='n',
             title="Number of\nSpawnings",title.adj=0,xpd=TRUE,inset = c(0.05,0.1))
    }
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