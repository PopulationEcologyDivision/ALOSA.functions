source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
TRage.ls<-list()
years<-2014:2026
for(i in 1:length(years))
{
  TRage.ls[[i]]<-get.age.data(year = years[i], siteID = 2, sppID = 3501, AgeStructure = T, PrimaryAger = "Y", channel = channel)
}
names(TRage.ls)<-c("Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022", "Y2023", "Y2024", "Y2025")
TRage<-do.call(rbind, TRage.ls)
unique(TRage$YEAR)

####length at age graphs####
age.key<-3:7
size.range<-c(min(TRage$FORK_LENGTH),max(TRage$FORK_LENGTH))
png("~/git/ALOSA.functions/lengthatage.png",width=5,height=11,units="in",res=300)
par(mfrow=c(5,1),mar=c(2.3,5,0,1),oma=c(0,0,1,0))
for(age in age.key)
{
  plot(jitter(TRage$YEAR[TRage$CURRENT_AGE==age]),TRage$FORK_LENGTH[TRage$CURRENT_AGE==age],
       type="p", axes=F, xlab="", ylab="",ylim=size.range)
  box()
  if(age==max(age.key))
  {
    axis(side=1,at=min(TRage$YEAR):max(TRage$YEAR),
       labels=min(TRage$YEAR):max(TRage$YEAR))
  }
  axis(side=1,at=min(TRage$YEAR):max(TRage$YEAR),labels=F)
  axis(side=2,at=seq(floor(size.range[1]),ceiling(size.range[2]),1),las=2)
  text(2025,30,paste("Age-",age,sep=""))
  rect(xleft=2024.6, ybottom=29.2, xright=2025.6, ytop=30.6)
  mtext("Fork Length (cm)",side=2,line=3)
 }
dev.off()