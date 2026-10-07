source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
TR25<-get.age.data(year = 2025, siteID = 2, sppID = 3501, AgeStructure = T, PrimaryAger = "Y", channel = channel)
TR16<-get.age.data(year = 2016, siteID = 2, sppID = 3501, AgeStructure = T, PrimaryAger = "Y", channel = channel)
TRage.ls<-list()
years<-2014:2026
for(i in 1:length(years))
{
  TRage.ls[[i]]<-get.age.data(year = years[i], siteID = 2, sppID = 3501, AgeStructure = T, PrimaryAger = "Y", channel = channel)
}
names(TRage.ls)<-c("Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022", "Y2023", "Y2024", "Y2025")
TRage<-do.call(rbind, TRage.ls)
