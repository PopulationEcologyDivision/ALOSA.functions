##check if logs have been submitted this year
#outputs a list of licences that have submitted catch or a DNF in the selected year
#outputs a list of outstanding licences, using the "Gaspereau Licences Area Project.xlsx file from RM, and the last time they reported


#uses catch and didnotfish dataframes from MARFIS_all in one.R

did.they.submit<-function(catch,didnotfish,year)
{
  library(readxl)
  library(openxlsx)
  #read in Gaspereau Licences Area Project.xlsx
  lics<-read_excel("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/Gaspereau Licences Area Project.xlsx")
  #this has multiple rows for one licence, based on gear type
  #sub out the columns we want and remove duplicates, so each row is one licence
  lics<-lics[,c(2,4,5,6,8,9,11,12,13,14,15,16)]
  lics<-lics[!duplicated(lics$`Licence Id`),]
  
  #subset to the desired year
  x<-catch[catch$YEAR==year,]
  y<-didnotfish[didnotfish$YEAR==year,]
  
  rep.catch<-unique(x$LICENCE_ID) #licences that reported catch
  rep.DNF<-unique(y$LICENCE_ID) #licences that reported a DNF
  
  all.rep<-c(rep.catch,rep.DNF) #combine both types of reporting
  
  #get a subset of all those licences that did report in the year
  lics.rep<-lics[lics$`Licence Id`%in%all.rep,]
  lics.rep$`Report in 2026`<-ifelse(lics.rep$`Licence Id`%in%rep.catch,"Catch",
                              ifelse(lics.rep$`Licence Id`%in%rep.DNF,"DNF",NA))
  
  #subset out the complement of the non reporting licences
  lics.out<-lics[lics$`Licence Id`%!in%all.rep,] #existing licences not reporting in specified year
  
  #some of the licences in the lics object have never reported since 2008. this section removes them
  #did these licences ever report?
  #never report catch?
  lics.never.catch<-lics[lics$`Licence Id`%!in%catch$LICENCE_ID,]
  #never report DNF?
  lics.never.dnf<-lics[lics$`Licence Id`%!in%didnotfish$LICENCE_ID[didnotfish$NIL_REPORT_FLAG=="Y"],]
  #both?
  lics.never.anything<-lics.never.catch[lics.never.catch$`Licence Id`%!in%didnotfish$LICENCE_ID[didnotfish$NIL_REPORT_FLAG=="Y"],]
  #remove those from df
  lics.out<-lics.out[lics.out$`Licence Id`%!in%lics.never.anything$`Licence Id`,]
  
  #of those that remain, when was the last time they reported catch and DNF
  for(i in 1:nrow(lics.out))
  {
    lics.out$lastcatch[i]<-max(as.integer(catch$YEAR[catch$LICENCE_ID==lics.out$`Licence Id`[i]]))
    lics.out$lastdnf[i]<-max(as.integer(didnotfish$YEAR[didnotfish$NIL_REPORT_FLAG=="Y" & didnotfish$LICENCE_ID==lics.out$`Licence Id`[i]]))
    lics.out$lastreport[i]<-max(c(lics.out$lastcatch[i],lics.out$lastdnf[i]))
  }
  #change -Infs to NA's when  licence never reported catch or DNF
  lics.out[lics.out==-Inf]<-NA
  
  
  #report card
  years<-2008:2026
  rep.card<-data.frame(LICENCE_ID=lics$`Licence Id`,Y2008=NA,
                       Y2009=NA,Y2010=NA,Y2011=NA,Y2012=NA,Y2013=NA,Y2014=NA,
                       Y2015=NA,Y2016=NA,Y2017=NA,Y2018=NA,Y2019=NA,Y2020=NA,
                       Y2021=NA,Y2022=NA,Y2023=NA,Y2024=NA,Y2025=NA,Y2026=NA)
  for(i in 1:nrow(lics))
  {
    for(yr in 1:length(years))
    {
      #is there reported catch?
      catch.temp<-nrow(catch[catch$LICENCE_ID==rep.card$LICENCE_ID[i] & catch$YEAR==years[yr],])
      #is there reported DNF?
      dnf.temp<-didnotfish$NIL_REPORT_FLAG[didnotfish$LICENCE_ID==rep.card$LICENCE_ID[i] & didnotfish$YEAR==years[yr]]
      #assign a value of catch, DNF ,or no report depending on above query
      if(catch.temp>0){z<-"Catch"}else if("Y"%in%dnf.temp){z<-"DNF"}else{z<-"No Report"}
      rep.card[i,yr+1]<-z
    }
  }
  
  #output the three objects into an excel sheet
  namesforlist=c(paste0("Licences Reporting in ",year),
                paste0("Licences Not Reporting in ",year),
                "Report Card")#do names here outside of maknig the list
  sheet.names<-list(lics.rep,lics.out,rep.card)
  names(sheet.names)<-namesforlist
  #combine output into a spreadsheet with pages
  write.xlsx(sheet.names,file=paste0("Gaspereau Licence Reporting Summary ",year,".xlsx"))
  
}