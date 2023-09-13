# A function to output missing logs or unentered data from MARFIS in a year,
# based on file names containing licence ID in a specified directory, and the
# catch data frame in MARFIS
#
#TODO!!
#read in csv of confirmed DNFs to filter out scanned logs with out catches,
#since those don't have line entries in MARFIS
#add DNF.file arguement to function
#
# Requires input of:
# Catch dataframe from MARFIS_all in one.R
#
# This function is used by:
#     - 
#
# This function uses:
#     - 
#
# Example function call
# check.for.missing.logbook.scans(directory="R:/Shared/BillardM/2019 FFLR scans Gaspereau/",
#                                 year=2019,
#                                 catch=catch,
#                                 DNF.file="R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/DNF_confirmed.csv",
#                                 write=F)



check.for.missing.logbook.scans<-function(directory,year,catch,DNF.file,write=F)
{
  x<-list.files(directory,recursive=T)
  log.scans<-gsub(".*/([0-9]{6}).*","\\1",x)
  log.scans<-as.integer(log.scans) #might give warnings, thats fine
  MARFIS.licences<-as.integer(unique(catch$LICENCE_ID[catch$YEAR==year]))
  
  ##I want the licence numbers in MARFIS NOT in scans
  #these are data entries that we do not have a physical scan of
  print(paste("Logs with catch in MARFIS that weren't scanned for",year))
  print(setdiff(MARFIS.licences,log.scans))
  if(write==T)
  {write.csv(setdiff(MARFIS.licences,log.scans),file=paste("Unscanned logs",year),row.names=F)}
  
  ##doing the opposite, log scans that are not in MARFIS, should give us all DNF
  ##logs, and also logs that were received but not entered (unlikely)
  ##checking this vector against the confirmed DNF file should give 0
  dnf<-read.csv(DNF.file)
  dnf<-dnf[dnf$YEAR==year,]
  dnf.sub<-dnf$LICENCE_ID[dnf$NIL_REPORT=="Y" & dnf$NIL_CONFIRMED=="Y"]
  scan.no.catch<-setdiff(log.scans,MARFIS.licences)
  scan.no.catch.no.dnf<-setdiff(scan.no.catch,dnf.sub)
  
  print(paste("Scanned logs with no catch in MARFIS exclduing DNFs",year))
  print(scan.no.catch.no.dnf)
  if(write==T)
  {write.csv(scan.no.catch.no.dnf,file=paste("Unentered logs",year),row.names=F)}
  
  ##check for unconfirmed DNF logs that are scanned
  dnf.sub2<-dnf$LICENCE_ID[dnf$NIL_REPORT=="Y" & dnf$NIL_CONFIRMED=="N"]
  print(paste("Scanned logs with an unconfirmed DNF",year))
  print(dnf.sub2[which(dnf.sub2 %in% log.scans)])
  
  ##check for unconfirmed DNF logs that are NOT scanned
  print(paste("Unscanned logs with an unconfirmed DNF",year))
  print(setdiff(dnf.sub2,log.scans))
}
# 
# missing.scans.2020<-check.for.missing.logbook.scans(directory="R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans/2020_FFLR_Scans_Gaspereau/",
#                         year=2020,
#                         catch=catch)
# 
# missing.scans.2019<-check.for.missing.logbook.scans(directory="R:/Shared/BillardM/2019 FFLR scans Gaspereau/",
#                                             year=2019,
#                                             catch=catch)
