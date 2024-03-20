# first clear the work enviro if you need
#rm(list = ls())
#cat("\014")

# Load functions if not already loaded.
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
#---===---===---===---===---===---===---===---===---===---===---===---===---===
#---===---===---===---===---===---===---===---===---===---===---===---===---===
require(ROracle)
  #Set account name, password, and server
  channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, "PTRAN" , 
                    believeNRows=FALSE)
  
##LOAD IN DATA:
# There are three options
#    1. Pull data directly from MARFIS using your ROracle connection. 
#    2. Run SQL queries in SQL developer and save csvs to the working directory
#    3. Load Cleaned Worked space from a previous session.
#---===---===---===---===---===---===---===---===---===---===---===---===---===
# Option 1: Pull new data directly (most up to date)
loaddata<-function(datasource){
  
  if (datasource==1){
  
  # Source script that runs the queries to load the three objects.
  source("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/ROracle pulls.R")
  
  
  #Loads in data from SQL pulls, cleans up logbook rivernames
  MARFIS_queries(local.csv=F,catch=catch,
                 didnotfish=didnotfish,
                 licencerenewals=licencerenewals)
  
  assign("catch", catch, envir = .GlobalEnv)
  assign("didnotfish",didnotfish, envir = .GlobalEnv)
  assign("licencerenewals",licencerenewals, envir = .GlobalEnv)
  
  }
 
#---===---===---===---===---===---===---===---===---===---===---===---===---===
#Option 2: Load in saved csvs
  if (datasource==2){
    MARFIS_queries(local.csv=T)
    assign("catch", catch, envir = .GlobalEnv)
    assign("didnotfish",didnotfish, envir = .GlobalEnv)
    assign("licencerenewals",licencerenewals, envir = .GlobalEnv)
    
  }
#---===---===---===---===---===---===---===---===---===---===---===---===---===
# Option 3: Load saved workspace:
#OR - load old workspace with cleaned up dataframes. You only need to run the 
# query code if you do a new SQL pull

  if (datasource ==3){
    setwd("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/")
    load("SEP24_SQL PULL.Rdata")
    assign("catch", catch, envir = .GlobalEnv)
    assign("didnotfish",didnotfish, envir = .GlobalEnv)
    assign("licencerenewals",licencerenewals, envir = .GlobalEnv)
    
 
  #If you want to save the tables from a new pull
#save(catch,didnotfish,licencerenewals,file="DATE_SQL PULL.Rdata")
  }
}
loaddata(datasource = 1)
#---===---===---===---===---===---===---===---===---===---===---===---===---===

#---===---===---===---===---===---===---===---===---===---===---===---===---===
# Cleaned River Names: Reference List
#
# "ANNIS", "ARCADIA", "ARGYLE",  "ASPY BAY", "AULAC", "AVON",  "BARRINGTON", 
# "BEAR RIVER", "BEAVER RIVER", "BELLEISLE BAY", "BELYEAS POINT", "BLACK BROOK", 
# "BRAS D'OR LAKES", "CAP-PELE", "CATALONE", "CATALONE?", "CHEBOGUE", 
# "CHEGGOGIN", "CHEZZETCOOK", "CLYDE",  # "COGMAGUN", "COOKS FALLS", 
# "CORAL BROOK", "COW BAY", "CRUSHER", "DARLING'S ISLAND", "DARLING'S LAKE", 
# "DOREY ROAD", "DORY MILLS", "DOUGLAS HARBOUR", "DOYLES COVE", "DUNN", "EAST",  
# "EAST CHEZZETCOOK", "EASTERN PASSAGE", "EEL LAKE", "FALL RIVER", 
# "FRENCH LAKE", "GAETZ BROOK", "GASPEREAU", "GLACE BAY", "GRAND LAKE", 
# "GRANVILLE", "GREENVILLE", "GREY ROCK", "HALFWAY", "HIPSON'S BROOK", 
# "HUBBARDS POINT", "INDIAN",  "INGONISH", "ISLAND POND", "KENNEBECASIS", 
# "KEYHOLE LAKE", "KIACK BROOK", "KINGS RIVER", "KINGSTON CREEK", "KINSAC",  
# "LAHAVE",  "LAPLANCHE", "LITTLE LORRAINE HARBOUR", "LITTLE RAWDON", 
# "LITTLE RIVER", "LITTLE WEST RIVER", "MAGAGUADAVIC", "MAHONE BAY", 
# "MAIN-A-DIEU", "MAQUAPIT LAKE", "MARTIN'S RIVER", "MEDWAY",  "MELONEY CREEK", 
# "MERSEY",  "METEGHAN", "MIDDLE RIVER", "MILKISH CREEK", "MILL POND", "MILO", 
# "MILTON",  "MINAS BASIN", "MINESVILLE", "MIRA", "MISSAQUASH", "MORRELL RD", 
# "MULTI", "MUSHAMUSH", "MUSQUODOBOIT", "MUSQUODOBOIT HARBOUR" , 
# "NEIL'S HARBOUR", "NORTH SYDNEY", "OROMOCTO", "OWLS HEAD", "OYSTER POND", 
# "PEMBROKE", "PETIT DE GRAT", "PETITCODIAC", "PETITE RIVIERE", "PONDVILLE", 
# "PORCUPINE BROOK", "PORT DUFFERIN", "PORT HILFORD", "PORT MAITLAND", 
# "PORT MORIEN", "PORT SAXON", "PORTERS LAKE", "PUBNICO", "RAWDON RIVER", 
# "RIVER HERBERT", "RIVER RYAN", "ROBERTSON LAKE", "SABLE RIVER", 
# "SAINT CATHERINES", "SAINT CROIX", "SAINT JOHN", "SALMON",  "SAMBRO",  
# "SANFORD", "SCOTT BROOK", "SHEET HARBOUR", "SHIP HARBOUR", "SHORT BEACH", 
# "SHRUB BEACH", "SHUBENACADIE", "SHUBENACADIE ", "SHUBENACADIE GRAND LAKE" 
# "SMELT BROOK", "SMITH FALLS", "STEWIACKE", "STINK PLANT", "SWAN LAKE", 
# "SYDNEY RIVER", "TANGIER RIVER", "TANTRAMAR", "THE KEYHOLE", 
# "THREE FATHOM HARBOUR", "TUSKET",  "UNKNOWN", "VAUGHN BROOK", 
# "WASHADEMOAK LAKE", "WEDGEPORT", "WEST ARICHAT", "YARMOUTH HARBOUR" 

#Run the summary code for the River for a summary of the annual catch 

RIVER.summary("TUSKET",
              plot=T,
              writeplot = F,
              CATCHdata=catch,
              DNFdata=didnotfish,
              RENEWdata=licencerenewals)

# sub<-present.MARFIS.data(licence = 120423,
#                          year = 2019,
#                          catch = catch)
# 
# sub<-sub[order(sub$MONTH,sub$DAY),]
# sub$weight<-sub$FV_WEIGHT/0.24

# Save the output if you want:
#write.csv(annualcatch.GAS,file="Gaspereau Catch in KGS.csv",na="",row.names=F)
#write.csv(reportcounts.GAS,file="Gaspereau reporting summary.csv",na="",row.names=F)
#write.csv(annualcatch.GAS,file="Gaspereau Catch in KGS.csv",na="",row.names=F)


####Pull filenames from logbooks scans####
directory<-"R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans/2021_FFLR_Scans_Gaspereau"
j<-check.for.missing.logbook.scans(directory = directory,
                                   year=2021,
                                   catch = catch,
                                   DNF.file = "R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/DNF_confirmed.csv",
                                   write=T)

#2022, but not all logs have been checked over yet, or even scanned. 
#just use the first output, logs with catch that weren't scanned
directory<-"R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans/2022_FFLR_Scans_Gaspereau"
check.for.missing.logbook.scans(directory = directory,
                                   year=2022,
                                   catch = catch,
                                   DNF.file = "R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/DNF_confirmed.csv")

#2023, but not all logs have been checked over yet, or even scanned. 
#just use the first output, logs with catch that weren't scanned
directory<-"R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans/2023_FFLR_Scans_Gaspereau"
check.for.missing.logbook.scans(directory = directory,
                                year=2023,
                                catch = catch,
                                DNF.file = "R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/DNF_confirmed.csv")
##get logs that were scanned for each year
#2022
directory<-"R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans/2023_FFLR_Scans_Gaspereau"
x<-list.files(directory,recursive=T)
log.scans<-gsub(".*/([0-9]{6}).*","\\1",x)
log.scans<-unique(as.integer(log.scans)) #might give warnings, thats fine

#---===---===---===---===---===---===---===---===---===---===---===---===---===
#  ```````````````````````````````````````````````````````````````````````````` 
#   The below portion of the script is designed to run specific questions we have 
#   about MARFIS. If you start a new question, make it into a function and
#   then add it to the list below so we can keep track of what questions we have 
#   summaries for and what data has been cleaned. There is far too much data to 
#   clean everything so we are limited to only cleaning up the specific
#   data we are using.
#
# ``````````````````````````````````````````````````````````````````````````````
# List of Information in this script:
#
# 1. Tusket River 2021 - Total catch by gear and daily summary by gear.
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Pull yearly summary or X years and a given river
catch$YEAR<-as.numeric(catch$YEAR)
catch.t<-catch[catch$RIVERNAME_CLEANED=="TUSKET" & catch$YEAR>2018,]
##NAs get introduced during subsetting for some reason
catch.t<-catch.t[complete.cases(catch.t[,'LICENCE_ID']),] 
catch.t<-convert.KGS(catch.t)

x1=aggregate(catch.t$KGS[is.na(catch.t$GEAR_DESCRIPTION)],
             by=list(LICENCE_ID=catch.t$LICENCE_ID[is.na(catch.t$GEAR_DESCRIPTION)]),
             FUN=sum)

x1=aggregate(catch.t$KGS, by=list(YEAR=catch.t$YEAR),FUN=sum)



TUSKET.2021=function(){

## PULL all Tusket records for 2021.
catch.TUS2021=catch[catch$RIVERNAME_CLEANED=="TUSKET" &
,    catch$YEAR==2021,]
catch.TUS2021<-convert.KGS(catch.TUS2021)
#List gears
table(catch.TUS2021$LICENCE_ID,catch.TUS2021$GEAR_DESCRIPTION,useNA = "ifany")
licenceswithnogear=unique(catch.TUS2021$LICENCE_ID[is.na(catch.TUS2021$GEAR_DESCRIPTION)])


x0=aggregate(catch.TUS2021$KGS,by=list(GEAR=catch.TUS2021$GEAR_DESCRIPTION),
             FUN=sum)
x1=aggregate(catch.TUS2021$KGS[is.na(catch.TUS2021$GEAR_DESCRIPTION)],
             by=list(LICENCE_ID=catch.TUS2021$LICENCE_ID[is.na(catch.TUS2021$GEAR_DESCRIPTION)]),
             FUN=sum)
licencelist=unique(licencereportsummary.TUS$LICENCE_ID)

licenceswithnogear=unique(x1$LICENCE_ID)

}


myfunction<-function(ilovecats){
  temp<-ilovecats*2
  temp1<-rep(temp,5)
  return(temp1)
}



# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
