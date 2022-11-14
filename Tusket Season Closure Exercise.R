##Tukset season closure sim##
#Using escapement data paired with daily landings data rom 2021 and 2022
#I will see what effect closing various days will have on escapement,
#by assuming all fish not caught ascend the ladder the same day

####SETUP####
source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()

#...............................................................................
#Set account name, password, and server
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 


####Escapement####
##2021


##2022
setwd("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2022/Data Sheets")

accessory.datafile="TUSKET_2022_VAUGHAN_accessory_data.csv"
species.split<-split.spp(year=2022,site=2,channel,accessory.datafile)

out<-twospecies.river.escapement(year=2022,
                                 site=2,
                                 channel=channel,
                                 species.split=species.split)
##so few bluebacks will just use Alewife for simiplicity
daily.summary.A<-out[[172]] ##This still has dayofyear out of order