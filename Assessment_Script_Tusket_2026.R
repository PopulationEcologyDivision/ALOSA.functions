source("~/git/ALOSA.functions/functions/sourcery.R")
sourcery()
require(ROracle)
channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , 
                  believeNRows=FALSE) 
TR25<-get.age.data(year = 2025, siteID = 2, sppID = 3501, AgeStructure = T, PrimaryAger = "Y", channel = channel)
