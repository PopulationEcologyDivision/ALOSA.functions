#
# Creates a summary table for each licence for a specific river. 
# Data is not QAQC'd. Specifies for each year if the licence was renewed, DNF or 
# reported catch 
#
# Inputs:
#        - alldata: data frame of all MARFIS data
#        - riverdata: river specific MARFIS data
#        - licences: df of licence data (From MARFIS)
#        - RENEWdata: df of renewal data (From MARFIS)
#        - DNFdata,: df of DNF data (From MARFIS)

#
# This function is used by:
#     - RIVER.summary

# This functions uses:
#     - no user defined functions are used by this function


reporting.summary<-function(alldata,riverdata,licences,RENEWdata,DNFdata,
                            rivername){
  DF=data.frame(LICENCE_ID=rep(licences,each=length(2008:max(alldata$YEAR))),
                YEAR=rep((2008:max(alldata$YEAR)),
                         times=length(licences)),
                RENEWED=NA,
                CATCH_RIVER=NA,
                DNF=NA,
                CATCH_OTHER=NA)
  
  for (i in licences ){
    for (j in 2008: max(alldata$YEAR)){
      if(i %in%RENEWdata$LICENCE_ID[RENEWdata$LICENCE_YEAR==j]){
        DF$RENEWED[DF$LICENCE_ID==i & DF$YEAR==j]= "Y"
      }else{DF$RENEWED[DF$LICENCE_ID==i & DF$YEAR==j]= "N"}
      
      if(i %in%DNFdata$LICENCE_ID[DNFdata$YEAR==j & 
                                  DNFdata$NIL_REPORT_FLAG=="Y"]){
        DF$DNF[DF$LICENCE_ID==i & DF$YEAR==j]= "Y"
      }else{DF$DNF[DF$LICENCE_ID==i & DF$YEAR==j]= "N"}
      
      if(i %in%riverdata$LICENCE_ID[riverdata$YEAR==j]){
        DF$CATCH_RIVER[DF$LICENCE_ID==i & DF$YEAR==j]= "Y"
      }else{DF$CATCH_RIVER[DF$LICENCE_ID==i & DF$YEAR==j]= "N"}
      
      if(i %in%alldata$LICENCE_ID[alldata$YEAR==j & 
                                  !alldata$RIVERNAME_CLEANED==rivername]){
        DF$CATCH_OTHER[DF$LICENCE_ID==i & DF$YEAR==j]= "Y"
      }else{DF$CATCH_OTHER[DF$LICENCE_ID==i & DF$YEAR==j]= "N"}
    }
  }
  return(DF) 
}
#---