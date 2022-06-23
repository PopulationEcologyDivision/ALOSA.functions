#
#Description: Creates a summary table per year of the number of licences for a specific river
# with catch, DNF reports, missing reports or catch from another river.
# Data is not QAQC'd. 
#
# Inputs: river.reports: output from the reporting.summary function

#
# This function is used by:
#       - RIVER.summary

# This functions uses:
#           - output df from the reporting.summary function


reporting.count<-function(river.reports){   
  catch.count=aggregate(river.reports$LICENCE_ID[river.reports$CATCH_RIVER=="Y"]
                        ,by=list(YEAR=river.reports$YEAR[river.reports$CATCH_RIVER=="Y"]),
                        FUN=user.count)
  names(catch.count)[2]<-"N_CATCH"
  
  DNF.count=aggregate(river.reports$LICENCE_ID[river.reports$DNF=="Y"]
                      ,by=list(YEAR=river.reports$YEAR[river.reports$DNF=="Y"]),
                      FUN=user.count)
  names(DNF.count)[2]<-"N_DNF"
  
  
  renewed.count=aggregate(river.reports$LICENCE_ID[river.reports$RENEWED=="Y"]
                          ,by=list(YEAR=river.reports$YEAR[river.reports$RENEWED=="Y"]),
                          FUN=user.count)
  names(renewed.count)[2]<-"N_RENEWED"
  
  if(length(river.reports$CATCH_OTHER[river.reports$CATCH_OTHER=="Y"])>0){
    othercatch.count=aggregate(river.reports$LICENCE_ID[river.reports$CATCH_OTHER=="Y"&
                                                          river.reports$CATCH_RIVER=="N"]
                               ,by=list(YEAR=river.reports$YEAR[river.reports$CATCH_OTHER=="Y"&
                                                                  river.reports$CATCH_RIVER=="N"]),
                               FUN=user.count)
    names(othercatch.count)[2]<-"N_OTHERONLY"  
  }else{
    othercatch.count=data.frame(YEAR=2008:max(river.reports$YEAR),N_OTHERONLY=0)
  }
  t1=merge(catch.count,DNF.count,all=T)
  t2=merge(t1,renewed.count,all=T)
  t3=merge(t2,othercatch.count,all=T)
  t3=t3[order(t3$YEAR),]
  t3[is.na(t3)]<-0
  t3$REPORTING_RATE=format(round(rowSums(t3[,c(2,3,5)])/t3$N_RENEWED, 2), nsmall = 2)
  return(t3)
}