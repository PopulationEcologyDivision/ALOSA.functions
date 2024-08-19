#######
#requires MARFIS_all in one.R AND log.check.R

missing.scans <- function(years=NA){
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  logs2scan_catch<-list()
  logs2scan_dnf<-list()
  
  for(i in 1:length(years))
  {
    catch_compare <- catch[catch$YEAR %in% years[i],]
    dnf_compare <- didnotfish[didnotfish$YEAR %in% years[i] & didnotfish$NIL_REPORT_FLAG=="Y",]
    
    df <- log.check(years = years[i])
    
    need_catch <- catch_compare[catch_compare$LICENCE_ID %!in% df$LICENCE_ID,]
    need_dnf <- dnf_compare[dnf_compare$LICENCE_ID %!in% df$LICENCE_ID,]
    
    temp <- need_catch[need_catch$YEAR == years[i],]
    logs2scan_catch[[i]] <- data.frame(LICENCE_ID = unique(temp$LICENCE_ID), YEAR = rep(years[i], length(unique(temp$LICENCE_ID))))
    
    temp <- need_dnf[need_dnf$YEAR == years[i],]
    logs2scan_dnf[[i]] <- data.frame(LICENCE_ID = unique(temp$LICENCE_ID), YEAR = rep(years[i], length(unique(temp$LICENCE_ID))))
  }
  
  for_scanning_CATCH<-do.call("rbind",logs2scan_catch)
  for_scanning_DNF<-do.call("rbind",logs2scan_dnf)
  
  for_scanning_CATCH$log_type <- "catch"
  for_scanning_DNF$log_type <- "dnf"
  
  for_scanning <- rbind(for_scanning_CATCH, for_scanning_DNF)
  
  return(for_scanning)
}