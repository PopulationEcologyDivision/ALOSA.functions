#######
#requires MARFIS_all in one.R AND log.check.R

missing.scans <- function(years=NA){

'%!in%' <- function(x,y)!('%in%'(x,y))
df <- log.check(years = years)

catch_compare <- catch[catch$YEAR %in% years,]
dnf_compare <- didnotfish[didnotfish$YEAR %in% years,]
lic_ren_compare <- licencerenewals[licencerenewals$LICENCE_YEAR %in% years,]
lic_ren_compare$YEAR <- lic_ren_compare$LICENCE_YEAR

need_catch <- catch_compare[catch_compare$LICENCE_ID %!in% df$LICENCE_ID,]
need_dnf <- dnf_compare[dnf_compare$LICENCE_ID %!in% df$LICENCE_ID,]
need_renewal <- lic_ren_compare[lic_ren_compare$LICENCE_ID %!in% df$LICENCE_ID,]

for_scanning_CATCH <- data.frame()
for(i in 1:length(years)){
  logs2scan <- need_catch[need_catch$YEAR == years[i],]
  logs2scan <- data.frame(LICENCE_ID = unique(logs2scan$LICENCE_ID), YEAR = rep(years[i], length(unique(logs2scan$LICENCE_ID))))
  for_scanning_CATCH <- rbind(for_scanning_CATCH, logs2scan)
}
for_scanning_DNF <- data.frame()
for(i in 1:length(years)){
  logs2scan <- need_dnf[need_dnf$YEAR == years[i],]
  logs2scan <- data.frame(LICENCE_ID = unique(logs2scan$LICENCE_ID), YEAR = rep(years[i], length(unique(logs2scan$LICENCE_ID))))
  for_scanning_DNF <- rbind(for_scanning_DNF, logs2scan)
}

for_scanning_CATCH$log_type <- "catch"
for_scanning_DNF$log_type <- "dnf"
for_scanning <- rbind(for_scanning_CATCH, for_scanning_DNF)

return(for_scanning)
}