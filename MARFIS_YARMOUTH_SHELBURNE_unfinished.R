#for getting number of licences for 2023 for yarmouth and shelburne counties
##note that it relies on a modified version of the river.summary function so that the 
#licencereportsummary gets return rather than assigned. it needs to return so it can be assigned in a list
#since objects will get overwritten if their first the letters match, ie EEl lake and EEl brook, Greenville and Great whatever

catch.ysall<-catch[catch$COUNTY=="YARMOUTH COUNTY" | catch$COUNTY=="SHELBURNE COUNTY",]

rivs<-unique(catch.ysall$RIVERNAME_CLEANED)

rivs<-rivs[-c(3,13,19)] #get rid of Na, sable river (only one in 2011), and unknown
rivs<-sort(rivs)

licrenew<-list()
for(i in 1:length(rivs))
{
  print(i)
  ##THIS ONLY WORKS WHEN THE RIVER.summary FUNCTION IS MODIFIED TO RETURN licencereportsummary.XXX from river reports, rather than assign it
  licrenew[[i]]<-
    RIVER.summary(rivs[i],
                  plot=T,
                  writeplot = F,
                  CATCHdata=catch,
                  DNFdata=didnotfish,
                  RENEWdata=licencerenewals)
}

for(i in 1:length(rivs))
{
  licrenew[[i]]$RIVNAME<-sort(rivs)[i]
}

all.lic<-do.call("rbind",licrenew)
all.lic23<-all.lic[all.lic$YEAR==2023,] # get only 2023
#need to remove some NNNN rows, since a lic may have been renewed previously but stopped, and is still in this DF
all.lic23<-all.lic23[all.lic23$RENEWED=="Y",] #remove licences not renewed for 2023 (2 submitted DNFs, noted in onenote)
all.lic23<-all.lic23[all.lic23$LICENCE_ID!=120317,] #this got in here from NB becuase of "indian lake"


#how many licences renewed for 2023?
length(unique(all.lic23$LICENCE_ID))
lics<-sort(unique(all.lic23$LICENCE_ID))

#how many DNF?
dnf.df<-all.lic23[all.lic23$DNF=="Y",]
length(unique(dnf.df$LICENCE_ID))
dnfs<-sort(unique(dnf.df$LICENCE_ID))

catch.df<-all.lic23[all.lic23$CATCH_RIVER=="Y",]
length(unique(catch.df$LICENCE_ID))
catchs<-sort(unique(catch.df$LICENCE_ID))

naughty.df<-all.lic23[all.lic23$CATCH_RIVER=="N" & all.lic23$DNF=="N" & all.lic23$CATCH_OTHER=="N",]
length(unique(naughty.df$LICENCE_ID))
naughties<-sort(unique(naughty.df$LICENCE_ID))

#use this data.frame to summarize catch from various areas
catch.df<-all.lic23[all.lic23$CATCH_RIVER=="Y",]
catch.ysall<-catch.ysall[complete.cases(catch.ysall[,'LICENCE_ID']),]
catch.ysall<-convert.KGS(catch.ysall)
sum(catch.ysall$KGS[catch.ysall$LICENCE_ID %in% catch.df$LICENCE_ID & catch.ysall$YEAR==2023])
sum(catch.ysall$KGS[catch.ysall$YEAR==2023])