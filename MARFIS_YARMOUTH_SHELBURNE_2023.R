# yar<-catch[catch$COUNTY=="YARMOUTH COUNTY",]
# shel<-catch[catch$COUNTY=="SHELBURNE COUNTY",]
# 
# lics<-unique(c(unique(yar$LICENCE_ID),unique(shel$LICENCE_ID)))
# 
# 
# temp<-catch[catch$LICENCE_ID==120629,]
# 
# 
# yarshel23<-catch[catch$YEAR==2023,]
# yarshel23<-yarshel23[,c(1,3,10,11,2,15,16,17,5,6,4,8,9)]
# write.csv(yarshel23,"gasp catch 2023.csv",row.names=F)

##reading in manually cleaned landings from excel
yarshel23.clean<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/Locations/Tusket River/Tusket 2023/YarShel Landings Cleaned 2023.csv")
x<-yarshel23.clean
tab<-table(x$LICENCE_ID,x$RIVERNAME_CLEANED)
tab.df<-as.data.frame.matrix(tab)

#unique river names
rivs<-unique(x$RIVERNAME_CLEANED)
rivs.lics<-data.frame(rivs=rivs,
                      nlics=rep(NA,length(rivs)),
                      lics=rep(NA,length(rivs)))
for(i in 1:length(rivs))
{
  temp<-unique(x$LICENCE_ID[x$RIVERNAME_CLEANED==rivs.lics$rivs[i]])
  temp<-temp[!is.na(temp)]
  print(temp)
  rivs.lics$nlics[i]<-length(temp)
  rivs.lics$lics[i]<-paste(temp, collapse=" ")
}

##summary for Tusket, Annis, Argyle, Eel Lake/Kiack Brook, shelburne county, yarmouth county others
x$rivkey<-rep(NA,nrow(x))
for(i in 1:nrow(x))
{
  if(x$RIVERNAME_CLEANED[i]=="TUSKET"){x$rivkey[i]<-"TUSKET"}
  else if(x$RIVERNAME_CLEANED[i]=="ANNIS" | x$RIVERNAME_CLEANED[i]=="GREENVILLE"){x$rivkey[i]<-"ANNIS"}
  else if(x$RIVERNAME_CLEANED[i]=="ARGYLE"){x$rivkey[i]<-"ARGYLE"}
  else if(x$RIVERNAME_CLEANED[i]=="KIACK BROOK" | x$RIVERNAME_CLEANED[i]=="EEL LAKE"){x$rivkey[i]<-"KBEL"}
  else if(x$COUNTY[i]=="SHELBURNE COUNTY"){x$rivkey[i]<-"SHELBURNE"}
  else{x$rivkey[i]<-"YARMOUTH OTHER"}
}

##now we can redo the rivs.lics table with our custom river categories
rivs<-unique(x$rivkey)
rivs.lics.sum<-data.frame(rivs=rivs,
                          nlics=rep(NA,length(rivs)),
                          lics=rep(NA,length(rivs)),
                          catch=rep(NA,length(rivs)))
for(i in 1:length(rivs))
{
  temp<-unique(x$LICENCE_ID[x$rivkey==rivs.lics.sum$rivs[i]])
  temp<-temp[!is.na(temp)]
  rivs.lics.sum$nlics[i]<-length(temp)
  rivs.lics.sum$lics[i]<-paste(temp, collapse=" ")
  rivs.lics.sum$catch[i]<-sum(x$NUMBER.OF.FISH[x$rivkey==rivs.lics.sum$rivs[i]])
}

#write out the riv/lics/sum table for presentation
write.csv(rivs.lics.sum,"YarShel 2023 landings.csv",row.names=F)

####reporting rates####
yarshel.lics<-read.csv("yarshel licences.csv")
yarshel.lics$COUNTIES[yarshel.lics$COUNTIES=="YARMOUTH   "]<-"YARMOUTH"
yarshel.lics$COUNTIES[yarshel.lics$COUNTIES=="YARMOUTH LUNENBURG  "]<-"YARMOUTH"
yarshel.lics$COUNTIES[yarshel.lics$COUNTIES=="SHELBURNE   "]<-"SHELBURNE"
yarshel.lics$FATE<-rep(NA,nrow(yarshel.lics))

for(i in 1:nrow(yarshel.lics))
{
  if(yarshel.lics$LICENCE.NUMBER[i] %in% x$LICENCE_ID)
  {
    yarshel.lics$FATE[i]<-"CATCH"
  }
  else if(yarshel.lics$LICENCE.NUMBER[i] %in% didnotfish$LICENCE_ID[didnotfish$YEAR==2023 & didnotfish$NIL_REPORT_FLAG=="Y"])
  {
    yarshel.lics$FATE[i]<-"DNF"
  }
}
##went through the remaining NA's by hand, and got this
yarshel.lics$FATE[yarshel.lics$LICENCE.NUMBER==120310]<-"LOG, NO MARFIS"
yarshel.lics$FATE[yarshel.lics$LICENCE.NUMBER==120543]<-"LOG, NO MARFIS"
yarshel.lics$FATE[yarshel.lics$LICENCE.NUMBER==120613]<-"LOG, NO MARFIS"
yarshel.lics$FATE[yarshel.lics$LICENCE.NUMBER==120747]<-"LOG, NO MARFIS"

yarshel.lics$FATE[is.na(yarshel.lics$FATE)]<-"DID NOT REPORT"




z<-table(yarshel.lics$COUNTIES,yarshel.lics$FATE)
#reporting rate shelburne
shel.reports<-z[1,1]+z[1,3]+z[1,4]
print(paste("Shelburne reporting rate",shel.reports/sum(z[1,])))

#reporting rate shelburne
yar.reports<-z[2,1]+z[2,3]+z[2,4]
print(paste("Yarmouth reporting rate",yar.reports/sum(z[2,])))

##for fun
##when was the last year a licence reported a catch or DNF?
for(i in 1:nrow(yarshel.lics))
{
  if(yarshel.lics$FATE[i]!="DID NOT REPORT"){next()}
  print(paste("Licence",yarshel.lics$LICENCE.NUMBER[i]))#print licence number in question
  catch.years<-unique(catch$YEAR[catch$LICENCE_ID==yarshel.lics$LICENCE.NUMBER[i]])#get years the lcience reported catch
  catch.years<-as.integer(catch.years) #clean up
  catch.years<-catch.years[order(catch.years)] #sort em
  if(length(catch.years)>0){
    print("Catch Years")
    print(catch.years)
    }
  dnf.years<-didnotfish$YEAR[didnotfish$NIL_REPORT_FLAG=="Y" & didnotfish$LICENCE_ID==yarshel.lics$LICENCE.NUMBER[i]]
  dnf.years<-as.integer(dnf.years) #clean up
  dnf.years<-dnf.years[order(dnf.years)] #sort em
  if(length(dnf.years)>0){
    print("DNF Years")
    print(dnf.years)
  }
}
