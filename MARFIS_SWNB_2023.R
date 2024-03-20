
#get all catch in NB in 2023
# nb23<-catch[catch$PROVINCE=="NB" & catch$YEAR==2023,]
# 
# lics<-unique(c(unique(nb23$LICENCE_ID)))
# 
# 
# nb23<-nb23[,c(1,3,10,11,2,15,16,17,5,6,4,8,9)]
# write.csv(nb23,"NB gasp catch 2023.csv",row.names=F)

#read in catch objects
load("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/13MAR24_SQL PULL.Rdata")

nb23.clean<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/2023 catch/NB gasp catch 2023.csv")
nb23.clean$NUMBER.OF.FISH<-as.integer(nb23.clean$NUMBER.OF.FISH)
nblics<-read.csv("R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/2023 catch/SWNB Licenses.csv")

##clean up nblics based on convo with Jarrad
nblics<-nblics[nblics$Licence.Type.Description..English.=="NON-VESSEL BASED LIMITED",]
nblics<-nblics[nblics$Licence.Id<300000,]
'%!in%' <- function(x,y)!('%in%'(x,y))
badlicnames<-c("READING, TAMMY","Fisheries & Oceans, Scotia-Fundy")
nblics<-nblics[nblics$Name..Last..First.%!in%badlicnames,]
nblics<-nblics[,c(1,4)]
names(nblics)<-c("LICENCE_ID","COUNTY")
lics<-nblics
lics$FATE<-rep(NA,nrow(lics))

for(i in 1:nrow(lics))
{
  if(lics$LICENCE_ID[i] %in% x$LICENCE_ID)
  {
    lics$FATE[i]<-"CATCH"
  }
  else if(lics$LICENCE_ID[i] %in% didnotfish$LICENCE_ID[didnotfish$YEAR==2023 & didnotfish$NIL_REPORT_FLAG=="Y"])
  {
    lics$FATE[i]<-"DNF"
  }
}

lics$FATE[lics$LICENCE_ID==120077]<-"LOG, NO MARFIS" #added
lics$FATE[lics$LICENCE_ID==120245]<-"LOG, NO MARFIS" #added

lics$FATE[lics$LICENCE_ID==120205]<-"MARFIS, NO LOG" #added
lics$FATE[lics$LICENCE_ID==120271]<-"MARFIS, NO LOG" #added
lics$FATE[lics$LICENCE_ID==120288]<-"MARFIS, NO LOG" #added
lics$FATE[lics$LICENCE_ID==120359]<-"MARFIS, NO LOG" #added

lics$FATE[is.na(lics$FATE)]<-"DID NOT REPORT"

z<-table(lics$FATE)

# #catch
# x<-nb23.clean
# tab<-table(x$LICENCE_ID,x$RIVERNAME_CLEANED)
# tab.df<-as.data.frame.matrix(tab)
