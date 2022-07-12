# This function expands the in season species ID table to match the 
# Biocharacteristics format.
# Note: MARK change the data input to a direct filename

# Inputs:
#        - 
# This function is used by:
#     - no user defined functions use this function

# This functions uses:
#     - 

speciesID.expand<-function(data,rivername,year){
  data[is.na(data)]=0
  totalfish<-sum(data$all.ID,na.rm=T)
  newdata<-data.frame(river=rep(rivername,totalfish),
                     day=NA,
                     mon=NA,
                     year=rep(year,totalfish),
                     sample=10001:(10000+totalfish),
                     species=NA,
                     sex=NA,
                     fork.length=NA,
                     weight=NA,
                     scale=rep('N',totalfish),
                     notes=rep('species id only',totalfish))
  
  newdata$day<-rep(data$day[!is.na(data$all.ID)],times=data$all.ID[!is.na(data$all.ID)])
  newdata$mon<-rep(data$mon[!is.na(data$all.ID)],times=data$all.ID[!is.na(data$all.ID)])
  
  sppID=NULL
  for (i in 1:dim(data)[1]){
    temp=rep('B',times=data$BB.ID[i])
    sppID=c(sppID,temp)
    temp=rep('A',times=(data$all.ID[i]-data$BB.ID[i]))
    sppID=c(sppID,temp)
  }
  newdata$species<-sppID
  write.table(newdata,file="TUSKET_Vaughan_2022_biocharacteristics data.csv",
            na="",col.names=F, row.names=F,sep=",",
            append=TRUE)
}

