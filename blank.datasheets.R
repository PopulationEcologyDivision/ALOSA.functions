#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:


blank.datasheets<-function(seed,startday,rivername,year,recordtime=T,
                           speciesID=F,strata,samplesperstrata){
  random.sample.2way.5min(seed,
                          startday,
                          filename=paste(rivername,year,"count data.csv",sep="_"),
                          year,
                          recordtime=recordtime,
                          strata,
                          samplesperstrata)
  
  biodata=data.frame(river=NA,
                     day=NA,
                     mon=NA,
                     sample=NA,
                     species=NA,
                     sex=NA,
                     fork.length=NA,
                     weight=NA,
                     scale=NA,
                     notes=NA)
  write.csv(biodata,file=paste(rivername,year,"biocharacteristics data.csv",sep="_"),
            na="",row.names = F)
  
  cat("\n","Biological characteristics data sheet saved to WD")
  if(speciesID==T){
    speciesprop=data.frame(mon=unlist(mapply(rep,times=c(12,20),x=c(5,6))),
                           day=c(20:31,1:20),
                           BB.sampled=NA,
                           all.sampled=NA,
                           BB.ID=NA,
                           all.ID=NA,
                           BB.prop=NA)
    write.csv(speciesprop,file=paste(rivername,year,"in season Species ID data.csv",sep="_"),
              na="",row.names = F)
    cat("\n","Species ID data sheet saved to WD")
  }
  
}