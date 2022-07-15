#
#Description: Need to better describe variables
#
# Inputs: countdata is actually daily.summary object from escapement script

#
# This function is used by:
# 

# This functions uses:


ageing.selection<-function(countdata,biodata,missingdays,mergedays,seed,nsamples){
  biodata$date=as.Date(paste(biodata$DAY,biodata$MON,biodata$YEAR,sep="-"),
                       format="%d-%m-%Y")
  
  #dayofyear uses "strftime" to evaluate which day of the year each date aligns with
  biodata$dayofyear=as.numeric(strftime(biodata$date, format="%j"))
  
  
  scaledata=biodata[biodata$SCALE=="Y",]
  n.sampled<-aggregate(scaledata$dayofyear,by=list(scaledata$dayofyear),FUN=function(x){length(x[!is.na(x)])})
  
  colnames(n.sampled)=c("dayofyear","n.sampled")
  
  biodata.with.weights<-merge(biodata,n.sampled,by="dayofyear",all.x=T)
  
  for(i in 1:length(missingdays)){
    countdata$dayofyear[countdata$dayofyear==missingdays[i]]<-mergedays[i]
  }
  
  mergedcountdata=aggregate(countdata$total,by=list(countdata$dayofyear),FUN=sum)
  colnames(mergedcountdata)=c("dayofyear","merged.total")
  
  biodata.with.weights<-merge(biodata.with.weights,mergedcountdata[,c("dayofyear","merged.total")],
                              by="dayofyear",all.x=T)
  
  
  biodata.with.weights$weighting<- biodata.with.weights$merged.total/biodata.with.weights$n.sampled
  
  ##Remove infinite values from the weighting column
  biodata.with.weights$weighting[biodata.with.weights$weighting==Inf]<-0
  
  
  #sample
  set.seed(seed)
  ladd.sample <- data.frame(sample(biodata.with.weights$FISH_ID[biodata.with.weights$SCALE=='Y'],
                                   nsamples, replace = F, prob= biodata.with.weights$weighting[biodata.with.weights$SCALE=='Y']))
  colnames(ladd.sample) <- ("FISH_ID")
  ladd.sample$to.be.aged<-"Y"
  ladd.sample<-ladd.sample[with(ladd.sample, order(FISH_ID)),]
  
  
  
  out<-merge(biodata.with.weights,ladd.sample,by="FISH_ID",all.y=T)
  out$current.age=NA
  out$age.at.first.spawn=NA
  out$age.structure.sample="T"
  out<-out[,c("YEAR","FISH_ID","current.age","age.at.first.spawn","NOTES","age.structure.sample")]
  names(out)<-c("year","sample","current.age","age.at.first.spawn","notes","age.structure.sample")
  out$replacementfor.which.sample=NA
  write.csv(out,"to be aged_rename this file.csv",row.names = F,na="")
}