#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



format.IDfish.toBIODATA=function(filename){
  data=read.csv(filename,header = T,stringsAsFactors = F)
  goodnames<-(c("mon","day","BB.sampled",	"all.sampled",	"BB.ID",	"all.ID")) 
  missingnames=goodnames[!goodnames%in%(names(data))]
  if (length(missingnames>0)){
    cat("Missing column name(s):","\n", missingnames,"\n")
    stop("Please fix column names before continuing")
  }
  
  
  month=unlist(mapply(rep,times=data$all.ID,x=data$mon))
  day=unlist(mapply(rep,times=data$all.ID,x=data$day))
  year=unlist(mapply(rep,times=data$all.ID,x=2021))
  
  countID=NULL
  for(i in 1:length(data$mon)){
    spptemp=c(data$BB.ID[i],
              data$all.ID[i]-data$BB.ID[i])
    countID=c(countID,spptemp)
  }
  
  specieslist=rep(c("B","A"),times=length(data$mon))
  species=unlist(mapply(rep,times=countID,x=specieslist))
  sample=10000:(10000+(length(species)-1))
  
  x=data.frame(year=year,
               mon=month,
               day=day,
               sample=sample,
               species=species,
               sex=NA,
               fork.length=NA,
               weight=NA,
               scale="N")
  
  write.csv(x,file="tempdataforspeciesID.csv",na="") 
  cat("Temp file was written to working directory under tempdataforspeciesID.csv")
  cat("\n","Please copy over to biological data sheet then delete temp file.")
}