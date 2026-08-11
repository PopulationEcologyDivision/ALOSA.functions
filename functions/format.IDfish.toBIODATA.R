#
#Description:
# converts the species ID data from conuts of BB and A on each day to a row entry
# of BB or A for each fish, matching the format of the bio.data. The output file
# can then be manually added to the biodata and uploaded to the database,
# or if only species ID data were collected, the output file uploaded direclty
#
# Inputs: a species ID csv generated from the blank.datasheets.R function
# !Note that all cells should be filled in in the csv. if no fish were sampled,
# then all columns other than day and mon should be 0
#
# This function is used by:
# Writing annual data to oracle, for adding species ID data to bio.data
#
# This functions uses:


format.IDfish.toBIODATA=function(filename,year){
  data=read.csv(filename,header = T,stringsAsFactors = F)
  goodnames<-(c("mon","day","BB.sampled",	"all.sampled",	"BB.ID",	"all.ID")) 
  missingnames=goodnames[!goodnames%in%(names(data))]
  if (length(missingnames>0)){
    cat("Missing column name(s):","\n", missingnames,"\n")
    stop("Please fix column names before continuing")
  }
  
  
  month=unlist(mapply(rep,times=data$all.ID,x=data$mon))
  day=unlist(mapply(rep,times=data$all.ID,x=data$day))
  
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
               scale="N",
               notes="species id only")
  
  write.csv(x,file="tempdataforspeciesID.csv",na="",row.names=F) 
  cat("Temp file was written to working directory under tempdataforspeciesID.csv")
  cat("\n","Please copy over to biological data sheet then delete temp file.")
}