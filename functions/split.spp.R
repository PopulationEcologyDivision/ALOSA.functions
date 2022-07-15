#
# Calculate daily species split in the samples.
#
# Inputs:
#        - 
#
# This function is used by:
#     - no user defined functions use this function

# This functions uses:
#     - 


split.spp<-function(year,siteID,channel,accessory.datafile){
  biodataA<-get.bio.data(year,siteID,3501,channel)
  biodataB<-get.bio.data(year,siteID,3502,channel)
  biodataALL<-rbind(biodataA,biodataB)
  accessorydata=read.csv(accessory.datafile,header=T,stringsAsFactors = F)
  biodataALL=biodataALL[,c("DAY","MON","SPECIES_ID")]
  goodnames<-c("DAY","MON","SPECIES_ID")
  name.check<-names(accessorydata)==goodnames
  if(any(name.check==FALSE)){
    stop("accessory data format does not match DAY,MON,SPECIES_ID format")
  }
    allID=rbind(biodataALL,accessorydata)
  temp0=aggregate(allID$SPECIES_ID,by=list(DAY=allID$DAY,MON=allID$MON),
                  FUN=user.count)
  names(temp0)[3]<-'all'
  temp1=aggregate(allID$SPECIES_ID[allID$SPECIES_ID==3502],
                  by=list(DAY=allID$DAY[allID$SPECIES_ID==3502],
                          MON=allID$MON[allID$SPECIES_ID==3502]),
                  FUN=user.count)
  names(temp1)[3]<-'BB'
  summarycounts<-merge(temp0,temp1,all.x=T)
  summarycounts[is.na(summarycounts)]<-0
  summarycounts$BBprop=summarycounts$BB/summarycounts$all
  summarycounts<-summarycounts[order(summarycounts$MON,summarycounts$DAY),]
  names(summarycounts)<-c("day","mon","all","BB","BBprop")
  return(summarycounts)
}


accessory.datafile="accessory_data.csv"
