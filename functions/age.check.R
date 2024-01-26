#
#Description: Function to QAQC age data
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



age.check<-function(filename){
  data=read.csv(filename, header=T, stringsAsFactors = F)
  
  goodnames<-(c("year","sample","species","current.age","age.at.first.spawn","notes","age.structure.sample")) 
  missingnames=goodnames[!goodnames%in%(names(data))]
  if (length(missingnames>0)){
    cat("Missing column name(s):","\n", missingnames,"\n")
    stop("Please fix column names before continuing")
  }
  cat("\n")
  # Look for duplicated sample numbers 
  duplicates=data$sample[which(duplicated(data$sample))]
  
  #"if" compares "duplicated" results to a set parameter (in this case 0) 
  #if there are duplicates it will print all duplicates found in sample column
  if(length(duplicates)>0){
    cat("Duplicated sample numbers found:", "\n", "\n")
    print(data[data$sample%in%duplicates,])
    cat("\n", "\n")
  }
  
  A.names=c("Alewife","A","a","alewife","ale","Ale","ALE")
  B.names=c("Blueback Herring","blueback herring","Blueback",
            "blueback","BBH","b","B","bbh","Bbh","Bb","BB","bb")
  n.A=length(data$current.age[!is.na(data$current.age) &
                                data$species%in%A.names])
  n.B=length(data$current.age[!is.na(data$current.age) &
                                data$species%in%B.names])
  
  cat("Number of Alewife Aged:",n.A,"\n")
  cat("Number of Blueback Aged:",n.B,"\n","\n")
  
  data$xx=as.numeric(paste(data$current.age,data$age.at.first.spawn,sep="."))
  
  cat("Alewife Ages:")
  print(table(data$xx[data$species%in%A.names]))
  cat("\n","Youngest Alewife:", min(data$current.age[data$species%in%A.names]), "\n")
  cat(" Oldest Alewife:", max(data$current.age[data$species%in%A.names]), "\n","\n")
  
  if (n.B>0){
    cat("Blueback Ages:")
    print(table(data$xx[data$species%in%B.names]))
    cat("\n","Youngest Blueback:", min(data$current.age[data$species%in%B.names]), "\n")
    cat(" Oldest Blueback:", max(data$current.age[data$species%in%B.names]), "\n","\n")
  }
}