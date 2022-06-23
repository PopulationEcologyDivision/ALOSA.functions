#
#Description:
#
# Inputs:

#
# This function is used by:
# 

# This functions uses:



MARFIS_queries=function(local.csv=F,catch,didnotfish,licencerenewals){
  #1_import_MARFIS_queries.r
  # Run this script on the raw exported MARFIS csv. This should be the first file run after a fresh MARFIS pull.
  # Allows for easy import of the results from the MARFISSCI SQL pulls. 
  # Input: .csv files for catch, licence info, fisher info, area info, and did not fish list.
  
  
  WD = choose.dir(caption = "Navigate to Desired WORKING DIRECTORY") 
  setwd(WD)
  today = date()
  today = strsplit(today, " ")
  today_date = paste0(today[[1]][2],"_",today[[1]][3],"_",today[[1]][5])
  
  ###
  # Read in Data files: 
  if(local.csv==T){
    catch=read.csv(choose.files(caption="Select CATCH CSV"),header=T,stringsAsFactors = F)
    didnotfish=read.csv(choose.files(caption="Select DidNotFish CSV"),header=T, stringsAsFactors = F)
    licencerenewals=read.csv(choose.files(caption="Select LICENCE RENEWALS CSV"),header=T,stringsAsFactors = F)
  }
  #fisherinfotype=read.csv(choose.files(caption="Select FISHER CSV"),header=T,stringsAsFactors = F)
  #areainfo=read.csv(choose.files(caption="Select AREA CSV"),header=T,stringsAsFactors = F)
  #licenceinfo=read.csv(choose.files(caption="Select LICENCE CSV"),header=T,stringsAsFactors = F)
  
  #Remove instances when weight is NA. This happened at some point 
  # during the MARFIS data query
  catch=catch[!is.na(catch$FV_WEIGHT),]
  #There is one fisher who keeps adding the location as '0000' and R
  # keeps converting it to '0' when updating the masterlogbook records.
  # This is very annoying, so I am just changing it to 0 right away to 
  # avoid the error.
  catch$RIVERNAME_LOGBOOK[catch$RIVERNAME_LOGBOOK=="0000"]="0"
  #Remove duplicate data points
  # Some duplicates are data entry errors and some are real data that
  # was split due to large catches (silly way of entering data)
  # I don't have time to pick through this but I will add it to a check
  # in river specific pulls
  #catch = unique(catch)
  #===
  # 2_find_new_rivernames.r
  # find and add new rivername variants entered into MARFISSCI catch data
  # This should be redone after each new MARFIS pull.
  
  rivernames = read.csv("MASTER_RIVERNAME_DATABASE.csv") 
  
  
  x=unique(catch[,c("RIVERNAME_LOGBOOK","YEAR","LICENCE_ID")])
  y=unique(rivernames[,c("RIVERNAME_LOGBOOK","YEAR","LICENCE_ID")])
  y$DF="MASTERLIST"
  zz=merge(x,y,all.x=T)
  addme=zz[is.na(zz$DF),]
  
  new_to_add = addme[,c("LICENCE_ID","RIVERNAME_LOGBOOK","YEAR")]
  
  rivernames.add=rivernames[1:dim(new_to_add)[1],]
  rivernames.add[,]=NA
  rivernames.add[,1:3]=new_to_add
  
  if(!is.na(rivernames.add[1,2])){
    rivernames=rbind(rivernames,rivernames.add)       
    write.csv(rivernames,file="MASTER_RIVERNAME_DATABASE.csv",row.names=F)
    
    cat(paste(dim(new_to_add)[1], "new lines were saved to the Master Rivername Database",
              "\n","\n","Please fill in the cleaned information directly into the CSV"),
        "\n","and save when complete.","\n",sep=" ")
    
  }else{
    cat(" No new lines were saved to the Master Rivername Database","\n")        
  }
  
  
  readline(prompt= "Please press [Enter] to contine once CSV has been updated with cleaned rivername information")
  
  rivernames = read.csv("MASTER_RIVERNAME_DATABASE.csv") 
  
  
  # Merge cleaned rivernames to catch
  
  cleaned_catch = merge(catch,rivernames,all.x=T)
  
  cat(paste("Difference between merged catch and original =",
            (dim(cleaned_catch)[1]-dim(catch)[1]),sep=" "))
  
  assign("catch",cleaned_catch,envir = .GlobalEnv)
  assign("didnotfish",didnotfish,envir = .GlobalEnv)
  assign("licencerenewals",licencerenewals,envir = .GlobalEnv)
}