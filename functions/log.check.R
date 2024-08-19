#### TO MAKE TO COMPARE LOGS AND MARFIS####
log.check <- function(
    years = NA
){
  setwd("R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans")
  if(length(years)>=1){
    prefix_df <- data.frame(LICENCE_ID = character(0), stringsAsFactors = FALSE)
    for (i in 1:length(years)){
    setwd(paste(as.character(years[i]), "_FFLR_Scans_Gaspereau", sep = ""))
      logs <- list.files(pattern = "*.pdf$", recursive = TRUE, full.names = F)
      file_name <- sub(paste("_Gaspereau_",as.character(years[i]),".pdf", sep = ""), "", basename(logs))
      file_name2 <- sub(paste("_Gaspereau_",as.character(years[i]),"_1.pdf", sep = ""), "", file_name)
      file_name3 <- sub(paste("_Gaspereau_",as.character(years[i]),"_2.pdf", sep = ""), "", file_name2)
      file_name4 <- sub(paste("_Gaspereau_",as.character(years[i]),"_3.pdf", sep = ""), "", file_name3)
      file_name5 <- sub(paste("_Gaspereau_",as.character(years[i]),"_4.pdf", sep = ""), "", file_name4)
      file_name6 <- sub(paste("_Gaspereau_",as.character(years[i]),"_5.pdf", sep = ""), "", file_name5)
      file_name7 <- sub(paste("_Gaspereau_",as.character(years[i]),"_6.pdf", sep = ""), "", file_name6)
      file_name8 <- sub(paste("_Gaspereau_",as.character(years[i]),"_7.pdf", sep = ""), "", file_name7)
      file_name9 <- sub(paste("_Gaspereau_",as.character(years[i]),"_8.pdf", sep = ""), "", file_name8)
      file_name10 <- sub(paste("_Gaspereau_",as.character(years[i]),"_9.pdf", sep = ""), "", file_name9)
      file_name11 <- sub(paste("_Gaspereau_",as.character(years[i]),"_10.pdf", sep = ""), "", file_name10)
      file_name12 <- sub(paste("_Gaspereau_",as.character(years[i]),"_11.pdf", sep = ""), "", file_name11)
      
      prefix_df <- rbind(prefix_df, data.frame(LICENCE_ID = file_name12, YEAR = rep(x=years[i], length(file_name12))))
      #all.logs <- rbind(all.logs, logs)
      setwd("R:/Science/Population Ecology Division/DFD/Alosa/Freshwater Fishing Logbooks/Logbook Scans")
      
    }
    
  }
  
  return(prefix_df)
  
  
  
}


