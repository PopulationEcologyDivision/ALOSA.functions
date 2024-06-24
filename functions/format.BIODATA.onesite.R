# Description:
# Inputs:
# This function is used by:
# This functions uses:

format.BIODATA.onesite <- function(filename) {
  
  data <- read.csv(filename, header = T, stringsAsFactors = F)
  
  goodnames <- (c("year", "mon", "day", "sample", "species", "sex", "fork.length", "weight", "scale", "notes"))
  
  missingnames = goodnames[!goodnames%in%(names(data))]
  
  if(length(missingnames > 0)){
    
    cat("Missing column name(s):","\n", missingnames,"\n")
    
    stop("Please fix column names before continuing")
    
  }
  
  siteID = as.numeric(readline(prompt = "Please enter site number \n If site number is unknown enter 0"))
  
  if(siteID == 0){
    
    x <- as.numeric(readline('Which River? \n 1. "GASPEREAU" \n 2. "TUSKET" \n 3. "MERSEY" \n 4. "MEDWAY" \n 5. "SHUBIE" \n 0. "NONE OF THESE" \n'))
  
    if(x == 0){ stop("Go find the site ID from the database") }
    if(x == 1){ siteID = as.numeric(readline('Which Site? \n 3. "WHITE ROCK" \n 4. "LANES MILL" \n')) }
    if(x == 2){ siteID = as.numeric(readline('Which Site? \n 1. "CARLETON" \n 2. "VAUGHAN" \n 14. "POWERHOUSE" \n')) }
    if(x == 3){ siteID = as.numeric(readline('Which Site? \n 5. "ROLL DAM" \n 6. "COWIE FALLS" \n 7. "DEEP BROOK" \n 8. "LOWER GREAT BROOK" \n 9. "BIG FALLS" \n 10. "LOWER LAKE FALLS" \n 11. "UPPER LAKE FALLS" \n')) }
    if(x == 4){ siteID = as.numeric(readline('Which Site? \n 12. "SITE 1"\n')) }
    if(x == 5){ siteID = as.numeric(readline('Which Site? \n 13. "BASS TRAP" \n')) }
    
  }
  
  # Fix up data type and add missing columns
  #data <- data[!(is.na(data$count.upstream)), ]
  data <- data[ , names(data) %in% goodnames]
  
  data$SITE_ID <- siteID
  
  A.names = c("Alewife", "Alewife ", "A", "a", "alewife", "ale", "Ale", "ALE")
  B.names = c("Blueback Herring", "blueback herring", "Blueback", "blueback", "BBH", "b", "B", "bbh", "Bbh", "Bb", "BB", "bb")
  
  data$species[data$species %in% A.names] <- 3501
  data$species[data$species %in% B.names] <- 3502
  data$species <- as.numeric(data$species)
  
  F.names <- c("female", "F", "f", "FEMALE", "Female")
  M.names <- c("male", "m", "M", "MALE", "Male")
  PS.names <- c("Post Spawn", "PS", "postspawn", "ps")
  U.names <- c("Unknown", "u", "U", "UNKNOWN", "?", "??", " ")
  
  data$sex[data$sex %in% F.names] <- 2
  data$sex[data$sex %in% M.names] <- 1
  data$sex[data$sex %in% PS.names] <- 3
  data$sex[data$sex %in% U.names] <- 4
  data$sex <- as.numeric(data$sex)
  
  data$fork.length = round(data$fork.length, digits = 1)
  
  col_order <- c("sample", "SITE_ID", "year", "mon", "day","species", "sex", "fork.length", "weight", "scale", "notes")
  
  data <- data[ , col_order]
  
  names(data) <- c("FISH_ID", "SITE_ID", "YEAR", "MON", "DAY","SPECIES_ID", "SEX_ID", "FORK_LENGTH", "WEIGHT", "SCALE", "NOTES")
  
  return(data)
  
}