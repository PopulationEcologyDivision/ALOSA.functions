# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Name: format.AGE.onesite.R
# 
# Note: I edited this script to deal with the changes made to the age table in
# the GASPEREA database (i.e. ALOSA_FISH_AGE_DATA). It needed new column names
# etc.
# 
# ʕ•ᴥ•ʔ
# 
# ~ Logan (Nov 2025)
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


format.AGE.onesite <- function(filename) {
  
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  
  # 'goodnames' had some columns added to it to match with what is now in the db: ager_id, structure_id, ager_notes, primary_age_record
  # These columns should now be added to the CSV you use to do the aging
  goodnames <- (c("year", "sample", "current.age", "age.at.first.spawn", "notes", "age.structure.sample", "ager.id", "structure.id", "ager.notes", "primary.age.record")) 
  
  # We want to see if any of the required column names are missing from the data
  missingnames = goodnames[!goodnames %in% (names(data))]
  
  # If they are, we remind the user to fix the names in the CSV
  if (length(missingnames > 0)) {
    
    cat("Missing column name(s):","\n", missingnames,"\n")
    
    stop("Please fix column names before continuing")
    
    }
  
  # We prompt the user for the correct integer that will be used to represent the SITE_ID
  siteID = as.numeric(readline(prompt = "Please enter site number \n If site number is unknown enter 0"))
  
  # If the user does not know the site number, this will show them some choices
  if (siteID == 0) {
    
    x <- as.numeric(readline('Which River? \n 1. "GASPEREAU" \n 2. "TUSKET" \n 3. "MERSEY" \n 4. "MEDWAY" \n 5. "SHUBIE" \n 0. "NONE OF THESE"\n'))
    
    if (x == 0) { stop(" Go find site ID from database") }
    
    if (x == 1) { siteID = as.numeric(readline('Which Site? \n 3. "WHITE ROCK" \n 4. "LANES MILL" \n')) }
    
    if (x == 2) { siteID = as.numeric(readline('Which Site? \n 1. "CARLETON" \n 2. "VAUGHAN" \n')) }
    
    if (x == 3) { siteID = as.numeric(readline('Which Site? \n 5. "ROLL DAM" \n 6. "COWIE FALLS" \n 7. "DEEP BROOK" \n 8. "LOWER GREAT BROOK" \n 9. "BIG FALLS" \n 10. "LOWER LAKE FALLS" \n 11. "UPPER LAKE FALLS" \n')) }
    
    if (x == 4) { siteID = as.numeric(readline('Which Site? \n 12. "SITE 1"\n')) }
    
    if (x == 5) { siteID = as.numeric(readline('Which Site? \n 13. "BASS TRAP" \n')) }
  
    }
  
  # Only take columns from the data that appear in 'goodnames'
  data <- data[ , names(data) %in% goodnames]
  
  # Create a column for SITE_ID
  data$SITE_ID <- siteID
  
  # Reorder how the columns appear in the data so that they line up when we rename them below
  col_order <- c("sample", "SITE_ID", "year", "current.age", "age.at.first.spawn", "notes", "age.structure.sample", "ager.id", "structure.id", "ager.notes", "primary.age.record")
  data <- data[, col_order]
  
  # We format the column names so they match what is in the ALOSA_FISH_AGE_DATA table
  names(data) <- c("FISH_ID", "SITE_ID", "YEAR", "CURRENT_AGE", "AGE_AT_FIRST_SPAWN", "OLD_NOTES", "AGE_STRUCTURE_SAMPLE", "AGER_ID", "STRUCTURE_ID", "AGER_NOTES", "PRIMARY_AGE_RECORD")
  
  # Keep only the rows of data that have no missing values in CURRENT_AGE and AGE_AT_FIRST_SPAWN i.e. removes rows with NAs in these columns
  data <- data[complete.cases(data[ , c("CURRENT_AGE", "AGE_AT_FIRST_SPAWN")]), ]
  
  return(data)
  
  }