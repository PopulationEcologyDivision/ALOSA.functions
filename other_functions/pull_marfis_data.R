# Library
library(ROracle)

# This is where we keep our MARFIS pulls
file_path <- "R:/Science/Population Ecology Division/DFD/Alosa/MARFISSCI/Old pulls/marfis_pull.Rdata"

# Check if the MARFIS pull exists
if (file.exists(file_path)) {
  
  # Get the last modification date of the MARFIS pull
  file_date <- file.info(file_path)$mtime
  
  # Get the current date
  current_date <- Sys.Date()
  
  # Prompt the user whether or not to update the MARFIS pull
  response <- readline(prompt = paste0("The last MARFIS pull is from ", as.Date(file.info(file_path)$mtime), ". Would you like to update it? (y / n): "))
  
  # Process user response
  if (tolower(response) == "y" | tolower(response) == "yes") {
    
    message("Updating the MARFIS pull")
    
    # Connect to MARFIS db and pull
    oracle.password <- "_REMOVED"
    oracle.username <- "BILLARDM"
    
    channel <- dbConnect(
      DBI::dbDriver("Oracle"),
      username = oracle.username,
      password = oracle.password,
      dbname = "PTRAN",
      believeNRows = FALSE
    )
    
    source("~/git/ALOSA.functions/MARFIS_all in one.R")
    save(catch, didnotfish, licencerenewals, file = file_path)
    
  } else {
    message(paste0("Using previous MARFIS pull from ", as.Date(file.info(file_path)$mtime)))
  }
  
} else {
  message("MARFIS pull not found")
  message("Pulling data from MARFIS ...")
  
  # Connect to MARFIS db and pull
  oracle.password <- "_REMOVED"
  oracle.username <- "BILLARDM"
  
  channel <- dbConnect(
    DBI::dbDriver("Oracle"),
    username = oracle.username,
    password = oracle.password,
    dbname = "PTRAN",
    believeNRows = FALSE
  )
  source("~/git/ALOSA.functions/MARFIS_all in one.R")
  save(catch, didnotfish, licencerenewals, file = file_path)
  message("Updated MARFIS pull saved")
  
}

# Load in the data
load(file_path)
message("MARFIS data loaded")
