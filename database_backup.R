# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Name: database_backup.R
# 
# This little script simply grabs all the tables we currently have in the 
# GASPEREA database and backs them up to the R drive as CSV. We can use this
# script when we make bigger changes to the database that have the potential
# to be bad.
# 
# ʕ•ᴥ•ʔ LG Nov 2025
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Connect to the GASPEREA db - the password and username should be in your env
# or you can just change this part so that you provide them in the dbConnect
# call
require("ROracle") 
channel <- dbConnect(
  DBI::dbDriver("Oracle"),
  oracle.username.GASP,
  oracle.password.GASP,
  "PTRAN",
  believeNRows = FALSE
  )

# This is the list of tables in the db and will act as the list of tables that
# are backed up in the for loop below
tables_to_backup <- dbListTables(channel)

# This is where we want to backup the database
backup_dir <- "R:/Science/Population Ecology Division/DFD/Alosa/Database Management/GASPEREA_backup"

# If the directory doesn't exist yet, create it
if(!dir.exists(backup_dir)) {dir.create(backup_dir)}

# Go through all the tables in the db, grab their data, and write to CSV
for (table in tables_to_backup) {
  
  message(paste("Backing up table:", table))
  
  # Grab data
  query <- paste("SELECT * FROM", table)
  data <- dbGetQuery(channel, query)
  
  # Create file path
  file_path <- file.path(backup_dir, paste0(table, ".csv"))
  
  # Write to CSV on the R drive
  write.csv(data, file_path, row.names = FALSE)
  
  message(paste("Backup saved to:", file_path))
  
}

# Disconnect from the database
dbDisconnect(channel)
message("Backup completed for all tables")

