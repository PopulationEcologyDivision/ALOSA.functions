logbook_find_missing <- function(year = NULL) {
  
  if (!exists("catch")) {
    stop("Error: there is no 'catch' dataframe found in your environment. Please load the 'catch' dataframe from MARFIS.")
  }
  
  if (is.null(year)) {
    
    year_input <- readline(prompt = "Please enter the year to check for missing logbooks:")
    
    year <- as.numeric(year_input)
    
  }
  
  library(tidyverse)

  # Set the parent folder that contains the scanned logs
  parent_folder <- tcltk::tk_choose.dir(default = getwd(), caption = "Select folder containing scanned freshwater logbooks")
  
  # Get all PDF file paths from the subfolders
  pdf_files <- list.files(path = parent_folder, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
  
  # Extract just the filename (not the full path)
  file_names <- basename(pdf_files)
  
  # Extract the 6-digit licence number at the beginning of the filename
  licence_no <- stringr::str_extract(file_names, "^\\d{6}")
  
  # Get unique licences
  licence_no <- unique(licence_no)
  
  catch |> 
    filter(YEAR == year) |> 
    distinct(LICENCE_ID) -> marfis_licences
  
  marfis_licences |> 
    filter(!LICENCE_ID %in% licence_no) -> licences_missing_scans
  
  write_csv(licences_missing_scans, paste0(parent_folder, "/licences_missing.scan.csv"))
  
  message("A .csv of licence numbers with missing logs has been saved to ", parent_folder)
  
}

