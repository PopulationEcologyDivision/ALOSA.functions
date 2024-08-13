#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script splits .pdfs from large scans into singular pages.
# It was originally named split_pdf.R and was located in the logbook directory
# so I decided to put it here and rename it (and other scripts associated with
# log book scanning) so we could keep track of it.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean up the environment and clear the console of crud
rm(list = ls())
cat("\014")

# Load the pdftools package
library(pdftools)

# Show R the directory containing the scans of the log-books. These scans are 
# assumed to contain multiple licences per .pdf file as they are scanned in
# batches instead of a single .pdf per logbook.

# There is a bug in R 4.3.2 that doesn't let choose.dir work without using 
# choose.files first for some reason. Just choose a random file on your
# system to get the choose.dir function to work. It is weird.
choose.files()
PATH2WD = choose.dir(caption = "Navigate to DIRECTORY containing the pdfs you wish you split")
setwd(PATH2WD)

# Scans of the log books have historically been saved in folders named with the
# format: "YEAR_FFLR_Scans_Gaspereau". Assuming that we have created a folder
# for the year we are doing scans, the following code extracts what the current
# year is from the folder name using "grep".

# Split the file path of the folder into pieces
path = strsplit(PATH2WD, "\\\\")

# Search for the year by finding the folder containing the year with grep
for (i in path) { p = grep("FFLR_Scans_Gaspereau", i, value = T) }
year = substr(p, 0, 4)

# List all the files in the directory. This is used when selector == 2 below
loglist = list.files(PATH2WD)

# The following controls whether the script splits .pdfs that are in a single
# file in the directory versus splitting multiple files, each with multiple
# .pdfs. The user would know this ahead of time.

selector = readline(prompt = "Enter 1 to select single pdf / Enter 2 to loop through each pdf in directory: ")

# This single .pdf option
if (selector == 1) {
  
  # If the split-logs folder does not exist, create one
  if (file.exists(paste0(getwd(), "//SPLIT_LOGS")) == FALSE) { dir.create(paste0(getwd(), "//SPLIT_LOGS")) }
  
  # Here we choose the pdf file to split
  PATH2PDF = choose.files(default = paste0(PATH2WD, "/*.*"), caption = "SELECT PDF YOU WANT SPLIT")
  
  # This extracts the number of pages from the .pdf file using the pdf_info 
  # function from pdftools
  pdf_info = pdf_info(PATH2PDF)
  length_pdf = pdf_info$pages
  ln = 0
  
  # Go through each page of the .pdf file  
  for (i in 1:length_pdf) {
    
    # This must be for if there is only one page?
    file_name = paste0(PATH2WD, "\\SPLIT_LOGS\\", i, "_Gaspereau_", year, ".pdf")
    
    # This adds page number (ln) to each file name
    if (file.exists(file_name) == TRUE) {
      ln = ln + 1
      file_name = paste0(PATH2WD, "\\SPLIT_LOGS\\", i, "_", ln, "_Gaspereau_", year, ".pdf")
    }
    
    # Create new .pdfs by sub-setting the larger file of .pdfs. The pages 
    # argument tells which pages to rotate by 90 degrees
    pdf_subset(PATH2PDF, pages = i, output = file_name)
    
  }
  
}

# The multiple .pdf option
if (selector == 2) {
  
  if (file.exists(paste0(getwd(), "//SPLIT_LOGS")) == FALSE) { dir.create(paste0(getwd(), "//SPLIT_LOGS")) }
  
  # Loop through each of the .pdf files
  for (l in loglist) {
    
    # Get the path of the current .pdf file
    PATH2PDF = paste0(PATH2WD, "/", l)
    
    # Get the number of pages from current .pdf file
    pdf_info = pdf_info(PATH2PDF)
    length_pdf = pdf_info$pages
    
    # For the current .pdf file, name it
    for (i in 1:length_pdf) {
      
      filen = paste0("\\SPLIT_LOGS\\", i, "_Gaspereau_", year, ".pdf")
      file_name = paste0(PATH2WD, filen)
      
      while (file.exists(file_name) == TRUE) {
        
        # Add a page number to each file
        n = length(list.files("SPLIT_LOGS"))
        n = n + 1
        file_name = paste0(PATH2WD, "\\SPLIT_LOGS\\", n, "_Gaspereau_", year, ".pdf")
        
      }
      
      # Create new .pdfs by sub-setting the larger file of .pdfs. The pages 
      # argument tells which pages to rotate by 90 degrees
      pdf_subset(PATH2PDF, pages = i, output = file_name)
      
    }
    
  }
  
}

print("Job done")
