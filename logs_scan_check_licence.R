#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Loop through scanned logs to manually assign license related files names to 
# scans of submitted Freshwater Fishing Log Book each year. This is so that 
# the original logs can easily be searched and compared to what was entered 
# into MARFISSCI. Originally named check_Lic_pdf_v2.r (by Mike Adams)
#
# Note: program sometimes freezes for unknown reason after converting PDF to 
# image (png), if this happens close program, delete the png from the working 
# directory, restart R, and rerun program.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear your environment and console
rm(list = ls())
cat("\014")

# Load required packages
library(pdftools)
library(png)
library(magick)
library(tesseract)
library(imager)

# Choose directory containing .pdfs which you want to process using the GUI
PATH2DATA = choose.dir(default = "R:\\Science\\Population Ecology Division\\DFD\\Alosa\\Logbooks\\Logbook Scans\\", caption = "Navigate to DIRECTORY containing the pdfs you wish you check")

# Alternatively, comment the line above and un-comment the line below, 
# which allows manual enter of the directory path
#PATH2DATA = "ENTER//DIRECTORY//PATH//HERE"

# Change the working directory for ease of reading and writing files
setwd(PATH2DATA)

# Create a list of pdfs within the directory selected above
loglist = list.files(PATH2DATA)

# order list of PDFs numerically
loglist = loglist[order(as.numeric(gsub("[^0-9]+", "", loglist)))]

# This piece of logic allows to either process 
# (1) a single file after which the program terminates or 
# (2) steps through each file in the selected directory
# Set junk to 1 to default to single file mode
#junk = 1 
junk = readline(prompt = "Enter 1 to select single pdf / Enter 2 to loop through each pdf in directory: ")

# This is the start of the single file switch
if (junk == 1) {
  
  # Get the path to the files and split it into strings
  PATH2PDF = choose.files(default = paste0(PATH2DATA, "/*.*"), caption = "SELECT PDF YOU WANT CHECK")
  path = strsplit(PATH2PDF, "\\\\")
  
  # Look for .pdf files in the path
  for (r in path) {
    p = grep(".pdf", r, value = T)
  }
  
  # Get the name before the .pdf in the file name
  temp = strsplit(p, "\\.")
  temp =  temp[[1]][1]
  filename = paste0(temp, '.png')
  
  # Convert .pdf to .png images
  log_pic = pdf_convert(
    PATH2PDF,
    format = "png",
    pages = NULL,
    filenames = filename,
    dpi = 300,
    antialias = TRUE,
    opw = "",
    upw = "",
    verbose = TRUE
  )
  
  # Crop to check rotation using the magick image processing package
  img_log <- image_read(filename)
  img_log = image_quantize(img_log, colorspace = 'gray')
  img_log = image_modulate(img_log,
                           brightness = 100,
                           saturation = 300,
                           hue = 0)
  img_info = image_info(img_log)
  # First rotation happens here because of the orientation of most scans
  fisheries_check = image_rotate(img_log,-90)
  # Change image resolution to 1000 x 250
  fisheries_check = image_crop(fisheries_check, "1000x250")
  
  # This section of code iteratively rotates and searches for the words 
  # "Fisheries and Oceans" in the corner of the scans. Once the words are found,
  # the document is in the correct orientation.
  test_param = "Fisheries and Oceans"
  eng <- tesseract("eng")
  text <- tesseract::ocr(fisheries_check, engine = eng)
  cat(text)
  text = gsub("[\r\n]", "", text)
  test = grepl(test_param, text, fixed = TRUE)
  
  flip_count = 1
  
  while (test == FALSE) {
    fisheries_check = image_rotate(fisheries_check, -90)
    flip_count = flip_count + 1
    text <- tesseract::ocr(fisheries_check, engine = eng)
    cat(text)
    text = gsub("[\r\n]", "", text)
    test = grepl(test_param, text, fixed = TRUE)
    
    # This section breaks the rotation test loop, so that if the test is false 
    # on the fourth 90 degree turn, the whole scanned page is printed to the 
    # viewer and it asks for the License # to entered manually. Alternatively,
    # entering "c" will terminate the program, clear the image from the 
    # R environment, and delete the .png file
    if (flip_count > 4) {
      test = TRUE
      print(img_log)
      newlic = readline(prompt = "Manually enter licence: ")
      runk = strsplit(filename, "_")
      fn_end = paste0(runk[[1]][2], sep = "_", runk[[1]][3])
      fn_end = strsplit(fn_end, "\\.")
      filenum = 1
      newfn = paste0(newlic, sep = "_", fn_end[[1]][1], "_", filenum, '.pdf')
      fn_b = newfn
      oldfn = paste0(substr(filename, 1, nchar(filename) - 4), '.pdf')
      
      while (file.exists(newfn) == TRUE) {
        newfn = paste0(newlic, sep = "_", fn_end[[1]][1], '_', filenum + 1, '.pdf')
        filenum = filenum + 1
      }
      
      file.rename(oldfn, newfn)
      
      stop()
      
      if (newlic == "c") {
        rm(img_log)
        file.remove(filename)
        stop()
      }
      
    }
    
  }
  
  # The following section rotates the original un-cropped image,
  # then crops it to the approximate location of the License number
  # The user is then asked to verify if the filename of the pdf matches the 
  # license printed on the logbook. If yes, the programs moves onto the next 
  # file. If no, the user is asked to input the license number, which, once 
  # entered will rename the origin pdf with the entered license number. 
  # There is also some built in logic that if a pdf with that license already 
  # exists then a _x+1 will delineate the files accordingly.  Entering "c" at 
  # any point will terminate the program, clear the image from the R 
  # environment, and delete the .png file.
  lic_image = image_rotate(img_log, -90 * flip_count)
  lic_image = image_crop(lic_image, "600x500+2900+350")
  print(lic_image)
  print(filename)
  swtch = readline(prompt = "Does Licence match filename? Y/N \nPress c to quit")
  
  if (swtch == "c") {
    rm(img_log)
    file.remove(filename)
    stop()
  }
  
  if (swtch == "N" | swtch == "n") {
    newlic = readline(prompt = "Manually enter licence: ")
    runk = strsplit(filename, "_")
    fn_end = paste0(runk[[1]][2], sep = "_", runk[[1]][3])
    fn_end = strsplit(fn_end, "\\.")
    filenum = 1
    newfn = paste0(newlic, sep = "_", fn_end[[1]][1], "_", filenum, '.pdf')
    fn_b = newfn
    oldfn = paste0(substr(filename, 1, nchar(filename) - 4), '.pdf')
    
    while (file.exists(newfn) == TRUE) {
      newfn = paste0(newlic, sep = "_", fn_end[[1]][1], '_', filenum + 1, '.pdf')
    }
    
    file.rename(oldfn, newfn)
  }
  
  # Remove image from both the R environment and the directory after each 
  # iteration to save physical and virtual memory.
  rm(img_log)
  file.remove(filename)
} # This is the end of the single file switch

# This is the start of the multi-file switch
if (junk == 2) {
  # start loop here to step through each file in the directory
  for (l in loglist) {
    PATH2PDF = paste0(PATH2DATA, "\\", l)
    path = strsplit(PATH2PDF, "\\\\")
    
    for (r in path) {
      p = grep(".pdf", r, value = T)
    }
    
    temp = strsplit(p, "\\.")
    temp =  temp[[1]][1]
    filename = paste0(temp, '.png')
    
    # Convert to PNG pages
    log_pic = pdf_convert(
      PATH2PDF,
      format = "png",
      pages = NULL,
      filenames = filename,
      dpi = 300,
      antialias = TRUE,
      opw = "",
      upw = "",
      verbose = TRUE
    )
    
    # Crop to check rotation.
    img_log <- image_read(filename)
    img_log = image_quantize(img_log, colorspace = 'gray')
    img_log = image_modulate(
      img_log,
      brightness = 100,
      saturation = 300,
      hue = 0
    )
    img_info = image_info(img_log)
    fisheries_check = image_rotate(img_log,-90)
    fisheries_check = image_crop(fisheries_check, "1000x250")
    # Test to find "Fishe" in top left hand corner to identify proper orientation
    test_param = "Fishe"
    eng <- tesseract("eng")
    text <- tesseract::ocr(fisheries_check, engine = eng)
    cat(text)
    text = gsub("[\r\n]", "", text)
    test = grepl(test_param, text, fixed = TRUE)
    
    flip_count = 1
    
    # Flip log image by 90 degrees until "Fishe" is found or it has been turned 
    # 360 degrees
    while (test == FALSE) {
      fisheries_check = image_rotate(fisheries_check,-90)
      flip_count = flip_count + 1
      text <- tesseract::ocr(fisheries_check, engine = eng)
      cat(text)
      text = gsub("[\r\n]", "", text)
      test = grepl(test_param, text, fixed = TRUE)
      
      # If flipped 360 degrees and still test is false print the whole 
      # uncropped image and prompt for licence.
      if (flip_count > 4) {
        test = TRUE
        print(img_log)
        newlic = readline(prompt = "Manually enter licence: ")
        
        if (newlic == "skip") {
          rm(img_log)
          file.remove(filename)
          next
        }
        
        if (newlic == "c") {
          rm(img_log)
          file.remove(filename)
          stop()
        }
        
        runk = strsplit(filename, "_")
        fn_end = paste0(runk[[1]][2], sep = "_", runk[[1]][3])
        fn_end = strsplit(fn_end, "\\.")
        filenum = 1
        newfn = paste0(newlic, sep = "_", fn_end[[1]][1], "_", filenum, '.pdf')
        fn_b = newfn
        oldfn = paste0(substr(filename, 1, nchar(filename) - 4), '.pdf')
        
        while (file.exists(newfn) == TRUE) {
          newfn = paste0(newlic, sep = "_", fn_end[[1]][1], '_', filenum + 1, '.pdf')
          filenum = filenum + 1
        }
        
        file.rename(oldfn, newfn)
        rm(img_log)
        file.remove(filename)
      } # end flip count
    }
    
    if (exists("img_log") == FALSE) {
      next
    }
    
    lic_image = image_rotate(img_log,-90 * flip_count)
    lic_image = image_crop(lic_image, "600x500+2900+350")
    print(lic_image)
    print(filename)
    swtch = readline(prompt = "Does Licence match filename? Y/N ")
    
    if (swtch == "skip" | swtch == "Y" | swtch == "y") {
      rm(img_log)
      file.remove(filename)
      next()
    }
    
    if (swtch == "c") {
      rm(img_log)
      file.remove(filename)
      stop()
    }
    
    if (swtch == "N" | swtch == "n") {
      print(lic_image)
      newlic = readline(prompt = "Manually enter licence or 'more' to view entire page: ")
      
      if (newlic == "more") {
        print(img_log)
        newlic = readline(prompt = "Manually enter licence:")
      } #add zoom out
      
      runk = strsplit(filename, "_")
      fn_end = paste0(runk[[1]][2], sep = "_", runk[[1]][3])
      fn_end = strsplit(fn_end, "\\.")
      filenum = 1
      newfn = paste0(newlic, sep = "_", fn_end[[1]][1], "_", filenum, '.pdf')
      oldfn = paste0(substr(filename, 1, nchar(filename) - 4), '.pdf')
      
      while (file.exists(newfn) == TRUE) {
        #print(newfn)
        newfn = paste0(newlic, sep = "_", fn_end[[1]][1], '_', filenum +
                         1, '.pdf')
        filenum = filenum + 1
      }
      
      file.rename(oldfn, newfn)
      print("check2")
    }
    
    # should probably remove image after each iteration or might run out of memory.
    
    rm(img_log)
    file.remove(filename)
    print("check3")
    
  }   # end loop
} # this is the end of the multi-file switch
