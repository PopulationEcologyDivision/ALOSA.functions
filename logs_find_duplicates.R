#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Originally named find_dups.r (by Mike Adams) this script finds duplicate files
# in a directory tree or compare two locations for duplicates even if file name
# is different. Ingests files and compare file contents not just filename.
# Takes some time to run. Works with video files too. Could be used to make sure
# videos are backed up fully by running.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear the working environment and the console
rm(list = ls())
cat("\014")

# Load the digest package which creates hash functions for R objects or files
library("digest")

# If switch is set to 1 the program will compare two locations (drives or folders)
# If switch is set to 0 the program will just check one folder for duplicates
swtch = 0

# Comparing two locations:
if (swtch == 1) {
  
  # If comparing two locations, enter first location here ...
  test_dir1 = "C:\\Users\\Adamsmi\\Documents\\Delete1"
  
  # ... and the second location here
  test_dir2 = "C:\\Users\\Adamsmi\\Documents\\Delete2"
  
  # Get recursive file-list from each designated location
  filelist1 <- dir(test_dir1, pattern = "h264", recursive = TRUE, all.files = TRUE, full.names = TRUE) 
  
  # Change the pattern = to whatever file type you wish to search for
  filelist2 <- dir(test_dir2, pattern = "h264", recursive = TRUE, all.files = TRUE, full.names = TRUE)
  filelist = c(filelist1, filelist2)
  
} else {
  
  # If just looking for duplicates in one location, enter it here
  #test_dir = "R:\\Science\\Population Ecology Division\\DFD\\Alosa\\Freshwater Fishing Logbooks\\Logbook Scans"
  test_dir = choose.dir(default = "R:\\Science\\Population Ecology Division\\DFD\\Alosa\\Logbooks\\Logbook Scans\\", caption = "Navigate to DIRECTORY containing the pdfs you wish you check")
  
  # Get recursive file-list from each designated location. You can change the 
  # pattern = to whatever file type you wish to search for.
  filelist <- dir(test_dir, pattern = "pdf", recursive = TRUE, all.files = TRUE, full.names = TRUE)
  head(filelist)
  
}

# Digest the first 5000 lines into hash output R objects associated with a 
# filename and location
md5s <- sapply(filelist, digest, file = TRUE, algo = "md5", length = 5000)
duplicate_files = split(filelist, md5s)
head(duplicate_files)

# The object "z" will store any duplicated files
z = duplicate_files
z2 = sapply(z, function(x) { length(x) > 1 })
z3 = split(z, z2)
head(z3$"TRUE")

# Once run the z3 data-frame will be a list of either 1 or 2. The "TRUE" object
# will be a nested list of all files which have duplicates (along with their
# full path name), the "FALSE" object is a nest list of files that do not have
# duplicates. If there are no files in either the "TRUE" or "FALSE" objects then
# the respective object will be missing resulting in only one object being present.
