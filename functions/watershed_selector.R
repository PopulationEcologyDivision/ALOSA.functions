###function to find watershed shapefiles given point
###perhaps using graphical interaction with a plot of watershed boundaries?
###First: function to download list of watershed boundaries, and display a list of watersheds, perhaps using province to filter.
###select watershed number to create a directory and then download the shapefiles.

watershed.selector <- function(watershed = NULL,
                               directory = NULL,
                               flush = T){
  options(timeout=100)
  if (require(sf)==FALSE){install.packages("sf")}
  library(sf)
  original.dir <- getwd()
  if (dir.exists("temp_zip") == F) {
  site <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/index/NHN_INDEX_WORKUNIT_LIMIT_2.zip"
  temp <- tempfile()
  download.file(site, temp)
  unzip(zipfile=temp, exdir="temp_zip") }
  test <- read_sf("temp_zip/NHN_INDEX_22_INDEX_WORKUNIT_LIMIT_2.shp")
  
  ###get shapefiles for the provinces
  if (dir.exists("canada_zip") == F) {
  site <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"
  temp <- tempfile()
  download.file(site, temp)
  unzip(zipfile=temp, exdir="canada_zip") }
  canada <- read_sf("canada_zip/lpr_000b16a_e.shp")
  
  
  if (is.null(watershed)){
    oceans <- data.frame(Selection=1:length(unique(test$OCEAN[!is.na(test$OCEAN)])), Ocean=unique(test$OCEAN[!is.na(test$OCEAN)]))
    print("Select the geographic region below...")
    print(oceans)
    ocean.select <- readline(prompt = "Enter Selection: ")
    ocean.select <- oceans$Ocean[oceans$Selection==ocean.select]
    test <- test[test$OCEAN==ocean.select,]
    
    #now to regional drainage
    regions <- data.frame(Selection=1:length(unique(test$WSCMDANAME[!is.na(test$WSCMDANAME)])), Region=unique(test$WSCMDANAME[!is.na(test$WSCMDANAME)]))
    print("Select the geographic region below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    region.select <- regions$Region[regions$Selection==region.select]
    test <- test[test$WSCMDANAME==region.select,]
    
    #now to subregional drainages
    regions <- data.frame(Selection=1:length(unique(test$WSCSDANAME[!is.na(test$WSCSDANAME)])), Region=unique(test$WSCSDANAME[!is.na(test$WSCSDANAME)]))
    print("Select the geographic region below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    region.select <- regions$Region[regions$Selection==region.select]
    test <- test[test$WSCSDANAME==region.select,]
    
    #now to watersheds
    regions <- data.frame(Selection=1:length(unique(test$WSCSSDANAM[!is.na(test$WSCSSDANAM)])), Region=unique(test$WSCSSDANAM[!is.na(test$WSCSSDANAM)]))
    print("Select the river below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    region.select <- regions$Region[regions$Selection==region.select]
    test <- test[test$WSCSSDANAM==region.select,]
    
  }
  else{
    test <- test[test$WSCSSDANAM==watershed,]
  }
  
  ############## now to download
  
  if (is.null(directory)){
    if(dir.exists(region.select)){
      setwd(region.select)
      where <- getwd()
    }
    else {print(paste("Creating a new directory", region.select, "in", getwd()))
    dir.create(region.select)
    setwd(region.select)
    to.download <- paste("https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/shp_en/", unique(test$WSCMDA[!is.na(test$WSCMDA)]), "/nhn_rhn_", tolower(as.character(unique(test$DATASETNAM[!is.na(test$DATASETNAM)]))), "_shp_en.zip", sep = "")
    
    temp2 <- tempfile()
    print(paste("Downloading .zip file for", region.select, "from", to.download))
    download.file(to.download, temp2)
    print(paste("Unpacking zip file..."))
    unzip(zipfile=temp2)
    print(paste("Shapefiles available at", getwd()))
   where <- getwd()}
  }
  else{
    setwd(directory)
    to.download <- paste("https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/shp_en/", unique(test$WSCMDA[!is.na(test$WSCMDA)]), "/nhn_rhn_", unique(test$DATASETNAM[!is.na(test$DATASETNAM)]), "_shp_en.zip", sep = "")
    
    temp2 <- tempfile()
    print(paste("Downloading .zip file for", region.select, "from", to.download))
    download.file(to.download, temp2, quiet=quiet)
    print(paste("Unpacking zip file..."))
    unzip(zipfile=temp2, exdir=region.select)
    print(paste("Shapefiles available at", getwd()))
   
  }
  setwd(original.dir)
  if (flush == T) {unlink(c("temp_zip", "canada_zip"), recursive = TRUE)}
  return(list(output.path = where, name = region.select))
}
  