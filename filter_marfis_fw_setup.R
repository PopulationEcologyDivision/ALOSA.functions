#' filter_marfis_fw helps find fw data in marfis.  Initially, it extracts a number 
#' of huge tables from the db.  Initial extraction takes a while (don't do it
#' over the VPN).  But once extracted to `fw.data.dir`. the filtering process is 
#' pretty fast.
#' 
#' This setup script extracts and or loads the data locally, but also saves all 
#' of these huge objects to a single object called `bkup`.  This is because once 
#' a filter has been applied, the local objects get replaced with filtered 
#' versions of those same objects.  Instead of the lengthy re-loading process, 
#' we can just do `list2env(bkup, envir = globalenv())` to return all of the 
#' objects to their unfiltered state.
#' 
#' By default, the data will filter for only Gaspereau records using the species 
#' code (350) and sum_doc_defn_id of 11 (but these could ceivably be changed to 
#' any freshwater species).
#' 
#' The script optionally permits passing a number of variables (which otherwise 
#' should be NULL).  Passing different values will filter all of the MARFIS
#' data in the environment to be filtered to leave only the records that match
#' the parameter (e.g. you could extract all of the records for a given licence
#' and/or year and/or gear_code, etc).  
#' 
#' The result is that instead of searching through 100s of 1000s of records, you 
#' can quickly limit the data to vastly smaller subsets.  

CXN <- ROracle::dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, "PTRAN")
fw.data.dir <- "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/Individuals/BillardM/fwData"

YEARS  <- NULL
LICENCE <- NULL
SUM_DOC_ID <- NULL
SD_LOG_ID  <- NULL
DOC_SERIAL  <- NULL
GEAR_CODE <- NULL

if (F){
  YEARS  <- c(2021:2025)
  LICENCE <- 120310
  SUM_DOC_ID <- 530691
  SD_LOG_ID  <- c(3153745, 3152877:3152886)
  DOC_SERIAL  <- 10489
  GEAR_CODE <- 41
}

if (!exists("bkup")){
  fwTbls <-c("COMMUNITIES",
             "COUNTIES",
             "DISTRICTS",
             "GEARS",
             "LICENCE_PARTICIPANTS",
             "MARFLEETS_LIC",
             "PARTICIPANTS",
             "SD_LOG_EFF_STD_INFO",
             "SD_LOG_ENTRD_DETS",
             "SD_LOG_SPC_STD_INFO",
             "SD_LOGS",
             "SUM_DOCS",
             "VR_FRESHWATER")
  
  Mar.utils::get_data_tables(cxn = CXN, schema = "MARFISSCI", data.dir = fw.data.dir, tables = fwTbls, quietly = FALSE, fuzzyMatch=FALSE)
  bkup <- lapply(fwTbls, function(x) get(x))
  names(bkup) <- fwTbls
}else{
  list2env(bkup, envir = globalenv())
}

filter_marfis_fw(years = YEARS, spp = 350, sum_doc_defn_id = 11, 
                 licences = LICENCE, 
                 sum_doc_id = SUM_DOC_ID, 
                 doc_serial_num = DOC_SERIAL, 
                 sd_log_id = SD_LOG_ID, 
                 gear_code= GEAR_CODE)