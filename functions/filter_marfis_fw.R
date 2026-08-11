#' Filter MARFIS freshwater data tables based on specified criteria
#'
#' This function filters various MARFIS freshwater data tables based on user-specified criteria.
#' It iteratively applies filters until no more rows are removed from the tables.
#' 
#' For it to work, the following tables MUST exist in your Global environment:  
#' COMMUNITIES, COUNTIES, DISTRICTS, GEARS, LICENCE_PARTICIPANTS,
#'   MARFLEETS_LIC, PARTICIPANTS, SD_LOG_EFF_STD_INFO, SD_LOG_ENTRD_DETS,
#'   SD_LOG_SPC_STD_INFO, SD_LOGS, SUM_DOCS, VR_FRESHWATER
#'   
#' Please use filter_marfis_fw_setup.R if you need help loading them
#'
#' @param years A vector of years to filter by
#' @param spp A vector of species codes to filter by
#' @param sum_doc_defn_id A vector of sum document definition IDs to filter by
#' @param licences A vector of licence IDs to filter by
#' @param sum_doc_id A vector of sum document IDs to filter by
#' @param doc_serial_num A vector of document serial numbers to filter by
#' @param sd_log_id A vector of survey data log IDs to filter by
#' @param gear_code A vector of gear codes to filter by
#'
#' @return Assigns filtered data frames to the global environment:
#'   COMMUNITIES, COUNTIES, DISTRICTS, GEARS, LICENCE_PARTICIPANTS,
#'   MARFLEETS_LIC, PARTICIPANTS, SD_LOG_EFF_STD_INFO, SD_LOG_ENTRD_DETS,
#'   SD_LOG_SPC_STD_INFO, SD_LOGS, SUM_DOCS, VR_FRESHWATER
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @examples
#' filter_marfis_fw(fwTbls = fwTbls, years = c(2020, 2021, 2022),
#'                   spp = 350, sum_doc_defn_id = 11,
#'                   licences = NULL, sum_doc_id = NULL,
#'                   doc_serial_num = NULL, sd_log_id = NULL,
#'                   gear_code = NULL)
#'
#' @note The function uses a while loop to iteratively apply filters until
#'   no more rows are removed from the tables.
filter_marfis_fw <- function(years = NULL,  
                             spp = NULL, 
                             sum_doc_defn_id = NULL, 
                             licences = NULL, 
                             sum_doc_id = NULL, 
                             doc_serial_num = NULL, 
                             sd_log_id = NULL, 
                             gear_code = NULL){
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
  
  message("filtering")
  LOOPAGAIN = T
  while (LOOPAGAIN){
    tblsPre <- lapply(fwTbls, get, envir=.GlobalEnv)
    precnt = sum(sapply(tblsPre, NROW))
    if (!is.null(sum_doc_defn_id)) SUM_DOCS <- subset(SUM_DOCS, SUM_DOC_DEFN_ID %in% sum_doc_defn_id)
    if (!is.null(sum_doc_id))      SUM_DOCS <- subset(SUM_DOCS, SUM_DOC_ID %in% sum_doc_id)
    if (!is.null(licences)){
      MARFLEETS_LIC        <- subset(MARFLEETS_LIC, LICENCE_ID %in% licences)
      VR_FRESHWATER        <- subset(VR_FRESHWATER, LICENCE_ID %in% licences)
      SUM_DOCS             <- subset(SUM_DOCS, LICENCE_ID %in% licences)
      LICENCE_PARTICIPANTS <- subset(LICENCE_PARTICIPANTS, LICENCE_ID %in% licences)
    }
    
    if (!is.null(spp)){
      MARFLEETS_LIC        <- subset(MARFLEETS_LIC, SPECIES_CODE %in% spp) #IGNORED SUBTYPE -99
      # don't filter VR_FRESHWATER by spp - sometimes records show code 0 if none caught; should be filtered by SUM_DOC_ID
    }
    
    if (!is.null(years)){ 
      #the following ensure that the licences are actually valid for the specified time period
      MARFLEETS_LIC        <- subset(MARFLEETS_LIC, lubridate::year(L_ORIGIN_DATE) <= min(years) & lubridate::year(L_EXPIRY_DATE)>= max(years))
      MARFLEETS_LIC        <- subset(MARFLEETS_LIC, lubridate::year(LG_START_DATE) <= min(years) & lubridate::year(LG_END_DATE)>= max(years))
      LICENCE_PARTICIPANTS <- subset(LICENCE_PARTICIPANTS, lubridate::year(START_DATE) <=  min(years) & lubridate::year(END_DATE) >= max(years))
      SUM_DOCS             <- subset(SUM_DOCS, lubridate::year(START_DATE) >= min(years) & lubridate::year(END_DATE) <= max(years))
      # don't filter VR_FRESHWATER by date_fished - sometimes records show code 0 if none caught; should be filtered by SUM_DOC_ID
    }
    
    if (!is.null(doc_serial_num)){ 
      SUM_DOCS <- SUM_DOCS[SUM_DOCS$DOC_SERIAL_NUM %in% doc_serial_num,]
    }
    
    if (!is.null(sd_log_id)){ 
      SD_LOGS              <- subset(SD_LOGS, SD_LOG_ID %in% sd_log_id)
    }
    
    if (!is.null(gear_code)){ 
      SD_LOG_EFF_STD_INFO <- subset(SD_LOG_EFF_STD_INFO, FV_GEAR_CODE %in% gear_code)
    }
    SD_LOGS              <- subset(SD_LOGS, SUM_DOC_ID %in% SUM_DOCS$SUM_DOC_ID)
    # filter by SD_LOG_ID
    VR_FRESHWATER        <- subset(VR_FRESHWATER, SD_LOG_ID %in% SD_LOGS$SD_LOG_ID)
    SD_LOG_SPC_STD_INFO  <- subset(SD_LOG_SPC_STD_INFO ,  SD_LOG_ID %in% SD_LOGS$SD_LOG_ID)
    SD_LOG_ENTRD_DETS    <- subset(SD_LOG_ENTRD_DETS, SD_LOG_ID %in% SD_LOGS$SD_LOG_ID)
    SD_LOG_EFF_STD_INFO  <- subset(SD_LOG_EFF_STD_INFO, SD_LOG_ID %in% SD_LOGS$SD_LOG_ID)
    
    PARTICIPANTS         <- subset(PARTICIPANTS, FIN %in% LICENCE_PARTICIPANTS$FIN)
    COMMUNITIES          <- subset(COMMUNITIES, COMMUNITY_CODE %in% PARTICIPANTS$COMMUNITY_CODE)
    DISTRICTS            <- subset(DISTRICTS, DISTRICT_ID %in% COMMUNITIES$DISTRICT_ID)
    COUNTIES             <- subset(COUNTIES, COUNTY_ID %in% DISTRICTS$COUNTY_ID)
    GEARS                <- subset(GEARS, GEAR_CODE %in% MARFLEETS_LIC$GEAR_CODE)
    
    # # filter by SD_LOG_EFF_STD_INFO_ID                                   
    SD_LOG_ENTRD_DETS    <- subset(SD_LOG_ENTRD_DETS, SD_LOG_EFF_STD_INFO_ID %in% SD_LOG_EFF_STD_INFO$SD_LOG_EFF_STD_INFO_ID)
    SD_LOG_SPC_STD_INFO  <- subset(SD_LOG_SPC_STD_INFO ,  SD_LOG_EFF_STD_INFO_ID %in% SD_LOG_EFF_STD_INFO$SD_LOG_EFF_STD_INFO_ID)
    
    #re-filter some earlier objects
    PARTICIPANTS <- subset(PARTICIPANTS, FIN %in% SUM_DOCS$FIN)
    LICENCE_PARTICIPANTS <- subset(LICENCE_PARTICIPANTS, FIN %in% PARTICIPANTS$FIN)
    MARFLEETS_LIC        <- subset(MARFLEETS_LIC, LICENCE_ID %in% LICENCE_PARTICIPANTS$LICENCE_ID)
    COMMUNITIES          <- subset(COMMUNITIES, COMMUNITY_CODE %in% SUM_DOCS$COMMUNITY_CODE)
    
    tblsPost <- lapply(fwTbls, get, envir=.GlobalEnv)
    postcnt =  sum(sapply(tblsPost, NROW))
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    }else{
      message("... and filtering")
    }
  }
  
  assign("COMMUNITIES", COMMUNITIES, envir = .GlobalEnv)
  assign("COUNTIES", COUNTIES, envir = .GlobalEnv)
  assign("DISTRICTS", DISTRICTS, envir = .GlobalEnv)
  assign("GEARS", GEARS, envir = .GlobalEnv)
  assign("LICENCE_PARTICIPANTS", LICENCE_PARTICIPANTS, envir = .GlobalEnv)     
  assign("MARFLEETS_LIC", MARFLEETS_LIC, envir = .GlobalEnv)
  assign("PARTICIPANTS", PARTICIPANTS, envir = .GlobalEnv)
  assign("SD_LOG_EFF_STD_INFO", SD_LOG_EFF_STD_INFO, envir = .GlobalEnv)
  assign("SD_LOG_ENTRD_DETS", SD_LOG_ENTRD_DETS, envir = .GlobalEnv)
  assign("SD_LOG_SPC_STD_INFO", SD_LOG_SPC_STD_INFO, envir = .GlobalEnv)
  assign("SD_LOGS", SD_LOGS, envir = .GlobalEnv)
  assign("SUM_DOCS", SUM_DOCS, envir = .GlobalEnv)
  assign("VR_FRESHWATER", VR_FRESHWATER, envir = .GlobalEnv)
  
}    

