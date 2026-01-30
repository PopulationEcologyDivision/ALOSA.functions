# This is a simple function to quickly see a table of reported catches or DNF
# for certain areas for certain years. It assumes you have access to the catch
# object that gets returned when we do a MARFIS pull
# 
# Note: this does no QA/QC of catch data, so if you want that you need
# to do it before using this function.
# 
# 

licences_reporting <- function(
    catch = NULL,
    year = NULL,
    river = NULL
    ) {
  
  require(tidyverse)
  
  if (missing(catch)) {
    stop("You must provide a catch dataframe", call. = FALSE)
  }
  
  if (missing(year)) {
    stop("You must provide years for which you want comparisons.\n
         If you want multiple rivers, provide them in a vector\n
         e.g. c(2024, 2025)", call. = FALSE)
  }
  
  if (missing(river)) {
    stop("You must provide a river (or rivers) to summarise.\n
         If you want multiple rivers, provide them in a vector\n
         e.g. c('tusket', 'annis')", call. = FALSE)
  }
  
  river <- toupper(river)
  
  catch |> 
    filter(str_detect(RIVERNAME_CLEANED, str_c(river, collapse = "|"))) |> 
    filter(YEAR %in% year) |> 
    group_by(YEAR, RIVERNAME_CLEANED) |> 
    distinct(LICENCE_ID) |> 
    count() |> 
    pivot_wider(names_from = YEAR, values_from = n, values_fill = NA)
  
}