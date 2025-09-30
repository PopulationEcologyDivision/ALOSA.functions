# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# fwl = Fresh Water Logbook
# 
# This function takes licence and year info and uses it to haul pertinent data
# from the catch (or other) dataframe produced when we do a MARFIS pull. I used
# to just use a snippet, but I often want to convert weights to compare with the
# log, so I got tired of adding a mutate() to the snippet.
# 
# ~ LG
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# licence = licence number in question
# 
# year = year of interest (defaults to current year)
# 
# data = catch dataframe from MARFIS pull (can supply your own edited df too)
# 
# convert_kg_to_count = converts entries that have been converted to kilograms 
# by the DMC into count so it is easier to compare with the original log entry
# when doing data QC
# 
# conversion = the factor used to convert kilograms of fish to counts of fish,
# typically we use 0.24 fish / kilogram, but you can supply a different value
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fwl <- function(
    licence = NULL,
    year = lubridate::year(Sys.Date()),
    data = catch,
    convert_kg_to_count = FALSE,
    conversion = 0.24
) {
  
  # conversion_flag ####
  if (convert_kg_to_count) {
    
    data |>
      filter(LICENCE_ID == licence & YEAR == year) |>
      select(LICENCE_ID, MONTH, DAY, FV_DATE_FISHED, RIVERNAME_CLEANED, COUNTY, PROVINCE, FV_GEAR_CODE, GEAR_DESCRIPTION, FV_HOURS_FISHED, FV_WEIGHT, MEASUREMENT_UNIT) |>
      arrange(FV_DATE_FISHED) |> 
      mutate(FV_WEIGHT = FV_WEIGHT / conversion) -> out
    
  } else {
    
    data |>
      filter(LICENCE_ID == licence & YEAR == year) |>
      select(LICENCE_ID, MONTH, DAY, FV_DATE_FISHED, RIVERNAME_CLEANED, COUNTY, PROVINCE, FV_GEAR_CODE, GEAR_DESCRIPTION, FV_HOURS_FISHED, FV_WEIGHT, MEASUREMENT_UNIT) |>
      arrange(FV_DATE_FISHED) -> out
    
  }
  
  # output ####
  return(out)

}