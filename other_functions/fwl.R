fwl <- function(
    licence = NULL,
    year = NULL,
    data = catch,
    convert_kg_to_count = FALSE
) {
  
  if (convert_kg_to_count) {
    
    data |>
      filter(LICENCE_ID == licence & YEAR == year) |>
      select(LICENCE_ID, MONTH, DAY, FV_DATE_FISHED, RIVERNAME_CLEANED, COUNTY, PROVINCE, FV_GEAR_CODE, GEAR_DESCRIPTION, FV_HOURS_FISHED, FV_WEIGHT, MEASUREMENT_UNIT) |>
      arrange(FV_DATE_FISHED) |> 
      mutate(FV_WEIGHT = FV_WEIGHT / 0.24) -> out
    
  } else {
    
    data |>
      filter(LICENCE_ID == licence & YEAR == year) |>
      select(LICENCE_ID, MONTH, DAY, FV_DATE_FISHED, RIVERNAME_CLEANED, COUNTY, PROVINCE, FV_GEAR_CODE, GEAR_DESCRIPTION, FV_HOURS_FISHED, FV_WEIGHT, MEASUREMENT_UNIT) |>
      arrange(FV_DATE_FISHED) -> out
    
  }
  
  return(out)

}