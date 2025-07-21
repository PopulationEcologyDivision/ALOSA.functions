# The catch dataframe should probably be converted to kilograms first using
# convert.KGS(). This function will work without doing that, but the max_diff
# you can use to filter the differences between catches won't make as much sense
# since you won't be sure what the units are.

# data = the catch dataframe from MARFIS
# max_percent = how much of a percentage increase you are interested in
# max_diff = the max difference you want to see between years

changes_in_catch <- function(
    data,
    max_percent = NULL,
    max_diff = NULL
    ) {
  
  data |> 
    group_by(LICENCE_ID, YEAR) |> 
    summarise(
      TOTAL_WEIGHT = sum(FV_WEIGHT),
      RIVERNAME_CLEANED = first(RIVERNAME_CLEANED),
      .groups = "drop") |> 
    arrange(LICENCE_ID, YEAR) |> 
    group_by(LICENCE_ID) |> 
    mutate(
      PREV_WEIGHT = lag(TOTAL_WEIGHT),
      DIFF_WEIGHT = round(TOTAL_WEIGHT - PREV_WEIGHT),
      PERCENT_CHANGE = round((TOTAL_WEIGHT - PREV_WEIGHT) / PREV_WEIGHT * 100),
      TOTAL_WEIGHT = round(TOTAL_WEIGHT),
      PREV_WEIGHT = round(PREV_WEIGHT)
    ) |> 
    select(YEAR, RIVERNAME_CLEANED, LICENCE_ID, TOTAL_WEIGHT, PREV_WEIGHT, DIFF_WEIGHT, PERCENT_CHANGE) -> catch_changes
  
  if (is.null(max_percent)) {
    return(catch_changes)
  } else {
    catch_changes |> 
      filter(abs(PERCENT_CHANGE) <= max_percent) -> catch_changes
  }
  
  if (is.null(max_diff)) {
    return(catch_changes)
  } else {
    catch_changes |> 
      filter(abs(DIFF_WEIGHT) <= max_diff) -> catch_changes
  }
  
  return(catch_changes)
  
}