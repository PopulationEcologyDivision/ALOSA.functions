# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function is used to standardise column names that we commonly use. 
# Some of our files have month, day, and year in all caps and some use the
# shortened versions of the words (e.g. MON instead of month). This function
# takes the column names, makes them all lower case, and looks up shortened 
# versions of the column name in a table and replaces them with an appropriate
# name
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


standardise_column_names <- function(data){
  
  # We need janitor for clean_names() to get everything to lower case
  library(dplyr)
  library(janitor)
  
  # This is a named character vector where the function we use in rename_with()
  # looks for column names to change to whatever value you describe 
  # (e.g. "mon" becomes "month")
  name_map <- c(
    mon = "month", month = "month", mth = "month", mo = "month",
    day = "day", dy = "day", 
    year = "year", yr = "year",
    sp = "species", spec = "species"
  )
  
  # The clean_names function should handle most of the other problems we 
  # run into (e.g. upper case). It will make the column names lower case
  # with underscores and removes spaces or symbols (e.g. a ".")
  data |> 
    clean_names() |> 
    rename_with(
      # If the column is not in the name map, it will leave it alone after it
      # is cleaned (i.e. it won't rename it beyond cleaning it up)
      function(name) {
        mapped <- name_map[name]
        if_else(!is.na(mapped), mapped, name)
      },
      .cols = everything()
    )
  
}