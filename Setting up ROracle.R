## Setting up ROracle. 
## Follow the instructions in 
#





writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
install.packages("jsonlite", type = "source") 
require(devtools)
require(pkgbuild)

devtools::find_rtools() 


require("ROracle") 



channel=dbConnect(DBI::dbDriver("Oracle"), oracle.username.GASP, oracle.password.GASP, "PTRAN" , believeNRows=FALSE) 



dbWriteTable(connection, 'IRIS', 
             iris[sample(nrow(iris),10,replace = T),], 
             overwrite = TRUE, row.names = FALSE,col.names = FALSE)

dbWriteTable(conn = channel, schema=target_schema, value = source_df,
             name = target_table, date=TRUE,row.names = FALSE, overwrite = FALSE, append = TRUE)