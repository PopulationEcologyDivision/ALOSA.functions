## Setting up ROracle. 
## Follow the instructions in 
#





writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
install.packages("jsonlite", type = "source") 
require(devtools)
require(pkgbuild)

devtools::find_rtools() 


require("ROracle") 


ora.use="GASPEREA" 
ora.pass="_REMOVED"
ora.base="PTRAN"
channel=dbConnect(DBI::dbDriver("Oracle"), ora.use, ora.pass, ora.base, believeNRows=FALSE) 



#NOTE: In the above example, the connection parameters were defined in an R profile document as follows: 
  
ora.use="GASPEREA" 
  
ora.pass="_REMOVED"
  
ora.base="PTRAN"


dbWriteTable(connection, 'IRIS', 
             iris[sample(nrow(iris),10,replace = T),], 
             overwrite = TRUE, row.names = FALSE,col.names = FALSE)

dbWriteTable(conn = channel, schema=target_schema, value = source_df,
             name = target_table, date=TRUE,row.names = FALSE, overwrite = FALSE, append = TRUE)