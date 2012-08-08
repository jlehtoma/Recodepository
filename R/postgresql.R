library("RPostgreSQL")

# For examples, see https://code.google.com/p/rpostgresql/

connect.rli <- function(config.file="config.R") {
  
  source(config.file)
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname=DBNAME, user=USER, password=PASSWORD, 
                   host=HOST, port=PORT)
  return(con)
}

fetch.rli.data <- function(what="*", table, rows) {
  # dbSendQuery(con, statement, ...) submits one statement to the database. Eg.
  rs <- dbSendQuery(con, paste("select", what, "from", table))
  # fetch all elements from the result set
  return(fetch(rs, n=rows))
}

# Establish a connection
con <- connect.rli()
# Fetch all the data (rows = -1) from table aves
data <- fetch.rli.data(table="aves", rows=-1)

# Create a new table from a R data frame
table.name <- "testtable"
test.data <- data.frame(ID=c(1,2,3), var1=c("foo", "bar", "spam"), 
                        var2=c(TRUE, FALSE, TRUE))

if (dbExistsTable(con, table.name)) {
  
  if (readline("Table exists, drop the old table? [y/N] > ") %in% c("y", "Y")) {
    print("Dropping old table")
    dbRemoveTable(con, table.name)
    
  } else {
    message("Aborting.")
  }
}
dbWriteTable(con, table.name, test.data)

## Closes the connection
dbDisconnect(con)