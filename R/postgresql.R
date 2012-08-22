install.deps <- function() {
  if (!require("pgUtils")) {
    source("http://bioconductor.org/biocLite.R")
    biocLite("pgUtils")
    library("pgUtils")
  }
  
  if (!require("RPostgreSQL")) {
    install.packages("RPostgreSQL")
    library("RPostgreSQL")
  }
  
}

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


