source("R/postgresql.R")

# Install dependencies if not already installed
install.deps()

# For examples, see https://code.google.com/p/rpostgresql/
# NOTE!!! For security reasons config.R file is not included in this repository
# Ask Joona to send it via email

# Establish a connection
con <- connect.rli("R/config.R")


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

upload(con, test.data, "test_data", overwrite=TRUE)

## Closes the connection
dbDisconnect(con)