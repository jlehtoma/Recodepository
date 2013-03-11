source("R/postgresql.R")

# Install dependencies if not already installed
install.deps()

# For examples, see https://code.google.com/p/rpostgresql/
# NOTE!!! For security reasons config.R file is not included in this repository
# Ask Joona to send it via email

# Establish a connection
con <- connect.rli("R/config.R")

# Fetch all the data
habitat <-  fetch.rli.data(con, table="habitat")
habitat.threat <- fetch.rli.data(con, table="habitat_threat")
imp <-  fetch.rli.data(con, table="implementation")
prog.targets <- fetch.rli.data(con, table="programme_targets")
progs <-  fetch.rli.data(con, table="programmes")
threat <-  fetch.rli.data(con, table="threat")

# Example: how to combine programme codes with descriptions?
imp <- merge(imp, progs)
# Get overall euros and hectares per programme
imp.by.prog <- ddply(imp, c("ProgrammeID", "LongDesc"), summarise, total_euros = sum(Euros), 
                     total_ha = sum(Hectares))