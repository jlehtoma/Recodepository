if (!require("XLConnect")){
  install.packages("XLConnect")
}

if (!require("reshape")){
  install.packages("reshape")
}

source("R/postgresql.R")
source("R/utilities.R")

# Install dependencies if not already installed
install.deps()

# For examples, see https://code.google.com/p/rpostgresql/
# NOTE!!! For security reasons config.R file is not included in this repository
# Ask Joona to send it via email

# Establish a connection
con <- connect.rli("R/config.R")

# Set up the data folder
data.folder <- ("data")

# Threats on various habitats (according to LuTU), originally compiled by
# Daniel ("Habitats_RL and Habitat_threats 0428.xlsx")

file.hab.threats <- file.path(data.folder, 
                              "Habitats_RL and Habitat_threats 0428.xlsx")

wb.hab.threats <- loadWorkbook(file.hab.threats)
# 1. Load the actual data
data.hab.threats <- readWorksheet.disjoint(wb.hab.threats, 
                                           sheet = "Habitat threats",
                                           region = "A1:Z26;A30:Z77;A82:Z82")

# 1.1 Add a numerical identifier ID
data.hab.threats <- data.frame(ID=1:nrow(data.hab.threats), data.hab.threats)

# 1.2 separate only the first 2 columns (also the habitat definition table)
desc.hab  <- data.hab.threats[c(1:3)]
# 1.3 simplify the habitat threats table by dithing long descriptions
data.hab.threats <- data.hab.threats[c(1:2, 4:length(data.hab.threats))]
mdata.hab.threats  <- melt(data.hab.threats, c("ID", "Habitat_RL"), 
                           variable_name="Threat", na.rm=TRUE)
# 1.4 Habitat_RL is redundant
mdata.hab.threats <- mdata.hab.threats[c(1, 3:4)]
# 1.5 fix field names
colnames(mdata.hab.threats) <- c("ID", "Threat", "Code")



# 2. Load the habitat descriptions
desc.hab.threats  <- readWorksheet(wb.hab.threats, 
                                   sheet = "Definition of Threats (Finnish)")
# 2.1 Update the field names
colnames(desc.hab.threats) <- c("Threat", "Definition")


