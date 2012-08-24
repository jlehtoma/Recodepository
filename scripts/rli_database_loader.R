if (!require("XLConnect")){
  install.packages("XLConnect")
}

if (!require("reshape")){
  install.packages("reshape")
}

if (!require("ggplot2")){
  install.packages("ggplot2")
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


# 1. Threatened habitats ---------------------------------------------------

# Threats on various habitats (according to LuTU), originally compiled by
# Daniel ("Habitats_RL and Habitat_threats 0428.xlsx")

rlh.file.threats <- file.path(data.folder, 
                              "Habitats_RL and Habitat_threats 0428.xlsx")

rlh.wb.threats <- loadWorkbook(rlh.file.threats)
# 1. Load the actual data
rlh.data.threats <- readWorksheet.disjoint(rlh.wb.threats, 
                                           sheet = "Habitat threats",
                                           region = "A1:Z26;A30:Z77;A82:Z82")

# 1.1 Add a numerical identifier ID
rlh.data.threats <- data.frame(ID=1:nrow(rlh.data.threats), rlh.data.threats)

# 1.2 separate only the first 2 columns (also the habitat definition table)
desc.hab  <- rlh.data.threats[c(1:3)]
# 1.3 simplify the habitat threats table by ditching long descriptions
rlh.data.threats <- rlh.data.threats[c(1:2, 4:length(rlh.data.threats))]
rlh.mdata.threats  <- melt(rlh.data.threats, c("ID", "Habitat_RL"), 
                           variable_name="Threat", na.rm=TRUE)
# 1.4 Habitat_RL is redundant
rlh.mdata.threats <- rlh.mdata.threats[c(1, 3:4)]
# 1.5 fix field names
colnames(rlh.mdata.threats) <- c("ID", "Threat", "Code")


# 2. Habitat descriptions -------------------------------------------------

# 2. Load the habitat descriptions
desc.hab.threats  <- readWorksheet(rlh.wb.threats, 
                                   sheet = "Definition of Threats (Finnish)")
# 2.1 Update the field names
colnames(desc.hab.threats) <- c("Threat", "Definition")


# 3. Conservation programmes -----------------------------------------------

## 3 How funding sources are linked to the habitat classes in the spp RL

rls.file.progs <- file.path(data.folder, 
                            "Habitats_RL and Cons_Prog 0427.xlsx")

rls.wb.progs <- loadWorkbook(rls.file.progs)
# 3.1 Load the actual data
rls.data.progs <- readWorksheet.disjoint(rls.wb.progs, 
                                           sheet = "RL_hab&Cons.prog.",
                                           region = "A1:N226;A30:N77;A82:N82")
# 3.2 Add a numerical identifier ID
rls.data.progs <- data.frame(ID=1:nrow(rls.data.progs), rls.data.progs)

# 3.3 separate only the first 2 columns (also the habitat definition table)
rls.desc.hab  <- rls.data.progs[c(1:3)]
# 3.4 simplify the habitat threats table by ditching long descriptions
rls.data.progs <- rls.data.progs[c(1:2, 4:length(rls.data.progs))]
rls.mdata.progs  <- melt(rls.data.progs, c("ID", "Habitat_RL"), 
                           variable_name="Programme", na.rm=TRUE)
# 3.5 Habitat_RL is redundant
rlh.mdata.threats <- rlh.mdata.threats[c(1, 3:4)]
# 3.6 fix field names
colnames(rlh.mdata.threats) <- c("ID", "Threat", "Code")


# 4. Conservation programmes (ha) -----------------------------------------

# 4 Programme implementation in hectares
rls.file.progs.ha <- file.path(data.folder, 
                               "summary_of_hectares.xlsx")

rls.wb.progs.ha <- loadWorkbook(rls.file.progs.ha)
# 4.1 Load the actual data
rls.data.progs.ha <- readWorksheet.disjoint(rls.wb.progs.ha, 
                                            sheet = "Cons_ha",
                                            region = "C1:R17")

# 4.2 Fill in the column names with right years
colnames(rls.data.progs.ha) <- c("cons_name", as.character(1996:2010))
# 4.3 Get rid of some of the insignificant rows and replace NAs with 0s
rls.data.progs.ha <- subset(rls.data.progs.ha, 
                            !is.na(rls.data.progs.ha$cons_name))
rls.data.progs.ha[is.na(rls.data.progs.ha)] <- 0

# 4.4 Melt the data for plotting
rls.mdata.progs.ha <- melt(rls.data.progs.ha, c("cons_name"), 
                           variable_name="Year")

a <- ggplot(rls.mdata.progs.ha, aes(x = Year, y = value, fill = cons_name)) + 
     opts(title = "Conservation allocation by programme") +
     labs(x = "Year", y = "Hectares", fill = NULL)
a + geom_bar(stat = "identity", position = "stack")
