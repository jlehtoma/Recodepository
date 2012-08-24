if (!require("XLConnect")){
  install.packages("XLConnect")
}

if (!require("reshape")){
  install.packages("reshape")
}

if (!require("ggplot2")){
  install.packages("ggplot2")
}

if (!require("RColorBrewer")){
  install.packages("RColorBrewer")
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
rlh.desc.habitat  <- rlh.data.threats[c(1:3)]
# 1.3 simplify the habitat threats table by ditching long descriptions
rlh.data.threats <- rlh.data.threats[c(1:2, 4:length(rlh.data.threats))]
rlh.mdata.threats  <- melt(rlh.data.threats, c("ID", "Habitat_RL"), 
                           variable_name="Threat", na.rm=TRUE)
# 1.4 Get rid off "."s in Threat codes
rlh.mdata.threats$Threat <- gsub("\\.", "", rlh.mdata.threats$Threat)

# 1.5 Habitat_RL is redundant
rlh.mdata.threats <- rlh.mdata.threats[c(1, 3:4)]
# 1.6 fix field names
colnames(rlh.mdata.threats) <- c("ID", "Threat", "Code")
# 1.7 Done! Upload data into database
upload(con, rlh.desc.habitat, "rlh_desc_habitat", overwrite=TRUE)
upload(con, rlh.mdata.threats, "rlh_data_threat", overwrite=TRUE)


# 2. Habitat descriptions -------------------------------------------------

# 2.1 Load the habitat descriptions
rlh.desc.threat  <- readWorksheet(rlh.wb.threats, 
                                   sheet = "Definition of Threats (Finnish)")
# 2.2 Update the field names
colnames(rlh.desc.threat) <- c("Threat", "Definition")
# 2.3 Done! Upload data into database
upload(con, rlh.desc.threat, "rlh_desc_threat", overwrite=TRUE)

# 3. Conservation programmes and habitats ---------------------------------

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
rls.data.progs <- rls.data.progs[,c(1:2, 4:length(rls.data.progs))]
rls.mdata.progs  <- melt(rls.data.progs, c("ID", "Habitat_RL"), 
                           variable_name="Programme", na.rm=TRUE)
# 3.5 Done! Upload data into database
upload(con, rls.mdata.progs, "rls_data_progs", overwrite=TRUE)

# 4. Conservation programmes  and hectares (ha) --------------------------

# 4.1 Programme implementation in hectares
rls.file.progs.ha <- file.path(data.folder, 
                               "summary_of_hectares.xlsx")

rls.wb.progs.ha <- loadWorkbook(rls.file.progs.ha)
# 4.2 Load the actual data
rls.data.progs.ha <- readWorksheet.disjoint(rls.wb.progs.ha, 
                                            sheet = "Cons_ha",
                                            region = "C1:R17")

# 4.3 Fill in the column names with right years
colnames(rls.data.progs.ha) <- c("Programme", as.character(1996:2010))
# 4.4 Get rid of some of the insignificant rows and replace NAs with 0s
rls.data.progs.ha <- subset(rls.data.progs.ha, 
                            !is.na(rls.data.progs.ha$Programme))
rls.data.progs.ha[is.na(rls.data.progs.ha)] <- 0

# 4.5 Melt the data for plotting
rls.mdata.progs.ha <- melt(rls.data.progs.ha, c("Programme"), 
                           variable_name="Year")

# 4.6 Upload data into database
upload(con, rls.mdata.progs.ha, "rls_data_progs_ha", overwrite=TRUE)

# 4.7 Rename "value" to "Hectares"
colnames(rls.mdata.progs.ha) <- c(colnames(rls.mdata.progs.ha)[1:2], "Hectares")

a <- ggplot(rls.mdata.progs.ha, aes(x = Year, y = Hectares, fill = Programme)) + 
     opts(title = "Conservation allocation by programme") +
     labs(x = "Year", y = "Hectares", fill = NULL)
b <- a + geom_bar(stat = "identity", position = "stack") + 
     scale_fill_brewer(palette = "Paired")
  
c <- b + facet_grid(Programme ~ .) + opts(legend.position = "none")

rls.mdata.progs.ha.total <- cast(rls.mdata.progs.ha, Year ~ ., sum)
rls.mdata.progs.ha.total <- rename(rls.mdata.progs.ha.total, 
                                   c(`(all)` = "Hectares"))
rls.mdata.progs.ha.total$Programme <- "Total"
df.m.t <- rbind(rls.mdata.progs.ha.total, rls.mdata.progs.ha)
c1 <- c %+% df.m.t
c2 <- c1 + facet_grid(Programme ~ ., scale = "free_y")
c2