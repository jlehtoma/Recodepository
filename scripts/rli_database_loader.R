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
# and rename the columns
rlh.desc.habitat  <- rlh.data.threats[c(1:3)]
colnames(rlh.desc.habitat) <- c("HabitatID", "ShortDesc", "LongDesc")

# 1.3. Get rid of white spaces in ShortDesc
rlh.desc.habitat$ShortDesc <- gsub(" ", "", rlh.desc.habitat$ShortDesc)

# 1.3 simplify the habitat threats table by ditching long descriptions
rlh.data.threats <- rlh.data.threats[c(1:2, 4:length(rlh.data.threats))]
rlh.mdata.threats  <- melt(rlh.data.threats, c("ID", "Habitat_RL"), 
                           variable_name="Threat", na.rm=TRUE)

# 1.4 Get rid off "."s in Threat codes
rlh.mdata.threats$Threat <- gsub("\\.", "", rlh.mdata.threats$Threat)

# 1.5 Habitat_RL is redundant
rlh.mdata.threats <- rlh.mdata.threats[c(1, 3:4)]
# 1.6 fix field names
colnames(rlh.mdata.threats) <- c("HabitatID", "ThreatID", "PresentIn")
# 1.7 Done! Upload data into database
upload(con, rlh.desc.habitat, "Habitat", overwrite=TRUE)
upload(con, rlh.mdata.threats, "HabitatThreat", overwrite=TRUE)


# 2. Habitat descriptions -------------------------------------------------

# 2.1 Load the habitat descriptions
rlh.desc.threat  <- readWorksheet(rlh.wb.threats, 
                                   sheet = "Definition of Threats (Finnish)")

# 2.2 Update the field names
colnames(rlh.desc.threat) <- c("ShortDesc", "LongDesc")

# 2.3 Create an ID column
rlh.desc.threat <- data.frame(ThreatID=1:nrow(rlh.desc.threat), rlh.desc.threat)

# 2.4 Done! Upload data into database
upload(con, rlh.desc.threat, "Threat", overwrite=TRUE)

# 3. Conservation programmes and habitats ---------------------------------

## 3 How funding sources are linked to the habitat classes in the spp RL

rls.file.progs <- file.path(data.folder, 
                            "Habitats_RL and Cons_Prog 0427.xlsx")

rls.wb.progs <- loadWorkbook(rls.file.progs)
# 3.1 Load the actual data
rls.data.progs <- readWorksheet.disjoint(rls.wb.progs, 
                                           sheet = "RL_hab&Cons.prog.",
                                           region = "A1:L26;A30:L77;A82:L82")

# 3.2 First, a little detour. A table defining all the unique conservation
# programmes needs to be defined. The the unique names.d
cons.progs.names <- colnames(rls.data.progs)[3:ncol(rls.data.progs)]

# 3.3. Generate long descriptions for the programmes (this should probably
# be done in the Excel file?)

cons.progs.longnames <- c("Etelä-Suomen metsien monimuotoisuusohjelma",
                         "VN:n päätökset kansallispuistoista",
                         "Rantojensuojeluohjelma",
                         "Lintuvesien suojeluohjelma",
                         "Turvemaiden suojeluohjelma",
                         "Lehtojen suojeuohjelma",
                         "Vanhojen metsien suojeluohjelma",
                         "Lajiensuojeluohjelma",
                         "Maatalouden ympäristötuet",
                         "Kestävän metsätalouden rahoituslaki")

# 3.4 Create a new data frame fo
rls.desc.progs <- data.frame(ProgrammeID=1:length(cons.progs.names),
                             ShortDesc=cons.progs.names,
                             LongDesc=cons.progs.longnames)

# 3.5 Back to the main story, add a numerical identifier ID
rls.data.progs <- data.frame(HabitatID=1:nrow(rls.data.progs), rls.data.progs)

# 3.6  The ID should match with that of rlh.desc.threat, check
if (!all(rls.data.progs$Habitat_RL == rlh.desc.habitat$ShortDesc)) {
  stop("Habitat categories in rls.data.progs and rlh.desc.habitat do not match")
}

# 3.7 separate only the first 2 columns (also the habitat definition table)
rls.desc.hab  <- rls.data.progs[c(1:3)]

# 3.8 simplify the habitat threats table by ditching long descriptions
rls.data.progs <- rls.data.progs[,c(1:2, 4:length(rls.data.progs))]
rls.mdata.progs  <- melt(rls.data.progs[,c(1, 3:length(rls.data.progs))], 
                         c("HabitatID"), variable_name="ProgrammeID", 
                         na.rm=TRUE)

# 3.9 Done! Upload data into database
upload(con, rls.desc.progs, "Programmes", overwrite=TRUE)
upload(con, rls.mdata.progs, "ProgrammeTargets", overwrite=TRUE)

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

# 4.5 Re-order the data frame based on the programme name, INVERTLY! This is
# because of quirks in how ggplot2 plots these...
rls.data.progs.ha <- rls.data.progs.ha[with(rls.data.progs.ha, 
                                            order(Programme, decreasing=TRUE)), ]

# 4.6 Instead of the programme name (as string), use the ProgrammeID (int) from
# rls.data.progs
rls.data.progs.ha.ids <- merge(rls.desc.progs, rls.data.progs.ha, by.x="ShortDesc", 
                               by.y="Programme")
rls.data.progs.ha.ids <- rls.data.progs.ha.ids[,c(2, 4:ncol(rls.data.progs.ha.ids))]

# 4.7 Melt the data for storing...
rls.mdata.progs <- melt(rls.data.progs.ha.ids, c("ProgrammeID"), 
                        variable_name="Year")

# 4.8 ... and for plotting
rls.mdata.progs.ha <- melt(rls.data.progs.ha, c("Programme"), 
                           variable_name="Year")

# 4.9 Rename "value" to "Hectares"
colnames(rls.mdata.progs) <- c(colnames(rls.mdata.progs)[1:2], "Hectares")
colnames(rls.mdata.progs.ha) <- c(colnames(rls.mdata.progs.ha)[1:2], "Hectares")

# 4.10 Upload data into database
upload(con, rls.mdata.progs, "Implementation", overwrite=TRUE)

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