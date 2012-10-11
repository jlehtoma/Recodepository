# Author: Joona Lehtomäki <joona.lehtomaki@gmail.com>
# Updated: 23.09.2011
# Version: 0.1

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

if (!require("plyr")) {
  install.packages("plyr")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

if (!require("XLConnect")) {
  install.packages("XLConnect")
}

if (.Platform$OS.type == "unix") {
  setwd("/home/jlehtoma/Dropbox/DAta/RedList/")
} else {
  # Fill in if on Windows
  dsn <- ""
}

source("parser.R")

# Input data as csv, comma-separated

# Fields
# [1] group  
# [2] scientific_name	
# [3] scientific_syn	
# [4] finnish_name	
# [5] finnish_syn	
# [6] swedish_name	
# [7] category2010	
# [8] criteria	
# [9] habitat	
# [10] threatenedess	
# [11] threats	
# [12] category2000	
# [13] reason	
# [14] mystery -> Ehd. u/e, what is this

data <- read.csv('data/redlist2010.csv', header=TRUE, as.is=TRUE, sep="\t")

# Codes for various RedList variables, read directly from data/codes.xls

# Fields
# [1] abbreviation  
# [2] category name
categories <- readWorksheetFromFile("data/codes.xls", sheet = "Categories", 
                                    header = TRUE)

# Fields
# [1] abbreviation  
# [2] habitat name
# [3] group number (helper code)
habitats <- readWorksheetFromFile("data/codes.xls", sheet = "Habitats", 
                                  header = TRUE)

# Fields
# [1] abbreviation  
# [2] threat name
threats <- readWorksheetFromFile("data/codes.xls", sheet = "CausesOfThreat", 
                                 header = TRUE)

# Fields
# [1] abbreviation  
# [2] reason name
change <- readWorksheetFromFile("data/codes.xls", sheet = "ReasonForChange", 
                                 header = TRUE)

## DATA FORMATTING #############################################################

# Break the habitats, threatenedness, threats and reason into sequences
data$habitat  <- strsplit(data$habitat, ",")
data$threatenedess <- strsplit(data$threatenedess, ",")
data$threats  <- strsplit(data$threats, ",")
data$reason  <- strsplit(data$reason, ",")

# Get threatened species (CR, EN, VU)
data.threat <- subset(data, category2010 %in% c('CR', 'EN', 'VU'))

# Get all criteria for the threatened species
data.threat$parsed.criteria <- vector(mode="list", 
                                      length=length(data.threat$criteria))
for (i in 1:length(data.threat$criteria)) {
  data.threat$parsed.criteria[[i]] <- parse.criteria(data.threat[i,]$criteria)
}

# Calculate criteria counts per group (n=28)
threat.counts <- ddply(data.threat, c('group'), criteria.count)
threat.counts.molten <- melt(threat.counts, id=c('group', 'x'))
# Pivot the counts
threat.counts.casted <- dcast(threat.counts, x~group, value='freq')
row.names(threat.counts.casted) <- threat.counts.casted$x
threat.counts.matrix <- data.matrix(threat.counts.casted[2:length(threat.counts.casted)])

# Get just the birds
data.birds <- subset(data, group == "AV")

## PLOTTING ####################################################################

# CRITERIA

red_colors <- c("#ffd3cd", "#ffc4bc", "#ffb5ab", "#ffa69a", "#ff9789", "#ff8978", 
                "#ff7a67", "#ff6b56", "#ff5c45", "#ff4d34")
threat_heatmap <- heatmap(threat.counts.matrix, Rowv=NA, Colv=NA, 
                          col = red_colors, margins=c(5,10))

# Count criteria-classes and sort
parsed.criteria <- as.data.frame(count(unlist(data.threat$parsed.criteria)))
threat.counts$group <- factor(threat.counts$group)
c <- ggplot(threat.counts, aes(x=reorder(factor(x), freq), y=freq)) 
c + geom_bar(stat="identity") + labs(y="Frequency", x="Criteria") + coord_flip() +
    facet_wrap(~group)

# THREATS

# Birds: get the primary habitat for each bird
data.birds$pr.habitat <- sapply(data.birds$habitat, function(x) x[1])
data.birds$pr.habitat.group <- merge(data.birds, habitats, by.x="pr.habitat",
                                    by.y="abbr")
data.birds$pr.threat <- sapply(data.birds$threats, function(x) x[1])
count.habitats <- count(data.birds$pr.habitat)


c <- ggplot(count.habitats, aes(x=reorder(factor(x), freq), y=freq))
c + geom_bar(stat="identity") + labs(y="Frequency", x="Habitat") + coord_flip()