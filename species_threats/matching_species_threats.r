## matching species, threats and Red List category
## each species has as many rows as the threats indicated on the Red List for it

library("plyr")

## load data
sptable <- read.table("data/species_threats.txt", header=T, sep="\t")
sptable <- sptable[,1:7]
sptable <- sptable[1:237,]

spth <- matrix(NA, ncol=5, nrow=1)
colnames(spth) <- c("name","threat","order","RL2000back","RL2010")

for (t in 1:4) {
  tvector <- which(sptable[,t+3]!="")
  
  tmatrix <- matrix(NA, nrow=length(tvector), ncol=5)
  tmatrix[,1] <- as.character(sptable[tvector,1]) #name of species

  tmatrix[,2] <- as.character(sptable[tvector,t+3]) #threat code
  
  tmatrix[,3] <- t #order number of threat
  
  tmatrix[,4] <- as.numeric(sptable[tvector,3]) #RL 2000
  tmatrix[,5] <- as.numeric(sptable[tvector,2]) #RL 2010
  
  spth <- rbind(spth, tmatrix) 
}
  
# Remove the first NA row and convert to a dataframe
spth <- as.data.frame(spth[-1,])

# Sort the table according to name and threat order. This fits better the 
# database table design
spth <- arrange(spth, name, order)

setwd(path)
write.table(spth, "species_threats_database.txt", row.names=F, sep="\t")

