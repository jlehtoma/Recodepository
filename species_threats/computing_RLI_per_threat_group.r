### computing the Red List Index per threat category


path <- "I:/PhD/Collaborations/RedList"



##########################################################
## RLI function
##########################################################

RLI <- function(vector) {1-(sum(vector) / (5*length(vector)))}



##########################################################
## loading data
##########################################################

setwd(path)
spth <- read.table("species_threats_database.txt", header=T, sep="\t")

threatlist <- unique(spth[,2]) #list of threat categories

#table to store results
rli_per_threat <- matrix(NA, nrow=length(threatlist), ncol=2)
colnames(rli_per_threat) <- c("RLI2000back","RLI2010")
rownames(rli_per_threat) <- threatlist



##########################################################
### RLI per threat category 
##########################################################

for (i in 1:length(threatlist)) {
    rli_per_threat[i,1] <- RLI(vector=spth[which(spth[,2]==threatlist[i]),4])
    rli_per_threat[i,2] <- RLI(vector=spth[which(spth[,2]==threatlist[i]),5])
    
}

setwd(path)
write.table(rli_per_threat, "RLI_per_threat.txt", row.names=T, sep="\t")



# barplot
setwd(path)
png("RLI_per_threat.png", width=1000, height=300)

layout(matrix(c(1:2),1,2,byrow=T),widths=c(30,4),heights=c(7,7),T)

par(mar=c(1,1,1,1))
par(oma=c(2,2,2,2))

bp <- barplot(t(rli_per_threat), beside=T, ylab="", xlab="", ylim=c(0,1), col=c("black", "darkgrey"))

mtext("RLI per threat", side=3, line=2)
mtext("threat category", side=1, line=3)
mtext("RLI", side=2, line=2)

# add number of species per threat group
numbers <- rep(NA, length(threatlist))
for (t in 1:length(threatlist))
{
  numbers[t] <- length(which(spth[,2]==threatlist[t]))
}


for (b in 1:length(bp))
{
  mtext(numbers[b], at=mean(bp[1,b],bp[2,b]), side=1, line=2)
}


# legend
plot(0,0,type="n", ann=F, axes=F)
legend("left", legend=c("2000", "2010"), col=c("black", "darkgrey"), fill=c("black", "darkgrey"), ncol=1)

dev.off()





##########################################################
### RLI per primary threat category 
##########################################################


spth1 <- spth[which(spth[,3]==1),]


for (i in 1:length(threatlist)) {
    rli_per_threat[i,1] <- RLI(vector=spth[which(spth1[,2]==threatlist[i]),4])
    rli_per_threat[i,2] <- RLI(vector=spth[which(spth1[,2]==threatlist[i]),5])
}


setwd(path)
write.table(rli_per_threat, "RLI_per_primary_threat.txt", row.names=T, sep="\t")


# barplot
setwd(path)
png("RLI_per_primary_threat.png", width=1000, height=300)

layout(matrix(c(1:2),1,2,byrow=T),widths=c(30,4),heights=c(7,7),T)

par(mar=c(1,1,1,1))
par(oma=c(2,2,2,2))

bp <- barplot(t(rli_per_threat), beside=T, ylab="", xlab="", ylim=c(0,1), col=c("black", "darkgrey"))

mtext("RLI per primary threat", side=3, line=2)
mtext("threat category", side=1, line=3)
mtext("RLI", side=2, line=2)

# add number of species per threat group
numbers <- rep(NA, length(threatlist))
for (t in 1:length(threatlist))
{
  numbers[t] <- length(which(spth1[,2]==threatlist[t]))
}


for (b in 1:length(bp))
{
  mtext(numbers[b], at=mean(bp[1,b],bp[2,b]), side=1, line=2)
}


# legend
plot(0,0,type="n", ann=F, axes=F)
legend("left", legend=c("2000", "2010"), col=c("black", "darkgrey"), fill=c("black", "darkgrey"), ncol=1)

dev.off()

