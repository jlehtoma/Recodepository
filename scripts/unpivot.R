library(reshape)
# Generate some toy data
df.kemera <- data.frame("Habitat_RL"=c("Mrf", "Mrg", "Ik", "Ij"),
                        "Lehto"=c(0, 1, 0, 0),
                        "Paahde"=c(1, 0, 0, 1),
                        "Lähde"=c(0, 0, 1, 0),
                        "Letto"=c(1, 0, 0, 0))

df.natura <- data.frame("Habitat_RL"=c("Mrf", "Mrg", "Ik", "Ij"),
                        "Heinävarpu"=c(0, 0, 1, 1),
                        "Luhta"=c(1, 0, 0, 0),
                        "Varvikko"=c(0, 1, 1, 0),
                        "Jänkä"=c(0, 0, 1, 0))

mdf.kemera <- melt(df.kemera, id=c("Habitat_RL"))
# Select only the columns holding the corresponding habitat names
mdf.kemera <- mdf.kemera[which(mdf.kemera$value == 1),][,1:2]
colnames(mdf.kemera) <- c("HabitatRL", "HabitatKemera")

mdf.natura <- melt(df.natura, id=c("Habitat_RL"))
mdf.natura <- mdf.natura[which(mdf.natura$value == 1),][,1:2]
colnames(mdf.natura) <- c("HabitatRL", "HabitatNatura")

# Merge the 2 together
habitat.map <- merge(mdf.kemera, mdf.natura)
