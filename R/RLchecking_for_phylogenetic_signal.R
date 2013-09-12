######################################################
######### checking for phylogenetic signal ###########
######################################################


# path <- "G:/PhD/Collaborations/RedList"
path <- "C:/HY-Data/BURGASRI/Dropbox/Recodepository/data/Birds"
setwd(path)

require(geiger)


### loading RL species data ###
RL <- read.table("RL237sp_120524_plain.txt", header=T)
RL <- RL[7:dim(RL)[1],]
spplist <- as.character(RL$NameSci)
rownames(RL) <- spplist



### loading phylogenetic dissimilarity matrix ###
phy <- read.table("dist_mat_best_tree_original.txt", header=T)


### changing names to match nomenclature in our dataset ###
# Change species names for 12 species to same spelling than in the RL data 
colnames(phy)[c(275,379,380,55,290,154,69,222,31,269,373,325)] <- c("Actitishypoleuca",          
    "Anaspenelope","Anasstrepera","Buboscandiaca","Calidrisalpinaalpina",      
    "Carduelischloris","Corvuscoronecornix","Delichonurbicum","Dendrocoposleucotos",       
    "Larusfuscus","Mergusalbellus","Phalacrocoraxcarbosinensis") 
# 7 species with no phylogenetic data:
# "Alcatorda",        "Uriaaalge"     "Calidrisalpinaschinzii",  "Cepphusgrylle" 
# "Loxialeucoptera", "Sylvianisoria",  "Tarsigercyanurus",  
phynames <- colnames(phy)
phynames <- gsub("_", "", phynames)
colnames(phy) <- phynames
rownames(phy) <- phynames
dim(phy)




### matching species names ###
common <- intersect(spplist,phynames)
phy <- phy[common,common]
RL <- RL[common,]

### converting matrix to dist object ###
phydist <- as.dist(phy)


### converting matrix to tree ###
tree <- hclust(phydist)


### converting to phylo object ###
treephylo <- as.phylo(tree)


### ordering data frame by tree tip names ###
tips <- treephylo$tip.label
RLordertips <- RL[match(tips, RL$NameSci),] 


### selecting traits to check for signal ###
colnames(RLordertips)
traits <- as.data.frame(RLordertips[,20])
rownames(traits) <- tips  
traits[,1] <- as.numeric(traits[,1])


### model fitting ###
physig <- fitContinuous(treephylo, traits, model="lambda")
print(names(physig))
physig$opt


fitDiscrete(treephylo, traits, treeTransform="lambda")
