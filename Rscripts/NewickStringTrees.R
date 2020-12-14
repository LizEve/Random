library(ape)
library(phytools)
library(geiger)
library(phangorn)

start1 <- "((taxon5,taxon2),((taxon1,taxon3),taxon4));"
start2 <- "(taxon5,(((taxon2,taxon3),taxon1),taxon4));"
start3 <- "((((taxon2,taxon3),taxon1),taxon4),taxon5);"
tree1 <- read.newick(text=start1)
tree2 <- read.newick(text=start2)
tree3 <- read.newick(text=start3)
plot.phylo(tree3)

phy <- c(tree1,tree2,tree3)
# give unrooted
multiRF(phy)
# gives rooted
RF <- RF.dist(tree1, tree2, rooted=TRUE)
tips <- 5
RF_max <- 2*(tips-2)
RF_norm <- (RF)/RF_max
RF
RF_max
RF_norm


pyron <- "(Outgroup,(Gekkota,(Dibamidae,(Scincoidea,((Lacertoidea,(Teioidea,Amphisbaenia)),(Iguania,(Serpentes,Anguimorph)))))));"
phyP <- read.newick(text=pyron)
plot.phylo(phyP)

# 9 families
weins <- "(Outgroup,((Gekkota,Dibamidae),(Scincoidea,((Teioidea,(Lacertoidea,Amphisbaenia)),(Serpentes,(Iguania, Anguimorph))))));"
phyW <- read.newick(text=weins)
plot.phylo(phyW)

# 7 families
gauthier <- "(Outgroup,(Iguania,(Gekkota,(Scincoidea,(Anguimorph,(Serpentes,(Dibamidae,Amphisbaenia)))))));"
phyG <- read.newick(text=gauthier)
plot.phylo(phyG)

# 7 families
singhal <- "(Outgroup,(Gekkota,(Scincoidea,((Amphisbaenia,(Teioidea,Gymnothalmidae)),(Iguania,Serpentes)))));"
phyS <- read.newick(text=singhal)
plot.phylo(phyS)



