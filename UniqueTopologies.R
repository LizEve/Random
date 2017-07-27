library(phytools)
library(ape)


setwd("/Users/ChatNoir/Projects/TreeScaper/TreeScaper_wrappers_2017")
file1 <- "10tip_100trees_0.125_1.0start_9_cloud.nex"
file1 <- "10tip_200trees_0.5_0.5start_1_cloud.nex"
file1 <- "10tip_200trees_0.5_0.5start_2_cloud.nex"
file1 <- "10tip_200trees_0.5_1.0start_2_cloud.nex"
tree1 <- read.nexus(file=file1)
unique(tree1)
