library(ape)
library(phytools)
# set variables
wdir <- "/Users/zach/Dropbox/projects/taxonomy-2017/trees"
tfile <- "mytree.tre"
pdfname <- "mytree.pdf"
tree <- ladderize(tree)
# read and plot
pdf(pdfname,width=6,height=11)
plotTree(tree, fsize=0.8, ftype="bi",lwd=2)
dev.off()

# BOOTSTRAP VALUES ON TREE
#==============================================================================
# Subsetting: white circles for low support
nodes <- 1:tree$Nnode+Ntip(tree)
bs <- as.numeric(tree$node.label)
# get bs values
bs90.bs <- subset(bs, bs < 90)
bs100.bs <- subset(bs, bs > 90)
# get node numbers
bs90 <- which(bs < 90)+Ntip(tree)
bs100 <- which(bs > 90)+Ntip(tree)
# plot
plotTree(tree, fsize=0.8, ftype="bi",lwd=3)
nodelabels(node=bs100, pch=21, cex=1.0, bg="black")
nodelabels(node=bs90,text=bs90.bs, frame="circle", bg = "white", cex=0.5)

# write to pdf
pdf(pdfname, width = 6, height = 11)
plotTree(tree, fsize=0.8, ftype="bi",lwd=3, edge.color="black")
if(!length(bs100)==0){nodelabels(node=bs100, pch=21, cex=1.0, bg="black")}
if(!length(bs90)==0){nodelabels(node=bs90,text=bs90.bs, frame="circle", bg = "white", cex=0.3)}
dev.off()