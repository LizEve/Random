library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyr)

####Scatter Plots#####
scatter_graph <- function(data,x,y,xlab,ylab,xtix,ytix,title){ 
  p <- ggplot(data, aes(x, y)) 
  # theme and colors
  p <- p + theme_minimal() 
  p <- p + geom_point(color="darkorchid4")  
  # trend line
  p <- p + geom_smooth(method=lm, formula = y ~ x, color="yellow2", se=FALSE) 
  # axis
  p <- p + scale_x_continuous(name=xlab, breaks = round(seq(0, max(x),by = xtix)))
  p <- p + scale_y_continuous(name=ylab, breaks = round(seq(0, max(y),by = ytix))) 
  # title
  p <- p + ggtitle(title) +
    theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
  p
}


#Import data
setwd("/Users/ChatNoir/Projects/TreeScaper/outFiles_graphs/xxx")
data <- read.csv("numTrees_Aug4.csv", stringsAsFactors = FALSE, header=T)

# Split into affinity and covariance subsets
aff <- data[data$Network=="Affinity",]
cov <- data[data$Network=="Covariance",]

### Apply scatter plots
title <- "Time vs Trees, Affinity"
data <- aff
x <- data$Trees
y <- data$TimeTotal/60
xlab <- "Number of Trees"
ylab <- "Total time, minutes"
xtix <- 500
ytix <- 10
scatter_graph(data,x,y,xlab,ylab,xtix,ytix,title)