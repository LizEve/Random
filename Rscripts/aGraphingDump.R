library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(scales)
title <- "Covariance"
data <- nuData[nuData$ComOrder==1,]
x <- as.factor(data$Trees)
y <- data$NumCom
size <- data$ComLen
color <- as.factor(data$ComOrder)
xlab <- "Number of Trees"
ylab <- "Number Communities"
xtix <- 5000
ytix <- 1

p <- ggplot(data, aes(x, y))
# theme and colors
p <- p + theme_minimal() 
#p <- p + geom_count(aes(color=color, size=..prop.., group=Trees), shape=1)
p <- p + geom_count(aes(color=color, size=..prop.., group=Trees), alpha = 8/10)
#p <- p + geom_point(aes(size=size))
#p <- p + geom_point(aes(color=color))
#p <- p + geom_point(aes(size=size,color=color))
p <- p + scale_size_area()
p <- p + scale_color_brewer(palette = "Dark2")
#p <- p + scale_radius()
#p <- p + scale_size(range = c(0, 5))
p
# axis
p <- p + scale_x_continuous(name=xlab, breaks = round(seq(0, max(x),by = xtix)))
p <- p + scale_y_continuous(name=ylab, breaks = round(seq(0, max(y),by = ytix))) 
# title
p <- p + ggtitle(title) +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
p

####Bar Plots#####
barPlot <- function(nuData,x,f,legendTitle, titleTitle){
  # Get columns for two variables, x axis and fill color
  v1 <- grep(x, colnames(nuData))
  v2 <- grep(f, colnames(nuData))
  # add in NA for 0 counts counts, replace NA with 0
  nuData <- nuData %>% complete(nuData[v1], nuData[v2])
  nuData$freq[is.na(nuData$freq)] <- 0
  ##### Run this plot
  ggplot(nuData, aes(factor(Trees), freq, fill = nuData[v2])) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Dark2") + 
    guides(fill=guide_legend(title=legendTitle)) + 
    ggtitle(titleTitle) +
    theme(plot.title = element_text(hjust = 0.5, size=20))
}




