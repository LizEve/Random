library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyr)
setwd("/Users/ChatNoir/Projects/TreeScaper/outFiles_graphs/numTips")

#Import data
data <- read.csv("numTipsold.csv", stringsAsFactors = FALSE, header=T)
aff <- data[data$Network=="Affinity",]
cov <- data[data$Network=="Covariance",]


###### Differe way to arrange data #####
data <- cov
data <- aff
# Arrange data differently
# count number of times each option in numcoms occurs for each numtips 
fdf <- count(data, c("Tips", "NumberCommunities")) 
# sub out 2 and NA for true/false. 
fdf$NumberCommunities <- gsub("2","True",fdf$NumberCommunities)
fdf$NumberCommunities[is.na(fdf$NumberCommunities)] <- "False"
# add in NA for 0 counts counts
fdf <- fdf %>% complete(Tips, NumberCommunities)
fdf$freq[is.na(fdf$freq)] <- 0
# sort by tip number
#fdf <- arrange(fdf,Tips)


ggplot(fdf, aes(factor(Tips), freq, fill = NumberCommunities)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired")

# Covariance. sometimes outputs incorrect # that is still within max community limit set by user. 
fdf <- count(data, c("Tips", "NumberCommunities")) 
# sub out 2 and other #s for true/false. Could do this much more neatly.
fdf$NumberCommunities <- gsub("2","True",fdf$NumberCommunities)
fdf$NumberCommunities <- gsub("3","False",fdf$NumberCommunities)
fdf$NumberCommunities <- gsub("4","False",fdf$NumberCommunities)
fdf$NumberCommunities <- gsub("5","False",fdf$NumberCommunities)
fdf$NumberCommunities <- gsub("6","False",fdf$NumberCommunities)
fdf$NumberCommunities <- gsub("7","False",fdf$NumberCommunities)
# add in NA for 0 counts counts
fdf <- fdf %>% complete(Tips, NumberCommunities)
fdf$freq[is.na(fdf$freq)] <- 0
# sort by tip number
#fdf <- arrange(fdf,Tips)


