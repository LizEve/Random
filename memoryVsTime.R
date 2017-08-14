library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(plyr)
# See https://github.com/jeetsukumaran/Syrupy for info about variables. 
# RSS and VSIZE are in kb
# VSIZe - RSS = swap 
getwd()
# Take elasped time and turn into total seconds elapsed. Add a column on with this data. 
add_elapsed_seconds <- function(data_frame){
  time_list <- integer()
  time_list2 <- numeric()
  for (i in data_frame$ELAPSED){
    j <- unlist(strsplit(i, ":"))
    if (length(j)==2) {
      time_sec <- as.integer(j[1])*60+as.integer(j[2])
      time_list <- c(time_list, as.integer(time_sec))
      time_min <- time_sec/60
      time_list2 <- c(time_list2, as.numeric(time_min))
    } else if (length(j)==3){
      time_sec <- as.integer(j[1])*60*60+as.integer(j[2])*60+as.integer(j[3])
      time_list <- c(time_list, as.integer(time_sec))
      time_min <- time_sec/60
      time_list2 <- c(time_list2, as.numeric(time_min))
    } else {
      print("lalalalapoopypants")
    }
  }
  data_frame$TIMESEC <- time_list
  data_frame$TIMEMIN <- time_list2
  return(data_frame)
}

# Take 3 datasets and prep to be combined. 
single_dataset_prep <- function(data1,data2,data3,nTrees) {
  #Combine multiple runs
  datak <- rbind.fill(data1,data2,data3)
  # Add columns with minutes and seconds
  datak <- add_elapsed_seconds(datak) 
  # Add column with total number of trees per cluster as discrete variable
  datak$NUMTREES <- rep(as.character(nTrees*2), nrow(datak))
  # Subset potentially. Not implemented right now.
  subset_by <- 1
  dk <- datak[seq(1,nrow(datak),subset_by),]
  return(dk)
}

# Take 2 datasets and prep to be combined. 
single_dataset_prep2 <- function(data1,data2,nTrees) {
  #Combine multiple runs
  datak <- rbind.fill(data1,data2)
  # Add columns with minutes and seconds
  datak <- add_elapsed_seconds(datak) 
  # Add column with total number of trees per cluster as discrete variable
  datak$NUMTREES <- rep(as.character(nTrees*2), nrow(datak))
  # Subset potentially. Not implemented right now.
  subset_by <- 1
  dk <- datak[seq(1,nrow(datak),subset_by),]
  return(dk)
}

setwd("/Users/ChatNoir/Projects/TreeScaper/numTrees/memoryTest/memoryInfo")

# Affinity runs Import Data
data75k3<- read.csv("10tip_750trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data1k3 <- read.csv("10tip_1000trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data2k3 <- read.csv("10tip_2000trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data5k3 <- read.csv("10tip_5000trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data10k3 <- read.csv("10tip_10000trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data20k3 <- read.csv("10tip_20000trees_0.125_1.0start_3_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data75k2<- read.csv("10tip_750trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data1k2 <- read.csv("10tip_1000trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data2k2 <- read.csv("10tip_2000trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data5k2 <- read.csv("10tip_5000trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data10k2 <- read.csv("10tip_10000trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data20k2 <- read.csv("10tip_20000trees_0.125_1.0start_2_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data75k1<- read.csv("10tip_750trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data1k1 <- read.csv("10tip_1000trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data2k1 <- read.csv("10tip_2000trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data5k1 <- read.csv("10tip_5000trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data10k1 <- read.csv("10tip_10000trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
data20k1 <- read.csv("10tip_20000trees_0.125_1.0start_1_cloud.ps.log", stringsAsFactors = FALSE, header=T)
# 4 and 5 didn't finish completely. 
#data5k4 <- read.csv("10tip_5000trees_0.125_1.0start_4_cloud.ps.log", stringsAsFactors = FALSE, header=T)
#data5k5 <- read.csv("10tip_5000trees_0.125_1.0start_5_cloud.ps.log", stringsAsFactors = FALSE, header=T)

# Covariance runs Import Data
##Does not exist for some reason
##data75k3<- read.csv("10tip_750trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data1k3 <- read.csv("10tip_1000trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data2k3 <- read.csv("10tip_2000trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data5k3 <- read.csv("10tip_5000trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data10k3 <- read.csv("10tip_10000trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data20k3 <- read.csv("10tip_20000trees_0.125_1.0start_3_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data75k2<- read.csv("10tip_750trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data1k2 <- read.csv("10tip_1000trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data2k2 <- read.csv("10tip_2000trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data5k2 <- read.csv("10tip_5000trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data10k2 <- read.csv("10tip_10000trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data20k2 <- read.csv("10tip_20000trees_0.125_1.0start_2_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data75k1<- read.csv("10tip_750trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data1k1 <- read.csv("10tip_1000trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data2k1 <- read.csv("10tip_2000trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data5k1 <- read.csv("10tip_5000trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data10k1 <- read.csv("10tip_10000trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)
#data20k1 <- read.csv("10tip_20000trees_0.125_1.0start_1_cloudCOV.ps.log", stringsAsFactors = FALSE, header=T)

#### Set up data
#d5kb <- single_dataset_prep2(data5k4, data5k5, 5000)
#d75k <- single_dataset_prep2(data75k1,data75k2,750)
d75k <- single_dataset_prep(data75k1,data75k2,data75k3,750)
d1k <- single_dataset_prep(data1k1,data1k2,data1k3,1000)
d2k <- single_dataset_prep(data2k1,data2k2,data2k3,2000)
d5k <- single_dataset_prep(data5k1,data5k2,data5k3,5000)
d10k <- single_dataset_prep(data10k1,data10k2,data10k3,10000)
d20k <- single_dataset_prep(data20k1,data20k2,data20k3,20000)

#Combine dataframes. Fill in extra space.
dall <- rbind.fill(d75k,d1k,d2k,d5k,d10k,d20k)
dsmall <- rbind.fill(d75k,d1k,d2k,d5k)

#Set conversion from kb to gb
kb2gb = 1000000

#Total memory in Gb-----------------------------------
affinity_network_graph <- function(data_frame){
  p1 <- ggplot(data=data_frame, mapping = aes(x = TIMEMIN, y = VSIZE/kb2gb, color=NUMTREES)) 
  # data
  p1 <- p1 + geom_point(data=data_frame, aes(TIMEMIN, VSIZE/kb2gb), cex=2)
  # axis
  p1 <- p1 + scale_y_continuous(name="Total memory in Gb", breaks=seq(0,150,5)) +
    scale_x_continuous(name="Time in minutes", breaks=seq(0,540,20))
  # theme
  p1 <- p1 + theme_minimal() 
  # label
  p1 <- p1 + labs(color = "Total Trees")
  # colors
  p1 <- p1 + scale_color_brewer(palette="Dark2")
  # title
  p1 <- p1 + ggtitle("Auto community detection: affinity network") +
    theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
  p1
}
affinity_network_graph(dall)
affinity_network_graph(dsmall)


covariance_network_graph <- function(data_frame){
  p1 <- ggplot(data=data_frame, mapping = aes(x = TIMEMIN, y = VSIZE/kb2gb, color=NUMTREES)) 
  # data
  p1 <- p1 + geom_point(data=data_frame, aes(TIMEMIN, VSIZE/kb2gb))
  # axis
  p1 <- p1 + scale_y_continuous(name="Total memory in Gb", breaks=seq(0,1,0.1)) +
    scale_x_continuous(name="Time in minutes", breaks=seq(0,5,0.5))
  # theme
  p1 <- p1 + theme_minimal() 
  # label
  p1 <- p1 + labs(color = "Total Trees")
  # colors
  p1 <- p1 + scale_color_brewer(palette="Dark2")
  # title
  p1 <- p1 + ggtitle("Auto community detection: Covariance network") +
    theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
  p1
}
covariance_network_graph(dall)
covariance_network_graph(dsmall)



###Depreciated code-------------------------
#
##Combine multiple runs
#data75k <- rbind.fill(data75k1,data75k2,data75k3)
#data1k <- rbind.fill(data1k1,data1k2,data1k3)
#data2k <- rbind.fill(data2k1,data2k2,data2k3)
#data5k <- rbind.fill(data5k1,data5k2,data5k3)
#data10k <- rbind.fill(data10k1,data10k2,data10k3)
#data20k <- rbind.fill(data20k1,data20k2,data20k3)
#
## Add columns with minutes and seconds
#data75k <- add_elapsed_seconds(data75k) 
#data1k <- add_elapsed_seconds(data1k) 
#data2k <- add_elapsed_seconds(data2k) 
#data5k <- add_elapsed_seconds(data5k) 
#data10k <- add_elapsed_seconds(data10k) 
#data20k <- add_elapsed_seconds(data20k) 
#
## Add column with total number of trees per cluster as discrete variables. 
## Use "as.integer" for continuous values
#data75k$NUMTREES <- rep(as.character(750*2), nrow(data75k))
#data1k$NUMTREES <- rep(as.character(1000*2), nrow(data1k))
#data2k$NUMTREES <- rep(as.character(2000*2), nrow(data2k))
#data5k$NUMTREES <- rep(as.character(5000*2), nrow(data5k))
#data10k$NUMTREES <- rep(as.character(10000*2), nrow(data10k))
#data20k$NUMTREES <- rep(as.character(20000*2), nrow(data20k))
#
## subset all to 40 data points
##d2k <- data2k3[seq(1,nrow(data2k3),1),]
##d5k <- data5k3[seq(1,nrow(data5k3),4.4),]
##d10k <- data10k3[seq(1,nrow(data10k3),6.5),]
##d20k <- data20k3[seq(1,nrow(data20k3),6.6),]
#
##Rename instead of subsetting
#d75k <- data75k
#d1k <- data1k
#d2k <- data2k
#d5k <- data5k
#d10k <- data10k
#d20k <- data20k
#
##Rename columns before combining dataframes
#colnames(d2k) <- paste("d2k", colnames(d2k), sep = "_")
#colnames(d5k) <- paste("d5k", colnames(d5k), sep = "_")
#colnames(d10k) <- paste("d10k", colnames(d10k), sep = "_")
#colnames(d20k) <- paste("d20k", colnames(d20k), sep = "_")
#dall <- cbind(d2k,d5k,d10k,d20k)
#
##Total memory in Gb-----------------------------------
#cols <- c()
#basic <- ggplot(data=dall, mapping = aes(x = d20k_TIMEMIN, y = d20k_VSIZE/kb2gb)) +
#  labs(x="Time in minutes",y = "VSIZE in Gb") +
#  scale_y_continuous(name="Total memory in Gb", breaks=seq(0,110,10)) +
#  scale_x_continuous(name="Time in minutes", breaks=seq(0,140,10))
#p1 <- basic + geom_point(data=dall, aes(d2k_TIMEMIN, d2k_VSIZE/kb2gb),color="darkgreen")
#p1 <- p1 + geom_point(data=dall, aes(d5k_TIMEMIN, d5k_VSIZE/kb2gb),color="OliveDrab")
#p1 <- p1 + geom_point(data=dall, aes(d10k_TIMEMIN, d10k_VSIZE/kb2gb),color="Orange3")
#p1 <- p1 + geom_point(data=dall, aes(d20k_TIMEMIN, d20k_VSIZE/kb2gb),color="red3")