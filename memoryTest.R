library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(plyr)
# See https://github.com/jeetsukumaran/Syrupy for info about variables. 
# RSS and VSIZE are in kb
# VSIZe - RSS = swap 

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




setwd("/Users/ChatNoir/Projects/TreeScaper/numTrees/memoryTest")

# Affinity runs
data2k3 <- read.csv("10tip_2000trees_0.125_1.0start_3_cloud/10tip_2000trees_0.125_1.0start_3_cloud.ps.log", 
                   stringsAsFactors = FALSE, header=T)
data5k3 <- read.csv("10tip_5000trees_0.125_1.0start_3_cloud/10tip_5000trees_0.125_1.0start_3_cloud.ps.log", 
                   stringsAsFactors = FALSE, header=T)
data10k3 <- read.csv("10tip_10000trees_0.125_1.0start_3_cloud/10tip_10000trees_0.125_1.0start_3_cloud.ps.log", 
                    stringsAsFactors = FALSE, header=T)
data20k3 <- read.csv("10tip_20000trees_0.125_1.0start_3_cloud/10tip_20000trees_0.125_1.0start_3_cloud.ps.log", 
                    stringsAsFactors = FALSE, header=T)

# Add columns with minutes and seconds
data2k3 <- add_elapsed_seconds(data2k3) 
data5k3 <- add_elapsed_seconds(data5k3) 
data10k3 <- add_elapsed_seconds(data10k3) 
data20k3 <- add_elapsed_seconds(data20k3) 

# Add column with total number of trees per cluster as discrete variables. 
# Use "as.integer" for continuous values
data2k3$NUMTREES <- rep(as.character(2000*2), nrow(data2k3))
data5k3$NUMTREES <- rep(as.character(5000*2), nrow(data5k3))
data10k3$NUMTREES <- rep(as.character(10000*2), nrow(data10k3))
data20k3$NUMTREES <- rep(as.character(20000*2), nrow(data20k3))


# subset all to 40 data points
#d2k <- data2k3[seq(1,nrow(data2k3),1),]
#d5k <- data5k3[seq(1,nrow(data5k3),4.4),]
#d10k <- data10k3[seq(1,nrow(data10k3),6.5),]
#d20k <- data20k3[seq(1,nrow(data20k3),6.6),]

#Rename instead of subsetting
d2k <- data2k3
d5k <- data5k3
d10k <- data10k3
d20k <- data20k3

#Combine dataframes. Fill in extra space.
dall <- rbind.fill(d2k,d5k,d10k,d20k)

#Set conversion from kb to gb
kb2gb = 1000000

#Total memory in Gb-----------------------------------
p1 <- ggplot(data=dall, mapping = aes(x = TIMEMIN, y = VSIZE/kb2gb, color=NUMTREES)) 
# data
p1 <- p1 + geom_point(data=dall, aes(TIMEMIN, VSIZE/kb2gb))
# axis
p1 <- p1 + scale_y_continuous(name="Total memory in Gb", breaks=seq(0,110,10)) +
  scale_x_continuous(name="Time in minutes", breaks=seq(0,140,10))
# theme
p1 <- p1 + theme_minimal() 
# label
p1 <- p1 + labs(color = "Total Trees")
# colors
p1 <- p1 + scale_color_brewer(palette="Set1")
# title
p1 <- p1 + ggtitle("Auto community detection: affinity network") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
p1



###Depreciated code-------------------------
#Rename columns before combining dataframes
#colnames(d2k) <- paste("d2k", colnames(d2k), sep = "_")
#colnames(d5k) <- paste("d5k", colnames(d5k), sep = "_")
#colnames(d10k) <- paste("d10k", colnames(d10k), sep = "_")
#colnames(d20k) <- paste("d20k", colnames(d20k), sep = "_")
#dall <- cbind(d2k,d5k,d10k,d20k)

#Total memory in Gb-----------------------------------
#cols <- c()
#basic <- ggplot(data=dall, mapping = aes(x = d20k_TIMEMIN, y = d20k_VSIZE/kb2gb)) +
#  labs(x="Time in minutes",y = "VSIZE in Gb") +
#  scale_y_continuous(name="Total memory in Gb", breaks=seq(0,110,10)) +
#  scale_x_continuous(name="Time in minutes", breaks=seq(0,140,10))
#p1 <- basic + geom_point(data=dall, aes(d2k_TIMEMIN, d2k_VSIZE/kb2gb),color="darkgreen")
#p1 <- p1 + geom_point(data=dall, aes(d5k_TIMEMIN, d5k_VSIZE/kb2gb),color="OliveDrab")
#p1 <- p1 + geom_point(data=dall, aes(d10k_TIMEMIN, d10k_VSIZE/kb2gb),color="Orange3")
#p1 <- p1 + geom_point(data=dall, aes(d20k_TIMEMIN, d20k_VSIZE/kb2gb),color="red3")