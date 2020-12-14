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

## Covariance runs Import Data
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

single_dataset_prep <- function(data, ntrees) {
  # Add columns with minutes and seconds
  datak <- add_elapsed_seconds(data) 
  meanTime <- mean(datak[["TIMESEC"]])
  totalTime <- max(datak[["TIMESEC"]])
  meanMem <- mean(datak[["VSIZE"]])
  maxMem <- max(datak[["VSIZE"]])
  info <- c(meanTime,totalTime,meanMem,maxMem,ntrees*2)
  return(info)
}

threetwoone <- function(data1, data2, data3, ntrees, header) {
  d1 <- single_dataset_prep(data1,ntrees)
  d2 <- single_dataset_prep(data2,ntrees)
  d3 <- single_dataset_prep(data3,ntrees)
  df <- rbind.data.frame(d1,d2,d3)
  names(df) <- header
  return(df)
}

twoone <- function(data1, data2,  ntrees, header) {
  d1 <- single_dataset_prep(data1,ntrees)
  d2 <- single_dataset_prep(data2,ntrees)
  df <- rbind.data.frame(d1,d2)
  names(df) <- header
  return(df)
}

header <- c("meanTime","totalTime","meanMemory","maxMemory","totalTrees")
d1k <- threetwoone(data1k1,data1k2,data1k3,1000, header)
d2k <- threetwoone(data2k1,data2k2,data2k3,2000, header)
d5k <- threetwoone(data5k1,data5k2,data5k3,5000, header)
d10k <- threetwoone(data10k1,data10k2,data10k3,10000, header)
d20k <- threetwoone(data20k1,data20k2,data20k3,20000, header)
d75k <- twoone(data75k1,data75k2,750, header)
dten <- twoone(data10k2,data10k2,10000, header)


df <- rbind.data.frame(d1k, d2k, d5k, d10k, d20k, d75k)
dfsmall <- rbind.data.frame(d1k, d2k, d5k, d75k, dten)

#Set conversion from kb to gb
kb2gb = 1000000


#Total memory in Gb-----------------------------------

p1 <- ggplot(data=dfsmall, mapping = aes(x = totalTrees, y = maxMemory/kb2gb, color=totalTrees)) 
# data
p1 <- p1 + geom_point(data=dfsmall, aes(totalTrees, maxMemory/kb2gb), cex=5)
# axis
p1 <- p1 + scale_y_continuous(name="Total memory in Gb", breaks=seq(0,150,5)) +
  scale_x_continuous(name="Total trees per file", breaks=seq(0,40000,5000))
# theme
p1 <- p1 + theme_minimal() 
# color
p1 <- p1 + scale_colour_gradientn(colours=rainbow(3))
# title
p1 <- p1 + ggtitle("Auto community detection: affinity network") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
p1

'''
x <- dfsmall$totalTrees
y <- dfsmall$maxMemory/kb2gb
y <- 0.0000002*((x)^2.0632)
'''
x = 18000
0.0000002*((x)^2.0632)


p1 <- ggplot(data=df, mapping = aes(x = totalTrees, y = totalTime/60, color=totalTrees)) 
# data
p1 <- p1 + geom_point(data=df, aes(totalTrees, totalTime/60), cex=5)
# axis
p1 <- p1 + scale_y_continuous(name="Total time", breaks=seq(0,350,20)) +
  scale_x_continuous(name="Total trees per file", breaks=seq(0,40000,1000))
# theme
p1 <- p1 + theme_minimal() 
# color
p1 <- p1 + scale_colour_gradientn(colours=rainbow(4))
# title
p1 <- p1 + ggtitle("Auto community detection: affinity network") +
  theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
p1
