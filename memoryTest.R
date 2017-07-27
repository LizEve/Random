library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
# See https://github.com/jeetsukumaran/Syrupy for info about variables. 
# RSS and VSIZE are in kb
# VSIZe - RSS = swap ?

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

colsplit(data2k3$ELAPSED,":",c("min","sec"))


'''
d2k$d2k_ELAPSED
iterate over values
stringsplit 
force into numeric
make into seconds
'''

# subset all to 40 data points
d2k <- data2k3[seq(1,nrow(data2k3),1),]
d5k <- data5k3[seq(1,nrow(data5k3),4.4),]
d10k <- data10k3[seq(1,nrow(data10k3),6.5),]
d20k <- data20k3[seq(1,nrow(data20k3),6.6),]

colnames(d2k) <- paste("d2k", colnames(d2k), sep = "_")
colnames(d5k) <- paste("d5k", colnames(d5k), sep = "_")
colnames(d10k) <- paste("d10k", colnames(d10k), sep = "_")
colnames(d20k) <- paste("d20k", colnames(d20k), sep = "_")

dall <- cbind(d2k,d5k,d10k,d20k)

kb2gb = 1000000

ggplot()+
  geom_point(data=dall, aes(d2k_ELAPSED, d2k_VSIZE/kb2gb),color="darkgreen") +
  geom_point(data=dall, aes(d5k_ELAPSED, d5k_VSIZE/kb2gb),color="OliveDrab") +
  geom_point(data=dall, aes(d10k_ELAPSED, d10k_VSIZE/kb2gb),color="Orange3") +
  geom_point(data=dall, aes(d20k_ELAPSED, d20k_VSIZE/kb2gb),color="red3") +
  labs(x="Time",y = "VSIZE in Gb") +
  scale_y_continuous(name="VSIZE in Gb", breaks=seq(0,110,10))
  
ggplot()+
  geom_point(data=dall, aes(d2k_ELAPSED, d2k_RSS/kb2gb),color="darkgreen") +
  geom_point(data=dall, aes(d5k_ELAPSED, d5k_RSS/kb2gb),color="OliveDrab") +
  geom_point(data=dall, aes(d10k_ELAPSED, d10k_RSS/kb2gb),color="Orange3") +
  geom_point(data=dall, aes(d20k_ELAPSED, d20k_RSS/kb2gb),color="red3") +
  labs(x="Time",y = "RSS in Gb") +
  scale_y_continuous(name="RSS in Gb", breaks=seq(0,110,10))

ggplot()+
  geom_point(data=dall, aes(d2k_ELAPSED, (d2k_VSIZE-d2k_RSS)/kb2gb),color="darkgreen") +
  geom_point(data=dall, aes(d5k_ELAPSED, (d5k_VSIZE-d5k_RSS)/kb2gb),color="OliveDrab") +
  geom_point(data=dall, aes(d10k_ELAPSED, (d10k_VSIZE-d10k_RSS)/kb2gb),color="Orange3") +
  geom_point(data=dall, aes(d20k_ELAPSED, (d20k_VSIZE-d20k_RSS)/kb2gb),color="red3") +
  labs(x="Time",y = "swap in Gb") +
  scale_y_continuous(name="swap in Gb", breaks=seq(0,110,5))

  
  

kb2gb = 1000000

swap_2k_Gb=(d2k$VSIZE-d2k$RSS)/kb2gb
rss_2k_Gb=d2k$RSS/kb2gb
vsize_2k_Gb=d2k$VSIZE/kb2gb

swap_5k_Gb=(d5k$VSIZE-d5k$RSS)/kb2gb
rss_5k_Gb=d5k$RSS/kb2gb
vsize_5k_Gb=d5k$VSIZE/kb2gb

swap_10k_Gb=(d10k$VSIZE-d10k$RSS)/kb2gb
rss_10k_Gb=d10k$RSS/kb2gb
vsize_10k_Gb=d10k$VSIZE/kb2gb

swap_20k_Gb=(d20k$VSIZE-d20k$RSS)/kb2gb
rss_20k_Gb=d20k$RSS/kb2gb
vsize_20k_Gb=d20k$VSIZE/kb2gb


