library(ggplot2)
library(RColorBrewer)

setwd("/Users/ChatNoir/Projects/TreeScaper/Cluster_downloads")

data<- read.csv("Compiled_info.out", stringsAsFactors = FALSE, header=T)

###BOX
ggplot(data, aes(x=Network, y=Communities)) + 
  geom_boxplot(aes(group = cut_width(Network, 500)),outlier.shape=NA) + 
  geom_jitter(position=position_jitter(width=5, height=0),size=0.5) +
  xlab("Network") +
  ylab("Communities") +
  theme(axis.title.x = element_text(color="Black", size=10, vjust=-0.35, family='Helvetica'), 
        axis.title.y = element_text(color="Black", size=10, vjust=0.35, family='Helvetica')) 

###BOX Cat
ggplot(data, aes(x=Network, y=Communities)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(position=position_jitter(width=0.3, height=0),size=0.5) +
  xlab("Network") +
  ylab("Communities") +
  theme(axis.title.x = element_text(color="Black", size=10, vjust=-0.35, family='Helvetica'), 
        axis.title.y = element_text(color="Black", size=10, vjust=0.35, family='Helvetica')) 

