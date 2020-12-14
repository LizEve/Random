library(ggplot2)
library(RColorBrewer)
library(gridExtra)

setwd("/Users/ChatNoir/Projects/TreeScaper/numTrees")

#Import data
data <- read.csv("numTrees.csv", stringsAsFactors = FALSE, header=T)
aff <- data[data$Network=="Affinity",]
cov <- data[data$Network=="Covariance",]


####Scatter Plots#####
ggplot(aff, aes(Trees, TimeTotal/60)) + geom_point(color="darkorchid4") + 	
  geom_smooth(method=lm, formula = y ~ x, color="cyan4", se=TRUE) 

ggplot(aff, aes(Trees, TimeTotal/60)) + geom_point(color="darkorchid4") + 	
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))

ggplot(cov, aes(Trees, TimeTotal/60)) + geom_point(color="darkorchid4") + 	
  geom_smooth(method=lm, formula = y ~ x, color="cyan4", se=TRUE) 

ggplot(cov, aes(Trees, TimeTotal/60)) + geom_point(color="darkorchid4") + 	
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))

ggplot(cov, aes(Trees, NumberCommunities)) + geom_point(color="darkorchid4") + 	
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))

####Pie Charts#####
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

##Data frame
#Get number of communities
cov_numCom <- data.frame(
  Number_Communities = c("Two", "Three", "Four", "Five", "Seven"),
  value = c(nrow(cov[cov$NumberCommunities==2,]), 
            nrow(cov[cov$NumberCommunities==3,]), 
            nrow(cov[cov$NumberCommunities==4,]), 
            nrow(cov[cov$NumberCommunities==5,]), 
            nrow(cov[cov$NumberCommunities==7,])))

ggplot(cov_numCom, aes(x="", y=value, fill=Number_Communities)) +
  geom_bar(width = 2, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void()

#Get number of communities
aff_numCom <- data.frame(
  Number_Communities = c("Two", "Three", "Four", "Five", "Seven"),
  value = c(nrow(aff[aff$NumberCommunities==2,]), 
            nrow(aff[aff$NumberCommunities==3,]), 
            nrow(aff[aff$NumberCommunities==4,]), 
            nrow(aff[aff$NumberCommunities==5,]), 
            nrow(aff[aff$NumberCommunities==7,])))

ggplot(aff_numCom, aes(x="", y=value, fill=Number_Communities)) +
  geom_bar(width = 2, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void()