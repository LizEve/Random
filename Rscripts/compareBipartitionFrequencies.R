library(ggplot2)
library(ggpubr)

####Scatter Plots#####
scatter_graph <- function(data,x,y,xlab,ylab,xtix,ytix){ 
  #create new data frame with counts of how many time each value occurs
  data2 <- aggregate(x,by=list(x=x,y=y),length)
  names(data2)[3] <- "count"
  data2$prop <- (data2$count/sum(data2$count))*50
  p <- ggplot(data, aes(x, y)) 
  # theme and colors
  #p <- p + theme_minimal() 
  p <- p + geom_count(color="darkorchid4",size=data2$prop)  
  #p <- p + scale_size_continuous(guide = "legend")
  # trend line
  p <- p + geom_smooth(method=lm, formula = y ~ x, color="yellow2", se=FALSE) 
  # axis
  p <- p + scale_x_continuous(name=xlab, breaks = round(seq(0, max(x),by = xtix)))
  p <- p + scale_y_continuous(name=ylab, breaks = round(seq(0, max(y),by = ytix))) 
  # title
  #p <- p + ggtitle(title) +
   # theme(plot.title = element_text(vjust=1, hjust=0.5, size = 14))
  p
}

###Put multiple graphs on one page
outGraph <- function(dataFile,outName){
  data <- read.table(dataFile, stringsAsFactors = FALSE, header=F)
  p1<-scatter_graph(data,data$V2,data$V3,"run1","run2",0.01,0.01)
  p2<-scatter_graph(data,data$V2,data$V4,"run1","run3",0.01,0.01)
  p3<-scatter_graph(data,data$V2,data$V5,"run1","run4",0.01,0.01)
  p4<-scatter_graph(data,data$V3,data$V4,"run2","run3",0.01,0.01)
  p5<-scatter_graph(data,data$V3,data$V5,"run2","run4",0.01,0.01)
  p6<-scatter_graph(data,data$V4,data$V5,"run3","run4",0.01,0.01)
  x<-ggarrange(p1,p2,p3,p4,p5,p6, nrow=2, ncol=3)
  ggsave(file=outName,x)
}

outGraph("combifreq.txt","blahblah.pdf")
