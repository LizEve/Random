library(ggplot2)

NLDRplot <- function (aFile){
  # Read in file
  d <- read.table(aFile)
  p <- ggplot(d, aes(V1, V2)) + geom_point(color="black") + theme(axis.line=element_blank(),
                                                                  axis.text.x=element_blank(),
                                                                  axis.text.y=element_blank(),
                                                                  axis.ticks=element_blank(),
                                                                  axis.title.x=element_blank(),
                                                                  axis.title.y=element_blank(),
                                                                  legend.position="none",
                                                                  panel.background=element_blank(),
                                                                  panel.border=element_blank(),
                                                                  panel.grid.major=element_blank(),
                                                                  panel.grid.minor=element_blank(),
                                                                  plot.background=element_blank())
  
  
  # Grab run name from longer file path
  runName <- strsplit(aFile,"/")[[1]][1]
  outFile <- paste(runName,"2D.svg", sep="_")
  outPath <- paste(runName,outFile,sep="/")
  
  # Write out results
  ggsave(file=outPath, plot=p, width=20, height=11, units=c("cm"))
}




mainDir <- getwd()
mainDir

b <- list.files(pattern="_2D_CCA_COR_STOCHASTIC.out",recursive=TRUE)
lapply(b,NLDRplot)


#################################################################

#aFile <- '25tip_500trees_0.125_1.0start_30_cloud/25tip_500trees_0.125_1.0start_30_cloud_rooted_unweighted_URF-distance_2D_CCA_COR_STOCHASTIC.out'
# ggsave(file=outPath, plot=p, width=21, height=21, units=c("cm")) ggsave(file=outPath, plot=p, width=20, height=21, units=c("cm"))
#aFile <- '25tip_1000trees_0.04_1.0start_25_cloud/25tip_1000trees_0.04_1.0start_25_cloud_rooted_unweighted_URF-distance_2D_CCA_COR_STOCHASTIC.out'
#ggsave(file=outPath, plot=p, width=12, height=25, units=c("cm")), ggsave(file=outPath, plot=p, width=12, height=20, units=c("cm"))
#aFile <- '25tip_1000trees_0.04_1.0start_25_cloud/25tip_1000trees_0.04_1.0start_25_cloud_rooted_unweighted_URF-distance_HALF_2D_CCA_COR_STOCHASTIC.out'
#ggsave(file=outPath, plot=p, width=7, height=7, units=c("cm")) ggsave(file=outPath, plot=p, width=12, height=12, units=c("cm"))
#aFile <- '25tip_1000trees_1.0_1.0start_36_cloud/25tip_1000trees_1.0_1.0start_36_cloud_rooted_unweighted_URF-distance_HALF_2D_CCA_COR_STOCHASTIC.out'
#ggsave(file=outPath, plot=p, width=20, height=20, units=c("cm")) ggsave(file=outPath, plot=p, width=12, height=12, units=c("cm")) ggsave(file=outPath, plot=p, width=7, height=7, units=c("cm"))


d <- read.table(aFile)
p <- ggplot(d, aes(V1, V2)) + geom_point(color="black") + theme(axis.line=element_blank(),
                                                                axis.text.x=element_blank(),
                                                                axis.text.y=element_blank(),
                                                                axis.ticks=element_blank(),
                                                                axis.title.x=element_blank(),
                                                                axis.title.y=element_blank(),
                                                                legend.position="none",
                                                                panel.background=element_blank(),
                                                                panel.border=element_blank(),
                                                                panel.grid.major=element_blank(),
                                                                panel.grid.minor=element_blank(),
                                                                plot.background=element_blank())


# Grab run name from longer file path
runName <- strsplit(aFile,"/")[[1]][1]
outFile <- paste(runName,"2D.svg", sep="_")
outPath <- paste(runName,outFile,sep="/")

# Write out results
ggsave(file=outPath, plot=p, width=20, height=20, units=c("cm"))


