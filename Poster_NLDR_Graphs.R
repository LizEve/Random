library(ggplot2)


setwd("/Applications/MAC_TreeScaper_v1_Binary_2017-05-03/")
setwd("/Applications/MAC_TreeScaper_v1_Binary_2017-05-03/PosterTrees/25tip_0.25_1.0start/NLDRcoordinates")


data=read.table("10tip_200trees_0_URF_2D_CCA_COR_STOCHASTIC.out", header=F)
data=read.table("25tip_0_URF_2D_CCA_COR_STOCHASTIC.out", header=F)


ggplot(data, aes(V1, V2)) + geom_point(color="darkorchid4") + theme(axis.line=element_blank(),
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


