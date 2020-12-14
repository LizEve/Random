library(plotly)

packageVersion('plotly')

setwd("/Applications/MAC_TreeScaper_v1_Binary_2017-05-03/PosterTrees/25tip_0.5_0.5start/graphs")

data=read.table("25tip_0_URF_3D_CCA_COR_STOCHASTIC.out", header=F)

plot_ly(data, x = data$V1, y = data$V2, z = data$V3, colors = c('#BF382A', '#0C4B8E')) 


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="scatter3d/basic")
chart_link