library(rwty)
rwty.processors <<- 4
setwd("/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/")


runPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/"
mult <- load.multi(path=runPath, format = 'revbayes')
mult.rwty <- analyze.rwty(mult, burnin = 0)

