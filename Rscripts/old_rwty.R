library(devtools)
#install_github("lizeve/RWTY")
#install_github("danlwarren/RWTY")
library(rwty)
library(coda)
setwd("/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/")



combPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/tenlines.trees"
logPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/tenlines.log"
combRuns <- load.trees(combPath, logfile = logPath, format = "revbayes")
comb.rwty <- analyze.rwty(combRuns, burnin=0)
makeplot.asdsf(mult[c(1,2)])


data(salamanders)
salamanders.rwty <- analyze.rwty(salamanders, burnin=50, fill.color = 'LnL')



combPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/tenlines.trees"
data <- read.table(combPath,stringsAsFactors = FALSE, header=T)

param <- "Posterior"
burnin = 0
# get lenght of dataframe
end <- length(data[[param]])
effectiveSize(mcmc(data[,param][(burnin+1):length(data[[param]])]))

ess <- unlist(lapply(chains, FUN = function(x) effectiveSize( mcmc(data[,param][(burnin+1):length(data[[param]])] )))

ess <- unlist(lapply(chains, FUN = function(x) effectiveSize( mcmc(x$ptable[parameter][(burnin+1):length(x$ptable[[parameter]]),]) )))
x <- chain







combPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/tenlines.trees"
combRuns <- load.trees(combPath, format = "revbayes")
#combAnal <- analyze.rwty(combRuns, burnin=0, fill.color = 'Likelihood')
# Error in names(param.plots) <- plot.names :   
#   'names' attribute [2] must be the same length as the vector [1]


#makeplot.all.params.R
strip = 1
burnin = 0
facet=TRUE
chains <- combRuns
chains = check.chains(chains)
chain = chains[[1]]
if(is.null(chain$ptable)) stop("No parameters associated with your chains")
#params <- names(chain$ptable)[-strip] ## names doesnt work, gives null
params <- colnames(chain$ptable)[-strip]
param.plots <- lapply(params, FUN = function(x) makeplot.param(parameter = x, burnin = burnin, chains = chains, facet = facet))
# ERROR1 go to different script. 
# for parameters in params. do the following script
parameter <- "Posterior"
#makeplot.param.R
chains = check.chains(chains)
#ptable = combine.ptables(chains, burnin)
ptable = chains[1]$ptable
parameter %in% colnames(ptable)

# get ESS values
# get list of parameter values from burnin to end 
# double brackets are a way to grab something from a dataframe using a variable for a column name
#(for some reason this isnt working for how rb file is read in, it probably worked with and older version of the output file.)
ess <- unlist(lapply(chains, FUN = function(x) effectiveSize( mcmc(x$ptable[parameter][(burnin+1):length(x$ptable[[parameter]]),]) )))
x <- chain




ess <- round(ess, digits = 0)
labels = paste(names(chains), " (ESS=", ess, ")", sep="")
names(chains) = labels
ptable = combine.ptables(chains, burnin)
title = paste(parameter, "trace")


#Warning message:
#  In ptable$chain = chain : Coercing LHS to a list

param.plots <- lapply(params, FUN = function(x) makeplot.param(parameter = x, burnin = burnin, chains = chains, facet = facet))



t.plot = makeplot.topology(chains, burnin = burnin, facet = facet)
param.plots[[length(param.plots)+1]] <- t.plot
plot.names = c(paste(params, ".trace", sep=""), "topology.trace.plot")
names(param.plots) <- plot.names
return(param.plots)


#ERROR 1
'''
[1] "Creating trace for Posterior"
Show Traceback

Rerun with Debug
Error in makeplot.param(parameter = x, burnin = burnin, chains = chains,  : 
                          The variable 'Posterior' is not a column in the table of parameters you have supplied In addition: Warning message:
                          In ptable$chain = chain : Coercing LHS to a list
'''

########################
#Import Data
########################
#setwd("/Users/ChatNoir/Projects/Squam/Sq_CL/geneTreeTest/Part")
#setwd("/Users/ChatNoir/Projects/Squam/Sq_CL/geneTreeTest/noPart/")
#runPath=paste(run,"/output/", sep="")
#combPath=paste(run, "/output/all", run, sep="")


setwd("/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/")

run="AHE-L204"
#runPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204"
combPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/tenlines.trees"
#combPath="/Users/ChatNoir/Projects/Squam/Sq_CL/rwty/allAHE-L204/AHE-L204_posterior.trees"
combRuns <- load.trees(combPath, format = "revbayes")
combAnal <- analyze.rwty(combRuns, burnin=0, fill.color = 'Likelihood')
# Error in names(param.plots) <- plot.names :   
#   'names' attribute [2] must be the same length as the vector [1]


########################
#RWTY settings
########################
# set number of processors to use
rwty.processors <<- 4
# combined log is 4004 lines
combRuns <- load.trees(combPath, format = "revbayes")
# each log is 1001 lines
# allRuns <- load.multi(runPath, format = "revbayes", trim = 10)

combAnal <- analyze.rwty(combRuns, burnin=0, fill.color = 'Likelihood', treespace.points = 100)
# ERROR "Error in names(param.plots) <- plot.names :   'names' attribute [2] must be the same length as the vector [1]"
# allAnal <- analyze.rwty(allRuns, burnin=0, fill.color = 'Likelihood')
# Dies on ASDSF plot. subsample runs. 

#########################
# Installation
########################
#https://github.com/danlwarren/RWTY
install.packages("devtools")
library(devtools)
install_github("danlwarren/RWTY")
library(rwty)
