#####################################################################################
# COMPARISON OF RT
# osf.io project: 
# This code reproduces the results presented on the article:
#
# To reproduce the modelling and result generation run enterely
# To reproduce figure as in the paper run PREAMBLE and POSTERIOR DISTRIBUTIONS PLOTTING, this will upload
# the mcmc chain used for the paper
# Everything should work after changing the path variable below and if all required packages are available
#
# The analisis presented here are inspired by Kruschke (2011) and Gelman & Hill (2007) 
# and rely heavily on scripts by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
#
# by José Ossandón (jose.ossandon@uni-hamburg.de) 
#####################################################################################

# PREAMBLE ------------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))

path       = "/Users/jossando/trabajo/E299"
setwd(path)

source("code/analysis/rcode/utilities/openGraphSaveGraph.R", chdir=T) # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("code/analysis/rcode/utilities/HDIofMCMC.R", chdir=T)
source("code/analysis/rcode/utilities/DBDA2E-utilities.R", chdir=T)
require(runjags)
require(ggplot2)
require(plyr)
require(yarrr)
library(extrafont)

logn          = FALSE                            # log-normal data model

if(logn){Tstr='logNorm'}
if(!logn){Tstr='Norm'}

fileNameRoot = paste("trialRT",Tstr,sep="_") # for constructing output filenames


if(logn){model = "trialRT_lognmodel.R"}
if(!logn){model = "trialRT_normmodel.R"}
source("code/analysis/rcode/utilities/get_trialLH2cross_data.R")
source("code/analysis/rcode/plotting/rawPlot_LH2cross.R")
# RUN THE CHAINS ------------------------------------------------------------------------
source(paste("code/analysis/rcode/models/",model,sep=''), chdir=T)   
source('code/analysis/rcode/utilities/init_chains.R')             # we need this for the Censor model
parameters    = c("mu","aS","SDa0S", "a", "aC", "nu", "sigma" ,"asigma" ,"aCsigma" )  

adaptSteps    <- 5000     
burnInSteps   <- 5000     
nChains       <- 4         
numSavedSteps <- 5000     
thinSteps     <- 2        

runjagsModel = run.jags(paste(path,"/models/",model,sep=""), monitor=parameters, data=dataList,
                        adapt=adaptSteps, n.chains=nChains, thin=thinSteps,
                        method='parallel', burnin=burnInSteps, 
                        sample=numSavedSteps, inits=initsList,  psrf.target=1.05)#inits=initsList,

# CHECK MODEL CONVERGENCE ---------------------------------------------------------------
checkConvergence = FALSE
checkPartialPath = "/figures/LH2cross/bayes/checks/"
source("utilities/convergenceChecks.R", chdir=T)

# SAVE MODEL RESULT ---------------------------------------------------------------------
mcmcChain = as.matrix(as.mcmc(runjagsModel))
save(mcmcChain, file=paste(path,"/data/LH2cross/mcmcData/",fileNameRoot,'.Rdata',sep=''))

# POSTERIOR DISTRIBUTIONS PLOTTING ------------------------------------------------------

#load(file=paste(path,"/mcmcData/",fileNameRoot,'.Rdata',sep=''))
#param = "RTf"
#source("plotting/plotPost_compRTtrial.R")

# POSTERIOR PREDICTIVE CHECKS -----------------------------------------------------------
# Comparison between actual data and data generated from the parameters posterior estimates 

#source('plotting/pred_vs_data_compRTtrial.R')
