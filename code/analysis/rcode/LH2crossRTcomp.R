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
avgs          = FALSE
factorial     = TRUE

if(avgs){Tstr='subjMeans'}
if(!avgs){Tstr='allData'}
if(factorial){fstr='factorial'}
if(!factorial){fstr='simpleEffect'}
#if(logn){Tstr='logNorm'}
#if(!logn){Tstr='Norm'}

fileNameRoot = paste("trialRT_ANOVA",Tstr,fstr,sep="_") # for constructing output filenames

#model       = "anova_legC_SRa_SRE.R"
if(avgs&factorial){model = "trialRT_ANOVA_subjMeans.R"}
if(!avgs&factorial){model = "trialRT_ANOVA_allData.R"}
if(!avgs&!factorial){model = "trialRT_ANOVA_allData_simpleEffect.R"}

source("code/analysis/rcode/utilities/get_trialLH2cross_data.R")
source("code/analysis/rcode/plotting/rawPlot_LH2cross.R")
# RUN THE CHAINS ------------------------------------------------------------------------
#source(paste("code/analysis/rcode/models/",model,sep=''), chdir=T)   
#source('code/analysis/rcode/utilities/init_chains.R')             # we need this for the Censor model
parameters    = c("ySigma","m","nu","b0","b1","b2","b3","aS","b1b2","b1b3","b2b3","b1b2b3",
                  "sigmaMode","sigmaSD","a1SD","a2SD","a3SD","SDa0S","a1a2SD","a1a3SD","a2a3SD","a1a2a3SD")  

adaptSteps    <- 5000     
burnInSteps   <- 5000     
nChains       <- 4         
numSavedSteps <- 5000     
thinSteps     <- 4       

runjagsModel = run.jags(paste(path,"/code/analysis/rcode/models/",model,sep=""), monitor=parameters, data=dataList,
                        adapt=adaptSteps, n.chains=nChains, thin=thinSteps,
                        method='parallel', burnin=burnInSteps, 
                        sample=numSavedSteps, 
                        #inits=initsList,  
                        psrf.target=1.05)#inits=initsList,

# CHECK MODEL CONVERGENCE ---------------------------------------------------------------
checkConvergence = TRUE
checkPartialPath = "/figures/LH2cross/bayes/checks/"
source("code/analysis/rcode/utilities/convergenceChecks.R", chdir=T)

# SAVE MODEL RESULT ---------------------------------------------------------------------
mcmcChain = as.matrix(as.mcmc(runjagsModel))
save(mcmcChain, file=paste(path,"/data/LH2cross/mcmcData/",fileNameRoot,'.Rdata',sep=''))

# POSTERIOR DISTRIBUTIONS PLOTTING ------------------------------------------------------

#load(file=paste(path,"/mcmcData/",fileNameRoot,'.Rdata',sep=''))
#param = "RTf"
source("code/analysis/rcode/plotting/plotPost_trialRT_ANOVA.R")

# POSTERIOR PREDICTIVE CHECKS -----------------------------------------------------------
# Comparison between actual data and data generated from the parameters posterior estimates 

#source('plotting/pred_vs_data_compRTtrial.R')
