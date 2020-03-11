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

source("05_Code/02_Analysis/rcode/utilities/openGraphSaveGraph.R", chdir=T) # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("05_Code/02_Analysis/rcode/utilities/HDIofMCMC.R", chdir=T)
source("05_Code/02_Analysis/rcode/utilities/DBDA2E-utilities.R", chdir=T)
require(runjags)
require(ggplot2)
require(plyr)
require(yarrr)
library(extrafont)
require(plotrix)
require(gridExtra)

perf          = FALSE
logn          = FALSE                            # log-normal data model
avgs          = FALSE
factorial     = TRUE
exp           = 2
task          = "normal"

if(exp==1){
  rem_subjects = c(3,9,13)
  pathfig = "LH2cross"}
if(exp==2){
  rem_subjects = c()
  if(task=="normal"){
    pathfig = "LH2crossExp2"}
  if(task=="anti"){
    pathfig = "LH2crossAnti"}
}                         # reasons to remove: subject 3 the button did not work well, subject 9 stimulator stop working for the last four block, s13 stimulator broke down and subject did not feel stimulation for long periods
if(avgs){Tstr='subjMeans'}
if(!avgs){Tstr='allData'}
if(factorial){fstr='factorial'}
if(!factorial){fstr='simpleEffect'}

#if(logn){Tstr='logNorm'}
#if(!logn){Tstr='Norm'}

if(perf){fileNameRoot = paste("bern_ANOVA",Tstr,fstr,sep="_")}
if(!perf){fileNameRoot = paste("trialRT_ANOVA",Tstr,fstr,sep="_")} # for constructing output filenames

#model       = "anova_legC_SRa_SRE.R"
if(perf){if(!avgs&factorial){model = "bern_ANOVA_allData.R"}
}
if(!perf){
  if(avgs&factorial){model = "trialRT_ANOVA_subjMeans.R"}
  if(!avgs&factorial){model = "trialRT_ANOVA_allData.R"}
  if(!avgs&!factorial){model = "trialRT_ANOVA_allData_simpleEffect.R"}
}

source("05_Code/02_Analysis/rcode/utilities/get_trialLH2cross_data.R")
if(!perf){source("05_Code/02_Analysis/rcode/plotting/rawPlot_LH2cross.R")}
if(perf){source("05_Code/02_Analysis/rcode/plotting/rawPlotperf_LH2cross.R")}
# RUN THE CHAINS ------------------------------------------------------------------------
#source(paste("05_Code/02_Analysis/rcode/models/",model,sep=''), chdir=T)   
#source('05_Code/02_Analysis/rcode/utilities/init_chains.R')             # we need this for the Censor model
parameters    = c("ySigma","m","nu","b0","b1","b2","b3","aS","b1b2","b1b3","b2b3","b1b2b3",
                  "sigmaMode","sigmaSD","a1SD","a2SD","a3SD","SDa0S","a1a2SD","a1a3SD","a2a3SD","a1a2a3SD")  

adaptSteps    <- 5000     
burnInSteps   <- 5000     
nChains       <- 4         
numSavedSteps <- 5000     
thinSteps     <- 2       

runjagsModel = run.jags(paste(path,"/05_Code/02_Analysis/rcode/models/",model,sep=""), monitor=parameters, data=dataList,
                        adapt=adaptSteps, n.chains=nChains, thin=thinSteps,
                        method='parallel', burnin=burnInSteps, 
                        sample=numSavedSteps, 
                        #inits=initsList,  
                        psrf.target=1.05)#inits=initsList,

# CHECK MODEL CONVERGENCE ---------------------------------------------------------------
checkConvergence = FALSE
checkPartialPath = "/figures/LH2cross/bayes/checks/"
source("05_Code/02_Analysis/rcode/utilities/convergenceChecks.R", chdir=T)

# SAVE MODEL RESULT ---------------------------------------------------------------------
mcmcChain = as.matrix(as.mcmc(runjagsModel))
save(mcmcChain, file=paste(path,"/07_Analyses/LH2cross/bayes/mcmcData/",fileNameRoot,'.Rdata',sep=''))

# POSTERIOR DISTRIBUTIONS PLOTTING ------------------------------------------------------

load(file=paste(path,"/data/LH2cross/mcmcData/",fileNameRoot,'.Rdata',sep=''))
#param = "RTf"
source("05_Code/02_Analysis/rcode/plotting/plotPost_trialRT_ANOVA.R")

# POSTERIOR PREDICTIVE CHECKS -----------------------------------------------------------
# Comparison between actual data and data generated from the parameters posterior estimates 

#source('plotting/pred_vs_data_compRTtrial.R')
