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

model         = "Lc_Hc_cLxL" # cond


rem_subjects = c()
pathfig  = c("LH2crossExp2","LH2crossAnti")

if(avgs){Tstr='subjMeans'}
if(!avgs){Tstr='allData'}
if(factorial){fstr='factorial'}
if(!factorial){fstr='simpleEffect'}

#if(logn){Tstr='logNorm'}
#if(!logn){Tstr='Norm'}

if(perf){fileNameRoot = paste("bern_ANOVA",Tstr,model,fstr,sep="_")}
if(!perf){fileNameRoot = paste("trialRT_ANOVA",Tstr,model,fstr,sep="_")} # for constructing output filenames

if(perf){if(!avgs&factorial){model = "bern_ANOVA_allData.R"}
}
if(!perf){
  if(!avgs&factorial&model=="cond"){modelSource = "trialRT_ANOVA_Exp2_allData.R"}
  if(!avgs&factorial&model=="Lc_Hc_cLxL"){modelSource = "trialRT_lm_Lc_Hc_cLxL_allData.R"}
  if(avgs&factorial&model=="SR"){modelSource = "trialRT_ANOVA_SR_subjMeans.R"}
  if(!avgs&factorial&model=="SR"){modelSource = "trialRT_ANOVA_SR_allData.R"}
  if(!avgs&factorial&model=="SRcond"){modelSource = "trialRT_ANOVA_SRcond_allData.R"}      # model with SR condition plus task and response mode
  if(!avgs&factorial&model=="SRred"){modelSource = "trialRT_ANOVA_SRred_allData.R"}
  if(!avgs&factorial&model=="SRchain"){modelSource = "trialRT_ANOVA_SRchain_allData.R"}
  if(model=="test"){modelSource = "trialRT_test.R"}
  #if(!avgs&!factorial){model = "trialRT_ANOVA_allData_simpleEffect.R"}
}

source("05_Code/02_Analysis/rcode/utilities/get_exp2_data.R")
#if(!perf){
#source("code/analysis/rcode/plotting/rawPlot_Exp2.R")
#source("code/analysis/rcode/plotting/rawPlot_Exp2_prevT.R")
#}
#if(perf){source("code/analysis/rcode/plotting/rawPlotperf_LH2cross.R")}
# RUN THE CHAINS ------------------------------------------------------------------------
#source(paste("code/analysis/rcode/models/",model,sep=''), chdir=T)   
#source('code/analysis/rcode/utilities/init_chains.R')             # we need this for the Censor model
# parameters    = c("ySigma","m","nu","b0","b1","b2","b3","b4","b5","b6","b1b2","b1b3","b1b4","b2b3","b2b4","b3b4",
#                   "b1b2b3","b1b2b4","b2b3b4","b1b3b4","b1b2b3b4",
#                   "sigmaMode","sigmaSD","a1SD","a2SD","a3SD","a4SD","a5SD","a6SD","SDa0S")  
parameters    = c("ySigma","m","b0","b1","b2","b3","b1b2","b1b3","b2b3","b1b2b3","bs")  
adaptSteps    <- 1000   
burnInSteps   <- 1000     
nChains       <- 4         
numSavedSteps <- 10000    
thinSteps     <- 2       

runjagsModel = run.jags(paste(path,"/code/analysis/rcode/models/",modelSource,sep=""), monitor=parameters, data=dataList,
                        adapt=adaptSteps, n.chains=nChains, thin=thinSteps,
                        method='parallel', burnin=burnInSteps, 
                        sample=numSavedSteps, 
                        #inits=initsList,  
                        psrf.target=1.05)#inits=initsList,

  # CHECK MODEL CONVERGENCE ---------------------------------------------------------------
checkConvergence = T
checkPartialPath = "/figures/LH2crossExp2/bayes/checks/"
source("code/analysis/rcode/utilities/convergenceChecks.R", chdir=T)

# SAVE MODEL RESULT ---------------------------------------------------------------------
mcmcChain = as.matrix(as.mcmc(runjagsModel))
save(mcmcChain, file=paste(path,"/data/LH2crossAnti/mcmcData/",fileNameRoot,'.Rdata',sep=''))

# POSTERIOR DISTRIBUTIONS PLOTTING ------------------------------------------------------

load(file=paste(path,"/data/LH2crossAnti/mcmcData/",fileNameRoot,'.Rdata',sep=''))
#param = "RTf"
source("code/analysis/rcode/plotting/plotPost_Exp2_trialRT_ANOVA.R")

# POSTERIOR PREDICTIVE CHECKS -----------------------------------------------------------
# Comparison between actual data and data generated from the parameters posterior estimates 

#source('plotting/pred_vs_data_compRTtrial.R')
