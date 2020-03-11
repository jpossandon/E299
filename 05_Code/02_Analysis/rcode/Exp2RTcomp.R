#####################################################################################
# COMPARISON OF RT
# osf.io project: 
# This code reproduces the results presented on the article:
#
# To reproduce the modelling and result generation run enterely with runmodel = T
# To reproduce figure as in the paper run PREAMBLE and POSTERIOR DISTRIBUTIONS PLOTTING with runmodel = F, this will upload
# the mcmcchain used for the paper
# Everything should work after changing the path variable below, preserving the subfolder structure provided and with
# all required packages are available
#
# To reproduce:
# Experiment 1, set exp = 1 and model = cond
# Experiment 2, factorial analysis set exp = 22 and model = condtask
# Experiment 2, reduced analysis set exp = 22 and model = conf
# Experiment 3, reduced analysis set exp = 3 and model = conf
# The analisis presented here are inspired by Kruschke (2011) and Gelman & Hill (2007) 
# and rely heavily on scripts by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
# failed.jags('model')
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
library(magick)
library(gtools)
library(RColorBrewer)
#display.brewer.all(colorblindFriendly = TRUE)

perf          = TRUE
avgs          = FALSE
factorial     = TRUE
runmodel      = T

model         = "condtask" # cond / condtask /  conf
exp           = 22      # 1 / 22 / 3

if(exp==1){
  rem_subjects = c(3,9,13)
  tsks        <- c("Normal")
  pathfig = "LH2cross"}
if(exp==2){
  rem_subjects = c()
  pathfig  = c("LH2crossExp2","LH2crossAnti")
  tsks        <- c("Normal","Anti")}
if(exp==22){
  rem_subjects = c()
  pathfig  = c("LH2crossboth","LH2crossboth")
  tsks        <- c("both")}
if(exp==3){
  rem_subjects = c()
  pathfig  = c("LH3cross","HL3cross")
  tsks        <- c("Normal","HL")}
if(exp==4){
  rem_subjectALL = list(i=c(3,9,13), ii=c(),ii=c())
  pathfig  = c("All")
  tsks        <- c("Normal")}

if(avgs){Tstr='subjMeans'}
if(!avgs){Tstr='allData'}
if(factorial){fstr='factorial'}
if(!factorial){fstr='simpleEffect'}

if(perf){fileNameRoot = paste("bern_ANOVA",Tstr,model,fstr,sep="_")}
if(!perf){fileNameRoot = paste("trialRT_ANOVA",Tstr,model,fstr,sep="_")} # for constructing output filenames


if(!perf){
  if(avgs&factorial&model=="cond"){modelSource = "trialRT_ANOVA_subjMeansnomix_int.R"
                                   expFactors  = c('Response Mode','Leg Position','Hand Position','Intensity')
                                   factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Intensity (high – low)')}
  if(avgs&factorial&model=="condtask"){modelSource = "trialRT_ANOVA_subjMeansnomix_task.R"
  expFactors  = c('Response Mode','Leg Position','Hand Position','Task')
  factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Task (Anti – normal)')}
  if(avgs&factorial&model=="conf"){modelSource = "trialRT_ANOVA_subjMeansnomix_conf.R"
  expFactors  = c('Anatomical Conflict','Leg Position','Hand Position')
    factCont    = c('AC (yes – no)','Legs (cross – uncross)','Hands (cross – uncross)')
  }
}
  if(perf){
    if(model=="cond"){modelSource = "bern_ANOVA_allData.R"
                                    expFactors  = c('Response Mode','Leg Position','Hand Position','Intensity')
                                    factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Intensity (high – low)')}
    if(model=="condtask"){modelSource = "bern_ANOVA_subjMeansnomix_task.R"
    expFactors  = c('Response Mode','Leg Position','Hand Position','Task')
    factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Task (Anti – normal)')}
    
  }
 

# POSTERIOR DISTRIBUTIONS PLOTTING ------------------------------------------------------------------------------
if (runmodel){
  mcmcChain = list()
  thistask = 1
  for (thistask in c(1:length(tsks))){
    source("05_Code/02_Analysis/rcode/utilities/get_exp2_data.R")
    if(!perf){
     # source("05_Code/02_Analysis/rcode/plotting/rawPlot_Exp2.R")
     #  source("05_Code/02_Analysis/rcode/plotting/rawPlot_Exp2_prevT.R")
    }
    #if(perf){source("code/analysis/rcode/plotting/rawPlotperf_LH2cross.R")}
    # RUN THE CHAINS ------------------------------------------------------------------------
  
    parameters    = c("ySigma","m","b0","b1","b2","b3","b4","b1b2","b1b3","b2b3","b1b4","b2b4","b3b4",
                    "b1b2b3","b2b3b4","b1b3b4","b1b2b4","b1b2b3b4","bS","nu","mC",
                    "deviance")#,  "resid.sum.sq","loglik")  

    adaptSteps    <- 2000#5000   
    burnInSteps   <- 2000#5000     
    nChains       <- 4         
    numSavedSteps <- 10000#10000   
    thinSteps     <- 1#2       
  
    runjagsModel = run.jags(paste(path,"/05_code/02_analysis/rcode/models/",modelSource,sep=""), monitor=parameters, data=dataList,
                          adapt=adaptSteps, n.chains=nChains, thin=thinSteps,
                          method='parallel', burnin=burnInSteps, 
                          sample=numSavedSteps, 
                          #inits=initsList,  
                          psrf.target=1.05)#inits=initsList,
  
    # CHECK MODEL CONVERGENCE ---------------------------------------------------------------
    checkConvergence = T
    checkPartialPath = paste("/07_Analyses/", pathfig,"/bayes/checks/",sep='')
    source("05_Code/02_Analysis/rcode/utilities/convergenceChecks.R", chdir=T)
  
    # SAVE MODEL RESULT ---------------------------------------------------------------------
    mcmcChain[[thistask]] = as.matrix(as.mcmc(runjagsModel))
  }
  save(mcmcChain, file=paste(path,"/07_Analyses/mcmcData/Exp_",exp,'_',fileNameRoot,'.Rdata',sep=''))
}else{
  thistask = 1
  source("05_Code/02_Analysis/rcode/utilities/get_exp2_data.R")
}

# PREAMBLE ------------------------------------------------------------------------------
load(file=paste(path,"/07_Analyses/mcmcData/Exp_",exp,'_',fileNameRoot,'.Rdata',sep=''))
doublePlot=F
source("05_Code/02_Analysis/rcode/plotting/plotDataHDIint.R", chdir=T)         # plots data with HDI and table figure with all comparisons
if(model!="conf"){source("05_Code/02_Analysis/rcode/plotting/plotconddiff.R", chdir=T)} # all difference between conditions, only when using factorial model (not conf)
source("05_Code/02_Analysis/rcode/plotting/multiPlot.R")
if(model!="condtask"){
source("05_Code/02_Analysis/rcode/plotting/plotPost_trialRT_ANOVA.R", chdir=T) # plot factors and interactiions posteriors
source("05_Code/02_Analysis/rcode/utilities/makeTables.R", chdir=T)            # makes a pdf table
}
source('05_Code/02_Analysis/rcode/plotting/postpredcheck.R')                   # makes a figure with simulated data from the model

# double plot exp2
load(file=paste(path,"/07_Analyses/mcmcData/Exp_22_trialRT_ANOVA_subjMeans_condtask_factorial.Rdata",sep=''))
mcmcChain2 = mcmcChain 
load(file=paste(path,"/07_Analyses/mcmcData/Exp_22_trialRT_ANOVA_subjMeans_conf_factorial.Rdata",sep=''))
doublePlot=T
model = "conf"
source("05_Code/02_Analysis/rcode/plotting/plotDataHDIint.R", chdir=T)         # plots data with HDI and table figure with all comparisons

