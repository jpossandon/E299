# analysis with BMRS

# PREAMBLE ------------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))

path       = "/Users/jossando/trabajo/E299"
setwd(path)

source("05_Code/02_Analysis/rcode/utilities/openGraphSaveGraph.R", chdir=T) # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("05_Code/02_Analysis/rcode/utilities/HDIofMCMC.R", chdir=T)
source("05_Code/02_Analysis/rcode/utilities/DBDA2E-utilities.R", chdir=T)
#require(runjags)
require(ggplot2)
require(plyr)
require(yarrr)
library(extrafont)
require(plotrix)
require(gridExtra)
library(magick)
library(gtools)
library('brms')
perf          = FALSE
avgs          = TRUE
factorial     = TRUE

model         = "cond" # cond
exp           = 1

if(exp==1){
  rem_subjects = c(3,9,13)
  tsks        <- c("Normal")
  pathfig = "LH2cross"}
if(exp==2){
  rem_subjects = c()
  pathfig  = c("LH2crossExp2","LH2crossAnti")
  tsks        <- c("Normal","Anti")}
if(exp==3){
  rem_subjects = c()
  pathfig  = c("LH3cross","HL3cross")
  tsks        <- c("Normal","HL")}

if(avgs){Tstr='subjMeans'}
if(!avgs){Tstr='allData'}
if(factorial){fstr='factorial'}
if(!factorial){fstr='simpleEffect'}

#if(logn){Tstr='logNorm'}
#if(!logn){Tstr='Norm'}

if(perf){fileNameRoot = paste("bern_ANOVA",Tstr,model,fstr,sep="_")}
if(!perf){fileNameRoot = paste("trialRT_ANOVA",Tstr,model,fstr,sep="_")} # for constructing output filenames


if(!perf){
  # if(!avgs&factorial&model=="cond"&exp==2){modelSource = "trialRT_ANOVA_Exp2_allData.R"}
  #  if(!avgs&factorial&model=="cond"&exp==1){modelSource = "trialRT_ANOVA_allData.R"}
  
  if(avgs&factorial&model=="cond"){modelSource = "trialRT_ANOVA_subjMeansnomix_int.R"
  expFactors  = c('Response Mode','Leg Position','Hand Position','Intensity')
  factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Intensity (high – low)')}
  if(avgs&factorial&model=="condtask"){modelSource = "trialRT_ANOVA_subjMeansnomix_task.R"
  expFactors  = c('Response Mode','Leg Position','Hand Position','Task')
  factCont    = c('Response (anat. – ext.)','Legs (cross – uncross)','Hands (cross – uncross)','Task (Anti – normal)')}
  
}

mcmcChain = list()
thistask = 1
for (thistask in c(1:length(tsks))){
  source("05_Code/02_Analysis/rcode/utilities/get_exp2_data.R")
  fit1 = brm(meanRT ~ RespM*LegC*HandC, data = datFrameOK, iter = 2000, warmup = 500, chains = 4,
                  thin = 2, refresh = 0, prior = c(prior(normal(0, 100), class = "Intercept"),
                                                   prior(normal(0, 100), class = "b"), prior(cauchy(0, 5), class = "sigma")))
  marginal_effects(fit1)
}