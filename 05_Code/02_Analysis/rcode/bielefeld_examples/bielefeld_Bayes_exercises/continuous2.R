# set path and require and source necessary functions and packages
path = "/Users/jossando/trabajo/E299/05_Code/02_Analysis/rcode/bielefeld_examples/bielefeld_Bayes_exercises/"
setwd(path)
source("openGraphSaveGraph.R") # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("HDIofMCMC.R")
source("DBDA2E-utilities.R")
require(runjags)
require(ggplot2)

# Get the data

load(paste(path,"/dataE299_bielefeld.Rdata",sep=''))

# this time we take two conditions
dFc        = subset(dFc, cond==c("RaLuHu","RaLuHc"))
dFc$cond   = factor(dFc$cond )
cond       = as.numeric(dFc$cond)

# data for RJAGS as a list
dataList = list(y          = dFc$mRT,
                cond       = cond,
                nCond      = length(unique(cond)),
                nTotal     = length(dFc$mRT),
                MPriormean = mean(dFc$mRT),
                MPriorstd  = sd(dFc$mRT)*100,
                SPriorL    = sd(dFc$mRT)/1000,
                SPriorH    = sd(dFc$mRT)*1000)

# where do initialize the MCMC
initsList = list(mu    = c(mean(dFc$mRT[dFc$cond=="RaLuHu"]),mean(dFc$mRT[dFc$cond=="RaLuHc"])), 
                 sigma =  c(sd(dFc$mRT[dFc$cond=="RaLuHu"]),sd(dFc$mRT[dFc$cond=="RaLuHc"])))

# model specification as a strin
modelString = 'model{
  for ( i in 1:nTotal ) {
     y[i] ~ dt( mu[cond[i]] , 1/sigma[cond[i]]^2 , nu )
  }
  for ( j in 1:nCond ) {
    mu[j] ~ dnorm(MPriormean, 1/(MPriorstd)^2) 
    sigma[j] ~ dunif(SPriorL, SPriorH )
  }
  nu <- nuMinusOne+1
  nuMinusOne ~ dexp(1/29)
}'
writeLines(modelString, con="model2.txt" )

# run the model
parameters    = c("mu","sigma","nu")
runjagsModel = run.jags("model2.txt",
                 data     = dataList,
                 adapt    = 1000,
                 monitor  = parameters,
                 n.chains = 4,
                 burnin   = 1000,
                 sample   = 10000,
                 inits    = initsList)

# check convergence, run the model again with adapt=1,burn=1, 
# and different amount of samples and check again
 codaSamples = as.mcmc.list(runjagsModel)
 mcmcChain = as.matrix(as.mcmc(runjagsModel))
  for ( parName in colnames(codaSamples[[1]]) ) {
      diagMCMC( codaObject=codaSamples, parName=parName)
 #   saveGraph(file=paste(path,"/figures/","convergence_",parName,sep=""), type="png")
    }

 # Example of parameter plots, make the rest, the differences and effect sizes
  openGraph(4,3)
  plotPost( mcmcChain[,"mu[1]"] , main="mu1" ,
            cenTend="mean", credMass=0.95 )

  openGraph(4,3)
  plotPost( mcmcChain[,"sigma[1]"], main="sigma1" ,
            cenTend="mean", credMass=0.95 )

  