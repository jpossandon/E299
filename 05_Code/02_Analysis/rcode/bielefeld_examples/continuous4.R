# set path and require and source necessary functions and packages
path = "/Users/jossando/trabajo/E299/05_Code/02_Analysis/rcode/bielefeld_examples"
setwd(path)
source("openGraphSaveGraph.R") # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("HDIofMCMC.R")
source("DBDA2E-utilities.R")
require(runjags)
require(ggplot2)

# Get the data

load(paste(path,"/dataE299_bielefeld.Rdata",sep=''))

# this time we take all four conditions
dFc$cond   = factor(dFc$cond )
cond       = as.numeric(dFc$cond)
subj       = as.numeric(as.factor(dFc$subjIndx))
# another column to make condition 2-3-4 as only one condition
cond2      = cond
cond2[cond>2] = 2 

# data for RJAGS as a list
dataList = list(y          = dFc$mRT,
                cond       = cond,
                cond2      = cond2,
                subj       = subj,
                nCond      = length(unique(cond)),
                nTotal     = length(dFc$mRT),
                nSubj      = length(unique(subj)),
                MPriormean = mean(dFc$mRT),
                MPriorstd  = sd(dFc$mRT)*10,
                SPriorL    = sd(dFc$mRT)/100,
                SPriorH    = sd(dFc$mRT)*100,
                aGammaShRa = gammaShRaFromModeSD(mode = sd(dFc$mRT)/2,sd = sd(dFc$mRT)*2))

# model specification as a strin
modelString = "
model {
  for ( i in 1:nTotal ) {
    y[i]  ~ dnorm( mu[i] , 1/sigma^2)
    #mu[i] <- a0 + a1[cond[i]] + aS[subj[i]]
    # folowwing lines are for model comparison
    mucond[i] <- a0 + a1[cond[i]] + aS[subj[i]]
    mucond2[i] <- a0 + a1[cond2[i]]  + aS[subj[i]]
    mu[i]  <- equals(mC,1)*mucond[i] + equals(mC,2)*mucond2[i]
    }
  sigma ~ dunif(SPriorL, SPriorH )
  a0 ~ dnorm(MPriormean,1/MPriorstd^2)
  
  for ( j in 1:nCond ) { 
    a1[j] ~ dnorm(0, 1/(aSigma)^2) 
    }
  aSigma ~ dgamma( aGammaShRa[1] , aGammaShRa[2] ) 

  for ( j in 1:nSubj ) { aS[j] ~ dnorm( 0.0 , 1/(sSigma)^2 ) }
  sSigma ~ dgamma( aGammaShRa[1] , aGammaShRa[2] ) 

 # Convert a0,a[] to sum-to-zero b0,b[] :
  for ( j in 1:nCond ) { for (s in 1:nSubj){
            m[j,s] <- a0 + a1[j] + aS[s] }} # cell means 
  b0 <- mean( m[1:nCond,1:nSubj] )
  for ( jS in 1:nSubj ) { bS[jS] <- mean( m[1:nCond, jS] ) - b0}
  for ( jC in 1:nCond ) { b1[jC] <- mean( m[jC,1:nSubj] ) - b0}
  
  # this is for model comparison
    mC ~ dcat( mPriorProb[] )
    mPriorProb[1] <- .5
    mPriorProb[2] <- .5
}"

writeLines(modelString, con="model4.txt" )

# run the model
parameters    = c("b0","b1","sigma","aSigma","bS","mC")
runjagsModel = run.jags("model4.txt",
                 data     = dataList,
                 adapt    = 0,
                 monitor  = parameters,
                 n.chains = 4,
                 thin     = 4,
                 burnin   = 0,
                 sample   = 10)

# check convergence
 codaSamples = as.mcmc.list(runjagsModel)
 mcmcChain = as.matrix(as.mcmc(runjagsModel))
# for ( parName in colnames(codaSamples[[1]]) ) {
 #     diagMCMC( codaObject=codaSamples, parName=parName)
   # saveGraph(file=paste(path,"/figures/","convergence10000",parName,sep=""), type="png")
#   }

 openGraph(4,3)
 plotPost( mcmcChain[,"b0"]+mcmcChain[,"b1[1]"] , main="b0+b1[1]" ,
           cenTend="mean", credMass=0.95)
 saveGraph(file=paste(path,"/figures/","example4b11",sep=""), type="png")
 openGraph(4,3)
 plotPost( mcmcChain[,"b0"]+mcmcChain[,"b1[2]"] , main="b0+b1[2]" ,
           cenTend="mean", credMass=0.95)
 saveGraph(file=paste(path,"/figures/","example4b12",sep=""), type="png")
 
  openGraph(4,3)
  plotPost( mcmcChain[,"b1[2]"]-mcmcChain[,"b1[1]"] , main="b1[2]-b1[1]" ,
            cenTend="mean", credMass=0.95, ROPE = c(-.01,.01), compVal=0 )
  saveGraph(file=paste(path,"/figures/","example4b12-b11",sep=""), type="png")
  
  openGraph(4,3)
  plotPost( mcmcChain[,"sigma"] , main="sigma" ,
            cenTend="mean", credMass=0.95)
  saveGraph(file=paste(path,"/figures/","example4sigma",sep=""), type="png")
  
effect_size = (mcmcChain[,"b1[2]"]-mcmcChain[,"b1[1]"])/mcmcChain[,"sigma"]
openGraph(4,3)
plotPost( effect_size, main="ES" ,
          cenTend="mean", credMass=0.95 , ROPE = c(-.1,.1), compVal=0 )
saveGraph(file=paste(path,"/figures/","example3EF",sep=""), type="png")
# 