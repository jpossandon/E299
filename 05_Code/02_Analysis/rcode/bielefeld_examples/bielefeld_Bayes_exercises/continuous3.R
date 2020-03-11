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

# this time we take all four conditions
dFc$cond   = factor(dFc$cond )
cond       = as.numeric(dFc$cond)

# data for RJAGS as a list
dataList = list(y          = dFc$mRT,
                cond       = cond,
                nCond      = length(unique(cond)),
                nTotal     = length(dFc$mRT),
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
    mu[i] <- a0 + a1[cond[i]]
}
  sigma ~ dunif(SPriorL, SPriorH )
  a0 ~ dnorm(MPriormean,1/MPriorstd^2)
  
for ( j in 1:nCond ) { 
    a1[j] ~ dnorm(0, 1/(aSigma)^2) 
    }
  aSigma ~ dgamma( aGammaShRa[1] , aGammaShRa[2] ) 
 
 # Convert a0,a[] to sum-to-zero b0,b[] :
  for ( j in 1:nCond ) { m[j] <- a0 + a1[j] } # cell means 
  b0 <- mean( m[1:nCond] )
  for ( j in 1:nCond ) { b1[j] <- m[j] - b0 }
}"

writeLines(modelString, con="model3.txt" )

# run the model
parameters    = c("b0","b1","sigma","aSigma")
runjagsModel = run.jags("model3.txt",
                 data     = dataList,
                 adapt    = 1000,
                 monitor  = parameters,
                 n.chains = 4,
                 burnin   = 1000,
                 sample   = 10000)

# check convergence
 codaSamples = as.mcmc.list(runjagsModel)
 mcmcChain = as.matrix(as.mcmc(runjagsModel))
 for ( parName in colnames(codaSamples[[1]]) ) {
      diagMCMC( codaObject=codaSamples, parName=parName)
   # saveGraph(file=paste(path,"/figures/","convergence10000",parName,sep=""), type="png")
    }

 # this is an example of plotting parameter of specific conditions
 # plus the intercept parameter, plot the other condtion effect sizes and decide which are credible differences
 openGraph(4,3)
 plotPost( mcmcChain[,"b0"]+mcmcChain[,"b1[1]"] , main="b0+b1[1]" ,
           cenTend="mean", credMass=0.95)
 
 openGraph(4,3)
 plotPost( mcmcChain[,"b0"]+mcmcChain[,"b1[2]"] , main="b0+b1[2]" ,
           cenTend="mean", credMass=0.95)
 
  openGraph(4,3)
  plotPost( mcmcChain[,"b1[2]"]-mcmcChain[,"b1[1]"] , main="b1[2]-b1[1]" ,
            cenTend="mean", credMass=0.95, ROPE = c(-.01,.01), compVal=0 )
  saveGraph(file=paste(path,"/figures/","example3b12-b11",sep=""), type="png")
  openGraph(4,3)
  plotPost( mcmcChain[,"sigma"] , main="sigma" ,
            cenTend="mean", credMass=0.95)
  
