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

# this time we take two conditions
dFc        = subset(dFc, cond==c("RaLuHu","RaLuHc"))
dFc[nrow(dFc) + 1,] = list("64","RaLuHu",1,mean(dFc$mRT[dFc$cond=="RaLuHu"]))
dFc[nrow(dFc) + 1,] = list("64","RaLuHc",1,2.5*mean(dFc$mRT[dFc$cond=="RaLuHc"]))
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
   # y[i] ~ dnorm(mu[cond[i]], 1/sigma[cond[i]]^2) 
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

# check convergence
 codaSamples = as.mcmc.list(runjagsModel)
 mcmcChain = as.matrix(as.mcmc(runjagsModel))
 # for ( parName in colnames(codaSamples[[1]]) ) {
 #     diagMCMC( codaObject=codaSamples, parName=parName)
 #   saveGraph(file=paste(path,"/figures/","convergence10000",parName,sep=""), type="png")
 #   }

  openGraph(4,3)
  plotPost( mcmcChain[,"sigma[1]"] , main="sigma1" ,
            cenTend="mean", credMass=0.95 )
  saveGraph(file=paste(path,"/figures/","example2sigma1",sep=""), type="png")
  openGraph(4,3)
  plotPost( mcmcChain[,"sigma[2]"], main="sigma2" ,
            cenTend="mean", credMass=0.95 )
  saveGraph(file=paste(path,"/figures/","example2sigma2",sep=""), type="png")
  openGraph(4,3)
  plotPost( mcmcChain[,"mu[1]"]-mcmcChain[,"mu[2]"] , main="mu1-mu2" ,
            cenTend="mean", credMass=0.95 )
  saveGraph(file=paste(path,"/figures/","example2mu1-mu2",sep=""), type="png")
  # plot the rest of the parameters and their differences (sigma,nu), plot the effect size
effect_size = (mcmcChain[,"mu[1]"]-mcmcChain[,"mu[2]"])/sqrt((mcmcChain[,"sigma[1]"]^2+mcmcChain[,"sigma[2]"]^2)/2)
openGraph(4,3)
plotPost( effect_size, main="ES" ,
          cenTend="mean", credMass=0.95 )
saveGraph(file=paste(path,"/figures/","example2EF",sep=""), type="png")
# post_effectSize      = with(density(effect_size, adjust=2 ),data.frame(x,y))
# HDI95post_effectSize = HDIofMCMC(effect_size,.95)
# p1 <- ggplot() +  theme_bw() +
#   geom_ribbon(data=post_effectSize,aes(x=x,ymax=y),ymin=0,alpha=.75,size=.1) +
#   geom_ribbon(data=subset(post_effectSize,x>HDI95post_effectSize[1] & x<HDI95post_effectSize[2]),aes(x=x,ymax=y),fill="#006D2C", ymin=0,alpha=.75,size=.1)
# openGraph(4,3)
# print(p1)