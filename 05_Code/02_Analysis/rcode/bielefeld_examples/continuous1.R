# set path to your bielefeld_example folder and require and source necessary functions and packages
path = "/Users/jossando/trabajo/E299/05_Code/02_Analysis/rcode/bielefeld_examples/"
setwd(path)
source("openGraphSaveGraph.R") # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
source("HDIofMCMC.R")
source("DBDA2E-utilities.R")
require(runjags)
require(ggplot2)

# Get the data
load(paste(path,"dataE299_bielefeld.Rdata",sep=''))

# in this example we look only the data of one condition
dFc   = subset(dFc, cond=="RaLuHu")
#dFc <- dFc[dFc$subjIndx %in% c(31,32,33,36,37,38,49,40,41,42,43),] #selection of subjects

# put the data for RJAGS in a list
dataList = list(y         = dFc$mRT,
                nTotal    = length(dFc$mRT),
                stdData   = sd(dFc$mRT),       # this one is not necessary for running the model below
                meanData  = mean(dFc$mRT),     # this one is not necessary for running the model below
                MPriormean = mean(dFc$mRT),
                MPriorstd  = sd(dFc$mRT)*100,
                SPriorL    = sd(dFc$mRT)/1000,
                SPriorH    = sd(dFc$mRT)*1000)

# we make a list with the values where we want to initialize the MCMC
initsList = list(mu    = mean(dFc$mRT), 
                 sigma = sd(dFc$mRT))

# model specification as a string (note: the normal density take a precision=1/sigma^2 paramter instead of directly sigma)
modelStr = 'model{
  for ( i in 1:nTotal ) {
    y[i] ~ dnorm(mu, 1/sigma^2)    
  }
  mu ~ dnorm(MPriormean, 1/(MPriorstd)^2) 
  sigma ~ dunif(SPriorL, SPriorH )
}'
# get's saved as a text file in the current folder, go and look at it!
writeLines(modelStr, con="model1.txt" )

# this are the model parameters we want to get from the modelling, 
# that is, the distribution of posterior values for the mean and standard deviation
parameters    = c("mu","sigma")

# run the model
runjagsModel = run.jags("model1.txt",        # the model
                 data     = dataList,        # the data  
                 sample   = 10000,           # how many sample are we making
                 monitor  = parameters,      # the parameters we want
                 inits    = initsList,       # were to start the models
                 n.chains = 4,               # how many chains (independent runs of the model), total number of samples is n.chains*sample
                 adapt    = 1000,            # how many intial samples are used to adapt some parameters of the sample, we look a this in another example
                 burnin   = 1000)            # how many samples after adapt are discarded before starting to take sample for the posterior, we look a this in another example

mcmcChain = as.matrix(as.mcmc(runjagsModel)) # this transform the result of the model to a matrix, each column is one parameter
codaSamples = as.mcmc.list(runjagsModel)

# check convergence, we look at this later in another exampple
# for ( parName in colnames(codaSamples[[1]]) ) {
#     diagMCMC( codaObject=codaSamples, parName=parName)
#   }

# Kruschke plots
# posterior of the mean
openGraph(4,3)
plotPost( codaSamples[,"mu"] , main="mu" , xlab=bquote(mu) ,
          cenTend="mean", credMass=0.95 )
# posterior of the sd
openGraph(4,3)
plotPost( codaSamples[,"sigma"] , main="sigma" , xlab=bquote(sigma) ,
          cenTend="median", credMass=0.95 )

# we look here the posterior distribution of the mean, but we could also look at the posterior of the SD
# plot sampling distribution, prior and posterior for Mean
# random values from prior

# prior density, as the prior is normal we sample random numbers 
# from a normal distirbution with the corresponding prior mean and SD values
priorM      = rnorm(dim(mcmcChain)[1], mean = dataList$MPriormean, sd = dataList$MPriorstd) 
priorMd     = with(density(priorM, adjust=2 ),data.frame(x,y))

# sampling distribution density, we sample random numbers 
# from a normal distirbution with the corresponding sample mean and standard error SD/sqrt(N) 
samplingM   = rnorm(dim(mcmcChain)[1], mean = dataList$meanData, sd = dataList$stdData/sqrt(dataList$nTotal)) # the *100 is to match the prior specified in the model
samplingMd  = with(density(samplingM, adjust=2 ),data.frame(x,y))

# and this is the posterior of the mean parameter, 
# in this case correspond to the first column of the mcmcChain
postMd      = with(density(mcmcChain[,1], adjust=2 ),data.frame(x,y))
HDI95post   = HDIofMCMC(mcmcChain[,1],.95)    # 95% HDI

# all densities into one dataframe
plotDF      = dplyr::bind_rows(list(prior=priorMd, sampling=samplingMd, post=postMd), .id = 'source')

# the plotting
p1 <- ggplot() +  theme_bw() + theme(legend.justification=c(1,1),legend.position=c(.99, .99)) +
  geom_point(data=dFc,aes(x=mRT,y=-1),shape=21,colour = "#000000",fill="#cccccc", size = 2,stroke=.2)+    # plotting of the data points
  geom_line(data=plotDF,aes(x=x,y=y,color=source),ymin=0,alpha=.75,size=1) +                              # a line for each density
  geom_ribbon(data=subset(postMd,x>HDI95post[1] & x<HDI95post[2]),aes(x=x,ymax=y),fill="#006D2C", ymin=0,alpha=.5,size=.1)+ # and a ribbon for the posterior 95% HDI
  scale_x_continuous(limits=c(0,1),expand = c(0, 0)) +                       #formatting
  scale_y_continuous(name='density',limits=c(0,40),expand = c(0, 0)) +
  scale_color_manual(values=c("#006D2C","#1F78B4","#E31A1C"))
openGraph(4,3)
print(p1)
#saveGraph(file=paste(path,"/figures/","continuous1",sep=""), type="pdf")
