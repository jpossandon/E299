# Data plot
library(LaplacesDemon)
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
cbbPaletteLine  <- c("#52958b","#002C54","#52958b","#002C54")
shapeLine  <- c("solid","dashed","solid","dashed")
cbbPalettexlabel  <- c("#662225","#B51D0A","#662225","#B51D0A")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
xpos        <- c(1,4,2,5,6,9,7,10)
grupvar     <- "LegC"
gap         <- .5
xlims       <- c(0,11.5)
rzfact      <-.075
rzfact2      <-.006
xpos[xpos>5]<- xpos[xpos>5]+gap
if (exp==1){
  tsks        <- c("Normal")}
cbbPalette1  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")
cbbPalette2  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")
if (exp==2){
  tsks        <- c("Normal","Anti")}
if (exp==22){
  tsks        <- c("Normal","Anti")}
if(exp==3){
  tsks        <- c("LH","HL")  
}
prf         <- c("Incorrect","Correct")


# RT all conditions, plots for correct and incorrect for both tasks
nsims=100
for (c in c(1)){
  for (t in c(1:length(tsks))){
    if (exp==2 & t==1){cbbPalette1  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")
    cbbPalette2  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")}
    if (exp==2 & t==2){cbbPalette1  <- c("#1F78B4","#1F78B4","#1F78B4","#1F78B4","#1F78B4","#E31A1C","#E31A1C","#1F78B4")
    cbbPalette2  <- c("#E31A1C","#1F78B4","#1F78B4","#E31A1C","#E31A1C","#E31A1C","#E31A1C","#E31A1C")}
    if (exp==22){
      this_mcmcChain <- mcmcChain[[1]]}else{
        this_mcmcChain <- mcmcChain[[t]]}
    ######################################## 
    ########################################
    # plot RT per condition and response mode
    ########################################
    ########################################
    datFrameaux = subset(datFrame,task==t)
    jitterVal   <- runif(length(unique(datFrameaux$subjIndx)), max = 0.4)-.2
    
    datFrameaux = subset(datFrameaux,trial_RT>.150)
    datFrameaux = subset(datFrameaux,trial_correct==c)
    dat1        = ddply(datFrameaux, .(subjIndx,cond,RespM,LegC,HandC,cLxL,cSxB), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
    datperInt   = ddply(datFrameaux, .(subjIndx,cond,trial_int), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
    openGraph(width = 3.5, height = 3) 
    # fig <- image_graph(width = 400, height = 400, res = 96)
    p1 <- ggplot()  + 
      theme_bw() +
      theme(axis.line         = element_line(colour = "black"),
            axis.line.y         = element_line(color="black"),
            axis.line.x         = element_line(color="black"),
            panel.grid.minor.y  = element_line(size = .3),
            panel.grid.major.y  = element_line(size = .3),
            panel.grid.major.x  = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.border        = element_blank(),
            panel.background    = element_blank(),
            #axis.title.x        = element_blank(),
            #    axis.text.x         = element_text(color=cbbPalettexlabel),
            plot.margin         = unit(c(2, 2, 2, 2), "mm"),
            text                = element_text(family="Helvetica",size=10))
      xs <-1
    dat2 = ddply(dat1, .(cond), summarize,  meanRT=mean(meanRT, na.rm=T),  median=median(meanRT, na.rm=T))
    
    for (ss in levels(dat1$cond)){               # loop through each condition
      auxdat1 <- dat1[dat1$cond %in% ss,]
      auxdat1$Condition <- xpos[xs]              # position in the plot
      uniqSubj <- unique(datFrame$subjIndx) %in% auxdat1$subjIndx
      auxdat1$ConditionJit = auxdat1$Condition+jitterVal[uniqSubj]
      thispos    <-  unique(auxdat1$Condition) 
      if (exp==22 & model=="condtask"){
        auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d,%s]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC),t)]
        auxsd   <- this_mcmcChain[,sprintf("ySigma[%d,%d,%d,1]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC),t)]
      }
      if(exp==22 & model=="conf"){
        auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d]",unique(auxdat1$cLxL),unique(auxdat1$LegC),unique(auxdat1$HandC))]
        auxsd   <- this_mcmcChain[,sprintf("ySigma[%d,%d,%d]",unique(auxdat1$cLxL),unique(auxdat1$LegC),unique(auxdat1$HandC),t)]
      }
      if(exp!=22){
      auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC))]
      auxsd   <- sqrt(((this_mcmcChain[,sprintf("ySigma[%d,%d,%d,1]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC))])^2+
        (this_mcmcChain[,sprintf("ySigma[%d,%d,%d,2]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC))])^2)/2)
      }
      auxnu   <- this_mcmcChain[,"nu"]
      postMd      = with(density(auxmcmc, adjust=2 ),data.frame(x,y))
       postMd$y2   = thispos-(postMd$y*rzfact2)
      postMd$y    = (postMd$y*rzfact2)+thispos
      HDI95post   = HDIofMCMC(auxmcmc,.95)  
      dat2$meanPost[dat2$cond==ss] <-mean(auxmcmc)
      auxmcmcP <- data.frame(x=unique(auxdat1$Condition),mean=mean(auxmcmc),ymin=HDIofMCMC(auxmcmc,.95)[1],ymax=HDIofMCMC(auxmcmc,.95)[2])
      nsuj = length(unique(auxdat1$subjIndx))
      xVec = seq(mean(auxdat1$meanRT)-4*mean(auxsd),mean(auxdat1$meanRT)+4*mean(auxsd),length=50)
      
      # for (nn in 1:nsims){
      #   samV = sample(1:length(auxsd),1)
      #   dtaux = data.frame(x=xVec,y=thispos+.1+rzfact*dt((xVec-auxmcmc[samV])/auxsd[samV],df=auxnu[samV])/auxsd[samV])
      #   rtaux = data.frame(x=auxmcmc[samV]-rt(nsuj,df=auxnu[samV])*auxsd[samV],y=thispos+jitterVal)
      #    p1 <- p1 + geom_line(data=dtaux,aes(x=x,y=y),color="blue",size=.1) +
      #     geom_point(data=rtaux,aes(y=y, x=x),size=.4,shape=20,color="black", stroke=.1)
      # }
     dfsim <-data.frame(x=numeric(),y=numeric(),nsim=numeric())
      for (nn in 1:nsims){
        samV = sample(1:length(auxsd),1)
        for (suj in 1:nsuj){
          # WHAT IS THE DIFFERENCE BETWEEN RT AND RST?
          dfsim[nrow(dfsim)+1,] = list(x=auxmcmc[samV]+this_mcmcChain[samV,paste0("bS[",suj,"]")][[1]]+rt(1,df=auxnu[samV])*auxsd[samV],y=thispos+runif(1, max = 0.4)-.2,nsim=nn)
          #dfsim[nrow(dfsim)+1,] = list(x=auxmcmc[samV]+this_mcmcChain[samV,paste0("bS[",suj,"]")][[1]]+rnorm(1,mean=0,sd=auxsd[samV]),y=thispos+runif(1, max = 0.4)-.2,nsim=nn)
       #   dfsim[nrow(dfsim)+1,] = list(x=rst(1,mu=auxmcmc[samV],sigma=auxsd[samV],nu=auxnu[samV]),y=thispos+runif(1, max = 0.4)-.2,nsim=nn)
        }
      }
     p1 <- p1 + geom_point(data=dfsim,aes(y=y, x=x),size=.4,shape=20,color="black", stroke=.1)

      
      p1 <- p1 +
        geom_point(data=auxdat1,aes(y=ConditionJit, x=meanRT),fill='red',size=1.5,shape=21, stroke=.2, alpha = .7) +         # plot each subjecy
        geom_segment(data=auxmcmcP,aes(x=mean,xend=mean),y=min(postMd$y2),yend=max(postMd$y),size=.25,color="red") +  # plot mean
        geom_point(data=auxdat1,aes(y=Condition,x=mean(meanRT)),size=1.25,shape=23,color="black",fill="yellow", stroke=.1) + # plot mean
        geom_point(data=auxdat1,aes(y=Condition,x=mean(meanRT)),size=.05,shape=20,color="black", stroke=.1)+  # plot mean
        coord_flip()
      
      xs <- xs +1
    }
    
    dat2$numcond <-xpos[as.numeric(dat2$cond)]
    #dat2$numcond[dat2$numcond>4] <- dat2$numcond[dat2$numcond>4]+gap
    #dat2$paircond <-c(1,2,3,4,1,2,3,4)
    dat2$paircond <-c(1,1,2,2,3,3,4,4)
    for (ss in c(1,2,3,4)){
      p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(y=numcond, x=meanPost), linetype=shapeLine[ss],size=.3),p1$layers)
      # p1 <- p1 + 
      #   geom_text(data=NULL,aes(x = 6.5+gap), y = 0+ss/20,label = crossLabels[ss],hjust=0,
      #             size=2.5,color=cbbPalette[ss])
    }
    # line between subject data in the same response mode
  
    
    p1 <- p1 + 
      scale_x_continuous(limits=c(0.138,1.062),breaks=c(0,.2,.4,.6,.8,1),labels=1000*c(0,.2,.4,.6,.8,1),expand = c(0, 0)) +
      scale_y_continuous(limits=xlims,expand = c(0, 0),
                         breaks=c(1.5,4.5,6.5+gap,9.5+gap),labels=c('uH','cH','uH','cH'))+
      xlab('Reaction Time (ms)')+
      ylab('External                  Anatomical')
    #   ggtitle(sprintf('Task %s, %s reponses, N = %d',tsks[t],prf[c+1],length(unique(datFrameaux$subjIndx))))
    # 
    print(p1)
    saveGraph(file=paste(getwd(),"/07_Analyses/",pathfig[t],"/postpredCheck",tsks[t],prf[c+1],model,sep=""), type="pdf")
    
    
  }
}
