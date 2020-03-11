# Data plot

cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
cbbPaletteLine  <- c("#52958b","#002C54","#52958b","#002C54")
shapeLine  <- c("solid","dashed","solid","dashed")
cbbPalettexlabel  <- c("#662225","#B51D0A","#662225","#B51D0A")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
inttype = 1

grupvar     <- "LegC"
gap         <- .5
xlims       <- c(0,11.5)
rzfact      <-.25
rzfact      <-.006

cbbPalette1  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")
cbbPalette2  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")

if (exp==1){
  tsks        <- c("Normal")
  ylimH <-1}

if (exp==2 || exp==22){
  tsks      <- c("Normal","Anti")
  ylimH      <-1.06}
if(exp==3){
  tsks        <- c("LH","HL")  
  ylimH      <-1.155}

prf         <- c("Incorrect","Correct")


# RT all conditions, plots for correct and incorrect for both tasks

for (c in c(1)){
  for (t in c(1:length(tsks))){
    if ((exp==2 | exp==22) & t==1){cbbPalette1  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")
                       cbbPalette2  <- c("#303030","#E31A1C","#E31A1C","#303030","#303030","#1F78B4","#1F78B4","#303030")}
    if ((exp==2 | exp==22) & t==2){cbbPalette1  <- c("#1F78B4","#1F78B4","#1F78B4","#1F78B4","#1F78B4","#E31A1C","#E31A1C","#1F78B4")
                       cbbPalette2  <- c("#E31A1C","#1F78B4","#1F78B4","#E31A1C","#E31A1C","#E31A1C","#E31A1C","#E31A1C")}
    
    if (inttype==1){
      if (t==1 | ((exp==2 | exp==22) & t==2)){
      xpos        <- c(1,4,2,5,6,9,7,10)
      ylabels     <- c('uH','cH','uH','cH')}
      if (exp==3 & t==2){
        xpos        <- c(1,2,4,5,6,7,9,10)
        ylabels     <- c('uL','cL','uL','cL')}
      xlab = 'External                  Anatomical'}else{
        xpos        <- c(1,4,6,9,2,5,7,10)
        xlab = 'ul                        cL'}
    
    xpos[xpos>5]<- xpos[xpos>5]+gap
    if (exp==22){
      this_mcmcChain <- mcmcChain[[1]]
      if (doublePlot){this_mcmcChain2 <- mcmcChain2[[1]]}
      }else{
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
    if(model=="condtask"){
      dat1        = ddply(datFrameaux, .(subjIndx,cond,RespM,LegC,HandC,task), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
      }else{
    dat1        = ddply(datFrameaux, .(subjIndx,cond,RespM,LegC,HandC,cLxL,cSxB), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))}
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
    # p1 <- p1 + geom_rect(aes(xmin = c(0,6), xmax = c(3,9), ymin = c(-Inf,-Inf), ymax = c(Inf,Inf)),
    #           fill = "pink", alpha = 0.3)+
    #            geom_rect(aes(xmin = c(3,9), xmax = c(6,12), ymin = c(-Inf,-Inf), ymax = c(Inf,Inf)),
    #             fill = "cyan", alpha = 0.3)
    xs <-1
    dat2 = ddply(dat1, .(cond), summarize,  meanRT=mean(meanRT, na.rm=T),  median=median(meanRT, na.rm=T))
    
    for (ss in levels(dat1$cond)){               # loop through each condition
      auxdat1 <- dat1[dat1$cond %in% ss,]
      auxdat1$Condition <- xpos[xs]              # position in the plot
      uniqSubj <- unique(datFrame$subjIndx) %in% auxdat1$subjIndx
      auxdat1$ConditionJit = auxdat1$Condition+jitterVal[uniqSubj]
      thispos    <-  unique(auxdat1$Condition) 
      if(avgs&factorial&model=="cond"){
      auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC))]}
      if(avgs&factorial&model=="condtask"){
        auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d,%d]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC),unique(auxdat1$task))]}
    if(avgs&factorial&model=="conf"){
      auxmcmc <- this_mcmcChain[,sprintf("m[%d,%d,%d]",unique(auxdat1$cLxL),unique(auxdat1$LegC),unique(auxdat1$HandC))]
    }

      postMd      = with(density(auxmcmc, adjust=2 ),data.frame(x,y))
     # postMd$y2   = thispos-(postMd$y/max(postMd$y)*rzfact)
    #  postMd$y    = (postMd$y/max(postMd$y)*rzfact)+thispos
      postMd$y2   = thispos-(postMd$y*rzfact)
      postMd$y    = (postMd$y*rzfact)+thispos
      HDI95post   = HDIofMCMC(auxmcmc,.95)  
      dat2$meanPost[dat2$cond==ss] <-mean(auxmcmc)
      auxframe <- data.frame(x=unique(auxdat1$Condition),mean=mean(auxmcmc),ymin=HDIofMCMC(auxmcmc,.95)[1],ymax=HDIofMCMC(auxmcmc,.95)[2])
      
      if(doublePlot){
        auxmcmc2 <- this_mcmcChain2[,sprintf("m[%d,%d,%d,%d]",unique(auxdat1$RespM),unique(auxdat1$LegC),unique(auxdat1$HandC),t)]
      
        postMd2      = with(density(auxmcmc2, adjust=2 ),data.frame(x,y))
        # postMd$y2   = thispos-(postMd$y/max(postMd$y)*rzfact)
        #  postMd$y    = (postMd$y/max(postMd$y)*rzfact)+thispos
        postMd2$y2   = thispos-(postMd2$y*rzfact)
        postMd2$y    = (postMd2$y*rzfact)+thispos
        HDI95post2   = HDIofMCMC(auxmcmc2,.95)  
       # dat2$meanPost[dat2$cond==ss] <-mean(auxmcmc)
        auxframe2 <- data.frame(x=unique(auxdat1$Condition),mean=mean(auxmcmc2),ymin=HDIofMCMC(auxmcmc2,.95)[1],ymax=HDIofMCMC(auxmcmc2,.95)[2])
         p1 <- p1 +
            geom_ribbon(data=postMd2,aes(x=x,ymax=y), ymin=thispos,fill=NA,linetype="solid",colour="black",alpha=.5,size=.1)+ # and a ribbon for the posterior 95% HDI
            geom_ribbon(data=postMd2,aes(x=x,ymax=y2), ymin=thispos,fill=NA,linetype="solid",colour="black",alpha=.5,size=.1)+
         geom_segment(data=auxframe2,aes(x=mean,xend=mean),y=min(postMd2$y2),yend=max(postMd2$y),size=.1,color="black")   # plot mean
           
        }
      p1 <- p1 +
         geom_point(data=auxdat1,aes(y=ConditionJit, x=meanRT),fill='gray',size=1.5,shape=21, stroke=.1, alpha = .2) +         # plot each subjecy
       #  geom_errorbar(data=auxmcmc,aes(x=x,ymin=ymin,ymax=ymax),width=.35,size=.3)+ #,color=cbbPaletteLine[unique(auxdat1[,grupvar])]
      #  geom_point(data=auxmcmc,aes(x=x, y=mean),color='black',fill=cbbPalette[xs],size=2.5,shape=22, stroke=.1) +       # plot mean
      #  geom_point(data=auxmcmc,aes(x=x, y=mean),color='black',size=2,shape="-", stroke=.2) +  
         geom_text(data=auxframe,aes(y = x, x=ymax+.025,label = sprintf('%d',round(mean*1000))),vjust=0.5,  # text RT
                  size=3)+
        geom_ribbon(data=postMd,aes(x=x,ymax=y),fill="grey40", ymin=thispos,alpha=.5)+ # and a ribbon for the posterior 95% HDI
        geom_ribbon(data=postMd,aes(x=x,ymax=y2),fill="grey40", ymin=thispos,alpha=.5)+
        geom_ribbon(data=subset(postMd,x>HDI95post[1] & x<HDI95post[2]),aes(x=x,ymax=y),fill=cbbPalette1[xs], ymin=thispos,alpha=.6,size=.1)+ # and a ribbon for the posterior 95% HDI
        geom_ribbon(data=subset(postMd,x>HDI95post[1] & x<HDI95post[2]),aes(x=x,ymax=y2),fill=cbbPalette2[xs], ymin=thispos,alpha=.6,size=.1) +
        #geom_segment(data=auxmcmc,aes(y=x-rzfact,yend=x+rzfact,x=mean,xend=mean),size=.25,color="black") +  # plot mean
        geom_segment(data=auxframe,aes(x=mean,xend=mean),y=min(postMd$y2),yend=max(postMd$y),size=.25,color="black") +  # plot mean
        geom_point(data=auxdat1,aes(y=Condition,x=mean(meanRT)),size=.75,shape=23,color="black",fill="yellow", stroke=.1) + # plot mean
        geom_point(data=auxdat1,aes(y=Condition,x=mean(meanRT)),size=.05,shape=20,color="black", stroke=.1)+  # plot mean
        coord_flip()
      
      xs <- xs +1
    }
    
   dat2$numcond <-xpos[as.numeric(dat2$cond)]
    #dat2$numcond[dat2$numcond>4] <- dat2$numcond[dat2$numcond>4]+gap
    #dat2$paircond <-c(1,2,3,4,1,2,3,4)
    #
   if (inttype==1){
     if (t==1 | ((exp==2 | exp==22) & t==2)){
     dat2$paircond <-c(1,1,2,2,3,3,4,4)
     wlines = c(1,2,3,4,5,6,7,8)}
     if (exp==3 & t==2){
       dat2$paircond <-c(1,2,1,2,3,4,3,4) 
       wlines = c(1,2,3,4,5,6,7,8)}
     }else{
      dat2$paircond <-c(1,1,3,3,2,2,4,4) 
      wlines = c(1,2,5,6,3,4,7,8)
    }
    for (ss in c(1,2,3,4)){
      p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(y=numcond, x=meanPost), linetype=shapeLine[ss],size=.3),p1$layers)
     }
    # line between subject data in the same response mode
    if (c==1){
      #auxdat3 <-dat1[dat1$cond %in% levels(dat1$cond)[1:4],]
      auxdat3 <-dat1[dat1$cond %in% levels(dat1$cond)[wlines[1:4]],]
      auxdat3$numcond <-xpos[as.numeric(auxdat3$cond)]
      auxdat3 <-auxdat3[order(auxdat3$numcond),]
      p1$layers <- c(geom_path(data=auxdat3,aes(y=numcond+rep(jitterVal,4), x=meanRT, group=subjIndx),color='gray88', alpha = 1 ,size=.2), p1$layers)
      #auxdat3 <-dat1[dat1$cond %in% levels(dat1$cond)[5:8],]
      auxdat3 <-dat1[dat1$cond %in% levels(dat1$cond)[wlines[5:8]],]
      auxdat3$numcond <-xpos[as.numeric(auxdat3$cond)]
      auxdat3 <-auxdat3[order(auxdat3$numcond),]
      p1$layers <- c(geom_path(data=auxdat3,aes(y=numcond+rep(jitterVal,4), x=meanRT, group=subjIndx),color='gray88', alpha = 1,size=.2),p1$layers)
    }
    
     p1 <- p1 + 
       geom_text(data=NULL,aes(x=1,y=xlims-.5),label="spatial conflict",color="#1F78B4",hjust=1,vjust=1,size=3)+
       geom_text(data=NULL,aes(x=.95,y=xlims-.5),label="anatomical conflict",color="#E31A1C",hjust=1,vjust=1,size=3)+
       scale_x_continuous(limits=c(0.145,ylimH),breaks=c(0,.2,.4,.6,.8,1),labels=1000*c(0,.2,.4,.6,.8,1),expand = c(0, 0)) +
       scale_y_continuous(limits=xlims,expand = c(0, 0),
                          breaks=c(1.5,4.5,6.5+gap,9.5+gap),labels=ylabels)+
       xlab('Reaction Time (ms)')+
       ylab(xlab)
       #ylab('External                  Anatomical')
    #   ggtitle(sprintf('Task %s, %s reponses, N = %d',tsks[t],prf[c+1],length(unique(datFrameaux$subjIndx))))
    # 
    print(p1)
    if (doublePlot){
      saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/allSubjMeansInt",tsks[t],prf[c+1],"confcond",sep=""), type="pdf")
      
    }else{
    saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/allSubjMeansInt",tsks[t],prf[c+1],model,sep=""), type="pdf")
}
   }
}



