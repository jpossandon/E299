# Data plot
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
xpos        <- c(1,2,3,4,5,6,7,8)
gap         <- 1
xlims       <- c(-.5,10.5)
xpos[xpos>4]<- xpos[xpos>4]+gap
if (exp==1){
  tsks        <- c("Normal")}
if (exp==2){
tsks        <- c("Normal","Anti")}
if(exp==3){
  tsks        <- c("LH","HL")  
}
if(exp==4){
  tsks        <- c("Normal")  
}
prf         <- c("Incorrect","Correct")
plotbytrialorder = 0
plotrtvscorrect  = 0
# RT all conditions, plots for correct and incorrect for both tasks

for (c in c(1,0)){
  for (t in c(1:length(tsks))){
    ######################################## 
    ########################################
    # plot RT per condition and response mode
    ########################################
    ########################################
    datFrameaux = subset(datFrame,task==t)
    jitterVal   <- runif(length(unique(datFrameaux$subjIndx)), max = 0.4)-.2
    
    datFrameaux = subset(datFrameaux,trial_RT>.150)
    datFrameaux = subset(datFrameaux,trial_correct==c)
    dat1        = ddply(datFrameaux, .(subjIndx,cond), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
    datperInt   = ddply(datFrameaux, .(subjIndx,cond,trial_int), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
    openGraph(width = 3.5, height = 3) 
    p1 <- ggplot()  + 
          theme_bw() +
          theme(axis.line         = element_line(colour = "black"),
              axis.line.y         = element_line(color="black"),
              axis.line.x         = element_line(color="black"),
             # panel.grid.minor.y  = element_blank(),
              panel.grid.major.x  = element_blank(),
              panel.grid.minor.x  = element_blank(),
              panel.border        = element_blank(),
              panel.background    = element_blank(),
              axis.title.x        = element_blank(),
              #axis.text.x         = element_text(size=14),
              plot.margin         = unit(c(2, 2, 2, 2), "mm"),
              text                = element_text(family="Helvetica",size=10))
    xs <-1
    for (ss in levels(dat1$cond)){               # loop through each condition
      auxdat1 <- dat1[dat1$cond %in% ss,]
      auxdat1$Condition <- xpos[xs]              # position in the plot
      auxdat2 <- datperInt[datperInt$cond %in% ss,]
      auxdat2$Condition <- NA               # position in the plot by intensity
      auxdat2$Condition[auxdat2$trial_int==1] <- xpos[xs]-.3
      auxdat2$Condition[auxdat2$trial_int==2] <- xpos[xs]+.3             
      uniqSubj <- unique(datFrame$subjIndx) %in% auxdat1$subjIndx
      auxdat1$ConditionJit = auxdat1$Condition+jitterVal[uniqSubj]
      p1 <- p1 +
    #  geom_line(data=auxdat1,aes(x=meanRT),stat="density")+
    #geom_violin(data=auxdat1,aes(x=Condition, y=meanRT),draw_quantiles = c(0.25, 0.5, 0.75),trim = FALSE,adjust = .9,scale='width')+
      geom_point(data=auxdat1,aes(x=ConditionJit, y=meanRT),fill=cbbPalette[xs],size=1.5,shape=21, stroke=.1, alpha = .3) +         # plot each subjecy
      geom_point(data=auxdat1,aes(x=Condition, y=mean(meanRT)),color='black',fill=cbbPalette[xs],size=2,shape=22, stroke=.25) +       # plot mean
      geom_errorbar(data=auxdat1,aes(x=Condition, ymin=mean(meanRT)-std.error(meanRT), ymax=mean(meanRT)+std.error(meanRT)),color='black',width=.2,size=.3)+
        
      geom_point(data=auxdat2[auxdat2$trial_int %in% 1 ,],aes(x=Condition, y=mean(meanRT)),color='black',fill=cbbPalette[xs],size=2,shape=22, stroke=.25) +       # plot mean
      geom_text(data=auxdat2[auxdat2$trial_int %in% 1 ,],aes(x=Condition, y=mean(meanRT)),label="L",hjust=0.5,size=1.25)+
      geom_point(data=auxdat2[auxdat2$trial_int %in% 2 ,],aes(x=Condition, y=mean(meanRT)),color='black',fill=cbbPalette[xs],size=2,shape=22, stroke=.25) +       # plot mean
      geom_text(data=auxdat2[auxdat2$trial_int %in% 2 ,],aes(x=Condition, y=mean(meanRT)),label="H",hjust=0.5,size=1.25)+
      geom_text(data=auxdat1,aes(x = Condition, y=mean(meanRT)+.06,label = sprintf('%d',round(mean(meanRT)*1000))),hjust=0.5,  # text RT
                  size=2)
        # geom_segment(data=auxdat1, aes(x=Condition-.25, y=mean(meanRT), xend=Condition+.25, yend=mean(meanRT)),
      #              color=cbbPalette[xs],size=1) +
        # geom_segment(data=auxdat1, aes(x=Condition-.25, y=median(meanRT), xend=Condition+.25, yend=median(meanRT)),
        #              color='black',size=1)
      xs <- xs +1
    }
  
    dat2 = ddply(dat1, .(cond), summarize,  meanRT=mean(meanRT, na.rm=T),  median=median(meanRT, na.rm=T))
    dat2$numcond <-as.numeric(dat2$cond)
    dat2$numcond[dat2$numcond>4] <- dat2$numcond[dat2$numcond>4]+gap
    dat2$paircond <-c(1,2,3,4,1,2,3,4)
    for (ss in c(1,2,3,4)){
      p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(x=numcond, y=meanRT, group=paircond),colour=cbbPalette[ss],size=.4),p1$layers)
      p1 <- p1 + 
         geom_text(data=NULL,aes(x = 6.5+gap), y = 0+ss/20,label = crossLabels[ss],hjust=0,
                  size=2.5,color=cbbPalette[ss])
    }
    # line between subject data in the same response mode
    if (c==1){
      auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[1:4],]
      auxdat1$numcond <-as.numeric(auxdat1$cond)
      p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5 ,size=.2), p1$layers)
      auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[5:8],]
      auxdat1$numcond <-as.numeric(auxdat1$cond)+gap
      p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5,size=.2),p1$layers)
    }
  
    p1 <- p1 + 
      scale_y_continuous(limits=c(0,1.2),breaks=c(0,.2,.4,.6,.8,1),expand = c(0, 0)) +
      scale_x_continuous(limits=xlims,expand = c(0, 0),
                         breaks=c(2.5,6.5+gap),labels=c('External','Anatomical'))+
      ylab('Reaction Time (Mean +- SEM)')+
      ggtitle(sprintf('Task %s, %s reponses, N = %d',tsks[t],prf[c+1],length(unique(datFrameaux$subjIndx))))
      
    print(p1)
    saveGraph(file=paste(getwd(),"/07_Analyses/",pathfig[t],"/allSubjMeans",tsks[t],prf[c+1],sep=""), type="pdf")
    ################################# 
    #################################
    
    
    
   
    ################################# 
    #################################
    # by trial order
    #################################
    #################################
    if (plotbytrialorder){
  
      datext    = ddply(datFrameaux[datFrameaux$cond %in% c("ReLuHu","ReLuHc","ReLcHu","ReLcHc"),]
                        , .(subjIndx,cond,trialnr), summarize,
                        meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
      datext$subjCond = as.factor(paste0(datext$subjIndx,datext$cond))
    
       if (c==1){
        dataux = list()
        dataux[[1]]    = ddply(datFrameaux[datFrameaux$cond %in% c("ReLuHu","ReLuHc","ReLcHu","ReLcHc"),]
                           , .(cond,trialnr), summarize,
                           meanRT=mean(trial_RT, na.rm=T),
                           sem=sd(trial_RT, na.rm=T)/sqrt(length(unique(datFrameaux$subjIndx))))
        dataux[[2]]    = ddply(datFrameaux[datFrameaux$cond %in% c("RaLuHu","RaLuHc","RaLcHu","RaLcHc"),]
                           , .(cond,trialnr), summarize,
                           meanRT=mean(trial_RT, na.rm=T),
                           sem=sd(trial_RT, na.rm=T)/sqrt(length(unique(datFrameaux$subjIndx))))
        labelsResp=c('External','Anatomical')
        pltList = list()
        for (dT in c(1,2)){
          p1 <- ggplot()  +
          theme_bw() +
          theme(axis.line         = element_line(colour = "black"),
                 axis.line.y         = element_line(color="black"),
                 axis.line.x         = element_line(color="black"),
                 # panel.grid.minor.y  = element_blank(),
                panel.grid.major.x  = element_blank(),
                 panel.border        = element_blank(),
                 panel.background    = element_blank(),
               #  axis.title.x        = element_blank(),
                 legend.position     ="none",
                 plot.margin         = unit(c(2, 2, 2, 2), "mm"),
                 text                = element_text(family="Helvetica",size=16))
        
           p1 <- p1 +
             geom_line(data=dataux[[dT]],aes(x=trialnr,y=meanRT,color=cond))+
             geom_ribbon(data=dataux[[dT]],aes(x=trialnr,ymin=meanRT-sem,ymax=meanRT+sem,fill=cond),alpha=0.3)+
             geom_point(data=dataux[[dT]],aes(x=trialnr,y=meanRT,color=cond))
           if (dT==2){
             for (ss in c(1,2,3,4)){
                p1 <- p1 +
                 geom_text(data=NULL,aes(x = 50), y = 1.2-ss/20,label = crossLabels[ss],hjust=0,
                         size=5,color=cbbPalette[ss])
             }
           }
           p1 <- p1 +
             scale_fill_manual(values=cbbPalette)+
             scale_color_manual(values=cbbPalette)+
             scale_y_continuous("RT+-SEM",limits=c(0,1.2),expand = c(0, 0)) +
            scale_x_continuous(labelsResp[dT],limits=c(0,100),expand = c(0, .75)) +
             ggtitle(sprintf('N = %d',length(unique(datFrameaux$subjIndx))))
           pltList[[dT]] <- p1
         }
        
         openGraph(width = 8, height = 4)
        
         do.call(grid.arrange, c(pltList, nrow=1, ncol=2, as.table = FALSE))
         saveGraph(file=paste(getwd(),"/07_Analyses/",pathfig[t],"/allSubjsbytrial",tsks[t],prf[c+1],sep=""), type="pdf")
      }
    }
    #################################
    #################################
  }
}


################################# 
#################################
# RT vs % incorrect response
if (plotrtvscorrect){
pltListRTvsP = list()
dT=1
for (t in c(1:length(tsks))){
  datFrameaux = subset(datFrame,task==t)
  datFrameaux = subset(datFrameaux,trial_RT>.150) 
  xs <-1
  shapes = c(21,23)
#  if(t==1){dT=1}else{dT=5}
  for (ss in levels(datFrameaux$cond)){ 
    datRTp = subset(datFrameaux,cond==ss)
    datRTp      = ddply(datRTp, .(subjIndx,cond,trial_int,trial_correct), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T),N=length(trial_RT))
    datRTp      = ddply(datRTp, .(subjIndx,cond,trial_int), function(df) return(with(df, data.frame(trial_correct = trial_correct,  meanRT=meanRT, N =N, totN=sum(N))))) 
    datRTpinc   = subset(datRTp,trial_correct==0)
    datRTpinc$inc   =datRTpinc$N/datRTpinc$totN
    datRTpc     = subset(datRTp,trial_correct==1)
    datRTpc$inc   =(datRTpc$totN-datRTpc$N)/datRTpc$totN
    
    p2 <- ggplot()  + 
      theme_bw() +
      theme(axis.line         = element_line(colour = "black"),
            axis.line.y         = element_line(color="black"),
            axis.line.x         = element_line(color="black"),
            # panel.grid.minor.y  = element_blank(),
            panel.grid.major.x  = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.border        = element_blank(),
            panel.background    = element_blank(),
            #axis.title.x        = element_blank(),
            axis.title.y        = element_blank(),
            axis.text.y        = element_blank(),
            #axis.text.x         = element_text(size=14),
            plot.margin         = unit(c(2, 2, 2, 2), "mm"),
            text                = element_text(family="Helvetica",size=10))
    p2 <- p2 +
      geom_point(data=datRTpc,aes(x=inc, y=meanRT),fill=cbbPalette[xs],size=3,shape=21, stroke=.1, alpha = .3) +         # plot each subjecy
      geom_point(data=datRTpinc,aes(x=inc, y=meanRT),fill=cbbPalette[xs],size=3,shape=23, stroke=.1, alpha = .8)
    
    p2 <- p2 +
      scale_y_continuous("RT",limits=c(0,1.6),expand = c(0, 0)) 
    # scale_x_continuous("% incorrect",limits=c(0,.6),expand = c(0, 0))
    
    if (dT==1 || dT==9){
      p2 <- p2 + theme(axis.title.y = element_text(family="Helvetica",size=10),
                       axis.text.y = element_text(family="Helvetica",size=10))
    }
    if (length(tsks)>1 & dT<9){
      p2 <- p2 + theme(axis.title.x = element_blank())
    }
    xs = xs +1
    pltListRTvsP[[dT]] <- p2
    dT=dT+1
  }
}
if (length(tsks)==1){
  openGraph(width = 7, height = 3) 
  do.call(grid.arrange, c(pltListRTvsP, nrow=1, ncol=8, as.table = FALSE))
}
if (length(tsks)==2){
  openGraph(width = 7, height = 6) 
  do.call(grid.arrange, c(pltListRTvsP, nrow=2, ncol=8, as.table = T))
} 
}
#################
#################
# RT interactions
#################

dipDact   =c(0,1.5)
for (cc in c(1,2,3,4)){
  if (cc==1){tP = list(legslabel = c("uL","cL"),    setFact = "LegC",  xFact = "HandC", groupFact = "RespM",
                       nameX = "uL                   cL",  xlabels = c("uH","cH","uH","cH"), glabels = c("ext","anat"))}
  if (cc==2){tP = list(legslabel = c("uH","cH"),    setFact = "HandC", xFact = "LegC",  groupFact = "RespM",
                       nameX = "uH                   cH",  xlabels = c("uL","cL","uL","cL"), glabels = c("ext","anat"))}
  if (cc==3){tP = list(legslabel = c("ext","anat"), setFact = "RespM", xFact = "LegC",  groupFact = "HandC",
                       nameX = "ext                 anat", xlabels = c("uL","cL","uL","cL"), glabels   = c("uH","cH"))}
  if (cc==4){tP = list(legslabel =c("ext","anat"),  setFact = "RespM", xFact = "HandC", groupFact = "LegC",
                       nameX = "ext                 anat", xlabels = c("uH","cH","uH","cH"), glabels   = c("uL","cL"))}
  for (t in c(1:length(tsks))){
    openGraph(width = 3.5, height = 3) 
    p1 <- ggplot()+ theme_bw() +
            theme(axis.line     = element_line(colour = "black"),
            axis.line.y         = element_line(color="black"),
            axis.line.x         = element_line(color="black"),
            # panel.grid.minor.y  = element_blank(),
            panel.grid.major.x  = element_blank(),
            panel.border        = element_blank(),
            panel.background    = element_blank(),
            #axis.title.x        = element_blank(),
            #legend.position     ="none",
            legend.justification= c(0,0), 
            legend.position     = c(1,0),
            legend.text         = element_text(size=10),
            legend.title        = element_blank(),
            legend.key.size     = unit(3,"mm"),
            #legend.key.width = unit(1,"mm"),
            plot.margin         = unit(c(2, 10, 2, 2), "mm"),
            text                = element_text(family="Helvetica",size=16))
    for (l in c(1:2)){
      datFrameaux = subset(datFrame,task==t)
      datFrameaux = subset(datFrameaux,trial_RT>.150)
      datFrameaux = subset(datFrameaux,trial_correct==1)
      datFrameaux = subset(datFrameaux,get(tP$setFact)==l)
      dat1        = ddply(datFrameaux, .(subjIndx,HandC,LegC,RespM,cLxL,cSxB), summarize,  meanRT1=mean(trial_RT, na.rm=T))
     # dat1$meanRT1 = log(1000*dat1$meanRT1) 
      dat1        = ddply(dat1, .(HandC,LegC,RespM,cLxL,cSxB), summarize,  meanRT=mean(meanRT1, na.rm=T),se=std.error(meanRT1, na.rm=T))
      dat1[tP$xFact] = dat1[tP$xFact]+dipDact[l]
      dat1$LE      = dat1$meanRT-dat1$se
      dat1$HE      = dat1$meanRT+dat1$se
      dat1$cFill   = "nc"
      dat1$cFill[dat1$cLxL==1 & dat1$cSxB==2]   = "sc"
      dat1$cFill[dat1$cLxL==2 & dat1$cSxB==1]   = "ac"
      dat1$cFill[dat1$cLxL==2 & dat1$cSxB==2]   = "ac&sc"
      dat1[[tP$groupFact]] = as.factor(dat1[[tP$groupFact]])
      pd = position_dodge(0.1)
      
      
      p1 <- p1 +
        geom_errorbar(data=dat1,aes_string(x=tP$xFact,colour=tP$groupFact,group=tP$groupFact,ymin="LE",ymax="HE"),colour="black",width=.1,position=pd)+
        geom_line(data=dat1,aes_string(x=tP$xFact,y="meanRT",linetype=tP$groupFact,group=tP$groupFact),position=pd)+
        geom_point(data=dat1,aes_string(x=tP$xFact,y="meanRT",group=tP$groupFact,fill="cFill"),position=pd, size=1.5, shape=21,colour="black") #+
       # geom_text(data=dat1[dat1$cLxL%in% 2 ,],aes_string(x=xFact,y="meanRT"),label="Ac",hjust=1,size=5) +
      #  geom_text(data=dat1[dat1$cSxB%in% 2 ,],aes_string(x=xFact,y="meanRT"),label="Sc",hjust=1,size=5)
   }
    
    p1 <- p1 +
      scale_y_continuous("RT+-SEM",limits=c(.35,.75),expand = c(0, 0)) +
      scale_x_continuous(name=tP$nameX,breaks=c(1,2,2.5,3.5),labels=tP$xlabels,limits=c(.5,4),expand = c(0, 0))+
      scale_linetype_discrete(labels=tP$glabels)+
      ggtitle(tsks[t])
    print(p1)
    saveGraph(file=paste(getwd(),"/07_Analyses/",pathfig[t],"/",tsks[t] ,"_interactPlot_",tP$setFact,"_",tP$groupFact, sep=""), type="pdf")
  }
}
  
  