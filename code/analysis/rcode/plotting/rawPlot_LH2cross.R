# Data plot
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
xpos        <- c(1,2,3,4,5,6,7,8)
gap         <- 3
xlims       <- c(-1.5,13.5)
xpos[xpos>4]<- xpos[xpos>4]+gap
jitterVal   <- runif(length(unique(datFrame$subjIndx)), max = 0.4)-.2
dat1    = ddply(datFrame, .(subjIndx,cond), summarize,  meanRT=mean(trial_RT[areOK], na.rm=T), median=median(trial_RT[areOK], na.rm=T))
openGraph(width = 8, height = 6) 
p1 <- ggplot()  + 
      theme_bw() +
      theme(axis.line         = element_line(colour = "black"),
          axis.line.y         = element_line(color="black"),
          axis.line.x         = element_line(color="black"),
         # panel.grid.minor.y  = element_blank(),
          panel.grid.major.x  = element_blank(),
          panel.border        = element_blank(),
          panel.background    = element_blank(),
          axis.title.x        = element_blank(),
          #axis.text.x         = element_text(size=14),
          plot.margin         = unit(c(2, 2, 2, 2), "mm"),
          text                = element_text(family="Helvetica",size=16))
xs <-1
for (ss in levels(dat1$cond)){
  auxdat1 <-dat1[dat1$cond %in% ss,]
  auxdat1$Condition <-xpos[xs]
  p1 <- p1 +
#  geom_line(data=auxdat1,aes(x=meanRT),stat="density")+
#geom_violin(data=auxdat1,aes(x=Condition, y=meanRT),draw_quantiles = c(0.25, 0.5, 0.75),trim = FALSE,adjust = .9,scale='width')+
  geom_point(data=auxdat1,aes(x=Condition+jitterVal, y=meanRT),fill=cbbPalette[xs],size=2,shape=21, stroke=.1, alpha = .5) +
    geom_errorbar(data=auxdat1,aes(x=Condition, ymin=mean(meanRT)-std.error(meanRT), ymax=mean(meanRT)+std.error(meanRT)),color='black',width=.2,size=.3)+
       geom_point(data=auxdat1,aes(x=Condition, y=mean(meanRT)),color='black',fill=cbbPalette[xs],size=3,shape=22, stroke=.5) +
    geom_text(data=auxdat1,aes(x = Condition+.13, y=mean(meanRT)+.015,label = substring(sprintf('%.3f',mean(meanRT)),2)),hjust=0,
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
  p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(x=numcond, y=meanRT, group=paircond),colour=cbbPalette[ss],size=.5),p1$layers)
  p1 <- p1 + 
     geom_text(data=NULL,aes(x = 6.5+gap), y = 1-ss/20,label = crossLabels[ss],hjust=0,
              size=5,color=cbbPalette[ss])
}
# line between subject data in the same response mode
auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[1:4],]
auxdat1$numcond <-as.numeric(auxdat1$cond)
p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5),p1$layers)
auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[5:8],]
auxdat1$numcond <-as.numeric(auxdat1$cond)+gap
p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5),p1$layers)


p1 <- p1 + 
  scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  scale_x_continuous(limits=xlims,expand = c(0, 0),
                     breaks=c(2.5,6.5+gap),labels=c('External','Anatomical'))+
  ylab('Reaction Time (Mean +- SEM)')+
  ggtitle(sprintf('N = %d',length(unique(datFrame$subjIndx))))
  
print(p1)
saveGraph(file=paste(getwd(),"/figures/LH2cross/allSubjectsMeans",sep=""), type="pdf")

# by truial order
#datext    = ddply(datFrame[datFrame$cond %in% c("ReLuHu","ReLuHc","ReLcHu","ReLcHc"),]
#                  , .(subjIndx,cond,trialnr), summarize, 
#                  meanRT=mean(trial_RT[areOK], na.rm=T), median=median(trial_RT[areOK], na.rm=T))
#datext$subjCond = as.factor(paste0(datext$subjIndx,datext$cond))

dataux = list()
dataux[[1]]    = ddply(datFrame[datFrame$cond %in% c("ReLuHu","ReLuHc","ReLcHu","ReLcHc"),]
                  , .(cond,trialnr), summarize, 
                  meanRT=mean(trial_RT[areOK], na.rm=T), 
                  sem=sd(trial_RT[areOK], na.rm=T)/sqrt(length(unique(datFrame$subjIndx))))
dataux[[2]]    = ddply(datFrame[datFrame$cond %in% c("RaLuHu","RaLuHc","RaLcHu","RaLcHc"),]
                  , .(cond,trialnr), summarize, 
                  meanRT=mean(trial_RT[areOK], na.rm=T), 
                  sem=sd(trial_RT[areOK], na.rm=T)/sqrt(length(unique(datFrame$subjIndx))))
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
      geom_text(data=NULL,aes(x = 50), y = .9-ss/25,label = crossLabels[ss],hjust=0,
              size=5,color=cbbPalette[ss])
  }
}
p1 <- p1 + 
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)+
  scale_y_continuous("RT+-SEM",limits=c(.3,.9),expand = c(0, 0)) +
 scale_x_continuous(labelsResp[dT],limits=c(0,100),expand = c(0, .75)) +
  ggtitle(sprintf('N = %d',length(unique(datFrame$subjIndx))))
pltList[[dT]] <- p1
}

openGraph(width = 8, height = 4) 

do.call(grid.arrange, c(pltList, nrow=1, ncol=2, as.table = FALSE)) 
saveGraph(file=paste(getwd(),"/figures/LH2cross/allSubjectsbytrial",sep=""), type="pdf")
