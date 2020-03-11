# Data plot
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
prevLabels  <- c("-2d -1d","-2d -1s","-2s -1d","-2s -1s")
xpos        <- c(1,2,3,4,5,6,7,8)
gap         <- 1.5
xlims       <- c(0,10.5)
xpos[xpos>4]<- xpos[xpos>4]+gap
jitterVal   <- runif(length(unique(datFrame$subjIndx)), max = 0.1)-.05
if (exp==1){
  tsks        <- c("Normal")}
if (exp==2){
  tsks        <- c("Normal","Anti")}
if(exp==3){
  tsks        <- c("LH","HL")}
if (exp==4){
  tsks        <- c("Normal")}
for (t in 1:length(tsks)){
  datFrameaux = subset(datFrame,task==t)
  
  dat1    = ddply(datFrameaux, .(subjIndx,cond,prevT), summarize,  meanRT=mean(trial_RT[areOK], na.rm=T), median=median(trial_RT[areOK], na.rm=T))
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
    auxdatX <-dat1[dat1$cond %in% ss,]
    xss = -.225
    xsym = 0
    for (sss in levels(auxdatX$prevT)){
      auxdat1 <-auxdatX[auxdatX$prevT %in% sss,]
    auxdat1$Condition <-xpos[xs]+xss
    p1 <- p1 +
  #  geom_line(data=auxdat1,aes(x=meanRT),stat="density")+
  #geom_violin(data=auxdat1,aes(x=Condition, y=meanRT),draw_quantiles = c(0.25, 0.5, 0.75),trim = FALSE,adjust = .9,scale='width')+
    geom_point(data=auxdat1,aes(x=Condition+jitterVal, y=meanRT),fill=cbbPalette[xs],size=2,shape=21+xsym, stroke=.1, alpha = .5) +
      geom_errorbar(data=auxdat1,aes(x=Condition, ymin=mean(meanRT)-std.error(meanRT), ymax=mean(meanRT)+std.error(meanRT)),color='black',width=.2,size=.3)+
         geom_point(data=auxdat1,aes(x=Condition, y=mean(meanRT)),color='black',fill=cbbPalette[xs],size=3,shape=21+xsym, stroke=.5) +
      geom_text(data=auxdat1,aes(x = Condition, y=mean(meanRT)+std.error(meanRT)+.02,label = substring(sprintf('%.3f',mean(meanRT)),2)),hjust=0.5,
                size=2)
      # geom_segment(data=auxdat1, aes(x=Condition-.25, y=mean(meanRT), xend=Condition+.25, yend=mean(meanRT)),
    #              color=cbbPalette[xs],size=1) +
      # geom_segment(data=auxdat1, aes(x=Condition-.25, y=median(meanRT), xend=Condition+.25, yend=median(meanRT)),
      #              color='black',size=1)
    xss <- xss+.225
    xsym = xsym+1
    }
    xs <- xs +1
  }
  
  dat2 = ddply(dat1, .(cond), summarize,  meanRT=mean(meanRT, na.rm=T),  median=median(meanRT, na.rm=T))
  dat2$numcond <-as.numeric(dat2$cond)
  dat2$numcond[dat2$numcond>4] <- dat2$numcond[dat2$numcond>4]+gap
  dat2$paircond <-c(1,2,3,4,1,2,3,4)
  for (ss in c(1,2,3,4)){
    #p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(x=numcond, y=meanRT, group=paircond),colour=cbbPalette[ss],size=.5),p1$layers)
    p1 <- p1 + 
       geom_text(data=NULL,aes(x = 6.5+gap), y = .25-ss/20,label = crossLabels[ss],hjust=0,
                size=5,color=cbbPalette[ss]) +
      geom_point(data=NULL,aes(x= 1),color='black', fill="#d3d3d3", y=.25-ss/20,size=3,shape=21+ss-1, stroke=.5, alpha = .5) +
      geom_text(data=NULL,aes(x = 1.2), y = .25-ss/20,label = prevLabels[ss],hjust=0,
                size=5) 
  }
  # line between subject data in the same response mode
 # auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[1:4],]
#  auxdat1$numcond <-as.numeric(auxdat1$cond)
 # p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5),p1$layers)
 # auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[5:8],]
#  auxdat1$numcond <-as.numeric(auxdat1$cond)+gap
  #p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=meanRT, group=subjIndx),color='gray', alpha = .5),p1$layers)
  
  
  p1 <- p1 + 
    scale_y_continuous(limits=c(0,1.2),expand = c(0, 0)) +
    scale_x_continuous(limits=xlims,expand = c(0, 0),
                       breaks=c(2.5,6.5+gap),labels=c('External','Anatomical'))+
    ylab('Reaction Time (Mean +- SEM)')+
    ggtitle(sprintf('N = %d',length(unique(datFrameaux$subjIndx))))
    
  print(p1)
  saveGraph(file=paste(getwd(),"/07_Analyses/",pathfig[t],"/allSubjMeansbyprev",tsks[t],sep=""), type="pdf")
  
   
  
}