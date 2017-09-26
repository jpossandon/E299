# Data plot
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
crossLabels <- c("|| Legs  || Hands","|| Legs  X Hands","X Legs  || Hands","X Legs  X Hands")
xpos        <- c(1,2,3,4,5,6,7,8)
gap         <- 3
xlims       <- c(-1.5,13.5)
xpos[xpos>4]<- xpos[xpos>4]+gap
jitterVal   <- runif(length(unique(datFrameOK$subjIndx)), max = 0.4)-.2
dat1    = ddply(datFrame, .(subjIndx,cond), summarize, 
                errors=1-sum(trial_correct, na.rm=T)/length(trial_correct),
                misses = sum(trial_correct=='NaN')/length(trial_correct) )
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
  geom_point(data=auxdat1,aes(x=Condition+jitterVal, y=errors),fill=cbbPalette[xs],size=2,shape=21, stroke=.1, alpha = .5) +
    geom_errorbar(data=auxdat1,aes(x=Condition, ymin=mean(errors)-std.error(errors), ymax=mean(errors)+std.error(errors)),color='black',width=.2,size=.3)+
       geom_point(data=auxdat1,aes(x=Condition, y=mean(errors)),color='black',fill=cbbPalette[xs],size=3,shape=22, stroke=.5) +
    geom_text(data=auxdat1,aes(x = Condition+.13, y=mean(errors)+.015,label = substring(sprintf('%.3f',mean(errors)),2)),hjust=0,
              size=2)
  xs <- xs +1
}

dat2 = ddply(dat1, .(cond), summarize,  errors=mean(errors, na.rm=T),  median=median(errors, na.rm=T))
dat2$numcond <-as.numeric(dat2$cond)
dat2$numcond[dat2$numcond>4] <- dat2$numcond[dat2$numcond>4]+gap
dat2$paircond <-c(1,2,3,4,1,2,3,4)
for (ss in c(1,2,3,4)){
  p1$layers <- c(geom_line(data=subset(dat2, paircond %in% ss),aes(x=numcond, y=errors, group=paircond),colour=cbbPalette[ss],size=.5),p1$layers)
  p1 <- p1 + 
     geom_text(data=NULL,aes(x = 6.5+gap), y = .65-ss/25,label = crossLabels[ss],hjust=0,
              size=5,color=cbbPalette[ss])
}
# line between subject data in the same response mode
auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[1:4],]
auxdat1$numcond <-as.numeric(auxdat1$cond)
p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=errors, group=subjIndx),color='gray', alpha = .5),p1$layers)
auxdat1 <-dat1[dat1$cond %in% levels(dat1$cond)[5:8],]
auxdat1$numcond <-as.numeric(auxdat1$cond)+gap
p1$layers <- c(geom_line(data=auxdat1,aes(x=numcond+rep(jitterVal,each=4), y=errors, group=subjIndx),color='gray', alpha = .5),p1$layers)


p1 <- p1 + 
  scale_y_continuous(limits=c(0,.65),expand = c(0, 0)) +
  scale_x_continuous(limits=xlims,expand = c(0, 0),
                     breaks=c(2.5,6.5+gap),labels=c('External','Anatomical'))+
  ylab('Performance (Mean +- SEM)')+
  ggtitle(sprintf('N = %d',length(unique(datFrameOK$subjIndx))))
  
print(p1)
saveGraph(file=paste(getwd(),"/figures/",pathfig,"/allSubjectsperf",sep=""), type="pdf")

