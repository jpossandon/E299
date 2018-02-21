cbbPalette        <- c("#606060","#006D2C","#1F78B4","#E31A1C","#BB6644","#BB6644")
cbbPaletteGroups  <- c("#44BBA8","#BB6644")

subgroupNames     <- Cnames
groupNames        <- c("PD","Ctrl")

Nsims    <- 500
Ntrials  <- 108 # leave out the not-areOK-trials?
nn       <- 1
for (dd in 1:length(subgroupNames)) {
  DBSc     <- subgroupNames[dd]
  Subjects <- unique(datFrame$PDID[datFrame$DBS==DBSc])
  Nsimdata <- Nsims*length(Subjects)*length(subgroupNames)
  if (dd==1){
    simFrame <- data.frame(PDID=rep(NaN, Nsimdata),     DBS=rep(NaN, Nsimdata),
                sim=rep(NaN, Nsimdata), RTf=rep( NA, Nsimdata), RTfmedian=rep( NA, Nsimdata))}else{
    simFrame <- rbind(simFrame,data.frame(PDID=rep(NaN, Nsimdata),     DBS=rep(NaN, Nsimdata),
                                          sim=rep(NaN, Nsimdata), RTf=rep( NA, Nsimdata), RTfmedian=rep( NA, Nsimdata)))                    
  }
  
  for (ii in 1:Nsims) {
    chainIDX <- sample(1:dim(mcmcChain)[1],1, replace = TRUE, prob = NULL)
    for (ss in 1:length(Subjects)) {
      id  <- Subjects[[ss]]
      dbs <- DBSc
      TF  <- datFrame$DBS[areOK]==dbs & datFrame$PDID[areOK]==id
      if (!any(TF)) { next }
      dbsN <- unique(C[TF])
      idN  <- unique(S[TF])
      cLim <- unique(datFrame$DynDur[areOK][TF])
    #  if(censor&!logn){cLim <- unique(censorLimit[TF] +yM)}
      
      A     <- mcmcChain[chainIDX, sprintf("a[%d]", dbsN)]
      AS    <- mcmcChain[chainIDX, sprintf("aS[%d]",  idN)]
      
      if (dd<5){
        sig       = mcmcChain[chainIDX,paste("sigma[1]",sep="")]}else{
          sig        = mcmcChain[chainIDX,paste("sigma[2]",sep="")]
        }
      #sig   <- mcmcChain[chainIDX, "sigma"] 
      mu    <- yM+A+AS#yM+A+AS
      if(logn){rrs <- rlnorm(Ntrials, meanlog=mu, sdlog=sig)}
      if(!logn){rrs <- rnorm(Ntrials, mean=mu, sd=sig)}
      if(censor){rrs[rrs > cLim]      <- NA}
      
      simFrame$PDID[nn]    <- id
      simFrame$DBS[nn]     <- dbs
      simFrame$sim[nn]     <- ii
      simFrame$RTf[nn]        <- mean(rrs, na.rm=T)
      simFrame$RTfmedian[nn]  <- median(rrs, na.rm=T)
      nn <- nn+1
    }
  print(sprintf("%03d/%d", ii, Nsims))
  }
}
dat1    = ddply(datFrame, .(PDID, DBS), summarize,  meanRTf=mean(RTf, na.rm=T),  medianRTf=median(RTf, na.rm=T))
dat2    = ddply(dat1, .(DBS), summarize, mean=mean(meanRTf, na.rm=T),se=sd(meanRTf, na.rm=T)/sqrt(length(meanRTf)),df=length(meanRTf))

simFrame <- na.omit(simFrame)
simFrame$DBS <- factor(simFrame$DBS,levels = subgroupNames) 
titleStr <- sprintf("%s pred vs data", fileNameRoot)
openGraph() # TODO: add 95% data HDI

ru <- ddply(simFrame, .(sim,DBS), summarize, meanmean=mean(RTf, na.rm=T), medianmedian=median(RTfmedian, na.rm=T))
pirateplot(formula = meanRTf ~ DBS,
           data = dat1,
           main = titleStr,
           line.fun = mean,
           xlab = "X Pos",
           ylab = "Reaction Time",
           pal = cbbPalette,
           inf.o = .5,
           line.o = .6,
           bar.o = 0,
           bean.o = 0.5,
           point.o = 1,
           point.pch = 16,
           #inf="hdi",hdi.iter=1000,
           bean.lwd = 2,
           jitter.val=.05,
           ylim = c(0,9))
# add=T)

pirateplot(formula = meanmean ~ DBS,
           data = ru,
           xlab = "X Pos",
           ylab = "Reaction Time",
           #main = "Black and White Pirate Plot",
           pal = "black",
           inf.o = 0,
           line.o = 0,
           bar.o = 0,
           bean.o = .5,
           point.o = .05,
           point.pch = 16,
           #inf="hdi",hdi.iter=1000,
           bean.lwd = 2,
           jitter.val=.05,
           add=T)
saveGraph(file=paste(getwd(),"/figures/compRTtrial/",fileNameRoot,"_allpred",sep=""), type="pdf")

# per subjects
openGraph(width = 15, height = 10)
pltList <- list()
Subjects <- unique(datFrame$PDID)
for (ss in 1:length(Subjects)) {
  id  <- Subjects[ss]
  aux <- dat1[dat1$PDID==id,]
  auxsim <-simFrame[simFrame$PDID %in% ss,]
  p1 <- ggplot()+
        theme_bw() +
        theme(axis.line       = element_line(colour = "black"),
          axis.line.y         = element_blank(),
          axis.line.x         = element_line(color="black"),
          axis.text.y         = element_blank(),
          axis.text.x         = element_blank(),
          axis.ticks.x         = element_blank(),
          axis.title.x        = element_blank(),
          axis.title.y        = element_blank(),
          panel.grid.minor.y  = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.border        = element_blank(),
          panel.background    = element_blank(),
          plot.margin         = unit(c(0, 2, 0, 2), "mm"),
          legend.position     = "none",
          plot.title          = element_text(lineheight=.8, face="bold"),
          text                = element_text(family="Helvetica"))+
        scale_y_continuous(limits=c(0,8),breaks=seq(0,8,4),expand = c(0, 0))+
        scale_x_discrete(limits = subgroupNames, drop = FALSE)+
        ggtitle(id)

    sess = unique(auxsim$DBS)
    for (ssi in 1:length(sess)) {
      if (ss>17){cplus = 4}else{cplus=0}
      p1 <- p1+geom_violin(data = subset(auxsim,DBS %in% sess[ssi]),aes(x=DBS,y=RTf,scale='width'),colour=cbbPalette[ssi+cplus])
      }
    p1 <- p1 + geom_point(data=aux,aes(x=DBS, y=meanRTf),size=2)
  if (ss==1 | ss==8 | ss==15 | ss==22 | ss==29){
    p1 <- p1 +
      theme( axis.line.y         = element_line(color="black"),
             axis.text.y         = element_text(family="Helvetica"))}
  pltList[[ss]] <- p1
}
do.call(grid.arrange, c(pltList, nrow=5, ncol=7))
saveGraph(file=paste(getwd(),"/figures/compRTtrial/",fileNameRoot,"_subjpred",sep=""), type="pdf")

# 
