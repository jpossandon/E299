datFrameOKLC <-ddply(datFrameOK,.(subjIndx,LegC,task),summarize,meanRT=mean(meanRT, na.rm=T))
dft1lc = datFrameOKLC[datFrameOKLC$task==1 & datFrameOKLC$LegC==2,]
dft1lu = datFrameOKLC[datFrameOKLC$task==1 & datFrameOKLC$LegC==1,]
dfTask1 = dft1lc
dfTask1$meanRTLC = dft1lc$meanRT-dft1lu$meanRT
datFrameOKHC <-ddply(datFrameOK,.(subjIndx,HandC,task),summarize,meanRT=mean(meanRT, na.rm=T))
dft2hc = datFrameOKHC[datFrameOKHC$task==2 & datFrameOKHC$HandC==2,]
dft2hu = datFrameOKHC[datFrameOKHC$task==2 & datFrameOKHC$HandC==1,]
dfTask2 = dft2hc
dfTask2$meanRTHC = dft2hc$meanRT-dft2hu$meanRT

plotData = merge(dfTask1,dfTask2,by.x="subjIndx",by.y="subjIndx")
personTest = cor.test(plotData$meanRTLC,plotData$meanRTHC)

plotData2 =merge(datFrameOK[datFrameOK$task==1 & datFrameOK$LegC==2 & datFrameOK$RespM==1 &datFrameOK$HandC==1,],
 datFrameOK[datFrameOK$task==2 & datFrameOK$HandC==2 & datFrameOK$RespM==1 &datFrameOK$LegC==1,],by.x="subjIndx",by.y="subjIndx")
personTest2 = cor.test(plotData2$meanRT.x,plotData2$meanRT.y)

openGraph(1.6*1.5,1.6*1.5)
p1 <- ggplot()+
  geom_abline(intercept = 0, slope = 1,color="gray",linetype="dotted",size=.6)+
  geom_point(data=plotData,aes(x=1000*meanRTLC,y=1000*meanRTHC),fill="red",shape=21,size = 1.5,stroke=.1,alpha=.5)+
  geom_text(aes(x=50,y=300,label=sprintf("r = %1.2f\np = %1.4f ",personTest$estimate,personTest$p.value)),size=3,hjust=0)
p1 <- p1 +
  theme_bw() +
  theme(axis.line           = element_line(colour = "black"),
        axis.line.y         = element_line(color="black",size=.2),
        axis.line.x         = element_line(color="black",size=.2),
        panel.grid.major.y  = element_line(linetype="dotted"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_line(linetype="dotted"),
        panel.border        = element_blank(),
        panel.background    = element_blank(),
        axis.title.x       = element_text(family="Helvetica",size=9),
        axis.title.y       = element_text(family="Helvetica",size=9),
        plot.margin         = unit(c(2, 2, 5, 2), "mm"),
        legend.position     = "none",
        text                = element_text(family="Helvetica",size=12))+
  # geom_text(aes(x=-5.75,y=8.5,label="Left"),color=cbPalette[1],size=4,fontface="bold")+
  # geom_text(aes(x=-5.75,y=7.75,label="Right"),color=cbPalette[2],size=4,fontface="bold")+
  # geom_text(aes(x=7.2,y=22,label="H0:avg=0"),size=4)+
   coord_cartesian(ylim = c(0,350),xlim=c(0,350), expand = FALSE)+
   scale_y_continuous(name="Cross. Effect Stim. Hand (ms)",expand = c(0, 0))+
  # geom_vline(xintercept = 0,color="gray",linetype="dotted",size=.4)+
   scale_x_continuous(name="Cross. Effect Stim. Feet (ms)",expand = c(0, 0))
  # scale_fill_manual(values=cbPalette)

print(p1)
saveGraph(file=paste("07_Analyses/LH3cross/exp3corr"), type="pdf")

openGraph(1.6*1.5,1.6*1.5)
p1 <- ggplot()+
  geom_abline(intercept = 0, slope = 1,color="gray",linetype="dotted",size=.6)+
  geom_point(data=plotData2,aes(x=1000*meanRT.x,y=1000*meanRT.y),fill="red",shape=21,size = 1.5,stroke=.1,alpha=.5)+
  geom_text(aes(x=450,y=900,label=sprintf("r = %1.2f\np = %1.4f ",personTest2$estimate,personTest2$p.value)),size=3,hjust=0)
p1 <- p1 +
  theme_bw() +
  theme(axis.line           = element_line(colour = "black"),
        axis.line.y         = element_line(color="black",size=.2),
        axis.line.x         = element_line(color="black",size=.2),
        panel.grid.major.y  = element_line(linetype="dotted"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_line(linetype="dotted"),
        panel.border        = element_blank(),
        panel.background    = element_blank(),
        axis.title.x       = element_text(family="Helvetica",size=9),
        axis.title.y       = element_text(family="Helvetica",size=9),
        plot.margin         = unit(c(2, 2, 5, 2), "mm"),
        legend.position     = "none",
        text                = element_text(family="Helvetica",size=12))+
  # geom_text(aes(x=-5.75,y=8.5,label="Left"),color=cbPalette[1],size=4,fontface="bold")+
  # geom_text(aes(x=-5.75,y=7.75,label="Right"),color=cbPalette[2],size=4,fontface="bold")+
  # geom_text(aes(x=7.2,y=22,label="H0:avg=0"),size=4)+
  coord_cartesian(ylim = c(400,1000),xlim=c(400,1000), expand = FALSE)+
  scale_y_continuous(name="HCLU ext. Stim. Hand (ms)",expand = c(0, 0))+
  # geom_vline(xintercept = 0,color="gray",linetype="dotted",size=.4)+
  scale_x_continuous(name="LCHU ext.  Stim. Feet. (ms)",expand = c(0, 0))
# scale_fill_manual(values=cbPalette)

print(p1)
saveGraph(file=paste("07_Analyses/LH3cross/exp3corrabs"), type="pdf")

