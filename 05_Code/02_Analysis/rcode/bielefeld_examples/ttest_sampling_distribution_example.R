# example sampling distribution
require(ggplot2)
require(plyr)
path = "/Users/jossando/trabajo/E299/05_Code/02_Analysis/rcode/bielefeld_examples/"
setwd(path)
source("openGraphSaveGraph.R") # utilities by Kruschke from http://doingbayesiandataanalysis.blogspot.de/
# Get the data

load(paste(path,"dataE299_bielefeld.Rdata",sep=''))
dFc        = subset(dFc, cond==c("RaLuHu","RaLuHc"))
dFc$cond   = factor(dFc$cond )
dFc$condnum = as.numeric(dFc$cond)
# plot data

p1 <- ggplot() +  theme_bw() + theme(legend.position     = "none",
                                     panel.border        = element_blank(),
                                     panel.background    = element_blank())+
  geom_point(data=dFc,aes(x=mRT,y=condnum, fill=cond),shape=21,colour = "#000000", size = 4,stroke=.2,position = position_jitter(h = 0.15))+
  scale_fill_manual(values=c("#606060","#006D2C"))+
  scale_y_continuous(limits=c(0.5,2.5),expand = c(0, 0),breaks=c(1,2),labels=c("RaLuHu","RaLuHc")) 
openGraph(4,3)
print(p1)
saveGraph(file=paste(path,"/figures/","data_sampling_example",sep=""), type="pdf")

diffData = ddply(dFc,.(subjIndx),transform,diffRT=c(NA,diff(mRT)))
sampM   = rnorm(100000, mean = mean(diffData$diffRT,na.rm=T), sd = sd(diffData$diffRT,na.rm=T)) # the *100 is to match the prior specified in the model
sampMd     = with(density(sampM, adjust=2 ),data.frame(x,y))

p1 <- ggplot() +  theme_bw() + theme(legend.position     = "none",
                                     panel.border        = element_blank(),
                                     panel.background    = element_blank())+
      geom_histogram(data=diffData, aes(diffRT),stat="bin",breaks=seq(-.1,.3,.01),color="#000000",fill="gray")+
      geom_point(data=diffData,aes(x=diffRT,y=-.5),shape=21,fill="#E31A1C", size = 3,stroke=.2,position = position_jitter(h = 0.15))+
      geom_line(data=sampMd,aes(x=x,y=y/3),color="#E31A1C")+
      geom_segment(data=NULL,x=mean(diffData$diffRT,na.rm=T),xend=mean(diffData$diffRT,na.rm=T),y=0,yend=max(sampMd$y)/3,colour="#E31A1C",size=1)+
      scale_x_continuous(limits=c(-0.025,.15),expand = c(0, 0))+  
      scale_y_continuous(limits=c(-0.75,5),expand = c(0, 0))  
openGraph(4,3)
print(p1)