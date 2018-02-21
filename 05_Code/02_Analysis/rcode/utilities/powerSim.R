graphics.off()
require(plyr)
library("GGally")
rm(list=ls(all=TRUE))
path       = "/Users/jossando/trabajo/E299"
setwd(path)
source("05_Code/02_Analysis/rcode/utilities/openGraphSaveGraph.R", chdir=T) 
##
# GET the data
datFrame          <- read.csv(file="06_RawData/LH2cross/allSubjectExp2.csv" , sep="," , strip.white=T)

datFrame          <- subset(datFrame, select = -c(X))
bt                <- datFrame$trial_blockType
bt                <- gsub('1','Re',bt)
bt                <- gsub('2','Ra',bt)
cl                <- datFrame$trial_crossed_legs
cl                <- gsub('0','Lu',cl)
cl                <- gsub('1','Lc',cl)
ch                <- datFrame$trial_crossed_hand
ch                <- gsub('0','Hu',ch)
ch                <- gsub('1','Hc',ch)


datFrame$cond     <- as.factor(paste0(bt,cl,ch))
datFrame$cond     <- factor(datFrame$cond,levels = c("ReLuHu","ReLuHc","ReLcHu","ReLcHc","RaLuHu","RaLuHc","RaLcHu","RaLcHc"))

isNAN             <- is.nan(datFrame$trial_RT)
datFrame$areOK    <- !(isNAN) & datFrame$trial_RT>.150 & datFrame$trial_correct==1

datFrame          <- datFrame[,c("subjIndx","trial_RT","cond","areOK")]
datFrame          <- datFrame[!datFrame$subjIndx==59,]
subjs             <- unique(datFrame$subjIndx)

auxdat   =  datFrame[datFrame$areOK,]
auxres   = ddply(auxdat,.(subjIndx,cond),summarize,RT=mean(trial_RT,na.rm=T))
Actualp  = pairwise.t.test(x=auxres$RT,g=auxres$cond,paired=TRUE)
Actualh  = Actualp$p.value<.05
####
# sims
nSims  = 200
nSTr   = seq(30,300,30)
nSsubj = seq(10,25,1)

resultequal =array(data =NA, dim = c(length(nSsubj),length(nSTr),nSims),dimnames=list(nSsubj,nSTr,NULL))
resultein =array(data =NA, dim = c(length(nSsubj),length(nSTr),nSims),dimnames=list(nSsubj,nSTr,NULL))
resultzwei =array(data =NA, dim = c(length(nSsubj),length(nSTr),nSims),dimnames=list(nSsubj,nSTr,NULL))
resultdrei =array(data =NA, dim = c(length(nSsubj),length(nSTr),nSims),dimnames=list(nSsubj,nSTr,NULL))
s = 1

for (ss in nSsubj){
  t = 1  
  for (tt in nSTr){
    for (ns in 1:nSims){
          simSubjs = sample(subjs,ss)
      auxdat   = datFrame[datFrame$subjIndx %in% simSubjs,]
      auxdat   = auxdat[sample(dim(auxdat)[1],tt*ss*8),]
      auxdat   = auxdat[auxdat$areOK,]
      auxres   = ddply(auxdat,.(subjIndx,cond),summarize,RT=mean(trial_RT,na.rm=T))
      bla      = pairwise.t.test(x=auxres$RT,g=auxres$cond,paired=TRUE)
      blah     = bla$p.value<.05
      resultequal[s,t,ns] = !any(!(Actualh==blah),na.rm=T)
      resultein[s,t,ns] = sum(!(Actualh==blah),na.rm=T)<2
      resultzwei[s,t,ns] = sum(!(Actualh==blah),na.rm=T)<3
      resultdrei[s,t,ns] = sum(!(Actualh==blah),na.rm=T)<4
      print(ns)
    }
    t =t+1
  }
  s=s+1
}
save(datFrame,resultequal,resultein,resultzwei,resultdrei,nSims, file=paste(path,"/07_Analyses/powerSim/",format(Sys.Date(),"%m%d%Y"),"powerSim.R",sep=''))
pequal = setNames(as.data.frame(as.table(rowSums(1*resultequal,dims=2)/nSims*100)),c("nSubjects","nTrials","Power"))
peins = setNames(as.data.frame(as.table(rowSums(1*resultein,dims=2)/nSims*100)),c("nSubjects","nTrials","Power"))
pzwei = setNames(as.data.frame(as.table(rowSums(1*resultzwei,dims=2)/nSims*100)),c("nSubjects","nTrials","Power"))
pdrei = setNames(as.data.frame(as.table(rowSums(1*resultdrei,dims=2)/nSims*100)),c("nSubjects","nTrials","Power"))
openGraph(6,4)
getPalette = colorRampPalette(brewer.pal(name="PuBuGn",n=9))
ggplot(data =pzwei)+
geom_raster(aes(x=nSubjects,y=nTrials,fill=Power))+
  geom_text(aes(x=nSubjects,y=nTrials,label=sprintf("%2.0f",Power)),color="red",size=3)+
#scale_fill_brewer(type="seq",palette ="Blues")+
   #scale_fill_continuous(low="black", high="pink", limits=c(.5,1))+
scale_fill_gradientn(colours=getPalette(100))+
scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))
