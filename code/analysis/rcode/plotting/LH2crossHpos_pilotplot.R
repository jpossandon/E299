datFrame = read.csv(file="/Users/jossando/trabajo/E299/data/LH2crossHpos/s1_LH2crossHpos/pilot.csv" , sep="," , strip.white=T)
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
hp                <- datFrame$trial_handpos
hp                <- gsub('2','HPc',hp)
hp                <- gsub('3','HPr',hp)
ls                <- datFrame$trial_limbside
ls                <- gsub('1','L_',ls)
ls                <- gsub('2','R_',ls)
datFrame$cond     <- as.factor(paste0(ls,bt,cl,ch,hp))
datFrame$cond     <- factor(datFrame$cond,levels = c("L_ReLcHcHPc","R_ReLcHcHPc","L_ReLcHcHPr", "R_ReLcHcHPr",
                                                     "L_ReLcHuHPc", "R_ReLcHuHPc","L_ReLcHuHPr",  "R_ReLcHuHPr",
                                                     "L_ReLuHcHPc", "R_ReLuHcHPc","L_ReLuHcHPr", "R_ReLuHcHPr",
                                                     "L_ReLuHuHPc",  "R_ReLuHuHPc", "L_ReLuHuHPr", "R_ReLuHuHPr"))
isNAN             <- is.nan(datFrame$trial_RT)
datFrame$areOK             <- !(isNAN) & datFrame$trial_RT>.150 & datFrame$trial_correct==1

datFrame$numcond = as.numeric(datFrame$cond)
datFrame        <- datFrame[datFrame$areOK,] 
dtf = ddply(datFrame, .(cond), summarize, mean = mean(trial_RT,na.rm=T), SD = sd(trial_RT,na.rm=T), N=length(trial_RT), intlow = sum(trial_int==1))

ggplot()+
  geom_point(data = datFrame, aes(y=trial_RT,x=cond),color='gray')+
  geom_errorbar(data=dtf, aes(x=cond,ymax=mean+SD,ymin=mean-SD))+
  geom_point(data = dtf, aes(y=mean,x=cond),color='red')