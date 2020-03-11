# get_exp2_data.R 
# read the csv files for the different experiments

#######
# EXP 1
if(exp==1){datFrame     <- read.csv(file="06_RawData/LH2cross/allSubject.csv" , sep="," , strip.white=T)
datFrame$task           <- 1         # task 1 - normal
datFrame$trialnr        <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrame)[1]/100)
}

#######
# EXP 2
if(exp==2 | exp==22){
datFrameNormal          <- read.csv(file="06_RawData/LH2cross/allSubjectExp2.csv" , sep="," , strip.white=T)
datFrameNormal$task     <- 1         # task 1 - normal 2 - anti
datFrameNormal$trialnr  <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrameNormal)[1]/100)

datFrameAnti            <- read.csv(file="06_RawData/LH2crossAnti/allSubjectExp2Anti.csv" , sep="," , strip.white=T)
datFrameAnti$task       <- 2
datFrameAnti$trialnr    <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrameAnti)[1]/100)

datFrame                <- rbind(datFrameNormal,datFrameAnti)
}

if(exp==3){
  datFrameNormal          <- read.csv(file="06_RawData/LH3cross/allSubjectExp3LHcross.csv" , sep="," , strip.white=T)
  datFrameNormal$task     <- 1         # task 1 - LH 2 - HL
  datFrameNormal$trialnr  <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrameNormal)[1]/100)
  
  datFrameHL            <- read.csv(file="06_RawData/HL3cross/allSubjectExp3HLcross.csv" , sep="," , strip.white=T)
  datFrameHL$task       <- 2
  datFrameHL$trialnr    <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrameHL)[1]/100)
  
  datFrame                <- rbind(datFrameNormal,datFrameHL)
}
if (exp==4){
    for (exp in 1:3){
    rem_subjects = rem_subjectALL$i
    source("05_Code/02_Analysis/rcode/utilities/get_exp2_data.R")
    if (exp==1){datFrameAll = subset(datFrame,task==1)}else{datFrameAll = rbind(datFrameAll,subset(datFrame,task==1))}
  }
  exp = 4
  datFrame          <- datFrameAll
  isNAN             <- is.nan(datFrame$trial_RT)
  areOK             <- !(isNAN) & datFrame$trial_RT>.150 & datFrame$trial_correct==1
  
}
# RECODE CONDITIONS
if (exp<4 | exp==22){
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
  tsk               <- datFrame$task 
  if(exp==2 | exp==22){
    tsk               <- gsub('1','Tn',tsk)
    tsk               <- gsub('2','Ta',tsk)}
  if(exp==3){
    tsk               <- gsub('1','LH',tsk)
    tsk               <- gsub('2','HL',tsk)}
  
  datFrame$cond     <- as.factor(paste0(bt,cl,ch))
  datFrame$cond     <- factor(datFrame$cond,levels = c("ReLuHu","ReLuHc","ReLcHu","ReLcHc","RaLuHu","RaLuHc","RaLcHu","RaLcHc"))
  datFrame$condAll  <- datFrame$cond
  
  if(exp==2 | exp==22){
  datFrame$condAll  <- as.factor(paste0(bt,cl,ch,tsk))
  datFrame$condAll     <- factor(datFrame$condAll,levels = c("ReLuHuTn","ReLuHcTn","ReLcHuTn","ReLcHcTn","RaLuHuTn","RaLuHcTn","RaLcHuTn","RaLcHcTn",
                                                       "ReLuHuTa","ReLuHcTa","ReLcHuTa","ReLcHcTa","RaLuHuTa","RaLuHcTa","RaLcHuTa","RaLcHcTa"))
  }
  if(exp==3){
    datFrame$condAll  <- as.factor(paste0(bt,cl,ch,tsk))
    datFrame$condAll     <- factor(datFrame$condAll,levels = c("ReLuHuLH","ReLuHcLH","ReLcHuLH","ReLcHcLH","RaLuHuLH","RaLuHcLH","RaLcHuLH","RaLcHcLH",
                                                               "ReLuHuHL","ReLuHcHL","ReLcHuHL","ReLcHcHL","RaLuHuHL","RaLuHcHL","RaLcHuHL","RaLcHcHL"))
  }
  datFrame          <- datFrame[ ! datFrame$subjIndx %in% rem_subjects,]
  
  isNAN             <- is.nan(datFrame$trial_RT)
  if(!perf){
  areOK             <- !(isNAN) & datFrame$trial_RT>.150 & datFrame$trial_correct==1
  }
  if(perf){
    areOK             <- !(isNAN) 
  }
  datFrame$LegC     <- 1+as.numeric(datFrame$trial_crossed_legs)
  datFrame$HandC    <- 1+as.numeric(datFrame$trial_crossed_hand)
  datFrame$RespM    <- as.numeric(datFrame$trial_blockType)
  
  nT = dim(datFrame)[1]
  sameBlock = c(F,F,datFrame$condAll[3:nT]==datFrame$condAll[2:c(nT-1)] & datFrame$condAll[3:nT]==datFrame$condAll[1:c(nT-2)])
  datFrame$prevT = NA
  datFrame$prevT[c(F,F,datFrame$trial_limbside[3:nT]==datFrame$trial_limbside[2:c(nT-1)] & 
                     datFrame$trial_limbside[3:nT]==datFrame$trial_limbside[1:c(nT-2)]) &
                   sameBlock] = "ss"
  datFrame$prevT[c(F,F,datFrame$trial_limbside[3:nT]==datFrame$trial_limbside[2:c(nT-1)] & 
                     datFrame$trial_limbside[3:nT]!=datFrame$trial_limbside[1:c(nT-2)]) &
                   sameBlock] = "ds"
    datFrame$prevT[c(F,F,datFrame$trial_limbside[3:nT]!=datFrame$trial_limbside[2:c(nT-1)] & 
                     datFrame$trial_limbside[3:nT]==datFrame$trial_limbside[1:c(nT-2)]) &
                   sameBlock] = "sd"
  datFrame$prevT[c(F,F,datFrame$trial_limbside[3:nT]!=datFrame$trial_limbside[2:c(nT-1)] & 
                     datFrame$trial_limbside[3:nT]!=datFrame$trial_limbside[1:c(nT-2)]) &
                   sameBlock] = "dd"
  datFrame$prevT <-as.factor(datFrame$prevT)
  
  nT = dim(datFrame)[1]
  sameBlock = c(F,F,F,datFrame$condAll[4:nT]==datFrame$condAll[3:c(nT-1)] & datFrame$condAll[4:nT]==datFrame$condAll[2:c(nT-2)] & datFrame$condAll[4:nT]==datFrame$condAll[1:c(nT-3)])
  datFrame$prev3T = NA
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[3:c(nT-1)] & 
                         datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[2:c(nT-2)] &
                          datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "sss"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "ssd"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "sds"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "sdd"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "ddd"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "dds"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "dsd"
  datFrame$prev3T[c(F,F,F,datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[3:c(nT-1)] & 
                      datFrame$trial_limbside[4:nT]==datFrame$trial_limbside[2:c(nT-2)] &
                      datFrame$trial_limbside[4:nT]!=datFrame$trial_limbside[1:c(nT-3)]) &
                      sameBlock] = "dss"
  datFrame$prev3T <-as.factor(datFrame$prev3T)
  # stimulus response conflic factors
  # LxL = Simulated limb anatomical side vs effector limb anatomical side
  # LxB = Simulated limb anatomical side vs button external location
  # SxB = Stimulated external location vs button external location
  # SxL = Stimulated external location vs effector limb anatomical side
  
  datFrame$cLxL     <- 1
  datFrame$cLxL[(xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand) & datFrame$RespM == 1 & (datFrame$task == 1 | (datFrame$task == 2 & exp==3) )) |
                  (!xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand) & datFrame$RespM == 1 & datFrame$task == 2 & (exp==2 | exp==22)) |
                  (datFrame$RespM == 2 & datFrame$task == 2 &  (exp==2 | exp==22))]     <- 2
  datFrame$cLxB     <- 1
  datFrame$cLxB[(datFrame$trial_crossed_legs & datFrame$RespM == 1 & datFrame$task == 1) |
                (datFrame$trial_crossed_hand & datFrame$RespM == 2 & datFrame$task == 1) |
                (!datFrame$trial_crossed_legs & datFrame$RespM == 1 & datFrame$task == 2 & exp==2) |
                (!datFrame$trial_crossed_hand & datFrame$RespM == 2 & datFrame$task == 2 & exp==2)] <- 2
  datFrame$cSxB     <- 1
  datFrame$cSxB[(xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand) & datFrame$RespM == 2 & (datFrame$task == 1)) |
                  (xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand) & datFrame$RespM == 2 & datFrame$task == 2 & exp==3) |
                (datFrame$RespM == 1 & datFrame$task == 2 & exp==2) |
                (!xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand) & datFrame$RespM == 2 & datFrame$task == 2 & exp==2)] <- 2  
  datFrame$cSxL     <-  1
  datFrame$cSxL[(datFrame$trial_crossed_hand & datFrame$RespM == 1 & datFrame$task == 1) |
                                       (datFrame$trial_crossed_legs & datFrame$RespM == 2 & datFrame$task == 1) |
                                       (!datFrame$trial_crossed_hand & datFrame$RespM == 1 & datFrame$task == 2 & exp==2) |
                                       (!datFrame$trial_crossed_legs & datFrame$RespM == 2 & datFrame$task == 2 & exp==2)] <-2
  datFrame$cLcSxBcHfac   <- as.factor(paste0(datFrame$LegC,datFrame$cSxB,datFrame$HandC))
  datFrame$cLcSxLfac     <- as.factor(paste0(datFrame$LegC,datFrame$cSxL))
  datFrame$cLxBcHfac     <- as.factor(paste0(datFrame$cLxB,datFrame$HandC))
  
  datFrame$cLcSxBcH     <- as.numeric(datFrame$cLcSxBcHfac)
  datFrame$cLcSxL       <- as.numeric(datFrame$cLcSxLfac)
  datFrame$cLxBcH       <- as.numeric(datFrame$cLxBcHfac)
  #ddply(datFrame,.(condAll),summarize,val=unique(cLxB)) # check
  datFrame$confSetfac  <- as.factor(paste0(datFrame$cLxL,datFrame$cLcSxBcH,datFrame$cLcSxL,datFrame$cLxBcH))
datFrame$confSet     <- as.numeric(datFrame$confSetfac)
}  

datFrame$areOK       <- areOK
datFrameOK           <- datFrame[datFrame$areOK,]
if (modelSource == "trialRT_ANOVA_subjMeansnomix_int.R"){
datFrameOK           <- datFrameOK[datFrameOK$task==thistask,]}
datFrameOK$RespMF    = as.factor(datFrameOK$RespM) 
datFrameOK$HandCF    = as.factor(datFrameOK$HandC) 
datFrameOK$LegCF     = as.factor(datFrameOK$LegC) 
datFrameOK$trialintF     = as.factor(datFrameOK$trial_int) 
datFrameOK$taskF     = as.factor(datFrameOK$task) 
datFrameOK$cLxLF     = as.factor(datFrameOK$cLxL) 
if(!perf){
  if(avgs & (model=='cond' | model=='condtask')){datFrameOK <-ddply(datFrameOK,.(subjIndx,RespM,LegC,HandC,task,cond,trial_int),summarize,meanRT=mean(trial_RT, na.rm=T))
  y <- as.vector(datFrameOK$meanRT) }
  if(avgs & (model=='comp')){datFrameOK <-ddply(datFrameOK,.(subjIndx,cLxL,RespM,LegC,HandC,task,cond,trial_int),summarize,meanRT=mean(trial_RT, na.rm=T))
  y <- as.vector(datFrameOK$meanRT) }
  if(avgs & (model=='conf')){datFrameOK <-ddply(datFrameOK,.(subjIndx,cLxL,LegC,HandC,trial_int),summarize,meanRT=mean(trial_RT, na.rm=T))
  y <- as.vector(datFrameOK$meanRT) }
  if(!avgs){y     <- as.vector(datFrameOK$trial_RT)}
}
if(perf){
  y <- datFrameOK$trial_correct
}

S                 <- as.numeric(as.factor(paste0("c",datFrameOK$subjIndx)))       ## not to forget that this changes the number of the subject
C                 <- as.numeric(datFrameOK$cond)         # which conditions
Call              <- as.numeric(datFrameOK$condAll)     
Cnames            <- levels(datFrameOK$cond)
CnamesAll         <- levels(datFrameOK$condAll)
LegC              <- datFrameOK$LegC
HandC             <- datFrameOK$HandC
RespM             <- datFrameOK$RespM
Int               <- datFrameOK$trial_int
cLxL              <- datFrameOK$cLxL

task              <- datFrameOK$task
Ndata             <- length(y)
NSubj             <- length(unique(S))
NCond             <- length(unique(C))

if(!perf){
  #if(logn)   {y     <- log(as.vector(datFrameOK$trial_RT[areOK]))}
  yMean = mean( y, na.rm=T ); ySD = sd( y, na.rm=T )
  
  # For prior on deflections:
   aGammaShRa = unlist( gammaShRaFromModeSD( mode=sd(y)/2 , sd=2*sd(y) ) )
  # # For prior on cell SDs:
   if(factorial & (model=='cond' | model=='comp')){
     cellSDs = aggregate( y , list(RespM,LegC,HandC,task,Int) , FUN=sd )
   }else if(factorial & model=='condtask'){
     cellSDs = aggregate( y , list(RespM,LegC,HandC,task) , FUN=sd )
     }else if(factorial & model=='conf'){
     cellSDs = aggregate( y , list(cLxL,LegC,HandC,Int) , FUN=sd )
   }else if(factorial & (model=='SR'  | model=='test')){
     cellSDs = aggregate( y , list(LegC,HandC,datFrameOK$cLxL,datFrameOK$cLxB,datFrameOK$cSxB,datFrameOK$cSxL) , FUN=sd )
   }else if(factorial & model=='SRred'){
     cellSDs = aggregate( y , list(LegC,HandC,datFrameOK$cLxL,datFrameOK$cLxB,datFrameOK$cSxB) , FUN=sd )
   }else if(factorial & model=='SRcond'){
     cellSDs = aggregate( y , list(  datFrameOK$confSet,RespM,task) , FUN=sd )
   }else if(factorial & model=='Lc_Hc_cLxL'){
     cellSDs = aggregate( y , list( LegC,HandC,datFrameOK$cLxL) , FUN=sd )
   }else if(factorial & model=='SRchain'){
  #   cellSDs = aggregate( y , list(datFrameOK$cLxL,datFrameOK$cLcSxBcH,datFrameOK$cLcSxL,datFrameOK$cLxBcH) , FUN=sd )
  #   
     }
   if(!factorial){
     cellSDs = aggregate( y , list(C) , FUN=sd )
   }
  cellSDs = cellSDs[ !is.na(cellSDs$x) ,]
  medianCellSD = median( cellSDs$x , na.rm=TRUE )
  sdCellSD = sd( cellSDs$x , na.rm=TRUE )
  show( paste( "Median cell SD: " , medianCellSD ) )
  show( paste( "StDev. cell SD: " , sdCellSD ) )
  sGammaShRa = unlist( gammaShRaFromModeSD( mode=medianCellSD , sd=2*sdCellSD ) )
}

if(!perf & model=='cond'){
dataList = list(
    y     = y ,
    yMean = yMean,
    ySD   = ySD,
    LegC  = LegC,
    HandC = HandC,
  #  task  = task,
    RespM = RespM, 
 #   cLxL  = cLxL,
    Int   = Int,
    S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    medianCellSD = medianCellSD ,
    aGammaShRa = aGammaShRa ,
    sGammaShRa = sGammaShRa)}

if(!perf & model=='conf'){
  dataList = list(
    y     = y ,
    yMean = yMean,
    ySD   = ySD,
    LegC  = LegC,
    HandC = HandC,
    #RespM = RespM, 
       cLxL  = cLxL,
    Int   = Int,
    S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    medianCellSD = medianCellSD ,
    aGammaShRa = aGammaShRa ,
    sGammaShRa = sGammaShRa)}
if(!perf & (model=='comp' | model=='condtask')){
  dataList = list(
    y     = y ,
    yMean = yMean,
    ySD   = ySD,
    LegC  = LegC,
    HandC = HandC,
    RespM = RespM, 
   # cLxL  = cLxL,
    task = task,
    Int   = Int,
    S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    medianCellSD = medianCellSD ,
    aGammaShRa = aGammaShRa ,
    sGammaShRa = sGammaShRa)}
if(perf){
  dataList = list(
    y     = y ,
   C     = C , # condition
   RespM = RespM,
    LegC  = LegC,
    HandC = HandC,
    task  = task,
   Int = Int,
     S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    NCond = NCond)}

#####################
# data for bielefeld
# keeps <- c("subjIndx", "trial_RT","trial_correct","RespM","trialnr","cond","task")
# dF = datFrame[keeps]
# isNANdF             <- is.nan(dF$trial_RT)
# areOKdF             <- !(isNANdF) & dF$trial_RT>.150 & dF$RespM==2 & dF$task==1
# 
# dF   <- dF[areOKdF,]
# dF    = subset(dF, select = -c(RespM,task))
# dFallC   = subset(dF, trial_correct==1)
# dFc   = ddply(dF,.(subjIndx,cond,trial_correct),summarize,mRT=mean(trial_RT))
# dFc   = subset(dFc, trial_correct==1)
# save(dFallC,dFc, file="/Users/jossando/trabajo/E299/05_Code/02_Analysis/rcode/bielefeld_examples/dataE299_bielefeld.Rdata")
