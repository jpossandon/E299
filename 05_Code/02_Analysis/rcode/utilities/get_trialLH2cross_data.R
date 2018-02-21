
if(exp==1){datFrame          <- read.csv(file="data/LH2cross/allSubject.csv" , sep="," , strip.white=T)}
if(exp==2 & task=="normal"){datFrame          <- read.csv(file="data/LH2cross/allSubjectExp2.csv" , sep="," , strip.white=T)}
if(exp==2 & task=="anti"){datFrame          <- read.csv(file="data/LH2crossAnti/allSubjectExp2Anti.csv" , sep="," , strip.white=T)}

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
#datFrame$trialnr  <- rep(rep(seq(1,10,1),rep(10,10)),dim(datFrame)[1]/100)
datFrame$trialnr  <- rep(rep(seq(2.5,100,5),rep(5,20)),dim(datFrame)[1]/100)

datFrame          <- datFrame[ ! datFrame$subjIndx %in% rem_subjects,]

isNAN             <- is.nan(datFrame$trial_RT)
areOK             <- !(isNAN) & datFrame$trial_RT>.150 & datFrame$trial_correct==1
datFrame$LegC     <- 1+as.numeric(datFrame$trial_crossed_legs)
datFrame$HandC    <- 1+as.numeric(datFrame$trial_crossed_hand)
datFrame$task     <- as.numeric(datFrame$trial_blockType)
#datFrame$SRa      <- 1+as.numeric(datFrame$trial_blockType==2 | !xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand)) # Stim-resp anat 0 - congruent, 1 -incongruent
#datFrame$SRe      <- 1+as.numeric(datFrame$trial_blockType==1 | !xor(datFrame$trial_crossed_legs, datFrame$trial_crossed_hand)) # Stim-resp ext 0 - congruent, 1 -incongruent
datFrame$areOK    <- areOK
datFrameOK        <- datFrame[datFrame$areOK,]

if(!perf){
  if(avgs){datFrameOK <-ddply(datFrameOK,.(subjIndx,LegC,HandC,task,cond),summarize,meanRT=mean(trial_RT, na.rm=T))
  y <- as.vector(datFrameOK$meanRT) }
  if(!avgs){y     <- as.vector(datFrameOK$trial_RT)}
}
if(perf){
  y <- datFrameOK$trial_correct
}

S                 <- as.numeric(as.factor(paste0("c",datFrameOK$subjIndx)))       ## not to forget that this changes the number of the subject
C                 <- as.numeric(datFrameOK$cond)         # which conditions

Cnames            <- levels(datFrameOK$cond)
LegC              <- datFrameOK$LegC
HandC             <- datFrameOK$HandC
#SRa               <- datFrameOK$SRa
#SRe               <- datFrameOK$SRe
task              <- datFrameOK$task
Ndata             <- length(y)
NSubj             <- length(unique(S))
NCond             <- length(unique(C))

if(!perf){
  #if(logn)   {y     <- log(as.vector(datFrameOK$trial_RT[areOK]))}
  yMean = mean( y, na.rm=T ); ySD = sd( y, na.rm=T )
  
  # For prior on deflections:
  aGammaShRa = unlist( gammaShRaFromModeSD( mode=sd(y)/2 , sd=2*sd(y) ) )
  # For prior on cell SDs:
  if(factorial){
  cellSDs = aggregate( y , list(LegC,HandC,task) , FUN=sd )
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

if(!perf){
dataList = list(
    y     = y ,
    yMean = yMean,
    ySD   = ySD,
    C     = C , # condition
    LegC  = LegC,
    HandC = HandC,
    task  = task,
#    SRa   = SRa,
#    SRe   = SRe,
    S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    NCond = NCond,
    medianCellSD = medianCellSD ,
    aGammaShRa = aGammaShRa ,
    sGammaShRa = sGammaShRa)}

if(perf){
  dataList = list(
    y     = y ,
   C     = C , # condition
    LegC  = LegC,
    HandC = HandC,
    task  = task,
     S     = S , # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    NCond = NCond)}