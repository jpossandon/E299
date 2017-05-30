
datFrame          <- read.csv(file="data/LH2cross/allSubject.csv" , sep="," , strip.white=T)
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
isNAN             <- is.nan(datFrame$trial_RT)
areOK             <- !(isNAN)
datFrame$areOK    <- areOK

if(logn)   {y     <- log(as.vector(datFrame$trial_RT[areOK]))}
if(!logn)  {y     <- as.vector(datFrame$trial_RT[areOK])}

S                 <- as.numeric(datFrame$subjIndx); S <- S[areOK]        ## not to forget that this changes the number of the subject
C                 <- as.numeric(datFrame$cond) ; C <- C[areOK]         # which conditions

Ndata             <- length(y)
NSubj             <- length(unique(S))
NCond             <- length(unique(C))

yM = mean( y, na.rm=T ); ySD = sd( y, na.rm=T )

# zx = ( x - xM ) # / xSD 
zy = ( y - yM )  / ySD # change to chekc the gamma distribution in which y values cannot be negative
#if(censor){censorLimit = ( censorLimit - yM )} # / ySD} # change to chekc the gamma distribution in which y values cannot be negative

dataList = list(
    y   = zy ,
    C   = C , # condition
    S   = S, # subject condition
    Ndata = Ndata ,
    NSubj = NSubj ,
    NCond = NCond)
