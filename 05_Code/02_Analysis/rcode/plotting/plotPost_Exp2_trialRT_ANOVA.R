library(gridExtra)

chainLength = length(mcmcChain[, "b0" ])

source("code/analysis/rcode/plotting/multiPlot.R")

if(model=="Lc_Hc_cLxL"){
  
  # MAIN EFFECTS
  mainEffects = c('LegC','HandC','cLxL')
  Sample     = array( 0 , dim=c( 6 , chainLength ) )
  xidx <-1
  for ( bs in 1:3 ) { for ( lev in 1:2 ) { 
    Sample[xidx,] = mcmcChain[, paste("b",bs,"[",lev,"]",sep="") ]
    xidx <- xidx +1
  }}
  gnames = c("UC_Legs","C_Legs","UC_Hands","C_Hands","uLxL","cLxL")
  contrastList = list(UC_Legs = (gnames=="UC_Legs"), C_Legs = (gnames=="C_Legs"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
                      UC_Hands = (gnames=="UC_Hands"), C_Hands = (gnames=="C_Hands"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"),
                      ULxL = (gnames=="uLxL"), CLxL = (gnames=="cLxL"),ULxL_vs_CLxL = (gnames=="uLxL")-(gnames=="cLxL"))
  
  cbbPalette  <- rep("#606060",9)
  
  multiPlot(Sample,contrastList = contrastList,contPplot = c(3,3),cbbPalette = cbbPalette,xlims = c(-.2,.1,.05),
            SData = F, diffplot = rep(c('F','F','T'),3), subj_dataALL)
  saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")
  
  # 2xway INTERACTIONS
  bss = c("b1","b2","b3")
  allComb = combn(bss,2)
  Sample     = array( 0 , dim=c( dim(allComb)[2]*4 , chainLength ) )
  xidx <-1
  for (asC in 1:dim(allComb)[2]){for (i in 1:2) {for (ii in 1:2) {
    Combname   = paste(allComb[1,asC],allComb[2,asC],"[",i,",",ii,"]",sep="")
    Sample[xidx,] = mcmcChain[, Combname]
    gnames[xidx] = Combname
    
    xidx <- xidx +1
  } } }
  
  contrastList = list()
  for (asC in 1:length(gnames)){
    contrastList[[gnames[asC]]] <- (gnames==gnames[asC])
  }
  cbbPalette  <- rep("#606060",12)
  multiPlot(Sample,contrastList = contrastList,contPplot = c(4,3),cbbPalette = cbbPalette,xlims = c(-.2,.1,.05),
            SData = F, diffplot = rep(c('F','F','F','F'),3), subj_dataALL)
  
}
  








######################################################################################
# Main Effects and diffs
if(model=='SR'){
mainEffects = c('LegC','HandC','cLxL','cLxB','cSxB','cSxL')
xidx <-1
Sample     = array( 0 , dim=c( 12 , chainLength ) )
for ( bs in 1:6 ) { for ( lev in 1:2 ) { 
  Sample[xidx,] = mcmcChain[, paste("b",bs,"[",lev,"]",sep="") ]
  xidx <- xidx +1
}}
gnames = c("UC_Legs","C_Legs","UC_Hands","C_Hands","uLxL","cLxL","uLxB","cLxB","uSxB","cSxB","uSxL","cSxL")
contrastList = list(UC_Legs = (gnames=="UC_Legs"), C_Legs = (gnames=="C_Legs"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
                    UC_Hands = (gnames=="UC_Hands"), C_Hands = (gnames=="C_Hands"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"),
                    ULxL = (gnames=="uLxL"), CLxL = (gnames=="cLxL"),ULxL_vs_CLxL = (gnames=="uLxL")-(gnames=="cLxL"),
                    ULxB = (gnames=="uLxB"), CLxB = (gnames=="cLxB"),ULxB_vs_CLxB = (gnames=="uLxB")-(gnames=="cLxB"),
                    USxB = (gnames=="uSxB"), CSxB = (gnames=="cSxB"),USxB_vs_CSxB = (gnames=="uSxB")-(gnames=="cSxB"),
                    USxL = (gnames=="uSxL"), CSxL = (gnames=="cSxL"),USxL_vs_CSxL = (gnames=="uSxL")-(gnames=="cSxL"))

cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")

multiPlot(Sample,contrastList = contrastList,contPplot = c(6,3),cbbPalette = cbbPalette,xlims = c(-.2,.1,.05),
          SData = F, diffplot = c(F,F,T,F,F,T,F,F,T,F,F,T,F,F,T,F,F,T), subj_dataALL)
saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")
}
if(model=='SRcond'){
  mainEffects = c('confSet','RespM','task')
  xidx <-1
  Sample     = array( 0 , dim=c( 12 , chainLength ) )
  for ( bs in 1:3 ) { 
    if(bs==1){ levs = 8}else{levs=2}
    for ( lev in 1:levs ) { 
    Sample[xidx,] = mcmcChain[, paste("b",bs,"[",lev,"]",sep="") ]
    xidx <- xidx +1
  }}
  gnames = c("uLuHTn","uLcHTnReTaRa","uLuHTa","uLcHTnRaTaRe","cLuHTnReTaRa","cLcHTn","cLuHTnRaTaRe","cLcHTa","RespMext","RespManat","Taskpro","Taskanti")
  contrastList = list(uLuHTn = (gnames=="uLuHTn"),uLcHTnReTaRa = (gnames=="uLcHTnReTaRa"),uLuHTa = (gnames=="uLuHTa"),uLcHTnRaTaRe = (gnames=="uLcHTnRaTaRe"),
                      cLuHTnReTaRa = (gnames=="cLuHTnReTaRa"),cLcHTn = (gnames=="cLcHTn"),cLuHTnRaTaRe = (gnames=="cLuHTnRaTaRe"),cLcHTa = (gnames=="cLcHTa"),
                      RespMext = (gnames=="RespMext"), RespManat = (gnames=="RespManat"),RespMext_vs_RespManat = (gnames=="RespMext")-(gnames=="RespManat"),
                      Taskpro = (gnames=="Taskpro"), Taskanti = (gnames=="Taskanti"),Taskpro_vs_Taskanti = (gnames=="Taskpro")-(gnames=="Taskanti"))
  
  cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")
  
  multiPlot(Sample,contrastList = contrastList,contPplot = c(6,3),cbbPalette = cbbPalette,xlims = c(-.2,.1,.05),
            SData = F, diffplot = c(F,F,F,F,F,F,F,F,F,F,T,F,F,T), subj_dataALL)
  saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")
}
if(model=='SRchain'){
  mainEffects = c('cLxL','cLcSxBcH','cLcSxL','cLxBcH')
  mElevs      = c(2,8,4,4)
  xidx <-1
  Sample     = array( 0 , dim=c( sum(mElevs) , chainLength ) )
  for ( bs in 1:length(mainEffects) ) { for ( lev in 1:mElevs[bs] ) { 
    Sample[xidx,] = mcmcChain[, paste("b",bs,"[",lev,"]",sep="") ]
    xidx <- xidx +1
  }}
  gnames = c("uLxL","cLxL",
             "uLuSxBuH","uLuSxBcH","uLcSxBuH","uLcSxBcH","cLuSxBuH","cLuSxBcH","cLcSxBuH","cLcSxBcH",
             "uLuSxL","uLcSxL","cLuSxL","cLcSxL",
             "uLxBuH","uLxBcH","cLxBuH","cLxBcH")
  contrastList = list(uLxL = (gnames=="uLxL"),cLxL = (gnames=="cLxL"),
                      uLuSxBuH= (gnames=="uLuSxBuH"), uLuSxBcH = (gnames=="uLuSxBcH"), uLcSxBuH = (gnames=="uLcSxBuH"), uLcSxBcH = (gnames=="uLcSxBcH"),
                      cLuSxBuH = (gnames=="cLuSxBuH"), cLuSxBcH = (gnames=="cLuSxBcH"), cLcSxBuH = (gnames=="cLcSxBuH"), cLcSxBcH = (gnames=="cLcSxBcH"),
                      uLuSxL = (gnames=="uLuSxL"), uLcSxL = (gnames=="uLcSxL"), cLuSxL = (gnames=="cLuSxL"), cLcSxL = (gnames=="cLcSxL"),
                      uLxBuH = (gnames=="uLxBuH"), uLxBcH = (gnames=="uLxBcH"),cLxBuH = (gnames=="cLxBuH"), cLxBcH = (gnames=="cLxBcH"))
  
  cbbPalette  <- rep("#606060",length(  contrastList ))
  
  multiPlot(Sample,contrastList = contrastList,contPplot = c(6,3),cbbPalette = cbbPalette,xlims = c(-.2,.2,.05),
            SData = F, diffplot = c(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F), subj_dataALL)
  saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")
}

###########################################################
# Simple Effects
# arrenging data and setting-up contrasts and colors
#condList = ddply(datFrame,.(condAll),summarize,cL=unique(LegC),cH=unique(HandC), cLxL=unique(cLxL),cLxB=unique(cLxB),cSxB=unique(cSxB),cSxL=unique(cSxL))

# Sample     = array( 0 , dim=c( 16 , chainLength ) )
# for ( bs in 1:16 ) { 
#   Sample[bs,] = mcmcChain[, paste("m[",condList$cL[bs],",",condList$cH[bs],",",condList$cLxB[bs],",",condList$cSxB[bs],"]",sep="") ]
# }

#######################################################################
# COND MODEL PLOTS
# FIXED EFFECTS FACTORS ARE RespM,LegC,HandC, and task
if(model=='SRcond'){

  condList = ddply(datFrame, .(condAll), summarize, RespM=unique(RespM), cL=unique(LegC), cH=unique(HandC), task=unique(task))

  Sample     = array( 0 , dim=c( 16 , chainLength ) )
  for ( bs in 1:16 ) { 
    Sample[bs,] = mcmcChain[, paste("m[",condList$RespM[bs],",",condList$cL[bs],",",condList$cH[bs],",",condList$task[bs],"]",sep="") ]
  }

  contrastList  = list(ReLuHuTn = (CnamesAll=="ReLuHuTn"),ReLuHcTn = (CnamesAll=="ReLuHcTn"),ReLcHuTn = (CnamesAll=="ReLcHuTn"),ReLcHcTn = (CnamesAll=="ReLcHcTn"),
                       RaLuHuTn = (CnamesAll=="RaLuHuTn"),RaLuHcTn = (CnamesAll=="RaLuHcTn"),RaLcHuTn = (CnamesAll=="RaLcHuTn"),RaLcHcTn = (CnamesAll=="RaLcHcTn"),
                       ReLuHuTa = (CnamesAll=="ReLuHuTa"),ReLuHcTa = (CnamesAll=="ReLuHcTa"),ReLcHuTa = (CnamesAll=="ReLcHuTa"),ReLcHcTa = (CnamesAll=="ReLcHcTa"),
                       RaLuHuTa = (CnamesAll=="RaLuHuTa"),RaLuHcTa = (CnamesAll=="RaLuHcTa"),RaLcHuTa = (CnamesAll=="RaLcHuTa"),RaLcHcTa = (CnamesAll=="RaLcHcTa"))
  cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
  
  subj_dataALL    <- list()
  for ( cIdx in 1:length(contrastList) ){
  subj_dataALL[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,value=mean(get("trial_RT")[cond==names(contrastList[cIdx])], na.rm=T))
  }

 multiPlot(Sample,contrastList = contrastList,contPplot = c(4,4),cbbPalette = cbbPalette,xlims = c(.2,.95,.1),
          SData = TRUE, diffplot = FALSE, subj_dataALL)
 saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_simpleEffect",sep=""), type="pdf")

# # Differences
 assign(paste(CnamesAll[1],'vs',CnamesAll[2],sep="_") ,1)
allSimpleComb = combn(CnamesAll,2);
 contrastList = list()
for (asC in 1:dim(allSimpleComb)[2]){
  #contrastList = list.append(contrastList,assign(paste(allSimpleComb[1,asC],'vs',allSimpleComb[2,asC],sep="_"),(CnamesAll==allSimpleComb[1,asC])-(CnamesAll==allSimpleComb[2,asC])))
  contrastList[[paste(allSimpleComb[1,asC],'vs',allSimpleComb[2,asC],sep="_")]] <- (CnamesAll==allSimpleComb[1,asC])-(CnamesAll==allSimpleComb[2,asC])
  
}
  subj_dataALLdiff    <- list()
 for ( cIdx in 1:length(contrastList) ){
   difIndxs <- which(contrastList[[cIdx]] !=0)
   subj_dataALLdiff[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,
                                     value=mean(get("trial_RT")[condAll==CnamesAll[difIndxs[1]]], na.rm=T)-mean(get("trial_RT")[condAll==CnamesAll[difIndxs[2]]], na.rm=T))
 }
 multiPlot(Sample,contrastList = contrastList,contPplot = c(12,10),cbbPalette = cbbPalette,xlims = c(-.4,.26,.1),
           SData = TRUE, diffplot = TRUE, subj_dataALLdiff)
 saveGraph(file=paste(getwd(),"/figures/LH2crossExp2/bayes/",fileNameRoot,"_simpleEffectDiff",sep=""), type="pdf")
}

