library(gridExtra)

mainEffects = c('LegC','HandC','task')
chainLength = length(mcmcChain[, "b0" ])

source("code/analysis/rcode/plotting/multiPlot.R")

###########################################################
# Simple Effects
# arrenging data and setting-up contrasts and colors

xidx <-1
Sample     = array( 0 , dim=c( dataList$NCond , chainLength ) )
for ( bt in 1:2 ) { for ( cl in 1:2 ) { for ( ch in 1:2 ) { 
  Sample[xidx,] = mcmcChain[, paste("m[",bt,",",cl,",",ch,"]",sep="") ]
  xidx <- xidx +1
}}}
contrastList  = list(ReLuHu = (Cnames=="ReLuHu"),ReLuHc = (Cnames=="ReLuHc"),ReLcHu = (Cnames=="ReLcHu"),ReLcHc = (Cnames=="ReLcHc"),
                     RaLuHu = (Cnames=="RaLuHu"),RaLuHc = (Cnames=="RaLuHc"),RaLcHu = (Cnames=="RaLcHu"),RaLcHc = (Cnames=="RaLcHc"))
cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")

subj_dataALL    <- list()
for ( cIdx in 1:length(contrastList) ){
subj_dataALL[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,value=mean(get("trial_RT")[cond==names(contrastList[cIdx])], na.rm=T))
}

multiPlot(Sample,contrastList = contrastList,contPplot = c(4,2),cbbPalette = cbbPalette,xlims = c(.2,.95,.1),
          SData = TRUE, diffplot = FALSE, subj_dataALL)
saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_simpleEffect",sep=""), type="pdf")

# Differences
contrastList  = list(ReLuHu_vs_ReLuHc = (Cnames=="ReLuHu")-(Cnames=="ReLuHc"),ReLuHu_vs_ReLcHu = (Cnames=="ReLuHu")-(Cnames=="ReLcHu"),
                     ReLuHu_vs_ReLcHc = (Cnames=="ReLuHu")-(Cnames=="ReLcHc"),ReLuHc_vs_ReLcHu = (Cnames=="ReLuHc")-(Cnames=="ReLcHu"),
                     ReLuHc_vs_ReLcHc = (Cnames=="ReLuHc")-(Cnames=="ReLcHc"),ReLcHu_vs_ReLcHc = (Cnames=="ReLcHu")-(Cnames=="ReLcHc"),
                     RaLuHu_vs_RaLuHc = (Cnames=="RaLuHu")-(Cnames=="RaLuHc"),RaLuHu_vs_RaLcHu = (Cnames=="RaLuHu")-(Cnames=="RaLcHu"),
                     RaLuHu_vs_RaLcHc = (Cnames=="RaLuHu")-(Cnames=="RaLcHc"),RaLuHc_vs_RaLcHu = (Cnames=="RaLuHc")-(Cnames=="RaLcHu"),
                     RaLuHc_vs_RaLcHc = (Cnames=="RaLuHc")-(Cnames=="RaLcHc"),RaLcHu_vs_RaLcHc = (Cnames=="RaLcHu")-(Cnames=="RaLcHc"),
                     ReLuHu_vs_RaLuHu = (Cnames=="ReLuHu")-(Cnames=="RaLuHu"),ReLuHc_vs_RaLuHc = (Cnames=="ReLuHc")-(Cnames=="RaLuHc"),
                     ReLcHc_vs_RaLuHu = (Cnames=="ReLcHc")-(Cnames=="RaLuHu"),ReLcHu_vs_RaLcHu = (Cnames=="ReLcHc")-(Cnames=="RaLcHc"))

subj_dataALLdiff    <- list()
for ( cIdx in 1:length(contrastList) ){
  difIndxs <- which(contrastList[[cIdx]] !=0)
  subj_dataALLdiff[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,
                                    value=mean(get("trial_RT")[cond==Cnames[difIndxs[1]]], na.rm=T)-mean(get("trial_RT")[cond==Cnames[difIndxs[2]]], na.rm=T))
}
multiPlot(Sample,contrastList = contrastList,contPplot = c(6,3),cbbPalette = cbbPalette,xlims = c(-.4,.26,.1),
          SData = TRUE, diffplot = TRUE, subj_dataALLdiff)
saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_simpleEffectDiff",sep=""), type="pdf")

######################################################################################
# Main Effects and diffs

xidx <-1
Sample     = array( 0 , dim=c( 6 , chainLength ) )
for ( bs in 1:3 ) { for ( lev in 1:2 ) { 
  Sample[xidx,] = mcmcChain[, paste("b",bs,"[",lev,"]",sep="") ]
  xidx <- xidx +1
}}
gnames = c("Ext","Anat","UC_Legs","C_Legs","UC_Hands","C_Hands")
contrastList = list(Ext = (gnames=="Ext"), Anat = (gnames=="Anat"),Ext_vs_Anat = (gnames=="Ext")-(gnames=="Anat"),
                    UC_Legs = (gnames=="UC_Legs"), C_Legs = (gnames=="C_Legs"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
                    UC_Hands = (gnames=="UC_Hands"), C_Hands = (gnames=="C_Hands"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"))

cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")

multiPlot(Sample,contrastList = contrastList,contPplot = c(3,3),cbbPalette = cbbPalette,xlims = c(-.2,.1,.05),
          SData = F, diffplot = c(F,F,T,F,F,T,F,F,T), subj_dataALL)
saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")