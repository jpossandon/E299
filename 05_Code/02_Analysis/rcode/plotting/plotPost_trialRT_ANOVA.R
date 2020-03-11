library(gridExtra)
chainLength = length(mcmcChain[[1]][, "b0" ])


######################################################################################
# Main Effects and diffs
for (t in 1:length(tsks)){
  if(exp==1 | exp==3){
    
    Sample     = array( 0 , dim=c( 8 , chainLength ) )
    Sample[1,] = 2*mcmcChain[[t]][,"b1[1]"]          # RespM ([1] = ext = (Ext-anat)/2)
    Sample[2,] = 2*mcmcChain[[t]][,"b2[2]"]          # LegC  ([2] = cLeg = (cLeg-uLeg)/2)
    Sample[3,] = 2*mcmcChain[[t]][,"b3[2]"]          # HandC ([2] = cHand = (cHand-uHand)/2)
    Sample[4,] = 2*mcmcChain[[t]][,"b4[1]"]          # Int ([1] = high = (high-low)/2)
    if(t==1){
    Sample[5,] = -2*mcmcChain[[t]][,"b1b2[1,1]"]     # -RespMxLegC = cLxB Ext - cSxL Anat 
    Sample[6,] = 2*mcmcChain[[t]][,"b1b3[1,1]"]      # RespMxHandC = cLxB Anat - cSxL Ext 
    }
    if(t==2){
      Sample[5,] = 2*mcmcChain[[t]][,"b1b2[1,1]"]     # RespMxLegC = cLxB Ext - cSxL Anat 
      Sample[6,] = -2*mcmcChain[[t]][,"b1b3[1,1]"]      # -RespMxHandC = cLxB Anat - cSxL Ext 
    }
    Sample[7,] = -2*mcmcChain[[t]][,"b2b3[1,1]"]   # LegCxHandC  = -LxLext-SxBanat = LxLext+SxBanat   
    Sample[8,] = -2*mcmcChain[[t]][,"b1b2b3[1,1,1]"] # RespMxLegCxHandC  = LxLext-SxBanat = LxLext+cSxBanat = cSxBanat-cLxLext = -(cLxLext-cSxBanat)
    Sample = round(Sample*1000)
  }
  if(exp==22 & model == "conf"){
    Sample     = array( 0 , dim=c( 7 , chainLength ) )
    Sample[1,] = -2*mcmcChain[[t]][,"b1[1]"]          # LxL   
    Sample[2,] = 2*mcmcChain[[t]][,"b2[2]"]          # LegC  ([2] = cLeg = (cLeg-uLeg)/2)
    Sample[3,] = 2*mcmcChain[[t]][,"b3[2]"]          # HandC ([2] = cHand = (cHand-uHand)/2)
    Sample[4,] = -2*mcmcChain[[t]][,"b1b2[1,1]"]     # LxLcxLegC = -SxL
    Sample[5,] = -2*mcmcChain[[t]][,"b1b3[1,1]"]      # LxLcxHandC = LxB
    Sample[6,] = -2*mcmcChain[[t]][,"b2b3[1,1]"]   # LegCxHandC  = -LxLext-SxBanat = LxLext+SxBanat   
    Sample[7,] = -2*mcmcChain[[t]][,"b1b2b3[1,1,1]"] # LxLcxLegCxHandC  = SxB
    Sample = round(Sample*1000)
    xlmsES = c(-2,3.5,1)
    xlms = c(-100,200,50)
  }
  if (exp==1){ xlmsES = c(-1,4,1)
  xlms = c(-50,200,50)}
  if (exp==3){ xlmsES = c(-1,3.5,1)
  xlms = c(-50,200,50)}
  
  if (model=='conf'){
      gnames = c("cLxL","C_Legs","C_Hands","cLxLxC_Legs","cLxLxC_Hands","CLegsxC_Hands","cLxLxC_LegsxC_Hands")
      contrastList = list(cLxL_LxL = (gnames=="cLxL"), C_Legs_vs_UC_Legs = (gnames=="C_Legs"),C_Hands_vs_UC_Hands = (gnames=="C_Hands"),
                          cLxLxC_Legs = (gnames=="cLxLxC_Legs"), cLxLxC_Hands = (gnames=="cLxLxC_Hands"),CLegsxC_Hands = (gnames=="CLegsxC_Hands"),
                          cLxLxC_LegsxC_Hands = (gnames=="cLxLxC_LegsxC_Hands"))}
  if (model=='cond' & (exp %in% c(1,3,4))){
    gnames = c("Ext","C_Legs","C_Hands","HInt","RespMxC_Legs","RespMxC_Hands","CLegsxC_Hands","RespMxC_LegsxC_Hands")
    contrastList = list(Ext_vs_Anat = (gnames=="Ext"), C_Legs_vs_UC_Legs = (gnames=="C_Legs"),C_Hands_vs_UC_Hands = (gnames=="C_Hands"),Hint_vs_Lint = (gnames=="HInt"),
    RespMxC_Legs = (gnames=="RespMxC_Legs"), RespMxC_Hands = (gnames=="RespMxC_Hands"),CLegsxC_Hands = (gnames=="CLegsxC_Hands"),
    RespMxC_LegsxC_Hands = (gnames=="RespMxC_LegsxC_Hands"))}
  cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")
  
  pltList <- multiPlot(Sample,contrastList = contrastList,contPplot = c(8,1),cbbPalette = cbbPalette,xlims = xlms,
            SData = F, diffplot = c(F,F,F,F,F,F,F,F), subj_dataALL)

  #ggsave(file = paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,".pdf",sep=""), arrangeGrob(grobs = pltList, nrow=contPplot[1], ncol=contPplot[2])) 
  
  saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,sep=""), type="pdf")
  #ggsave(file=paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,sep=""))
  # Effect Size
  if(exp==1 | exp==3){
    Sample     = array( 0 , dim=c( 8 , chainLength ) )
    Sample[1,] = 2*mcmcChain[[t]][,"b1[1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))          # RespM ([1] = ext = (Ext-anat)/2)
    Sample[2,] = 2*mcmcChain[[t]][,"b2[2]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))          # LegC  ([2] = cLeg = (cLeg-uLeg)/2)
    Sample[3,] = 2*mcmcChain[[t]][,"b3[2]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))          # HandC ([2] = cHand = (cHand-uHand)/2)
    Sample[4,] = 2*mcmcChain[[t]][,"b4[1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))           # Int ([1] = high = (high-low)/2)
    if(t==1){
      Sample[5,] = -2*mcmcChain[[t]][,"b1b2[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))      # -RespMxLegC = cLxB Ext - cSxL Anat 
      Sample[6,] = 2*mcmcChain[[t]][,"b1b3[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))       # RespMxHandC = cLxB Anat - cSxL Ext 
    }
    if(t==2){
      Sample[5,] = 2*mcmcChain[[t]][,"b1b2[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))      # RespMxLegC = cLxB Ext - cSxL Anat 
      Sample[6,] = -2*mcmcChain[[t]][,"b1b3[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))       # -RespMxHandC = cLxB Anat - cSxL Ext 
    }
     Sample[7,] = -2*mcmcChain[[t]][,"b2b3[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2))   # LegCxHandC  = -LxLext-SxBanat = -2(LxLext+SxBanat)/2   
    Sample[8,] = -2*mcmcChain[[t]][,"b1b2b3[1,1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:16]^2)) # RespMxLegCxHandC  = LxLext-SxBanat = LxLext+cSxBanat = cSxBanat-cLxLext = -(cLxLext-cSxBanat)
  }
  if(exp==22 & model == "conf"){
    Sample     = array( 0 , dim=c( 7 , chainLength ) )
    Sample[1,] = -2*mcmcChain[[t]][,"b1[1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))          # LxL   
    Sample[2,] = 2*mcmcChain[[t]][,"b2[2]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))          # LegC  ([2] = cLeg = (cLeg-uLeg)/2)
    Sample[3,] = 2*mcmcChain[[t]][,"b3[2]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))          # HandC ([2] = cHand = (cHand-uHand)/2)
    Sample[4,] = -2*mcmcChain[[t]][,"b1b2[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))     # LxLcxLegC = -SxL
    Sample[5,] = -2*mcmcChain[[t]][,"b1b3[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))      # LxLcxHandC = LxB
    Sample[6,] = -2*mcmcChain[[t]][,"b2b3[1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2))   # LegCxHandC  = -LxLext-SxBanat = LxLext+SxBanat   
    Sample[7,] = -2*mcmcChain[[t]][,"b1b2b3[1,1,1]"]/sqrt(rowMeans(mcmcChain[[t]][,1:8]^2)) # LxLcxLegCxHandC  = SxB
   } 
  
  # if (model=='conf'){
  #   gnames = c("LxL","cLxL","UC_Legs","C_Legs","UC_Hands","C_Hands")
  #   contrastList = list(cLxL_vs_LxL = (gnames=="cLxL")-(gnames=="LxL"),
  #                       C_Legs_vs_UC_Legs = (gnames=="C_Legs")-(gnames=="UC_Legs"),
  #                       C_Hands_vs_UC_Hands = (gnames=="C_Hands")-(gnames=="UC_Hands"))}
  # if (model=='cond' & (exp %in% c(1,4))){
  #   gnames = c("Ext","C_Legs","C_Hands","HInt","RespMxC_Legs","RespMxC_Hands","CLegsxC_Hands","RespMxC_LegsxC_Hands")
  #   contrastList = list(Ext_vs_Anat = (gnames=="Ext"), C_Legs_vs_UC_Legs = (gnames=="C_Legs"),C_Hands_vs_UC_Hands = (gnames=="C_Hands"),Hint_vs_Lint = (gnames=="HInt"),
  #                       RespMxC_Legs = (gnames=="RespMxC_Legs"), RespMxC_Hands = (gnames=="RespMxC_Hands"),CLegsxC_Hands = (gnames=="CLegsxC_Hands"),
  #                       RespMxC_LegsxC_Hands = (gnames=="RespMxC_LegsxC_Hands"))}
  #contrastList = list(LxL = (gnames=="LxL"), cLxL = (gnames=="cLxL"),LxL_vs_cLxL = (gnames=="LxL")-(gnames=="cLxL"),
  #                  UC_Legs = (gnames=="UC_Legs"), C_Legs = (gnames=="C_Legs"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
  #                UC_Hands = (gnames=="UC_Hands"), C_Hands = (gnames=="C_Hands"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"))
  
  cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")
  
  pltList = multiPlot(Sample,contrastList = contrastList,contPplot = c(8,1),cbbPalette = cbbPalette,xlims = xlmsES,
            SData = F, diffplot = c(F,F,F,F,F,F,F,F), subj_dataALL)
  #ggsave(file = paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,"efect_size.pdf",sep=""), arrangeGrob(grobs = pltList, nrow=contPplot[1], ncol=contPplot[2])) 
  saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,"efect_size",sep=""), type="pdf")
}





# 
# 
# ###########################################################
# # Simple Effects
# # arrenging data and setting-up contrasts and colors
# 
# xidx <-1
# Sample     = array( 0 , dim=c( dataList$NCond , chainLength ) )
# for ( bt in 1:2 ) { for ( cl in 1:2 ) { for ( ch in 1:2 ) { 
#   Sample[xidx,] = mcmcChain[, paste("m[",bt,",",cl,",",ch,"]",sep="") ]
#   xidx <- xidx +1
# }}}
# contrastList  = list(ReLuHu = (Cnames=="ReLuHu"),ReLuHc = (Cnames=="ReLuHc"),ReLcHu = (Cnames=="ReLcHu"),ReLcHc = (Cnames=="ReLcHc"),
#                      RaLuHu = (Cnames=="RaLuHu"),RaLuHc = (Cnames=="RaLuHc"),RaLcHu = (Cnames=="RaLcHu"),RaLcHc = (Cnames=="RaLcHc"))
# cbbPalette  <- c("#606060","#006D2C","#1F78B4","#E31A1C","#606060","#006D2C","#1F78B4","#E31A1C")
# 
# subj_dataALL    <- list()
# for ( cIdx in 1:length(contrastList) ){
# subj_dataALL[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,value=mean(get("trial_RT")[cond==names(contrastList[cIdx])], na.rm=T))
# }
# 
# multiPlot(Sample,contrastList = contrastList,contPplot = c(4,2),cbbPalette = cbbPalette,xlims = c(.2,.95,.1),
#           SData = TRUE, diffplot = FALSE, subj_dataALL)
# saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_simpleEffect",sep=""), type="pdf")
# 
# # Differences
# contrastList  = list(ReLuHu_vs_ReLuHc = (Cnames=="ReLuHu")-(Cnames=="ReLuHc"),ReLuHu_vs_ReLcHu = (Cnames=="ReLuHu")-(Cnames=="ReLcHu"),
#                      ReLuHu_vs_ReLcHc = (Cnames=="ReLuHu")-(Cnames=="ReLcHc"),ReLuHc_vs_ReLcHu = (Cnames=="ReLuHc")-(Cnames=="ReLcHu"),
#                      ReLuHc_vs_ReLcHc = (Cnames=="ReLuHc")-(Cnames=="ReLcHc"),ReLcHu_vs_ReLcHc = (Cnames=="ReLcHu")-(Cnames=="ReLcHc"),
#                      RaLuHu_vs_RaLuHc = (Cnames=="RaLuHu")-(Cnames=="RaLuHc"),RaLuHu_vs_RaLcHu = (Cnames=="RaLuHu")-(Cnames=="RaLcHu"),
#                      RaLuHu_vs_RaLcHc = (Cnames=="RaLuHu")-(Cnames=="RaLcHc"),RaLuHc_vs_RaLcHu = (Cnames=="RaLuHc")-(Cnames=="RaLcHu"),
#                      RaLuHc_vs_RaLcHc = (Cnames=="RaLuHc")-(Cnames=="RaLcHc"),RaLcHu_vs_RaLcHc = (Cnames=="RaLcHu")-(Cnames=="RaLcHc"),
#                      ReLuHu_vs_RaLuHu = (Cnames=="ReLuHu")-(Cnames=="RaLuHu"),ReLuHc_vs_RaLuHc = (Cnames=="ReLuHc")-(Cnames=="RaLuHc"),
#                      ReLcHc_vs_RaLcHc = (Cnames=="ReLcHc")-(Cnames=="RaLcHc"),ReLcHu_vs_RaLcHu = (Cnames=="ReLcHu")-(Cnames=="RaLcHc"))
# 
# subj_dataALLdiff    <- list()
# for ( cIdx in 1:length(contrastList) ){
#   difIndxs <- which(contrastList[[cIdx]] !=0)
#   subj_dataALLdiff[[cIdx]]     <- ddply(datFrameOK,.(subjIndx),summarize,
#                                     value=mean(get("trial_RT")[cond==Cnames[difIndxs[1]]], na.rm=T)-mean(get("trial_RT")[cond==Cnames[difIndxs[2]]], na.rm=T))
# }
# multiPlot(Sample,contrastList = contrastList,contPplot = c(6,3),cbbPalette = cbbPalette,xlims = c(-.4,.26,.1),
#           SData = TRUE, diffplot = TRUE, subj_dataALLdiff)
# saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_simpleEffectDiff",sep=""), type="pdf")
# 
# ######################################################################################
# # Main Effects and diffs
# 
# xidx <-1
# Sample     = array( 0 , dim=c( 6 , chainLength ) )
# for ( bs in 1:3 ) { for ( lev in 1:2 ) { 
#   Sample[xidx,] = mcmcChain[[1]][, paste("b",bs,"[",lev,"]",sep="") ]
#   xidx <- xidx +1
# }}
# gnames = c("cLxL","LxL","UC_Legs","C_Legs","UC_Hands","C_Hands")
# contrastList = list(cLxL_vs_LxL = (gnames=="cLxL")-(gnames=="LxL"),
#                     C_Legs_vs_UC_Legs = (gnames=="C_Legs")-(gnames=="UC_Legs"),
#                     C_Hands_vs_UC_Hands = (gnames=="C_Hands")-(gnames=="UC_Hands"))
# 
# #contrastList = list(LxL = (gnames=="LxL"), cLxL = (gnames=="cLxL"),LxL_vs_cLxL = (gnames=="LxL")-(gnames=="cLxL"),
#  #                   UC_Legs = (gnames=="UC_Legs"), C_Legs = (gnames=="C_Legs"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
#   #                  UC_Hands = (gnames=="UC_Hands"), C_Hands = (gnames=="C_Hands"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"))
# 
# cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")
# 
# multiPlot(Sample,contrastList = contrastList,contPplot = c(3,1),cbbPalette = cbbPalette,xlims = c(-.05,.20,.1),
#           SData = F, diffplot = c(T,T,T), subj_dataALL)
# saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/effecs",tsks[t],prf[c+1],model,sep=""), type="pdf")
# 
# #multiPlot(Sample,contrastList = contrastList,contPplot = c(3,3),cbbPalette = cbbPalette,xlims = c(-.15,.10,.025),
#  #         SData = F, diffplot = c(F,F,T,F,F,T,F,F,T), subj_dataALL)
# #saveGraph(file=paste(getwd(),"/figures/LH2cross/bayes/",fileNameRoot,"_MainEffect",sep=""), type="pdf")
# 
# # interaction
# contrastList = list(Ext_vs_Anat = (gnames=="Ext")-(gnames=="Anat"),UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),
#                     Ext_vs_Anat_x_UC_Legs_vs_C_Legs = (gnames=="Ext")-(gnames=="Anat")-(gnames=="UC_Legs")+(gnames=="C_Legs"),
#                     Ext_vs_Anat = (gnames=="Ext")-(gnames=="Anat"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"),
#                     Ext_vs_Anat_x_UC_Hands_vs_C_Hands = (gnames=="Ext")-(gnames=="Anat")-(gnames=="UC_Hands")+(gnames=="C_Hands"),
#                     UC_Legs_vs_C_Legs = (gnames=="UC_Legs")-(gnames=="C_Legs"),UC_Hands_vs_C_Hands = (gnames=="UC_Hands")-(gnames=="C_Hands"),
#                     UC_Legs_vs_C_Legs_x_UC_Hands_vs_C_Hands = (gnames=="UC_Legs")-(gnames=="C_Legs")-(gnames=="UC_Hands")+(gnames=="C_Hands"))
# 
# cbbPalette  <- c("#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060","#606060")
# 
# multiPlot(Sample,contrastList = contrastList,contPplot = c(3,3),cbbPalette = cbbPalette,xlims = c(-.2,.2,.05),
#           SData = F, diffplot = c(F,F,T,F,F,T,F,F,T), subj_dataALL)