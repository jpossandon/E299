# Data plot

if (exp==1){
  tsks        <- c("Normal")
  ylimH <-1}

if (exp==2 ){
  tsks      <- c("Normal","Anti")
  ylimH      <-1.06}
if(exp==22){
  pathfig  = c("LH2crossboth","LH2crossboth")
  tsks        <- c("both")}
if(exp==3){
  tsks        <- c("LH","HL") }

prf         <- c("Incorrect","Correct")


# RT all conditions, plots for correct and incorrect for both tasks

for (c in c(1)){
  for (t in c(1:length(tsks))){
  
    if (exp==22){
      this_mcmcChain <- mcmcChain[[1]]}else{
        this_mcmcChain <- mcmcChain[[t]]}
    ######################################## 
    ########################################
    # plot RT per condition and response mode
    ########################################
    ########################################
    datFrameaux = subset(datFrame,task==t)
   
    ###################################
    # table figure difference between conditions
    eachcond = this_mcmcChain[,grep( "^m" , colnames( this_mcmcChain ),value = TRUE )] 
    eachSigma = this_mcmcChain[,grep( "^yS" , colnames( this_mcmcChain ),value = TRUE )] 
    
    
    if(exp==22 & model=="condtask"){
      names      = c("rEuLuHtS","rAuLuHtS","rEcLuHtS","rAcLuHtS","rEuLcHtS","rAuLcHtS","rEcLcHtS","rAcLcHtS",
                     "rEuLuHtA","rAuLuHtA","rEcLuHtA","rAcLuHtA","rEuLcHtA","rAuLcHtA","rEcLcHtA","rAcLcHtA")
      namesOrder = c("rEuLuHtS","rEuLcHtS","rEcLuHtS","rEcLcHtS","rAuLuHtS","rAuLcHtS","rAcLuHtS","rAcLcHtS",
                     "rEuLuHtA","rEuLcHtA","rEcLuHtA","rEcLcHtA","rAuLuHtA","rAuLcHtA","rAcLuHtA","rAcLcHtA")
      combdiff = combinations(16,2,v=1:16, repeats.allowed=T)
      }else{
      names      = c("rEuLuH","rAuLuH","rEcLuH","rAcLuH","rEuLcH","rAuLcH","rEcLcH","rAcLcH")
      namesOrder = c("rEuLuH","rEuLcH","rEcLuH","rEcLcH","rAuLuH","rAuLcH","rAcLuH","rAcLcH")
      combdiff = combinations(8,2,v=1:8, repeats.allowed=T)
      }
    df <- data.frame(matrix(ncol = 5, nrow = dim(combdiff)[1]))
    names(df) <- c('cond1','cond2','diff','diffES','cred')
    for (ii in 1:dim(combdiff)[1]){
      this_M        <- round(mean(eachcond[,combdiff[ii,1]]-eachcond[,combdiff[ii,2]])*1000)
      df$cond1[ii]   <- namesOrder[combdiff[ii,1]]
      df$cond2[ii]   <- namesOrder[combdiff[ii,2]]
      if (df$cond1[ii]!=df$cond2[ii]){
        a1             <- names==namesOrder[combdiff[ii,1]]
        a2             <- names==namesOrder[combdiff[ii,2]]
        df$diff[ii]    <- round(mean(eachcond[,which(a1)]-eachcond[,which(a2)])*1000)
        
        if(exp==22 & model=="condtask"){
          this_difmcmcES <- (eachcond[,a1]-eachcond[,a2])/sqrt(((eachSigma[,which(a1)])^2+(eachSigma[,which(a2)])^2)/2)
        }else{
          this_difmcmcES <- (eachcond[,a1]-eachcond[,a2])/sqrt((((eachSigma[,which(a1)]+eachSigma[,which(a1)+8])/2)^2+((eachSigma[,which(a2)]+eachSigma[,which(a2)+8])/2)^2)/2)
        }
        this_hdiES     <- HDIofMCMC(this_difmcmcES)
        
        df$diffES[ii]  <-  round(mean(this_difmcmcES)*10)/10
        df$cred[ii]    <-  this_hdiES[1]>.1 | this_hdiES[2]<.1
      }
     
    }
    # df$diff[df$diff==0] = NA
    #df$diffES[df$diffES==0] = NA
    df$sameC = NA
    df$sameC[(df$cond1=="rEuLuH"&df$cond2=="rAuLuH")|(df$cond2=="rEuLuH"&df$cond1=="rAuLuH")] = "nC"
    df$sameC[(df$cond1=="rEcLcH"&df$cond2=="rAcLcH")|(df$cond2=="rEcLcH"&df$cond1=="rAcLcH")] = "nynyyy"
    df$sameC[(df$cond1=="rEuLuHtS"&df$cond2=="rAuLuHtS")|(df$cond2=="rEuLuHtS"&df$cond1=="rAuLuHtS")] = "nC"
    df$sameC[(df$cond1=="rEcLcHtS"&df$cond2=="rAcLcHtS")|(df$cond2=="rEcLcHtS"&df$cond1=="rAcLcHtS")] = "nynyyy"
    
    df$sameC[(df$cond1=="rEuLuHtA"&df$cond2=="rAuLuHtA")|(df$cond2=="rEuLuHtA"&df$cond1=="rAuLuHtA")] = "yyyynn"
    df$sameC[(df$cond1=="rEcLcHtA"&df$cond2=="rAcLcHtA")|(df$cond2=="rEcLcHtA"&df$cond1=="rAcLcHtA")] = "ynynyy"
    
    df$sameC[(df$cond1=="rEuLcHtS"&df$cond2=="rAuLcHtA")|(df$cond2=="rEuLcHtS"&df$cond1=="rAuLcHtA")] = "ynnyny"
    df$sameC[(df$cond1=="rAuLcHtS"&df$cond2=="rEuLcHtA")|(df$cond2=="rAuLcHtS"&df$cond1=="rEuLcHtA")] = "nyynny"
    
    df$sameC[(df$cond1=="rEcLuHtS"&df$cond2=="rAcLuHtA")|(df$cond2=="rEcLuHtS"&df$cond1=="rAcLuHtA")] = "yynnyn"
    df$sameC[(df$cond1=="rAcLuHtS"&df$cond2=="rEcLuHtA")|(df$cond2=="rAcLuHtS"&df$cond1=="rEcLuHtA")] = "nnyyyn"
    
    if(exp==22 & model=="condtask"){
    openGraph(width = 10, height = 6)}else{
      openGraph(width = 3, height = 3)
    } 
    p1 <- ggplot()+
      theme(#axis.line         = element_line(colour = "black"),
        # axis.line.y         = element_line(color="black"),
        #  axis.line.x         = element_line(color="black"),
        panel.grid.minor.y  = element_line(size = .3),
        panel.grid.major.y  = element_line(size = .3),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.border        = element_blank(),
        panel.background    = element_blank(),
        axis.title.x        = element_blank(),
        axis.title.y        = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        #    axis.text.x         = element_text(color=cbbPalettexlabel),
        plot.margin         = unit(c(2, 2, 2, 2), "mm"),
        text                = element_text(family="Helvetica",size=8))+
      geom_tile(data=df,aes(x=cond2,y=cond1,fill=diff),width=0.95, height=0.95)+                    #upper part tri
      geom_tile(data=df,aes(x=cond1,y=cond2,fill=diffES*100*4/6),width=0.95, height=0.95)+          #lower part tri
      geom_tile(data=df,aes(x=cond1,y=cond2,color=sameC),fill=NA,size=.5,width=0.95, height=0.95)+
      geom_tile(data=df,aes(x=cond2,y=cond1,color=sameC),fill=NA,size=.5,width=0.95, height=0.95)+
      geom_text(data=df,aes(x=cond2,y=cond1,label=diff,color=cred,fontface="plain"),size=2.75)+
      geom_text(data=df,aes(x=cond1,y=cond2,label=diffES,color=cred,fontface="plain"),size=2.75)+
      scale_fill_gradientn(colours=brewer.pal(11,"RdBu"),limit = c(-400,400), space = "Lab", 
                           name="difference", guide = "none",na.value = "white")+
      
      scale_x_discrete(position = "top",limits=namesOrder,expand = c(0, 0))+
      scale_y_discrete(expand = c(0, 0),limits=rev(namesOrder))+coord_equal()+
      scale_color_manual(values = c('TRUE' = 'black', 'FALSE' = 'gray','nC'="black",'allC'="black",
                                    "nynyyy"="black","ynnyny"="black","nyynny"="black","yynnyn"="black",
                                    "nnyyyn"="black","yyyynn"="black","ynynyy"="black"), guide = "none")
    print(p1)
    saveGraph(file=paste(path,"/07_Analyses/",pathfig[t],"/allSubjdiffs",tsks[t],prf[c+1],model,sep=""), type="pdf")
    
  }
}

