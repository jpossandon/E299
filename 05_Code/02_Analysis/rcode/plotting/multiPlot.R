multiPlot <- function(Sample,contrastList,cbbPalette,
                      SData, diffplot,subj_dataALL,xlims=NULL,contPplot=NULL){
  nContrasts   = length(contrastList)
  if(is.null(contPplot)){
    contPplot <- c(nContrasts,1)
  }

  conmat       <- matrix(nrow=nContrasts, ncol=chainLength)
  for ( cIdx in 1:nContrasts ){
    contrast       <- matrix(contrastList[[cIdx]], nrow=1) # make it a row matrix
    conmat[cIdx,]  <- contrast %*% Sample
  }
  openGraph(width = 3.5/2, height = 3)
  #openGraph(width=3.2*contPplot[2] , height=3.2/3*contPplot[1])
  pltList     <- list()
  # from here the individual plots
  for ( cIdx in 1:nContrasts) {
    # this is to define a ylim that is the same on all subplots
    maxContdens <- NULL
    for ( cIdx2 in 1:nContrasts) {
      auxdata     = conmat[cIdx2, ]
      densCurve   = density(auxdata, adjust=2 )
      maxContdens = append(maxContdens,max(densCurve$y))
    }
    maxContdens <- 1.05*max(maxContdens)
    negLimY     <- -.6
    if (SData){
      ylims   <-c(negLimY*maxContdens,maxContdens)}else{
        ylims   <-c(0,maxContdens+maxContdens*.1)}
    print(ylims)
    
    #contrast  = matrix( contrastList[[cIdx]], nrow=1) # make it a row matrix
    #incIdx    = contrast!=0
    ttl       = paste( gsub("vs","-", gsub("_", " ", names(contrastList)[cIdx])))
    auxdata   = conmat[cIdx, ]
    
    HDI       = HDIofMCMC(auxdata,.95)
    HDIytext   = HDIofMCMC(auxdata,.93)
    HDIxtext   = HDIofMCMC(auxdata,.97)
    HDIall    = HDIofMCMC(auxdata,.999)
    densCurve = density(auxdata[auxdata>HDIall[1] & auxdata<HDIall[2]], adjust=2 )
    d         <- with(densCurve,data.frame(x,y))
    hdifill   = cbbPalette[cIdx]
    xgap      = 1.5*(HDIofMCMC(auxdata,.99)[2]-HDIofMCMC(auxdata,.95)[2])
    
    if (cIdx<=length(diffplot)){cIdxdiff <- cIdx}else{cIdxdiff <-length(diffplot)}
    if (diffplot[cIdxdiff]){alp = .5
    if(!(0>HDI[1]&0<HDI[2])){hdifill = "black"}else{hdifill = "#cccccc"}
    }else{alp=1}
    
    hdy <- max(c(d[which(d[,1]< HDIytext [1])[1],2],d[which(d[,1]> HDIytext[2])[1],2]))
    if (is.null(xlims)){
      xlms       <- HDIofMCMC(conmat,.999)
      xlms[1]    = xlms[1]-.15*(xlms[2]-xlms[1])
      xlms[3]    = (xlms[2]-xlms[1])/4
      }else{
        if (is.null(dim(xlims))){
        xlms = xlims}else{
          xlms = xlims[cIdx,]
        }
      }
    p1 = qplot(x,y,data=d,geom="line")+
      theme_bw() +
      theme(axis.line           = element_line(colour = "black"),
            axis.line.y         = element_blank(),#element_line(color="black"),
            axis.line.x         = element_line(color="black"),
            axis.text.y         = element_blank(),
            axis.ticks.y        = element_blank(),
            axis.text.x         = element_blank(),
            axis.title.y        = element_blank(),
            panel.grid.major.y  = element_blank(),
            panel.grid.minor.y  = element_blank(),
            panel.border        = element_blank(),
            panel.background    = element_blank(),
            plot.margin         = unit(c(0, 2, 0, 2), "mm"),
            text                = element_text(family="Helvetica"))+
      geom_ribbon(data=subset(d,x>HDIall[1] & x<HDIall[2]),aes(ymax=y),ymin=0,                       #
                  fill="#cccccc",colour="NA",alpha=1,size=0)+
      geom_ribbon(data=subset(d,x>HDI[1] & x<HDI[2]),aes(ymax=y),ymin=0,          
                  fill=hdifill,colour="NA",alpha=alp)+
      geom_point(data = NULL,x=mean(auxdata),y=max(densCurve$y),shape=21,
                 colour = "#000000",fill='red', size = .75,stroke=.2)+
      geom_text(data = NULL, x = mean(auxdata), y = max(densCurve$y),
                hjust=-.3,vjust=.4, size=2.5,family="Helvetica",
                label = formatC(mean(auxdata),digits = 1, format = "f", drop0trailing = TRUE))+
      geom_text(data = NULL, x = HDIxtext[1], y = hdy,
                hjust=1,vjust=0, size=2.5,family="Helvetica",
                label = formatC(HDI[1],digits = 1, format = "f", drop0trailing = TRUE))+
      geom_text(data = NULL, x = HDIxtext[2], y = hdy,
                hjust=0,vjust=0, size=2.5,family="Helvetica",
                label = formatC(HDI[2],digits = 1, format = "f", drop0trailing = TRUE))+
      # geom_text(data = NULL, x = xlms[1]+.01*(xlms[2]-xlms[1]), y = 0+.7*ylims[2],#+.8*max(ylims),
      #           hjust=0,vjust=1, size=1,family="Helvetica",
      #           label = ttl)+
      scale_y_continuous(limits=ylims,expand = c(0, 0))+
      geom_hline(yintercept = 0)+
      scale_x_continuous(limits=xlms[1:2],expand = c(0, 0),breaks=round(seq(xlms[1],xlms[2],by=xlms[3])*1000)/1000)
    
    if(SData){
      if (cIdx<=length(subj_dataALL)){
        subj_data <- subj_dataALL[[cIdx]]
        qua = data.frame(x=quantile(subj_data$value, probs = c(.25,.75), na.rm = TRUE),y=c(.8*ylims[1],.8*ylims[1]),g='hor_line')
        quamed  <-  data.frame(x=quantile(subj_data$value, probs = c(.5,.5), na.rm = TRUE),y=c(.7*ylims[1],.9*ylims[1]),g='median')
        quamean  <- data.frame(x=mean(subj_data$value, na.rm = TRUE),y=c(.7*ylims[1],.9*ylims[1]),g='mean')
        
        p1 <- p1 + 
          geom_point(data = subj_data,aes(x=value,y=.4*negLimY*maxContdens),shape=21,colour = "#2D2D2D",fill=hdifill, size = 2,stroke=.2,position = position_jitter(height = -.4*ylims[1],width= 0))+
          geom_line(data=qua,aes(x,y),color=hdifill, size=.6)+
          geom_line(data=quamed,aes(x,y),color=hdifill, size=.6)+
          geom_line(data=quamean,aes(x,y),color="black", size=.6)
      }
    }
    if(xlms[1]<0 & xlms[2]>0){
      p1$layers <- c(geom_line(data=data.frame(x=c(0,0),y=.8*ylims),aes(x,y),color="black",linetype = "dotted", size=.4),p1$layers)
    }
    if (cIdx %in% seq(from = contPplot[1], to = prod(contPplot), by = contPplot[1]) ){ #| length(diffplot)>1
      p1 <- p1 +
        theme(#axis.text.x  = element_blank(),     # if we keep the number the proportions of the las plot will be wrong
              axis.line.x = element_blank(),
             # axis.ticks.x = element_blank(),
              axis.title.x = element_blank()) # unfortunately if we add the title in the last line the proportions of the last plot change
    }else{
         p1 <- p1 +
        theme(axis.text.x  = element_blank(),
              axis.line.x  = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank())}

    
    pltList[[cIdx]] <- p1
  }
  #grid.arrange(pltList, nrow=contPplot[1], ncol=contPplot[2], as.table = FALSE)
  do.call(grid.arrange, c(pltList, nrow=contPplot[1], ncol=contPplot[2], as.table = FALSE))
return(pltList)
  #  ggsave(file = "whatever.pdf", arrangeGrob(grobs = pltList, nrow=contPplot[1], ncol=contPplot[2])) 
}