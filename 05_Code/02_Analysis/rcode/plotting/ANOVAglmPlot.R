ANOVAglmPlot <- function(mcmcChain,factorsToPlot,cbbpalette){
  library(gtools) 
  cols <- colnames(mcmcChain)
  
  for (pl in 1:length(factorsToPlot)){       # loop though plots
    thisPlot = factorsToPlot[[pl]]
    xlims=c(.35,.65)
    auxFandI = grepl("factorNames",names(thisPlot))+grepl("interaction",names(thisPlot))
    if (sum(auxFandI)!=2){stop("Factor names and interaction not specified")}
    if (sum(!auxFandI)!=length(thisPlot$factorNames)){stop("Number of specified factors and number of factors names do not match")}
    factNames = names(thisPlot)
    
    # main factors TODO
    if (!thisPlot$interaction){            
      for (tF in which(!auxFandI)){
        thisFact = factNames[tF]           # factor as in data string
        whichC = grep(paste("^",thisFact,"\\[",sep=""),cols) # where in the data is the string
        if (!length(thisPlot[[thisFact]])==length(whichC==2)){stop(sprintf("Number of levels in specified factor %s different to number of columns in data",thisFact))}
      # to be defined        
      }
    }
    #intearctions
     if (thisPlot$interaction){ 
       thisFacts = factNames[which(!auxFandI)]
       # alternative
       aux1 = paste(thisFacts,collapse="")
       withinBetas = regmatches(aux1,gregexpr("(.\\d)?",aux1))[[1]]
       perms = list()
       for (cc in 1:length(withinBetas)){
         if (length(withinBetas)-cc+1>1){
            perms[[cc]] = permutations(length(withinBetas),length(withinBetas)-cc+1,withinBetas)
         }
       }
   # search the col and get the respective order
     #  perms = permutations(length(thisFacts),length(thisFacts),thisFacts)
       
       thisRow = 0
       for (pls in 1:dim(perms[[1]])[1]){
         posterm = paste(perms[[1]][pls,],collapse="")
         whichC = grep(paste("^",posterm,"\\[",sep=""),cols)
         if (any(whichC)){
           thisRow     = pls
           thisTerm    = posterm
           thisCols    = cols[whichC]
        #   withinBetas = regmatches(thisTerm,gregexpr("(.\\d)?",thisTerm))[[1]]
           }
       }
       # getdata and plot
       elpos =matrix(c(1,2,1,2,1,1,2,2),nrow=2,ncol=4,byrow=T)
       if (which(perms[[1]][thisRow,] %in% thisFacts[1])==2){
         pos  = c(-1,-1,1,1)*.75
         lpos = 1
                 
       }else{
         pos=c(-1,1,-1,1)*.75
         lpos = 2
      #   elpos =matrix(c(1,1,2,2,1,2,1,2),nrow=2,ncol=4,byrow=T)
         }
       openGraph(width =1.75, height = 1.5)   
       p1 <- ggplot() +  theme_bw()+
             theme(axis.line         = element_line(colour = "black"),
                   axis.line.y         = element_line(color="black"),
                   axis.line.x         = element_line(color="black"),
                   panel.grid.major.y  = element_line(size=rel(0.5)),
                   panel.grid.major.x  = element_blank(),
                   panel.grid.minor.x  = element_blank(),
                   panel.border        = element_blank(),
                   panel.background    = element_blank(),
                   axis.title.x        = element_blank(),
                   legend.position     ="none",
                   #axis.text.x         = element_text(size=14),
                   plot.margin         = unit(c(2, 2, 2, 2), "mm"),
                   text                = element_text(family="Helvetica",size=10))
       quamed = NULL
       conect = NULL
       conline=NULL
       for (pp in 1:length(thisCols)){
         thispos = pos[pp]
         print(thispos)
         rzfact      = .25
        # addTerms    = mcmcChain[,"b0"]
          for (cc in 1:length(perms)){
            thisPerm = perms[[cc]][thisRow,]
            for (ccc in  1:length(thisPerm)){
              
            }
          }
       #  addTerms    = addTerms+
         thisData    = mcmcChain[,"b0"]+mcmcChain[,paste(perms[[1]][thisRow,1],"[",elpos[1,pp],"]",sep="")]+mcmcChain[,paste(perms[[1]][thisRow,2],"[",elpos[2,pp],"]",sep="")]+mcmcChain[,thisCols[pp]]
         postMd      = with(density(thisData, adjust=2 ),data.frame(x,y))
         postMd$y2   = thispos-(postMd$y/max(postMd$y)*rzfact)
         postMd$y    = (postMd$y/max(postMd$y)*rzfact)+thispos
         HDI95post   = HDIofMCMC(thisData,.95)  
         medianpost  = median(thisData)
         quamed      <- rbind(quamed,data.frame(x=c(medianpost,medianpost),y=c(thispos-rzfact,thispos+rzfact),g=paste('median',pp,sep="")))
         conline     <- rbind(conline,data.frame(x=medianpost,y=thispos,g=thisPlot[[2]][elpos[lpos,pp]]))
         p1<- p1 + 
           geom_ribbon(data=postMd,aes(x=x,ymax=y),fill="grey40", ymin=thispos,alpha=.5,size=.1)+ # and a ribbon for the posterior 95% HDI
           geom_ribbon(data=postMd,aes(x=x,ymax=y2),fill="grey40", ymin=thispos,alpha=.5,size=.1)+
           geom_ribbon(data=subset(postMd,x>HDI95post[1] & x<HDI95post[2]),aes(x=x,ymax=y),fill=cbbpalette[elpos[lpos,pp]], ymin=thispos,alpha=.75,size=.1)+ # and a ribbon for the posterior 95% HDI
           geom_ribbon(data=subset(postMd,x>HDI95post[1] & x<HDI95post[2]),aes(x=x,ymax=y2),fill=cbbpalette[elpos[lpos,pp]], ymin=thispos,alpha=.75,size=.1) +
           coord_flip()# and a ribbon for the posterior 95% HDI
           #% geom_line line for the median
       }
        p1$layers <- c(geom_line(data=conline,aes(x=x,y=y,group=g,color=g),size=.25),p1$layers)
       p1<- p1 +geom_line(data=quamed,aes(x=x,y=y,group=g),size=.1) +
         geom_text(data=conline,aes(x = x, y=y+sign(y)*rzfact+.2*sign(y)*rzfact,hjust=(sign(y)-1)/2/sign(y),label = sprintf('%d',round(x*1000))),  # text RT
                  size=1.5)+
         geom_text(data=data.frame(y = c(max(pos),max(pos)), x =c(xlims[1]+diff(xlims)*.1,xlims[1]+diff(xlims)*.2),labels = thisPlot[[2]]),
                   aes(x =x,y = y, label = labels,color=labels),
                   size=2.5)+
         scale_y_continuous("",limits=unique(pos)+c(-diff(unique(pos))/2,diff(unique(pos))/2),breaks=unique(pos),labels=thisPlot[[1]],expand = c(0, 0)) +
         scale_x_continuous("posterior (ms)",limits=xlims,expand = c(0,0),breaks=seq(xlims[1],xlims[2],length.out=3))+
         scale_color_manual(values=cbbpalette)
      print(p1)   
     }
  }
}
