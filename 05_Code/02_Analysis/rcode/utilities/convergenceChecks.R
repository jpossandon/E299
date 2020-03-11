codaSamples = as.mcmc.list(runjagsModel)
if ( checkConvergence ) {
  for ( parName in colnames(codaSamples[[1]]) ) {
    if (!grepl("^loglik",parName)){
    diagMCMC( codaObject=codaSamples, parName=parName)
    saveGraph(file=paste(path,checkPartialPath,fileNameRoot,"_",tsks[thistask],"_",parName, sep=""), type="pdf")
    graphics.off()
    }
  }
}