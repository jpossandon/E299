codaSamples = as.mcmc.list(runjagsModel)
if ( checkConvergence ) {
  for ( parName in colnames(codaSamples[[1]]) ) {
    diagMCMC( codaObject=codaSamples, parName=parName)
    saveGraph(file=paste(path,checkPartialPath,fileNameRoot,"_",parName, sep=""), type="pdf")
    graphics.off()
  }
}