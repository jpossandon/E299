modelstring = "
model {
for ( i in 1:Ndata ) {
#y[i] ~ dt( mu[i] ,1/(ySigma[C[i]])^2, nu)
y[i] ~ dnorm( mu[i] ,1/(ySigma[C[i]])^2)
mu[i] <- a0 + a1[C[i]] + aS[S[i]] + a1aS[C[i],S[i]] 
}
#
#nu <- nuMinusOne+1
#nuMinusOne ~ dexp(1/29)

# VARIABLE SIGMA PER CONDITION
for (j1 in 1:NCond){
      ySigma[j1] ~ dgamma(sigmaSh, sigmaRa)
}

sigmaSh <- 1 + sigmaMode * sigmaRa
sigmaRa <- ( sigmaMode + sqrt( sigmaMode^2 + 4*sigmaSD^2 ) ) /(2*sigmaSD^2)
sigmaMode ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 
sigmaSD ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 

# BASELINE
a0 ~ dnorm( yMean , 1/(ySD*5)^2 ) 
#

# MAIN EFFECTS
for ( j1 in 1:NCond ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
a1SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

# SUBJECT CONTRIBUTIONS
for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
SDa0S ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0

for (j1 in 1:NCond){
  for (js in 1:NSubj){
a1aS[j1,js] ~ dnorm( 0.0 , 1/a1aSSD^2 )
} }
a1aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:NCond) { for (js in 1:NSubj){
mm[j1,js] <- a0 + a1[j1] + aS[js] + a1aS[j1,js] 
} } 

b0 <- mean( mm[1:NCond,1:NSubj] )
for ( j1 in 1:NCond ) { b1[j1] <- mean( mm[j1,1:NSubj] ) - b0 }
for ( js in 1:NSubj ) { bS[js] <- mean( mm[1:NCond,js] ) - b0 }

for ( j1 in 1:NCond) { 
m[j1]<- mean(mm[j1,1:NSubj])
}

}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")