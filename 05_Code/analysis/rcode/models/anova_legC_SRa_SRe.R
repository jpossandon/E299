modelstring = "
model {
for ( i in 1:Ndata ) {
y[i] ~ dt( mu[i] ,1/(ySigma[LegC[i],SRa[i],SRe[i]])^2, nu)
mu[i] <- a0 + a1[LegC[i]] + a2[SRa[i]] + a3[SRe[i]] +  aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[LegC[i],SRa[i]] + a1a3[LegC[i],SRe[i]] + a2a3[SRa[i],SRe[i]] + 
a1a2a3[LegC[i],SRa[i],SRe[i]] +
a1aS[LegC[i],S[i]] + a2aS[SRa[i],S[i]] + a3aS[SRe[i],S[i]] +  
a1a2aS[LegC[i],SRa[i],S[i]] + a1a3aS[LegC[i],SRe[i],S[i]] + a2a3aS[SRa[i],SRe[i],S[i]] + 
a1a2a3aS[LegC[i],SRa[i],SRe[i],S[i]]
}
#
nu <- nuMinusOne+1
nuMinusOne ~ dexp(1/29)

# VARIABLE SIGMA PER CONDITION
for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
      ySigma[j1,j2,j3] ~ dgamma(sigmaSh, sigmaRa)
      #ySigma[j1,j2] <- max( sigma[j1,j2] , medianCellSD/1000 )
    }
  }
}

sigmaSh <- 1 + sigmaMode * sigmaRa
sigmaRa <- ( sigmaMode + sqrt( sigmaMode^2 + 4*sigmaSD^2 ) ) /(2*sigmaSD^2)
sigmaMode ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 
sigmaSD ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 

# BASELINE
a0 ~ dnorm( yMean , 1/(ySD*5)^2 ) 
#

# MAIN EFFECTS
for ( j1 in 1:2 ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
a1SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j2 in 1:2 ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
a2SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

# SUBJECT CONTRIBUTIONS
for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
SDa0S ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0

# 2-WAY INTERACTIONS
for (j1 in 1:2){
  for (j2 in 1:2){
    a1a2[j1,j2] ~ dnorm( 0.0 , 1/a1a2SD^2 )
} }
a1a2SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j1 in 1:2){
  for (j2 in 1:2){
a1a3[j1,j2] ~ dnorm( 0.0 , 1/a1a3SD^2 )
} }
a1a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j1 in 1:2){
  for (j2 in 1:2){
a2a3[j1,j2] ~ dnorm( 0.0 , 1/a2a3SD^2 )
} }
a2a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)


for (j1 in 1:2){
  for (jS in 1:NSubj){
a1aS[j1,jS] ~ dnorm( 0.0 , 1/a1aSSD^2 )
} }
a1aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j1 in 1:2){
  for (jS in 1:NSubj){
a2aS[j1,jS] ~ dnorm( 0.0 , 1/a2aSSD^2 )
} }
a2aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j1 in 1:2){
  for (jS in 1:NSubj){
a3aS[j1,jS] ~ dnorm( 0.0 , 1/a3aSSD^2 )
} }
a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)


# 3-WAY INTERACTIONS
for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
    a1a2a3[j1,j2,j3] ~ dnorm( 0.0 , 1/a1a2a3SD^2 )
    }
  }
}
a1a2a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j2 in 1:2){
    for (jS in 1:NSubj){
      a1a2aS[j1,j2,jS] ~ dnorm( 0.0 , 1/a1a2aSSD^2 )
    }
  }
}
a1a2aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j3 in 1:2){
    for (jS in 1:NSubj){
      a1a3aS[j1,j3,jS] ~ dnorm( 0.0 , 1/a1a3aSSD^2 )
    }
  }
}
a1a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)


for (j2 in 1:2){
  for (j3 in 1:2){
    for (jS in 1:NSubj){
      a2a3aS[j2,j3,jS] ~ dnorm( 0.0 , 1/a2a3aSSD^2 )
    }
  }
}
a2a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# 4-WAY INTERACTIONS

for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
      for (jS in 1:NSubj){
        a1a2a3aS[j1,j2,j3,jS] ~ dnorm( 0.0 , 1/a1a2a3aSSD^2 )
      }
    }
  }
}
a1a2a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")