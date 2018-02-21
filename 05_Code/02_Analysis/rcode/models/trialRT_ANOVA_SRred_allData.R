modelstring = "
model {
for ( i in 1:Ndata ) {
y[i] ~ dnorm( mu[i] ,1/(ySigma^2))
mu[i] <- a0 + a1[LegC[i]] + a2[HandC[i]] +  a4[cLxB[i]] + a5[cSxB[i]]  +  aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1aS[LegC[i],S[i]] + a2aS[HandC[i],S[i]] + a4aS[cLxB[i],S[i]] + a5aS[cSxB[i],S[i]] 
#mu[i] <- a0 + a1[LegC[i]] + a2[HandC[i]] + a3[cLxL[i]] + a4[cLxB[i]] + a5[cSxB[i]] + a6[cSxL[i]] +  aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
#a1aS[LegC[i],S[i]] + a2aS[HandC[i],S[i]] + a3aS[cLxL[i],S[i]] + a4aS[cLxB[i],S[i]] +a5aS[cSxB[i],S[i]] +a6aS[cSxL[i],S[i]]

}

ySigma ~ dgamma(sigmaSh, sigmaRa)
# # VARIABLE SIGMA PER CONDITION
# for (j1 in 1:2){
#   for (j2 in 1:2){
#     for (j3 in 1:2){
#       ySigma[j1,j2,j3] ~ dgamma(sigmaSh, sigmaRa)
#       #ySigma[j1,j2] <- max( sigma[j1,j2] , medianCellSD/1000 )
#     }
#   }
# }

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
#for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
#a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j4 in 1:2 ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }
a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j5 in 1:2 ) { a5[j5] ~ dnorm( 0.0 , 1/a5SD^2 ) }
a5SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
#for ( j6 in 1:2 ) { a6[j6] ~ dnorm( 0.0 , 1/a6SD^2 ) }
#a6SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

# SUBJECT CONTRIBUTIONS
for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
SDa0S ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0

# 2-WAY INTERACTIONS
for (j1 in 1:2){
for (js in 1:NSubj){
a1aS[j1,js] ~ dnorm( 0.0 , 1/a1aSSD^2 )
} }
a1aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
for (js in 1:NSubj){
a2aS[j2,js] ~ dnorm( 0.0 , 1/a2aSSD^2 )
} }
a2aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# for (j3 in 1:2){
# for (js in 1:NSubj){
# a3aS[j3,js] ~ dnorm( 0.0 , 1/a3aSSD^2 )
# # }
#}
# a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j4 in 1:2){
for (js in 1:NSubj){
a4aS[j4,js] ~ dnorm( 0.0 , 1/a4aSSD^2 )
} }
a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

 for (j5 in 1:2){
 for (js in 1:NSubj){
 a5aS[j5,js] ~ dnorm( 0.0 , 1/a5aSSD^2 )
 } }
 a5aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

#for (j6 in 1:2){
#for (js in 1:NSubj){
#a6aS[j6,js] ~ dnorm( 0.0 , 1/a6aSSD^2 )
#}
#}
a6aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# Convert a0,a1[],a2[],a3[],a4[],a5[],a46[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:2) { for ( j2 in 1:2 ) {  for ( j4 in 1:2 ) { for ( j5 in 1:2 ) { for (js in 1:NSubj){
mm[j1,j2,j4,j5,js] <- a0 + a1[j1] + a2[j2] + a4[j4]+ a5[j5]+  aS[js] + # cell means 
a1aS[j1,js] + a2aS[j2,js] + a4aS[j4,js] + a5aS[j5,js]   
} } } } }

b0 <- mean( mm[1:2,1:2,1:2,1:2,1:NSubj] )
for ( j1 in 1:2 ) { b1[j1] <- mean( mm[j1,1:2,1:2,1:2,1:NSubj] ) - b0 }
for ( j2 in 1:2 ) { b2[j2] <- mean( mm[1:2,j2,1:2,1:2,1:NSubj] ) - b0 }
#for ( j3 in 1:2 ) { b3[j3] <- mean( mm[1:2,1:2,j3,1:2,1:2,1:2,1:NSubj] ) - b0 }
for ( j4 in 1:2 ) { b4[j4] <- mean( mm[1:2,1:2,j4,1:2,1:NSubj] ) - b0 }
for ( j5 in 1:2 ) { b5[j5] <- mean( mm[1:2,1:2,1:2,j5,1:NSubj] ) - b0 }
#for ( j6 in 1:2 ) { b6[j6] <- mean( mm[1:2,1:2,1:2,1:2,1:2,j6,1:NSubj] ) - b0 }
for ( js in 1:NSubj ) { bS[js] <- mean( mm[1:2,1:2,1:2,1:2,js] ) - b0 }

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j4 in 1:2 ) { for ( j5 in 1:2 ) { 
m[j1,j2,j4,j5]<- mean(mm[j1,j2,j4,j5,1:NSubj])
}}}}

}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")