modelstring = "
model {
for ( i in 1:Ndata ) {
y[i] ~ dt( mu[i] ,1/(ySigma[i])^2, nu)
#y[i] ~ dnorm( mu[i] ,1/(ySigma[RespM[i],LegC[i],HandC[i],task[i]])^2)

#mun[i,1] <- a0 + a1[RespM[i]] + a2[LegC[i]] + a3[HandC[i]] + a4[task[i]] + aS[S[i]] +       # this is the full model with all taskeraction, included the taskeraction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
#a1a2[RespM[i],LegC[i]] + a1a4[RespM[i],task[i]] + a1a3[RespM[i],HandC[i]] + a2a3[LegC[i],HandC[i]] + a2a4[LegC[i],task[i]] + a3a4[HandC[i],task[i]] +
#a1a2a3[RespM[i],LegC[i],HandC[i]] +a1a2a4[RespM[i],LegC[i],task[i]] +a2a3a4[LegC[i],HandC[i],task[i]] + a1a3a4[RespM[i],HandC[i],task[i]] +
#a1a2a3a4[RespM[i],LegC[i],HandC[i],task[i]] 

mun[i,1] <- a0 + a1[cLxL[i]] + a2[LegC[i]] + a3[HandC[i]] + a4[task[i]]  + aS[S[i]] +       # this is the full model with all taskeraction, included the taskeraction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[cLxL[i],LegC[i]] + a1a4[cLxL[i],task[i]] + a1a3[cLxL[i],HandC[i]] + a2a3[LegC[i],HandC[i]] + a2a4[LegC[i],task[i]] + a3a4[HandC[i],task[i]] +
a1a2a3[cLxL[i],LegC[i],HandC[i]]  +a1a2a4[cLxL[i],LegC[i],task[i]] +a2a3a4[LegC[i],HandC[i],task[i]]  + a1a3a4[cLxL[i],HandC[i],task[i]] +a1a2a3a4[cLxL[i],LegC[i],HandC[i],task[i]] 

mun[i,2] <- a0 + a1[cLxL[i]] + a2[LegC[i]] + a3[HandC[i]] + aS[S[i]] +       # this is the full model with all taskeraction, included the taskeraction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[cLxL[i],LegC[i]] + a1a3[cLxL[i],HandC[i]] + a2a3[LegC[i],HandC[i]] + 
a1a2a3[cLxL[i],LegC[i],HandC[i]] 

ySigman[i,1] <- ySigmaFF[cLxL[i],LegC[i],HandC[i],task[i]]
ySigman[i,2] <- ySigmaSS[cLxL[i],LegC[i],HandC[i]]


#mu[i] <-equals(mcomp,2)*muS[i] + equals(mcomp,1)*muF[i]
mu[i] <-mun[i,mC]


#ySigma[i] <- equals(mcomp,2)*ySigmaS[i] + equals(mcomp,1)*ySigmaF[i]
ySigma[i] <- ySigman[i,mC]

}
nu <- nuMinusOne+1
nuMinusOne ~ dexp(1/29)

#mC <-2
mC ~ dcat( mPriorProba[] )
mPriorProba[1] <- .5
mPriorProba[2] <- .5


# VARIABLE SIGMA PER CONDITION
for (j1 in 1:2){
for (j2 in 1:2){
for (j3 in 1:2){
for (j4 in 1:2){
ySigmaFF[j1,j2,j3,j4] ~ dgamma(sigmaSh, sigmaRa)
#ySigma[j1,j2] <- max( sigma[j1,j2] , medianCellSD/1000 )
}
}
}
}
for (j1 in 1:2){
for (j2 in 1:2){
for (j3 in 1:2){

ySigmaSS[j1,j2,j3] ~ dgamma(sigmaSh, sigmaRa)
#ySigma[j1,j2] <- max( sigma[j1,j2] , medianCellSD/1000 )
}
}
}

sigmaSh <- 1 + sigmaMode * sigmaRa
sigmaRa <- ( sigmaMode + sqrt( sigmaMode^2 + 4*sigmaSD^2 ) ) /(2*sigmaSD^2)
sigmaMode ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 
sigmaSD ~ dgamma(sGammaShRa[1],sGammaShRa[2]) 
delta1 ~dbern(0.5)
# BASELINE
a0 ~ dnorm( yMean , 1/(ySD*5)^2 )
a0s[1]~ dnorm( yMean , 1/(ySD*5)^2 )
a0s[2]~ dnorm( yMean , 1/(ySD*5)^2 )
#

# MAIN EFFECTS
for ( j1 in 1:2 ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
a1SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j2 in 1:2 ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
a2SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j4 in 1:2 ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }
a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

# MAIN EFFECTS
for ( j1 in 1:2 ) { a1s[j1,1] ~ dnorm( 0.0 , 1/a1SDs^2 )
 a1s[j1,2] ~ dnorm( 0.0 , 1/a1SDs^2 )}
a1SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j2 in 1:2 ) { a2s[j2,1] ~ dnorm( 0.0 , 1/a2SDs^2 )
a2s[j2,2] ~ dnorm( 0.0 , 1/a2SDs^2 )}
a2SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
for ( j3 in 1:2 ) { a3s[j3,1] ~ dnorm( 0.0 , 1/a3SDs^2 ) 
a3s[j3,2] ~ dnorm( 0.0 , 1/a3SDs^2 ) }
a3SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

# SUBJECT CONTRIBUTIONS
for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
SDa0S ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0

for ( jS in 1:NSubj ) { aSs[jS] ~ dnorm( 0 , 1/SDa0Ss^2 ) }
SDa0Ss ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0

# 2-WAY INTERACTIONS
for (j1 in 1:2){
for (j2 in 1:2){
a1a2[j1,j2] ~ dnorm( 0.0 , 1/a1a2SD^2 )
a1a2s[j1,j2,1] ~ dnorm( 0.0 , 1/a1a2SDs^2 )
a1a2s[j1,j2,2] ~ dnorm( 0.0 , 1/a1a2SDs^2 )
} }
a1a2SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
a1a2SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j1 in 1:2){
for (j3 in 1:2){
a1a3[j1,j3] ~ dnorm( 0.0 , 1/a1a3SD^2 )
a1a3s[j1,j3,1] ~ dnorm( 0.0 , 1/a1a3SDs^2 )
a1a3s[j1,j3,2] ~ dnorm( 0.0 , 1/a1a3SDs^2 )
} }
a1a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
a1a3SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
for (j2 in 1:2){
for (j3 in 1:2){
a2a3[j2,j3] ~ dnorm( 0.0 , 1/a2a3SD^2 )
a2a3s[j2,j3,1] ~ dnorm( 0.0 , 1/a2a3SDs^2 )
a2a3s[j2,j3,2] ~ dnorm( 0.0 , 1/a2a3SDs^2 )
} }
a2a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
a2a3SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
for (j4 in 1:2){
a1a4[j1,j4] ~ dnorm( 0.0 , 1/a1a4SD^2 )
} }
a1a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
for (j4 in 1:2){
a2a4[j2,j4] ~ dnorm( 0.0 , 1/a2a4SD^2 )
} }
a2a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j3 in 1:2){
for (j4 in 1:2){
a3a4[j3,j4] ~ dnorm( 0.0 , 1/a3a4SD^2 )
} }
a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# 3-WAY INTERACTIONS
for (j1 in 1:2){
for (j2 in 1:2){
for (j3 in 1:2){
a1a2a3[j1,j2,j3] ~ dnorm( 0.0 , 1/a1a2a3SD^2 )
a1a2a3s[j1,j2,j3,1] ~ dnorm( 0.0 , 1/a1a2a3SDs^2 )
a1a2a3s[j1,j2,j3,2] ~ dnorm( 0.0 , 1/a1a2a3SDs^2 )
}
}
}
a1a2a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)
a1a2a3SDs ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
for (j2 in 1:2){
for (j4 in 1:2){
a1a2a4[j1,j2,j4] ~ dnorm( 0.0 , 1/a1a2a4SD^2 )
}
}
}
a1a2a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
for (j3 in 1:2){
for (j4 in 1:2){
a1a3a4[j1,j3,j4] ~ dnorm( 0.0 , 1/a1a3a4SD^2 )
}
}
}
a1a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
for (j3 in 1:2){
for (j4 in 1:2){
a2a3a4[j2,j3,j4] ~ dnorm( 0.0 , 1/a2a3a4SD^2 )
}
}
}
a2a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
for (j2 in 1:2){
for (j3 in 1:2){
for (j4 in 1:2){
a1a2a3a4[j1,j2,j3,j4] ~ dnorm( 0.0 , 1/a1a2a3a4SD^2 )
}
}
}
}
a1a2a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) { for (js in 1:NSubj){
m[j1,j2,j3,j4,js] <- a0 + a1[j1] + a2[j2] + a3[j3] + a4[j4] + aS[js] +
a1a2[j1,j2] + a1a3[j1,j3] + a2a3[j2,j3] + a1a4[j1,j4] + a2a4[j2,j4] + a3a4[j3,j4] +
a1a2a3[j1,j2,j3] + a1a2a4[j1,j2,j4] +a2a3a4[j2,j3,j4] +a1a3a4[j1,j3,j4] +
a1a2a3a4[j1,j2,j3,j4]
} } } } }

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {  for (js in 1:NSubj){
ms[j1,j2,j3,js] <- a0s[2] + a1s[j1,2] + a2s[j2,2] + a3s[j3,2] + aS[js] +
a1a2s[j1,j2,2] + a1a3s[j1,j3,2] + a2a3s[j2,j3,2] + 
a1a2a3s[j1,j2,j3,2] 
} } } } 

b0 <- mean( m[1:2,1:2,1:2,1:2,1:NSubj] )
b0s <- mean( ms[1:2,1:2,1:2,1:NSubj] )
for ( j1 in 1:2 ) { b1[j1] <- mean( m[j1,1:2,1:2,1:2,1:NSubj] ) - b0 
b1s[j1] <- mean( ms[j1,1:2,1:2,1:NSubj] ) - b0s}
for ( j2 in 1:2 ) { b2[j2] <- mean( m[1:2,j2,1:2,1:2,1:NSubj] ) - b0 
b2s[j2] <- mean( ms[1:2,j2,1:2,1:NSubj] ) - b0s }
for ( j3 in 1:2 ) { b3[j3] <- mean( m[1:2,1:2,j3,1:2,1:NSubj] ) - b0
b3s[j3] <- mean( ms[1:2,1:2,j3,1:NSubj] ) - b0s}
for ( j4 in 1:2 ) { b4[j4] <- mean( m[1:2,1:2,1:2,j4,1:NSubj] ) - b0 }
for ( js in 1:NSubj ) { bS[js] <- mean( m[1:2,1:2,1:2,1:2,js] ) - b0
bSs[js] <- mean( ms[1:2,1:2,1:2,js] ) - b0}

for ( j1 in 1:2 ) { for ( j2 in 1:2 ) {
b1b2[j1,j2] <- mean(m[j1,j2,1:2,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] )
b1b2s[j1,j2] <- mean(ms[j1,j2,1:2,1:NSubj]) - ( b0s + b1s[j1] + b2s[j2] )
}}

for ( j1 in 1:2 ) { for ( j3 in 1:2) {
b1b3[j1,j3] <- mean(m[j1,1:2,j3,1:2,1:NSubj]) - ( b0 + b1[j1] + b3[j3] )
b1b3s[j1,j3] <- mean(ms[j1,1:2,j3,1:NSubj]) - ( b0s + b1s[j1] + b3s[j3] )
}}

for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b2b3[j2,j3] <- mean(m[1:2,j2,j3,1:2,1:NSubj]) - ( b0 + b2[j2] + b3[j3] )
b2b3s[j2,j3] <- mean(ms[1:2,j2,j3,1:NSubj]) - ( b0s + b2s[j2] + b3s[j3] )
}}

for ( j1 in 1:2 ) { for ( j4 in 1:2 ) {
b1b4[j1,j4] <- mean(m[j1,1:2,1:2,j4,1:NSubj]) - ( b0 + b1[j1] + b4[j4] )
}}

for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b2b4[j2,j4] <- mean(m[1:2,j2,1:2,j4,1:NSubj]) - ( b0 + b2[j2] + b4[j4] )
}}

for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b3b4[j3,j4] <- mean(m[1:2,1:2,j3,j4,1:NSubj]) - ( b0 + b3[j3] + b4[j4] )
}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b1b2b3[j1,j2,j3] <-  mean(m[j1,j2,j3,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b3[j3] + b1b2[j1,j2] + b1b3[j1,j3] + b2b3[j2,j3] )
b1b2b3s[j1,j2,j3] <-  mean(ms[j1,j2,j3,1:NSubj]) - ( b0s + b1s[j1] + b2s[j2] + b3s[j3] + b1b2s[j1,j2] + b1b3s[j1,j3] + b2b3s[j2,j3] )
}}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b1b2b4[j1,j2,j4] <-  mean(m[j1,j2,1:2,j4,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b4[j4] + b1b2[j1,j2] + b1b4[j1,j4] + b2b4[j2,j4] )
}}}

for ( j2 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b2b3b4[j2,j3,j4] <-  mean(m[1:2,j2,j3,j4,1:NSubj]) - ( b0 + b2[j2] + b3[j3] + b4[j4] + b2b3[j2,j3] + b2b4[j2,j4] + b3b4[j3,j4] )
}}}

for ( j1 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b1b3b4[j1,j3,j4] <-  mean(m[j1,1:2,j3,j4,1:NSubj]) - ( b0 + b1[j1] + b3[j3] + b4[j4] + b1b3[j1,j3] + b1b4[j1,j4] + b3b4[j3,j4] )
}}}

for ( j1 in 1:2) {  for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b1b2b3b4[j1,j2,j3,j4] <-  mean(m[j1,j2,j3,j4,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b3[j3] + b4[j4] + b1b2[j1,j2] + b1b3[j1,j3] + b1b4[j1,j4] + b2b3[j2,j3] + b2b4[j2,j4]  + b3b4[j3,j4] + b1b2b3[j1,j2,j3] + b1b2b4[j1,j2,j4] + b2b3b4[j2,j3,j4] + b1b3b4[j1,j3,j4])
}}}}

}
"