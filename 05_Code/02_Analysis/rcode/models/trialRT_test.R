modelstring = "
model {
  for ( i in 1:Ndata ) {
    y[i] ~ dnorm( mu[i] ,1/(ySigma^2))
   # mu[i] <- a0 + aS[S[i]] + equals(m[confSet[i]],1)*path1[i] + equals(m[confSet[i]],2)*path2[i] + equals(m[confSet[i]],3)*path3[i] + equals(m[confSet[i]],4)*path4[i] 
   # mu[i] <- a0 + aS[S[i]] + equals(m,1)*path1[i] + equals(m,2)*path2[i] + equals(m,3)*path3[i] + equals(m,4)*path4[i]     
    mu[i] <- a0 + aS[S[i]] + equals(confSet[i],4)*path1[i] + equals(confSet[i],6)*path1[i] + equals(confSet[i],7)*path1[i] + equals(confSet[i],1)*path2[i] + equals(confSet[i],2)*path2[i] + equals(confSet[i],3)*path2[i] + equals(confSet[i],5)*path2[i] + equals(confSet[i],8)*path2[i]   
    path1[i] <- a3[cLxL[i]] 
    path2[i] <- a1[LegC[i]] + a5[cSxB[i]] + a2[HandC[i]]  
    path3[i] <- a1[LegC[i]] + a6[cSxL[i]]
    path4[i] <- a4[cLxB[i]] + a2[HandC[i]] 
  }
  
  ySigma ~ dgamma(1.01005,0.1005)
  a0 ~ dnorm( yMean , 1/(ySD*5)^2 ) 
  
  # SUBJECT CONTRIBUTIONS
  for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
  SDa0S ~ dgamma(1.01005,0.1005) # mode=0.1,sd=10.0
  
  # MAIN EFFECTS
  for ( j1 in 1:2 ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
  a1SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  for ( j2 in 1:2 ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
  a2SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
  a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  for ( j4 in 1:2 ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }
  a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  for ( j5 in 1:2 ) { a5[j5] ~ dnorm( 0.0 , 1/a5SD^2 ) }
  a5SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  for ( j6 in 1:2 ) { a6[j6] ~ dnorm( 0.0 , 1/a6SD^2 ) }
  a6SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 
  
 # for ( m1 in 1:8 ) { 
#    m ~  dcat( mPriorProb[] )}
 #   mPriorProb[1] <- 1/4
  #  mPriorProb[2] <- 1/4
   # mPriorProb[3] <- 1/4
  #  mPriorProb[4] <- 1/4
 
}"