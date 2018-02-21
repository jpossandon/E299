modelstring = "
model {
for ( i in 1:Ndata ) {
y[i] ~ dbern(p[i])
logit( p[i] ) <- Xbeta[i] 
Xbeta[i] <- a0 + a1[task[i]] + a2[LegC[i]] + a3[HandC[i]]  +  aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[task[i],LegC[i]] + a1a3[task[i],HandC[i]] + a2a3[LegC[i],HandC[i]] + 
a1a2a3[task[i],LegC[i],HandC[i]] +
a1aS[task[i],S[i]] + a2aS[LegC[i],S[i]] + a3aS[HandC[i],S[i]] +         # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2aS[task[i],LegC[i],S[i]] + a1a3aS[task[i],HandC[i],S[i]] + a2a3aS[LegC[i],HandC[i],S[i]] + 
a1a2a3aS[task[i],LegC[i],HandC[i],S[i]] 
}
#

# BASELINE
a0 ~ dnorm( 0, 1/10^2) 
#

# MAIN EFFECTS
for ( j1 in 1:2 ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
for ( j2 in 1:2 ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }

a1SD <- abs( SDa1Sunabs ) + .1
a2SD <- abs( SDa2Sunabs ) + .1
a3SD <- abs( SDa3Sunabs ) + .1

SDa1Sunabs ~ dt( 0, 1/10^2, 2 )
SDa2Sunabs ~ dt( 0, 1/10^2, 2 )
SDa3Sunabs ~ dt( 0, 1/10^2, 2 )

# SUBJECT CONTRIBUTIONS
for ( jS in 1:NSubj ) { aS[jS] ~ dnorm( 0 , 1/SDa0S^2 ) }
SDa0S <- abs( SDa0Sunabs ) + .1
SDa0Sunabs ~ dt( 0, 1/10^2, 2 )

# 2-WAY INTERACTIONS
for (j1 in 1:2){
  for (j2 in 1:2){
    a1a2[j1,j2] ~ dnorm( 0.0 , 1/a1a2SD^2 )
} }
a1a2SD <-  abs( SDa1a2Sunabs ) + .1
SDa1a2Sunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j3 in 1:2){
a1a3[j1,j3] ~ dnorm( 0.0 , 1/a1a3SD^2 )
} }
a1a3SD <- abs( SDa1a3Sunabs ) + .1
SDa1a3Sunabs ~ dt( 0, 1/10^2, 2 )

for (j2 in 1:2){
  for (j3 in 1:2){
a2a3[j2,j3] ~ dnorm( 0.0 , 1/a2a3SD^2 )
} }
a2a3SD <- abs( SDa2a3Sunabs ) + .1
SDa2a3Sunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (js in 1:NSubj){
a1aS[j1,js] ~ dnorm( 0.0 , 1/a1aSSD^2 )
} }
a1aSSD <- abs( SDa1aSSunabs ) + .1
SDa1aSSunabs ~ dt( 0, 1/10^2, 2 )

for (j2 in 1:2){
  for (js in 1:NSubj){
a2aS[j2,js] ~ dnorm( 0.0 , 1/a2aSSD^2 )
} }
a2aSSD <- abs( SDa2aSSunabs ) + .1
SDa2aSSunabs ~ dt( 0, 1/10^2, 2 )

for (j3 in 1:2){
  for (js in 1:NSubj){
a3aS[j3,js] ~ dnorm( 0.0 , 1/a3aSSD^2 )
} }
a3aSSD <- abs( SDa3aSSunabs ) + .1
SDa3aSSunabs ~ dt( 0, 1/10^2, 2 )

# 3-WAY INTERACTIONS
for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
    a1a2a3[j1,j2,j3] ~ dnorm( 0.0 , 1/a1a2a3SD^2 )
    }
  }
}
a1a2a3SD <- abs( SDa1a2a3Sunabs ) + .1
SDa1a2a3Sunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j2 in 1:2){
for (js in 1:NSubj){
a1a2aS[j1,j2,js] ~ dnorm( 0.0 , 1/a1a2aSSD^2 )
}
}
}
a1a2aSSD <- abs( SDa1a2aSSunabs ) + .1
SDa1a2aSSunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j3 in 1:2){
for (js in 1:NSubj){
a1a3aS[j1,j3,js] ~ dnorm( 0.0 , 1/a1a3aSSD^2 )
}
}
}
a1a3aSSD <- abs( SDa1a3aSSunabs ) + .1
SDa1a3aSSunabs ~ dt( 0, 1/10^2, 2 )

for (j2 in 1:2){
  for (j3 in 1:2){
for (js in 1:NSubj){
a2a3aS[j2,j3,js] ~ dnorm( 0.0 , 1/a2a3aSSD^2 )
}
}
}
a2a3aSSD <- abs( SDa2a3aSSunabs ) + .1
SDa2a3aSSunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j2 in 1:2){
 for (j3 in 1:2){
for (js in 1:NSubj){
a1a2a3aS[j1,j2,j3,js] ~ dnorm( 0.0 , 1/a1a2aSSD^2 )
}
}
  }
}
a1a2a3aSSD <- abs( SDa1a2a3aSSunabs ) + .1
SDa1a2a3aSSunabs ~ dt( 0, 1/10^2, 2 )

# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for (js in 1:NSubj){
mm[j1,j2,j3,js] <- a0 + a1[j1] + a2[j2] + a3[j3] + aS[js] +
a1a2[j1,j2] + a1a3[j1,j3] + a2a3[j2,j3] + a1a2a3[j1,j2,j3] +
a1aS[j1,js] + a2aS[j2,js] + a3aS[j3,js] +      
a1a2aS[j1,j2,js] + a1a3aS[j1,j3,js] + a2a3aS[j2,j3,js] + 
a1a2a3aS[j1,j2,j3,js] 
# cell means 
} } } }

b0 <- mean( mm[1:2,1:2,1:2,1:NSubj] )
for ( j1 in 1:2 ) { b1[j1] <- mean( mm[j1,1:2,1:2,1:NSubj] ) - b0 }
for ( j2 in 1:2 ) { b2[j2] <- mean( mm[1:2,j2,1:2,1:NSubj] ) - b0 }
for ( j3 in 1:2 ) { b3[j3] <- mean( mm[1:2,1:2,j3,1:NSubj] ) - b0 }
for ( js in 1:NSubj ) { bS[js] <- mean( mm[1:2,1:2,1:2,js] ) - b0 }

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { 
m[j1,j2,j3]<- mean(mm[j1,j2,j3,1:NSubj])
}}}

for ( j1 in 1:2 ) { for ( j2 in 1:2 ) {
b1b2[j1,j2] <- mean(mm[j1,j2,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] )
}}

for ( j1 in 1:2 ) { for ( j3 in 1:2) {
b1b3[j1,j3] <- mean(mm[j1,1:2,j3,1:NSubj]) - ( b0 + b1[j1] + b3[j3] )
}}

for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b2b3[j2,j3] <- mean(mm[1:2,j2,j3,1:NSubj]) - ( b0 + b2[j2] + b3[j3] )
}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b1b2b3[j1,j2,j3] <- m[j1,j2,j3] - ( b0 + b1[j1] + b2[j2] + b1[j3] + b1b2[j1,j2] + b1b3[j1,j3] + b2b3[j2,j3] )
}}}
}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")