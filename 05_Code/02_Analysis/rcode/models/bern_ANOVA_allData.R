modelstring = "
model {
for ( i in 1:Ndata ) {
y[i] ~ dbern(p[i])
logit( p[i] ) <- Xbeta[i] 
Xbeta[i] <- a0 + a1[RespM[i]] + a2[LegC[i]] + a3[HandC[i]]  + a4[Int[i]] + aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[RespM[i],LegC[i]] + a1a4[RespM[i],Int[i]] + a1a3[RespM[i],HandC[i]] + a2a3[LegC[i],HandC[i]] + a2a4[LegC[i],Int[i]] + a3a4[HandC[i],Int[i]] +
a1a2a3[RespM[i],LegC[i],HandC[i]] +a1a2a4[RespM[i],LegC[i],Int[i]] +a2a3a4[LegC[i],HandC[i],Int[i]] + a1a3a4[RespM[i],HandC[i],Int[i]] +
a1a2a3a4[RespM[i],LegC[i],HandC[i],Int[i]] 

}
#

# Ba4ELINE
a0 ~ dnorm( 0, 1/10^2) 
#

# MAIN EFFECTS
for ( j1 in 1:2 ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
for ( j2 in 1:2 ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
for ( j3 in 1:2 ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
for ( j4 in 1:2 ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }

a1SD <- abs( SDa1Sunabs ) + .1
a2SD <- abs( SDa2Sunabs ) + .1
a3SD <- abs( SDa3Sunabs ) + .1
a4SD <- abs( SDa4Sunabs ) + .1

SDa1Sunabs ~ dt( 0, 1/10^2, 2 )
SDa2Sunabs ~ dt( 0, 1/10^2, 2 )
SDa3Sunabs ~ dt( 0, 1/10^2, 2 )
SDa4Sunabs ~ dt( 0, 1/10^2, 2 )

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
  for (j4 in 1:2){
a1a4[j1,j4] ~ dnorm( 0.0 , 1/a1a4SD^2 )
} }
a1a4SD <- abs( SDa1a4Sunabs ) + .1
SDa1a4Sunabs ~ dt( 0, 1/10^2, 2 )

for (j2 in 1:2){
  for (j4 in 1:2){
a2a4[j2,j4] ~ dnorm( 0.0 , 1/a2a4SD^2 )
} }
a2a4SD <- abs( SDa2a4Sunabs ) + .1
SDa2a4Sunabs ~ dt( 0, 1/10^2, 2 )

for (j3 in 1:2){
  for (j4 in 1:2){
a3a4[j3,j4] ~ dnorm( 0.0 , 1/a3a4SD^2 )
} }
a3a4SD <- abs( SDa3a4Sunabs ) + .1
SDa3a4Sunabs ~ dt( 0, 1/10^2, 2 )

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
for (j4 in 1:2){
a1a2a4[j1,j2,j4] ~ dnorm( 0.0 , 1/a1a2a4SD^2 )
}
}
}
a1a2a4SD <- abs( SDa1a2a4Sunabs ) + .1
SDa1a2a4Sunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j3 in 1:2){
for (j4 in 1:2){
a1a3a4[j1,j3,j4] ~ dnorm( 0.0 , 1/a1a3a4SD^2 )
}
}
}
a1a3a4SD <- abs( SDa1a3a4Sunabs ) + .1
SDa1a3a4Sunabs ~ dt( 0, 1/10^2, 2 )

for (j2 in 1:2){
  for (j3 in 1:2){
for (j4 in 1:2){
a2a3a4[j2,j3,j4] ~ dnorm( 0.0 , 1/a2a3a4SD^2 )
}
}
}
a2a3a4SD <- abs( SDa2a3a4Sunabs ) + .1
SDa2a3a4Sunabs ~ dt( 0, 1/10^2, 2 )

for (j1 in 1:2){
  for (j2 in 1:2){
 for (j3 in 1:2){
for (j4 in 1:2){
a1a2a3a4[j1,j2,j3,j4] ~ dnorm( 0.0 , 1/a1a2a4SD^2 )
}
}
  }
}
a1a2a3a4SD <- abs( SDa1a2a3a4Sunabs ) + .1
SDa1a2a3a4Sunabs ~ dt( 0, 1/10^2, 2 )

# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for (j4 in 1:2){ for (jS in 1:NSubj){
mm[j1,j2,j3,j4,jS] <- a0 + a1[j1] + a2[j2] + a3[j3] + a4[j4] + aS[jS] +
a1a2[j1,j2] + a1a3[j1,j3] + a2a3[j2,j3] + a1a2a3[j1,j2,j3] +
a1a4[j1,j4] + a2a4[j2,j4] + a3a4[j3,j4] +      
a1a2a4[j1,j2,j4] + a1a3a4[j1,j3,j4] + a2a3a4[j2,j3,j4] + 
a1a2a3a4[j1,j2,j3,j4] 
# cell means 
} } } } }

b0 <- mean( mm[1:2,1:2,1:2,1:2,1:NSubj] )
for ( j1 in 1:2 ) { b1[j1] <- mean( mm[j1,1:2,1:2,1:2,1:NSubj] ) - b0 }
for ( j2 in 1:2 ) { b2[j2] <- mean( mm[1:2,j2,1:2,1:2,1:NSubj] ) - b0 }
for ( j3 in 1:2 ) { b3[j3] <- mean( mm[1:2,1:2,j3,1:2,1:NSubj] ) - b0 }
for ( j4 in 1:2 ) { b4[j4] <- mean( mm[1:2,1:2,1:2,j4,1:NSubj] ) - b0 }
for ( jS in 1:NSubj ) { bS[jS] <- mean( mm[1:2,1:2,1:2,1:2,jS] ) - b0 }

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { 
m[j1,j2,j3]<- mean(mm[j1,j2,j3,1:2,1:NSubj])
}}}

for ( j1 in 1:2 ) { for ( j2 in 1:2 ) {
b1b2[j1,j2] <- mean(mm[j1,j2,1:2,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] )
}}

for ( j1 in 1:2 ) { for ( j3 in 1:2) {
b1b3[j1,j3] <- mean(mm[j1,1:2,j3,1:2,1:NSubj]) - ( b0 + b1[j1] + b3[j3] )
}}

for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b2b3[j2,j3] <- mean(mm[1:2,j2,j3,1:2,1:NSubj]) - ( b0 + b2[j2] + b3[j3] )
}}

for ( j1 in 1:2 ) { for ( j4 in 1:2 ) {
b1b4[j1,j4] <- mean(mm[j1,1:2,1:2,j4,1:NSubj]) - ( b0 + b1[j1] + b4[j4] )
}}

for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b2b4[j2,j4] <- mean(mm[1:2,j2,1:2,j4,1:NSubj]) - ( b0 + b2[j2] + b4[j4] )
}}

for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b3b4[j3,j4] <- mean(mm[1:2,1:2,j3,j4,1:NSubj]) - ( b0 + b3[j3] + b4[j4] )
}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b1b2b3[j1,j2,j3] <-  mean(mm[j1,j2,j3,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b3[j3] + b1b2[j1,j2] + b1b3[j1,j3] + b2b3[j2,j3] )
}}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b1b2b4[j1,j2,j4] <-  mean(mm[j1,j2,1:2,j4,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b4[j4] + b1b2[j1,j2] + b1b4[j1,j4] + b2b4[j2,j4] )
}}}

for ( j2 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b2b3b4[j2,j3,j4] <-  mean(mm[1:2,j2,j3,j4,1:NSubj]) - ( b0 + b2[j2] + b3[j3] + b4[j4] + b2b3[j2,j3] + b2b4[j2,j4] + b3b4[j3,j4] )
}}}

for ( j1 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b1b3b4[j1,j3,j4] <-  mean(mm[j1,1:2,j3,j4,1:NSubj]) - ( b0 + b1[j1] + b3[j3] + b4[j4] + b1b3[j1,j3] + b1b4[j1,j4] + b3b4[j3,j4] )
}}}

for ( j1 in 1:2) {  for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b1b2b3b4[j1,j2,j3,j4] <-  mean(mm[j1,j2,j3,j4,1:NSubj]) - ( b0 + b1[j1] + b2[j2] + b3[j3] + b4[j4] + b1b2[j1,j2] + b1b3[j1,j3] + b1b4[j1,j4] + b2b3[j2,j3] + b2b4[j2,j4]  + b3b4[j3,j4] + b1b2b3[j1,j2,j3] + b1b2b4[j1,j2,j4] + b2b3b4[j2,j3,j4] + b1b3b4[j1,j3,j4])
}}}}
}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")