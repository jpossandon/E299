modelstring = "
model {
for ( i in 1:Ndata ) {
#y[i] ~ dt( mu[i] ,1/(ySigma[RespM[i],LegC[i],HandC[i]])^2, nu)
y[i] ~ dnorm( mu[i] ,1/(ySigma[RespM[i],LegC[i],HandC[i],task[i]])^2)
mu[i] <- a0 + a1[RespM[i]] + a2[LegC[i]] + a3[HandC[i]]  + a4[task[i]] + aS[S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2[RespM[i],LegC[i]] + a1a3[RespM[i],HandC[i]] + a1a4[RespM[i],task[i]] + a2a3[LegC[i],HandC[i]] + a2a4[LegC[i],task[i]] + a3a4[HandC[i],task[i]] +
a1a2a3[RespM[i],LegC[i],HandC[i]] + a1a2a4[RespM[i],LegC[i],task[i]] + a2a3a4[LegC[i],HandC[i],task[i]] + a1a3a4[RespM[i],HandC[i],task[i]] +
a1a2a3a4[RespM[i],LegC[i],HandC[i],task[i]] + 
a1aS[RespM[i],S[i]] + a2aS[LegC[i],S[i]] + a3aS[HandC[i],S[i]] + a4aS[task[i],S[i]] +        # this is the full model with all interaction, included the interaction with the subject factor, this is only possible for when there is multiple data points per subject per cell 
a1a2aS[RespM[i],LegC[i],S[i]] + a1a3aS[RespM[i],HandC[i],S[i]] + a1a4aS[RespM[i],task[i],S[i]] + a2a3aS[LegC[i],HandC[i],S[i]] + a2a4aS[LegC[i],task[i],S[i]] + a3a4aS[HandC[i],task[i],S[i]] +
a1a2a3aS[RespM[i],LegC[i],HandC[i],S[i]] + a1a2a4aS[RespM[i],LegC[i],task[i],S[i]] + a2a3a4aS[LegC[i],HandC[i],task[i],S[i]] + a1a3a4aS[RespM[i],HandC[i],task[i],S[i]] +
a1a2a3a4aS[RespM[i],LegC[i],HandC[i],task[i],S[i]]
}
#
#nu <- nuMinusOne+1
#nuMinusOne ~ dexp(1/29)

# VARIABLE SIGMA PER CONDITION
for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
      for (j4 in 1:2){
      ySigma[j1,j2,j3,j4] ~ dgamma(sigmaSh, sigmaRa)
      #ySigma[j1,j2] <- max( sigma[j1,j2] , medianCellSD/1000 )
      }
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
for ( j4 in 1:2 ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }
a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) 

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
  for (j3 in 1:2){
a1a3[j1,j3] ~ dnorm( 0.0 , 1/a1a3SD^2 )
} }
a1a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
  for (j3 in 1:2){
a2a3[j2,j3] ~ dnorm( 0.0 , 1/a2a3SD^2 )
} }
a2a3SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

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

for (j3 in 1:2){
  for (js in 1:NSubj){
a3aS[j3,js] ~ dnorm( 0.0 , 1/a3aSSD^2 )
} }
a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j4 in 1:2){
  for (js in 1:NSubj){
a4aS[j4,js] ~ dnorm( 0.0 , 1/a4aSSD^2 )
} }
a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)


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
    for (j4 in 1:2){
      a1a2a4[j1,j2,j4] ~ dnorm( 0.0 , 1/a1a2a4SD^2 )
    }
  }
}
a1a2a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
  for (j3 in 1:2){
    for (j4 in 1:2){
      a2a3a4[j2,j3,j4] ~ dnorm( 0.0 , 1/a2a3a4SD^2 )
    }
  }
}
a2a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j3 in 1:2){
    for (j4 in 1:2){
      a1a3a4[j1,j3,j4] ~ dnorm( 0.0 , 1/a1a3a4SD^2 )
    }
  }
}
a1a3a4SD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j2 in 1:2){
    for (js in 1:NSubj){
      a1a2aS[j1,j2,js] ~ dnorm( 0.0 , 1/a1a2aSSD^2 )
    }
    }
}
a1a2aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j4 in 1:2){
    for (js in 1:NSubj){
a1a4aS[j1,j4,js] ~ dnorm( 0.0 , 1/a1a4aSSD^2 )
    }
  }
}
a1a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j3 in 1:2){
    for (js in 1:NSubj){
      a1a3aS[j1,j3,js] ~ dnorm( 0.0 , 1/a1a3aSSD^2 )
    }
  }
}
a1a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
  for (j3 in 1:2){
    for (js in 1:NSubj){
      a2a3aS[j2,j3,js] ~ dnorm( 0.0 , 1/a2a3aSSD^2 )
    }
  }
}
a2a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
  for (j4 in 1:2){
    for (js in 1:NSubj){
      a2a4aS[j2,j4,js] ~ dnorm( 0.0 , 1/a2a4aSSD^2 )
    }
  }
}
a2a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j3 in 1:2){
  for (j4 in 1:2){
    for (js in 1:NSubj){
      a3a4aS[j3,j4,js] ~ dnorm( 0.0 , 1/a3a4aSSD^2 )
    }
  }
}
a3a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

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

for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
      for (js in 1:NSubj){
        a1a2a3aS[j1,j2,j3,js] ~ dnorm( 0.0 , 1/a1a2a3aSSD^2 )
      }
    }
  }
}
a1a2a3aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j2 in 1:2){
    for (j4 in 1:2){
      for (js in 1:NSubj){
        a1a2a4aS[j1,j2,j4,js] ~ dnorm( 0.0 , 1/a1a2a4aSSD^2 )
      }
    }
  }
}
a1a2a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j2 in 1:2){
  for (j3 in 1:2){
    for (j4 in 1:2){
      for (js in 1:NSubj){
        a2a3a4aS[j2,j3,j4,js] ~ dnorm( 0.0 , 1/a2a3a4aSSD^2 )
      }
    }
  }
}
a2a3a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j3 in 1:2){
    for (j4 in 1:2){
      for (js in 1:NSubj){
        a1a3a4aS[j1,j3,j4,js] ~ dnorm( 0.0 , 1/a1a3a4aSSD^2 )
      }
    }
  }
}
a1a3a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

for (j1 in 1:2){
  for (j2 in 1:2){
    for (j3 in 1:2){
      for (j4 in 1:2){
        for (js in 1:NSubj){
          a1a2a3a4aS[j1,j2,j3,j4,js] ~ dnorm( 0.0 , 1/a1a2a3a4aSSD^2 )
        }
      }
    }
  }
}
a1a2a3a4aSSD ~ dgamma(aGammaShRa[1],aGammaShRa[2]) # or try a folded t (Cauchy)

# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) { for (js in 1:NSubj){
mm[j1,j2,j3,j4,js] <- a0 + a1[j1] + a2[j2] + a3[j3] + a4[j4] + aS[js] +
a1a2[j1,j2] + a1a3[j1,j3] + a2a3[j2,j3] + a1a4[j1,j4] + a2a4[j2,j4] + a3a4[j3,j4] +
a1a2a3[j1,j2,j3] + a1a2a4[j1,j2,j4] + a2a3a4[j2,j3,j4] + a1a3a4[j1,j3,j4] +
a1a2a3a4[j1,j2,j3,j4]+
a1aS[j1,js] + a2aS[j2,js] + a3aS[j3,js] + a4aS[j4,js] +     
a1a2aS[j1,j2,js] + a1a3aS[j1,j3,js] + a2a3aS[j2,j3,js] + a1a4aS[j1,j4,js] + a2a4aS[j2,j4,js] + a3a4aS[j3,j4,js] + 
a1a2a3aS[j1,j2,j3,js] + a1a2a4aS[j1,j2,j4,js] + a2a3a4aS[j2,j3,j4,js] + a1a3a4aS[j1,j3,j4,js] +
a1a2a3a4aS[j1,j2,j3,j4,js]
# cell means 
} } } } }

b0 <- mean( mm[1:2,1:2,1:2,1:2,1:NSubj] )
for ( j1 in 1:2 ) { b1[j1] <- mean( mm[j1,1:2,1:2,1:2,1:NSubj] ) - b0 }
for ( j2 in 1:2 ) { b2[j2] <- mean( mm[1:2,j2,1:2,1:2,1:NSubj] ) - b0 }
for ( j3 in 1:2 ) { b3[j3] <- mean( mm[1:2,1:2,j3,1:2,1:NSubj] ) - b0 }
for ( j4 in 1:2 ) { b4[j4] <- mean( mm[1:2,1:2,1:2,j4,1:NSubj] ) - b0 }
for ( js in 1:NSubj ) { bS[js] <- mean( mm[1:2,1:2,1:2,1:2,js] ) - b0 }

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
m[j1,j2,j3,j4]<- mean(mm[j1,j2,j3,j4,1:NSubj])
}}}}

for ( j1 in 1:2 ) { for ( j2 in 1:2 ) {
b1b2[j1,j2] <- mean(mm[j1,j2,1:2,1:2,1:NSubj]) - ( b0 + b1[j1] + b2[j2] )
}}

for ( j1 in 1:2 ) { for ( j3 in 1:2) {
b1b3[j1,j3] <- mean(mm[j1,1:2,j3,1:2,1:NSubj]) - ( b0 + b1[j1] + b3[j3] )
}}

for ( j1 in 1:2 ) { for ( j4 in 1:2 ) {
b1b4[j1,j4] <- mean(mm[j1,1:2,1:2,j4,1:NSubj]) - ( b0 + b1[j1] + b4[j4] )
}}

for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b2b3[j2,j3] <- mean(mm[1:2,j2,j3,1:2,1:NSubj]) - ( b0 + b2[j2] + b3[j3] )
}}

for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b2b4[j2,j4] <- mean(mm[1:2,j2,1:2,j4,1:NSubj]) - ( b0 + b2[j2] + b4[j4] )
}}

for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b3b4[j3,j4] <- mean(mm[1:2,1:2,j3,j4,1:NSubj]) - ( b0 + b3[j3] + b4[j4] )
}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) {
b1b2b3[j1,j2,j3] <- mean(m[j1,j2,j3,1:2]) - ( b0 + b1[j1] + b2[j2] + b3[j3] + b1b2[j1,j2] + b1b3[j1,j3] + b2b3[j2,j3] )
}}}

for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j4 in 1:2 ) {
b1b2b4[j1,j2,j4] <- mean(m[j1,j2,1:2,j4]) - ( b0 + b1[j1] + b2[j2] + b4[j4] + b1b2[j1,j2] + b1b4[j1,j4] + b2b4[j2,j4] )
}}}

for ( j2 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
b2b3b4[j2,j3,j4] <- mean(m[1:2,j2,j3,j4]) - ( b0 + b2[j2] + b3[j3] + b4[j4] + b2b3[j2,j3] + b2b4[j2,j4] + b3b4[j3,j4] )
}}}

 for ( j1 in 1:2) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
 b1b3b4[j1,j3,j4] <- mean(m[j1,1:2,j3,j4]) - ( b0 + b1[j1] + b3[j3] + b4[j4] + b1b3[j1,j3] + b1b4[j1,j4] + b3b4[j3,j4] )
 }}}
 
 for ( j1 in 1:2) { for ( j2 in 1:2 ) { for ( j3 in 1:2 ) { for ( j4 in 1:2 ) {
 b1b2b3b4[j1,j2,j3,j4] <- m[j1,j2,j3,j4] - ( b0 + b1[j1] + b2[j2] + b3[j3] + b4[j4] + b1b2[j1,j2] + b1b3[j1,j3] +  b1b4[j1,j4] + b2b3[j2,j3] + b2b4[j2,j4] + b3b4[j3,j4] + b1b2b3[j1,j2,j3] + b1b2b4[j1,j2,j4] + b2b3b4[j2,j3,j4] + b1b3b4[j1,j3,j4] )
 }}}}

}
" # close quote for modelstring
#writeLines(modelstring,con="model.txt")