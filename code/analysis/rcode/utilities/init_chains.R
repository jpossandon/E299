if(!censor){
  initsList <- function(chainIDX="1") {
    # intial values of censored data:
    yInit   <- zy
    yInit[] <- NA
     # PRNG names from http://www.r-project.org/useR-2006/Slides/Plummer.pdf
    getRNG <- function(cIDX) {
      return (switch (cIDX,
                      "1" = "base::Wichmann-Hill",
                      "2" = "base::Marsaglia-Multicarry",
                      "3" = "base::Super-Duper",
                      "4" = "base::Mersenne-Twister")
      )
    }
    return (
      list( # TODO? how to initialize with the glm module?
        a0C = rep(0, NCond)
        , a0S = rep(0, NSubj)
        , a1C = rep(0, NCond)
        , a1S = rep(0, NSubj)
        , a2C = rep(0, NCond)
        , a2S = rep(0, NSubj)
         , .RNG.name = getRNG(chainIDX)
      )
    )
  }
}
if(censor){
  initsList <- function(chainIDX="1") {
    # intial values of censored data:
    yInit   <- zy
    yInit[] <- NA
    if(logn){
      yInit[is.censored] = log(exp(censorLimit[is.censored]) + runif(sum(is.censored), .001, .01)) #is this ok when we do nos use the lognormal model??
    }
    if(!logn){
      yInit[is.censored] = censorLimit[is.censored] + runif(sum(is.censored), .001, .01) #is this ok when we do nos use the lognormal model??
    }
      #   yInit[is.censored] = log(exp(censorLimit[is.censored] + .001))
    # PRNG names from http://www.r-project.org/useR-2006/Slides/Plummer.pdf
    getRNG <- function(cIDX) {
      return (switch (cIDX,
                      "1" = "base::Wichmann-Hill",
                      "2" = "base::Marsaglia-Multicarry",
                      "3" = "base::Super-Duper",
                      "4" = "base::Mersenne-Twister")
      )
    }
    return (
      list( # TODO? how to initialize with the glm module?
        a0C = rep(0, NCond)
        , a0S = rep(0, NSubj)
        , a1C = rep(0, NCond)
        , a1S = rep(0, NSubj)
        , a2C = rep(0, NCond)
        , a2S = rep(0, NSubj)
        ,   y = yInit           # censored values must be > censor limit #CENSORING
        , .RNG.name = getRNG(chainIDX)
      )
    )
  }
}