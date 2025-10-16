# B010 analysis th 2024

################################################################################
## EXPERIMENT 1 (FOOT STIM)
################################################################################
d1 <- read.csv( path.join( dataPath, exp1_filename ), header = TRUE, sep = ",", dec = "." )
# create a factor that contains which task it was (for consistence across all datasets)
d1$proAnti <- "pro"
d1$proAnti <- as.factor( d1$proAnti )
d1$exp <- "Exp1"
d1$stimuliAt <- "feet"
d1$responsesWith <- "hands"
# rename variables to be consistent with the labs recent variable names and across experiments
d1 <- d1 %>% rename( stimLimbPost = trial_crossed_legs )
d1 <- d1 %>% rename( respLimbPost = trial_crossed_hand ) 
d1 <- rawdata_preprocessing( d1 )
d1 <- d1 %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
# keepList <- c()
# keepList <- append( keepList, c( "1", "2", "3", "4", "5", "6", "7", "8", "9", 
#                           "10", "11", "12", "13", "14", "15", "16", "17", "18", 
#                           "19", "20", "21", "22" ) )
# dC <- keep_participants( dC, keepList )
# participants 9, 10, and 13 have very low accuracy in some conditions
# Participant 12 looks as if s/he did the same in ext/anat, with the two datapoint series
# looking identical. We could try whether this influences the results in any important way,
# but there is no an objective reason to exclude this participant.
removeList <- c()
removeList <- append( removeList, c( "9", "10", "13" ) )
d1 <- remove_participants( d1, removeList )
d <- d1
expAbbrev <- "1footS"

################################################################################
## EXPERIMENT 2 FOOT STIM
################################################################################
d2.footStim <- read.csv( path.join( dataPath,exp2_filename_footStim ), header = TRUE, sep = ",", dec = "." )
d2.footStim$proAnti <- "pro"
d2.footStim$exp <- "Exp2"
d2.footStim$stimuliAt <- "feet"
d2.footStim$responsesWith <- "hands"
# rename variables to be consistent with the labs recent variable names and across experiments
d2.footStim <- d2.footStim %>% rename( stimLimbPost = trial_crossed_legs )
d2.footStim <- d2.footStim %>% rename( respLimbPost = trial_crossed_hand ) 
d2.footStim <- rawdata_preprocessing( d2.footStim )
d2.footStim <- d2.footStim %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
keepList <- c()
keepList <- append( keepList, c( "1", "45", "71", "72", "73", "74", "75", "78", "79", 
                                 "80", "81", "82", "83", "84", "85", "87", "88", 
                                 "89", "90", "91", "92", "93", "94" ) )
d2.footStim <- keep_participants( d2.footStim, keepList )
removeList <- c()
removeList <- append( removeList, c( "10", "45", "94" ) )
# removeList <- append( removeList, c( "10", "45", "75", "79", "80", "84", "94" ) )
d2.footStim <- remove_participants( d2.footStim, removeList )
d <- d2.footStim
expAbbrev <- "2footS"

################################################################################
## EXPERIMENT 2 HAND STIM
################################################################################
d2.handStim <- read.csv( path.join( dataPath,exp2_filename_handStim ), header = TRUE, sep = ",", dec = "." )
# create a factor that contains which task it was (for consistence across all datasets)
d2.handStim$proAnti <- "pro"
d2.handStim$exp <- "Exp2"
d2.handStim$stimuliAt <- "hands"
d2.handStim$responsesWith <- "feet"
# rename variables to be consistent with the labs recent variable names and across experiments
d2.handStim <- d2.handStim %>% rename( stimLimbPost = trial_crossed_hand )
d2.handStim <- d2.handStim %>% rename( respLimbPost = trial_crossed_legs ) 
d2.handStim <- rawdata_preprocessing( d2.handStim )
d2.handStim <- d2.handStim %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
keepList <- c()
keepList <- append( keepList, c( "1", "45", "71", "72", "73", "74", "75", "78", "79", 
                                 "80", "81", "82", "83", "84", "85", "87", "88", 
                                 "89", "90", "91", "92", "93", "94" ) )
d2.handStim <- keep_participants( d2.handStim, keepList )
removeList <- c()
removeList <- append( removeList, c( "10", "45", "94" ) )
# removeList <- append( removeList, c( "10", "45", "75", "79", "80", "84", "94" ) )
d2.handStim <- remove_participants( d2.handStim, removeList )
d <- d2.handStim
  expAbbrev <- "2handS"


################################################################################
## EXPERIMENT 3 PRO (FOOT STIM)
################################################################################
d3.pro <- read.csv( paste0( dataPath, exp3_filename_pro ), header = TRUE, sep = ",", dec = "." )
d3.pro$proAnti <- "pro"
d3.pro$proAnti <- as.factor( d3.pro$proAnti )
d3.pro$exp <- "Exp3"
d3.pro$stimuliAt <- "stim: feet"
d3.pro$responsesWith <- "resp: hands"
d3.pro <- d3.pro %>% rename( stimLimbPost = trial_crossed_legs )
d3.pro <- d3.pro %>% rename( respLimbPost = trial_crossed_hand ) 
d3.pro <- rawdata_preprocessing( d3.pro )
d3.pro <- d3.pro %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
removeList <- c()
removeList <- append( removeList, c( "32", "33", "49", "52" ) )
d3.pro <- remove_participants( d3.pro, removeList )
d <- d3.pro
expAbbrev <- "3pro"
# subjects: 31, 32, 33, 36, 37, 38, 39, 40, 41, 42, 43, 44
# 45, 47, 48, 49, 51, 52, 53, 54, 55, 59, 60, 61, 62, 63

################################################################################
## EXPERIMENT 3 ANTI (FOOT STIM)
################################################################################
d3.anti <- read.csv( paste0( dataPath, exp3_filename_anti ), header = TRUE, sep = ",", dec = "." )
d3.anti$proAnti <- "anti"
d3.anti$proAnti <- as.factor( d3.anti$proAnti )
d3.anti$exp <- "Exp3"
d3.anti$stimuliAt <- "feet"
d3.anti$responsesWith <- "hands"
d3.anti <- d3.anti %>% rename( stimLimbPost = trial_crossed_legs )
d3.anti <- d3.anti %>% rename( respLimbPost = trial_crossed_hand ) 
d3.anti <- rawdata_preprocessing( d3.anti )
d3.anti <- d3.anti %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
removeList <- c()
removeList <- append( removeList, c( "32", "33", "49", "52" ) )
d3.anti <- remove_participants( d3.anti, removeList )
d <- d3.anti
expAbbrev <- "3anti"

# testing plausibility of coding:
# in this condition, button has to be opposite of stimulated limb, i.e. respCorr must be 1
# if these two values are given in respButton and stimLimb
# dt <- d1 %>% filter( instruction == "ext", stimLimbPost == "sX", respLimbPost == "rII")
# in this condition, button has to be opposite of stimulated limb
# dt <- d1 %>% filter( instruction == "anat", stimLimbPost == "sII", respLimbPost == "rX")
# these tests pan out ;)




