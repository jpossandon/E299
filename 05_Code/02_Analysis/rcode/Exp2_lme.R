datFrameOK$RespMF    = as.factor(datFrameOK$RespM) 
datFrameOK$HandCF    = as.factor(datFrameOK$HandC) 
datFrameOK$LegCF     = as.factor(datFrameOK$LegC) 
datFrameOK$taskF     = as.factor(datFrameOK$task) 
datFrameOK$cLxLF      = as.factor(datFrameOK$cLxL) 
datFrameOK$cSxBF      = as.factor(datFrameOK$cSxB) 
datFrameOK$subjIndxF = as.factor(datFrameOK$subjIndx)

contrasts(datFrameOK$RespMF) <- contr.sum(2)
contrasts(datFrameOK$HandCF) <- contr.sum(2)
contrasts(datFrameOK$LegCF) <- contr.sum(2)
contrasts(datFrameOK$taskF) <- contr.sum(2)
contrasts(datFrameOK$cLxLF) <- contr.sum(2)
contrasts(datFrameOK$cSxBF) <- contr.sum(2)
contrasts(datFrameOK$subjIndxF) <- contr.sum(length(levels(datFrameOK$subjIndxF)))
datFrameOK$trial_RTms = datFrameOK$trial_RT*1000 

require(MuMIn)
###
# I am not sure about this one becuase they are not taking in acount the multiple measurements at every level 
# model with all together

mixmodel1 = lmer(trial_RTms ~ LegCF*HandCF*cLxLF + (1|subjIndxF), data=datFrameOK, REML=F)
summary(mixmodel1)
anova(mixmodel1)

# this two are equivalents (and very similat to above)
mixmodel1 = lme(trial_RTms ~ LegCF*HandCF*cLxLF , random= ~1|subjIndxF, data=datFrameOK)
summary(mixmodel1)
anova(mixmodel1)
anovamodel1 = aov(trial_RTms ~ LegCF*HandCF*cLxLF + Error(subjIndxF) , data=datFrameOK)
summary(anovamodel1)

###%###########################
# random slopes?
mixmodel1 = lmer(trial_RTms ~ LegCF*HandCF*cLxLF + (LegCF*HandCF*cLxLF|subjIndxF), data=datFrameOK)
summary(mixmodel1)
anova(mixmodel1)
anovamodel1 = aov(trial_RTms ~ LegCF*HandCF*cLxLF + Error(subjIndxF/(LegCF*HandCF*cLxLF)) , data=datFrameOK)
summary(anovamodel1)

#############################
# model with subject averages
#############################
datFrameAvg = ddply(datFrameOK,.(subjIndxF,LegCF,HandCF,cLxLF),summarize,trial_RTms=mean(trial_RTms))
mixmodel1 = lmer(trial_RTms ~ LegCF*HandCF*cLxLF + (1|subjIndxF), data=datFrameAvg, REML=F)
summary(mixmodel1)
anova(mixmodel1)
r.squaredGLMM(mixmodel1)
# this are equivalent
mixmodel1 = lme(trial_RTms ~ LegCF*HandCF*cLxLF , random= ~1|subjIndxF, data=datFrameAvg)
summary(mixmodel1)
anova(mixmodel1)
anovamodel1 = aov(trial_RTms ~ LegCF*HandCF*cLxLF + Error(subjIndxF) , data=datFrameAvg)
summary(anovamodel1)

# adding response mode
datFrameAvg = ddply(datFrameOK,.(subjIndxF,LegCF,HandCF,cLxLF,RespMF),summarize,trial_RTms=mean(trial_RTms))

anovamodel1 = aov(trial_RTms ~ LegCF*HandCF*cLxLF + Error(subjIndxF) , data=datFrameAvg)
summary(anovamodel1)
anovamodel2 = aov(trial_RTms ~ LegCF*HandCF*cLxLF*RespMF + Error(subjIndxF) , data=datFrameAvg)
summary(anovamodel2)


noRespM = lmer(trial_RTms ~ LegCF*HandCF*cLxLF + (1|subjIndxF), data=datFrameAvg,REML=F)
summary(noRespM)
wRespM = lmer(trial_RTms ~ LegCF*HandCF*cLxLF*RespMF + (1|subjIndxF), data=datFrameAvg,REML=F)
summary(wRespM)
anova(noRespM,wRespM)
r.squaredGLMM(noRespM)
r.squaredGLMM(wRespM)

# alternative model
datFrameAvg = ddply(datFrameOK,.(subjIndxF,LegCF,HandCF,cSxBF),summarize,trial_RTms=mean(trial_RTms))
mixmodel1 = lmer(trial_RTms ~ LegCF*HandCF*cSxBF + (1|subjIndxF), data=datFrameAvg, REML=F)
