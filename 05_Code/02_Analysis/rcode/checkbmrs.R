library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library("brms")
library("bayesplot")
library("data.table")
library("ggplot2")
dat1        = ddply(datFrame, .(subjIndx,RespM,LegC,HandC,cLxL,task), summarize,  meanRT=mean(trial_RT, na.rm=T), median=median(trial_RT, na.rm=T))
dat1$RespM <- as.factor(dat1$RespM)
dat1$LegC <- as.factor(dat1$LegC)
dat1$HandC <- as.factor(dat1$HandC)
dat1$cLxL <- as.factor(dat1$cLxL)
dat1$task<- as.factor(dat1$task)
fit1 <- brm(formula = bf(meanRT ~ RespM*LegC*HandC*task + (1|subjIndx)),  save_all_pars = TRUE, control = list(adapt_delta = 0.9),iter = 5000, warmup = 500,
            chains = 4, thin = 2,  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 10), class = "b"), prior(cauchy(0, 5), class = "sigma")),
            data = dat1)

fit2 <- brm(formula = bf(meanRT ~ cLxL*LegC*HandC+ (1|subjIndx)),  save_all_pars = TRUE, control = list(adapt_delta = 0.9),iter = 5000, warmup = 500,
            chains = 4, thin = 2,  prior = c(prior(normal(0, 10), class = "Intercept"),
            prior(normal(0, 10), class = "b"), prior(cauchy(0, 5), class = "sigma")),
            data = dat1)
library("loo")
loo1 = loo(fit1)
loo2 = loo(fit2)
loo_compare(loo1,loo2)