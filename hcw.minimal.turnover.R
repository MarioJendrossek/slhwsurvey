# minimal script for Sam

# read in data
setwd(folder)
hcw.data <- as.data.frame(read.csv("HCWsurvey_limited.csv"))

# Turnover script

require(RColorBrewer)
library(boot)
library(matrixStats)
require(fitdistrplus)
require(ggplot2)
require(gamlss)


# color
pal3 <- brewer.pal(n = 3, name = "RdBu")
pal4 <- brewer.pal(n = 4, name = "RdBu")
pal5 <- brewer.pal(n = 5, name = "RdBu")
pal6 <- brewer.pal(n = 6, name = "RdBu")


# 1. Duration current job
# A| FIT DISTRIBUTION
fitdist(hcw.data$duration_job[!is.na(hcw.data$duration_job)], distr = "exp", method = "mle")
fit_exp <- fitdist(hcw.data$duration_job[!is.na(hcw.data$duration_job)& hcw.data$duration_job>0 & hcw.data$duration_job<47], "exp")
summary(fit_exp)
dist_exp <- dexp(0:41, rate = 0.1675)

# rate for expo dist
summary(fit_exp)

# B| regression
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
gm_mean(hcw.data$duration_job)

mod0 <- lm(log(hcw.data$duration_job) ~1)
summary(mod0)
exp(coef(mod0))

# bivariate
#sex
mod_exp_sex <- lm(log(hcw.data$duration_job) ~ hcw.data$sex,  na.action = "na.omit")
summary(mod_exp_sex)
coef(mod_exp_sex)
exp(coef(mod_exp_sex))
exp(confint(mod_exp_sex))
# strongly associated

# age
mod_exp_age <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$age_gp), na.action = "na.omit")
summary(mod_exp_age)
coef(mod_exp_age)
exp(coef(mod_exp_age))
exp(confint(mod_exp_age))

wald.test(b = coef(mod_exp_age), Sigma = vcov(mod_exp_age), Terms = 2:5)

# strong asso

# district
mod_exp_dist <- lm(log(hcw.data$duration_job) ~ hcw.data$district,  na.action = "na.omit")
summary(mod_exp_dist)
coef(mod_exp_dist)
exp(coef(mod_exp_dist))
exp(confint(mod_exp_dist))
# strongly associated

# edu
mod_exp_edu <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$edu_gp),  na.action = "na.omit")
summary(mod_exp_edu)
coef(mod_exp_edu)
exp(coef(mod_exp_edu))
exp(confint(mod_exp_edu))

wald.test(b = coef(mod_exp_edu), Sigma = vcov(mod_exp_edu), Terms = 2:3)

# strongly associated

# income
mod_exp_inc <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$income_gp), na.action = "na.omit")
summary(mod_exp_inc)
coef(mod_exp_inc)
exp(coef(mod_exp_inc))
exp(confint(mod_exp_inc))

wald.test(b = coef(mod_exp_inc), Sigma = vcov(mod_exp_inc), Terms = 2:3)

# associated

# religion
mod_exp_rel <- lm(log(hcw.data$duration_job) ~ hcw.data$rel, na.action = "na.omit")
summary(mod_exp_rel)
coef(mod_exp_rel)
exp(coef(mod_exp_rel))
exp(confint(mod_exp_rel))
# not associated

# ethnicity
mod_exp_ethnic <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$ethnic_gp), na.action = "na.omit")
summary(mod_exp_ethnic)
coef(mod_exp_ethnic)
exp(coef(mod_exp_ethnic))
exp(confint(mod_exp_ethnic))

wald.test(b = coef(mod_exp_ethnic), Sigma = vcov(mod_exp_ethnic), Terms = 2:9)

# not associated

# prof
mod_exp_prof <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$prof_gp), na.action = "na.omit")
summary(mod_exp_prof)
coef(mod_exp_prof)
exp(coef(mod_exp_prof))
exp(confint(mod_exp_prof))
wald.test(b = coef(mod_exp_prof), Sigma = vcov(mod_exp_prof), Terms = 2:4)

# somewhat associated (gp3)

# hctype
mod_exp_hctype <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$hc_type_gp), na.action = "na.omit")
summary(mod_exp_hctype)
coef(mod_exp_hctype)
exp(coef(mod_exp_hctype))
exp(confint(mod_exp_hctype))

wald.test(b = coef(mod_exp_hctype), Sigma = vcov(mod_exp_hctype), Terms = 2:4)

# somewhat associated (cat 6)

# full-time
mod_exp_ft <- lm(log(hcw.data$duration_job) ~ hcw.data$full_time,  na.action = "na.omit")
summary(mod_exp_ft)
coef(mod_exp_ft)
exp(coef(mod_exp_ft))
exp(confint(mod_exp_ft))
#  associated

#payroll
mod_exp_payr <- lm(log(hcw.data$duration_job) ~ hcw.data$payroll, na.action = "na.omit")
summary(mod_exp_payr)
coef(mod_exp_payr)
exp(coef(mod_exp_payr))
exp(confint(mod_exp_payr))
# strongly associated

# urban
mod_exp_urb <- lm(log(hcw.data$duration_job) ~ hcw.data$urban, na.action = "na.omit")
summary(mod_exp_urb)
coef(mod_exp_urb)
exp(coef(mod_exp_urb))
exp(confint(mod_exp_urb))
# strongly associated

#multivariate
expmodel <- lm(log(hcw.data$duration_job)~ hcw.data$sex +  as.factor(hcw.data$age_gp) + hcw.data$district + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$income_gp) + hcw.data$full_time + hcw.data$payroll + as.factor(hcw.data$prof_gp) + hcw.data$urban,  na.action = "na.omit")
summary(expmodel)
coef(expmodel)
exp(coef(expmodel))
exp(confint(expmodel))

wald.test(b = coef(expmodel), Sigma = vcov(expmodel), Terms = 3:6)
wald.test(b = coef(expmodel), Sigma = vcov(expmodel), Terms = 8:9)
wald.test(b = coef(expmodel), Sigma = vcov(expmodel), Terms = 10:11)
wald.test(b = coef(expmodel), Sigma = vcov(expmodel), Terms = 14:16)

# associated: payroll, edu3, age, sex (hc type)

# 2. Duration HCW work
# A| FIT DISTRIBUTION
# fit distributions to data
fit_lnorm <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "lnorm")
fit_gamma <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "gamma")
fit_weibull <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "weibull")
# fit4 <- neg binomial (with duration in y)

# get the fitted densities and set parameters
fitD_lnorm <- dlnorm(0:45, meanlog=1.99, sdlog=1.03)
fitD_gamma <- dgamma(0:45, shape=1.354, rate=0.124)
fitD_weibull <- dweibull(0:45, shape=1.22, scale=11.71)


# B | regression
# no NAs/negatives/0s
#hcw.data$duration_hcw[is.na(hcw.data$duration_hcw)] <- 3
hcw.data$duration_hcw[hcw.data$duration_hcw<=0] <- NA

# a) lognormal regression

# model
lognormalmodel <- gamlss(hcw.data$duration_hcw ~ hcw.data$district + hcw.data$sex + hcw.data$full_time, family=LOGNO)
summary(lognormalmodel)


# b) gamma regression

# bivariate
#sex
mod_gam_sex <- glm(hcw.data$duration_hcw ~ hcw.data$sex, family = "Gamma", na.action = "na.omit")
summary(mod_gam_sex)
exp(coef(mod_gam_sex))
# not associated

# age
mod_gam_age <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$age_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_age)
exp(coef(mod_gam_age))
exp(confint((mod_gam_age)))
# strong asso

# district
mod_gam_dist <- glm(hcw.data$duration_hcw ~ hcw.data$district, family = "Gamma", na.action = "na.omit")
summary(mod_gam_dist)
exp(coef(mod_gam_dist))
# not associated

# edu
mod_gam_edu <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$edu_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_edu)
# weakly associated

# income
mod_gam_inc <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$income_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_inc)
# strongly associated

# religion
mod_gam_rel <- glm(hcw.data$duration_hcw ~ hcw.data$rel, family = "Gamma", na.action = "na.omit")
summary(mod_gam_rel)
# not associated

# ethnicity
mod_gam_ethnic <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$ethnic_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_ethnic)
# not associated

# prof
mod_gam_prof <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$prof_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_prof)
# somewhat weakly associated 

# hctype
mod_gam_hctype <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$health_ctr_type), family = "Gamma", na.action = "na.omit")
summary(mod_gam_hctype)
# not associated

# full-time
mod_gam_ft <- glm(hcw.data$duration_hcw ~ hcw.data$full_time, family = "Gamma", na.action = "na.omit")
summary(mod_gam_ft)
exp(coef(mod_gam_ft))
# weakly associated

#payroll
mod_gam_payr <- glm(hcw.data$duration_hcw ~ hcw.data$payroll, family = "Gamma", na.action = "na.omit")
summary(mod_gam_payr)
exp(coef(mod_gam_payr))
# strongly associated

# num hc
mod_gam_numhc <- glm(hcw.data$duration_hcw ~ hcw.data$num_hc, family = "Gamma", na.action = "na.omit")
summary(mod_gam_numhc)
# strongly associated

# urban
mod_gam_urban <- glm(hcw.data$duration_hcw ~ hcw.data$urban, family = "Gamma", na.action = "na.omit")
summary(mod_gam_urban)
# strongly associated

#multivariate
gammamodel <- glm(hcw.data$duration_hcw ~ hcw.data$sex +  hcw.data$age_gp + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$income_gp) + hcw.data$full_time + hcw.data$num_hc + hcw.data$payroll, family = "Gamma", na.action = "na.omit")
summary(gammamodel)

