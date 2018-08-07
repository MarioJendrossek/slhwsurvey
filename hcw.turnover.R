# Turnover script

require(RColorBrewer)
library(boot)
library(matrixStats)
require(fitdistrplus)
require(ggplot2)
require(gamlss)


# prepare color pallettes
display.brewer.pal(n = 3, name = 'RdBu')
display.brewer.pal(n = 5, name = 'RdBu')
pal3 <- brewer.pal(n = 3, name = "RdBu")
pal4 <- brewer.pal(n = 4, name = "RdBu")
pal5 <- brewer.pal(n = 5, name = "RdBu")
pal6 <- brewer.pal(n = 6, name = "RdBu")

# B|1 CURRENT JOB (DURATION_JOB)
# B|1|1 Histogram and fitted distribution

# jitter plot age/duration
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/durations_byage.pdf", height = 7, width = 15)
par(las=1)
par(mfrow=c(1,2), mar=c(4.5,4,2,1))
plot(hcw.data$duration_hcw ~ jitter(hcw.data$age_gp, 1), col=pal3[3], pch = 15, xlab="age group", ylab="duration in years", xaxt='n', main="Duration HCW")
axis(1, at=1:5, labels=c("18-24", "25-34", "35-44", "45-54", "55+"))
plot(hcw.data$duration_job ~ jitter(hcw.data$age_gp, 1), col=pal3[3], pch = 15, xlab="age group", ylab="duration in years", xaxt='n', main="Duration current job")
axis(1, at=1:5, labels=c("18-24", "25-34", "35-44", "45-54", "55+"))
dev.off()

# quantiles of duration of current job
mean(hcw.data$duration_job, na.rm=T) # average 6.26y
median(hcw.data$duration_job, na.rm=T) # median 4.18 (CI)
quantile(hcw.data$duration_job, 0.25, na.rm=T) # Q1: 1.43
quantile(hcw.data$duration_job, 0.75, na.rm=T) # Q3: 7.87


# FIT DISTRIBUTION
fitdist(hcw.data$duration_job[!is.na(hcw.data$duration_job)], distr = "exp", method = "mle")
fit_exp <- fitdist(hcw.data$duration_job[!is.na(hcw.data$duration_job)& hcw.data$duration_job>0 & hcw.data$duration_job<47], "exp")
summary(fit_exp)
dist_exp <- dexp(0:41, rate = 0.1675)


# rate for expo dist
summary(fit_exp)

# plot the histogram
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/hist_durationjob.pdf", height = 7, width = 10)
par(las=1, mfrow=c(1,1))
hist(hcw.data$duration_job, xlab="Time since job start", main="", col=pal3[3], ylab="Proportion", probability = T, breaks=40) # geometric distribution?
lines(dist_exp, lwd="3", col=pal3[1])
dev.off()

# bootstrap rate to get confidence interval on distribution plot

statistic <- function(x, inds) {fitdist(x[inds],"exp")$estimate}
# create bootstrap sample of rate
bs.job <- boot(hcw.data$duration_job[!is.na(hcw.data$duration_job)], statistic, R = 5000)
print(boot.ci(bs.job, conf=0.95, type="bca"))

# CI BS: 0.1469 - 0.1893 (ordinary nonparametric bootstrap)

#create table with distributions based on bootstrapped rate
nsims <- 5000
ncols <- 42
bs.table.job  <- matrix(NA, nrow=5000, ncol=42)
for( i in (1:nsims)) {
  for(k in (1:ncols)) {
    bs.table.job[i,k] <- dexp(0:(ncols-1),bs.job$t[i])[k]
  }
}


# plot as in Roz script
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
} 

plot(colQuantiles(bs.table.job, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2))
polygon(y=c(colQuantiles(bs.table.job, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.job, probs=0.975, na.rm=T))),
        x=c(1:42, rev(1:42)), col=transp("firebrick", 0.3), border=NA)

# overview of bootstrap results
fitted.dist <- data.frame(time=1:42, 
                          est=colQuantiles(bs.table.job, probs=0.5, na.rm=T),
                          lcl=colQuantiles(bs.table.job, probs=0.025, na.rm=T),
                          ucl=colQuantiles(bs.table.job, probs=0.975, na.rm=T))

# plot distribution data with fit and CI
par(las=1, mfrow=c(1,1))
hist(hcw.data$duration_job, xlab="Time since job start", main="", col=pal3[3], ylab="Proportion", probability = T, breaks=40) # geometric distribution?
lines(colQuantiles(bs.table.job, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2), ylab="% negative")
polygon(y=c(colQuantiles(bs.table.job, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.job, probs=0.975, na.rm=T))),
        x=c(1:42, rev(1:42)), col=transp("firebrick", 0.3), border=NA)

# with data as points
histo_job <-     hist(hcw.data$duration_job,  probability = T, breaks=40) # geometric distribution?
par(las=1, mfrow=c(1,1))
plot(x=histo_job$breaks[2:43], y=histo_job$density, xlab="Time since job start", main="", col=pal3[3], ylab="Proportion") # geometric distribution?
lines(colQuantiles(bs.table.job, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2), ylab="% negative")
polygon(y=c(colQuantiles(bs.table.job, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.job, probs=0.975, na.rm=T))),
        x=c(1:42, rev(1:42)), col=transp("firebrick", 0.3), border=NA)



# ++++++ B|1|2 regression++++++
# bivariate
#sex
mod_exp_sex <- lm(log(hcw.data$duration_job) ~ hcw.data$sex,  na.action = "na.omit")
summary(mod_exp_sex)
# strongly associated

# age
mod_exp_age <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$age_gp), na.action = "na.omit")
summary(mod_exp_age)
# strong asso

# district
mod_exp_dist <- lm(log(hcw.data$duration_job) ~ hcw.data$district,  na.action = "na.omit")
summary(mod_exp_dist)
# strongly associated

# edu
mod_exp_edu <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$edu_gp),  na.action = "na.omit")
summary(mod_exp_edu)
# strongly associated

# income
mod_exp_inc <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$income_gp), na.action = "na.omit")
summary(mod_exp_inc)
# associated

# religion
mod_exp_rel <- lm(log(hcw.data$duration_job) ~ hcw.data$rel, na.action = "na.omit")
summary(mod_exp_rel)
# not associated

# ethnicity
mod_exp_ethnic <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$ethnic_gp), na.action = "na.omit")
summary(mod_exp_ethnic)
# not associated

# prof
mod_exp_prof <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$prof_gp), na.action = "na.omit")
summary(mod_exp_prof)
# somewhat associated (gp3)

# hctype
mod_exp_hctype <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$health_ctr_type), na.action = "na.omit")
summary(mod_exp_hctype)
# somewhat associated (cat 6)

# full-time
mod_exp_ft <- lm(log(hcw.data$duration_job) ~ hcw.data$full_time,  na.action = "na.omit")
summary(mod_exp_ft)
#  associated

#payroll
mod_exp_payr <- lm(log(hcw.data$duration_job) ~ hcw.data$payroll, na.action = "na.omit")
summary(mod_exp_payr)
# strongly associated

# num hc
mod_exp_numhc <- lm(log(hcw.data$duration_job) ~ as.factor(hcw.data$num_hc), na.action = "na.omit")
summary(mod_exp_numhc)
# not associated

# urban
mod_exp_urb <- lm(log(hcw.data$duration_job) ~ hcw.data$urban, na.action = "na.omit")
summary(mod_exp_urb)
# strongly associated

#multivariate
expmodel <- lm(log(hcw.data$duration_job)~ hcw.data$sex +  hcw.data$age_gp + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$income_gp) + hcw.data$full_time + hcw.data$num_hc + hcw.data$payroll + as.factor(hcw.data$health_ctr_type),  na.action = "na.omit")
summary(expmodel)
# associated: payroll, edu3, age, sex (hc type)

# exponential regression model
model_exp <- lm(log(hcw.data$duration_job) ~ hcw.data$sex + as.factor(hcw.data$age_gp) + hcw.data$district+ as.factor(hcw.data$edu_gp)  + as.factor(hcw.data$income) + hcw.data$payroll + hcw.data$full_time + hcw.data$urban)
summary(model_exp)



# B|2 HCW WORK DURATION
# B|2|1 hisogram and fitted distribution
# histogram and quantiles of duration of hcw work
mean(hcw.data$duration_hcw, na.rm=T) # average 10.9y
median(hcw.data$duration_hcw, na.rm=T) # median 8y
quantile(hcw.data$duration_hcw, 0.25, na.rm=T) # Q1: 4,6y
quantile(hcw.data$duration_hcw, 0.75, na.rm=T) # Q3: 14,0y

# fit distributions to data
fit_lnorm <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "lnorm")
fit_gamma <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "gamma")
fit_weibull <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "weibull")
# fit4 <- neg binomial (with duration in y)

# get the fitted densities and set parameters
fitD_lnorm <- dlnorm(0:45, meanlog=1.99, sdlog=1.03)
fitD_gamma <- dgamma(0:45, shape=1.354, rate=0.124)
fitD_weibull <- dweibull(0:45, shape=1.22, scale=11.71)


# add fitted line (blue) to histogram
par(mfrow=c(1,1))
hist(hcw.data$duration_hcw, xlab = "Duration of HCW work (years)", ylab="Proportion", prob=TRUE, breaks=40, main="")
lines(fitD_lnorm, lwd="3", col="blue")
lines(fitD_gamma, lwd="3", col="red")
lines(fitD_weibull, lwd="3", col="green")


# compare fit
par(mfrow=c(2,2))
plot.legend <- c("lognormal", "gamma", "weibull")
denscomp(list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)
cdfcomp (list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)
qqcomp  (list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)
ppcomp  (list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)


# plot the histogram
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/hist_durationhcw.pdf", height = 7, width = 10)
par(las=1, mfrow=c(1,1))
hist(hcw.data$duration_hcw, col=pal3[3], xlab = "Duration of HCW work (years)", ylab="Proportion", prob=TRUE, breaks=40, main="")
lines(fitD_gamma, lwd="3", col=pal3[1])
dev.off()


# B|2|2 regression
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
# not associated

# age
mod_gam_age <- glm(hcw.data$duration_hcw ~ as.factor(hcw.data$age_gp), family = "Gamma", na.action = "na.omit")
summary(mod_gam_age)
# strong asso

# district
mod_gam_dist <- glm(hcw.data$duration_hcw ~ hcw.data$district, family = "Gamma", na.action = "na.omit")
summary(mod_gam_dist)
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
# weakly associated

#payroll
mod_gam_payr <- glm(hcw.data$duration_hcw ~ hcw.data$payroll, family = "Gamma", na.action = "na.omit")
summary(mod_gam_payr)
# strongly associated

# num hc
mod_gam_numhc <- glm(hcw.data$duration_hcw ~ hcw.data$num_hc, family = "Gamma", na.action = "na.omit")
summary(mod_gam_numhc)
# strongly associated

# urban
mod_gam_urb <- glm(hcw.data$duration_hcw ~ hcw.data$urban, family = "Gamma", na.action = "na.omit")
summary(mod_gam_urb)
# strongly associated

#multivariate
gammamodel <- glm(hcw.data$duration_hcw ~ hcw.data$sex +  hcw.data$age_gp + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$income_gp) + hcw.data$full_time + hcw.data$num_hc + hcw.data$payroll, family = "Gamma", na.action = "na.omit")
summary(gammamodel)



# plot both histograms on one graph
# plot the histogram
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/histograms_durations.pdf", height = 7, width = 10)
par(las=1)
par(mfrow=c(1,2))
hist(hcw.data$duration_job, col=pal3[3], xlab="Duration (years)", main="Current job", ylab="Proportion", probability = T, breaks=40) # geometric distribution?
lines(dist_exp, lwd="3", col=pal3[1])
hist(hcw.data$duration_hcw, col=pal3[3], xlab = "Duration (years)", main="HCW overall", ylab="Proportion", prob=TRUE, breaks=40)
lines(fitD_gamma, lwd="3", col=pal3[1])
dev.off()

# bootstrap
# CI for parameters
statistic <- function(x, inds) {fitdist(x[inds],"gamma")$estimate}

bs <- boot(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw)], statistic, R = 5000)
print(boot.ci(bs, conf=0.95, type="bca"))
#print(boot.ci(bs$t[,2], conf=0.95, type="bca"))

f1b<-fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) ],"gamma",method="mle")
b1b<-bootdist(f1b, niter = 5000)
summary(b1b)

# nonparametric
#shape (1.353939) 1.141 1.576)
#rate  0.123906 ??0.1046716 ??0.1486943

    # parametric bootstrap results: 
    #Median      2.5%     97.5%
    # shape 1.3601715 1.1814576 1.5796533
    #rate  0.1246467 0.1045813 0.1488273
#graphic
statistic <- function(x, inds) {fitdist(x[inds],"gamma")$estimate}

# create bootstrap sample of rate
bs.hcw <- boot(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw)], statistic, R = 5000)

#create table with distributions based on bootstrapped rate
nsims <- 5000
ncols <- 46
bs.table.hcw  <- matrix(NA, nrow=5000, ncol=46)
for( i in (1:nsims)) {
  for(k in (1:ncols)) {
    bs.table.hcw[i,k] <- dgamma(0:(ncols-1),bs.hcw$t[i,1], bs.hcw$t[i,2])[k]
  }
}


# plot as in Roz script
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
} 

plot(colQuantiles(bs.table.hcw, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2))
polygon(y=c(colQuantiles(bs.table.hcw, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.hcw, probs=0.975, na.rm=T))),
        x=c(1:46, rev(1:46)), col=transp("firebrick", 0.3), border=NA)

# with data as points
histo_hcw <- hist(hcw.data$duration_hcw,  probability = T, breaks=40) # geometric distribution?
par(las=1, mfrow=c(1,1))
plot(x=histo_hcw$breaks[2:46], y=histo_hcw$density, xlab="Time since HCW start", main="", col=pal3[3], ylab="Proportion") # geometric distribution?
lines(colQuantiles(bs.table.hcw, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.15))
polygon(y=c(colQuantiles(bs.table.hcw, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.hcw, probs=0.975, na.rm=T))),
        x=c(1:46, rev(1:46)), col=transp("firebrick", 0.3), border=NA)


# graph of both distributions with fit
# with data as points
histo_job <-     hist(hcw.data$duration_job,  probability = T, breaks=40) # geometric distribution?
histo_hcw <- hist(hcw.data$duration_hcw,  probability = T, breaks=40) # geometric distribution?

pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/durations_withfit_ci.pdf", height = 7, width = 10)
par(las=1, mfrow=c(1,2))
plot(x=histo_job$breaks[2:43], y=histo_job$density, xlab="Time since job start", main="", col=pal3[3], ylab="Proportion", pch=16) # geometric distribution?
lines(colQuantiles(bs.table.job, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2), ylab="% negative")
polygon(y=c(colQuantiles(bs.table.job, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.job, probs=0.975, na.rm=T))),
        x=c(1:42, rev(1:42)), col=transp("firebrick", 0.3), border=NA)
plot(x=histo_hcw$breaks[2:46], y=histo_hcw$density, xlab="Time since HCW start", main="", col=pal3[3], ylab="Proportion", pch=16) # geometric distribution?
lines(colQuantiles(bs.table.hcw, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.15))
polygon(y=c(colQuantiles(bs.table.hcw, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.hcw, probs=0.975, na.rm=T))),
        x=c(1:46, rev(1:46)), col=transp("firebrick", 0.3), border=NA)
dev.off()

# B|3 additional Qs on turnover
table(hcw.data$break.) # 270 (88,8%) without break, 294 (96,7%) with max 6mo break, 10 (3,3%)
table(hcw.data$stay_6mo) # 224 (73,4%) very likely, 11,8% very unlikely
table(hcw.data$stay_24mo) # 106 (34,8%) very likely
table(hcw.data$payroll) # 179 (58,9%) on payroll, 125 volunteer
table(hcw.data$full_time) # 284 (93,1%) full-time



# C| density function gamma
shape_hcw <- 1.360
rate_hcw=0.125

density_hcw <- pgamma(1:50,shape = shape_hcw, rate =rate_hcw)
acceptance <- 0.763
acceptance_high <- 0.964
waning_low <- 0
waning_mid <- 0.1
waning_high <- 0.5
reach <- 1
efficacy <- 1
efficacy_low <- 0.75
coverage <- c()
for(t in (1:20)) {
  coverage[t] <- acceptance*reach*efficacy*(1-waning_low)*(1-density_hcw[t])
}

# with high acceptance sensitivity
coverage_high <- c()
for(t in (1:20)) {
  coverage_high[t] <- acceptance_high*reach*efficacy*(1-waning_low)*(1-density_hcw[t])
}

# with waning immunity
coverage_waning <- c()
for(t in (1:20)) {
  coverage_waning[t] <- acceptance*reach*efficacy*((1-waning_mid)^t)*(1-density_hcw[t])
}

# 

