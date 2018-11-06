
# load packages
#require(manyregs)
#require(corrplot)
require(fitdistrplus)
require(ggplot2)
require(gamlss)
require(nnet)
require(aod)
require(RColorBrewer)
library(boot)
library(matrixStats)

# prepare color pallettes
display.brewer.pal(n = 3, name = 'RdBu')
display.brewer.pal(n = 5, name = 'RdBu')
pal3 <- brewer.pal(n = 3, name = "RdBu")
pal4 <- brewer.pal(n = 4, name = "RdBu")
pal5 <- brewer.pal(n = 5, name = "RdBu")
pal6 <- brewer.pal(n = 6, name = "RdBu")

# +++++++ A| descriptive statitics (for table 1) ++++++
# UNIVARIATE 
# sex
table(hcw.data$sex) # 195F, 110M
# age
table(hcw.data$age_gp) # 15<25, 109 25-34, 102 35-44, 54 45-54, 24 55+
# district
table(hcw.data$district) # 183 FT, 122 Kambia
# ethnic group
table(hcw.data$ethnic_gp) # 89 Temne, 87 Mende, 46 Limba, 12 Fula, 20 Mandingo...
# religion
table(hcw.data$rel) # 167 Christian, 138 Muslim
# health centre type
table(hcw.data$health_ctr_type) # 130 Gov hosp (42,6%), 4 private hosp (1,3%), 11 NGO clinic (3,6%), 119 CHC (39,0%), 24 CHP (7,9%), 17 MCHP (5,6%)

# profession
table(hcw.data$profession)

# Descriptive figures: 
# age distribution
ggplot(hcw.data, aes(x=age_gp)) + geom_histogram()

# BIVARIATE
# radio/tv
counts <- table(hcw.data$hh_radio, hcw.data$hh_tv)
legend1 <- c("No radio", "Radio")
barplot(counts, main="",
        xlab="TV ownership", col=pal2[c(1,3)],
        legend = legend1)

# most people own radio (>80%), most people also have tv (>66%). People with tv more likely to have radio.
inc.edu.table <- table(hcw.data$edu_gp, hcw.data$income_gp)
chisq.test(table(hcw.data$edu_gp, hcw.data$income_gp))
prop.table(inc.edu.table, 1) # better educated earn better on average
prop.table(inc.edu.table, 2) # better educated earn better on average

# proportion of income acc to edu group
inc.edu.table2 <- table(hcw.data$ses_gp, hcw.data$edu_gp)
counts2 <- prop.table(inc.edu.table2, 2)
#counts <- table(hcw.data$income_gp, hcw.data$edu_gp)
legend2 <- c("low income", "medium income", "high income")

pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/edu_income.pdf")
par(mfrow=c(1, 1), mar=c(5, 5, 4, 8), las=1)
barplot(counts2, main="",
        xlab="Education", ylab="Proportion", col=pal3[c(1,2,3)],
        legend = legend2, args.legend = list(x = "topright", bty = "n", inset=c(-0.35, 0)), names.arg = c("Low (n=34)", "Mid (n=136)", "High (n=135)"))
dev.off()

# profession by sex
table(hcw.data$sex, hcw.data$profession)
prof.sex.table <- table(hcw.data$sex, as.factor(hcw.data$prof_name))
counts3 <- prop.table(prof.sex.table,2)
legend3 <- c("female", "male")
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/sex_prof.pdf", height = 7, width = 15)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 6))
barplot(counts3, main="",
        xlab="Profession", ylab="Proportion", col=pal3[c(1,3)],
        legend = legend3, args.legend = list(x = "topright", bty = "n", inset=c(-0.07, 0)),
        names.arg = c("Admin(n=8)", "CHO/A(n=23)", "CHW(n=18)", "Support(n=35)", "Doctor(n=4)", "Lab(n=31)", "MCHA(n=24)", "Midwife(n=18)", "Nurse(n=134)", "Pharma(n=7)"))
dev.off()

# HC by district
dist_ctrtype_tbl <- table(hcw.data$health_ctr_type, hcw.data$district)
counts4 <- prop.table(dist_ctrtype_tbl,2)
legend4 <- c("Govt hospital", "Private clinic", "NGO clinic", "CHC", "CHP", "MCHP")
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/hctype_district.pdf", height = 7, width = 9)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 7.2))
barplot(counts4, main="",
        xlab="District", ylab="Proportion", col=pal6[1:6],
        legend = legend4, args.legend = list(x = "topright", bty = "n", inset=c(-0.19, 0)),
        names.arg = c("Freetown (n=183)", "Kambia (n=122)"))
dev.off()

# ++++ check whether population representative (asked Hana for more info) ++++
# sex, age...
# sex 63,8% in HRH strategy 62%

# (0.638-0.62)/(sqrt((0.62*(1-0.62))/305))
prop.test(x=195, n =305, p=0.62)

# professions: oversampled those in hospitals (lab workers), undersampled PHUs
# religion,
# profession (42,5% nurses vs. here 44,4%, 4,7% midwives (5.9%), 5,1% lab (10,2%), 28% MCHA (7,9%), 9,2% CHO(7,5%))
table(hcw.data$profession) # oversampled lab workers undersampled MCHA
# SES?
# volunteer
table(hcw.data$payroll) # 41,0% volunteers (vs. 3600/13500: 26,5%)



# correlations between vars

# calculate correlations between vars
# correlations <- cor(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)])
# corrplot(correlations, method="circle")
#_________________________________________________________________________________________________________________________

# B) ++++++HCW turnover+++++

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
# exponential regression model
model_exp <- lm(log(hcw.data$duration_job) ~ hcw.data$sex + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$age_gp) + as.factor(hcw.data$health_ctr_type) + hcw.data$district)
summary(model_exp)



# B|2 HCW WORK DURATION
# B|2|1 hisogram and fitted distribution
# histogram and quantiles of duration of hcw work
mean(hcw.data$duration_hcw, na.rm=T) # average 10.9y
median(hcw.data$duration_hcw, na.rm=T) # median 8y
quantile(hcw.data$duration_hcw, 0.25, na.rm=T) # Q1: 4,5y
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
gammamodel <- glm(hcw.data$duration_hcw ~ hcw.data$district + hcw.data$sex + as.factor(hcw.data$edu_gp) + as.factor(hcw.data$income_gp) +  as.factor(hcw.data$age_gp)+ hcw.data$rel + as.factor(hcw.data$profession_group) + hcw.data$payroll, family = "Gamma")
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

bs <- boot(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], statistic, R = 4000)
print(boot.ci(bs, conf=0.95, type="bca"))

f1b<-fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0],"gamma",method="mle")
b1b<-bootdist(f1b, niter = 4000)
summary(b1b)

#shape 1.3608255 1.1765463 1.5788897
#rate  0.1246115 0.1046716 0.1486943

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
        bs.table.gamma[i,k] <- dgamma(0:(ncols-1),bs.hcw$t[i,1], bs.hcw$t[i,2])[k]
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

par(las=1, mfrow=c(1,2))
plot(x=histo_job$breaks[2:43], y=histo_job$density, xlab="Time since job start", main="", col=pal3[3], ylab="Proportion") # geometric distribution?
lines(colQuantiles(bs.table.job, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.2), ylab="% negative")
polygon(y=c(colQuantiles(bs.table.job, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.job, probs=0.975, na.rm=T))),
        x=c(1:42, rev(1:42)), col=transp("firebrick", 0.3), border=NA)
plot(x=histo_hcw$breaks[2:46], y=histo_hcw$density, xlab="Time since HCW start", main="", col=pal3[3], ylab="Proportion") # geometric distribution?
lines(colQuantiles(bs.table.hcw, probs=0.5, na.rm=T), type="l", ylim=c(0, 0.15))
polygon(y=c(colQuantiles(bs.table.hcw, probs=0.025, na.rm=T), rev(colQuantiles(bs.table.hcw, probs=0.975, na.rm=T))),
        x=c(1:46, rev(1:46)), col=transp("firebrick", 0.3), border=NA)

# B|3 additional Qs on turnover
table(hcw.data$break.) # 270 (88,8%) without break, 294 (96,7%) with max 6mo break, 10 (3,3%)
table(hcw.data$stay_6mo) # 224 (73,4%) very likely, 11,8% very unlikely
table(hcw.data$stay_24mo) # 106 (34,8%) very likely
table(hcw.data$payroll) # 179 (58,9%) on payroll, 125 volunteer
table(hcw.data$full_time) # 284 (93,1%) full-time

#____________________________________________________________________________________________________________________

# C) +++++ vaccine acceptance++++++
# +++++++ 1| CRUDE ANALYSIS ++++++++
table(hcw.data$vacc_op) # 232 good opinion, 20 changed mind with info, 41 dont know, 11 negative opinion
table(hcw.data$vacc_pos) # 232 good op (76%), 72 not entirely positive
table(hcw.data$vacc_neg) # 232 good op (76%), 72 not entirely positive

# make vacc_op with 3 levels
hcw.data$vacc_op[hcw.data$vacc_op==1| hcw.data$vacc_op==2] <- 1
hcw.data$vacc_op[hcw.data$vacc_op==3] <- 2



# factors associated with acceptance
chisq.test(hcw.data$vacc_pos, hcw.data$sex) # not associated with sex
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$age_gp)) # not associated with age
chisq.test(hcw.data$vacc_pos, hcw.data$rel) # not associated with rel
chisq.test(hcw.data$vacc_pos, hcw.data$ethnic_gp) # not associated with rel
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$ethnic_gp)) # not associated with age
chisq.test(hcw.data$vacc_pos, hcw.data$district) # not associated with district
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$health_ctr_type)) # not associated with health ctr type
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$ses_gp)) # not associated with ses_gp
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$profession_group)) # not associated with prof group
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$prof_gp)) # not associated with prof group
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$profession)) # not associated with prof
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$edu_gp)) # somewhat associated with edu (but most acceptance in group 2)
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$ebola_contact_yn)) # not associated with contact
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$ebola_hcw_yn)) # associated with work exp (but negative)
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$payroll)) # associated payroll (but negative): on payroll have worse outcome
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$full_time)) # not associated with full-time
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$income_gp)) # associated with income group
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$hh_tv)) # not associated with tv
chisq.test(hcw.data$vacc_pos, as.factor(hcw.data$hh_radio)) # not associated with radio

# crude ORs
# 1. sex
crude1 <- glm(hcw.data$vacc_pos~hcw.data$sex)
summary(crude1)
exp(coef(crude1)) 
exp(confint(crude1))

# 2. age
crude2 <- glm(hcw.data$vacc_pos~relevel(as.factor(hcw.data$age_gp), ref="2"))
summary(crude2)
exp(coef(crude2)) 
exp(confint(crude2))

# Wald test
wald.test(b = coef(crude2), Sigma = vcov(crude2), Terms = 2:5)

# 3. Edu group
crude3 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$edu_gp))
summary(crude3)
exp(coef(crude3)) 
exp(confint(crude3))

# Wald test
wald.test(b = coef(crude3), Sigma = vcov(crude3), Terms = 2:3)

# 4. District
crude4 <- glm(hcw.data$vacc_pos~hcw.data$district)
summary(crude4)
exp(coef(crude4)) 
exp(confint(crude4))

# 5. Payroll
crude5 <- glm(hcw.data$vacc_pos~hcw.data$payroll)
summary(crude5)
exp(coef(crude5)) 
exp(confint(crude5))

# 6. HCW ebola
crude6 <- glm(hcw.data$vacc_pos~hcw.data$ebola_hcw_yn)
summary(crude6)
exp(coef(crude6)) 
exp(confint(crude6))

# 7. SES 
crude7 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$ses_gp))
summary(crude7)
exp(coef(crude7)) 
exp(confint(crude7))

# Wald test
wald.test(b = coef(crude7), Sigma = vcov(crude7), Terms = 2:3)

# 8. Income
crude8 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$income_gp))
summary(crude8)
exp(coef(crude8)) 
exp(confint(crude8))

# Wald test
wald.test(b = coef(crude8), Sigma = vcov(crude8), Terms = 2:3)

# 9. rel
crude9 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$rel))
summary(crude9)
exp(coef(crude9)) 
exp(confint(crude9))

# 10. ethnic group
crude10 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$ethnic_gp))
summary(crude10)
exp(coef(crude10)) 
exp(confint(crude10))

# Wald test
wald.test(b = coef(crude10), Sigma = vcov(crude10), Terms = 2:9)

# 11. prof gp
crude11 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$prof_gp))
summary(crude11)
exp(coef(crude11)) 
exp(confint(crude11))

# Wald test
wald.test(b = coef(crude11), Sigma = vcov(crude11), Terms = 2:4)

# 12. income gp
crude12 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$income_gp))
summary(crude12)
exp(coef(crude12)) 
exp(confint(crude12))

# Wald test
wald.test(b = coef(crude12), Sigma = vcov(crude12), Terms = 2:3)

# 13. full-time
crude13 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$full_time))
summary(crude13)
exp(coef(crude13)) 
exp(confint(crude13))

# 14. Ebola contact
crude14 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$ebola_contact_yn))
summary(crude14)
exp(coef(crude14)) 
exp(confint(crude14))

# 15. TV
crude15 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$hh_tv))
summary(crude15)
exp(coef(crude15)) 
exp(confint(crude15))

# 16. Radio
crude16 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$hh_radio))
summary(crude16)
exp(coef(crude16)) 
exp(confint(crude16))

# 17. break?/num hc? risks?

#18. urban
table(hcw.data$urban, hcw.data$vacc_pos)
crude18 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$urban))
summary(crude18)
exp(coef(crude18)) 
exp(confint(crude18))


# +++++++ 2| ADJUSTED  regression (for positive attitude on vacc: vacc_pos) ++++++
mod1 <- glm(hcw.data$vacc_pos ~ hcw.data$sex + relevel(as.factor(hcw.data$age_gp), ref = "2") + as.factor(hcw.data$prof_gp) +  as.factor(hcw.data$edu_gp)  + hcw.data$payroll + as.factor(hcw.data$income_gp) + hcw.data$ebola_hcw_yn, family=binomial(link='logit'))
summary(mod1)
exp(coef(mod1)) # exponentiated coefficients
exp(confint(mod1))
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 3:6)
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 7:9)
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 10:11)
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 13:14)

# corrplot
vars <- c("hcw.data$sex", "hcw.data$age_gp", "hcw.data$prof_gp", "hcw.data$edu_gp", "hcw.data$payroll" +" hcw.data$income_gp" + "hcw.data$ebola_hcw_yn")
# correlations <- cor(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)])
# corrplot(correlations, method="circle")


# MULTINOMIAL LOGRn
# associations with vacc_op
chisq.test(as.factor(hcw.data$vacc_op), hcw.data$sex) # not associated with sex
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$age_gp)) # not associated with age
chisq.test(hcw.data$vacc_op, hcw.data$rel) # not associated with rel
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$ethnic_gp)) # not associated with ethnic gp
chisq.test(hcw.data$vacc_op, hcw.data$district) # strongly//not associated with district
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$health_ctr_type)) # not associated with health ctr type
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$ses_gp)) # weakly associated with ses_gp
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$prof_gp)) # not associated with prof group
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$profession)) # not associated with prof
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$edu_gp)) # not associated with edu (but most acceptance in group 2)
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$ebola_contact_yn)) # not associated with contact
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$ebola_hcw_yn)) # weakly associated with work exp (but negative)
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$payroll)) # associated payroll (but negative): on payroll have worse outcome
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$full_time)) # not associated with full-time
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$income_gp)) # associated with income group
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$hh_tv)) # weakly associated with tv
chisq.test(hcw.data$vacc_op, as.factor(hcw.data$hh_radio)) # not associated with radio

hcw.data$vacc_op[hcw.data$vacc_op==1|hcw.data$vacc_op==2] <-1
hcw.data$vacc_op[hcw.data$vacc_op==3] <- 2

hcw.data$age_3ct <- NA
hcw.data$age_3ct[hcw.data$age_gp==1 | hcw.data$age_gp==2] <- 1
hcw.data$age_3ct[hcw.data$age_gp==3] <- 2
hcw.data$age_3ct[hcw.data$age_gp==4 | hcw.data$age_gp==5] <- 3


multinom.mod <- multinom(hcw.data$vacc_op ~  as.factor(hcw.data$income_gp)  +  hcw.data$payroll + hcw.data$ebola_hcw_yn)
summary(multinom.mod)
z <- summary(multinom.mod)$coefficients/summary(multinom.mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


# MULTINOMIAL REGRESSION IDEAL BUT Sample size not big enough


# by different variables 


# scatterplots (too many for now)
pairs(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34)], col=hcw.data$vacc_pos)



# additional Q: have you been vaccinated already?