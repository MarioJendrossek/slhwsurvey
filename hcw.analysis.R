#require(manyregs)
require(corrplot)
require(fitdistrplus)
require(ggplot2)

# hcw analysis

# descriptive statitics (for table 1)
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


# Descriptive figures: 
# age distribution
ggplot(hcw.data, aes(x=age_gp)) + geom_histogram()

# type of health centre // type of HC by district
bp <- ggplot(hcw.data, aes(x="", y=health_ctr_type, fill=group)) + 
  geom_bar(width=1, stat="identity")
pie <- bp + coord_polar("y", start=0)

# by district
par(mfrow=c(1,2))
pie(table(hcw.data$health_ctr_type[hcw.data$district==1]))
pie(table(hcw.data$health_ctr_type[hcw.data$district==2]))

# 
par(mfrow=c(1,2))
pie(table(hcw.data$edu_gp[hcw.data$district==1]))
pie(table(hcw.data$edu_gp[hcw.data$district==2]))

#_________________________________________________________________________________________________________________________

# A) ++++++HCW turnover+++++
# histogram of duration of current job
hist(hcw.data$duration_job, breaks=40) # geometric distribution?
mean(hcw.data$duration_job, na.rm=T) # average 6.26y
median(hcw.data$duration_job, na.rm=T) # median 4.62


# create dataset for exponential regression
# x num of years, y people in that group
df <- data.frame(x=numeric(length=37L), y=numeric(length=37L))
for (i in 1:37) {
  df$x[i] <- i
  df$y[i] <- sum(hcw.data$duration_job<=i & hcw.data$duration_job>i-1, na.rm=T)
}
df$y[is.na(df$y)] <- 0
  
model_rate <- lm(log(df$y) ~ df$x)
summary(model_rate)
# histogram of duration of hcw work
hist(hcw.data$duration_hcw, breaks=40) # neg binomial?
mean(hcw.data$duration_hcw, na.rm=T) # average 10.9y
median(hcw.data$duration_hcw, na.rm=T) # median 8y


# plot the histogram
hist(hcw.data$duration_hcw, prob=TRUE, breaks=40)

# fit the negative binomial distribution
fit1 <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "lnorm")
fit2 <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "gamma")
fit3 <- fitdist(hcw.data$duration_hcw[!is.na(hcw.data$duration_hcw) & hcw.data$duration_hcw>0], "weibull")
# fit4 <- neg binomial (with duration in y)


# get the fitted densities. mu and size from fit.
fitD1 <- dlnorm(0:45, meanlog=1.99, sdlog=1.03)
fitD2 <- dgamma(0:45, shape=1.39, rate=0.13)
fitD3 <- dweibull(0:45, shape=1.22, scale=11.71)

# add fitted line (blue) to histogram
lines(fitD1, lwd="3", col="blue")
lines(fitD2, lwd="3", col="red")
lines(fitD3, lwd="3", col="green")

# compare fit
par(mfrow=c(2,2))
plot.legend <- c("lognormal", "gamma", "weibull")
denscomp(list(fit1, fit2, fit3), legendtext = plot.legend)
cdfcomp (list(fit1, fit2, fit3), legendtext = plot.legend)
qqcomp  (list(fit1, fit2, fit3), legendtext = plot.legend)
ppcomp  (list(fit1, fit2, fit3), legendtext = plot.legend)


# additional Qs on turnover
table(hcw.data$break.) # 270 (88,8%) without break, 294 (96,7%) with max 6mo break, 10 (3,3%)
table(hcw.data$stay_6mo) # 224 (73,4%) very likely, 11,8% very unlikely
table(hcw.data$stay_24mo) # 106 (34,8%) very likely
table(hcw.data$payroll) # 179 (58,9%) on payroll, 125 volunteer
table(hcw.data$full_time) # 284 (93,1%) full-time

#____________________________________________________________________________________________________________________

# B) +++++ vaccine acceptance++++++
table(hcw.data$vacc_op) # 232 good opinion, 20 changed mind with info, 41 dont know, 11 negative opinion
table(hcw.data$vacc_pos) # 232 good op (76%), 72 not entirely positive

#Log regression (for positive attitude on vacc: vacc_pos)
mod1 <- glm(hcw.data$vacc_pos ~ hcw.data$sex + as.factor(hcw.data$edu_gp) + hcw.data$rel + as.factor(hcw.data$age_gp) + hcw.data$district + as.factor(hcw.data$health_ctr_type) + as.factor(hcw.data$ses_gp) + as.factor(hcw.data$profession_group) + hcw.data$ebola_contact_yn + hcw.data$ebola_hcw_yn + hcw.data$duration_hcw,family=binomial(link='logit'))
summary(mod1)
# by different variables 

# calculate correlations between vars
correlations <- cor(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)])
corrplot(correlations, method="circle")

# scatterplots (too many for now)
pairs(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34)], col=hcw.data$vacc_pos)



# additional Q: have you been vaccinated already?