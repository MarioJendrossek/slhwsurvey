# Vaccine acceptance
require(RColorBrewer)
require(nnet)
require(aod)

# prepare color pallettes
display.brewer.pal(n = 3, name = 'RdBu')
display.brewer.pal(n = 5, name = 'RdBu')
pal3 <- brewer.pal(n = 3, name = "RdBu")
pal4 <- brewer.pal(n = 4, name = "RdBu")
pal5 <- brewer.pal(n = 5, name = "RdBu")
pal6 <- brewer.pal(n = 6, name = "RdBu")

# C) +++++ vaccine acceptance++++++
# +++++++ 1| CRUDE ANALYSIS ++++++++
table(hcw.data$vacc_op) # 232 good opinion, 20 changed mind with info, 41 dont know, 11 negative opinion
table(hcw.data$vacc_pos) # 232 good op (76%), 72 not entirely positive

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

#19. Hc type
table(hcw.data$hc_type_gp, hcw.data$vacc_pos)
crude19 <- glm(hcw.data$vacc_pos~as.factor(hcw.data$hc_type_gp))
summary(crude19)
exp(coef(crude19)) 
exp(confint(crude19))
wald.test(b = coef(crude19), Sigma = vcov(crude19), Terms = 2:4)

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
#vars <- c("hcw.data$sex", "hcw.data$age_gp", "hcw.data$prof_gp", "hcw.data$edu_gp", "hcw.data$payroll" +" hcw.data$income_gp" + "hcw.data$ebola_hcw_yn")
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


multinom.mod <- multinom(hcw.data$vacc_op ~  hcw.data$sex + relevel(as.factor(hcw.data$age_gp), ref="2") + as.factor(hcw.data$income_gp)  +  hcw.data$payroll + hcw.data$ebola_hcw_yn)
summary(multinom.mod)
z <- summary(multinom.mod)$coefficients/summary(multinom.mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


# MULTINOMIAL REGRESSION IDEAL BUT Sample size not big enough