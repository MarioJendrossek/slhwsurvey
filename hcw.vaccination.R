# Vaccine acceptance
require(RColorBrewer)
require(nnet)
require(aod)

# we're carrying the data over from hcw.minimal.turnover.R

# C) +++++ vaccine acceptance++++++
# +++++++ 1| CRUDE ANALYSIS ++++++++
table(hcw.data$vacc_op) # 232 good opinion, 20 changed mind with info, 41 dont know, 11 negative opinion
table(hcw.data$vacc_pos) # 232 good op (76%), 72 not entirely positive

# tidy counting
dplyr::count(hcw.data, vacc_op)
dplyr::count(hcw.data, vacc_pos)

# make vacc_op with 3 levels
# hcw.data$vacc_op[hcw.data$vacc_op==1| hcw.data$vacc_op==2] <- 1
# hcw.data$vacc_op[hcw.data$vacc_op==3] <- 2

# factors associated with acceptance
hcw.factors <- c("sex",
                 "age_gp",
                 "rel",
                 "ethnic_gp",
                 "district",
                 "health_ctr_type",
                 "ses_gp",
                 "prof_gp",
                 "profession",
                 "urban",
                 "edu_gp",
                 "ebola_contact_yn",
                 "ebola_hcw_yn",
                 "payroll",
                 "hc_type_gp",
                 "full_time",
                 "income_gp",
                 "hh_tv",
                 "hh_radio")

hcw.data.long <- 
  hcw.data %>%
  dplyr::select(one_of(hcw.factors), vacc_pos) %>%
  mutate(row = row_number()) %>%
  tidyr::gather(key, value, -row, -vacc_pos)

# chi square tests
hcw.data.long %>%
  na.omit %>%
  split(.$key) %>%
  purrr::map(~chisq.test(.$vacc_pos, .$value)) %>%
  purrr::map_df(~data.frame(p.value = .x$p.value), .id="key") %>%
  dplyr::arrange(p.value)

# crude ORs

hcw.glm.data.long <- hcw.data.long %>%
  na.omit %>%
  split(.$key) %>% 
  purrr::map(~glm(family = binomial(),
                  data = .x, 
                  vacc_pos ~ value)) %>%
  purrr::map_df(~tidy(.x, conf.int=T), .id="key") %>%
  dplyr::select(key, term, estimate, p.value, conf.low, conf.high) %>%
  tidyr::gather(parameter, value, -c(key, term, p.value)) %>%
  dplyr::mutate(value = case_when(term == "(Intercept)" ~ inv.logit(value),
                                  TRUE ~ exp(value))) %>%
  tidyr::spread(parameter, value) %>%
  dplyr::mutate(value = sprintf("%.2f (%.2f, %.2f)", 
                                estimate, conf.low, conf.high)) %>%
  dplyr::select(key, term, p.value, value)

# convert the intercepts to a small data frame
hcw.glm.data.long.intercepts <- 
  dplyr::filter(hcw.glm.data.long, term == "(Intercept)") %>%
  dplyr::rename(`Baseline probability` = "value") %>%
  dplyr::select(-p.value, -term)

# convert the effects to something we can join with the intercepts
hcw.glm.data.long.effects <- 
  dplyr::filter(hcw.glm.data.long, term != "(Intercept)") %>%
  dplyr::mutate(term = gsub(pattern = "value", 
                            replacement = "",
                            x = term))

first_only <- function(x){
  if (!all(x == x[1])){stop("Not all elements the same")}
  x[-1] <- ""
  return(x)
}

# join together, fix formatting
inner_join(hcw.glm.data.long.intercepts, 
           hcw.glm.data.long.effects) %>%
  dplyr::mutate(p.value = sprintf("%.3f", p.value)) %>%
  dplyr::select(`Covariate` = key,
                `Baseline probability`, 
                Level = term, 
                `Odds ratio` = value,
                `p value` = p.value) %>%
  split(.$Covariate) %>%
  purrr::map_df(~dplyr::mutate_at(.x,
                               .vars = vars(Covariate,
                                            `Baseline probability`),
                               .funs = first_only)) %>%
  write_csv("Figures\\oddsratios.csv")

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