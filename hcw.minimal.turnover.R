# minimal script for Sam

# Turnover script

library(conflicted)
library(RColorBrewer)
library(boot)
library(matrixStats)
library(fitdistrplus)
library(tidyverse)
library(gamlss)
library(magrittr)
library(broom)

# read in data
hcw.data <- read_csv("HCWsurvey_limited.csv")

# many variables appear to be factors listed as numeric
# these should be coded as factors
likely_factor <- function(x){
  
  status <- FALSE
  
  if (!all(is.integer(x))){return(FALSE)}
  
  if (min(x, na.rm = T) == 1){status <- TRUE}
  
  return(status)
  
  # all(is.integer(x), na.rm = T) & (min(x, na.rm=T) ==  1)
}

hcw.data %<>% 
  dplyr::mutate(urban = as.integer(urban + 1)) %>%
  dplyr::mutate_if(.predicate = likely_factor,
                   .funs = factor) %>%
  dplyr::mutate(num_hc = parse_integer(num_hc))

hcw.data.forreg <- hcw.data %>%
  dplyr::select(
    # response
    duration_hcw,
    duration_job,
    # explanatory
    sex,
    age_gp ,
    urban,
    #num_hc, actually on causal pathway
    prof_gp,
    edu_gp,
    income_gp,
    payroll,
    ethnic_gp,
    health_ctr_type ,
    full_time
  ) %>%
  na.omit


# 1. Duration current job

# summary statistics

hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw) %>%
  tidyr::gather(key, value) %>%
  na.omit %>%
  group_by(key) %>%
  dplyr::summarise_at(.vars = vars(value),
                      .funs = funs(n = n(),
                                   Min = min,
                                   Median = median,
                                   Mean = mean,
                                   Max = max))

# who has the longest duration

hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw,
                X1) %>%
  tidyr::gather(key, value, -X1) %>%
  group_by(key) %>%
  dplyr::filter(value == max(value, na.rm=TRUE)) %>%
  ungroup %>%
  dplyr::select(X1) %>%
  inner_join(hcw.data)



# A| FIT DISTRIBUTION

# get distribution parameters and regular 
hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw) %>%
  tidyr::gather(key, value) %>%
  na.omit %>%
  split(.$key) %>%
  purrr::map(~MASS::fitdistr(.x$value,
                             densfun = "gamma")) %>%
  purrr::map_df(~bind_cols(
    tidy(.x),
    as.data.frame(confint(.x))),
    .id="Duration") %>%
  write_csv("Figures\\distribution_parameters.csv")


x_values <- seq(0,
                ceiling(max(hcw.data$duration_hcw, na.rm=T)),
                by = 1.25) + 0.5

# plot with uncertainty
distribution_bounds <- hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw) %>%
  tidyr::gather(key, value) %>%
  na.omit %>%
  split(.$key) %>%
  purrr::map(
    # estimate parameters of gamma distributions that 
    # describe the shape of the data
    ~MASS::fitdistr(.x$value,
                    densfun = "gamma")) %>%
  purrr::map(# sample parameters from MLE estimate
    ~mvtnorm::rmvnorm(n = 1000,
                      mean = .x$estimate,
                      sigma = .x$vcov)) %>%
  purrr::map_df(
    .id = "key",
    # calculate densities with sampled parameters
    ~data.frame(.x) %>%
      dplyr::mutate(row = 1:nrow(.)) %>%
      split(.$row) %>% # we may have been able to use nest() here
      purrr::map_df(
        # for each set of sampled parameters, what is the density?
        .id="row", 
        ~data.frame(x = x_values) %>%
          dplyr::mutate(y = 
                          dgamma(x = x,
                                 shape = .x$shape,
                                 rate = .x$rate)
          ) # end calculation of density
      ) 
  ) %>%
  group_by(key, x) %>% # calculate quantiles of draws of distributions
  dplyr::summarise(ymin = quantile(y, 0.025),
                   ymax = quantile(y, 0.975),
                   y = median(y))

# regular histogram, scaled as density rather than count
p_duration <- hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw) %>%
  tidyr::gather(key, value) %>%
  na.omit %>%
  ggplot(data = ., aes(x=value)) +
  geom_histogram(binwidth=2.5, 
                 center = 1.25,
                 color = "black",
                 fill = "grey90",
                 aes(y=..density..)) +
  facet_grid(. ~ key) +
  theme_bw() +
  xlab("Duration of employment (years)")

# add on the uncertainty bounds
p_duration <- p_duration +
  geom_ribbon(data = distribution_bounds,
              aes(x=x, ymin = ymin, ymax = ymax),
              color=NA, fill="lightskyblue", alpha=0.5) +
  geom_line(data= distribution_bounds,
            aes(x=x, y=y),
            lty=2)

ggsave(filename = "Figures\\duration.png",
       width = 15, height = 7.5, units = "cm", dpi = 600,
       plot = p_duration)


if (FALSE){ 
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
  mod_exp_sex <- lm(data = hcw.data,
                    log(duration_job) ~ factor(sex),  na.action = "na.omit")
  
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
  
  # a) lognormal regression
  
  # model
  lognormalmodel <- gamlss(data = hcw.data.forreg,
                           duration_hcw ~ district + sex + full_time, 
                           family=LOGNO)
  
  summary(lognormalmodel)
  
  
  # b) gamma regression
  
  # bivariate
  #sex
  mod_gam_sex <- glm(data = hcw.data.forreg,
                     duration_hcw ~ sex, 
                     family = "Gamma", 
                     na.action = "na.omit")
  
  summary(mod_gam_sex)
  exp(coef(mod_gam_sex))
  # not associated
  
  # univariate
  hcw.data.forreg %>%
    dplyr::select(-duration_job)
  
  
  
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
}

# sam's modelling

glm_dur_job <- glm(data=hcw.data.forreg,
                   formula = duration_job ~ . - duration_hcw,
                   family = "Gamma")

glm_dur_hcw <- glm(data=hcw.data.forreg,
                   formula = duration_hcw ~ . - duration_job,
                   family = "Gamma")

ecdf_predictions <- function(object){
  
  object$model %>%
    dplyr::mutate(pred = predict.glm(object,
                                     type = "response")) %>%
    dplyr::select(Fitted = pred) %>%
    dplyr::mutate(Observed = object$y) %>%
    tidyr::gather(key = source, value = value) %>%
    ggplot(data=., aes(x=value)) +
    stat_ecdf(aes(color=factor(source))) +
    scale_x_log10() +
    scale_color_brewer(palette="Set2", name="Data source") +
    ylab("ECDF") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.minor.x = element_blank()) +
    annotation_logticks(sides="bt")
  
}

# make plots showing goodness of fit
ecdf_predictions(glm_dur_job) + xlab("Duration of current job (years)")
ecdf_predictions(glm_dur_hcw) + xlab("Duration of career as health care worker (years)")

# # analysis of deviance tables
# anova(glm_dur_job)
# anova(glm_dur_hcw)

# drop variables not explaining variation
glm_dur_job_step <- stepAIC(glm_dur_job)
glm_dur_hcw_step <- stepAIC(glm_dur_hcw)

# new goodness of fit plots showing
ecdf_predictions(glm_dur_job_step) + xlab("Duration of current job (years)")
ecdf_predictions(glm_dur_hcw_step) + xlab("Duration of career as health care worker (years)")

# summaries of confidence intervals
tidy(glm_dur_job_step, conf.int=T)
tidy(glm_dur_hcw_step, conf.int=T)

mod_list <- list(`As health care worker` = glm_dur_hcw_step,
                 `In current job` = glm_dur_job_step)

mod_list %>%
  purrr::map_df(~tidy(.x, conf.int=T),
                .id="Outcome") %>%
  dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "age_gp",
                            fixed = T, replacement = "Age group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                term = fct_inorder(term)) %>%
  dplyr::select(Outcome, Term = term, 
                Estimate = estimate,
                conf.low, conf.high) %>%
  dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                             Estimate, conf.low, conf.high)) %>%
  dplyr::select(-c(Estimate, conf.low, conf.high)) %>%
  write_csv("Figures\\parameters.csv")


p_parameters <- mod_list %>%
  purrr::map_df(~tidy(.x, conf.int=T),
                .id="Outcome") %>%
  dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "age_gp",
                            fixed = T, replacement = "Age group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                term = fct_inorder(term)) %>%
  ggplot(data=., aes(x=term, y=estimate)) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high,
                      color = Outcome),
                  position = position_dodge(width=0.5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  geom_hline(yintercept = 0, lty=2) +
  ylab("Parameter value\n(link scale)") +
  xlab("Parameter label")

ggsave(filename = "Figures\\parameters.png",
       width = 15, height = 7.5, units = "cm", dpi = 600,
       plot = p_parameters)

# remember that the link function for the gamma family is the inverse
# i.e. g(mu) = 1/mu
# so a negative parameter indicates an increase in outcome
# and a positive parameter indicates a decrease in outcome


# what about the substantive modelling?

hcw.data.forsent <- hcw.data %>%
  dplyr::select(
    # response
    vacc_pos,
    # explanatory
    duration_hcw,
    duration_job,
    sex,
    age_gp ,
    urban,
    #num_hc, actually on causal pathway
    prof_gp,
    edu_gp,
    income_gp,
    payroll,
    ethnic_gp,
    health_ctr_type ,
    full_time
  ) %>%
  na.omit

sentiment <- glm(data = hcw.data.forsent,
                 vacc_pos ~ .,
                 family = binomial())

# variable selection
sentiment_step <- stepAIC(sentiment, 
                          trace = FALSE)#,
#k = log(nrow(hcw.data.forsent)))

tidy(sentiment_step, conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                   .funs = exp) %>%
  dplyr::select(-std.error, -statistic) %>%
  dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "_gp", 
                            replacement = " group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                term = gsub(pattern = "Edu",
                            replacement = "Education",
                            x = term),
                term = fct_inorder(term)) %>%
  dplyr::mutate(`Odds ratio` = sprintf("%0.2f (%0.2f, %0.2f)",
                                       estimate, conf.low, conf.high)) %>%
  dplyr::select(Term = term, `Odds ratio`)

sentiment_restricted <- glm(data = hcw.data.forsent,
                            vacc_pos ~ payroll + edu_gp,
                            family = binomial())

tidy(sentiment_restricted, conf.int=T) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                   .funs = exp) %>%
  dplyr::select(term, estimate, conf.low, conf.high ) %>%
  dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "_gp", 
                            replacement = " group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                term = gsub(pattern = "Edu",
                            replacement = "Education",
                            x = term),
                term = fct_inorder(term)) %>%
  dplyr::mutate(`Odds ratio` = sprintf("%0.2f (%0.2f, %0.2f)",
                                       estimate, conf.low, conf.high)) %>%
  dplyr::select(Term = term, `Odds ratio`)


# seems there's sensitivity to the data chosen when estimating the effect of
# payroll and education group. perhaps running a bootstrap on the regression
# can help get to the bottom

library(boot)  

logit_test <- function(d,indices) {  
  d <- d[indices,]  
  fit <- glm(vacc_pos ~ edu_gp + payroll,
             data = d, family = "binomial")  
  return(coef(fit))  
}

boot_fit <- boot(  
  data = hcw.data, 
  statistic = logit_test, 
  R = 2e3
) 

boot_estimates <- data.frame(estimate = boot_fit$t0) %>%
  mutate(term = rownames(.))

boot_cis <- data.frame(x=1:4) %>%
  split(.$x) %>%
  purrr::map(~boot.ci(boot_fit, index = .x$x)) %>% 
  purrr::map("normal") %>%
  purrr::map_df(~as.data.frame(.x)) 

bind_cols(boot_estimates, boot_cis) %>%
  dplyr::rename(conf.low = V2,
                conf.high = V3) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                   .funs = exp) %>%
  dplyr::select(term, estimate, conf.low, conf.high ) %>%
  dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "_gp", 
                            replacement = " group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                term = gsub(pattern = "Edu",
                            replacement = "Education",
                            x = term),
                term = fct_inorder(term)) %>%
  dplyr::mutate(`Odds ratio` = sprintf("%0.2f (%0.2f, %0.2f)",
                                       estimate, conf.low, conf.high)) %>%
  dplyr::select(Term = term, `Odds ratio`)
