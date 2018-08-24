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
  dplyr::mutate(term = rownames(.))

boot_cis <- data.frame(x=1:4) %>%
  split(.$x) %>%
  purrr::map(~boot.ci(boot_fit, index = .x$x)) %>% 
  purrr::map("normal") %>%
  purrr::map_df(~as.data.frame(.x)) 

# should really make this term clean-up into a function
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


# do we need regularised logistic regression?
# standard errors will be a bit off but it can at least help identify what's
# going on

library(glmnet)
X_net <-
  dplyr::select(hcw.data.forsent,
                -vacc_pos) %>%
  data.matrix

y_net <- hcw.data.forsent$vacc_pos

sentiment_glmnet <- glmnet(x = X_net,
                           y = y_net,
                           family = "binomial")

tidy(sentiment_glmnet) %>%
  dplyr::filter(step == max(step))


lambda_net <- cv.glmnet(X_net, y_net)$lambda.min


glance(sentiment_glmnet)
coef(sentiment_glmnet, s = lambda_net)


predict(object = sentiment_glmnet,
        newx = X_net, 
        type = "response",
        s = lambda_net) %>%
  data.frame(pred = as.numeric(.),
             obs = hcw.data.forsent$vacc_pos) %>%
  ggplot(data=., aes(x=factor(obs), y=pred)) +
  geom_boxplot()

# regularised logistic regression is bad here



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

hcw.data %>%
  dplyr::select(one_of(hcw.factors), vacc_op) %>%
  mutate(row = row_number()) %>%
  tidyr::gather(key, value, -row, -vacc_op) %>%
  na.omit %>%
  split(.$key) %>%
  purrr::map(~chisq.test(.$vacc_op, .$value)) %>%
  purrr::map_df(~data.frame(p.value = .x$p.value), .id="key") %>%
  dplyr::arrange(p.value)

# income seems to be the only variable potentially associated


multinom.mod <- multinom(hcw.data$vacc_op ~  hcw.data$sex + relevel(as.factor(hcw.data$age_gp), ref="2") + as.factor(hcw.data$income_gp)  +  hcw.data$payroll + hcw.data$ebola_hcw_yn)
summary(multinom.mod)
z <- summary(multinom.mod)$coefficients/summary(multinom.mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


# MULTINOMIAL REGRESSION IDEAL BUT Sample size not big enough