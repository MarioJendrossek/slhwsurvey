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