# what about the main GLM modelling of sentiment?
# make sure you've run hcw.minimal.turnover.R

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
    hc_type_gp ,
    full_time
  ) %>%
  na.omit


sentiment <- glm(data = hcw.data.forsent,
                 vacc_pos ~ .,
                 family = binomial())

# variable selection
sentiment_step <- MASS::stepAIC(sentiment, 
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

sentiment_intercept <- tidy(sentiment_restricted, conf.int=T) %>%
  dplyr::filter(term == "(Intercept)") %>%
  dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                   .funs = boot::inv.logit) %>%
  dplyr::transmute(`Baseline probability` = sprintf("%0.2f (%0.2f, %0.2f)",
                                                    estimate, conf.low, conf.high))

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
  dplyr::select(Term = term, `Odds ratio`) %>%
  cbind(., sentiment_intercept) %>%
  dplyr::select(Term, `Baseline probability`, `Odds ratio`) %>%
  write_csv("Figures/Table_3_GLM_Odds_ratios.csv")


