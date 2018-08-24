# Vaccine acceptance
# require(RColorBrewer)
# require(nnet)
# require(aod)

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
# which variables might be associated?
hcw.data.long %>%
  na.omit %>%
  split(.$key) %>%
  purrr::map(~chisq.test(.$vacc_pos, .$value)) %>%
  purrr::map_df(~data.frame(p.value = .x$p.value), .id="key") %>%
  dplyr::arrange(p.value)

# crude ORs
# fit a logistic GLM to each of the variables above and see if there's a relationship

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

# want to display the table without having the intercept repeated for every row
first_only <- function(x){
  if (!all(x == x[1])){stop("Not all elements the same")}
  x[-1] <- ""
  return(x)
}

# join together, fix formatting
inner_join(hcw.glm.data.long.intercepts, 
           hcw.glm.data.long.effects) %>%
  dplyr::mutate(p.value = sprintf("%.3f", p.value)) %>%
  dplyr::rename(`Covariate` = key,
                Level = term, 
                `Odds ratio` = value,
                `p value` = p.value) %>%
  split(.$Covariate) %>%
  purrr::map_df(~dplyr::mutate_at(.x,
                               .vars = vars(Covariate,
                                            `Baseline probability`),
                               .funs = first_only)) %>%
  write_csv("Figures\\Table_2_Univariate_GLM_Odds_ratios.csv")



# check hcw.sentiment.modelling for sam's logistic regression approaches