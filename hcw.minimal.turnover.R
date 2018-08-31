# script contains the code necessary to replicate the analysis of the paper

# Turnover script
set.seed(91)
library(conflicted)
library(tidyverse)
library(RColorBrewer)
library(boot)
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
  
}

# urban is stored as 0/1 instead of 1/2
# convert it and then turn likely factors into actual factors
# num_hc is a count variable though
hcw.data %<>% 
  dplyr::mutate(urban = as.integer(urban + 1)) %>%
  dplyr::mutate_if(.predicate = likely_factor,
                   .funs = factor) %>%
  dplyr::mutate(num_hc = parse_integer(num_hc))

# select which variables we need for the regression

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
    #edu_gp, closely related to income and profession
    income_gp,
    payroll,
    ethnic_gp,
    hc_type_gp,
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
  inner_join(hcw.data) %>%
  dplyr::select(X1, duration_hcw, duration_job)



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
    tidy(.x, conf.int = TRUE),
    as.data.frame(confint(.x))),
    .id="Duration") %>%
  dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                             estimate, `2.5 %`, `97.5 %`),
                Parameter = case_when(term == "shape" ~ "alpha",
                                      term == "rate" ~ "beta")) %>%
  dplyr::select(Duration, Parameter, `Value (95% CI)` = CI) %>%
  write_csv("Figures\\Table_2.5_Duration_Parameters.csv")

# stratify by type of job

duration_parameters <- 
  hcw.data %>%
  dplyr::select(`In current job` = duration_job, 
                `As health care worker` = duration_hcw,
                `Profession` = prof_gp) %>%
  tidyr::gather(key, value, -Profession) %>%
  na.omit %>%
  split(list(.$key, .$Profession)) %>%
  purrr::map(~MASS::fitdistr(.x$value,
                             densfun = "gamma"))


duration_parameters %>%
  purrr::map_df(
    ~bind_cols(tidy(.x),
               as.data.frame(confint(.x))),
    .id="Duration") %>%
  separate(Duration, into=c("Duration", "prof_gp"), sep="\\.") %>%
  ggplot(data=., aes(x = prof_gp, y=estimate)) +
  geom_pointrange(aes(ymin = `2.5 %`,
                      ymax = `97.5 %`)) +
  facet_grid(Duration ~ term)

# 

x_values <- seq(0,
                ceiling(max(hcw.data$duration_hcw, na.rm=T)),
                by = 1.25) + 0.5

# plot Gamma distribution of durations with uncertainty
duration_bounds <- duration_parameters %>%
  purrr::map(# sample parameters from MLE estimate
    ~mvtnorm::rmvnorm(n = 100,
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
  geom_ribbon(data = duration_bounds,
              aes(x=x, ymin = ymin, ymax = ymax),
              color=NA, fill="lightskyblue", alpha=0.5) +
  geom_line(data= distribution_bounds,
            aes(x=x, y=y),
            lty=2)

list(`pdf` = "pdf",
     `png` = "png") %>%
  purrr::map(~ggsave(filename = paste("Figures\\Figure_2_duration",.x, sep="."),
                     width = 15, height = 7.5, units = "cm",
                     dpi = 600,
                     device = .x,
                     plot = p_duration))

# sam's modelling





# are there any sex-profession interactions?
interaction <- FALSE

if (interaction){
  glm_dur_job <- glm(data=hcw.data.forreg,
                     formula = duration_job ~ 
                       . - duration_hcw + prof_gp*sex,
                     family = "Gamma")
  
  glm_dur_hcw <- glm(data=hcw.data.forreg,
                     formula = duration_hcw ~ 
                       . - duration_job + prof_gp*sex,
                     family = "Gamma")
} else {
  glm_dur_job <- glm(data=hcw.data.forreg,
                     formula = duration_job ~ . - duration_hcw,
                     family = Gamma())
  
  glm_dur_hcw <- glm(data=hcw.data.forreg,
                     formula = duration_hcw ~ . - duration_job,
                     family = "Gamma")
}

# drop variables not explaining variation
glm_dur_job_step <- MASS::stepAIC(glm_dur_job, trace = FALSE)
glm_dur_hcw_step <- MASS::stepAIC(glm_dur_hcw, trace = FALSE)

diagnostics <- FALSE


make_predictions <- function(object){
  object$model %>%
    dplyr::mutate(pred = predict.glm(object,
                                     type = "response")) %>%
    dplyr::select(Fitted = pred) %>%
    dplyr::mutate(Observed = object$y) %>%
    return
}



plot_ecdfs <- function(x){
  x %>%
    purrr::map_df(~make_predictions(.x) %>%
                    dplyr::mutate(row = 1:n()),
                  .id="Model") %>%
    tidyr::spread(Model, Fitted) %>%
    tidyr::gather(key = source, value = value, -row) %>%
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


if (diagnostics){
  
  # make plots showing goodness of fit
  job_plots <- list(`Full` = glm_dur_job,
                    `Restricted` = glm_dur_job_step) %>%
    plot_ecdfs(x= .) +
    xlab("Duration in current job")
  
  hcw_plots <-list(`Full` = glm_dur_hcw,
                   `Restricted` = glm_dur_hcw_step) %>%
    plot_ecdfs(x= .) +
    xlab("Duration as health care worker")
  
  dev.new()
  gridExtra::grid.arrange(job_plots, hcw_plots, ncol=1)
  
  # summaries of confidence intervals
  tidy(glm_dur_job_step, conf.int=T)
  tidy(glm_dur_hcw_step, conf.int=T)
}


model_list <- list(`As health care worker` = glm_dur_hcw_step,
                   `In current job` = glm_dur_job_step)

model_output_postprocessor <- function(x){
  
  # x a tidy data frame output by a model
  
  
  
  if (sum(grepl(pattern = "sex", x = x$term)) > 1){
    x %<>% mutate(Sex = case_when(grepl(pattern = "sex", x = term) ~ "Male",
                                  TRUE ~ "Baseline"),
                  term = gsub(pattern = "sex2:",
                              fixed = T,
                              replacement = "",
                              x = term)
    )
  }
  
  dplyr::mutate(x,
                term = gsub(pattern = "(\\(|\\))", 
                            replacement = "",
                            x = term),
                term = gsub(pattern = "payroll",
                            fixed = T,
                            replacement = "Volunteer",
                            x = term),
                term = gsub(pattern = "age_gp",
                            fixed = T,
                            replacement = "Age group ",
                            x = term),
                term = gsub(pattern = "prof_gp",
                            fixed = T,
                            replacement = "Profession group ",
                            x = term),
                term = gsub(pattern = "(?<! )2$",
                            replacement = "\\1", perl=T,
                            x = term),
                term = stringr::str_to_title(term),
                
                term = fct_inorder(term)
  ) %>%
    return(.)
  
}

mod_parameters <- model_list %>%
  purrr::map(~tidy(.x, conf.int=T)) %>%
  purrr::map_df(~model_output_postprocessor(.x),
                .id="Outcome") %>%
  dplyr::mutate(term = fct_inorder(term)) %>%
  dplyr::rename(Term = term, 
                Estimate = estimate) %>%
  dplyr::mutate(Term = fct_recode(Term,
                                  "25-34" = "Age Group 2",
                                  "35-44" = "Age Group 3",
                                  "45-54" = "Age Group 4",
                                  "55+"   = "Age Group 5",
                                  "Male" = "Sex")) 

mod_parameters %>%
  dplyr::select(one_of(c("Outcome", "Term", "conf.low", "conf.high", 
                         "Estimate", "Sex"))) %>%
  dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                             Estimate, conf.low, conf.high)) %>%
  dplyr::select(-c(Estimate, conf.low, conf.high)) %>%
  write_csv("Figures\\Table_4_Duration_Parameters.csv")


p_parameters <- ggplot(data=mod_parameters,
                       aes(x=Term, y=Estimate,
                           color = Outcome)) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high),
                  position = position_dodge(width=0.5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  geom_hline(yintercept = 0, lty=2) +
  ylab("Parameter value\n(link scale)") +
  xlab("Parameter label")

list(`pdf` = "pdf",
     `png` = "png") %>%
  purrr::map(~ggsave(filename = paste("Figures\\Figure_3_Duration_Parameters",.x, sep="."),
                     width = 15, height = 7.5, units = "cm",
                     dpi = 600,
                     device = .x,
                     plot = p_parameters))

# remember that the link function for the gamma family is the inverse
# i.e. g(mu) = 1/mu
# so a negative parameter indicates an increase in outcome
# and a positive parameter indicates a decrease in outcome

# let's make predictions and scale by the baseline to see how the parameters affect what's going on

make_duration_newdata <- function(obj){
  
  # obj a model
  
  # extract the unique values of the explanatory variables
  # then use expand.grid to get all combinations
  x <- obj$model %>%
    dplyr::select(-1) %>%
    dplyr::distinct(.) %>%
    c(tidyselect::vars_select(names(.))) %>%
    purrr::map(~levels(.x)) %>%
    expand.grid %>%
    dplyr::mutate(row = 1:n())
  
  # how many actual variables are there?
  n_c <- ncol(x) - 1
  
  # drop rows from the data frame when they don't represent
  # a change from baseline for only one variable, or the
  # baseline itself
  #
  # nb this will break if we have interactions in the model
  x %>%
    tidyr::gather(key, value, -row) %>%
    group_by(row) %>%
    dplyr::summarise(n = sum(value == 1)) %>%
    dplyr::filter(n >= n_c - 1) %>%
    inner_join(x) %>%
    dplyr::arrange(row) %>%
    dplyr::select(-row) %>%
    return
  
}


purrr::map_df(.x = model_list,
              .f = ~dplyr::bind_cols(
                make_duration_newdata(.x), 
                predict(.x,
                        newdata = make_duration_newdata(.x), 
                        type = "response", 
                        se.fit = T) %>%
                  data.frame),
              .id = "Duration") %>%
  # calculate confidence intervals
  dplyr::mutate(lwr = fit - 1.96*se.fit,
                upr = fit + 1.96*se.fit,
                base = fit[1]) %>%
  # scale by baseline
  dplyr::mutate(fit = fit/base,
                lwr = lwr/base,
                upr = upr/base) %>%
  # drop the intercept
  dplyr::filter(n < max(n)) %>%
  # combine the estimate and confidence bounds into one cell
  dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                             fit, lwr, upr)) %>%
  dplyr::select(-n, -base, -lwr, -upr) %>%
  # and now filter out everything that is only 1s
  gather(variable, value, -Duration, -CI) %>%
  dplyr::filter(value != 1) %>%
  # and now relabel so that they're human friendly names
  dplyr::mutate(Variable = case_when(variable == "sex" ~ "Male",
                                     variable == "payroll" ~ "Volunteer",
                                     variable == "urban" ~ "Urban",
                                     variable == "age_gp" ~ case_when(
                                       value == "2" ~ "25-34",
                                       value == "3" ~ "35-44",
                                       value == "4" ~ "45-54",
                                       value == "5" ~ "55+"
                                     ))) %>%
  # reshape to write as a wide format table
  dplyr::select(Duration, Variable, CI) %>%
  dplyr::mutate(Variable = fct_inorder(Variable)) %>%
  tidyr::spread(Duration, CI) %>% 
  write_csv(x = ., path = "Figures/Table_4_Odds_Ratio_for_Gamma_Regression.csv")
