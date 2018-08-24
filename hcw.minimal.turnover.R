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
    edu_gp,
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
  write_csv("Figures\\distribution_parameters.csv")


x_values <- seq(0,
                ceiling(max(hcw.data$duration_hcw, na.rm=T)),
                by = 1.25) + 0.5

# plot Gamma distribution of durations with uncertainty
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

list(`pdf` = "pdf",
     `png` = "png") %>%
  map(~ggsave(filename = paste("Figures\\Figure_2_duration",.x, sep="."),
              width = 15, height = 7.5, units = "cm",
              dpi = 600,
              device = .x,
              plot = p_duration))

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

# drop variables not explaining variation
glm_dur_job_step <- MASS::stepAIC(glm_dur_job, trace = FALSE)
glm_dur_hcw_step <- MASS::stepAIC(glm_dur_hcw, trace = FALSE)

diagnostics <- FALSE

if (diagnostics){
  
  # make plots showing goodness of fit
  ecdf_predictions(glm_dur_job) + xlab("Duration of current job (years)")
  ecdf_predictions(glm_dur_hcw) + xlab("Duration of career as health care worker (years)")
  
  # # analysis of deviance tables
  # anova(glm_dur_job)
  # anova(glm_dur_hcw)
  
  # new goodness of fit plots showing
  ecdf_predictions(glm_dur_job_step) + xlab("Duration of current job (years)")
  ecdf_predictions(glm_dur_hcw_step) + xlab("Duration of career as health care worker (years)")
  
  # summaries of confidence intervals
  tidy(glm_dur_job_step, conf.int=T)
  tidy(glm_dur_hcw_step, conf.int=T)
}


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
  write_csv("Figures\\Table_4_Duration_Parameters.csv")


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

list(`pdf` = "pdf",
     `png` = "png") %>%
  map(~ggsave(filename = paste("Figures\\Figure_3_Duration_Parameters",.x, sep="."),
              width = 15, height = 7.5, units = "cm",
              dpi = 600,
              device = .x,
              plot = p_parameters))

# remember that the link function for the gamma family is the inverse
# i.e. g(mu) = 1/mu
# so a negative parameter indicates an increase in outcome
# and a positive parameter indicates a decrease in outcome


# C| density function gamma
set.seed(2018)

HCW_values <- hcw.data %>%
  pull(duration_hcw) %>%
  na.omit %>%
  MASS::fitdistr(.,
                 densfun = "gamma") 

HCW_samples <- mvtnorm::rmvnorm(n = 1000,
                                mean = HCW_values$estimate,
                                sigma = HCW_values$vcov) %>%
  as.data.frame

parms_mat <- expand.grid(acceptance = c(0.763, 0.964),
                         waning = c(0, 0.1),
                         reach = 1,
                         efficacy = c(1, 0.75),
                         time = seq(0,20, by = 0.25))

coverage <- function(samples, parms){
  
  bind_cols(samples, parms[rep(1, nrow(samples)),]) %>%
    dplyr::mutate(density_hcw = pgamma(q = time,
                                       shape = .$shape,
                                       rate = .$rate)) %>%
    dplyr::mutate(acceptance_sampled = rbeta(
      n = nrow(.),
      shape1 = .$acceptance*304,
      shape2 = (1 - .$acceptance)*304)) %>%
    dplyr::mutate(
      coverage = 
        acceptance_sampled*reach*efficacy*((1-waning)^time)*(1-density_hcw)) %>%
    return
}

HCW_simulation <- parms_mat %>%
  dplyr::mutate(row = 1:n()) %>%
  dplyr::mutate(label = 
                  case_when(
                    acceptance == 0.964 & efficacy == 1 ~ "A",
                    acceptance == 0.964 & efficacy == 0.75 ~ "B",
                    acceptance == 0.763 & efficacy == 1 ~ "C",
                    acceptance == 0.763 & efficacy == 0.75 ~ "D")) %>%
  split(.$row) %>%
  purrr::map_df(~coverage(HCW_samples, .x), id="row") %>%
  dplyr::mutate(efficacy = case_when(
    efficacy == 1 ~ "High efficacy (100%)",
    TRUE ~ "Low efficacy (75%)"),
    acceptance = case_when(
      acceptance == 0.964 ~ "High acceptance (96.4%)",
      TRUE ~ "Low acceptance (76.3%)"),
    Waning = case_when(
      waning == 0 ~ "Low (0%)",
      waning == 0.1 ~ "Medium (10%)",
      TRUE ~ "High (50%)"),
    Waning = fct_inorder(Waning))

p_simulation <- HCW_simulation %>% 
  dplyr::filter(time <= 10) %>%
  group_by(acceptance, Waning, reach, efficacy, time, label) %>%
  dplyr::summarise(lo = quantile(coverage, 0.025),
                   med = median(coverage),
                   hi = quantile(coverage, 0.975)) %>%
  ungroup %>%
  dplyr::mutate(Waning = fct_rev(Waning)) %>%
  ggplot(data=., aes(x=time)) +
  geom_ribbon(aes(ymin = lo, ymax = hi,
                  fill = Waning),
              alpha = 0.25) + 
  geom_line(aes(y = med,
                color = Waning)) +
  facet_wrap( ~ label) +
  theme_bw() +
  theme(legend.position="bottom", 
        strip.text = element_blank()) +
  ylab("Immunisation coverage") +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  xlab("Time since vaccination campaign (years)") +
  geom_text(data = data.frame( x = 10, y=1, label=LETTERS[1:4]),
            aes(label = label,
                x = x,
                y = y))

list(`pdf` = "pdf",
     `png` = "png") %>%
  map(~ggsave(filename = paste("Figures\\Figure_4_Simulated_Coverage",.x, sep="."),
              width = 15, height = 15, units = "cm",
              dpi = 600,
              device = .x,
              plot = p_simulation))
