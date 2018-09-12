## MODELLING DURATION OF EMPLOYMENT

# Turnover script
set.seed(91)

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
duration_parameters_all <- hcw.data %>%
    dplyr::select(`In current job` = duration_job, 
                  `As health care worker` = duration_hcw) %>%
    tidyr::gather(key, value) %>%
    na.omit %>%
    split(.$key) %>%
    purrr::map(~MASS::fitdistr(.x$value,
                               densfun = "gamma"))

duration_parameters_all %>%
    purrr::map_df(~bind_cols(
        tidy(.x, conf.int = TRUE),
        as.data.frame(confint(.x))),
        .id="Duration") %>%
    dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                               estimate, `2.5 %`, `97.5 %`),
                  Parameter = case_when(term == "shape" ~ "alpha",
                                        term == "rate" ~ "beta")) %>%
    dplyr::select(Duration, Parameter, `Value (95% CI)` = CI) %>%
    write_csv("Figures\\Duration_parameters_for_Gamma_distribution.csv")

# stratify by type of job
# are there differences between the duration of employment for
# each different type of worker?

duration_parameters_by_prof <- 
    hcw.data %>%
    dplyr::select(`In current job` = duration_job, 
                  `As health care worker` = duration_hcw,
                  `Profession` = prof_gp) %>%
    tidyr::gather(key, value, -Profession) %>%
    na.omit %>%
    split(list(.$key, .$Profession)) %>%
    purrr::map(~MASS::fitdistr(.x$value,
                               densfun = "gamma"))

# make a plot of each of the shape and rate parameters for
# the four profession groups for both duration of current job and total career

duration_parameters_by_prof %>%
    purrr::map_df(
        ~bind_cols(tidy(.x),
                   as.data.frame(confint(.x))),
        .id="Duration") %>%
    separate(Duration, into=c("Duration", "prof_gp"), sep="\\.") %>%
    ggplot(data=., aes(x = prof_gp, y=estimate)) +
    geom_pointrange(aes(ymin = `2.5 %`,
                        ymax = `97.5 %`)) +
    facet_grid(term ~ Duration) +
    theme_bw() +
    xlab("Profession group")

# plot Gamma distribution of durations with uncertainty

# end simulation

set.seed(25)
x_values <- seq(0,
                ceiling(max(hcw.data$duration_hcw, na.rm=T)),
                by = 1.25) + 0.5

duration_bounds_all <- duration_parameters_all %>%
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
    geom_ribbon(data = duration_bounds_all,
                aes(x=x, ymin = ymin, ymax = ymax),
                color=NA, fill="lightskyblue", alpha=0.5) +
    geom_line(data= duration_bounds_all,
              aes(x=x, y=y),
              lty=2)

list(`pdf` = "pdf",
     `png` = "png") %>%
    purrr::map(
        ~ggsave(
            filename =
                paste("Figures\\Figure_3_Duration_of_employment",
                      .x,
                      sep="."),
            width = 15, 
            height = 7.5,
            units = "cm",
            dpi = 600,
            device = .x,
            plot = p_duration))

# gamma regression models to understand the drivers of duration of employment

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

# do we need regression diagnostics?
diagnostics <- TRUE

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
    
    
    # replace sex term with more meaningful labels
    if (sum(grepl(pattern = "sex", x = x$term)) > 1){
        x %<>% mutate(Sex = case_when(grepl(pattern = "sex", x = term) ~ "Male",
                                      TRUE ~ "Baseline"),
                      term = gsub(pattern = "sex2:",
                                  fixed = T,
                                  replacement = "",
                                  x = term)
        )
    }
    
    # tidy up punctuation for other terms
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

# replace the values of the age and sex terms to be human friendly
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

# make a neat table containing the estimates from the models of duration
mod_parameters %>%
    dplyr::select(one_of(c("Outcome", "Term", "conf.low", "conf.high", 
                           "Estimate", "Sex"))) %>%
    dplyr::mutate(CI = sprintf("%0.2f (%0.2f, %0.2f)",
                               Estimate, conf.low, conf.high)) %>%
    dplyr::select(-c(Estimate, conf.low, conf.high)) %>%
    tidyr::spread(Outcome, CI) %>%
    write_csv("Figures\\SI_Table_Duration_model_parameters.csv")

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
    purrr::map(~ggsave(
        filename =
            paste("Figures\\SI_Figure_Duration_model_parameters", .x, sep="."),
        width = 15, height = 7.5, units = "cm",
        dpi = 600,
        device = .x,
        plot = p_parameters))

# remember that the link function for the gamma family is the inverse
# i.e. g(mu) = 1/mu
# so a negative parameter indicates an increase in outcome
# and a positive parameter indicates a decrease in outcome

# let's make predictions and scale by the baseline to see how the
# parameters affect what's going on

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


make_duration_newdata <- function(obj){
    
    X_names <- obj$formula %>%
        as.character %>%
        .[3] %>%
        strsplit(
            x = .,
            split = " \\+ "
        ) %>%
        unlist
    
    # extract the unique values of the explanatory variables
    # then use expand.grid to get all combinations
    
    if (any(class(obj) == "glm")) {
        
        x <- obj$model %>%
            dplyr::select(one_of(X_names)) %>%
            dplyr::distinct(.)  %>%
            purrr::map(~levels(.x)) %>%
            expand.grid %>%
            dplyr::mutate(row = 1:n())
        
    } else if (attr(attributes(obj)$class, "package") == "Zelig"){
        # obj a zelig model
        
        x <- obj$data %>%
            dplyr::select(one_of(X_names)) %>%
            dplyr::distinct(.)  %>%
            purrr::map(~levels(.x)) %>%
            expand.grid %>%
            dplyr::mutate(row = 1:n())
        
    }
    
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
        ungroup %>%
        dplyr::mutate(row = 1:n()) %>%
        return
    
}


## simulate the durations so that we can take a comparison

## this requires re-fitting with the Zelig package so that we can simulate from the fitted model

zelig_dur_hcw <- zelig(data=hcw.data.forreg,
                       formula = duration_hcw ~ age_gp + payroll,
                       model = "gamma")

# make a new data frame containing only the required values for prediction
sims_newdata_hcw_df <- make_duration_newdata(zelig_dur_hcw) 

# the setx function makes this a data object that zelig can understand
sims_newdata_hcw <- setx(zelig_dur_hcw, 
                         age_gp = sims_newdata_hcw_df$age_gp,
                         payroll = sims_newdata_hcw_df$payroll)

# perform the simulation
sims_zelig_hcw <- Zelig::sim(obj = zelig_dur_hcw,
                             num = 1000,
                             x = sims_newdata_hcw)

# we want to compare each set of simulated duration values to the baseline
sims_zelig_table_hcw <- sims_zelig_hcw$sim.out[[1]] %>%
    map("ev") %>% # extract E(Y|X)
    map_df(~data.frame(value = unlist(.x)) %>%
               dplyr::mutate(sample = 1:n()),
           .id = "row") %>% # convert to a data frame
    dplyr::mutate_at(.vars = vars(row),
                     .funs = funs(parse_number)) %>% # ensure index in numeric
    inner_join(sims_newdata_hcw_df %>%
                   dplyr::select(row)) %>% # combine with data frame
    spread(row, value) %>% # make wide so we have all next to baseline
    gather(row, value, -sample, -`1`) %>% # make baseline | contrast label | contrast value for each sample
    dplyr::mutate(ratio = value/`1`) %>% # calculate ratio of baseline and contrast
    # take quantiles of ratios
    group_by(row) %>%
    dplyr::summarise(estimate = mean(ratio),
                     `2.5 %` = quantile(ratio, 0.025),
                     `97.5 %` = quantile(ratio, 0.975)) %>% 
    dplyr::mutate_at(.vars = vars(row),
                     .funs = funs(parse_number)) 

# make nice for table output

or_predictor_hcw <- 
    inner_join(sims_newdata_hcw_df, sims_zelig_table_hcw) %>%
    dplyr::mutate(age_gp = case_when(age_gp == "2" ~ "25-34",
                                     age_gp == "3" ~ "35-44",
                                     age_gp == "4" ~ "45-54", 
                                     age_gp == "5" ~ "55+"),
                  payroll = case_when(payroll == "2" ~ "Volunteer")) %>%
    dplyr::mutate(`Odds Ratio` = sprintf("%0.2f (%0.2f, %0.2f)",
                                         estimate, `2.5 %`, `97.5 %`)) %>%
    dplyr::mutate(Effect = paste0(age_gp, payroll),
                  Effect = gsub(pattern = "NA", replacement = "",
                                x= Effect)) %>%
    dplyr::select(Effect, `As health care worker` = `Odds Ratio`)


# and now for current job
zelig_dur_job <- zelig(data=hcw.data.forreg,
                       formula = duration_job ~ age_gp + payroll + 
                           sex + urban,
                       model = "gamma")

sims_newdata_job_df <- make_duration_newdata(zelig_dur_job) 

sims_newdata_job <- setx(zelig_dur_job, 
                         age_gp = sims_newdata_job_df$age_gp,
                         payroll = sims_newdata_job_df$payroll,
                         sex = sims_newdata_job_df$sex,
                         urban = sims_newdata_job_df$urban)

sims_zelig_job <- Zelig::sim(obj = zelig_dur_job,
                             num = 1000,
                             x = sims_newdata_job)

sims_zelig_table_job <- sims_zelig_job$sim.out[[1]] %>%
    map("ev") %>%
    map_df(~data.frame(value = unlist(.x)) %>%
               dplyr::mutate(sample = 1:n()),
           .id = "row") %>%
    dplyr::mutate_at(.vars = vars(row),
                     .funs = funs(parse_number)) %>%
    inner_join(sims_newdata_job_df %>%
                   dplyr::select(row)) %>% 
    spread(row, value) %>%
    gather(row, value, -sample, -`1`) %>%
    dplyr::mutate(ratio = value/`1`) %>%
    group_by(row) %>%
    dplyr::summarise(estimate = mean(ratio),
                     `2.5 %` = quantile(ratio, 0.025),
                     `97.5 %` = quantile(ratio, 0.975)) %>%
    dplyr::mutate_at(.vars = vars(row),
                     .funs = funs(parse_number)) 


or_predictor_job <- 
    inner_join(sims_newdata_job_df, sims_zelig_table_job) %>%
    dplyr::mutate(
        age_gp = case_when(age_gp == "2" ~ "25-34",
                           age_gp == "3" ~ "35-44",
                           age_gp == "4" ~ "45-54", 
                           age_gp == "5" ~ "55+"),
        payroll = case_when(payroll == "2" ~ "Volunteer"),
        sex = if_else(sex == 1, "NA", "Male"),
        urban = if_else(urban == 1, "NA", "Urban")) %>%
    dplyr::mutate(`Odds Ratio` = sprintf("%0.2f (%0.2f, %0.2f)",
                                         estimate, `2.5 %`, `97.5 %`)) %>%
    dplyr::mutate(Effect = paste0(age_gp, payroll, sex, urban),
                  Effect = gsub(pattern = "NA", replacement = "",
                                x= Effect)) %>%
    dplyr::select(Effect, `In current job` = `Odds Ratio`) 

full_join(or_predictor_hcw,
          or_predictor_job) %>%
    write_csv("Figures/Table_3_Odds_Ratios_for_Gamma_Predictions.csv")


