## MODELLING OF VACCINE SENTIMENT
## MULTIPLE EXPLANATORY VARIABLES

sentiment <- glm(data = hcw.data.forsent,
                 vacc_pos ~ .,
                 family = binomial())



# variable selection
sentiment_step <- MASS::stepAIC(sentiment, 
                                trace = FALSE)#,

# a null model
sentiment_mean <- glm(data = hcw.data.forsent,
    vacc_pos ~ 1, 
    family=binomial())

# what are the error rates?

calculate_MAPE <- function(obj){
    return(mean(abs(obj$y - obj$fitted.values)))
}

sentiment_models <- 
    list(Full     = sentiment,
         Stepwise = sentiment_step,
         Mean     = sentiment_mean) 

sentiment_models %>%
    map_df(~data.frame(AIC = AIC(.x),
                       MAPE = calculate_MAPE(.x)),
           .id = "Model") %>%
    mutate_if(.predicate = is.numeric, .funs = round, digits=3) %>%
    dplyr::arrange(AIC)

sentiment_preds <- sentiment_models %>%
    map_df(~data.frame(obs =.x$y,
                       fit = .x$fitted.values), 
           .id = "Model") %>%
    group_by(Model) %>%
    dplyr::mutate(pred = round(fit),
                  id = 1:n())  %>%
    ungroup %>%
    dplyr::mutate(value = case_when(obs == 1 & pred == 1 ~ "TP",
                                    obs == 0 & pred == 1 ~ "FP",
                                    obs == 1 & pred == 0 ~ "FN",
                                    obs == 0 & pred == 0 ~ "TN",
                                    TRUE ~ "Error")) 

sentiment_preds %>%
    dplyr::count(Model, value)

# do we identify the same people as being false positives?

sentiment_preds %>%
    dplyr::select(id, Model, value) %>%
    split(.$id) %>%
    map_df(~data.frame(Disagreement = length(unique(.$value))),
           .id="id") %>%
    dplyr::filter(Disagreement > 1) %>%
    dplyr::mutate(id = readr::parse_number(id)) %>%
    inner_join(sentiment_preds) %>%
    dplyr::select(id, Model, obs, pred, p=fit, value) %>%
    split(.$id)


    


# end error rates

tidy(sentiment_step, conf.int=TRUE) %>%
    model_output_postprocessor

tidy(sentiment_step, conf.int=TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                     .funs = exp) %>%
    model_output_postprocessor %>%
    dplyr::select(-std.error, -statistic) %>%
    dplyr::mutate(term = gsub(pattern = "(\\(|\\))", 
                              replacement = "",
                              x = term),
                  term = gsub(pattern = "_gp", 
                              replacement = " group",
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
                            vacc_pos ~ payroll ,
                            family = binomial())

# pull the intercept by itself
sentiment_intercept <- tidy(sentiment_restricted, conf.int=T) %>%
    dplyr::filter(term == "(Intercept)") %>%
    dplyr::mutate_at(.vars = vars(estimate, conf.low, conf.high),
                     .funs = boot::inv.logit) %>%
    dplyr::transmute(`Baseline probability` =
                         sprintf("%0.2f (%0.2f, %0.2f)",
                                 estimate, conf.low, conf.high))

# extract odds ratios
# tidy up the column values
# include the intercept
# write out the table
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
    write_csv("Figures/Vaccine_acceptance_after_stepwise_selection.csv")


## fit a GAM model for reviewer

library(mgcv)

gam.formula <- hcw.data.forsent %>% 
    dplyr::select(-starts_with("duration"), -vacc_pos) %>%
    names %>%
    paste(collapse = " + ") %>%
    paste("vacc_pos ~ ", ., " + s(duration_hcw) + s(duration_job)")


sentiment_gam <- gam(data = hcw.data.forsent,
                     formula = as.formula(gam.formula),
                     family = binomial(),
                     select = TRUE)

anova(sentiment_gam)

sentiment_gam_no_smooth <- 
    gam(data = hcw.data.forsent,
        formula = as.formula(
            paste("vacc_pos ~ ", hcw.data.forsent %>%
                      dplyr::select( -vacc_pos) %>%
                      names %>%
                      paste(collapse = " + "))
        ),
        family = binomial(),
        select = TRUE)

anova(sentiment_gam_no_smooth)

## end fit GAM

## fit interaction models

interaction_vars <- hcw.data.forsent %>% 
    dplyr::select(-vacc_pos, -payroll) %>%
    names

names(interaction_vars) <- interaction_vars

interaction_models <- interaction_vars %>%
    map(~paste0("vacc_pos ~ payroll * ", .x)) %>%
    map(~gam(data = hcw.data.forsent,
             formula = as.formula(.x),
             family = binomial())) 

interaction_AIC <- interaction_models %>%
    map(~AIC(.x)) %>%
    map_df(~data.frame(AIC = .x), .id="Variable") %>%
    bind_rows(data.frame(Variable = "None",
                         AIC = AIC(sentiment_restricted))) %>%
    arrange(AIC)

interaction_AIC_max <- interaction_AIC %>%
    dplyr::filter(AIC == min(AIC)) %>%
    pull(Variable) 

interaction_models[[interaction_AIC_max]] %>%
    anova.gam(.)


## visualise interaction model

interaction_plot <- 
    distinct(hcw.data.forsent[ , c("hc_type_gp", "payroll")]) %>%
    bind_cols(pred = predict(
        object = interaction_models[[interaction_AIC_max]], 
        newdata = ., type="link", se.fit=TRUE) %>%
            as.data.frame %>%
            dplyr::mutate(L = fit - 1.96*se.fit,
                          U = fit + 1.96*se.fit)) %>%
    dplyr::mutate_at(.tbl = .,
                     .vars = vars(fit, L, U),
                     .funs = inv.logit) %>%
    dplyr::mutate(payroll = case_when(payroll == 1 ~ "Payroll",
                                      payroll == 2 ~ "Volunteer"),
                  hc_type_gp = case_when(
                      hc_type_gp == 1 ~ "Government Hospital",
                      hc_type_gp == 2 ~ "Community Health Centre",
                      hc_type_gp == 3 ~ "CHP/MCHP",
                      hc_type_gp == 4 ~ "Private/NGO")) %>%
    ggplot(data=., aes(x=hc_type_gp, y=fit)) +
    geom_pointrange(aes(ymin = L, ymax = U, color=payroll),
                    position = position_dodge(width=0.5))  +
    coord_flip() +
    theme_bw() +
    theme(legend.position="bottom") +
    xlab("") + ylab("Sentiment") +
    geom_point(alpha=0.25, size=1,
               aes(y = vacc_pos,
                   color=payroll),
               position=position_jitter(width=0.2,
                                        height=0.1),
               data=hcw.data.forsent  %>%
                   dplyr::mutate(
                       payroll = case_when(payroll == 1 ~ "Payroll",
                                           payroll == 2 ~ "Volunteer"),
                       hc_type_gp = case_when(
                           hc_type_gp == 1 ~ "Government Hospital",
                           hc_type_gp == 2 ~ "Community Health Centre",
                           hc_type_gp == 3 ~ "CHP/MCHP",
                           hc_type_gp == 4 ~ "Private/NGO"),
                       aes(color=payroll))) +
    scale_y_continuous(breaks = seq(0,1,by=0.2)) +
    scale_color_discrete(name="")

ggsave(filename = "Figures/interaction_plot.pdf", 
       plot = interaction_plot,
       width = 15, height = 15, units = "cm",
       dpi = 600)
