## MODELLING OF VACCINE SENTIMENT
## READ IN AND PROCESS DATA FOR ANALYSIS

if (!dir.exists("Figures")){
    dir.create("Figures")
    message("Results may be found in the 'Figures' directory")
} else {
    warning("Directory 'Figures' already exists.
Do you mean for this to be empty?")
}

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
        #edu_gp,
        #income_gp,
        payroll,
        ethnic_gp,
        hc_type_gp ,
        full_time
    ) %>%
    na.omit
