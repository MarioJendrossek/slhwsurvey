# 0| +++++ Preparing data cleaning +++++
  # require packages
  require("stringr")
  
  # set working and data directory
  setwd("C:/Users/Mario/Desktop/docs LSHTM/R analysis")
  data.dir <- "C:/Users/Mario/Desktop/docs LSHTM/ODK data/"

  # read in survey data
  hcw.data <- read.csv(paste0(data.dir, "Health worker Survey.csv"), header=T, stringsAsFactors=FALSE)


# 1| +++++ data cleaning +++++
  
  # a) superficial cleaning
    # remove capitals
    hcw.data <- data.frame(apply(hcw.data, 2, tolower))
  
    # remove trailing and leading white spaces
    hcw.data <- data.frame(apply(hcw.data, 2, trimws))
    
    # remove special chars
    # hcw.data <- data.frame(apply(hcw.data, 2, str_replace_all("[[:punct:]]", "")
    
    # remove factors
      hcw.data$district <- as.numeric(as.character(hcw.data$district))
      hcw.data$health_ctr_name <- as.character(hcw.data$health_ctr_name)
      hcw.data$health_ctr_type <- as.numeric(as.character(hcw.data$health_ctr_type))
      hcw.data$health_ctr_type_other <- as.character(hcw.data$health_ctr_type_other)
      hcw.data$interv_name <- as.character(hcw.data$interv_name)
      hcw.data$sex <- as.numeric(as.character(hcw.data$sex))
      hcw.data$age_gp <- as.numeric(as.character(hcw.data$age_gp))
      hcw.data$ethnic_gp <- as.numeric(as.character(hcw.data$ethnic_gp))
      hcw.data$full_time <- as.numeric(as.character(hcw.data$full_time))
      hcw.data$payroll <- as.numeric(as.character(hcw.data$payroll))
      hcw.data$profession <- as.numeric(as.character(hcw.data$profession))
      hcw.data$profession_group <- as.numeric(as.character(hcw.data$profession_group))
      hcw.data$profession_other <- as.character(hcw.data$profession_other)
      hcw.data$rel <- as.numeric(as.character(hcw.data$rel))
      hcw.data$stay_6mo <- as.numeric(as.character(hcw.data$stay_6mo))
      hcw.data$stay_24mo <- as.numeric(as.character(hcw.data$stay_24mo))
      hcw.data$other_hc_yn <- as.numeric(as.character(hcw.data$other_hc_yn))
      hcw.data$other_hc <- as.character(as.character(hcw.data$other_hc))
      hcw.data$health_ctr_type_2 <- as.numeric(as.character(hcw.data$health_ctr_type_2))
      hcw.data$num_hc <- as.numeric(as.character(hcw.data$num_hc))
      hcw.data$break. <- as.numeric(as.character(hcw.data$break.))
      hcw.data$risk_contact <- as.character(as.character(hcw.data$risk_contact))
      hcw.data$vaccination_yn <- as.numeric(as.character(hcw.data$vaccination_yn))
      hcw.data$vacc_info_yn <- as.numeric(as.character(hcw.data$vacc_info_yn))
      hcw.data$ebola_contact_yn <- as.numeric(as.character(hcw.data$ebola_contact_yn))
      hcw.data$ebola_hcw_yn <- as.numeric(as.character(hcw.data$ebola_hcw_yn))
      hcw.data$prof_ebola <- as.numeric(as.character(hcw.data$prof_ebola))
      hcw.data$prof_ebola_other <- as.character(hcw.data$prof_ebola_other)
      hcw.data$edu <- as.numeric(as.character(hcw.data$edu))
      hcw.data$hh_num <- as.numeric(as.character(hcw.data$hh_num))
      hcw.data$hh_radio <- as.numeric(as.character(hcw.data$hh_radio))
      hcw.data$hh_tv <- as.numeric(as.character(hcw.data$hh_tv))
      hcw.data$hh_income_0 <- as.numeric(as.character(hcw.data$hh_income))
      hcw.data$ind_income <- as.numeric(as.character(hcw.data$ind_income))
      
       
  # b) remove errors
        # remove health facility name errors
        hcw.data$health_ctr_name <- as.character(hcw.data$health_ctr_name)
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="jenner weight chc"] <- "jenner wright chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="bamoi chp"] <- "bamoi luma chp"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="gbalamaya chc" | hcw.data$health_ctr_name=="gbalamunya chc" | hcw.data$health_ctr_name=="gbalamuya health clinic"] <- "gbalamuya chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="gibril"] <- "rokupr chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="good grace clinic" | hcw.data$health_ctr_name=="good grace private clinic"] <- "good grace hospital"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="grey bush" | hcw.data$health_ctr_name== "grey bush health center"] <- "grey bush chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="kambia referral hospital"] <- "kambia district hospital"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="king hermma" | hcw.data$health_ctr_name==".king hermma hospital" | hcw.data$health_ctr_name=="king herrma hospital" ] <- "king hermma hospital"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="kroobay health center"] <- "kroobay chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="mariana mchp"] <- "mariama mchp"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="murray town health center" | hcw.data$health_ctr_name=="murray town health centrer"] <- "murray town chc"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="kambia red cross clinic"] <- "red cross clinic"
        hcw.data$health_ctr_name[hcw.data$health_ctr_name=="st anthony clinic ngo" | hcw.data$health_ctr_name=="st antony clinic ngo"] <- "st antony clinic"
       hcw.data$health_ctr_name[hcw.data$health_ctr_name=="statatile hospital" | hcw.data$health_ctr_name=="statetile hospital" |hcw.data$health_ctr_name=="statelite hospital"] <- "king herrma hospital"
      
      # remove health centre type errors
      # good grace hospital
      hcw.data$health_ctr_type[hcw.data$health_ctr_name=="good grace hospital"] <- "2"
      
      # red cross clinic
      hcw.data$health_ctr_type[hcw.data$health_ctr_name=="red cross clinic"] <- "6"
      
      # other in kambia hospital
      hcw.data$health_ctr_type[hcw.data$health_ctr_name=="kambia district hospital"] <- "1"
      hcw.data$health_ctr_type_other[hcw.data$health_ctr_name=="kambia district hospital"] <- ""
      
      # kroobay chc
      hcw.data$health_ctr_type[hcw.data$health_ctr_name=="kroobay chc"] <- "5"
     
       # other errors
       #####
  
    # remove99/88-> NAs
    hcw.data$age_gp[hcw.data$age_gp==99 | hcw.data$age_gp==88] <- NA
    hcw.data$ethnic_gp[hcw.data$ethnic_gp==99 | hcw.data$ethnic_gp==88] <- NA
    hcw.data$payroll[hcw.data$payroll==99 | hcw.data$payroll==88] <- NA
    hcw.data$profession_group[hcw.data$profession_group==99 | hcw.data$profession_group==88] <- NA
    hcw.data$stay_6mo[hcw.data$stay_6mo==88] <- 2
    hcw.data$stay_6mo[hcw.data$stay_6mo==99] <- NA
    hcw.data$stay_24mo[hcw.data$stay_24mo==88] <- 2
    hcw.data$stay_24mo[hcw.data$stay_24mo==99] <- NA
    hcw.data$break.[hcw.data$break.==88 | hcw.data$break.==99] <- NA
      
   
  # c) variable management
  
    # 1. create 3 variables for risk contacts (body fluids etc)
    hcw.data$risk_contact_fluids <- NA
    hcw.data$risk_contact_clothes <- NA
    hcw.data$risk_contact_body <- NA
 
    for (i in 1:length(hcw.data$district)) {
      if(str_detect(hcw.data$risk_contact[i], "1")) {
        hcw.data$risk_contact_fluids[i] <- 1
      }
      else{hcw.data$risk_contact_fluids[i] <- 0}
      if(str_detect(hcw.data$risk_contact[i], "2")) {
        hcw.data$risk_contact_clothes[i] <- 1
      }
      else{hcw.data$risk_contact_clothes[i] <- 0}
      if(str_detect(hcw.data$risk_contact[i], "3")) {
        hcw.data$risk_contact_body[i] <- 1
      }
      else{hcw.data$risk_contact_body[i] <- 0}
    }

 
    # 2. make single variable for duration of current job (based on ages or duration answers) & duration of all job (substract break?)
      ### duration.job
       hcw.data$duration_job <- NA
       hcw.data$duration_hcw <- NA
       for (i in 1:length(hcw.data$district)) {
         if(hcw.data$age_jobstart[i]==2) {
           hcw.data$duration_job[i] <- (as.Date(as.character("08/07/2018"), format="%d/%m/%Y")-
             as.Date(as.character(hcw.data$start_job[i]), format="%d.%m.%Y"))/365.25
         }
         else if (hcw.data$age_jobstart[i]==1) {
           hcw.data$duration_job[i] <- (as.numeric(as.character(hcw.data$age_job.age_1[i])) - as.numeric(as.character(hcw.data$age_job.start_job_age[i])))
         }
         else if(hcw.data$age_jobstart[i]==99){
           hcw.data$duration_job[i]<-NA
         }
       }
  
      ### duration.hcw (difference between 1.7.2018 and start of job)
       for (i in 1:length(hcw.data$district)) {
         if(hcw.data$age_hcwstart[i]==2) {
           hcw.data$duration_hcw[i] <- (as.Date(as.character("08/07/2018"), format="%d/%m/%Y")-
                                          as.Date(as.character(hcw.data$start_hcw[i]), format="%d.%m.%Y"))/365.25
         }
         else if (hcw.data$age_hcwstart[i]==1) {
           hcw.data$duration_hcw[i] <- (as.numeric(as.character(hcw.data$age_hcw.age_2[i])) - as.numeric(as.character(hcw.data$age_hcw.start_hcw_age[i])))
         }
         else if(hcw.data$age_hcwstart[i]==99){
           hcw.data$duration_hcw[i]<-NA
         }
       }
       
       # compare duration hcw and job (job should not be longer than hcw!)
       hcw.data$duration_hcw-hcw.data$duration_job
        sum(hcw.data$duration_hcw-hcw.data$duration_job<0,na.rm=T)
        # replace duration_job by duration hcw if duration job > duration_hcw
        for (i in 1:length(hcw.data$district)) {
          if(!is.na(hcw.data$duration_hcw[i])& !is.na(hcw.data$duration_job[i])){         
            if(hcw.data$duration_job[i]>hcw.data$duration_hcw[i]) {
            hcw.data$duration_job[i] <- hcw.data$duration_hcw[i]
            }
          }
        }
 
    # 3. use info from profession other field
      hcw.data$profession[hcw.data$profession_other=="sechn nurse" | hcw.data$profession_other=="sechn" | hcw.data$profession_other=="staff nurse" | hcw.data$profession_other=="srn nurse" | hcw.data$profession_other=="nursing officer" | hcw.data$profession_other=="bsc in nursing"] <- 2
      hcw.data$profession[hcw.data$profession_other=="anesthesias"] <- 1
      hcw.data$profession[hcw.data$profession_other=="maintance"] <- 15
      hcw.data$profession[hcw.data$profession_other=="security"] <- 15
      hcw.data$profession[hcw.data$profession_other=="matron" |hcw.data$profession_other== "ritired matron"] <- 2
      # cold room 2 declared in group with maintenance and porters (little direct patient contact)
      hcw.data$profession[hcw.data$profession_other=="cold room"] <- 15
    
  

     # 5. info from 2 scenario Qs to construct vacc_opinion
       hcw.data$vacc_op <- NA
       for (i in 1:length(hcw.data$district)) {
         if(hcw.data$vaccination_yn[i]==0) {
           hcw.data$vacc_op[i] <- 3
         }
           else if(hcw.data$vaccination_yn[i]==1) {
             if (hcw.data$vacc_info_yn[i]==1) {
               hcw.data$vacc_op[i] <- 0
             }
             else if(hcw.data$vacc_info_yn[i]==0) {
               hcw.data$vacc_op[i] <- 2
             }
             else if(hcw.data$vacc_info_yn[i]==88) {
               hcw.data$vacc_op[i] <- 1
             }
             else if(hcw.data$vacc_info_yn[i]==99) {
               hcw.data$vacc_op[i] <- NA
             }
           }
           else if(hcw.data$vaccination_yn[i]==88) {
           hcw.data$vacc_op[i] <- 1
           }
           else if(hcw.data$vaccination_yn[i]==99) {
             hcw.data$vacc_op[i] <- NA
           }
       }
     
       # binary vaccination opinion var
       hcw.data$vacc_pos <- NA
       for (i in 1:length(hcw.data$district)) {
         if(!is.na(hcw.data$vacc_op[i])) {
         if(hcw.data$vacc_op[i]==3) {
           hcw.data$vacc_pos[i] <- 1
         }
         else if(hcw.data$vacc_op[i] %in% c(2,1,0)) {
           hcw.data$vacc_pos[i] <- 0
         }
         }
         else if(is.na(hcw.data$vacc_op[i])) {
           hcw.data$vacc_pos[i] <- NA
         }
       }
 
       
     # 6.+++++++ check income (if unrealistic take out 0: typing error) => change Q to be range of income 0-500,000 500-1M, 1-2M 2-5M Over 5M
     # make use only of info up to 1,5M Leones (9 over 10M---typing errors probably)
  
    #use info from 2 info fields for income (assume same income for partner? or make quantile for each value?)
       hcw.data$hh_income <- NA
        for (i in 1:length(hcw.data$district)) {
         income <- hcw.data$ind_income[i]
           if(is.na(income)) {
             income <- 0
           }
         income_2 <- income*2
           if(hcw.data$hh_income_0[i]==99) {
             if(income!=99) {
             hcw.data$hh_income[i] <- income_2
           } else {
             hcw.data$hh_income[i] <- NA
           }
          }
        else {hcw.data$hh_income[i] <- hcw.data$hh_income_0[i]
        }
     }
     
    
    # 7. create SES score variable (tv, radio, education, income)
      hcw.data$ses_score <- 0
      
      # add 1 if hcw has radio at home
      for (i in 1:length(hcw.data$district)) {
        if(hcw.data$hh_radio[i]==1) {
          hcw.data$ses_score[i] <- hcw.data$ses_score[i] + 1
        }
      }
      # add 2 if hcw has tv at home
      for (i in 1:length(hcw.data$district)) {
        if(hcw.data$hh_tv[i]==1) {
          hcw.data$ses_score[i] <- hcw.data$ses_score[i] + 2
        }
      }
      
      # add 1 if hh income between 500000 and 1 000 000 SLL per month
      for (i in 1:length(hcw.data$district)) {
        if(!is.na(hcw.data$hh_income[i])) {
        if(hcw.data$hh_income[i]>599999 & hcw.data$hh_income[i]<1100000) {
          hcw.data$ses_score[i] <- hcw.data$ses_score[i] + 1
        }
        }
      }
      # add 3 if income over 1 000 000 SLL per month
      for (i in 1:length(hcw.data$district)) {
        if(!is.na(hcw.data$hh_income[i])){
        if(hcw.data$hh_income[i]>=1100000) {
          hcw.data$ses_score[i] <- hcw.data$ses_score[i] + 3
        }
        }
      }


    # sesscore groups
    hcw.data$ses_gp <- NA
    hcw.data$ses_gp[hcw.data$ses_score<3] <- 1
    hcw.data$ses_gp[hcw.data$ses_score==3 | hcw.data$ses_score==4] <- 2
    hcw.data$ses_gp[hcw.data$ses_score>4] <- 3
  
  
  
    # 9. 2 & 3 is not full time for full_time    
    hcw.data$full_time[hcw.data$full_time==3] <- 2
  
    # 10. income group var
    quantile(hcw.data$hh_income, 0.3333, na.rm=T)
    quantile(hcw.data$hh_income, 0.6666, na.rm=T)
    hcw.data$income_gp[hcw.data$hh_income<600000] <- 1
    hcw.data$income_gp[hcw.data$hh_income>599999 & hcw.data$hh_income<1100000] <- 2
    hcw.data$income_gp[hcw.data$hh_income>1099999] <- 3
    
    # 11. edu grouped
    hcw.data$edu_gp <- NA
    hcw.data$edu_gp[hcw.data$edu %in% c(1,2,3,4)] <-1
    hcw.data$edu_gp[hcw.data$edu %in% c(5)] <-2
    hcw.data$edu_gp[hcw.data$edu %in% c(6)] <-3
  
    # 12. age gped
    hcw.data$age_gp[hcw.data$age_gp==6] <- 5
  
    
    # 13. profession vars
    # regroup a bit
    hcw.data$profession[hcw.data$profession %in% c(15)] <- 12
    hcw.data$profession[hcw.data$profession %in% c(4)] <- 10
    
    # create other prof group var
    hcw.data$prof_gp <- NA 
    hcw.data$prof_gp[hcw.data$profession=="1"] <- 1
      hcw.data$prof_gp[hcw.data$profession=="2"] <-1 
      hcw.data$prof_gp[hcw.data$profession=="3"] <- 1
      hcw.data$prof_gp[hcw.data$profession=="6"] <- 4
      hcw.data$prof_gp[hcw.data$profession=="8"] <- 4
      hcw.data$prof_gp[hcw.data$profession=="9"] <- 1
      hcw.data$prof_gp[hcw.data$profession=="10"] <- 1
      hcw.data$prof_gp[hcw.data$profession=="11"] <- 2
      hcw.data$prof_gp[hcw.data$profession=="12"] <- 3
      hcw.data$prof_gp[hcw.data$profession=="14"] <- 3
      
    # compare prof and prof_gp
    table(hcw.data$profession, hcw.data$profession_group)
  
    # prof name
    hcw.data$prof_name <- NA
    hcw.data$prof_name[hcw.data$profession==1] <- "Doctor"
    hcw.data$prof_name[hcw.data$profession==2] <- "Nurse"
    hcw.data$prof_name[hcw.data$profession==3] <- "Midwife"
    hcw.data$prof_name[hcw.data$profession==6] <- "Pharmacist"
    hcw.data$prof_name[hcw.data$profession==8] <- "Lab worker"
    hcw.data$prof_name[hcw.data$profession==9] <- "MCHA"
    hcw.data$prof_name[hcw.data$profession==10] <- "CHO/CHA"
    hcw.data$prof_name[hcw.data$profession==11] <- "CHW"
    hcw.data$prof_name[hcw.data$profession==12] <- "Cleaner/Porter"
    hcw.data$prof_name[hcw.data$profession==14] <- "Admin"
    
    # 14. ethnic group regroup small in others
    hcw.data$ethnic_gp[hcw.data$ethnic_gp==9] <- 14
    hcw.data$ethnic_gp[hcw.data$ethnic_gp==10] <-14
    hcw.data$ethnic_gp[hcw.data$ethnic_gp==11] <-14
    hcw.data$ethnic_gp[hcw.data$ethnic_gp==8] <- 14
 
    # urban rural?
    hcw.data$urban <- NA
    hcw.data$urban[hcw.data$district==1] <- 1
    hcw.data$urban[hcw.data$district==2] <- 0
    hcw.data$urban[hcw.data$health_ctr_name=="kambia district hospital" | hcw.data$health_ctr_name=="red cross clinic"] <- 1
       
    # hc types regroup
    
    hcw.data$hc_type_gp <- NA
    hcw.data$hc_type_gp[hcw.data$health_ctr_type==1] <- 1
    hcw.data$hc_type_gp[hcw.data$health_ctr_type==5] <- 2
    hcw.data$hc_type_gp[hcw.data$health_ctr_type==6 | hcw.data$health_ctr_type==7] <- 3
    hcw.data$hc_type_gp[hcw.data$health_ctr_type==2 | hcw.data$health_ctr_type==4] <- 4
    
     # keep only needed variables (need to add two duration vars)
    keep.these <- c("district", "health_ctr_name", "health_ctr_type", "interv_name", "sex", "age_gp", "ethnic_gp", "rel", "full_time", "payroll", "profession", "stay_6mo", "stay_24mo", "other_hc_yn","num_hc", "break.", "risk_contact_fluids","risk_contact_clothes", "risk_contact_body", "vacc_op", "vacc_pos",  "ebola_contact_yn", "ebola_hcw_yn", "edu", "hh_num", "hh_radio", "hh_tv", "hh_income", "duration_job", "duration_hcw", "ses_score", "ses_gp" , "edu_gp", "income_gp", "prof_name", "prof_gp", "urban", "hc_type_gp"  )
hcw.data <- hcw.data[, keep.these]
 