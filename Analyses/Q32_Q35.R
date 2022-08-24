library(tidyverse)

#######
#ADDITIONAL ANALYSES - HACE
#FOLLOWING THE SPEC OUTLINED ON eRDM: https://erdm.scotland.gov.uk:8443/documents/A36362549/details

##Got sick of manually exporting every dataset so wrote a function to do it for me:
exportfun <- function(df_names, QNum) {
  for (i in 1:length(df_names)) {
    write.csv(
      get(df_names[i]),
      paste0(
        "//s0177a/datashare/community_care/Health and Care Experience/2021-22/JB_outputs/",
        QNum,
        df_names[i],
        ".csv"
      ),
      row.names = F
    )
  }
}

#######################################################################################

#Q32: How much do you agree or disagree with the following about your help, care and support services over the past 12 months?

##Function to rename question responses

responsenames_Q32 <- function(df, varname) {
  df %>%
    mutate(
      varname = case_when(
        question == "q32a" ~ "I was aware of the help, care and support options available to me",
        question == "q32b" ~ "I had a say in how my help, care or support was provided",
        question == "q32c" ~ "People took account of the things that mattered to me",
        question == "q32d" ~ "I was treated with compassion and understanding",
        question == "q32e" ~ "I felt safe",
        question == "q32f" ~ " I was supported to live as independently as possible",
        question == "q32g" ~ "My health, support and care services seemed to be well coordinated",
        T ~ "The help, care or support improved or maintained my quality of life"
      )
    )
}

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 178, last_iref = 185

ANALYSIS_Q32_toplevel <- 
  responsenames_Q32(national_results_output, "question") %>% 
  mutate(mean = round(mean*100,1),
         var = str_sub(var, 11)) %>% 
  select(1,4,5,6) %>% 
  pivot_wider(names_from = var, values_from = mean)

####

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 178, last_iref = 185, by_var <- 'sex'

ANALYSIS_Q32_bysex <-
  responsenames_Q32(
    rbind(
      q32a_by_sex,
      q32b_by_sex,
      q32c_by_sex,
      q32d_by_sex,
      q32e_by_sex,
      q32f_by_sex,
      q32g_by_sex,
      q32h_by_sex),
    "question") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17) 


####

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 178, last_iref = 185, by_var <- 'age_band_6'

ANALYSIS_Q32_byage <-
  responsenames_Q32(
    rbind(
      q32a_by_age_band_6,
      q32b_by_age_band_6,
      q32c_by_age_band_6,
      q32d_by_age_band_6,
      q32e_by_age_band_6,
      q32f_by_age_band_6,
      q32g_by_age_band_6,
      q32h_by_age_band_6),
    "question") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17) 

####

## by HSCP
## Generated from 10 - Analysis by geography.R
## first_iref = 178, last_iref = 185

ANALYSIS_Q32_byHSCP <-
  responsenames_Q32(rbind(
    q32a_by_hscp,
    q32b_by_hscp,
    q32c_by_hscp,
    q32d_by_hscp,
    q32e_by_hscp,
    q32f_by_hscp,
    q32g_by_hscp,
    q32h_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%                       #Joining with HSCP_join file created in 60 - JB_HACEanalysis.R
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:18)



###
#EXPORTING
Q32datanames <-c("ANALYSIS_Q32_toplevel", "ANALYSIS_Q32_bysex", "ANALYSIS_Q32_byage", "ANALYSIS_Q32_byHSCP")

#function from above
exportfun(Q32datanames, "Q32/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q33 Overall how would you rate your care/support services?

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 186, last_iref = 186
#RESPONDENTS SELECT 1 - NO TICKBOX

ANALYSIS_Q33_toplevel <- national_results_output %>% 
  mutate(mean = round(mean*100,1),
         var = str_sub(var, 11)) 


## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 186, last_iref = 186, by_var <- 'sex'

ANALYSIS_Q33_bysex <- q33_by_sex %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:16) 
  
## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 186, last_iref = 186, by_var <- 'age_band_6'

ANALYSIS_Q33_byage <- q33_by_age_band_6 %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:16) 

####


## by HSCP
## Generated from 10 - Analysis by geography.R
## first_iref = 186, last_iref = 186

ANALYSIS_Q33_byHSCP <- q33_by_hscp %>% 
  left_join(HSCP_join, by="row_var") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17)

###

## by Q29
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 186, last_iref = 186, by_var <- 'q29a' through 'q29f'

ANALYSIS_Q33_by29 <- rbind(q33_by_q29a, q33_by_q29b, q33_by_q29c, q33_by_q29d, q33_by_q29e, q33_by_q29f) %>% 
  mutate(question = "Overall, how would you rate your help, care or support services",
         varname = case_when(byvar == "q29a"~"Yes,help for me with personal tasks",
                             byvar == "q29b"~"Yes, help for me with household tasks",
                             byvar == "q29c"~"Yes, help for me for activities outside my home",
                             byvar == "q29d"~"Yes, help for me with adaptations, and/or equipment for my home",
                             byvar == "q29e"~"Yes, an alarm service that can get me help",
                             byvar == "q29f"~"Yes, help to look after someone else",
                             byvar == "q29g"~"No, not had any help but I feel that I needed it",
                             T~"No, not had any help"),
         `%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17) 
#EXPORTING
Q33datanames <-c("ANALYSIS_Q33_toplevel", "ANALYSIS_Q33_bysex", "ANALYSIS_Q33_byage", "ANALYSIS_Q33_byHSCP")

#function from above
exportfun(Q33datanames, "Q33/")

exportfun("ANALYSIS_Q33_by29", "Q33/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q35  Do you look after, or give any regular help or support, to.....
#THIS ONE IS A TICKBOX - SINGLE ANSWER

##Function to rename question responses

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 187, last_iref = 187

responsenames_Q35 <- function(df, varname) {
  df %>%
    mutate(
      varname = case_when(
        var == "analysis_var1" ~ "Up to 4 hours a week",
        var == "analysis_var2" ~ "5-19 hours a week",
        var == "analysis_var3" ~ "20-34 hours a week",
        var == "analysis_var4" ~ "35-49 hours a week",
        var == "analysis_var5" ~ "50 or more hours a week",
        T ~ "No"
      )
    )
}

ANALYSIS_Q35_toplevel <- 
  responsenames_Q35(national_results_output, "var") %>% 
  mutate(mean = round(mean*100,1))

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 187, last_iref = 187, by_var <- 'sex'

ANALYSIS_Q35_bysex <- q35_by_sex %>% 
  select(1:7, 20:21) %>% 
  pivot_longer(2:7, names_to = "var", values_to = "mean") %>% 
  responsenames_Q35("var") %>% 
  mutate(mean = round(mean*100,1))

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 187, last_iref = 187, by_var <- 'age_band_6'

ANALYSIS_Q35_byage <- q35_by_age_band_6 %>% 
  select(1:7, 20:21) %>% 
  pivot_longer(2:7, names_to = "var", values_to = "mean") %>% 
  responsenames_Q35("var") %>% 
  mutate(mean = round(mean*100,1))

####

## by HSCP
## Generated from 10 - Analysis by geography.R
## first_iref = 187, last_iref = 187

ANALYSIS_Q35_byHSCP <- q35_by_hscp %>% 
  left_join(HSCP_join, by="row_var") %>% 
  select(1:7, 20:23) %>% 
  pivot_longer(2:7, names_to = "var", values_to = "mean") %>% 
  responsenames_Q35("var") %>% 
  mutate(mean = round(mean*100,1))

###
#EXPORTING
Q35datanames <-c("ANALYSIS_Q35_toplevel", "ANALYSIS_Q35_bysex", "ANALYSIS_Q35_byage", "ANALYSIS_Q35_byHSCP")

#function from above
exportfun(Q35datanames, "Q35/")
