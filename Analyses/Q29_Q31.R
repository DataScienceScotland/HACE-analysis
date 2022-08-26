library(tidyverse)

#######
#ADDITIONAL ANALYSES - HACE
#FOLLOWING THE SPEC OUTLINED ON eRDM: https://erdm.scotland.gov.uk:8443/documents/A36362549/details

#This project called for additional analyses of questiosn 29 through 40 of the Health and Care Experience Survey. Note that each survey question was to be analysed overall, by sex, by age and by geography. This required producing individual data frames for each of these breakdowns using the do loops (see script elsewhere in the repo). It also necessitated creating two additional functions: one for renaming question responses, and another for exporting - THE LATTER FUNCTION IS IN THE Q32 TO Q40 SCRIPT. 

#######################################################################################
#Q29: In the last 12 months, have you had any help or support with everyday living?

##Function to rename question responses

responsenames <- function(df, varname){
  df %>%
  mutate(varname = case_when(question == "q29a"~"Yes,help for me with personal tasks",
                             question == "q29b"~"Yes, help for me with household tasks",
                             question == "q29c"~"Yes, help for me for activities outside my home",
                             question == "q29d"~"Yes, help for me with adaptations, and/or equipment for my home",
                             question == "q29e"~"Yes, an alarm service that can get me help",
                             question == "q29f"~"Yes, help to look after someone else",
                             question == "q29g"~"No, not had any help but I feel that I needed it",
                             T~"No, not had any help"))
}
  

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 164, last_iref = 171

ANALYSIS_Q29_toplevel <- 
  responsenames(national_results_output, "question") %>% 
  mutate(response = case_when(str_detect(var, "1")~1, 
                              T~0),
         mean = round(mean*100,1)) %>% 
  select(-var, -deff) %>%
  pivot_wider(names_from = "response", values_from = "mean")

####

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 164, last_iref = 171, by_var <- 'sex'

ANALYSIS_Q29_bysex <- responsenames(rbind(q29a_by_sex, q29b_by_sex, q29c_by_sex, q29d_by_sex, q29e_by_sex, q29f_by_sex, q29g_by_sex,q29h_by_sex),
                                    "question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13)



####

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 164, last_iref = 171, by_var <- 'age_band_6'

ANALYSIS_Q29_byage <-
  responsenames(rbind(
    q29a_by_age_band_6,
    q29b_by_age_band_6,
    q29c_by_age_band_6,
    q29d_by_age_band_6,
    q29e_by_age_band_6,
    q29f_by_age_band_6,
    q29g_by_age_band_6,
    q29h_by_age_band_6),
    "question") %>%
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13) 

####

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 164, last_iref = 171

ANALYSIS_Q29_byHSCP <-
  responsenames(rbind(
    q29a_by_hscp,
    q29b_by_hscp,
    q29c_by_hscp,
    q29d_by_hscp,
    q29e_by_hscp,
    q29f_by_hscp,
    q29g_by_hscp,
    q29h_by_hscp), 
    "question")

####Grabbing HSCP names
HSCP_join <- hace2122 %>% 
  select(hscp_name, hscp) %>% 
  mutate(row_var = hscp) %>% 
  distinct() %>% 
  select(-hscp)
  
####joining
ANALYSIS_Q29_byHSCP <- ANALYSIS_Q29_byHSCP %>% 
  left_join(HSCP_join, by="row_var") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,11,12,13,14)


###Writing to CSV
Q29datanames <-c("ANALYSIS_Q29_toplevel", "ANALYSIS_Q29_bysex", "ANALYSIS_Q29_byage", "ANALYSIS_Q29_byHSCP")

for(i in 1:length(Q29datanames)) {
  write.csv(get(Q29datanames[i]),
             paste0("//s0177a/datashare/community_care/Health and Care Experience/2021-22/JB_outputs/Q29/",
                    Q29datanames[i],
                    ".csv"),
             row.names=F)
}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q30: Who funds your help or support with everyday living?

responsenames_Q30 <- function(df, varname){
  df %>%
    mutate(varname = case_when(question == "q30a"~"Council",
                               question == "q30b"~"Me/family",
                               question == "q30c"~"NHS",
                               question == "q30d"~"Other",
                               T~"Unpaid cair from friends/family"))
}

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 172, last_iref = 176

ANALYSIS_Q30_toplevel <- 
  responsenames_Q30(national_results_output, "question") %>% 
  mutate(response = case_when(str_detect(var, "1")~1, 
                              T~0),
         mean = round(mean*100,1)) %>% 
  select(-var, -deff) %>%
  pivot_wider(names_from = "response", values_from = "mean")


## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 172, last_iref = 176, by_var <- 'sex'

ANALYSIS_Q30_bysex <- responsenames_Q30(rbind(q30a_by_sex, q30b_by_sex, q30c_by_sex, q30d_by_sex, q30e_by_sex),
                                    "question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13)

####

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 172, last_iref = 176, by_var <- 'age_band_6'

ANALYSIS_Q30_byage <-
  responsenames_Q30(rbind(
    q30a_by_age_band_6,
    q30b_by_age_band_6,
    q30c_by_age_band_6,
    q30d_by_age_band_6,
    q29e_by_age_band_6),
    "question") %>%
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13) 

####

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 172, last_iref = 176

ANALYSIS_Q30_byHSCP <-
  responsenames(rbind(
    q30a_by_hscp,
    q30b_by_hscp,
    q30c_by_hscp,
    q30d_by_hscp,
    q30e_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%     #Joining with HSCP_join file created above
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1),
         varname = case_when(question == "q30a"~"Council",
                   question == "q30b"~"Me/family",
                   question == "q30c"~"NHS",
                   question == "q30d"~"Other",
                   T~"Unpaid cair from friends/family")) %>% 
  select(8,11,12,13,14)

###Writing to CSV
Q30datanames <-c("ANALYSIS_Q30_byHSCP")

for(i in 1:length(Q30datanames)) {
  write.csv(get(Q30datanames[i]),
            paste0("//s0177a/datashare/community_care/Health and Care Experience/2021-22/JB_outputs/Q30/",
                   Q30datanames[i],
                   ".csv"),
            row.names=F)
}

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q31 Which of the following applies to you and how your social care is arranged?
#RESPONDENTS SELECT 1 - NO TICKBOX

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 177, last_iref = 177

ANALYSIS_Q31_toplevel <- national_results_output %>% 
  mutate(var = case_when(var == "analysis_var1" ~ "I had a choice",
                         var == "analysis_var2" ~ "I did not want a choice",
                         var == "analysis_var3" ~ "I had no choices due to medical reasons",
                         var == "analysis_var4" ~ "I was not offered any choices",
                         T~ "Can't remember/don't know"),
         mean = round(mean*100,1))

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 177, last_iref = 177, by_var <- 'sex'

ANALYSIS_Q31_bysex <- q31_by_sex %>% 
  select(1:6, 17:19) %>% 
  pivot_longer(names_to="Question", values_to = "mean value", 2:6) %>% 
  mutate(Question = case_when(Question == "analysis_var1" ~ "I had a choice",
                              Question == "analysis_var2" ~ "I did not want a choice",
                              Question == "analysis_var3" ~ "I had no choices due to medical reasons",
                              Question == "analysis_var4" ~ "I was not offered any choices",
                         T~ "Can't remember/don't know"),
         mean = round(`mean value`*100,1)) %>% 
  select(-`mean value`)

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 177, last_iref = 177, by_var <- 'age_band_6'

ANALYSIS_Q31_byage <- q31_by_age_band_6 %>% 
  select(1:6, 17:19) %>% 
  pivot_longer(names_to="Question", values_to = "mean value", 2:6) %>% 
  mutate(Question = case_when(Question == "analysis_var1" ~ "I had a choice",
                              Question == "analysis_var2" ~ "I did not want a choice",
                              Question == "analysis_var3" ~ "I had no choices due to medical reasons",
                              Question == "analysis_var4" ~ "I was not offered any choices",
                              T~ "Can't remember/don't know"),
         mean = round(`mean value`*100,1)) %>% 
  select(-`mean value`)

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 177, last_iref = 177

ANALYSIS_Q31_byHSCP <- q31_by_hscp %>% 
  left_join(HSCP_join, by="row_var") %>% 
  select(1:6, 17,20) %>% 
  pivot_longer(names_to="Question", values_to = "mean value", 2:6) %>% 
  mutate(Question = case_when(Question == "analysis_var1" ~ "I had a choice",
                              Question == "analysis_var2" ~ "I did not want a choice",
                              Question == "analysis_var3" ~ "I had no choices due to medical reasons",
                              Question == "analysis_var4" ~ "I was not offered any choices",
                              T~ "Can't remember/don't know"),
         mean = round(`mean value`*100,1)) %>% 
  select(-`mean value`)

###Writing to CSV
Q31datanames <-c("ANALYSIS_Q31_toplevel", "ANALYSIS_Q31_bysex", "ANALYSIS_Q31_byage", "ANALYSIS_Q31_byHSCP")

for(i in 1:length(Q31datanames)) {
  write.csv(get(Q31datanames[i]),
            paste0("//s0177a/datashare/community_care/Health and Care Experience/2021-22/JB_outputs/Q31/",
                   Q31datanames[i],
                   ".csv"),
            row.names=F)
}
         
