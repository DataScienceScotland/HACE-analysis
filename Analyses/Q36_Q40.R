#Q36 Who do you help or support?

#Tidying function
responsenames_Q36 <- function(df, varname){
  df %>%
    mutate(varname = case_when(question == "q36a"~"Spouse/Partner",
                               question == "q36b"~"Parent/Grandparent",
                               question == "q36c"~"Child/Grandchild",
                               question == "q36d"~"Friend/Neighbour",
                               question == "q36e"~"Relative (any other relationship)",
                               T~"Someone else"))
}

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 188, last_iref = 193

ANALYSIS_Q36_toplevel <- 
  responsenames_Q36(national_results_output, "question") %>% 
  mutate(response = case_when(str_detect(var, "1")~1, 
                              T~0),
         mean = round(mean*100,1)) %>% 
  select(-var, -deff) %>%
  pivot_wider(names_from = "response", values_from = "mean")

####
## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 188, last_iref = 193, by_var <- 'sex'

ANALYSIS_Q36_bysex <-
  responsenames_Q36(
    rbind(
      q36a_by_sex,
      q36b_by_sex,
      q36c_by_sex,
      q36d_by_sex,
      q36e_by_sex,
      q36f_by_sex),"question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8:13)

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 188, last_iref = 193, by_var <- 'age_band_6'

ANALYSIS_Q36_byage <-
  responsenames_Q36(rbind(
    q36a_by_age_band_6,
    q36b_by_age_band_6,
    q36c_by_age_band_6,
    q36d_by_age_band_6,
    q36e_by_age_band_6,
    q36f_by_age_band_6),
    "question") %>%
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13) 

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 188, last_iref = 193

ANALYSIS_Q36_byHSCP <-
  responsenames_Q36(rbind(
    q36a_by_hscp,
    q36b_by_hscp,
    q36c_by_hscp,
    q36d_by_hscp,
    q36e_by_hscp,
    q36f_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%     #Joining with HSCP_join file created above
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,11,12,13,14)

###
#EXPORTING
Q36datanames <-c("ANALYSIS_Q36_toplevel", "ANALYSIS_Q36_bysex", "ANALYSIS_Q36_byage", "ANALYSIS_Q36_byHSCP")

#function from above
exportfun(Q36datanames, "Q36/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q37Have you received any support to help with your caring role?

#Tidying function
responsenames_Q37 <- function(df, varname){
  df %>%
    mutate(varname = case_when(question == "q37a"~"A written plan about your caring role and support available to you",
                               question == "q37b"~"Help from family/friends/neighbours",
                               question == "q37c"~"Help from Carer Centre/local organisation",
                               question == "q37d"~"Break(s) from caring",
                               question == "q37e"~"Services for the person(s) you care for, to enable you to have a break",
                               question == "q37f"~"Other support",
                               T~"No support/help"))
}

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 194, last_iref = 200

ANALYSIS_Q37_toplevel <- 
  responsenames_Q37(national_results_output, "question") %>% 
  mutate(response = case_when(str_detect(var, "1")~1, 
                              T~0),
         mean = round(mean*100,1)) %>% 
  select(-var, -deff) %>%
  pivot_wider(names_from = "response", values_from = "mean")


####
## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 194, last_iref = 200, by_var <- 'sex'

ANALYSIS_Q37_bysex <-
  responsenames_Q37(
    rbind(
      q37a_by_sex,
      q37b_by_sex,
      q37c_by_sex,
      q37d_by_sex,
      q37e_by_sex,
      q37f_by_sex,
      q37g_by_sex),"question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8:13)

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 194, last_iref = 200, by_var <- 'age_band_6'

ANALYSIS_Q37_byage <-
  responsenames_Q37(rbind(
    q37a_by_age_band_6,
    q37b_by_age_band_6,
    q37c_by_age_band_6,
    q37d_by_age_band_6,
    q37e_by_age_band_6,
    q37f_by_age_band_6, 
    q37g_by_age_band_6),
    "question") %>%
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13) 

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 194, last_iref = 200

ANALYSIS_Q37_byHSCP <-
  responsenames_Q37(rbind(
    q37a_by_hscp,
    q37b_by_hscp,
    q37c_by_hscp,
    q37d_by_hscp,
    q37e_by_hscp,
    q37f_by_hscp,
    q37g_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%     #Joining with HSCP_join file created above
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,11,12,13,14)

ANALYSIS_Q37_byHB <-
  responsenames_Q37(rbind(
    q37a_by_hb,
    q37b_by_hb,
    q37c_by_hb,
    q37d_by_hb,
    q37e_by_hb,
    q37f_by_hb,
    q37g_by_hb), 
    "question") %>% 
  left_join(HB_join, by="row_var") %>%     #Joining with HB_join file created in 60 - JB_mappin'.R
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,11,12,13,14)

###
#EXPORTING
Q37datanames <-c("ANALYSIS_Q37_toplevel", "ANALYSIS_Q37_bysex", "ANALYSIS_Q37_byage", "ANALYSIS_Q37_byHSCP")

#function from above
exportfun(Q37datanames, "Q37/")
exportfun("ANALYSIS_Q37_byHB", "Q37/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q38How much do you agree or disagree with the following about how you feel as a carer most of the time?

##Function to rename question responses

responsenames_Q38 <- function(df, varname) {
  df %>%
    mutate(
      varname = case_when(
        question == "q38a" ~ "I have a good balance between caring and other things in my life",
        question == "q38b" ~ "Caring has had a negative impact on my health and wellbeing",
        question == "q38c" ~ "I have a say in services provided for the person(s) I look after",
        question == "q38d" ~ "Local services are well coordinated for the person(s) I look after",
        T ~ "I feel supported to continue caring")
    )
}

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 201, last_iref = 205

ANALYSIS_Q38_toplevel <- 
  responsenames_Q38(national_results_output, "question") %>% 
  mutate(mean = round(mean*100,1),
         var = str_sub(var, 11)) %>% 
  select(1,4,5,6) %>% 
  pivot_wider(names_from = var, values_from = mean)

####

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 201, last_iref = 205, by_var <- 'sex'

ANALYSIS_Q38_bysex <-
  responsenames_Q38(
    rbind(
      q38a_by_sex,
      q38b_by_sex,
      q38c_by_sex,
      q38d_by_sex,
      q38e_by_sex),
    "question") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17) 

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 201, last_iref = 205, by_var <- 'age_band_6'

ANALYSIS_Q38_byage <-
  responsenames_Q38(
    rbind(
      q38a_by_age_band_6,
      q38b_by_age_band_6,
      q38c_by_age_band_6,
      q38d_by_age_band_6,
      q38e_by_age_band_6),
    "question") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:17) 

####

## by HSCP
## Generated from 10 - Analysis by geography.R
## first_iref = 201, last_iref = 205

ANALYSIS_Q38_byHSCP <-
  responsenames_Q38(rbind(
    q38a_by_hscp,
    q38b_by_hscp,
    q38c_by_hscp,
    q38d_by_hscp,
    q38e_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%                       #Joining with HSCP_join file created in 60 - JB_HACEanalysis.R
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(11:18)

##
#By Q37

ANALYSIS_Q38_byQ37 <- rbind(q38a_by_q37a, q38a_by_q37b, q38a_by_q37c, q38a_by_q37d, q38a_by_q37e, q38a_by_q37f, q38a_by_q37g,
                            q38b_by_q37a, q38b_by_q37b, q38b_by_q37c, q38b_by_q37d, q38b_by_q37e, q38b_by_q37f, q38b_by_q37g,
                            q38c_by_q37a, q38c_by_q37b, q38c_by_q37c, q38c_by_q37d, q38c_by_q37e, q38c_by_q37f, q38c_by_q37g,
                            q38d_by_q37a, q38d_by_q37b, q38d_by_q37c, q38d_by_q37d, q38d_by_q37e, q38d_by_q37f, q38d_by_q37g,
                            q38e_by_q37a, q38e_by_q37b, q38e_by_q37c, q38e_by_q37d, q38e_by_q37e, q38e_by_q37f, q38e_by_q37g) %>% 
  mutate(question = case_when(question == "q38a" ~ "I have a good balance between caring and other things in my life",
                              question == "q38b" ~ "Caring has had a negative impact on my health and wellbeing",
                              question == "q38c" ~ "I have a say in services provided for the person(s) I look after",
                              question == "q38d" ~ "Local services are well coordinated for the person(s) I look after",
                              T ~ "I feel supported to continue caring"),
         # byvar = case_when(byvar == "q37a"~"A written plan about your caring role and support available to you",
         #                   byvar == "q37b"~"Help from family/friends/neighbours",
         #                   byvar == "q37c"~"Help from Carer Centre/local organisation",
         #                   byvar == "q37d"~"Break(s) from caring",
         #                   byvar == "q37e"~"Services for the person(s) you care for, to enable you to have a break",
         #                   byvar == "q37f"~"Other support",
         #                   T~"No support/help"),
         support = case_when(byvar == "q37g"~"Received no support or help",
                             T~"Received support or help"),
         `%negative` = round(posneutneg..negative*100,1),
         `%neutral` = round(posneutneg..neutral*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  filter(var ==1 ) %>% 
  pivot_longer(15:17, names_to = "cat", values_to = "prop") %>% 
  group_by(question, support, cat) %>% 
  summarise(total = mean(prop)) %>% 
  ggplot(aes(x=question, y=total, fill = cat)) +
  geom_col(position = "stack") +
  facet_wrap(~support) +
  coord_flip() +
  theme_bw()+
  labs(title = "Figure X: Subjective feelings on being a carer by whether support was received, 2020-21",
       x="Question",
       y="%")

###
#EXPORTING
Q38datanames <-c("ANALYSIS_Q38_toplevel", "ANALYSIS_Q38_bysex", "ANALYSIS_Q38_byage", "ANALYSIS_Q38_byHSCP")



#function from above
exportfun(Q38datanames, "Q38/")

exportfun("ANALYSIS_Q38_byQ37", "Q38/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q39 In general, how well do you feel that you are able to look after your own health?

##Function to rename question responses

##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 206, last_iref = 206
#RESPONDENTS SELECT 1 - NO TICKBOX

ANALYSIS_Q39_toplevel <- national_results_output %>% 
  mutate(mean = round(mean*100,1),
         var = str_sub(var, 11)) 

## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 206, last_iref = 206, by_var <- 'sex'

ANALYSIS_Q39_bysex <- q39_by_sex %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(8:12) 

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 206, last_iref = 206, by_var <- 'age_band_6'

ANALYSIS_Q39_byage <- q39_by_age_band_6 %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(8:12) 

####

## by HSCP
## Generated from 10 - Analysis by geography.R
## first_iref = 206, last_iref = 206

ANALYSIS_Q39_byHSCP <- q39_by_hscp %>% 
  left_join(HSCP_join, by="row_var") %>% 
  mutate(`%negative` = round(posneutneg..negative*100,1),
         `%positive` = round(posneutneg..positive*100,1)) %>% 
  select(8:13)

###
#EXPORTING
Q39datanames <-c("ANALYSIS_Q39_toplevel", "ANALYSIS_Q39_bysex", "ANALYSIS_Q39_byage", "ANALYSIS_Q39_byHSCP")

#function from above
exportfun(Q39datanames, "Q39/")

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Q40 Do you have any of the following?

##Function to rename question responses

responsenames_Q40 <- function(df, varname) {
  df %>%
    mutate(
      varname = case_when(
        question == "q40a" ~ "Deafness/severe hearing impairment",
        question == "q40b" ~ "Blindness/severe vision impairment",
        question == "q40c" ~ "Physical disability",
        question == "q40d" ~ "Chronic pain lasting at least 3 months",
        question == "q40e" ~ "Mental health condition",
        question == "q40f" ~ "Learning disability",
        question == "q40g" ~ "Another long-term condition",
        question == "q40h" ~ "Full or partial loss of voice or significant difficulty speaking",
        T ~ "None of the above"
      )
    )
}


##Top-level analysis - generated from 06 - National Results Do Loop.R
##first_iref = 207, last_iref = 215

ANALYSIS_Q40_toplevel <- 
  responsenames_Q40(national_results_output, "question") %>% 
  mutate(response = case_when(str_detect(var, "1")~1, 
                              T~0),
         mean = round(mean*100,1)) %>% 
  select(1, 4,6,7) %>% 
  pivot_wider(names_from = response, values_from = mean)

####
## by sex
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 207, last_iref = 215, by_var <- 'sex'

ANALYSIS_Q40_bysex <-
  responsenames_Q40(
    rbind(
      q40a_by_sex,
      q40b_by_sex,
      q40c_by_sex,
      q40d_by_sex,
      q40e_by_sex,
      q40f_by_sex,
      q40g_by_sex,
      q40g_by_sex,
      q40i_by_sex),"question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8:13)

## by age
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 207, last_iref = 215, by_var <- 'age_band_6'

ANALYSIS_Q40_byage <-
  responsenames_Q40(rbind(
    q40a_by_age_band_6,
    q40b_by_age_band_6,
    q40c_by_age_band_6,
    q40d_by_age_band_6,
    q40e_by_age_band_6,
    q40f_by_age_band_6,
    q40f_by_age_band_6,
    q40f_by_age_band_6,
    q40f_by_age_band_6),
    "question") %>%
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8:13) 

## by HSCP
## Generated from 10 - Analysis by geography.R
##first_iref = 207, last_iref = 215

ANALYSIS_Q40_byHSCP <-
  responsenames_Q40(rbind(
    q40a_by_hscp,
    q40b_by_hscp,
    q40c_by_hscp,
    q40d_by_hscp,
    q40e_by_hscp,
    q40f_by_hscp,
    q40g_by_hscp,
    q40g_by_hscp,
    q40i_by_hscp), 
    "question") %>% 
  left_join(HSCP_join, by="row_var") %>%     #Joining with HSCP_join file created above
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,11,12,13,14)

###
#EXPORTING
Q40datanames <-c("ANALYSIS_Q40_toplevel", "ANALYSIS_Q40_bysex", "ANALYSIS_Q40_byage", "ANALYSIS_Q40_byHSCP")

#function from above
exportfun(Q40datanames, "Q40/")
