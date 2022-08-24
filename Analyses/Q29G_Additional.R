#ADDITIONAL ANALYSES ON Q29G ("No, not had any help but I feel that I needed it")

#NOTE: uses write function from Q32_Q35.R

#FUNCTION FOR CREATING PERCENTAGES
mutatefun <- function(df, analysis_var0, analysis_var1){
  df %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
    select(question, varname, byvar, var, `0`, `1`)
}

## by SIMD
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'simd2020v2_sc_quintile'


ANALYSIS_Q29g_bySIMD <- responsenames(q29g_by_simd2020v2_sc_quintile, "question") %>% 
  mutate(`0` = round(analysis_var0*100,1),
         `1` = round(analysis_var1*100,1)) %>% 
  select(8,9,10,11,12,13)

exportfun("ANALYSIS_Q29g_bySIMD", "Q29/")


## by Q41 (Are your day-to-day activities limited because of a health problem or disability) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q41'


ANALYSIS_Q29g_by_Q41 <- responsenames(q29g_by_q41, "question") %>% 
  mutate(byvar = "Are your day-to day activities limited because of a health problem/disability",
         var = case_when(var =="1" ~ "Yes, limited a lot",
                         var =="2" ~ "Yes, limited a little",
                         var =="3" ~ "No",
                         T~"Other")) %>% 
  mutatefun %>% 
  select(question, varname, byvar, var, `0`, `1`)

exportfun("ANALYSIS_Q29g_by_Q41", "Q29/")

## by Q43 (Ethnicity) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q43'

ANALYSIS_Q29g_by_Ethnicity <- responsenames(q29g_by_q43, "question") %>% 
  mutate(byvar = "Ethnicity",
         var = case_when(var =="1" ~ "White",
                         var =="2" ~ "Mixed or multiple ethnic groups",
                         var =="3" ~ "Asian, Asian Scottish or Asian British",
                         var =="4" ~ "African",
                         var =="5" ~ "Caribbean or Black",
                         var =="6" ~ "Other ethnic group",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_Ethnicity", "Q29/")

## by Q44 (SexualOrientation) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q44'

ANALYSIS_Q29g_by_Sexualorientation <- responsenames(q29g_by_q44, "question") %>% 
  mutate(byvar = "Sexual Orientation",
         var = case_when(var =="1" ~ "Heterosexual/Straight",
                         var =="2" ~ "Gay/Lesbian",
                         var =="3" ~ "Bisexual",
                         var =="4" ~ "Other",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_Sexualorientation", "Q29/")

## by Q45 (Employment) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q45'

ANALYSIS_Q29g_by_Employment <- responsenames(q29g_by_q45, "question") %>% 
  mutate(byvar = "Employment",
         var = case_when(var =="1" ~ "Employed(FT/PT)",
                         var =="2" ~ "Self-employed(FT/PT)",
                         var =="3" ~ "In FT education/training",
                         var =="4" ~ "Unemployed/looking for work",
                         var =="5" ~ "Don't work due to illness/disability",
                         var =="6" ~ "Don't work due to caring responsibilities",
                         var =="7" ~ "Retired",
                         var =="8" ~ "Other",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_Employment", "Q29/")

## by Q46 (Religion) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q46'

ANALYSIS_Q29g_by_Religion <- responsenames(q29g_by_q46, "question") %>% 
  mutate(byvar = "Religion",
         var = case_when(var =="1" ~ "None",
                         var =="2" ~ "Church of Scotland",
                         var =="3" ~ "Roman Catholic",
                         var =="4" ~ "Other Christian",
                         var =="5" ~ "Muslim",
                         var =="6" ~ "Buddhist",
                         var =="7" ~ "Sikh",
                         var =="8" ~ "Jewish",
                         var =="9" ~ "Hindu",
                         var =="10"~ "Pagan",
                         var =="11" ~"Another religion",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_Religion", "Q29/")

## by Q01 (GP contact) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q01'

ANALYSIS_Q29g_by_GPcontact <- responsenames(q29g_by_q01, "question") %>% 
  mutate(byvar = "Religion",
         var = case_when(var =="1" ~ "In the last 12 months",
                         var =="2" ~ "Attend a different GP practice",
                         var =="3" ~ "More than 12 months ago",
                         var =="4" ~ "Can't remember/don't know",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_Religion", "Q29/")

## by Q21 (GP contact out of hours) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q21'
ANALYSIS_Q29g_by_GPcontactOutofHours <- responsenames(q29g_by_q21, "question") %>% 
  mutate(byvar = "GP contact Out of hours",
         var = case_when(var =="1" ~ "Yes",
                         var =="2" ~ "No",
                         T~"Unknown")) %>% 
  mutatefun

exportfun("ANALYSIS_Q29g_by_GPcontactOutofHours", "Q29/")

## by Q35 (Caring responsibilities) 
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q35'
ANALYSIS_Q29g_by_CaringResponsibilities <- responsenames(q29g_by_q35, "question") %>% 
  mutate(byvar = "Caring responsibilities",
         var = case_when(var == "1" ~"Up to 4 hours a week",
                         var == "2" ~"5-19 hours a week",
                         var =="3" ~ "20-34 hours a week",
                         var =="4" ~ "35-49 hours a week",
                         var =="5" ~ "50 or more hours a week",
                         var =="6"~ "No",
                         T~"Unknown")) %>% 
  mutatefun


exportfun("ANALYSIS_Q29g_by_CaringResponsibilities", "Q29/")

## by health board
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 170, last_iref = 170, by_var <- 'Q35'
ANALYSIS_Q29g_by_healthboard <- responsenames(q29g_by_hb_name, "question") %>% 
  mutatefun


exportfun("ANALYSIS_Q29g_by_healthboard", "Q29/")

