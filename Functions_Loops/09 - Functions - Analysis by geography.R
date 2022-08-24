# survey v4.1-1
library(survey)


# Two functions created in this program - one for positive, neutral and negative, and another for info questions
# This creates functions that can be used to analyse the HACE data by geography
  # question_number - e.g. q01....q42 Case sensitive. Refer to the "question_data" data frame to identify the format.
  # geography - needs to be one of 'nat', 'hb', 'hscp', 'gpcl' or 'gp'
  # wt - the weight to be used for analysis - note: this needs to be the full weight, including the geography prefix.
  # ci_flag - whether you want confidence intervals to be calculated. "Y" if you do. Anything else for no.


single_question_bygeo_pnn <- function(question_number, geography, wt, ci_flag){
  question <- question_number
  section_weight <- wt
  
  # set up info from the questions data set
  pnn <- filter(hace2122_questions, question_no==question)
  
  high_positive_pnn <- pnn$high_positive
  low_positive_pnn <- pnn$low_positive
  neutral_values_pnn <- pnn$neutral_values
  low_negative_pnn <- pnn$low_negative
  high_negative_pnn <- pnn$high_negative
  excluded_values_pnn <- pnn$excluded_values
  
  
  
  
  
  # pull out the required info from the respondents data set
  responses <- hace2122 %>%
    select(patientid_sg, all_of(question), gp_prac_no, all_of(section_weight), eligible_pats, all_of(geography)) %>%
    rename(analysis_var = question,
           analysisweight = section_weight,
           analysisby = geography)   
  
  # recode responses into positive, neutral or negative
  responses$posneutneg <- ifelse(responses$analysis_var %in% c(high_positive_pnn,low_positive_pnn),"% positive",
                                 ifelse(responses$analysis_var %in% c(neutral_values_pnn), "% neutral",
                                        ifelse(responses$analysis_var %in% c(low_negative_pnn,high_negative_pnn),"% negative",NA)))
  responses$posneutneg <- ifelse(is.na(responses$analysis_var), NA, responses$posneutneg)
  
  
  responses$analysisby <- ifelse(is.na(responses$analysisby),999 , responses$analysisby)
  responses <- filter(responses, !is.na(responses$posneutneg))
  
  
  # This may not be necessary  
  responses$posneutneg <-as.factor(responses$posneutneg) 
  responses$analysisby <-as.factor(responses$analysisby)
  
  # for checking  
  # assign("responses", data.frame(responses), envir = .GlobalEnv)
  
  
  
  # set up the survey design
  options(survey.lonely.psu="remove")
  surveydesign<-svydesign(id=~1, strata=~gp_prac_no, fpc=~eligible_pats, 
                          weights=~analysisweight, data=responses)
  
  
  # run survey analysis and output to a data set
  output <- as.data.frame(svyby(~posneutneg, by=~analysisby, design=surveydesign, svymean, na.rm=TRUE, deff=TRUE, keep.var=TRUE))
  
  output$question <- question
  output$byvar <- geography
  output$row_var <- row.names(output)
  
  
    assign(paste0(question,"_by_", geography), data.frame(output), envir = .GlobalEnv)
  
  
  # produce confidence intervals
  if (confidence_intervals=="Y") {
    
    output2 <- as.data.frame(confint(svyby(~posneutneg, by=~analysisby, design=surveydesign, svymean, na.rm=TRUE, deff=TRUE, keep.var=TRUE)))

    output2$question <- question
    output2$byvar <- geography
    output2$row_var <- row.names(output2)
    
        assign(paste0(question,"_by_", geography,"_ci"), data.frame(output2), envir = .GlobalEnv)
    
    
  }  #end of CI if statement
  
  
  
}






#### As above, but for info questions. Only difference is that it doesn't code it into PNN.



single_question_bygeo_info <- function(question_number, geography, wt, ci_flag){
  question <- question_number
  section_weight <- wt
  
  
  # pull out the required info from the respondents data set
  responses <- hace2122 %>%
    select(patientid_sg, all_of(question), gp_prac_no, all_of(section_weight), eligible_pats, all_of(geography)) %>%
    rename(analysis_var = question,
           analysisweight = section_weight,
           analysisby = geography)   
  
  responses$analysisby <- ifelse(is.na(responses$analysisby),999, responses$analysisby)
  
  
  # this may not be necessary  
  responses$analysis_var <-as.factor(responses$analysis_var) 
  responses$analysisby <-as.factor(responses$analysisby)
  
  responses <- filter(responses, !is.na(responses$analysis_var))
  
  
  # for checking  
  # assign("responses", data.frame(responses), envir = .GlobalEnv)
  
  
  # set up the survey design
  options(survey.lonely.psu="remove")
  surveydesign<-svydesign(id=~1, strata=~gp_prac_no, fpc=~eligible_pats, 
                          weights=~analysisweight, data=responses)
  
  
  # run survey analysis and output to a data set
  output <- as.data.frame(svyby(~analysis_var, by=~analysisby, design=surveydesign, svymean, na.rm=TRUE, deff=TRUE, keep.var=TRUE))
  
  output$question <- question
  output$byvar <- geography
  output$row_var <- row.names(output)
  
  assign(paste0(question,"_by_", geography), data.frame(output), envir = .GlobalEnv)
  
  
  
  # produce confidence intervals
  if (confidence_intervals=="Y") {
    
    output2 <- as.data.frame(confint(svyby(~analysis_var, by=~analysisby, design=surveydesign, svymean, na.rm=TRUE, deff=TRUE, keep.var=TRUE)))
    
    output2$question <- question
    output2$byvar <- geography
    output2$row_var <- row.names(output2)
    
    assign(paste0(question,"_by_", geography,"_ci"), data.frame(output2), envir = .GlobalEnv)
    
    
  }  #end of CI if statement
  
  
}
