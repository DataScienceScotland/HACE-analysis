# survey v4.1-1
library(survey)



# Two functions created in this program - one for positive, neutral and negative, and another for info questions
# This creates functions that produce national level results for the chosen question
  # question_number - e.g. q01....q42 Case sensitive. Refer to the "question_data" data frame to identify the format.
  # wt - the weight to be used for analysis.


single_question_pnn <- function(question_number, wt){
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
    select(patientid_sg, all_of(question), gp_prac_no, all_of(section_weight), eligible_pats) %>%
    rename(analysis_var = question,
           analysisweight = section_weight)
  

# recode responses into positive, neutral or negative
  responses$posneutneg <- ifelse(responses$analysis_var %in% c(high_positive_pnn,low_positive_pnn),"% positive",
                                 ifelse(responses$analysis_var %in% c(neutral_values_pnn), "% neutral",
                                        ifelse(responses$analysis_var %in% c(low_negative_pnn,high_negative_pnn),"% negative",NA)))
  responses$posneutneg <- ifelse(is.na(responses$analysis_var), NA, responses$posneutneg)
  

  responses <- filter(responses, !is.na(responses$posneutneg))

  
# set up the survey design
  options(survey.lonely.psu="remove")
  surveydesign<-svydesign(ids=~1, strata=~gp_prac_no, fpc=~eligible_pats, 
                          weights=~analysisweight, data=responses)
  
  
# run survey analysis and output to a data set
  output <- as.data.frame(svymean(~posneutneg, surveydesign, na.rm=TRUE, deff=TRUE))
  output$question <- question
  output$var <- row.names(output)
  rownames(output) <- c()
  frequencies <- as.data.frame(table(responses$posneutneg))
  frequencies$question <- question
  assign("output", data.frame(output), envir = .GlobalEnv)
  assign("frequencies", data.frame(frequencies), envir = .GlobalEnv) 
  
# produce confidence intervals
    output2 <- as.data.frame(confint(svymean(~posneutneg, surveydesign, na.rm=TRUE, deff=TRUE)))
    output2$question <- question
    output2$var <- row.names(output2)
    rownames(output2) <- c()
    assign("output2", data.frame(output2), envir = .GlobalEnv)
    
     

  
  
  #this is just for checking
#  assign("analysis_data", data.frame(analysis_data), envir = .GlobalEnv)
}







##### Second function for information questions.
##### Basically the same as the above, but doesn't code it into PNN


single_question_info <- function(question_number, wt){
  question <- question_number
  section_weight <- wt
  

# select required info from the respondents data set
  responses <- hace2122 %>%
    select(patientid_sg, all_of(question), gp_prac_no, all_of(section_weight), eligible_pats) %>%
    rename(analysis_var = question,
           analysisweight = section_weight)
  
  responses$analysis_var <- as.character(responses$analysis_var)
  
  responses <- filter(responses, !is.na(responses$analysis_var))
  
# set up survey design
  options(survey.lonely.psu="remove")
  surveydesign<-svydesign(ids=~1, strata=~gp_prac_no, fpc=~eligible_pats, 
                          weights=~analysisweight, data=responses)
  
  
  
# run survey analysis and output to a data set
  output <- as.data.frame(svymean(~analysis_var, surveydesign, na.rm=TRUE, deff=TRUE))
  output$question <- question
  output$var <- row.names(output)
  rownames(output) <- c()
  frequencies <- as.data.frame(table(responses$analysis_var))
#  frequencies$Var1 <- as.numeric(frequencies$Var1)
  frequencies$question <- question
  frequencies$var <- as.numeric(paste0(frequencies$Var1))
  
  frequencies <- frequencies %>%
    select(Freq, question, var)
  
  frequencies <- frequencies  %>%
    rename(Var1 = var)
  
  assign("output", data.frame(output), envir = .GlobalEnv)
  assign("frequencies", data.frame(frequencies), envir = .GlobalEnv)
  
  
  # produce confidence intervals
    output2 <- as.data.frame(confint(svymean(~analysis_var, surveydesign, na.rm=TRUE, deff=TRUE)))
    output2$question <- question
    output2$var <- row.names(output2)
    rownames(output2) <- c()
    assign("output2", data.frame(output2), envir = .GlobalEnv)

  
  
  
  #this is just for checking
#  assign("analysis_data", data.frame(analysis_data), envir = .GlobalEnv)
} 
