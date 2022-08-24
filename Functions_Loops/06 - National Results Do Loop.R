# Produce the National Results dataset

# Requirements
  # hace2122 data frame
  # hace2122_questions data frame
  # single_question_info function
  # single_question_pnn function



# Start and end point of the do loop min=101 max=222 - refer to the "question_data" data frame to identify the irefs of interest
# Each loop takes roughly 20 seconds to run on our laptops, although this varies depending on the question
# refer to the questionnaire mapping document to map irefs to questions
first_iref = 101
last_iref = 222

# will need to specify the geography that we want this run at - right now this just selects the correct weight rather than does any 'by' analysis
# needs to be either 'nat', 'hb', 'hscp', 'gpcl' or 'gp'
geography = 'nat'



i = first_iref


national_results_output = NULL
national_frequencies = NULL
national_results_ci_output = NULL
  



while (i<=last_iref) {

# select the row of the question data set that contains the question we want
question_data <- filter(hace2122_questions, iref==i)

# identify the question number, type and weight based on info from the question data set
question_no <- question_data$question_no
section_weight <- paste0(geography,question_data$weight)
question_type <- question_data$question.type


# create a data frame containing the response options for this question based on the hace2122_questions data frame
response_options <- as.data.frame(strsplit(question_data$response.options, ";"))
colnames(response_options) <- c("responses")
response_options <- response_options %>% separate(responses, c("code", "response_text"), " = ")
response_options$code <- as.numeric(response_options$code)


# select correct function depending on whether it's a percentage positive or information question
if (question_type %in% c("Percent positive")) {
      single_question_pnn(question_number=question_no, wt=section_weight)
  
  
  output$response_text[output$var == "posneutneg% negative"] <- "negative"
  output$response_text[output$var == "posneutneg% positive"] <- "positive"
  output$response_text[output$var == "posneutneg% neutral"] <- "neutral"
  
  output$code <- NA


    output2$response_text[output2$var == "posneutneg% negative"] <- "negative"
    output2$response_text[output2$var == "posneutneg% positive"] <- "positive"
    output2$response_text[output2$var == "posneutneg% neutral"] <- "neutral"
    
    output2$code <- NA 
    

  
  
  
   } else {
      single_question_info(question_number=question_no, wt=section_weight)
     
     # identify the response code for that line and link the response option text to the file
     output$code <- as.numeric(gsub("analysis_var", "", output$var)) 
     output <- output %>%
       left_join(response_options, by=c('code'))
     
  
       
       output2$code <- as.numeric(gsub("analysis_var", "", output2$var)) 
       output2 <- output2 %>%
         left_join(response_options, by=c('code'))
       
    
     
     
   } # end of if statement

# add info from the question data set to the output file
output$question_type <- question_data$question.type
output$topic <- question_data$topic
output$question_text <- question_data$question.text

  
  output2$question_type <- question_data$question.type
  output2$topic <- question_data$topic
  output2$question_text <- question_data$question.text
  


#formatting
output$percentage <- output$mean*100
output$standard_error <- output$SE*100
output <- output %>% 
  rename(
    design_effect = deff
  )

  
output2$lower_ci <- output2$X2.5..*100
output2$upper_ci <- output2$X97.5..*100



# combine output into a single data set
national_results_output <- rbind(national_results_output, output)
national_frequencies <- rbind(national_frequencies, frequencies)
national_results_ci_output  <- rbind(national_results_ci_output, output2)




i <- i+1

} # end of loop



national_results_output$Var1 <-   ifelse(is.na(national_results_output$code),
                                            ifelse(national_results_output$var %in% c('posneutneg% negative'), '% negative',
                                                   ifelse(national_results_output$var %in% c('posneutneg% neutral'), '% neutral',
                                                          ifelse(national_results_output$var %in% c('posneutneg% positive'), '% positive', NA))),
                                         national_results_output$code)


national_results_ci_output$Var1 <-   ifelse(is.na(national_results_ci_output$code),
                                           ifelse(national_results_ci_output$var %in% c('posneutneg% negative'), '% negative',
                                                  ifelse(national_results_ci_output$var %in% c('posneutneg% neutral'), '% neutral',
                                                         ifelse(national_results_ci_output$var %in% c('posneutneg% positive'), '% positive', NA))),
                                           national_results_ci_output$code)



national_results_output <- national_results_output %>%
            left_join(national_frequencies, by=c('question','Var1')) %>%
            rename(frequencies = Freq)

  
ci_output_for_merge <- national_results_ci_output %>%
    select(upper_ci, lower_ci, question, Var1)
  
national_results_output <- national_results_output %>%
    left_join(ci_output_for_merge, by=c('question','Var1'))



# tidy up the column ordering and drop unnecessary columns

national_results_output <- national_results_output[, 
                                  c("topic", "question", "question_type", "question_text",  
                                   "response_text", "code", "frequencies", "percentage", "standard_error", "design_effect",
                                   "lower_ci","upper_ci")]


#write.csv(national_results_output,file= "C:/TEMP/output.csv", row.names = FALSE)
#write.csv(national_results_ci_output,file= "C:/TEMP/output_ci.csv", row.names = FALSE)



