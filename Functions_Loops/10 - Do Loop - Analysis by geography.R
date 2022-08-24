# Analyse selected questions by geography and output the results to a data set. 
# One data set per question selected - it does not aggregate output files.

# Start and end point of the do loop min=101 max=222
# Each loop takes roughly 20 seconds to run on our laptops, although this varies depending on the question
# refer to the questionnaire mapping document to map irefs to questions
first_iref = 104
last_iref = 104

# Do you want to output confidence intervals. "Y" for yes. Anything else for no. It's not optimised code and will add considerably to the runtime.
confidence_intervals = "N"

# specify the geography that we want the above questions analysed by
# needs to be either 'nat', 'hb', 'hscp', 'gpcl' or 'gp'
geography = 'hb'





i = first_iref


while (i<=last_iref) {
  
  # select the row of the question data set that contains the question we want
  question_data <- filter(hace2122_questions, iref==i)
  
  # identify the question number, type and weight based on info from the question data set
  question_no <- question_data$question_no
  section_weight <- paste0(geography,question_data$weight)
  question_type <- question_data$question.type  
  
  
  # select correct function depending on whether it's a percentage positive or information question
  if (question_type %in% c("Percent positive")) {
    single_question_bygeo_pnn(question_number=question_no, geography=geography, wt=section_weight, ci_flag=confidence_intervals)
  } else {
    single_question_bygeo_info(question_number=question_no, geography=geography, wt=section_weight, ci_flag=confidence_intervals)
  } # end of if statement
  
  
  i <- i+1
  
} # end of loop


