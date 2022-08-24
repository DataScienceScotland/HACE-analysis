#Additional analysis:

#NOTE: Uses export function from Q32_Q35.R

#Q38 by Q35 - Caring intensity
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 201, last_iref = 205, by_var <- 'q35'

Q38_by_Q35 <- rbind(q38a_by_q35, q38b_by_q35, q38c_by_q35, q38d_by_q35, q38e_by_q35) 


exportfun("Q38_by_Q35", "Q38/")

##########################

#Q38 by Q37G - support/help in caring role
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 201, last_iref = 205, by_var <- 'q37g'

#Q38 by Q37G
Q38_by_Q37G <- rbind(q38a_by_q37g, q38b_by_q37g, q38c_by_q37g, q38d_by_q37g, q38e_by_q37g) 

exportfun("Q38_by_Q37G", "Q38/")

##########################

##merging to add to markdown file

finalfig<- rbind(Q38_by_Q35, Q38_by_Q37G)%>% 
  mutate(question = case_when(question == "q38a" ~ "I have a good balance between caring and other things in my life",
                              question == "q38b" ~ "Caring has had a negative impact on my health and wellbeing",
                              question == "q38c" ~ "I have a say in services provided for the person(s) I look after",
                              question == "q38d" ~ "Local services are well coordinated for the person(s) I look after",
                              T ~ "I feel supported to continue caring"),
         Intensity = case_when((var ==1 & byvar == "q35") ~ "Up to 4 hours a week",
                               var %in% c(2,3)  ~"5 to 35 hours a week",
                               var %in% c(4,5)~"Over 35 hours a week",
                               var == 0 ~ "Help/support received",
                               (var == 1 & byvar == "q37g") ~ "No help/support received",
                               T~"?"),
         `Disagree/strongly disagree` = posneutneg..negative,
         `Neither agree/disagree` = posneutneg..neutral,
         `Strongly agree/agree` = posneutneg..positive) %>% 
  select(11:17) %>% 
  pivot_longer(names_to = "cat", values_to = "prop", 5:7) %>% 
  filter(Intensity != "?") %>% 
  group_by(question, Intensity, cat) %>% 
  summarise(proportion = round(mean(prop)*100, 0)) %>% 
  mutate(order = case_when(Intensity=="Up to 4 hours a week" ~1,
                           Intensity=="5 to 35 hours a week"~2,
                           Intensity=="Help/support received" ~4,
                           Intensity=="No help/support received"~5,
                           T~3)) %>% 
  ggplot(aes(x=reorder(Intensity,order), y=proportion, fill =cat))+
  geom_col(position = "stack")+
  plot_theme +
  coord_flip()+
  facet_wrap(~question, ncol=2) +
  geom_text(aes(label = paste0(proportion,"%")), position = position_stack(vjust =.5)) +
  guides(fill = guide_legend(title = "",
                             reverse = T))+
  scale_fill_brewer(palette = "Blues") +
  geom_vline(xintercept = 3.5, size = 2)
  

#Q37 by Q35 - Caring intensity
## Generated from 08 - National Results by a non-geographic variable do loop.R
##first_iref = 194, last_iref = 200, by_var <- 'q35'

View(rbind(q37a_by_q35,q37b_by_q35,q37c_by_q35,q37d_by_q35,q37e_by_q35,q37f_by_q35,q37g_by_q35) %>% 
  mutate(varname = case_when(question == "q37a"~"A written plan about your caring role and support available to you",
                      question == "q37b"~"Help from family/friends/neighbours",
                      question == "q37c"~"Help from Carer Centre/local organisation",
                      question == "q37d"~"Break(s) from caring",
                      question == "q37e"~"Services for the person(s) you care for, to enable you to have a break",
                      question == "q37f"~"Other support",
                      T~"No support/help"),
         variable = case_when(
           var == 1 ~ "Up to 4 hours a week",
           var == 2 ~ "5-19 hours a week",
           var == 3 ~ "20-34 hours a week",
           var == 4 ~ "35-49 hours a week",
           var == 5 ~ "50 or more hours a week",
           T ~ "No"),
         Yes = round(analysis_var1*100,1),
         No= round(analysis_var0*100,1)) %>% 
  pivot_longer(13:14, names_to = "category", values_to = "prop") %>% 
  select(11,12,13,14))

  

q37_by_35 <- rbind(q37a_by_q35,q37b_by_q35,q37c_by_q35,q37d_by_q35,q37e_by_q35,q37f_by_q35,q37g_by_q35) 

exportfun("q37_by_35", "Q37/")

