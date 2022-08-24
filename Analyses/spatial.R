library(tmap)
library(sf)

##Visualising care funding source by NHS Health Board

map <- read_sf("C:/Users/u449921/Documents/Workforce/Survey work/NHS_healthboards_2019/SG_NHS_HealthBoards_2019.shp")

HB_join <- hace2122 %>% 
  select(hb_name, hb) %>% 
  mutate(row_var = hb) %>% 
  distinct() %>% 
  select(-hb)

HB_stats <- rbind(q30a_by_hb,
                  q30b_by_hb,
                  q30c_by_hb,
                  q30d_by_hb,
                  q30e_by_hb) %>%
  left_join(HB_join, by = "row_var") %>%
  mutate(
    `0` = round(analysis_var0 * 100, 1),
    Yes = round(analysis_var1 * 100, 1),
    HBCode = row_var,
    varname = case_when(
      question == "q30a" ~ "Council",
      question == "q30b" ~ "Me/family",
      question == "q30c" ~ "NHS",
      question == "q30d" ~ "Other",
      T ~ "Unpaid care from friends/family")) %>%
  select(11:15) %>% 
  filter(varname %in% c("Me/family", "Unpaid care from friends/family"))

test <- left_join(map, HB_stats, by="HBCode") %>%  
  tm_shape()+
  tm_polygons("Yes", 
              title = "Proportion of respondents",
              style = 'cont',
              palette = 'Blues')+
  tm_facets('varname') +
  tm_layout(legend.outside.position =  "bottom",
            main.title = "Map 1: Care funding source by NHS Health Board, 2021-22", 
            main.title.size=1.5) 

tmap_save(test, 
          units = 'in',
          height = 7,
          width =7,
          filename = "C:/Users/u449921/Documents/HACE/markdown/test.png")
