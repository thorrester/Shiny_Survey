library(tidyverse)
library(ggpubr)
library(dplyr)
library(corrplot)
#1 import data
df <- read.csv("Data/df.csv")

#2 Subset name, dept name, and all curvey features including avg score
selected_vars <- c("Employee.Name", "Dept", "Delivering.Results", 
                   "Problem.Solving", "Knowledge.Skills", "Service", "Building.Trust",
                   "Collaboration", "Communication", "Taking.Initiative", "Avg.Rating")
new_df <-df[selected_vars]

#3 Rename Columns

colnames(new_df) <- c("Employee", "Department", "Delivering Results",
                         "Problem Solving", "Knowledge Skills", "Service", "Building Trust",
                         "Collaboration", "Communication", "Taking Initiative",
                         "Average Rating")

#4 Gather all survey features into one column

df_gather<-gather(new_df, key = "question", value = "score", 'Delivering Results':'Average Rating', factor_key = TRUE)

#5 Changing score to a factor level
df_gather$score <- as.factor(df_gather$score)


#6 Grouping by Dept and question, and then counting each level of score of a given question

df_gather_count <- df_gather %>% 
  dplyr::select(2:4) %>% 
  group_by(Department, question) %>% 
  count(score) %>% 
  arrange(Department)


#7  Adding proportional table on

df_gather_prop <- df_gather_count %>% 
                mutate(prop = prop.table(n)*100) %>% 
                mutate(group = ifelse(score == "0" | score =="1" | score == "2"| score == "3",
                "Less than 4", "4 or Greater"))

write.csv(df_gather_prop, "C:/Users/Test/Box Sync/OD Data/dashboard/df.csv")


