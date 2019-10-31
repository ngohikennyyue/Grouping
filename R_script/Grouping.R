library(tidyverse)
library(dplyr)
library(readxl)
library(animation)

student_fall2019 <- read_excel("Raw Data/student_fall2019.xlsx")

student_fall2019 %>% 
  group_by(USER_Gender) %>% 
  summarise(total = n()) %>% mutate(ratio = total /sum(total))

student_fall2019 %>% 
  ggplot() +
  geom_point(aes(x = X, y = Y))

M_student <- student_fall2019 %>% 
  filter(USER_Gender == "M")
F_student <- student_fall2019 %>% 
  filter(USER_Gender == "F")

# Initially divide seperate into 10 groups
set.seed(50)
kmeans.ani(M_student[16:17], 50) 
kmeans.ani(tile1[16:17], 10)
kmeans.ani(F_student[16:17],10)

# Divide the Male and Female in 50 groups and try to mix and match 
M1 <- kmeans(M_student[16:17],50)
M_student %>% 
  mutate(subgroup = M1$cluster) %>% 
  group_by(subgroup) %>% 
  summarise(Avg_x = mean(X), Avg_y = mean(Y))

F1 <- kmeans(F_student[16:17],50)
F_student %>% 
  mutate(subgroup = F1$cluster) %>% 
  group_by(subgroup) %>% 
  summarise(Avg_x = mean(X), Avg_y = mean(Y))

# 1st try
M_student %>% 
  mutate(subgroup = kmeans(M_student[16:17], 15)$cluster) %>%
  filter( subgroup == 1) %>% 
  mutate(sub_subgroup = kmeans(.[16:17],10)$cluster) %>% view()
  
# 2nd Try
  M_student %>% 
    mutate(subgroup = kmeans(M_student[16:17], 15)$cluster) %>%
    group_by(subgroup) %>% 
    summarise( Avg_x = mean(X), Avg_y = mean(Y)) %>%  ggplot(aes(x = Avg_x, y = Avg_y)) +geom_point()
F_student %>% mutate(subgroup = kmeans(M_student[16:17], 12)$cluster) %>% view()
