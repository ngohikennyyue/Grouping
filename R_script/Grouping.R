library(tidyverse)
library(stats)
library(base)
library(dplyr)
library(readxl)
library(animation)
student_fall2019 <- read_excel("Raw Data/student_fall2019.xlsx")

student_fall2019 %>% 
  group_by(USER_Gender) %>% 
  summarise(total = n())

student_fall2019 %>% 
  ggplot() +
  geom_point(aes(x = X, y = Y))

M_student <- student_fall2019 %>% 
  filter(USER_Gender == "M")
F_student <- student_fall2019 %>% 
  filter(USER_Gender == "F")

set.seed(2345)
kmeans.ani(M_student[16:17], 10)
kmeans(M_student[16:17], 10)$cluster
M_student %>% mutate(subgroup = kmeans(M_student[16:17], 10)$cluster) %>% view()

