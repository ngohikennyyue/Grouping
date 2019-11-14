pacman::p_load(tidyverse, dplyr)
# Read in files

student_fall2019 <- read_csv("https://raw.githubusercontent.com/ngohikennyyue/Grouping/master/Raw%20Data/student_fall2019.csv")

# Assigning file to the name and filter out the necessary details
Tab <- student_fall2019 %>% 
  mutate(Name = str_c(USER_First_Name,", ",USER_Last_Name)) %>% 
  select(Name, X, Y,USER_Gender, USER_Complex) %>% 
  rename(Longitude = X, Latitude = Y, Gender = USER_Gender, Complex = USER_Complex)
  

# This is the funtion that was found online to do the calculation 
# with one set point and distanc with other points.

# ratio of Man to Female
ratio <- Tab %>% summarise(pct.male = sum(Tab$Gender == 'M')/ length(Tab$Gender),
                  pct.female = sum(Tab$Gender == 'F')/ length(Tab$Gender))

# USe Raster function to create a sort of distance between points matrix
library(raster)
point <- pointDistance(Tab[c("Longitude", "Latitude")], lonlat = TRUE)
mm <- as.matrix(as.dist(point)) #This might be the matrix of distance that would work

i <-  apply(mm, 1, which.min)
p <-  cbind(1:nrow(mm),i)

Final_dat <- bind_cols(locations,)
