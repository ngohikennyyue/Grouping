pacman::p_load(tidyverse, dplyr, readxl, animation, factoextra, sp, SearchTrees, geosphere, fields, rlist)

student_fall2019 <- read_excel("Raw Data/Fall2019.xlsx")

Tab <- student_fall2019 %>% 
  mutate(Name = str_c(USER_First_Name,",",USER_Last_Name)) %>% 
  select(Name, X, Y, USER_Gender, USER_Complex) 

distance = function(long1, lat1, long2, lat2, units = "miles") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

Tab %>% 
  mutate("Logan Kay,Garner" = distance(Tab$X, Tab$Y,-111.7871, 43.82206)) %>% view
N = nrow(Tab)
i <- 1:N
mutate(Tab[i] = distance(Tab$X, Tab$Y,Tab$X[i], Tab$Y[i]))
  for (i in 1:N){
    mutate(Tab, Tab$Name[i] == distance(Tab$X, Tab$Y,Tab$X[1], Tab$Y[1]) )
  }
point <- as.point
