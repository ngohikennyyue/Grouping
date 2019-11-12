pacman::p_load(tidyverse, dplyr, readxl, animation, factoextra, sf, SearchTrees, geosphere, fields, rlist,raster)
# Read in files
student_fall2019 <- read_excel("Raw Data/Fall2019.xlsx")

# Assigning file to the name and filter out the necessary details
Tab <- student_fall2019 %>% 
  mutate(Name = str_c(USER_First_Name,",",USER_Last_Name)) %>% 
  select(Name, X, Y, USER_Gender, USER_Complex) %>% 
  rename(Longitude = X, Latitude = Y)


# This is the funtion that was found online to do the calculation 
# with one set point and distanc with other points.

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
  mutate("Logan Kay,Garner" = distance(Tab$Longitude, Tab$Latitude,Tab$Longitude[1], Tab$Latitude[1])) %>% view
N = nrow(Tab)
i <- 1:N
mutate(Tab[i] = distance(Tab$X, Tab$Y,Tab$X[i], Tab$Y[i]))


library(sf)
locations <- st_as_sf(Tab, coords = c("Longitude","Latitude"))
locations$geometry
C <- data_frame(locations$geometry)
B <- matrix(locations$geometry, ncol = 1) 
A <- cbind(B, C)

# USe Raster function to create a sort of distance between points matrix
library(raster)
point <- pointDistance(Tab[, c("Longitude", "Latitude")], lonlat = TRUE)
mm <- as.matrix(as.dist(point)) #This might be the matrix of distance that would work
i <-  apply(mm, 1, which.min)
p <-  cbind(1:nrow(mm),i)

Final_dat <- bind_cols(locations,)
