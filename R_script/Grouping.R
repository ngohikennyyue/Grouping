pacman::p_load(tidyverse, dplyr, readxl, animation, factoextra, sp, SearchTrees)

student_fall2019 <- read_excel("Raw Data/Fall2019.xlsx")

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

# Male cluster plot
Man1 <- kmeans(M_student[16:17], centers = 50, nstart = 25)
fviz_cluster(Man1, geom = "point", data = M_student[16:17])

# Female cluster plot
Fem1 <- kmeans(F_student[16:17], centers = 50, nstart = 25)
fviz_cluster(Fem1, geom = "point", data = F_student[16:17])

# Divide the Male and Female in 50 groups and try to mix and match 
M1 <- kmeans(M_student[16:17],50)
M2 <- M_student %>% 
  mutate(cluster = M1$cluster) %>% 
  group_by(cluster) %>% 
  summarise(x = mean(X), y = mean(Y))

F1 <- kmeans(F_student[16:17],50)
F2 <- F_student %>% 
  mutate(cluster = F1$cluster) %>% 
  group_by(cluster) %>% 
  summarise(x = mean(X), y = mean(Y))

tree <- createTree(coordinates(M2))
inds <- knnLookup(tree, newdat = coordinates(F2), k=1)

plot(M2$x,M2$y, pch=1, cex=1.2)
points(F2$x,F2$y, col=c("blue", "red", "green","red","blue","green"), pch=17, cex=1.5)
points(M2[inds[1:50],], pch=12)

# 1st try
M_student %>% 
  mutate(subgroup = kmeans(M_student[16:17], 15)$cluster) %>%
  filter( subgroup == 1) %>% 
  mutate(sub_subgroup = kmeans(.[16:17],10)$cluster) %>% view()
  
# 2nd Try
  M_student %>% 
    mutate(subgroup = kmeans(M_student[16:17], 15)$cluster) %>%
    group_by(subgroup) %>% 
    summarise( Avg_x = mean(X), Avg_y = mean(Y)) %>%  
    ggplot(aes(x = Avg_x, y = Avg_y)) +
    geom_point()
  
F_student %>% mutate(subgroup = kmeans(M_student[16:17], 12)$cluster) %>% view()
