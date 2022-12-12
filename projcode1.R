library(tidyverse)
library(trackter)

videos <- list.files(pattern="csv")

vid1 <- read_csv(videos[1]) %>% 
  select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
  rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
  pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
  mutate(d23=dist.2d(x_2,x_3,y_2,y_3),
         d34=dist.2d(x_3,x_4,y_3,y_4),
         gap=dist.2d(x_1,x_2,y_1,y_2),
         d42=dist.2d(x_4,x_2,y_4,y_2)) %>%
  na.omit %>% 
  group_by(frame) %>% 
  mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
  ungroup() %>% 
  mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
  mutate(hy_exp=dist.2d(x_5,y_3,x_5,y_5)/dist.2d(x_2,y_2,x_3,y_3)) %>% 
  mutate(hy_exp2=abs(hy_exp-first(hy_exp,1)))

vid2 <- read_csv(videos[2]) %>% 
  select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
  rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
  pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
  mutate(d23=dist.2d(x_2,x_3,y_2,y_3),
         d34=dist.2d(x_3,x_4,y_3,y_4),
         gap=dist.2d(x_1,x_2,y_1,y_2),
         d42=dist.2d(x_4,x_2,y_4,y_2)) %>%
  na.omit %>% 
  group_by(frame) %>% 
  mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
  ungroup() %>% 
  mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
  mutate(hy_exp=dist.2d(x_5,y_3,x_5,y_5)/dist.2d(x_2,y_2,x_3,y_3)) %>% 
  mutate(hy_exp2=abs(hy_exp-first(hy_exp,1)))

vid3 <- read_csv(videos[3]) %>% 
  select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
  rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
  pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
  mutate(d23=dist.2d(x_2,x_3,y_2,y_3),
         d34=dist.2d(x_3,x_4,y_3,y_4),
         gap=dist.2d(x_1,x_2,y_1,y_2),
         d42=dist.2d(x_4,x_2,y_4,y_2)) %>%
  na.omit %>% 
  group_by(frame) %>% 
  mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
  ungroup() %>% 
  mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
  mutate(hy_exp=dist.2d(x_5,y_3,x_5,y_5)/dist.2d(x_2,y_2,x_3,y_3)) %>% 
  mutate(hy_exp2=abs(hy_exp-first(hy_exp,1)))

vid4 <- read_csv(videos[4]) %>% 
  select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
  rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
  pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
  mutate(d23=dist.2d(x_2,x_3,y_2,y_3),
         d34=dist.2d(x_3,x_4,y_3,y_4),
         gap=dist.2d(x_1,x_2,y_1,y_2),
         d42=dist.2d(x_4,x_2,y_4,y_2)) %>%
  na.omit %>% 
  group_by(frame) %>% 
  mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
  ungroup() %>% 
  mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
  mutate(hy_exp=dist.2d(x_5,y_3,x_5,y_5)/dist.2d(x_2,y_2,x_3,y_3)) %>% 
  mutate(hy_exp2=abs(hy_exp-first(hy_exp,1)))


vid5 <- read_csv(videos[5]) %>% 
  select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
  rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
  pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
  mutate(d23=dist.2d(x_2,x_3,y_2,y_3),
         d34=dist.2d(x_3,x_4,y_3,y_4),
         gap=dist.2d(x_1,x_2,y_1,y_2),
         d42=dist.2d(x_4,x_2,y_4,y_2)) %>%
  na.omit %>% 
  group_by(frame) %>% 
  mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
  ungroup() %>% 
  mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
  mutate(hy_exp=dist.2d(x_5,y_3,x_5,y_5)/dist.2d(x_2,y_2,x_3,y_3)) %>% 
  mutate(hy_exp2=abs(hy_exp-first(hy_exp,1)))

# as hy_exp2 increases it means the bottom of the gullet is farther down



vid1 %>% 
  ggplot(aes(frame,CE_alpha2))+geom_point()

diff(range(vid1$CE_alpha2))

vid2 %>% 
  ggplot(aes(frame,CE_alpha2))+geom_point()

diff(range(vid2$CE_alpha2))

vid3 %>% 
  ggplot(aes(frame,CE_alpha2))+geom_point()

diff(range(vid3$CE_alpha2))

vid4 %>% 
  ggplot(aes(frame,CE_alpha2))+geom_point()

diff(range(vid4$CE_alpha2))

vid5 %>% 
  ggplot(aes(frame,CE_alpha2))+geom_point()

diff(range(vid5$CE_alpha2))


range(vid5$gap)
