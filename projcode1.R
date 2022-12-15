library(tidyverse)
library(trackter)

videos <- list.files(pattern="csv")

data <- list()
for(i in videos){
  exp <- gsub("Ram_vid(\\d)points.csv","\\1",i) %>% as.numeric
  d <- read_csv(i) %>% 
    select(TID,PID,`x [pixel]`,`y [pixel]`) %>% 
    rename(point=TID,frame=PID,x=`x [pixel]`,y=`y [pixel]`) %>% 
    pivot_wider(names_from = "point", values_from=c("x","y")) %>% 
    mutate(d23=dist.2d(x_1,x_3,y_1,y_3),
           d34=dist.2d(x_3,x_4,y_3,y_4),
           gap=dist.2d(x_1,x_2,y_1,y_2),
           d42=dist.2d(x_4,x_1,y_4,y_1)) %>%
    na.omit %>% 
    group_by(frame) %>% 
    mutate(CE_alpha=deg(cosine.ang(d23,d34,d42))) %>% 
    ungroup() %>% 
    mutate(CE_alpha2=CE_alpha-first(CE_alpha,1)) %>% 
    mutate(hy_exp=dist.2d(x_5,x_5,y_3,y_5)/dist.2d(x_1,x_3,y_1,y_3)) %>% 
    mutate(hy_exp2=abs(hy_exp-first(hy_exp,1))) %>% 
    mutate(gap2=abs(gap-first(gap,1))) %>% 
    mutate(exp=exp)
  
  data[[i]] <- d
}


data2 <- do.call(rbind,data)

data2 %>% 
  ggplot(aes(frame,CE_alpha2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+labs(y = "Cranial Elevation (\u00B0)")

data2 %>% 
  ggplot(aes(frame,CE_alpha2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+facet_wrap(~exp)+labs(y = "Cranial Elevation (\u00B0)")

data2 %>% 
  ggplot(aes(frame,hy_exp2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+labs(y = "Hyoid Expansion")

data2 %>% 
  ggplot(aes(frame,hy_exp2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+facet_wrap(~exp)+labs(y = "Hyoid Expansion")

data2 %>% 
  ggplot(aes(frame,gap2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+labs(y = "Mouth Gap Change")

data2 %>% 
  ggplot(aes(frame,gap2,col=as.factor(exp)))+geom_point()+ guides(color = guide_legend(title = "Trials"))+facet_wrap(~exp)+labs(y = "Mouth Gap Change")



# hy_exp looking at change in distance from imaginary point at top of head to
# bottom of gullet looking at that kind of expansion
# standardized by dividing by size of head to see change 

# CE_alpha is the change in elevation of top jaw -marks "closed" as start in 
# case they tilt head up or down

data2 %>% 
  group_by(exp) %>% 
  summarize(CE_alpha2=diff(range(CE_alpha2)),hy_exp2=diff(range(hy_exp2)),gap2=diff(range(gap)))

diff(range(data2$CE_alpha2))

diff(range(vid5$CE_alpha2))

