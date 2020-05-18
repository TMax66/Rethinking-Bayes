library(tibble)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggeffects)
library(broom)
library(gridExtra)
library(cowplot)

dt<-tibble(group=c(rep("Control", 5), rep("Treatment",5)), y=c(721,882.88,890.98,840.91,935.69,1060.71,1009.84,1208.20,1209.05,1350.41))
dt<-dt %>% data.frame()

z<-dt %>% 
  ggplot(aes(x=group, y=y))+geom_boxplot(fill="firebrick4")+labs(x="")+
  geom_jitter(aes(), alpha=0.9, 
              position=position_jitter(w=0.1,h=0.1))+coord_flip()


fit<-lm(y~group, data=dt)
fit%>% 
tidy(conf.int = TRUE) %>%
  filter(term=="groupTreatment") %>% 
  ggplot(aes(term, estimate))+
  geom_point()+labs(x="", y="effect size")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  coord_flip()#+scale_y_continuous(limits=c(-10,50))

plot_grid(x,z,ncol = 1, align = 'v')

plot_grid((dt %>% 
            ggplot(aes(x=group, y=y))+geom_boxplot(fill="firebrick4")+labs(x="")+
            geom_jitter(aes(), alpha=0.9, 
                        position=position_jitter(w=0.1,h=0.1))+coord_flip()),
          (fit%>% 
            tidy(conf.int = TRUE) %>%
            filter(term=="groupTreatment") %>% 
            ggplot(aes(term, estimate))+
            geom_point()+labs(x="", y="effect size")+
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
            coord_flip()), ncol=1,align='v')



###############################################
#################BAYES#########################
###############################################

library(rethinking)


