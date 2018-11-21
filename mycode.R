library(tibble)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggeffects)
library(broom)
library(gridExtra)
library(cowplot)

dt<-tibble(group=c(rep("Control", 5), rep("Treatment",5)), y=c(721,882.88,890.98,840.91,935.69,1060.71,1009.84,1208.20,1209.05,1350.41)/10)
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







dt<-tibble(group=c(rep("Control", 8), rep("Treatment",8)), 
    y=c(4.691,9.952,9.602,7.273,7.808,8.882,7.688,5.303,
        4.443,11.451,8.732,4.174,6.138,7.485,7.057,8.517))
dt<-dt %>% data.frame()

ggplot(dt, aes(x=group, y=y))+geom_boxplot()


library(brms)

x<-brm(data = dt, family = gaussian,
   y ~ 1+group,
    # prior = c(prior(normal(1000, 10), class = Intercept),
    #           prior(uniform(0, 200), class = sigma)),
    iter = 31000, warmup = 30000, chains = 4, cores = 4)

post<-posterior_samples(x)

post %>%
  select(b_Intercept, sigma) %>%
  cor()

post %>%
  transmute(gruppo = b_Intercept + b_groupTreatment) 




nd <- tibble(group = "Treatment")

fitted(x,
       newdata = nd)


newdt <- read_excel("newdt.xlsx")


xx<-brm(data = newdt, family = gaussian,
       y ~ 1+group,
       prior = c(prior(normal(85, 5), class = Intercept),
                 prior(normal(30,10),class=b),
                 prior(cauchy(5, 15), class = sigma)),
       iter = 31000, warmup = 30000, chains = 4, cores = 4)

