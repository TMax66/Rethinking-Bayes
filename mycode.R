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
  ggplot(aes(x=group, y=y))+geom_boxplot(fill="firebrick4")+
  geom_jitter(aes(), alpha=0.9, 
              position=position_jitter(w=0.1,h=0.1))+coord_flip()


fit<-lm(y~group, data=dt)
coef <- tidy(fit, conf.int = TRUE)

x<-ggplot(coef[2,], aes(term, estimate))+
  geom_point()+labs(x="")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  coord_flip()

plot_grid(x,z,ncol = 1, align = 'v')

library(rethinking)
flist <- alist(
  y ~ dnorm( mu , sigma ) ,
  mu ~ dunif( 0,2000 ) ,
  sigma ~ dunif( 0,500 )
)
slist <- list(
  mu=mean(dt$y),
  sigma=sd(dt$y)
)

bayes <- map( flist , data=newdt , start=slist)

library(brms)

x<-brm(data = dt, family = gaussian,
   y ~ 1+group,
    # prior = c(prior(normal(1000, 10), class = Intercept),
    #           prior(uniform(0, 200), class = sigma)),
    iter = 31000, warmup = 30000, chains = 4, cores = 4)



newdt <- read_excel("newdt.xlsx")


xx<-brm(data = newdt, family = gaussian,
       y ~ 1+group,
       prior = c(prior(normal(85, 5), class = Intercept),
                 prior(normal(30,10),class=b),
                 prior(cauchy(5, 15), class = sigma)),
       iter = 31000, warmup = 30000, chains = 4, cores = 4)

