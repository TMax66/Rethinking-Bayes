library(tibble)
library(dplyr)
dt<-tibble(group=c(rep("0", 5), rep("1",5)), y=c(721,882.88,890.98,840.91,935.69,1060.71,1009.84,1208.20,1209.05,1350.41))
dt<-dt %>% data.frame()
lm(y~group, data=dt)

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

bayes <- map( flist , data=dt , start=slist)

library(brms)

x<-brm(data = dt, family = gaussian,
   y ~ 1,
    # prior = c(prior(normal(1000, 10), class = Intercept),
    #           prior(uniform(0, 200), class = sigma)),
    iter = 31000, warmup = 30000, chains = 4, cores = 4)
