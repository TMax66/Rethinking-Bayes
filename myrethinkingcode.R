library(rethinking)
library(tibble)
library(dplyr)

pgrid<-seq(from=0, to=1, length.out = 10000)
prior<-rep(1, 10000)
likelihood<-dbinom(6, size=9, prob=pgrid)
post<-likelihood*prior
post<-post/sum(post)
plot(pgrid, post, type="b")


####################SAMPLING THE POSTERIOR##################

###describing posterior####
samples<-sample(pgrid, prob=post, size = 1e4, replace=TRUE)
dens(samples)#dens()
PI(samples, prob = .95)#PI()
HPDI(samples, prob=0.5)
chainmode(samples, adj=0.01)
mean(samples)
median(samples)

######sampling to simulate prediction!!!###########
###generare osservazioni implicite da un modello###########

w<-rbinom(1e4, size=9, prob=samples)
simplehist(w)
#########################################################



######a simple gaussian model####
##### mean and sd estimation from a sample of values observed#####
data("Howell1")
d<-Howell1
d2<-d[d$age >=18,]
d2$height 
#####modello###
# h~Normal(mu, sigma)
#prior: 
# mu~Normal(178,20)
# sigma~Uniform(0, 50)

##studiare la prior scelta#####
curve(dnorm(x, 178, 20), from=100, to=250)#distribuzione della prior per mu###

###simulazione delle altezze utilizzando le prior del modello####
samplemu<-rnorm(1e4,178,20)
samplesigma<-runif(1e4, 0,50)
priorh<-rnorm(1e4,samplemu,samplesigma)
dens(priorh)

####fit the model#####
### 1. MAP approx###
flist<-alist(
  height~dnorm(mu, sigma),
  mu~dnorm(178, 20),
  sigma~dunif(0,50))

fit<-map(flist, data=d2)

##sample the posterior##

precis(fit)
vcov(fit)

post<-extract.samples(fit)
precis(post)
