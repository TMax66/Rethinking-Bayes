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
#   h~Normal(mu, sigma)
#prior: 
#            mu~Normal(178,20)
#            sigma~Uniform(0, 50)

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

<<<<<<< HEAD

##########################
##########################
##MULTILEVEL MODEL CH.12####

library(rethinking)
data("reedfrogs")
d<-reedfrogs
str(d)

d$tank<-1:nrow(d)

m12.1<-map2stan(
  alist(
    surv~dbinom(density,p),
    logit(p)<-a_tank[tank],
    a_tank[tank]~dnorm(0,5)
  ),
  data=d
)

##multilevel model###
m12.2<-map2stan(
  alist(
    surv~dbinom(density,p),
    logit(p)<-a_tank[tank],
    a_tank[tank]~dnorm(a,sigma),
    a~dnorm(0,1),
    sigma~dcauchy(0,1)
  ),
  data=d, iter=4000,chains = 4
)

library(ggplot2)
library(rstan)
library(reshape2)
library(dplyr)


data(psid, package="faraway")
d<-psid
psid20 <- filter(psid, person <= 20)
ggplot(psid20, aes(x=year, y=income))+geom_line()+facet_wrap(~ person)
=======
#####add a predictor#####

###modello####

##  h~Normal(mu, sigma)
##  mu=alpha+beta*w

#prior: 
#   alpha~Normal(156,100)
#   beta~Normal(0,10)
#   sigma~Uniform(0, 50)



fit2<-map(alist(
  height~dnorm(mu, sigma),
  mu<-a+b*weight,
  a~dnorm(156,100),
  b~dnorm(0,10),
  sigma~dunif(0,50)), data=d2)

# # ###simulazione delle altezze utilizzando le prior del modello####
# # 
# samplealpha<-rnorm(352, 156, 100)
# samplebeta<-rnorm(352, 0, 10)
# # weight<-rnorm(1e4, 44.99,6.45) <-<-<-<-<- questa ? sbagliata!!!!
# weight<-d2$weight
# samplemu<-samplealpha+samplebeta*weight
# samplesigma<-dunif(352,0,50)
#  
#  priorh<-rnorm(352, samplemu,samplesigma)
#  dens(priorh)

precis(fit2, corr = TRUE)

plot(height~weight, data=d2)
abline(a=coef(fit2)["a"], b=coef(fit2)["b"], col="blue") 
abline(a=115.0808,b=0.8698418, col="red")
abline(a=116.0808,b=0.9098418, col="red")

post<-extract.samples(fit2)

mu50<-post$a+post$b*50 # esempio di calcolo di mu per individui di 50 kg cogliendo l'incertezza di a e beta###
dens(mu50, col=rangi2,xlab="mu|weigth=50")


mu<-link(fit2)

####generare 
>>>>>>> origin/master


###########################
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$A<-scale(d$MedianAgeMarriage)
d$B<-scale(d$Divorce)
d$M<-scale(d$Marriage)

m5.3<-quap(
  alist(
    D~dnorm(mu, sigma),
    mu<-a+bM*M+bA*A,
    a~dnorm(0,0.2),
    bM~dnorm(0,0.5),
    bA~dnorm(0,0.5),
    sigma~dexp(1)
  ), data=d
  )
#################################

data(milk)
d<-milk
str(d)

d$K<-scale(d$kcal.per.g)
d$N<-scale(d$neocortex.perc)
d$M<-scale(log(d$mass))

dcc<-d[complete.cases(d$K,d$N, d$M),]

m5.5d<-quap(
  alist(
    K~dnorm(mu, sigma),
    mu<-a+bN*N,
    a~dnorm(0,0.2),
    bN~dnorm(0,0.5),
    sigma~dexp(1)
  ), data=dcc
)


xseq<-seq(from=min(dcc$N)-0.15, to=max(dcc$N), length.out = 30)
mu<-link(m5.5d, data=list(N=xseq))
mu_mean<-apply(mu, 2, mean)
mu_PI<-apply(mu, 2, PI)
plot(K~N, data=dcc)
lines(xseq, mu_mean, lwd=2, col="blue")
shade(mu_PI, xseq)
