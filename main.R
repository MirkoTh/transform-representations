rm(list=ls())
library(mclust)
library(naivebayes)
library(tidyverse)

#read in data
d<-read.csv("data/infpro_stimuli.csv")

X<-as.matrix(cbind(d$d1i, d$d2i))
y<-d$category
m<-MclustDA(X, y, G=1)
predproposal<-predict(m, X)
proploss<-0
proploss<-ifelse(d$category=='A', predproposal$z[,1], proploss)
proploss<-ifelse(d$category=='B', predproposal$z[,2], proploss)
proploss<-ifelse(d$category=='C', predproposal$z[,3], proploss)
priorloss<-sum(log(proploss))

# visualize before
ggplot(d, aes(d1i, d2i, group = category)) + 
  geom_hex(aes(fill = category)) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", name = "Category") +
  labs(
    x = "X1",
    y = "X2"
  )


#initialize two dimenstions
d$prop1<-d$x1<-d$d1i
d$prop2<-d$x2<-d$d2i

#number of runs
nruns<-10000
acceptcount<-0
Xnew<-as.matrix(cbind(d$prop1, d$prop2))
predict(m, Xnew)
for (i in 1: nruns){
  #create X matrix
  X<-as.matrix(cbind(d$x1, d$x2))
  #sample random observation 
  index<-sample(1:nrow(d), 1)
  #randomly move that observation
  d$prop1[index]<-d$x1[index]+rnorm(1, 0, 0.1)
  d$prop2[index]<-d$x2[index]+rnorm(1, 0, 0.1)
  #create new X matrix
  Xnew<-as.matrix(cbind(d$prop1, d$prop2))
  #fit new clustering model
  predproposal<-predict(m, Xnew)
  #accept if new model performs better
  proploss<-0
  proploss<-ifelse(d$category=='A', predproposal$z[,1], proploss)
  proploss<-ifelse(d$category=='B', predproposal$z[,2], proploss)
  proploss<-ifelse(d$category=='C', predproposal$z[,3], proploss)
  proploss<-sum(log(proploss))
  if ((proploss > priorloss) & (d$prop1[index]  > 0) & (d$prop2[index]  > 0) & (d$prop1[index]  <15) & (d$prop2[index]  < 15)){
    d$x1<-d$prop1
    d$x2<-d$prop2
    priorloss<-proploss
    acceptcount<-acceptcount+1
    cat('accepted!\n')
  }
}

par(mfrow=c(2, 1))
plot(d$d1i, d$d2i, col=d$category)
plot(d$x1, d$x2, col=d$category)
d$x1==d$d1i
d %>% 
  select(d1i, d2i, x1, x2, category) %>%
  pivot_longer(cols = c(d1i, x1)) %>%
  pivot_longer(cols = c(d2i, x2), names_to = "name2", values_to = "value2") %>%
  ggplot(aes(value, value2, group = category)) +
  geom_hex(aes(fill = category)) +
  facet_wrap(~ name) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", name = "Category") +
  labs(
    x = "X1",
    y = "X2"
  )


# dpmm
library(tidyverse)

n <- 100
alpha <- 10

z <- rbeta(n, 1, alpha)
mult_samples <- function(z, k) {
  z[k] * prod(1 - z[0:(k-1)])
}

w <- map_dbl(seq(1, length(z)), mult_samples, z = z)
phi <- rnorm(n, 0, 1)
sum(w * phi)

hist(w)
format(w, digits = 2, scientific = FALSE)
