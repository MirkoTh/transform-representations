rm(list=ls())
library(mclust)
library(naivebayes)
library(tidyverse)
library(docstring)


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
  if ((proploss > priorloss) & (d$prop1[index]  > 0) & (d$prop2[index]  > 0) & (d$prop1[index]  < 15) & (d$prop2[index]  < 15)){
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





# Implementation Naive Bayes By Mirko -------------------------------------


files <- c("R/utils.R")
walk(files, source)

## create categories
thx <- c(1, 8)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
tbl <- crossing(x1, x2)
tbl <- create_categories(tbl, 4) %>% select(-c(x1_cat, x2_cat))
tbl$timepoint <- "Before Training"

## 1. see all the stimuli and categorize them
features <- c("x1", "x2")
label <- "category"
tbl$category <- as.factor(tbl$category)
categories <- levels(tbl$category)
fml <- formula(str_c(label, " ~ ", str_c(features, collapse = " + ")))
m_nb <- naive_bayes(fml, data = tbl)


## 2. go over nruns noise-distorted samples and categorize them according to your model
nruns <- 10000
tbl_new <- tbl
tbl_new$prop1 <- tbl_new$x1
tbl_new$prop2 <- tbl_new$x2
posterior <- predict(m_nb, tbl[, c("x1", "x2")], "prob")
l_prior_prep <- extract_posterior(posterior, m_nb)
tbl_prior_long <- l_prior_prep[[1]]
l_prior <- l_prior_prep[[2]]


pb <- txtProgressBar(min = 1, max = nruns, initial = 1)
for (i in 1:nruns) {
  # sample random observation 
  index <- sample(nrow(tbl_new), 1)
  # randomly move that observation
  tbl_new$prop1[index] <- tbl_new$x1[index] + rnorm(1, 0, 0.1)
  tbl_new$prop2[index] <- tbl_new$x2[index] + rnorm(1, 0, 0.1)
  # create new X matrix
  X <- as.matrix(tbl_new[, c("prop1", "prop2")])
  colnames(X) <- features
  posterior <- predict(m_nb, X, "prob")
  l_post <- extract_posterior(posterior, m_nb)[[2]]
  if (
    (as_vector(l_post) > as_vector(l_prior)) & 
    between(tbl_new$prop1[index], thx[1], thx[2]) & 
    between(tbl_new$prop2[index], thx[1], thx[2])
  ) {
    tbl_new$x1 <- tbl_new$prop1
    tbl_new$x2 <- tbl_new$prop2
    l_prior <- l_post
    posterior_fin <- posterior
  }
  setTxtProgressBar(pb,i)
}
close(pb)

tbl_posterior_long <- extract_posterior(posterior_fin, m_nb)[[1]]
sum(log(tbl_posterior_long$value))
sum(log(tbl_prior_long$value))



tbl_new$timepoint <- "After Training"

tbl_results <- tbl[, c(features, label, "timepoint")] %>%
  rbind(tbl_new[, c(features, label, "timepoint")])

tbl_centers <- map(m_nb$tables, function(x) x[1, ]) %>% 
  as_tibble() %>%
  mutate(
    category = categories
  )

tbl_results <- left_join(
  tbl_results, 
  tbl_centers[, c("x1", "x2", "category")], 
  by = "category",
  suffix = c("_data", "_center")
)
ncols <- length(names(tbl_results))
tbl_results$timepoint <- fct_relevel(tbl_results$timepoint, "Before Training", "After Training")

pl_centers <- ggplot(tbl_results, aes(x1_data, x2_data, group = as.numeric(category))) +
  geom_point(aes(color = as.numeric(category))) +
  geom_point(aes(x1_center, x2_center, color = as.numeric(category)), size = 3) +
  geom_segment(
    aes(
      x = x1_data, y = x2_data, xend = x1_center, yend = x2_center, 
      color = as.numeric(category)
    ),
    arrow = arrow(length = unit(.1, "inches"))
  ) +
  facet_wrap(~ timepoint, scales = "free") +
  theme_bw() +
  coord_cartesian(xlim = c(thx[1] - 1, thx[2] + 1), ylim = c(thx[1] - 1, thx[2] + 1)) +
  scale_color_viridis_c(name = "Category") +
  labs(
    x = "X1",
    y = "X2"
  )

tbl_posteriors <- tbl_prior_long %>%
  mutate(timepoint = "Before Training") %>%
  rbind(
    tbl_posterior_long %>%
      mutate(timepoint = "After Training")
  ) %>%
  mutate(
    timepoint = fct_relevel(timepoint, "Before Training", "After Training")
  )
pl_post <- ggplot(tbl_posteriors, aes(value)) +
  geom_histogram() +
  geom_density() +
  facet_wrap(~ timepoint) +
  theme_bw() +
  labs(
    x = "Posterior Probability",
    y = "Probability Density"
  )

plot_arrangement(list(pl_centers, pl_post), n_cols = 1)

ggplot(tbl_posteriors, aes(value)) +
  geom_density() +
  facet_wrap(~ timepoint)

# logic
## 1. you see all the stimuli and categorize them.
## 2. you see a noise-distorted version of the stimuli again during the categorization task
## 3. you update the posterior based on all the accepted samples (acceptance due to some logic) once you have finished on the task
## 4. something else that could be done is, that the model is updated every time a noisy sample is accepted
## a potential problem is that this approach assumes that people have seen 
## the original stimuli in the beginning with the "true" values
## this is inconsistent with the second part, in which we assume that people 
## perceive a stimulus with added noise
## or is this thought of people seeing the grid of categorized stimuli in the
## beginning of the experiment? it could also be that when the second task is the "identity" task, representations do not change.
## we could also test a version, in which people only see noise-distorted stimuli
## from the very beginning





