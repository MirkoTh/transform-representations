# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(mclust)
library(naivebayes)
library(tidyverse)
library(docstring)

#rm(list = ls())
files <- c("R/utils.R")
walk(files, source)


# Create Categories -------------------------------------------------------

thx <- c(0, 15)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
tbl <- crossing(x1, x2)
tbl <- create_categories(tbl, 4) %>% select(-c(x1_cat, x2_cat))
tbl$timepoint <- "Before Training"

# this has to be changed. the initial model is not trained on the prior means
# but on random samples from the stimuli priors, reflecting some initial contact
# with the stimuli by participants

features <- c("x1", "x2")
label <- "category"
tbl$category <- as.factor(tbl$category)
categories <- levels(tbl$category)
fml <- formula(str_c(label, " ~ ", str_c(features, collapse = " + ")))
m_nb <- naive_bayes(fml, data = tbl)

nruns <- 1000
tbl_new <- tbl
tbl_new$prop1 <- tbl_new$x1
tbl_new$prop2 <- tbl_new$x2
posterior_prior <- predict(m_nb, tbl[, c("x1", "x2")], "prob")
l_prior_prep <- extract_posterior(posterior_prior, m_nb)
tbl_prior_long <- l_prior_prep[[1]]
l_prior <- l_prior_prep[[2]]
posterior <- posterior_prior


# Categorization Simulation -----------------------------------------------

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
  posterior_new <- predict(m_nb, X, "prob")
  # l_post <- extract_posterior(posterior, m_nb)[[2]]
  if (
    (posterior_new[index, tbl_new$category[index]] > posterior[index, tbl_new$category[index]]) & 
    # could be likelihood ratio as well:
    # posterior_new[index, tbl_new$category[index]]/m_nb$prior[tbl_new$category[index]] > posterior[index, tbl_new$category[index]/m_nb$prior[tbl_new$category[index]]]
    between(tbl_new$prop1[index], thx[1], thx[2]) & 
    between(tbl_new$prop2[index], thx[1], thx[2])
  ) {
    tbl_new$x1[index] <- tbl_new$prop1[index]
    tbl_new$x2[index] <- tbl_new$prop2[index]
    # tbl_new$x1 <- tbl_new$prop1
    # tbl_new$x2 <- tbl_new$prop2
    # l_prior <- l_post
    posterior <- posterior_new
    # cat("accepted\n")
  }
  
  setTxtProgressBar(pb,i)
}
rclose(pb)


# Post Processing ---------------------------------------------------------

tbl_results <- c_before_after(tbl_new, tbl)
tbl_posterior_long <- extract_posterior(posterior, m_nb)[[1]]
tbl_posteriors <- tbl_prior_long %>%
  mutate(timepoint = "Before Training") %>%
  rbind(
    tbl_posterior_long %>%
      mutate(timepoint = "After Training")
  ) %>%
  mutate(
    timepoint = fct_relevel(timepoint, "Before Training", "After Training")
  )

pl_centers <- plot_moves(tbl_results)
pl_post <- plot_cat_probs(tbl_posteriors)
plot_arrangement(list(pl_centers, pl_post), n_cols = 1)





