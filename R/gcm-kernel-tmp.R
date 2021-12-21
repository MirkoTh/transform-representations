library(tidyverse)


x_1 <- seq(1, 12)
x_2 <- x_1

tbl_x <- crossing(
  x_1, x_2
)

N <- nrow(tbl_x)

distance_gcm <- function(i, df_x, N, r) {
  m <- matrix(unlist(rep(df_x[i, ], N)), N, 2, byrow = TRUE)
  colnames(m) <- c("x_1", "x_2")
  tbl_single <- as_tibble(m)
  tbl_single["sim"] <- (
    (tbl_single$x_1 - tbl_x$x_1)^r +
      (tbl_single$x_2 - tbl_x$x_2)^r
  )^(1/r)
}

similarity_gcm <- function(c, w, r, tbl_x, N){
  l <- map(1:N, distance_gcm, tbl_x, N, r)
  m <- matrix(unlist(l), N, N, byrow = TRUE)
  colnames(m) <- str_c("x_", 1:N)
  tbl <- as_tibble(exp(-c*m))
  return (tbl)
}


distance_rbf <- function(i, df_x, N, lambda, sigma){
  m <- matrix(unlist(rep(df_x[i, ], N)), N, 2, byrow = TRUE)
  colnames(m) <- c("x_1", "x_2")
  tbl_single <- as_tibble(m)
  tbl_single["sim"] <- - (sqrt(
    (tbl_single$x_1 - tbl_x$x_1)^2 +
      (tbl_single$x_2 - tbl_x$x_2)^2
  )^2) / (2*lambda^2)
return (tbl_single$sim)
}

similarity_rbf <- function(lambda, sigma, tbl_x, N){
  l <- map(1:N, distance_rbf, tbl_x, N, lambda, sigma)
  m <- matrix(unlist(l), N, N, byrow = TRUE)
  colnames(m) <- str_c("x_", 1:N)
  tbl <- as_tibble(sigma^2 * exp(m))
  return (tbl)
}


tbl_sim_gcm <- similarity_gcm(1, 1, 2, tbl_x, N)
tbl_sim_rbf <- similarity_rbf(.5, 1, tbl_x, N)


cols <- colnames(tbl_sim_gcm)
tbl_sim_gcm$var2 <- cols
tbl_sim_rbf$var2 <- cols

tbl_sim_gcm %>% pivot_longer(cols = all_of(cols)) %>%
  mutate(var2 = factor(var2, levels = cols),
         name = factor(name, levels = cols)) %>%
  ggplot() +
  geom_tile(aes(name, var2, color=value)) +
  scale_color_viridis_c()


tbl_sim_rbf %>% pivot_longer(cols = all_of(cols)) %>%
  mutate(var2 = factor(var2, levels = cols),
         name = factor(name, levels = cols)) %>%
  ggplot() +
  geom_tile(aes(name, var2, color=value)) +
  scale_color_viridis_c()




