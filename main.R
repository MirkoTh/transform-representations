# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(grid)
library(ggExtra)
library(furrr)
library(catlearn)
library(gridExtra)
library(assertthat)
library(mvtnorm)

files <- c(
  "R/utils.R", "R/plotting.R", 
  "R/analysis-utils.R",
  "R/analysis-plotting.R"
)
walk(files, source)

# Simulation Parameters ---------------------------------------------------

n_stimuli <- 100L
nruns <- 5000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  cat_type = c("prototype", "exemplar", "rule"), #, 
  sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(TRUE, FALSE), category_shape = c("square", "ellipses"),
  is_reward = FALSE,
  informed_by_data = c(TRUE),
  representation = c("psychological-representation") #, ,   # "object-properties"
)

tbl_vary$n_categories <- rep(c(2, 4), nrow(tbl_vary)/2) # 2, 4, 
tbl_vary <- tbl_vary %>% relocate(n_categories, .before = cat_type)

# rule-based strategy does not work well with ellipse category
filter_out <- tbl_vary$category_shape == "ellipses" & tbl_vary$cat_type == "rule"
tbl_vary <- tbl_vary[!filter_out, ]
tbl_vary$use_exptl_stimuli <- tbl_vary$informed_by_data

# these are the only available combinations
tbl_prior_lookup <- tibble(
  representation = c("object-properties", "psychological-representation", "object-properties"),
  use_exptl_stimuli = c(TRUE, TRUE, FALSE),
  prior_sd = c(7.5, .5, .75)
)
tbl_vary <- tbl_vary %>% left_join(
  tbl_prior_lookup, by = c("representation", "use_exptl_stimuli")
) %>% relocate(prior_sd, .after = cat_type)



l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6, is_reward = ..7,
      informed_by_data = ..8, representation = ..9, 
      use_exptl_stimuli = ..10
    )
  )
)

tbl_vary_seq <- tbl_vary %>% filter(cat_type == "exemplar") %>% mutate(cat_type = "no category")
l_info_seq <- pmap(
  tbl_vary_seq %>% unique(), ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6, is_reward = ..7,
      informed_by_data = ..8, representation = ..9,
      use_exptl_stimuli = ..10
    )
  )
)
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)

tbl_info_seq <- tibble(do.call(rbind.data.frame, l_info_seq)) %>%
  mutate(condition_id = seq(1:length(l_info_seq))) %>%
  relocate(condition_id, .before = n_stimuli)


# sanity checks of parameters
# makes only sense with square categories:
# walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))


# problem with rule-based model: idx 19
l_category_results <- future_map(
  l_info, categorize_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

l_seq_results <- future_map(
  l_info_seq, compare_subsequent_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

plan("sequential")

read_write <- "read"

td <- lubridate::today()

if (read_write == "write") {
  saveRDS(l_category_results, file = str_c("data/", td, "-grid-search-vary-constrain-space.rds"))
  saveRDS(l_seq_results, file = str_c("data/", td, "-grid-search-sequential-comparison.rds"))
} else if (read_write == "read") {
  l_category_results <- readRDS(file = "data/2024-02-18-grid-search-vary-constrain-space.rds")
  l_seq_results <- readRDS(file = "data/2024-02-18-grid-search-sequential-comparison.rds")
}


# Post Processing & Plotting ----------------------------------------------
if (read_write == "write") {
  l_results_plots <- map2(l_category_results, tbl_vary$category_shape, diagnostic_plots, is_simulation = TRUE, l_info)
  l_results_plots_seq <- map2(l_seq_results, tbl_info_seq$category_shape, diagnostics_seq, is_simulation = TRUE)
  saveRDS(l_results_plots, str_c("data/", td, "-category-learning-result-plots.RDS"))
  saveRDS(l_results_plots_seq, str_c("data/", td, "-sequential-comparison-result-plots.RDS"))
} else if (read_write == "read") {
  l_results_plots <- readRDS(str_c("data/2024-02-18-category-learning-result-plots.RDS"))
  l_results_plots_seq <- readRDS(str_c("data/2024-02-18-sequential-comparison-result-plots.RDS"))
}

dg <- position_dodge(width = .9)

# ellipse category structure example prediction
pl_pred_delta_ellipse_center <- l_results_plots[[1]][[2]][[4]]$tbl_cr_agg %>% mutate(condition = "Category Learning") %>% rbind(
  l_results_plots_seq[[1]]$tbl_avg_move %>% mutate(condition = "Sequential Comparison")
) %>% mutate(category = c("Bukil", "Venak")[category]) %>%
  ggplot(aes(session, d_closest_sqrt, group = condition)) +
  geom_col(aes(fill = condition), position = dg) +
  scale_fill_viridis_d(name = "Condition") +
  theme_bw()  +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.position = c(.75, .84)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time Point", y = "Distance To Closest Center") + facet_wrap(~ category)

pl_pred_ellipse <- l_results_plots[[17]][[2]][[1]] +
  scale_color_gradient(guide = "none", low = "lightskyblue2", high = "tomato3")

save_my_pdf_and_tiff(arrangeGrob(pl_pred_ellipse + theme(plot.title = element_blank()), pl_pred_delta_ellipse, nrow = 1), "figures/model-predictions-ellipses", 12, 3.5)


# square category example prediction
pl_pred_delta_square <- l_results_plots[[2]][[2]][[4]]$tbl_cr_agg %>% mutate(condition = "Category Learning") %>% rbind(
  l_results_plots_seq[[2]]$tbl_avg_move %>% mutate(condition = "Sequential Comparison")
) %>% mutate(category = c("Any Category")[category]) %>%
  ggplot(aes(session, d_closest_sqrt, group = condition)) +
  geom_col(aes(fill = condition), position = dg) +
  scale_fill_viridis_d(name = "Condition")  +
  theme_bw() + 
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time Point", y = "Distance To Closest Center") + facet_wrap(~ category)

pl_pred_square <- l_results_plots[[6]][[2]][[1]]  +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16)
  ) +
  scale_color_gradient(guide = "none", low = "lightskyblue2", high = "tomato3")


# get plot build
build <- ggplot_build(pl_pred_square)

# select hex value and x-value data
hex_df <- build$data[[1]][, c("colour", "x")]
unique(hex_df$colour)

save_my_pdf_and_tiff(arrangeGrob(
  pl_pred_square + theme(plot.title = element_blank()),
  pl_pred_delta_square, nrow = 1
), "figures/model-predictions-squares", 12, 3.5)

save_my_pdf_and_tiff(arrangeGrob(
  pl_pred_ellipse + theme(plot.title = element_blank()), pl_pred_delta_ellipse,
  pl_pred_square + theme(plot.title = element_blank()), pl_pred_delta_square,
  nrow = 2
), "figures/model-predictions-both-designs", 11.25, 7.5)
# extract sum of distances between prior and posterior
# in category learning
l_posteriors_cat <- map(map(l_results_plots, 1), "tbl_posterior")
l_sum_of_distances_cat <- map2(l_posteriors_cat, tbl_vary$category_shape, sum_of_pairwise_distances)

# in sequential comparison
l_posteriors_seq <- map(
  l_seq_results, ~ .x$tbl_new %>%
    group_by(stim_id, cat_type, category, timepoint) %>%
    summarize(x1_true = mean(x1), x2_true = mean(x2)) %>% ungroup()
)
l_sum_of_distances_seq <- map2(l_posteriors_seq, tbl_vary_seq$category_shape, sum_of_pairwise_distances)


l_conds <- split(tbl_info[, c("cat_type", "prior_sd", "sampling", "constrain_space", "category_shape")], tbl_info$condition_id)
l_conds_rep <- map2(l_conds, l_sum_of_distances_cat, ~ slice(.x, rep(1:n(), each = nrow(.y))))

l_sum_of_distances_cat <- map2(
  l_sum_of_distances_cat, 
  l_conds_rep,
  ~ cbind(tibble(.x), tibble(.y))
)


l_conds <- split(tbl_info_seq[, c("cat_type", "prior_sd", "sampling", "constrain_space", "category_shape")], tbl_info_seq$condition_id)
l_conds_rep <- map2(l_conds, l_sum_of_distances_seq, ~ slice(.x, rep(1:n(), each = nrow(.y))))

l_sum_of_distances_seq <- map2(
  l_sum_of_distances_seq,
  l_conds_rep,
  ~ cbind(tibble(.x), tibble(.y))
)

tbl_sum_of_distances <- reduce(l_sum_of_distances_cat, rbind) %>% as_tibble()
tbl_sum_of_distances$task <- "Category Learning"

tmp <- reduce(l_sum_of_distances_seq, rbind) %>% as_tibble()
tmp$task <- "Sequential Comparison"
tbl_sum_of_distances <- rbind(tbl_sum_of_distances, tmp)

tbl_ds_agg <- tbl_sum_of_distances %>%
  filter(tp == "After Training") %>%
  group_by(task, cat_type, comparison, sampling, constrain_space, category_shape, n_comparisons) %>%
  summarize(avg_ds_abs = mean(ds_abs), avg_ds_prop = mean(ds_prop)) %>%
  ungroup()
tbl_ds_agg$cat_type <- factor(tbl_ds_agg$cat_type, labels = c("Exemplar", "None", "Prototype", "Rule"))
tbl_ds_agg$cat_type <- fct_inorder(tbl_ds_agg$cat_type)
tbl_ds_agg$sampling <- factor(tbl_ds_agg$sampling, labels = c("Improvement", "Metropolis-Hastings"))
tbl_ds_agg$constrain_space <- factor(tbl_ds_agg$constrain_space, labels = c("No Space Constraints", "Space Constrained"))


# predictions for pairwise comparisons only have to be made for the squared category structure

dg <- position_dodge(width = .9)
pl_preds_ds <- ggplot(
  tbl_ds_agg %>% 
    filter(category_shape == "square" & constrain_space == "No Space Constraints" & sampling == "Improvement")
  , aes(comparison, avg_ds_prop, group = cat_type)) +
  geom_col(aes(fill = cat_type), position = dg, color = "black") +
  geom_hline(yintercept = 1, size = 1, color = "grey", linetype = "dotdash") +
  facet_grid(task ~ interaction(constrain_space, sampling, sep = " & \n")) +
  #facet_wrap(~ task) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom",
    text = element_text(size = 16)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "Category Learning Model") +
  labs(x = "Comparison", y = "Prop. Change of Pairwise Distances") +
  guides(fill = guide_legend(nrow=2,byrow=TRUE))

save_my_tiff(pl_preds_ds, "figures/model-predictions-distances.tiff", 5.5, 6)
save_my_pdf(pl_preds_ds, "figures/model-predictions-distances.pdf", 5.5, 6)


# Plot Prior Means & Posterior Means --------------------------------------

l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 2)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")
l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 4)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")


# Plot Movements of Individual Points -------------------------------------

stim_ids <- c(72, 1)
l_tbl_stimuli <- map(stim_ids, stimulus_before_after, l_results = l_category_results)
split_my_vars <- function(tbl){
  tbl %>% split(list(.$sampling, .$space))
}
l_tbl_stimuli_split <- map(l_tbl_stimuli, split_my_vars)
l_plt_stimulus_movements <- map(l_tbl_stimuli_split, plot_stimulus_movements)

l_plt_stimulus_movements[[1]][[1]]



l_tmp <- make_stimuli(l_info[[1]])
tbl_all_cats <- l_tmp[[1]]
tbl_ellipses <- l_tmp[[2]]$tbl_ellipses
ggplot() + geom_point(data = tbl_all_cats, aes(x1, x2, color = category), size = 2) + 
  geom_point(data = tbl_ellipses, aes(x_rotated, y_rotated, group = category), color = "grey", size = .5) +
  scale_color_brewer(palette = "Set1", name = "Category") +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12)) +
  theme_bw() +
  # theme(plot.background = element_rect(fill = "black"),
  #       panel.background = element_rect(fill = "black")) +
  labs(
    x = expression(X["1"]),
    y = expression(X["2"])
  )


## evaluate results from one simulation condition
l_category_results <- readRDS(file = "data/2021-11-05-grid-search.rds")
add_shape <- function(x) {x$l_info[["category_shape"]] <- "squares"; return(x)}
l_category_results <- map(l_category_results, add_shape)

# look at simulation conditions
map(l_category_results, function(x) c(x$l_info$sampling, x$l_info$cat_type, x$l_info$prior_sd))

tbl_test <- l_category_results[[35]]$tbl_new %>% arrange(stim_id)

ggplot(tbl_test, aes(x1, x2, group = stim_id)) +
  geom_point(aes(color = stim_id)) +
  scale_color_continuous()





# Simulations for Strategy Mix --------------------------------------------

ids_ellipses_cat <- tbl_info %>% filter(
  category_shape == "ellipses" & constrain_space &
    representation == "psychological-representation" &
    sampling == "improvement"
) %>% select(
  condition_id
) %>% as_vector()
ids_ellipses_seq <- tbl_info_seq %>% filter(
  category_shape == "ellipses" & constrain_space &
    representation == "psychological-representation" &
    sampling == "improvement"
) %>% select(
  condition_id
) %>% as_vector() %>% rep(length(ids_ellipses_cat))

ids_square_cat <-  tbl_info %>% filter(
  category_shape == "square" & constrain_space &
    representation == "psychological-representation" &
    sampling == "improvement"
) %>% select(
  condition_id
) %>% as_vector()
ids_square_seq <- tbl_info %>% filter(
  category_shape == "square" & constrain_space &
    representation == "psychological-representation" &
    sampling == "improvement" &
    cat_type == "exemplar"
) %>% select(
  condition_id
) %>% as_vector() %>% rep(length(ids_square_cat))


tbl_model_weights_ellipses <- tibble(
  cat_type = c("exemplar", "prototype"),
  prop = c(.613, .387)
)

tbl_model_weights_square <- tibble(
  cat_type = c("exemplar", "prototype", "rule"),
  prop = c(0.496, .479, .025)
)

averaged_movements_stimuli <- function(tbl_model_weights, ids) {
  
  tbl_move <- reduce(map(
    ids, ~ 
      l_results_plots[[.x]][[1]]$tbl_posterior %>% mutate(cat_type = tbl_info[.x, ]$cat_type)
  ), rbind) %>% arrange(
    stim_id
  ) %>% left_join(
    tbl_model_weights, by = c("cat_type")
  ) %>% mutate(
    x1_prop = x1_true * prop,
    x2_prop = x2_true * prop
  ) %>% group_by(
    stim_id, category, timepoint
  ) %>% summarize(
    x1 = sum(x1_prop), x2 = sum(x2_prop)
  ) %>% ungroup()
  
  if (l_info[[ids[1]]]$informed_by_data) {
    space_edges <- list(x1 = c(-0.06579467, 6.75626343), x2 = c(-0.151021, 7.410835))
  } else {
    space_edges <- list(x1 = c(0, 100), x2 = c(0, 100))
  }
  
  x_breaks <- round(seq(space_edges$x1[1], space_edges$x1[2], length.out = 5), 1)
  y_breaks <- round(seq(space_edges$x2[1], space_edges$x2[2], length.out = 5), 1)
  
  ggplot(tbl_move, aes(x1, x2, group = as.numeric(category))) +
    geom_point(aes(color = as.numeric(category)), shape = 8, size = 2) +
    geom_segment(
      data = tbl_move %>% 
        filter(timepoint == "After Training") %>% 
        select(stim_id, x1, x2) %>%
        left_join(
          tbl_move %>% filter(timepoint == "Before Training"), 
          by = "stim_id", suffix = c("_aft", "_bef")
        ) %>%
        mutate(
          timepoint = "After Training", 
          timepoint = factor(timepoint, levels = c("Before Training", "After Training"))
        ),
      aes(
        x = x1_bef, xend = x1_aft,
        y = x2_bef, yend = x2_aft,
        color = as.numeric(category)
      ), 
      arrow = grid::arrow(angle = 50, length = unit(.075, "in"), type = "closed")
    ) +
    facet_wrap(~ timepoint) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22),
      #axis.text.x = element_text(angle = 90, vjust = .5)
    ) +
    coord_cartesian(
      xlim = c(space_edges$x1[1] - 1, space_edges$x1[2] + 1), 
      ylim = c(space_edges$x2[1] - 1, space_edges$x2[2] + 1)
    ) +
    scale_color_gradient(guide = "none", low = "lightskyblue2", high = "tomato3") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(
      x = "Spikiness of Head",
      y = "Fill of Belly"
    )
  
}


averaged_movements_center <- function(
    tbl_model_weights, ids_cat, ids_seq, center_or_boundary = "center"
) {
  if(center_or_boundary == "center") {
    idx_center_or_boundary <- 4
    yttl <- "Distance (Cl. Center)"
  } else {
    idx_center_or_boundary <- 5
    yttl <- "Distance (Cl. Boundary)"
  }
  tbl_move_avg <- reduce(map(
    ids_cat,
    ~ l_results_plots[[.x]][[2]][[idx_center_or_boundary]]$tbl_cr_agg %>% 
      mutate(cat_type = tbl_info[.x, ]$cat_type)), rbind
  ) %>%
    left_join(
      tbl_model_weights, by = "cat_type"
    ) %>% mutate(
      d_closest_sqrt = d_closest_sqrt * prop,
      ci = ci * prop
    ) %>% group_by(
      session, category
    ) %>% summarize(
      d_closest_sqrt = sum(d_closest_sqrt),
      ci = sum(ci)
    ) %>% ungroup() %>% mutate(condition = "Category Learning") %>% rbind(
      reduce(map(
        ids_seq,
        ~ l_results_plots_seq[[.x]]$tbl_avg_move), rbind
      ) %>% group_by(
        session, category
      ) %>% summarize(
        d_closest_sqrt = mean(d_closest_sqrt),
        ci = mean(ci)
      ) %>% ungroup() %>% mutate(condition = "Sequential Comparison")
    )
  if (length(unique(tbl_move_avg$category)) == 1) {
    tbl_move_avg$category <- factor(tbl_move_avg$category, labels = "Any Category")
  } else {
    tbl_move_avg$category <- factor(tbl_move_avg$category, labels = c("Bukil", "Venak"))
  }
  dg <- position_dodge(width = .9)
  ggplot() +
    geom_col(
      data = tbl_move_avg,
      aes(session, d_closest_sqrt, group = condition, fill = condition),
      position = dg,
      alpha = .5
    ) +
    geom_point(
      data = tbl_move_avg,
      aes(session, d_closest_sqrt, color = condition),
      position = dg,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data = tbl_move_avg,
      aes(
        session,
        ymin = d_closest_sqrt - ci,
        ymax = d_closest_sqrt + ci,
        color = condition
      ),
      position = dg,
      width = .25,
      show.legend = FALSE
    )  + facet_wrap(~ category) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22),
      #axis.text.x = element_text(angle = 90, vjust = .5),
      legend.position = "none"
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    labs(x = "Time Point",
         y = yttl) +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
}




pl_stimuli_ell <- averaged_movements_stimuli(tbl_model_weights_ellipses, ids_ellipses_cat)
pl_stimuli_sq <- averaged_movements_stimuli(tbl_model_weights_square, ids_square_cat)


ids_cat <- ids_square_cat
ids_seq <- ids_square_seq
tbl_model_weights <- tbl_model_weights_square
pl_center_ell <- averaged_movements_center(tbl_model_weights_ellipses, ids_ellipses_cat, ids_ellipses_seq)
pl_center_sq <- averaged_movements_center(tbl_model_weights_square, ids_square_cat, ids_square_seq)

pl_boundary_ell <- averaged_movements_center(tbl_model_weights_ellipses, ids_ellipses_cat, ids_ellipses_seq, "boundary")
pl_boundary_sq <- averaged_movements_center(tbl_model_weights_square, ids_square_cat, ids_square_seq, "boundary")

pl_task_imprinting <- arrangeGrob(
  pl_stimuli_ell, pl_stimuli_sq, 
  pl_center_ell, pl_center_sq,
  pl_boundary_ell, pl_boundary_sq,
  nrow = 3, ncol = 2
)

save_my_pdf_and_tiff(pl_task_imprinting, "figures/figures-ms/model-predictions-both-designs", 11.25, 11.25)



tbl_boundary <- reduce(map(
  ids, ~ 
    l_results_plots[[.x]][[1]]$tbl_posterior %>% mutate(cat_type = tbl_info[.x, ]$cat_type)
), rbind) %>% left_join(
  tbl_model_weights, by = c("cat_type")
) %>% mutate(
  d_boundary = d_boundary * prop
) %>% group_by(stim_id, category, timepoint) %>% summarize(
  d_boundary = sum(d_boundary)
) %>% grouped_agg(c(category, timepoint), d_boundary)

ggplot(tbl_boundary, aes(timepoint, mean_d_boundary)) +
  geom_point(aes(group = category))


