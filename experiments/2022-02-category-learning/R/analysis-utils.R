fix_data_types <- function(tbl, fs, ns) {
  #' fix data types of columns of tbl
  #' 
  #' @description factors as factors and numerics as numerics, leave characters
  #' 
  #' @param tbl the columns of this tibble are changed
  #' @param fs vector of factors
  #' @param ns vector of numerics
  #' @return the tibble with the changed data types
  #' 
  cols <- colnames(tbl)
  fs_available <- intersect(fs, cols)
  ns_available <- intersect(ns, cols)
  tbl[, fs_available] <- map(tbl[, fs_available], as.factor)
  tbl[, ns_available] <- map(tbl[, ns_available], as.numeric)
  return(tbl)
}


load_data <- function(path_data) {
  #' load continuous reproduction ("cr") and category learning ("cat") data
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles
  #' 
  #' @return a list with the two tibbles
  #' 
  # read individual performance
  files_dir <- dir(path_data)
  paths_cat <- str_c(path_data, files_dir[startsWith(files_dir, "cat")])
  paths_cr <- str_c(path_data, files_dir[startsWith(files_dir, "cr")])
  
  json_to_tibble <- function(path_file) {
    js_txt <- read_file(path_file)
    js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
    js_txt <- str_replace(js_txt, ",\n]", "]")
    tbl_cr <- jsonlite::fromJSON(js_txt) %>% as_tibble()
    return(tbl_cr)
  }
  tbl_cr <- reduce(map(paths_cr, json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_cat <- reduce(map(paths_cat, json_to_tibble), rbind)
  # add stim_id
  tbl_cr$stim_id <- (floor(tbl_cr$x1_true/9) - 1) * 10 + (floor(tbl_cr$x2_true/9) - 1) + 1
  
    # only pilot data have to be corrected currently...
  tbl_cr$session <- as.numeric(tbl_cr$session)
  #tbl_cr[148:nrow(tbl_cr), "session"] <- 2 + tbl_cr[148:nrow(tbl_cr), "session"]
  
  factors <- c("participant_id", "session", "cat_true")
  numerics <- c("trial_id", "x1_true", "x2_true", "x1_response", "x2_response", "rt")
  tbl_cr <- fix_data_types(tbl_cr, factors, numerics)
  tbl_cat <- fix_data_types(tbl_cat, factors, numerics)
  
  l_data <- list(tbl_cr, tbl_cat)
  return(l_data)
}


exclude_incomplete_datasets <- function(l_tbl) {
  #' exclude incomplete data
  #' 
  #' @description exclude incomplete data from continuous reproduction ("cr")
  #' and category learning ("cat") data
  #' 
  #' @return a list with the two tibbles
  #' 
  tbl_cr <- l_tbl[[1]]
  tbl_cat <- l_tbl[[2]]
  tbl_cr_n <- tbl_cr %>% 
    group_by(participant_id) %>% summarize(n_resp = n()) %>%
    ungroup() %>% arrange(n_resp) %>% filter(n_resp > 194)
  
  tbl_cat_n <- tbl_cat %>% 
    group_by(participant_id) %>% summarise(n_resp = n()) %>% 
    ungroup() %>% arrange(n_resp) %>% filter(n_resp > 435)
  
  tbl_cr <- tbl_cr %>% inner_join(tbl_cr_n, by = "participant_id")
  tbl_cat <- tbl_cat %>% inner_join(tbl_cat_n, by = "participant_id")
  
  return(list(tbl_cr, tbl_cat))
}


exclude_reproduction_outliers <- function(tbl_cr, n_sds) {
  #' exclude outliers in reproduction task
  #' 
  #' @description average deviations > n_sds above mean deviation are excluded
  #' @param tbl_cr the tibble with the by-trial responses
  #' @param n_sds how many sds above the mean is the cutoff?
  #' 
  #' @return a tbl with the included participants
  #'
  tbl_cr_participant <- tbl_cr %>% group_by(participant_id) %>% 
    summarize(avg_deviation = mean(eucl_deviation)) %>% ungroup() 
  tbl_excl <- tbl_cr_participant %>%
    summarize(
      deviation_mean = mean(avg_deviation),
      deviation_sd = sd(avg_deviation)
    ) %>% ungroup() %>% 
    mutate(thx_hi = deviation_mean + n_sds * deviation_sd)
  tbl_cr_participant$thx_hi <- tbl_excl$thx_hi
  tbl_cr_participant %>% arrange(avg_deviation)
  tbl_cr_participant$exclude <- FALSE
  tbl_cr_participant$exclude[tbl_cr_participant$avg_deviation > tbl_cr_participant$thx_hi] <- TRUE
  return(tbl_cr_participant %>% filter(exclude == FALSE))
}


checkerboard_deviation <- function(tbl, n_agg_x) {
  #' deviation from true cr values in several bins of x1 and x2
  #' 
  #' @description average deviation from true value in reproduction task
  #' categorizing head spikiness and fill of belly into n_agg_x bins
  #' @param tbl the tibble with the by-trial responses
  #' @param n_agg_x into how many bins should x1 and x2 be cut?
  #' 
  #' @return the aggregated tbl
  #' 
  lims <- tbl %>% 
    summarise(min_x = min(x1_true), max_x = max(x2_true)) %>%
    mutate(min_x = min_x - 1, max_x = max_x + 1) %>%
    as_vector()
  cutpoints <- seq(lims[1], lims[2], length.out = n_agg_x + 1)
  tbl_cr_agg <- tbl %>%
    filter(session %in% c(1, 2)) %>%
    mutate(
      x1_true_binned = cut(x1_true, cutpoints, labels = FALSE),
      x2_true_binned = cut(x2_true, cutpoints, labels = FALSE),
      participant_id = substr(participant_id, 1, 6)
    ) %>% group_by(participant_id, x1_true_binned, x2_true_binned) %>%
    summarise(avg_deviation_x1x2 = mean(sqrt(x1_deviation^2 + x2_deviation^2))) %>%
    group_by(participant_id) %>%
    mutate(avg_deviation = mean(avg_deviation_x1x2)) %>%
    ungroup() %>% 
    arrange(avg_deviation) %>%
    mutate(participant_id = fct_inorder(participant_id, ordered = TRUE))
  tbl_cr_agg_2 <- tbl_cr_agg %>%
    group_by(participant_id) %>%
    mutate(rwn = row_number(x1_true_binned)) %>% filter(rwn == 1) %>%
    ungroup()
  return(list(tbl_cr_agg, tbl_cr_agg_2))
}


category_centers <- function() {
  #' helper function to define category centers
  #' 
  #' @description returns x1 and x2 means of the ellipse categories (n_categories - 1)
  #' 
  #' @return the list with the centers of the 2 and 4 category conditions
  #' 
  # read individual performance
  x1 <- seq(0, 9, by = 1)
  x2 <- seq(0, 9, by = 1)
  tbl_tmp <- crossing(x1, x2)
  tbl_tmp <- tbl_tmp %>% mutate(stim_id = seq(1, 100, by = 1))
  l_ellipses <- map(c(2, 3), create_ellipse_categories, tbl = tbl_tmp)
  cat_boundaries_2 <- l_ellipses[[1]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 9 + 1, y_rotated = (y_rotated + 1) * 9 + 1)
  cat_boundaries_3 <- l_ellipses[[2]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 9 + 1, y_rotated = (y_rotated + 1) * 9 + 1)
  
  category_means <- function(tbl) {
    tbl %>% group_by(category) %>%
      summarize(x_mn = mean(x_rotated), y_mn = mean(y_rotated)) %>% ungroup()
  }
  cat_2_mns <- category_means(cat_boundaries_2)
  cat_3_mns <- category_means(cat_boundaries_3)
  l_cat_mns <- list(cat_2_mns, cat_3_mns)
  return(list(l_cat_mns, l_ellipses))
}


add_distance_to_nearest_center <- function(tbl_cr) {
  #' add distance to closest category centroid
  #' 
  #' @description calculates distances to all possible category centroids and returns min of those
  #' @param tbl_cr the tibble with the by-trial responses
  #' 
  #' @return the tibble with the min distance as added column
  #' 
  # read individual performance
  l_tmp <- category_centers()
  l_cat_mns <- l_tmp[[1]]
  l_ellipses <- l_tmp[[2]]
  euclidian_distance_to_center <- function(x_mn, y_mn, tbl, is_response) {
    if(is_response) {
      sqrt((tbl$x1_response - x_mn)^2 + (tbl$x2_response - y_mn)^2)
    } else {
      sqrt((tbl$x1_true - x_mn)^2 + (tbl$x2_true - y_mn)^2)
    }
    
  }
  # split by nr of categories
  l_tbl_cr <- split(tbl_cr, tbl_cr$n_categories)
  # as only one category center, can directly compute distance from response to that center
  # for the baseline condition the midpoint of the grid is used as the "category center"
  tbl_d1 <- pmap(l_cat_mns[[1]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["1"]], is_response = TRUE) %>%
    unlist() %>% matrix(ncol = 1) %>% as.data.frame() %>% tibble()
  colnames(tbl_d1) <- c("d_closest")
  l_tbl_cr[["1"]] <- l_tbl_cr[["1"]] %>% cbind(tbl_d1) %>% mutate(category = 1)
  tbl_d2 <- pmap(l_cat_mns[[1]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["2"]], is_response = TRUE) %>%
    unlist() %>% matrix(ncol = 1) %>% as.data.frame() %>% tibble()
  colnames(tbl_d2) <- c("d_closest")
  l_tbl_cr[["2"]] <- l_tbl_cr[["2"]] %>% cbind(tbl_d2) %>% left_join(l_ellipses[[1]][[1]] %>% select(stim_id, category), by = c("stim_id"))
  
  # here, we first have to compute what the closest center of a given stimulus is and then index using that id
  tbl_d3_true <- pmap(l_cat_mns[[2]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["3"]], is_response = FALSE) %>% 
    unlist() %>% matrix(ncol = 2) %>% as.data.frame() %>% tibble()
  colnames(tbl_d3_true) <- c("d1", "d2")
  tbl_d3_response <- pmap(l_cat_mns[[2]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["3"]], is_response = TRUE) %>% 
    unlist() %>% matrix(ncol = 2) %>% as.data.frame() %>% tibble()
  colnames(tbl_d3_response) <- seq(1:2)
  col_idx_closest <- apply(tbl_d3_true, 1, function(x) which(x == min(x)))
  tbl_d3_response$col_idx_closest <- col_idx_closest
  tbl_d3_response$d_closest <- apply(tbl_d3_response, 1, function(x) x[1:2][x[3]])
  d_closest <- as_vector(tbl_d3_response$d_closest) %>% unname()
  
  l_tbl_cr[["3"]] <- l_tbl_cr[["3"]] %>% cbind(d_closest) %>%
    left_join(l_ellipses[[2]][[1]] %>% select(stim_id, category), by = c("stim_id"))
  
  tbl_cr <- rbind(l_tbl_cr[["3"]], l_tbl_cr[["2"]], l_tbl_cr[["1"]]) %>% as_tibble()
  return(tbl_cr)
}


chance_performance_cat <- function(tbl_cat) {
  #' chance performance with 2 and 3 categories
  #' 
  #' @description make a tbl with chance performance over aggregated blocks
  #' for 2 and 3 categories
  #' @param tbl_cat the tibble with the by-trial responses
  #' 
  #' @return the chance peformance tibble
  #' 
  n_categories <- unique(tbl_cat$n_categories)
  block <- factor(sort(unique(tbl_cat$trial_id_binned)))
  
  tbl_chance <- crossing(
    n_categories, block
  )
  tbl_chance$prop_chance <- 0
  for (nc in n_categories) {
    val <- as.numeric(as.character(nc))
    tbl_chance$prop_chance[tbl_chance$n_categories == nc] <- 1/val

  }
   return(tbl_chance)
}


add_deviations <- function(tbl_cr) {
  #' by-trial, binned, and average deviations of reproduction responses
  #' 
  #' @description calculate deviations from true coordinates to 
  #' response coordinates by-trial, averaged into bins, and
  #' further averaged over bins
  #' @param tbl_cr the tibble with the by-trial responses
  #' 
  #' @return a list with three tbls
  #' 
  # add deviation variables
  tbl_cr$x1_deviation <- tbl_cr$x1_true - tbl_cr$x1_response
  tbl_cr$x2_deviation <- tbl_cr$x2_true - tbl_cr$x2_response
  tbl_cr$eucl_deviation <- sqrt(tbl_cr$x1_deviation^2 + tbl_cr$x2_deviation^2)
  tbl_cr <- add_distance_to_nearest_center(tbl_cr)
  
  # average deviation in binned x1-x2 grid
  l_checkerboard <- checkerboard_deviation(tbl_cr, 4)
  tbl_checker <- l_checkerboard[[1]]
  # and average over bins
  tbl_checker_avg <- l_checkerboard[[2]]
  
  l_deviations <- list(
    tbl_cr = tbl_cr,
    tbl_checker = tbl_checker,
    tbl_checker_avg = tbl_checker_avg
  )
  
  return(l_deviations)
}


exclude_outliers <- function(tbl_cr, tbl_cat_sim, n_sds) {
  #' exclude outliers from reproduction and categorization tbls
  #' 
  #' @description participant is excluded if average deviation
  #' exceeds mean + n_sds standard deviations
  #' @param tbl_cr the tibble with the by-trial reproduction responses
  #' @param tbl_cat_sim the tibble with the by-trial categorization responses
  #' @param n_sds nr of standard deviations to calculate thx
  #' 
  #' @return a list with two tbls
  #' 
  # add deviation variables
  participants_included <- exclude_reproduction_outliers(tbl_cr, n_sds)
  tbl_cr <- inner_join(
    participants_included[, "participant_id"], tbl_cr, by = "participant_id"
  )
  tbl_cat_sim <- inner_join(
    participants_included[, "participant_id"], tbl_cat_sim, by = "participant_id"
  )
  l_outliers_excluded <- list(
    tbl_cr = tbl_cr,
    tbl_cat_sim = tbl_cat_sim
  )
  return(l_outliers_excluded)
}


add_binned_trial_id <- function(tbl_cat_sim, binsize, trial_start_incl) {
  #' add block variable
  #' 
  #' @description add blocks of binsize = n trials including trial_id >= trial_start_incl
  #' @param tbl_cat_sim the tibble with the by-trial categorization responses
  #' @param binsize nr of trials that go into one bin
  #' @param trial_start_incl first trial to be considered
  #' 
  #' @return the same tbl with the binned trial ids
  #' 

  tbl_cat_sim <- tbl_cat_sim %>%
    filter(trial_id >= trial_start_incl) %>%
    group_by(participant_id, cat_true) %>%
    arrange(n_categories, participant_id, trial_id) %>%
    mutate(
      trial_id_by_condition = row_number(trial_id)
    ) %>% ungroup() %>% mutate(
      trial_id_binned = as.factor(ceiling((trial_id_by_condition) / binsize))
    )
  return(tbl_cat_sim)
}
