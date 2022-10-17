timeout_and_returns_e2 <- function() {
  #' manually assign prolific returns and submissions
  #' 
  pilot_I <- c(
    '62d8108a083717cafb747770',
    '62d810977a3c6d676074778d',
    '62d8109a7a9e4810935be338',
    '62d810a165900cc3096001bd',
    '62d810a31af42c699187f4dc',
    '62d810b1167959341fa067bb',
    '62d810b6a53df36f6c1ddd3c',
    '62d810bab18f2bfe81c49f1d',
    '62d810bae2e7a65aad2abcb0',
    '62d810bc7031b229278b2d57',
    '62d810ce2827840a9710adc7',
    '62d810fbc451b68c780d36b6',
    '62d811eeb4b6869415c51742',
    '62d8148bd4b1733c1a7c59e6',
    '62d826c15d3dccc2ffebdca1'
  )
  
  pilot_II <- c(
    "608e74070357794c8c355e75",
    "6162c20e9c675c15e82494ec",
    "6110c7aa3d662738db309a89",
    "6047b29b56acb503ce4319f5"
  )
  
  e_true <- c(
    '606c7d27f4f3ae688332a55d'
  )
  
  e_true_ii <- c(
    "601941db6605160008690742",
    "606f6f81ff1ba0b8455ec8c4",
    "611118bb5a34e8119eb47ed6"
  )
  
  e_true_iii <- c(
    "5f13334daab04a01f1bee1bd",
    "601032f77969062d05802f88",
    "60c33a1bde764fbff560a573",
    "60cf6c61cd67587eba89a915",
    "60e703e4908998ebc5679e8a" 
  )
  
  e_true_iv <- c(
    "60ef5b1cf52939a80af77543",
    "60bb3b463887c2f9d1385cce",
    "60ddfb3db6a71ad9ba75e387",
    "605ddb3c61e1ce50865c3874",
    "603a758c5fc59967a708e5f4",
    "5eff6828a958150135ede8a4",
    "5e10709fb63853754eff7d28",
    "6105d72b52b6f2348973856f",
    "614aad5f39fe300e0f0b9be7",
    "6147e59cb48beb204aef2732",
    "616aea6b17045149d16aca39",
    "61608875c054bf0692dcd8ee",
    "613cfaecee50fc5d702c9cfc",
    "611ce118d137797315f04b9b",
    "6164e1b26996fe46860b2291"
  )
  
  e_true_v <- c(
    "60ddfb3db6a71ad9ba75e387"
  )
  
  e_true_vi <- c(
    "5ecae71ae38d170cd1ef0744",
    "5f6a50062979ef0ee95ba54b",
    "6022812b3081be01df18e8c8",
    "611ad1102273f42e9d8425a0",
    "5ee2726cb596ff36d1faa360",
    "6005684e172c9b77028985f3",
    "615aabfd1bbb3b87b2474b18",
    "612ecee5e7d44d3d46be3722",
    "614c9257e1f394d0fbc4477d",
    "611db360a2a64af97385a26a",
    "60dc2623fd85cf0a3e3ed8ef",
    "614e5e0f44f5b5284bea47b9",
    "60fa757249e2a29c22b22431",
    "61366992ad7770594c043ab6",
    "60ed8a50f6799ac63ad1cc81",
    "6107a6c1bb9d83f0f2a2b001",
    "6162daf69e4d012b71c4383c"
  )
  
  e_true_vii <- c(
    "61717173748006894b2b54ff",
    "601129f77e0c21000b0c408a",
    "616e5ae706e970fe0aff99b6",
    "5e5d66f2101bf703d65326bc",
    "5f338ba6ea047119dbd6e49e",
    "5ee7b7c9eef92207297a0ad4",
    "60eabe920b976e0972bfa41d",
    "5e11b252deea2b84136a5d21",
    "5eb6f85c1c54b4067b4ba65d",
    "60fdcd33665754977f930324",
    "5c7341f83a67ad00016ec50b",
    "6130e97d4106299f8c6120fa",
    "61372118b7dde713e24191e0",
    "61645364aa6fda7444570fe0"
  )
  
  returned_timeout <- c(
    pilot_I, pilot_II, 
    e_true, e_true_ii, e_true_iii,
    e_true_iv, e_true_v, e_true_vi,
    e_true_vii
  )
  
  return(returned_timeout)
}

timeout_and_returns_e3 <- function() {
  pilot_1 <- c("60a838a519ca13dddc5b36d3")
  
  all_ps <- c(pilot_1)
  
  return(all_ps)
}

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


load_data <- function(path_data, participants_returned) {
  #' load continuous reproduction ("cr") and category learning ("cat") data
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles;
  #' takes for each participant and task the table containing most data
  #'  
  #' @return a list with the two tibbles
  #' 
  # read individual performance
  
  # check for each participant which file has more data and select that one
  
  
  files_dir <- dir(path_data)
  fld_cat <- files_dir[startsWith(files_dir, "cat")]
  fld_cr <- files_dir[startsWith(files_dir, "cr")]
  paths_cr_individual <- str_c(path_data, fld_cr[!str_detect(fld_cr, "allinone")])
  paths_cat_individual <- str_c(path_data,  fld_cat[!str_detect(fld_cat, "allinone")])
  paths_cat_compound <- str_c(path_data,  fld_cat[str_detect(fld_cat, "allinone")])
  paths_cr1_compound <- str_c(path_data, fld_cr[str_detect(fld_cr, "allinone-p1")])
  paths_cr2_compound <- str_c(path_data, fld_cr[str_detect(fld_cr, "allinone-p2")])
  
  l_paths <- list(
    `cr` = paths_cr_individual, 
    `cat` = paths_cat_individual, 
    `cat-allinone` = paths_cat_compound, 
    `cr-allinone-p1` = paths_cr1_compound, 
    `cr-allinone-p2` = paths_cr2_compound
  )
  
  inner_map <- safely(function(x) map(x, json_to_tibble))
  l_tbl_all <- map(l_paths, inner_map)
  l_tbl_all <- map(l_tbl_all, "result")
  l_mask <- map_lgl(l_tbl_all, ~!(is.null(.x)))
  l_tbl_all <- l_tbl_all[l_mask]
  inner_map <- function(a, b) map(
    a, function(x) c(participant_id = x$participant_id[1], ntrials = nrow(x))
  ) %>% reduce(rbind) %>% rbind() %>% as_tibble() %>% mutate(savemethod = b)
  tbl_ntrials <- map2(l_tbl_all, names(l_tbl_all), inner_map) %>% reduce(rbind)
  tbl_ntrials$task <- factor(str_detect(tbl_ntrials$savemethod, "cr"), labels = c("cat", "cr"))
  files_select <- tbl_ntrials %>% group_by(participant_id, task) %>%
    mutate(rwn_max = row_number(desc(ntrials))) %>% 
    filter(rwn_max == 1)
  l_files_select <- split(files_select, files_select$task)
  c_paths <- function(x) str_c(path_data, x$savemethod, "-participant-", x$participant_id, ".json")
  l_paths <- map(l_files_select, c_paths)
  
  tbl_cr <- reduce(map(l_paths[["cr"]], json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_cat <- reduce(map(l_paths[["cat"]], json_to_tibble), rbind)
  # add stim_id
  tbl_cr$stim_id <- (floor(tbl_cr$x1_true/9) - 1) * 10 + (floor(tbl_cr$x2_true/9) - 1) + 1
  
  # only pilot data have to be corrected currently...
  tbl_cr$session <- as.numeric(tbl_cr$session)
  #tbl_cr[148:nrow(tbl_cr), "session"] <- 2 + tbl_cr[148:nrow(tbl_cr), "session"]
  
  factors <- c("participant_id", "session", "cat_true")
  numerics <- c("trial_id", "x1_true", "x2_true", "x1_response", "x2_response", "rt")
  tbl_cr <- fix_data_types(tbl_cr, factors, numerics)
  tbl_cat <- fix_data_types(tbl_cat, factors, numerics)
  
  tbl_cr <- tbl_cr %>% filter(!(participant_id %in% participants_returned))
  tbl_cat <- tbl_cat %>% filter(!(participant_id %in% participants_returned))
  
  l_data <- list(tbl_cr, tbl_cat)
  return(l_data)
}



json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  tbl_cr <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(tbl_cr)
}


load_data_e3 <- function(path_data, participants_returned) {
  #' load simultaneous comparison ("sim_simult") and category learning ("cat") data
  #' for E3
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles;
  #' takes for each participant and task the table containing most data
  #' 
  #' @return a list with the two tibbles
  #' 
  # read individual performance
  
  # check for each participant which file has more data and select that one
  files_dir <- dir(path_data)
  fld_cat <- files_dir[startsWith(files_dir, "cat")]
  fld_sim_simult <- files_dir[startsWith(files_dir, "sim_simult")]
  paths_sim_simult_individual <- str_c(path_data, fld_sim_simult[!str_detect(fld_sim_simult, "allinone")])
  paths_cat_individual <- str_c(path_data,  fld_cat[!str_detect(fld_cat, "allinone")])
  paths_cat_compound <- str_c(path_data,  fld_cat[str_detect(fld_cat, "allinone")])
  paths_sim_simult_compound1 <- str_c(path_data, fld_sim_simult[str_detect(fld_sim_simult, "allinone-p1")])
  paths_sim_simult_compound2 <- str_c(path_data, fld_sim_simult[str_detect(fld_sim_simult, "allinone-p2")])
  
  l_paths <- list(
    `sim_simult` = paths_sim_simult_individual, 
    `cat` = paths_cat_individual, 
    `cat-allinone` = paths_cat_compound, 
    `sim_simult-allinone-p1` = paths_sim_simult_compound1, 
    `sim_simult-allinone-p2` = paths_sim_simult_compound2
  )
  
  inner_map <- safely(function(x) map(x, json_to_tibble))
  l_tbl_all <- map(l_paths, inner_map)
  l_tbl_all <- map(l_tbl_all, "result")
  l_mask <- map_lgl(l_tbl_all, ~!(is.null(.x)))
  l_tbl_all <- l_tbl_all[l_mask]
  inner_map <- function(a, b) map(
    a, function(x) c(participant_id = x$participant_id[1], ntrials = nrow(x))
  ) %>% reduce(rbind) %>% rbind() %>% as_tibble() %>% mutate(savemethod = b)
  tbl_ntrials <- map2(l_tbl_all, names(l_tbl_all), inner_map) %>% reduce(rbind)
  tbl_ntrials$task <- factor(str_detect(tbl_ntrials$savemethod, "sim_simult"), labels = c("cat", "sim_simult"))
  files_select <- tbl_ntrials %>% group_by(participant_id, task) %>%
    mutate(rwn_max = row_number(desc(ntrials))) %>% 
    filter(rwn_max == 1)
  l_files_select <- split(files_select, files_select$task)
  c_paths <- function(x) str_c(path_data, x$savemethod, "-participant-", x$participant_id, ".json")
  l_paths <- map(l_files_select, c_paths)
  
  tbl_simult <- reduce(map(l_paths[["sim_simult"]], json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_cat <- reduce(map(l_paths[["cat"]], json_to_tibble), rbind)
  
  factors <- c("participant_id", "session", "cat_true", "n_categories")
  numerics <- c(
    "trial_id", "x1_true", "x2_true", "x1_true_l", "x2_true_l", 
    "x1_true_r", "x2_true_r", "x1_response", "x2_response", "response", 
    "accuracy", "rt"
  )
  tbl_simult <- fix_data_types(tbl_simult, factors, numerics)
  tbl_cat <- fix_data_types(tbl_cat, factors, numerics)
  
  tbl_simult <- tbl_simult %>% filter(!(participant_id %in% participants_returned))
  tbl_cat <- tbl_cat %>% filter(!(participant_id %in% participants_returned))
  
  # add comparison pool for simultaneous comparison task
  tbl_simult <- assign_comparison_pool(tbl_simult)
  
  l_data <- list(tbl_simult = tbl_simult, tbl_cat = tbl_cat)
  return(l_data)
}

assign_comparison_pool <- function(tbl_df) {
  tbl_df$comparison_pool <- "same"
  tbl_df$pool_left <- 1
  tbl_df$pool_left[tbl_df$x1_true_l < 50 & tbl_df$x2_true_l > 50] <- 2
  tbl_df$pool_left[tbl_df$x1_true_l > 50 & tbl_df$x2_true_l < 50] <- 3
  tbl_df$pool_left[tbl_df$x1_true_l > 50 & tbl_df$x2_true_l > 50] <- 4
  tbl_df$pool_right <- 1
  tbl_df$pool_right[tbl_df$x1_true_r < 50 & tbl_df$x2_true_r > 50] <- 2
  tbl_df$pool_right[tbl_df$x1_true_r > 50 & tbl_df$x2_true_r < 50] <- 3
  tbl_df$pool_right[tbl_df$x1_true_r > 50 & tbl_df$x2_true_r > 50] <- 4
  tbl_df$comparison_pool[tbl_df$pool_left == tbl_df$pool_right] <- 0
  
  is_cross_pool <- function(l, r) {
    lr_sorted <- sort(c(l, r))
    if (sum(lr_sorted == c(1, 4)) == 2 | sum(lr_sorted == c(2, 3)) == 2) {
      out <- "cross"
    } else if (lr_sorted[1] == lr_sorted[2]) {
      out <- "same"
    } else {out <- "side"}
    return(out)
  }  
  
  tbl_df$comparison_pool <- map2_chr(tbl_df$pool_left, tbl_df$pool_right, is_cross_pool)
  
  return(tbl_df)
}


exclude_incomplete_datasets <- function(l_tbl, n_resp_cr, n_resp_cat) {
  #' exclude incomplete data
  #' 
  #' @description exclude incomplete data from continuous reproduction ("cr")
  #' and category learning ("cat") data
  #' 
  #' @return a list with the two tibbles
  #' 
  tbl_cr <- l_tbl[[1]]
  tbl_cat <- l_tbl[[2]]
  participants_before <- unique(tbl_cr$participant_id)
  
  # some participants seem to have restarted the experiment: > 200 cr responses
  tbl_cr_n <- tbl_cr %>% 
    arrange(participant_id, session, trial_id) %>%
    mutate(
      trial_inseq = 1:nrow(tbl_cr)
    ) %>%
    group_by(participant_id, session, trial_id) %>%
    mutate(
      rwn = row_number(trial_inseq)
    ) %>% group_by(participant_id) %>% filter(rwn == 1) %>%
    summarize(n_resp = n()) %>%
    ungroup() %>% arrange(n_resp) %>% filter(n_resp >= n_resp_cr)
  
  # some participants seem to have restarted the experiment: > 640 cat responses
  tbl_cat_n <- tbl_cat %>% 
    group_by(participant_id) %>% summarise(n_resp = n()) %>% 
    ungroup() %>% arrange(n_resp) %>% filter(n_resp >= n_resp_cat)
  
  participants_after <- intersect(unique(tbl_cr_n$participant_id), unique(tbl_cat_n$participant_id))
  
  tbl_cr_incl <- tbl_cr %>% filter(participant_id %in% participants_after)
  tbl_cat_incl <- tbl_cat %>% filter(participant_id %in% participants_after)
  
  
  tbl_cr_excl <- tbl_cr %>% 
    filter(participant_id %in% participants_before[!(participants_before %in% participants_after)])
  tbl_cat_excl <- tbl_cat %>% 
    filter(participant_id %in% participants_before[!(participants_before %in% participants_after)])
  
  cat(str_c("\n", length(participants_before) - length(participants_after), " incomplete data sets (i.e., one data set per participant)\n"))
  
  return(list(
    keep = list(tbl_cr = tbl_cr_incl, tbl_cat = tbl_cat_incl), 
    drop = list(tbl_cr = tbl_cr_excl, tbl_cat = tbl_cat_excl)
  ))
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


exclude_simult_outliers <- function(l_tbl, n_sds) {
  #' exclude outliers in simultaneous comparison task
  #' 
  #' @description correlation response x euclidean deviation > n_sds above mean deviation are excluded
  #' @param l_tbl list containing tbl dfs with rating and cateogry learning / sequential comparison data
  #' @param n_sds how many sds above the mean is the cutoff?
  #' 
  #' @return a list with participants kept and participants dropped 
  #'
  tbl_simult <- l_tbl[[1]]
  l_simult <- split(tbl_simult[, c("d_euclidean", "response")], as.character(tbl_simult$participant_id))
  v_cor_simult <- map_dbl(l_simult, ~ cor(.x$d_euclidean, .x$response))
  tbl_cor_simult <- tibble(participant_id = names(v_cor_simult), cor = v_cor_simult)
  cor_summary <- summary(v_cor_simult)
  thx_dropout <- cor_summary["Mean"] + 3*sd(v_cor_simult)
  participants_drop <- tbl_cor_simult$participant_id[tbl_cor_simult$cor > thx_dropout]
  participants_keep <- tbl_cor_simult$participant_id[tbl_cor_simult$cor <= thx_dropout]
  return(list(
    keep = list(
      tbl_simult = tbl_simult %>% filter(participant_id %in% participants_keep), 
      tbl_cat_sim = l_tbl[[2]] %>% filter(participant_id %in% participants_keep)), 
    drop = list(
      tbl_simult = tbl_simult %>% filter(participant_id %in% participants_drop), 
      tbl_cat_sim = l_tbl[[2]] %>% filter(participant_id %in% participants_drop))
  ))
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
    ) %>% group_by(participant_id, n_categories, x1_true_binned, x2_true_binned) %>%
    summarise(
      avg_deviation_x1x2 = mean(sqrt(x1_deviation^2 + x2_deviation^2)),
      n_trials = n()
    ) %>%
    group_by(participant_id) %>%
    mutate(avg_deviation = mean(avg_deviation_x1x2), n_trials = sum(n_trials)) %>%
    ungroup() %>% 
    arrange(avg_deviation) %>%
    mutate(participant_id = fct_inorder(participant_id, ordered = TRUE))
  tbl_cr_agg_2 <- tbl_cr_agg %>%
    group_by(participant_id) %>%
    mutate(rwn = row_number(x1_true_binned)) %>% filter(rwn == 1) %>%
    ungroup()
  return(list(tbl_cr_agg, tbl_cr_agg_2))
}


category_centers <- function(f_stretch, f_shift) {
  #' helper function to define category centers
  #' 
  #' @description returns x1 and x2 means of the ellipse categories (n_categories - 1)
  #' @param f_stretch stretches ellipses, which are drawn from 0-9
  #' @param f_shift shifts ellipses according to the value
  #' 
  #' @return the list with the centers of the 2 and 4 category conditions
  #' 
  # read individual performance
  x1 <- seq(0, 9, by = 1)
  x2 <- seq(0, 9, by = 1)
  tbl_tmp <- crossing(x1, x2)
  tbl_tmp <- tbl_tmp %>% mutate(stim_id = seq(1, 100, by = 1))
  l_ellipses <- map(c(2, 3), create_ellipse_categories, tbl = tbl_tmp)
  cat_boundaries_2 <- l_ellipses[[1]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * f_stretch + f_shift, y_rotated = (y_rotated + 1) * f_stretch + f_shift)
  cat_boundaries_3 <- l_ellipses[[2]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * f_stretch + f_shift, y_rotated = (y_rotated + 1) * f_stretch + f_shift)
  l_ellipses[[1]][[2]] <- cat_boundaries_2
  l_ellipses[[2]][[2]] <- cat_boundaries_3
  
  
  category_means <- function(tbl) {
    tbl %>% group_by(category) %>%
      summarize(x_mn = mean(x_rotated), y_mn = mean(y_rotated)) %>% ungroup()
  }
  cat_2_mns <- category_means(cat_boundaries_2)
  cat_3_mns <- category_means(cat_boundaries_3)
  l_cat_mns <- list(cat_2_mns, cat_3_mns)
  return(list(l_cat_mns, l_ellipses))
}



category_centers_squares <- function(n_cats, is_simulation = FALSE) {
  #' helper function to define square category centers
  #' 
  #' @description returns x1 and x2 means of the square categories
  #' @param n_cats vector with total number of categories for all square conditions
  #' 
  #' @return a tbl_df with the two dimensional centers of the categories
  #'
  category_centers_one_condition <- function(n_cats) {
    if (is_simulation) {
      tbl_borders <- tibble(max_x = 10, min_x = 0)
    } else {
      tbl_borders <- tibble(max_x = 100, min_x = 1)
    }
    
    n_segments <- sqrt(n_cats)
    x_widths <- rep((tbl_borders$max_x - tbl_borders$min_x) / n_segments, n_segments - 1)
    x_boundaries <- c(
      tbl_borders$min_x,
      tbl_borders$min_x + cumsum(x_widths),
      tbl_borders$max_x
    )
    # save fine grid of values along decision boundaries
    x_boundaries_no_edges <- x_boundaries[2:(length(x_boundaries) - 1)]
    x_draws <- seq(x_boundaries[1], x_boundaries[length(x_boundaries)], by = .01)
    x_boundaries_draws <- rbind(
      crossing(x = x_boundaries_no_edges, y = x_draws),
      crossing(x = x_draws, y = x_boundaries_no_edges)
    )
    # calculate means of categories
    x_cat_means <- map2_dbl(
      1:(length(x_boundaries)-1), 2:length(x_boundaries), 
      ~ (x_boundaries[.x] + x_boundaries[.y]) / 2
    )
    cat_means <- crossing(x_mn = x_cat_means, y_mn = x_cat_means) %>%
      mutate(category = seq(1, n_cats, by = 1)) %>%
      relocate(category, .before = x_mn)
    return(list(cat_means = cat_means, x_boundaries_draws = x_boundaries_draws))    
  }
  
  l_centers_bds_conditions <- map(n_cats, category_centers_one_condition)
  names(l_centers_bds_conditions) <- n_cats
  return(l_centers_bds_conditions)
}


euclidian_distance_to_center <- function(x_mn, y_mn, tbl, is_response) {
  #' helper function calculating Euclidean distance
  if(is_response) {
    sqrt((tbl$x1_response - x_mn)^2 + (tbl$x2_response - y_mn)^2)
  } else {
    sqrt((tbl$x1_true - x_mn)^2 + (tbl$x2_true - y_mn)^2)
  }
}


add_distance_to_representation_center <- function(tbl_cr, l_m_nb_pds) {
  # this has to be calculated for each participant compared to theoretical mean
  distance_to_individual_center <- function(p_id) {
    l_mn <- list(x_mn = l_m_nb_pds[[p_id]][["x1_true"]][1, 2],
                 y_mn = l_m_nb_pds[[p_id]][["x2_true"]][1, 2])
    tbl_tmp <- tbl_cr %>% filter(substr(p_id, 1, 6) == p_id)
    tbl_tmp$d_rep_center <- euclidian_distance_to_center(
      l_mn[["x_mn"]], l_mn[["y_mn"]], tbl_tmp, is_response = TRUE
    )
    return(tbl_tmp)
  }
  p_ids_2_cats <- substr(
    unique(as.character(tbl_cr$participant_id[tbl_cr$n_categories == 2])), 1, 6
  )
  
  l_ds <- map(p_ids_2_cats, distance_to_individual_center)
  tbl_cr <- reduce(l_ds, rbind)
  return(tbl_cr)
}

add_distance_to_nearest_center <- function(tbl_cr, l_centers, is_simulation, sim_center) {
  #' add distance to closest category centroid
  #' 
  #' @description calculates distances to all possible category centroids 
  #' and returns min of those
  #' @param tbl_cr the tibble with the by-trial responses
  #' @param l_centers a nested list with three tbl_dfs containing
  #' @param is_simulation simulation or empirical results
  #' @param sim_center compute distance to ellipse center or square center in the similarity condition
  #' (a) the coordinates of the ellipse category centers
  #' (b) the coordinates of a large number of samples from the ellipse
  #' (c) the coordinates of the square category centers
  #' 
  #' @return the tibble with the min distance as added column
  #' 
  closest_distance_given_several_means <- function(n_cat, n_cat_compare, mns, is_ellipse) {
    n_cat_str <- as.character(n_cat)
    tbl_d_true <- pmap(mns, euclidian_distance_to_center, tbl = l_tbl_cr[[n_cat_str]], is_response = FALSE) %>% 
      unlist() %>% matrix(ncol = n_cat_compare) %>% as.data.frame() %>% tibble()
    colnames(tbl_d_true) <- str_c("d", 1:n_cat_compare)
    tbl_d_response <- pmap(mns, euclidian_distance_to_center, tbl = l_tbl_cr[[n_cat_str]], is_response = TRUE) %>% 
      unlist() %>% matrix(ncol = n_cat_compare) %>% as.data.frame() %>% tibble()
    colnames(tbl_d_response) <- seq(1, n_cat_compare, by = 1)
    col_idx_closest <- apply(tbl_d_true, 1, function(x) which(x == min(x))[1])
    tbl_d_response$col_idx_closest <- col_idx_closest
    tbl_d_response$d_closest <- apply(tbl_d_response, 1, function(x) x[1:n_cat_compare][x[(n_cat_compare+1)]])
    d_closest <- as_vector(tbl_d_response$d_closest) %>% unname()
    l_tbl_cr[[n_cat_str]] <- l_tbl_cr[[n_cat_str]] %>% cbind(d_closest)
    if (!is_simulation) l_tbl_cr[[n_cat_str]] <- l_tbl_cr[[n_cat_str]] %>% cbind(category = col_idx_closest)
    return(l_tbl_cr[[n_cat_str]])
  }
  
  l_cat_mns_ellipses <- l_centers[[1]]
  l_ellipses <- l_centers[[2]]
  if (sim_center == "square") {
    l_cat_mns_squares <- l_centers[[3]]$`4`$cat_means
    l_cat_bds_squares <- l_centers[[3]]$`4`$x_boundaries_draws
  }
  
  
  v_categories <- unique(tbl_cr$n_categories)
  
  # split by nr of categories
  l_tbl_cr <- split(tbl_cr, tbl_cr$n_categories)
  
  # as only one category center, can directly compute distance from response to that center
  tbl_d2 <- pmap(l_cat_mns_ellipses[[1]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["2"]], is_response = TRUE) %>%
    unlist() %>% matrix(ncol = 1) %>% as.data.frame() %>% tibble()
  colnames(tbl_d2) <- c("d_closest")
  
  # the following is not very clean, because it is not implemented for three categories a couple of lines below
  if (!is.null(l_tbl_cr[["2"]])) {
    if (is_simulation) {
      l_tbl_cr[["2"]] <- l_tbl_cr[["2"]] %>% cbind(tbl_d2)
    } else {
      l_tbl_cr[["2"]] <- l_tbl_cr[["2"]] %>% cbind(tbl_d2) %>% left_join(l_ellipses[[1]][[1]] %>% select(stim_id, category), by = c("stim_id"))
      l_tbl_cr[["2"]]$category <- as.numeric(l_tbl_cr[["2"]]$category)
    }
  }
  
  if ("3" %in% v_categories) {
    # for three categories, we first have to compute what the closest center of a given stimulus is and then index using that id
    l_tbl_cr[["3"]] <- closest_distance_given_several_means(3, 2, l_cat_mns_ellipses[[2]][, c("x_mn", "y_mn")], is_ellipse = TRUE)
  }
  
  if ("4" %in% v_categories) {
    # for four categories, we first have to compute what the closest center of a given stimulus is and then index using that id
    l_tbl_cr[["4"]] <- closest_distance_given_several_means(4, 4, l_cat_mns_squares[, c("x_mn", "y_mn")], is_ellipse = FALSE)
  }
  
  # for the baseline condition either the midpoint of the grid 
  # can be used as the "category center"; sim_center == "ellipse"
  if (sim_center == "ellipse") {
    tbl_d1 <- pmap(l_cat_mns_ellipses[[1]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["1"]], is_response = TRUE) %>%
      unlist() %>% matrix(ncol = 1) %>% as.data.frame() %>% tibble()
    colnames(tbl_d1) <- c("d_closest")
    l_tbl_cr[["1"]] <- l_tbl_cr[["1"]] %>% cbind(tbl_d1) %>% mutate(category = 1)
  } else if (sim_center == "square") {
    # or the four category centers of the squares
    l_tbl_cr[["1"]] <- closest_distance_given_several_means(1, 4, l_cat_mns_squares[, c("x_mn", "y_mn")], is_ellipse = FALSE)
  }
  
  idxs <- names(l_tbl_cr) %in% v_categories
  tbl_cr <- rbind(reduce(l_tbl_cr[idxs], rbind)) %>% as_tibble()
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


add_deviations <- function(
    l_tbl, sim_center, 
    subset_ids = NULL, 
    slider_start_postition = NULL
) {
  #' by-trial, binned, and average deviations of reproduction responses
  #' 
  #' @description calculate deviations from true coordinates to 
  #' response coordinates by-trial, averaged into bins, and
  #' further averaged over bins
  #' @param l_tbl a list containing the tibble with the by-trial cr 
  #' and sim/cat responses
  #' @param sim_center can be either of "ellipse" or "square" to 
  #' define whether distances in similarity condition are 
  #' computed with regards to ellipse or square categories
  #' @param subset_ids a subset of participants to filter
  #' @param slider_start_position were sliders in the reproduction task
  #' located in the middle or placed randomly in each trial
  #' 
  #' @return a list with three tbls
  #' 
  tbl_cr <- l_tbl[[1]]
  if (!is.null(subset_ids)){
    tbl_cr <- tbl_cr %>% filter(participant_id %in% subset_ids)
  }
  # add deviation variables
  tbl_cr$x1_deviation <- tbl_cr$x1_true - tbl_cr$x1_response
  tbl_cr$x2_deviation <- tbl_cr$x2_true - tbl_cr$x2_response
  tbl_cr$eucl_deviation <- sqrt(tbl_cr$x1_deviation^2 + tbl_cr$x2_deviation^2)
  if (!is.null(slider_start_postition)) {
    tbl_cr$move_x1 <- abs(tbl_cr$x1_start - tbl_cr$x1_response)
    tbl_cr$move_x2 <- abs(tbl_cr$x2_start - tbl_cr$x2_response)
    tbl_cr$move_sum <- tbl_cr$move_x1 + tbl_cr$move_x2
    tbl_cr <- dplyr::select(tbl_cr, -c(move_x1, move_x2))
  }
  l_centers <- category_centers(f_stretch = 9, f_shift = 1)
  l_centers[[3]] <- category_centers_squares(n_cats = c(4))
  # todo
  # variable indicating whether distance in similarity condition is calculated with regards to 2 or 4 category group
  tbl_cr <- add_distance_to_nearest_center(tbl_cr, l_centers, is_simulation = FALSE, sim_center = sim_center)
  tbl_cr <- add_distance_to_nearest_boundary(tbl_cr, l_centers, allocate_sim = sim_center)
  
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


exclude_cr_outliers <- function(l_tbl, n_sds) {
  #' exclude outliers from reproduction and categorization tbls
  #' 
  #' @description participant is excluded if average reproduction deviation
  #' exceeds mean + n_sds standard deviations
  #' @param l_tbl list containing tbl_cr the tibble with the by-trial reproduction responses and 
  #' tbl_cat_sim the tibble with the by-trial similarity/categorization responses
  #' @param n_sds nr of standard deviations to calculate thx
  #' 
  #' @return a list with two tbls
  #' 
  tbl_cr <- l_tbl[[1]]
  tbl_cat_sim <- l_tbl[[2]]
  participants_before <- unique(tbl_cr$participant_id)
  participants_included <- exclude_reproduction_outliers(tbl_cr, n_sds)
  participants_excluded <- participants_before[!(participants_before %in% participants_included$participant_id)]
  tbl_cr_incl <- inner_join(
    participants_included[, "participant_id"], tbl_cr, by = "participant_id"
  )
  cat(str_c(
    "excluded ", length(participants_excluded), " participants worse than ",
    n_sds, " sds below the mean reproduction error\n"
  ))
  tbl_cat_sim_incl <- inner_join(
    participants_included[, "participant_id"], tbl_cat_sim, by = "participant_id"
  )
  l_keep <- list(
    tbl_cr = tbl_cr_incl,
    tbl_cat_sim = tbl_cat_sim_incl
  )
  l_drop <- list(
    tbl_cr = tbl_cr %>% filter(participant_id %in% participants_excluded),
    tbl_cat_sim = tbl_cat_sim %>% filter(participant_id %in% participants_excluded)
  )
  return(list(keep = l_keep, drop = l_drop))
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


aggregate_category_responses_by_x1x2 <- function(tbl_cat, trial_id_start_incl) {
  #' aggregate category responses per x1-x2 grid cell
  #' 
  #' @description calculate mean and mode responses per x1-x2 grid cell
  #' @param tbl_cat_sim the tibble with the by-trial categorization responses
  #' @param trial_id_start_incl first trial to be considered
  #' 
  #' @return the aggregated tbl
  #' 
  tbl_cat_grid <- tbl_cat %>% 
    filter(trial_id >= trial_id_start_incl) %>%
    group_by(participant_id, n_categories, x1_true, x2_true, response) %>%
    count() %>% arrange(participant_id, x1_true, x2_true) %>%
    group_by(participant_id, n_categories, x1_true, x2_true) %>%
    mutate(response_mean = mean(response)) %>%
    filter(n == max(n)) %>% ungroup() %>%
    mutate(
      val_random = runif(nrow(.))
    ) %>% group_by(participant_id, n_categories, x1_true, x2_true) %>%
    mutate(rank_random = rank(val_random)) %>%
    arrange(n_categories) %>%
    ungroup() %>% filter(rank_random == 1) %>%
    pivot_longer(c(response_mean, response))
  
  
  tbl_cat_grid <- tbl_cat_grid %>% left_join(
    tbl_cat_overview[, c("participant_id", "mean_accuracy")], by = "participant_id"
  ) %>% mutate(
    name = fct_inorder(name),
    name = fct_relabel(name, function(x) return(c("Mean", "Mode")))
  )
  
  return(tbl_cat_grid)
  
}


fit_predict_nb <- function(participant_id, tbl) {
  #' fit and predict by-participant category responses using true x1-x2 values as predictors
  #' 
  #' @description fits a naive Bayes classifier to the data in tbl for the given participant_id
  #' additionally, creates a tbl with the posterior densities for an evenly space grid over x1 and x2
  #' only for category 2 (i.e., the target category)
  #' @param participant_id the participant to fit and predict
  #' @param tbl tbl with the category-learning data
  #' 
  #' @return a list with the fitted model and with the predicted probabilities
  #'
  
  # helper function calculating densities for grid 
  nb_cat2_densities <- function(params, cat = 2) {
    # evenly space grid
    x1 <- seq(0, 100, by = 2.5)
    x2 <- x1
    grid_eval <- crossing(x1, x2)
    grid_eval$density <- pmap_dbl(
      grid_eval, function(x1, x2) {
        dmvnorm(x = c(x1, x2), 
                mean = c(params[["m1"]], params[["m2"]]), 
                sigma = matrix(c(params[["sd1"]], 0, 0, params[["sd2"]]), nrow = 2))
      })
    grid_eval$category <- cat
    return(grid_eval)
  }
  
  tbl <- tbl[tbl$participant_id == participant_id, ]
  m_nb <- naive_bayes(tbl[, c("x1_true", "x2_true")], as.character(tbl$response))
  n_cats <- max(as.numeric(tbl$n_categories))
  l_params <- map(
    1:n_cats, 
    ~ c(
      m_nb$tables[["x1_true"]][, .x], 
      m_nb$tables[["x2_true"]][, .x]
    ) %>% as.list()
  )
  for (i in 1:length(l_params)) {
    names(l_params[[i]]) <- c("m1", "sd1", "m2", "sd2")
  }
  tbl_densities <- map2(l_params, 1:n_cats, nb_cat2_densities) %>% reduce(rbind)
  tbl_densities$participant_id <- participant_id
  l_nb <- list(m = m_nb, tbl_densities = tbl_densities)
  return(l_nb)
}

exclude_guessing_participants <- function(l_tbl, n_trials_cat) {
  #' exclude participants guessing in categorization task
  #' 
  #' @description exclude participants below 99.9% percentile
  #' of binomial distribution with prob = .5
  #' @param l_tbl list with tbl_cr and tbl_cat
  #'
  tbl_sim <- l_tbl[[2]] %>% filter(n_categories == "1")
  tbl_cat <- l_tbl[[2]] %>% filter(n_categories != "1")
  tbl_cr <- l_tbl[[1]]
  
  thx_guessing_excl_two <- qbinom(.999, n_trials_cat, .5)
  thx_guessing_excl_four <- qbinom(.999, n_trials_cat, .25)
  # this has to be moved to exclusion functions
  participants_guess_2 <- tbl_cat %>% filter(n_categories == 2) %>%
    group_by(participant_id) %>%
    summarize(count_true = sum(accuracy)) %>% ungroup() %>%
    arrange(count_true) %>% filter(count_true <= thx_guessing_excl_two) %>%
    dplyr::select(participant_id) %>% unique()
  participants_guess_4 <- tbl_cat %>% filter(n_categories == 4) %>%
    group_by(participant_id) %>%
    summarize(count_true = sum(accuracy)) %>% ungroup() %>%
    arrange(count_true) %>% filter(count_true <= thx_guessing_excl_four) %>%
    dplyr::select(participant_id) %>% unique()
  tbl_cr_keep <- tbl_cr %>% filter(!(participant_id %in% c(participants_guess_2$participant_id, participants_guess_4$participant_id)))
  tbl_cat_sim_keep <- tbl_cat %>% filter(!(participant_id %in% c(participants_guess_2$participant_id, participants_guess_4$participant_id))) %>%
    rbind(tbl_sim)
  tbl_cr_drop <- tbl_cr %>% filter((participant_id %in% c(participants_guess_2$participant_id, participants_guess_4$participant_id)))
  tbl_cat_drop <- tbl_cat %>% filter((participant_id %in% c(participants_guess_2$participant_id, participants_guess_4$participant_id)))
  cat(str_c("excluded ", nrow(participants_guess_2) + nrow(participants_guess_4), " participants guessing in categorization task\n"))
  
  return(list(
    keep = list(tbl_cr = tbl_cr_keep, tbl_cat_sim = tbl_cat_sim_keep), 
    drop = list(tbl_cr = tbl_cr_drop, tbl_cat = tbl_cat_drop)
  ))
}

preprocess_data <- function(l_tbl_data, n_resp_cr, n_resp_cat) {
  #' data preprocessing pipeline
  #' 
  #' @description excludes incomplete data sets, outliers in 
  #' reproduction task, and participants guessing in 
  #' categorization task
  #' @param l_tbl_data list with tbl_cr and tbl_cat as entries
  #' 
  #' @return a list containing the included and excluded data
  #' after each preprocessing step
  #'
  
  # data case handling
  ## people with incomplete data
  l_incomplete <- exclude_incomplete_datasets(l_tbl_data, n_resp_cr, n_resp_cat)
  
  ## reproduction outliers
  l_outliers <- exclude_cr_outliers(l_incomplete$keep, 3)
  
  ## people guessing in categorization task
  l_guessing <- exclude_guessing_participants(l_outliers$keep, n_resp_cat)
  
  ## exclude practice trials in reproduction task
  l_guessing$keep$tbl_cr <- l_guessing$keep$tbl_cr %>% filter(session %in% c(1, 2))
  
  return(list(
    l_incomplete = l_incomplete,
    l_outliers = l_outliers,
    l_guessing = l_guessing
  ))
}

preprocess_data_e3 <- function(l_tbl_data, n_resp_simult, n_resp_cat) {
  #' data preprocessing pipeline for E3
  #' 
  #' @description excludes incomplete data sets and participants guessing in 
  #' categorization task
  #' note. no exclusion criteria for rating task are applied
  #' @param l_tbl_data list with tbl_simult and tbl_cat as entries
  #' 
  #' @return a list containing the included and excluded data
  #' after each preprocessing step
  #'
  ## people with incomplete data
  l_incomplete <- exclude_incomplete_datasets(l_tbl_data, n_resp_simult, n_resp_cat)
  
  ## people guessing in categorization task
  l_guessing <- exclude_guessing_participants(l_incomplete$keep, n_resp_cat)
  
  ## outliers in simultaneous comparison task
  l_outliers <- exclude_simult_outliers(l_guessing$keep, 3)
  
  
  ## exclude practice trials in reproduction task
  l_outliers$keep$tbl_simult <- l_outliers$keep$tbl_simult %>% filter(session %in% c(1, 2))
  
  names(l_incomplete$keep) <- c("tbl_simult", "tbl_cat_sim")
  names(l_incomplete$drop) <- c("tbl_simult", "tbl_cat_sim")
  names(l_guessing$keep) <- c("tbl_simult", "tbl_cat_sim")
  names(l_guessing$drop) <- c("tbl_simult", "tbl_cat_sim")

  return(list(
    l_incomplete = l_incomplete,
    l_guessing = l_guessing,
    l_outliers = l_outliers
  ))
}

calc_bf_posterior <- function(tbl_cat) {
  options(mc.cores = parallel::detectCores() - 2)
  
  scales <- c(.353)
  prior <- paste0("cauchy(0, ", as.character(scales[1]), ")")
  fixefPrior <- c(set_prior(prior, class="b"))
  ranefPrior <- set_prior("gamma(1,0.04)", class="sd")
  
  tbl_cat_agg <- tbl_cat %>% group_by(participant_id, cat_true, trial_id_binned) %>%
    summarize(accuracy_mn = mean(accuracy)) %>% ungroup() %>%
    mutate(
      trial_id_binned = as.numeric(as.character(trial_id_binned)),
      trial_id_binned = scale(trial_id_binned)[,1]
    ) %>% group_by(cat_true, trial_id_binned) %>%
    mutate(
      participant_id_num = row_number(participant_id)
    )
  
  formula <- bf(
    accuracy ~ 1 + trial_id_binned * cat_true +
      (1 + trial_id_binned*cat_true | participant_id)
  )
  
  fit_cat_learn <- brm(
    formula, data = tbl_cat,
    family = bernoulli(link = "logit"),
    #family = gaussian(link = "identity"),
    iter = 2000, warmup = 1000, 
    chains = 3, 
    #cores = 4, 
    #control = list(max_treedepth = 15, adapt_delta = 0.99),       
    prior=c(fixefPrior, ranefPrior),
    save_pars = save_pars(group = FALSE),
    save_model = "experiments/2022-02-category-learning/R/brms-rs.txt"
  )
  #saveRDS(fit_cat_learn, file = "experiments/2022-02-category-learning/R/brms-rs.Rds")
  tbl_posterior <- pivot_chains(fit_cat_learn$fit)
  params_bf <- c("b_Intercept", "b_trial_id_binned", "b_cat_true2")
  l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
  bfs <- l[[1]]
  tbl_thx <- l[[2]]
  
  # plot the posteriors and the bfs
  map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
  
  
  tbl_posterior <- tbl_draws %>% 
    select(starts_with("mu"), .chain) %>% 
    rename(chain = .chain) %>%
    pivot_longer(starts_with("mu"), names_to = "parameter", values_to = "value")
  params_bf <- c("mu[1]")
  l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
  #l[[2]]$value[l[[2]]$variable == "thxhi_x"] <- 1
  rutils::plot_posterior("mu[1]", tbl_posterior, l[[2]])
}


stan_model <- function() {
  # work in progress using stan model
  
  stan_hlm <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int subj;
  vector[n_data] accuracy; // n trials
  matrix[n_data, 4] x; // ic, trial, category, trial:category
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  real <lower=0> sigma;
  matrix[n_subj, 4] b;
  vector[4] mu;
  vector <lower=0>[4] sigma_subject;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  mu_tf[3] = scale_cat * mu[3];
  mu_tf[4] = scale_cat * mu[4];
}

model {
  for (n in 1:n_data) {
    accuracy[n] ~ normal(
    b[subj[n], 1] + b[subj[n], 2] * x[n, 2] + b[subj[n], 3] * x[n, 3] +
    b[subj[n], 4] * x[n, 4], sigma
    );
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
    b[s, 3] ~ normal(mu_tf[3], sigma_subject[3]);
    b[s, 4] ~ normal(mu_tf[4], sigma_subject[4]);
  }
  
  sigma ~ uniform(0.001, 10);
  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  sigma_subject[3] ~ uniform(0.001, 10);
  sigma_subject[4] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
  mu[3] ~ student_t(1, 0, 1);
  mu[4] ~ student_t(1, 0, 1);
}

")
  
  mod <- cmdstan_model(stan_hlm)
  vars <- mod$variables()
  names(vars$data)
  mod$exe_file()
  
  
  mm <- model.matrix(accuracy_mn ~ trial_id_binned*cat_true, data = tbl_cat_agg) %>% as_tibble()
  mm$cat_true2 <- mm$cat_true2 - .5
  mm$`trial_id_binned:cat_true2` <- mm$trial_id_binned * mm$cat_true2
  
  
  l_data <- list(
    n_data = nrow(tbl_cat_agg), n_subj = length(unique(tbl_cat_agg$participant_id_num)), 
    subj = tbl_cat_agg$participant_id_num,
    accuracy = tbl_cat_agg$accuracy_mn,
    x = as.matrix(mm)
  )
  
  
  fit <- mod$sample(
    data = l_data, iter_sampling = 20000, iter_warmup = 1000,
    seed = 4321, max_treedepth = 15, adapt_delta = .99 
  )
  
  saveRDS(fit, file = "experiments/2022-02-category-learning/R/cmdstanr-full-rs.Rds")
  
  
  tbl_summary <- fit$summary()
  tbl_draws <- fit$draws(variables = c("mu_tf", "b"), format = "df")
  tbl_posterior <- tbl_draws %>% 
    select(starts_with("mu"), .chain) %>% 
    rename(chain = .chain) %>%
    pivot_longer(starts_with("mu"), names_to = "parameter", values_to = "value")
  params_bf <- c("mu_tf[1]", "mu_tf[2]", "mu_tf[3]", "mu_tf[4]")
  l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
  #l[[2]]$value[l[[2]]$variable == "thxhi_x"] <- 1
  plot_posterior("mu_tf[1]", tbl_posterior, l[[2]], l[[1]])
  plot_posterior("mu_tf[2]", tbl_posterior, l[[2]], l[[1]])
  plot_posterior("mu_tf[3]", tbl_posterior, l[[2]], l[[1]])
  plot_posterior("mu_tf[4]", tbl_posterior, l[[2]], l[[1]])
  
  
}


add_distance_to_nearest_boundary <- function(tbl_df, l_centers, allocate_sim) {
  #' euclidean distance to closest category boundary
  #' 
  #' @description calculates the distance to the closest point on a
  #' category boundary
  #' @param tbl_df tbl_df with with stimulus positions
  #' 
  #' @return a vector with distances
  #'
  l_tbl_df <- split(tbl_df, tbl_df$n_categories)
  if(allocate_sim == "ellipse") {
    v_cats_ell <- c("1", "2")
    v_cats_sq <- c("4")
  } else if (allocate_sim == "square") {
    v_cats_ell <- c("2")
    v_cats_sq <- c("1", "4")
  }
  tbl_df_ell <- reduce(l_tbl_df[v_cats_ell], rbind)
  tbl_df_sq <- reduce(l_tbl_df[v_cats_sq], rbind)
  # ellipse categories
  ell_samples <- l_centers[[2]][[1]][[2]]
  sq_samples <- l_centers[[3]]$`4`$x_boundaries_draws %>%
    mutate(x_rotated = x, y_rotated = y)
  
  min_distance_to_boundary <- function(bd, x1_data, x2_data) {
    map2_dbl(
      x1_data, x2_data, 
      ~ min(sqrt((.x - bd$x_rotated)^2 + (.y - bd$y_rotated)^2))
    )
  }
  tbl_df_ell$d2boundary_stim <- min_distance_to_boundary(
    ell_samples, as_vector(tbl_df_ell$x1_true), as_vector(tbl_df_ell$x2_true)
  )
  tbl_df_sq$d2boundary_stim <- min_distance_to_boundary(
    sq_samples, as_vector(tbl_df_sq$x1_true), as_vector(tbl_df_sq$x2_true)
  )
  tbl_df <- rbind(tbl_df_ell, tbl_df_sq)
  return(tbl_df)
  
}


representational_distances <- function(p_id, timepoint, tbl_cr) {
  #' pairwise distances between representations
  #' 
  #' @description calculates the matrix of pairwise distances
  #' between all stimuli for one session of continuous reproduction responses
  #' @param p_id the participant id to calculate the matrix for
  #' @param timepoint either "1" or "2" for before and after category
  #' learning, respectively
  #' @param tbl_cr tbl_df with by-trial continuous reproduction responses before and after category learning
  #' 
  #' @return a tbl_df with the pairwise distances
  #'
  tmp1 <- tbl_cr %>% 
    filter(participant_id == p_id) %>% 
    dplyr::select(stim_id, session, x1_response, x2_response, x1_true, x2_true)
  tbl_design <- crossing(l = tmp1$stim_id, r = tmp1$stim_id)
  tbl_pre <- tbl_design %>% 
    left_join(
      tmp1 %>% filter(session == timepoint) %>% dplyr::select(-session), 
      by = c("l" = "stim_id"), suffix = c("_l", "_r")
    ) %>% left_join(
      tmp1 %>% filter(session == timepoint) %>% dplyr::select(-session), 
      by = c("r" = "stim_id"), suffix = c("_l", "_r")
    )
  tbl_pre$d_euclidean_response <- sqrt(
    (tbl_pre$x1_response_l - tbl_pre$x1_response_r)^2 +
      (tbl_pre$x2_response_l - tbl_pre$x2_response_r)^2
  )
  tbl_pre$d_euclidean_true <- sqrt(
    (tbl_pre$x1_true_l - tbl_pre$x1_true_r)^2 +
      (tbl_pre$x2_true_l - tbl_pre$x2_true_r)^2
  )
  return(tbl_pre)
}


delta_representational_distance <- function(p_id, tbl_cr) {
  #' delta of representational distances after - before
  #' 
  #' @description calculates the delta of pairwise distances
  #' between all stimuli of continuous reproduction responses after vs. before
  #' the category learning task
  #' @param p_id the participant id to calculate the matrix for
  #' @param tbl_cr tbl_df with by-trial continuous reproduction responses before and after category learning
  #' 
  #' @return a tbl_df with the deltas after vs. before
  #'
  timepoints <- sort(unique(tbl_cr$session))
  tbl_rsa_before <- representational_distances(p_id, timepoints[1], tbl_cr)
  tbl_rsa_after <- representational_distances(p_id, timepoints[2], tbl_cr)
  tbl_rsa_delta <- tbl_rsa_before %>% 
    select(l, r, x1_true_l, x2_true_l, x1_true_r, x2_true_r, d_euclidean_response, d_euclidean_true) %>%
    left_join(
      tbl_rsa_after %>% select(l, r, d_euclidean_response), by = c("l", "r"),
      suffix = c("_before", "_after")
    ) %>% mutate(
      d_euclidean_delta = d_euclidean_response_after - d_euclidean_response_before,
      participant_id = p_id
    ) %>% relocate(participant_id, .before = l)
  return(tbl_rsa_delta)
}


plot_distance_matrix <- function(tbl_df) {
  #' plot symmetric euclidean distance matrix
  #' 
  #' @description plot euclidean distances between all stimuli in in the
  #' experiment; stimulus id is on both axes integrating x1 and x2 into one variable
  #' @param tbl_df tbl_df containing all pairs of the stimuli
  #' 
  #' @return a ggplot heatmap
  #'
  
  ggplot(tbl_df, aes(l, r)) +
    geom_raster(aes(fill = d_euclidean_delta)) +
    theme_bw() +
    scale_fill_viridis_c(name = "Euclidean Distance Delta") + #, limits = c(0, 75)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    labs(x = "Stimulus ID 1", y = "Stimulus ID 2") +
    theme(legend.position = "omit")
}


pairwise_distances <- function(tbl_cr) {
  #' pairwise distances between responses for all participants
  #' 
  #' @description calculates distances between responses for given pair of stimuli
  #' @param tbl_cr tbl_df with by-trial continuous reproduction responses before and after category learning
  #' 
  #' @return a nested list containing a list with all distance matrices, that list
  #' reduced to a tbl_df, and the distance matrices plotted 
  #' for a few sample participants in both groups
  
  # get by-participant distance matrices
  p_id <- unique(tbl_cr$participant_id)
  tbl_groups <- tbl_cr %>% group_by(participant_id, n_categories) %>% 
    count() %>% ungroup() %>% mutate(rwn = row_number(participant_id))
  l_rsa_delta <- map(tbl_groups$participant_id, delta_representational_distance, tbl_cr = tbl_cr)
  
  # plot distance matrices for sample participants
  list_ids_control <- tbl_groups$rwn[tbl_groups$n_categories == "Similarity"]
  list_ids_experimental <- tbl_groups$rwn[tbl_groups$n_categories != "Similarity"]
  l_rsa_delta_control <- map(l_rsa_delta, list_ids_control)
  l_rs_mat_control <- l_rsa_delta[list_ids_control]
  l_rs_mat_experimental <- l_rsa_delta[list_ids_experimental]
  l_plot_d_mat_control <- map(l_rs_mat_control, plot_distance_matrix)
  l_plot_d_mat_experimental <- map(l_rs_mat_experimental, plot_distance_matrix)
  pl_m_control <- plot_arrangement(l_plot_d_mat_control[sample(1:length(l_plot_d_mat_control), 3, replace = FALSE)], 3, 1)
  pl_m_experimental <- plot_arrangement(l_plot_d_mat_experimental[sample(1:length(l_plot_d_mat_experimental), 3, replace = FALSE)], 3, 1)
  
  # reduce list of by-participant tbl_dfs into one larger tbl_df
  tbl_rsa <- reduce(l_rsa_delta, rbind) %>% 
    filter(l >= r) %>% # remove upper triangle from distance matrix
    left_join(
      tbl_cr %>% group_by(participant_id, n_categories) %>% 
        count() %>% ungroup() %>% select(-n),
      by = "participant_id"
    ) %>% relocate(n_categories, .after = participant_id)
  
  return(list(
    l_rsa_delta = l_rsa_delta, tbl_rsa = tbl_rsa, 
    pl_m_control = pl_m_control, pl_m_experimental = pl_m_experimental
  ))
}


plot_true_ds_vs_response_ds <- function(tbl_rsa) {
  #' plot true distances between stimuli against distances 
  #' between responses for these stimuli
  #' 
  #' @description groups by experimental groups and true distances and 
  #' calculates average distances of responses
  #' @param tbl_rsa tbl_df with upper triangle of similarity matrix
  #' for all participants
  #' 
  #' @return a scatterplot and a fitted linear model using the
  #' aggregated data
  #'
  
  tbl_rsa_agg <- tbl_rsa %>% 
    grouped_agg(
      c(participant_id, n_categories, d_euclidean_true), 
      c(d_euclidean_response_before, d_euclidean_response_after)
    ) %>% grouped_agg(
      c(n_categories, d_euclidean_true),
      c(mean_d_euclidean_response_before, mean_d_euclidean_response_after)
    ) %>% ungroup() %>% 
    pivot_longer(c(mean_mean_d_euclidean_response_before, mean_mean_d_euclidean_response_after))
  tbl_rsa_agg$name <- fct_inorder(tbl_rsa_agg$name)
  levels(tbl_rsa_agg$name) <- c("Before", "After")
  
  tbl_rsa_agg %>% 
    ggplot(aes(d_euclidean_true, value, group = name)) +
    geom_point(aes(color = name), shape = 1) +
    geom_smooth(method = "lm", aes(color = name)) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~ n_categories) +
    theme_bw() +
    scale_color_brewer(name = "", palette = "Set1") +
    labs(
      x = "Euclidean Distance True",
      y = "Euclidean Distance Response"
    )
}


load_predictions <- function(f_name, sim_center, is_simulation){
  #' load predictions from simulation study using some arbitrary 
  #' parameter combinations
  #' 
  #' @description loads data from simulation run for the given file name
  #' with prior and posterior means of stimuli
  #' @param f_name the name of the data for a saved simulation run to be loaded
  #' @param sim_center category centers according to ellipse or square category structure
  #' @param is_simulation is the function used for simulations or empirical data
  #' 
  #' @return a tbl_df with all stimuli and the associated predictions 
  #' from the model before and after category learning
  #'
  
  # calculate delta of pairwise distances for model predictions aka model matrix
  l_category_results <- readRDS(file = f_name)
  l_results_plots <- map(l_category_results, diagnostic_plots, sim_center = sim_center, is_simulation = is_simulation)
  tbl_design <- l_results_plots[[5]][[1]]$tbl_posterior %>% filter(timepoint == "Before Training") %>%
    select(stim_id, x1_true, x2_true)
  tmp_before <- l_results_plots[[5]][[1]]$tbl_posterior %>% filter(timepoint == "Before Training") %>%
    select(stim_id, x1_response, x2_response)
  tmp_after <- l_results_plots[[5]][[1]]$tbl_posterior %>% filter(timepoint == "After Training") %>%
    select(stim_id, x1_response, x2_response)
  
  tbl_before <- tbl_design %>% left_join(tmp_before, on = "stim_id") %>% mutate(session = "before")
  tbl_after <- tbl_design %>% left_join(tmp_after, on = "stim_id") %>% mutate(session = "after")
  tbl_both <- rbind(tbl_before, tbl_after)  %>% 
    mutate(
      participant_id = "prediction", 
      x1_true = (x1_true + 1) * 9 + 1,
      x2_true = (x2_true + 1) * 9 + 1,
      x1_response = (x1_response + 1) * 9 + 1,
      x2_response = (x2_response + 1) * 9 + 1
    )
  return(tbl_both)
}


participant_report <- function(l_cases) {
  #' by-participant summaries of experimental results
  #' 
  #' @description exclusions by reason, nr trials by task, 
  #' cr heatmaps, categorization accuracy histogram, similarity-distance line plot
  #' 
  #' @param l_cases list of tbl_dfs after exclusion preprocessing
  #' containing keeps and drops
  #' 
  #' @return a list with tbl_dfs and ggplot objects
  #'
  
  tbl_exclusions <- rbind(crossing(
    participant_id = as.character(unique(l_cases$l_guessing$drop$tbl_cr$participant_id)), 
    reason = "Guessing Categorization"
  ), crossing(
    participant_id = as.character(unique(l_cases$l_incomplete$drop$tbl_cr$participant_id)),
    reason = "Incomplete Data Set"
  ), crossing(
    participant_id = as.character(unique(l_cases$l_outliers$drop$tbl_cr$participant_id)),
    reason = "CR Outlier"
  ))
  tbl_cr <- rbind(
    l_cases$l_guessing$keep$tbl_cr,
    l_cases$l_guessing$drop$tbl_cr,
    l_cases$l_incomplete$drop$tbl_cr,
    l_cases$l_outliers$drop$tbl_cr
  )
  n_participants_unique_after_practice <- tbl_cr %>% 
    count(participant_id, n_categories) %>% count(n_categories) %>%
    mutate("Dropout Stage" = "After Practice")
  tbl_cat_sim <- rbind(
    l_cases$l_guessing$keep$tbl_cat_sim,
    l_cases$l_guessing$drop$tbl_cat,
    l_cases$l_incomplete$drop$tbl_cat,
    l_cases$l_outliers$drop$tbl_cat_sim
  )
  n_participants_unique_after_cr_session1 <- tbl_cat_sim %>% 
    count(participant_id, n_categories) %>% count(n_categories) %>%
    mutate("Dropout Stage" = "After CR S1")
  n_trials_cr <- tbl_cr %>% 
    mutate(pidxncat = interaction(n_categories, participant_id, sep = " & ")) %>%
    group_by(participant_id, n_categories, pidxncat) %>%
    count() %>% group_by(participant_id) %>%
    mutate(attempt_nr = row_number(pidxncat)) %>% ungroup()
  n_trials_cat <- tbl_cat_sim %>% 
    mutate(pidxncat = interaction(n_categories, participant_id, sep = " & ")) %>% 
    group_by(participant_id, n_categories, pidxncat) %>%
    count() %>% group_by(participant_id) %>%
    mutate(attempt_nr = row_number(pidxncat)) %>% ungroup()
  # the following thx is hand-adjusted
  n_participants_unique_after_task2 <- n_trials_cat %>% filter(n >= 394) %>% 
    count(n_categories) %>%
    mutate("Dropout Stage" = "After Task 2")
  n_participants_unique_after_cr_session2 <- n_trials_cr %>% filter(n >= 192) %>%
    count(n_categories) %>%
    mutate("Dropout Stage" = "After CR S2")
  tbl_dropouts <- rbind(
    n_participants_unique_after_practice,
    n_participants_unique_after_cr_session1,
    n_participants_unique_after_task2,
    n_participants_unique_after_cr_session2
  )
  tbl_dropouts$`Dropout Stage` <- fct_inorder(tbl_dropouts$`Dropout Stage`)
  tbl_dropouts$n_categories <- str_c(tbl_dropouts$n_categories, " Categories")
  hist_dropouts <- ggplot(tbl_dropouts, aes(`Dropout Stage`, n)) +
    geom_col(aes(fill = as.numeric(`Dropout Stage`)), show.legend = FALSE) +
    geom_label(aes(y = n - 1, label = str_c("N = ", n))) +
    facet_wrap(~ n_categories) +
    scale_fill_gradient(low = "#FF9999", high = "#339999") +
    theme_bw() +
    labs(x = "Dropout Stage", y = "Nr. Participants Remaining")
  
  hist_cr <- ggplotly(
    ggplot(n_trials_cr, aes(n)) + 
      geom_histogram(aes(fill = pidxncat), show.legend = FALSE) +
      facet_grid(~ n_categories) +
      labs(
        title = "Continuous Reproduction",
        x = "Nr. Trials",
        y = "Nr. Participants"
      ) + theme_bw() +
      theme(legend.position = "none")
  )
  
  hist_cat_sim <- ggplotly(
    ggplot(n_trials_cat, aes(n)) + 
      geom_histogram(aes(fill = pidxncat), show.legend = FALSE) +
      facet_grid(~ n_categories) +
      labs(
        title = "Categorization & Similarity",
        x = "Nr. Trials",
        y = "Nr. Participants"
      ) + theme_bw() +
      theme(legend.position = "none")
  )
  
  pl_heatmaps <- plot_2d_binned_heatmaps(
    l_deviations$tbl_checker, l_deviations$tbl_checker_avg
  )
  tbl_cat_overview <- tbl_cat_sim %>%
    filter(n_categories > 1) %>%
    grouped_agg(c(n_categories, participant_id), c(accuracy, rt)) %>%
    arrange(mean_rt)
  
  # categorization accuracy overview
  l_hists <- histograms_accuracies_rts(tbl_cat_overview)
  pl_cat_hist <- subplot(l_hists[[1]], l_hists[[2]], nrows = 1)
  
  tbl_sim <- tbl_cat_sim %>%
    filter(n_categories == 1) %>%
    mutate(
      x1_prev_true = lag(x1_true, 1),
      x2_prev_true = lag(x2_true, 1),
      distance_euclidian = sqrt((x1_true - x1_prev_true) ^ 2 + (x2_true - x2_prev_true) ^
                                  2)
    ) %>% filter(trial_id != 0) %>% replace_na(list(distance_euclidian = 0))
  n_bins_distance <- 9
  bins_distance <-
    c(seq(-1, ifelse(is.infinite(max(tbl_sim$distance_euclidian)), 2, max(tbl_sim$distance_euclidian)), length.out = n_bins_distance), Inf)
  tbl_sim$distance_binned <-
    cut(tbl_sim$distance_euclidian, bins_distance, labels = FALSE)
  tbl_sim$distance_binned %>% unique()
  pl_sim_line <- tbl_sim %>% group_by(participant_id, n_categories, distance_binned) %>%
    summarize(response_mn = mean(response), n = n()) %>%
    ggplot(aes(distance_binned, response_mn, group = as.numeric(participant_id))) +
    geom_point(aes(color = as.numeric(participant_id), size = n)) +
    geom_line(aes(color = as.numeric(participant_id)), show.legend = FALSE) +
    scale_color_viridis_c(guide = "none") +
    scale_size_area(max_size = 4, name = "Nr. Trials") +
    theme_bw() +
    labs(x = "Euclidean Distance (Binned)",
         y = "Average Similarity (Range: 1 - 4)")
  
  l_screening <- list(
    tbl_exclusions = tbl_exclusions,
    n_trials_cr = DT::datatable(n_trials_cr),
    n_trials_cat = DT::datatable(n_trials_cat),
    hist_dropouts = hist_dropouts,
    hist_cr = hist_cr,
    hist_cat_sim = hist_cat_sim,
    pl_heatmaps = pl_heatmaps,
    pl_cat_hist = pl_cat_hist,
    pl_sim_line = pl_sim_line
  )
  
}


d2_rep_center_square <- function(tbl_participant, nb_participant) {
  #' calculate distance to representational category center
  #' 
  #' @description extracts nb centers and calculates distances to the
  #' representational center of the correct answer
  #' 
  #' @param tbl_participant tbl df with continuous reproduction data for one 
  #' participant
  #' @param nb_participant fitted naive Bayes model for the same participant
  #' 
  #' @return a tbl df with the distances per session and trial
  #'  
  
  p_id <- tbl_participant$participant_id
  session <- tbl_participant$session
  trial_id <- tbl_participant$trial_id
  tbl_params <- as_tibble(data.frame(map(
    nb_participant[[1]][["tables"]], ~ t(matrix(unlist(.x), nrow = 2, ncol = 4))
  ) %>% reduce(cbind)))
  names(tbl_params) <- c("mean_x1", "sd_x1", "mean_x2", "sd_x2")
  tbl_participant <- tbl_participant[, c("category", "x1_response", "x2_response")]
  
  distance_to_category_center <- function(category, x1_response, x2_response, tbl_params) {
    sqrt(
      (x1_response - tbl_params[category, "mean_x1"])^2 +
        (x2_response - tbl_params[category, "mean_x2"])^2
    )
  }
  
  v <- distance_to_category_center(tbl_participant$category, tbl_participant$x1_response, tbl_participant$x2_response, tbl_params)
  tbl_out <- tibble(participant_id = p_id, session = session, trial_id = trial_id, d_rep = as_vector(v))
  colnames(tbl_out)[4] <- "d_rep_center"
  return(tbl_out)
  
}


by_participant_nb <- function(tbl_cat, subset_participants) {
  #' fit by-participant naive Bayes
  #' 
  #' @description fits naive Bayes by participant and predicts on a 
  #' fine grid of the whole 2D stimulus space
  #' 
  #' @param tbl_cat tbl df with by trial category learning responses
  #' @param subset_participants of participants to fit the model to
  #' 
  #' @return a list with the fitted model and predictions
  #'    
  l_nb <- map(
    subset_participants, fit_predict_nb,
    tbl = tbl_cat %>% filter(n_categories == 4 & trial_id >= n_start_exclude)
  )
  names(l_nb) <- subset_participants
  return(l_nb)
}


representational_precision <- function(nb_participant) {
  #' calculate average sd of prototype representations from naive Bayes
  #' 
  #' @description extracts sds from representations and calculates their mean
  #' 
  #' @param nb_participant a fitted naive Bayes model for one participant
  #' 
  #' @return the average representational sd
  #'    
  tbl_params <- as_tibble(data.frame(map(
    nb_participant[[1]][["tables"]], 
    ~ t(matrix(unlist(.x), nrow = 2, ncol = 4))
  ) %>% reduce(cbind)))
  names(tbl_params) <- c("mean_x1", "sd_x1", "mean_x2", "sd_x2")
  mean(colMeans(tbl_params %>% select(starts_with("sd"))))
}


combine_precision_and_movements <- function(l_nb, subset_participants) {
  #' combine representational precision and representational movements
  #' 
  #' @description combines representational precision with movements
  #' towards true category centers and towards representational category
  #' centers in one tbl df
  #' 
  #' @param l_nb list with all by-participant fitted nb models
  #' @param subset_participants of participants to fit the model to
  
  #' @return a tbl df with aggregated info for all participants
  #' 
  v_precision_representation <- map_dbl(l_nb, representational_precision)
  tbl_precision_representation <- tibble(v_precision_representation)
  tbl_precision_representation$participant_id <- subset_participants
  tbl_precision <- tbl_precision_representation %>%
    left_join(
      tbl_movement_gt %>% ungroup() %>% 
        select(participant_id, movement), by = "participant_id"
    ) %>%
    rename(movement_gt = movement) %>%
    left_join(tbl_movement_representation %>% ungroup() %>% select(participant_id, movement), by = "participant_id") %>%
    rename(movement_representation = movement)
  return(tbl_precision)
}


separate_cat_and_sim <- function(tbl_cat_sim) {
  #' separate experimental from control group
  #' 
  #' @description separates category learning group (experimental) from
  #' similarity rating group (control); additionally adds euclidean
  #' distance to previously presented stimulus in similarity group
  #' 
  #' @param tbl_cat_sim the tbl df with by-trial and by-participant data
  #' from both groups
  #' 
  #' @return list with three tbl dfs containing data from:
  #' 1. both groups, 2. category learning group, 3. similarity rating group
  #'    
  # prepare tbl_cat
  # occasionally, the same trial was saved twice
  tbl_cat_sim <- tbl_cat_sim %>%
    group_by(participant_id, trial_id) %>%
    mutate(rwn = row_number(participant_id)) %>%
    filter(rwn == 1) %>% select(-rwn)
  
  tbl_cat_sim <- add_binned_trial_id(tbl_cat_sim, 20, 0)
  tbl_cat <- tbl_cat_sim %>% 
    filter(n_categories %in% c(2, 4))
  
  # prepare tbl_sim
  tbl_sim <- tbl_cat_sim %>%
    filter(n_categories == 1) %>%
    mutate(
      x1_prev_true = lag(x1_true, 1),
      x2_prev_true = lag(x2_true, 1),
      distance_euclidian = sqrt((x1_true - x1_prev_true) ^ 2 + (x2_true - x2_prev_true) ^
                                  2)
    ) %>% filter(trial_id != 0) %>% replace_na(list(distance_euclidian = 0))
  n_bins_distance <- 9
  bins_distance <-
    c(seq(-1, max(tbl_sim$distance_euclidian), length.out = n_bins_distance), Inf)
  tbl_sim$distance_binned <-
    cut(tbl_sim$distance_euclidian, bins_distance, labels = FALSE)
  
  return(list(tbl_cat = tbl_cat, tbl_sim = tbl_sim, tbl_cat_sim = tbl_cat_sim))
}


after_vs_before <- function(tbl_cr) {
  #' subtract distances to category means after from those before
  #' 
  #' @description calculates for each stimulus and participant the movement
  #' towards the respective category center
  #' 
  #' @param tbl_cr the tbl df with by-trial and by-participant cr responses
  #' 
  #' @return tbl df with only half of the rows as the input provides
  
  tbl_cr %>% group_by(participant_id, stim_id) %>%
    arrange(participant_id, stim_id, session) %>%
    mutate(
      d_closest_sqrt = sqrt(d_closest),
      d_closest_before_abs = lag(d_closest),
      d_closest_before_sqrt = lag(d_closest_sqrt),
      d_move_sqrt = d_closest_before_sqrt - d_closest_sqrt,
      d_move_abs = d_closest_before_abs - d_closest
    ) %>%
    ungroup() %>%
    mutate(
      n_categories = factor(n_categories, labels = c("Similarity", "4 Categories"))
    ) %>%
    dplyr::filter(!is.na(d_closest_before_abs)) %>%
    group_by(participant_id) %>%
    mutate(
      d_move_mn = mean(d_move_abs)
    ) %>% ungroup()
}



extract_movement_outliers <- function(tbl_cr_moves, n_sds, measurement) {
  #' extract participants with hi/lo tendency to move responses to
  #' category centers over time (i.e., before vs. after)
  #' 
  #' @description extracts those participants with an average move above/below
  #' n_sds of the mean
  #' uses measure of raw movement or movement in sqrt space
  #' 
  #' @param tbl_cr_moves the tbl df with by-item and by-participant moves
  #' towards the category centers
  #' @param n_sds number of sds to use as a cut off
  #' @param measurement either "Not Transformed" or "Square Root"
  #' 
  #' @return list with two tbl dfs
  #' (a) outliers and (b) average moves as labels for plotting
  
  tbl_outliers <- tbl_cr_moves %>%
    mutate(
      flag_hi = d_move_mn > mean(d_move_mn) + n_sds * sd(d_move_mn),
      flag_lo = d_move_mn < mean(d_move_mn) - n_sds * sd(d_move_mn),
      flag_outlier = factor(flag_hi, labels = c("Lo", "Hi"))
    ) %>%
    ungroup() %>%
    arrange(desc(d_move_mn)) %>%
    filter(flag_hi | flag_lo) %>%
    pivot_longer(c(d_move_sqrt, d_move_abs)) %>% 
    mutate(name = factor(name, labels = c("Not Transformed", "Square Root"))) %>%
    mutate(participant_id = fct_inorder(factor(str_c(substr(participant_id, 1, 6), ", ", n_categories))))
  
  tbl_labels <- tbl_outliers %>% filter(name == measurement) %>%
    group_by(participant_id, flag_outlier) %>%
    summarize(avg_move = mean(value)) %>%
    ungroup()
  
  return(list(tbl_outliers = tbl_outliers, tbl_labels = tbl_labels))
}


combine_data_with_posterior_outliers <- function(tbl_mix, tbl_cr_moves, tbl_draws, n_outliers) {
  #' combine posterior predictions and empirical data for n_outliers
  #' 
  #' @description half of the outliers are from the upper end of the distribution
  #' the other half of the outliers are taken from the lower end
  #' 
  #' @param tbl_mix mean posterior probabilities of using a categorical representation
  #' @param tbl_cr_moves tbl df with by participant and by trial moves before - after task 2
  #' @param tbl_draws tbl df with posterior samples
  #' @param n_outliers number of outliers to extract
  #' 
  #' @return list with two tbl dfs
  #' (a) with empirical data and (b) with posterior predictions
  #'   
  tbl_outlier_prob <- tbl_mix %>% head(round(n_outliers / 2)) %>% mutate(outlier = "Hi") %>%
    rbind(tbl_mix %>% tail(round(n_outliers / 2)) %>% mutate(outlier = "Lo")) %>%
    select(participant_id_num, mean, outlier)
  tbl_empirical <- tbl_cr_moves %>% 
    left_join(tbl_participants_lookup[, c("participant_id", "participant_id_num")], by = "participant_id") %>%
    inner_join(tbl_outlier_prob, by = "participant_id_num") %>%
    mutate(
      participant_id = str_c(substr(as.character(participant_id), 1, 6), ", ", n_categories)
    ) %>% arrange(desc(mean)) %>%
    mutate(participant_id = fct_inorder(participant_id))
  tbl_post_preds <- tbl_draws %>%
    select(starts_with("posterior_prediction"))
  tbl_post_preds <- tbl_post_preds %>%
    pivot_longer(cols = colnames(tbl_post_preds)) %>%
    mutate(participant_id_num = as.numeric(str_match(name, "[0-9]+")[,1])) %>%
    left_join(tbl_participants_lookup, by = "participant_id_num") %>%
    inner_join(tbl_outlier_prob, by = "participant_id_num") %>%
    mutate(
      participant_id = str_c(substr(as.character(participant_id), 1, 6), ", ", n_categories)
    ) %>% arrange(desc(mean)) %>%
    mutate(participant_id = fct_inorder(participant_id))
  
  return(list(tbl_empirical = tbl_empirical, tbl_post_preds = tbl_post_preds))
}


fix_data_types_simult <- function(tbl_simult) {
  #' 
  #' @description assign strings to factor levels and sort stimulus ids
  #' 
  #' @param tbl_simult tbl df with simultaneous comparison data
  #' 
  #' @return same tbl df with manipulated columns and added columns
  #' 
  tbl_simult$comparison_pool <- factor(
    tbl_simult$comparison_pool, 
    levels = c("same", "side", "cross"), ordered = TRUE
  )
  tbl_simult$comparison_pool_binary <- factor(
    tbl_simult$comparison_pool == "same", labels = c("Different", "Same")
  )
  levels(tbl_simult$session) <- c("Before Training", "After Training")
  levels(tbl_simult$n_categories) <- c("Similarity", "4 Categories")
  tbl_simult$stim_id_lo <- pmap_dbl(tbl_simult[, c("stim_id_l", "stim_id_r")], ~ min(.x, .y))
  tbl_simult$stim_id_hi <- pmap_dbl(tbl_simult[, c("stim_id_l", "stim_id_r")], ~ max(.x, .y))
  
  return(tbl_simult)
}


delta_simultaneous <- function(tbl_simult) {
  #' 
  #' @description compare deltas after - before for simultaneous task
  #' 
  #' @param tbl_simult tbl df with simultaneous comparison data
  #' 
  #' @return joined tbl df with added columns move_response and d_euclidean_cut
  #'
  # left join on t2 such that dropouts do not remain in tbl
  tbl_simult_move <- tbl_simult %>%
    filter(session == "After Training") %>%
    select(
      participant_id, n_categories, comparison_pool, comparison_pool_binary,
      stim_id_lo, stim_id_hi, d_euclidean, d_euclidean_cut, response, rt
    ) %>%
    left_join(
      tbl_simult %>% 
        filter(session == "Before Training") %>%
        select(
          participant_id, stim_id_lo, stim_id_hi, response, rt
        ),
      by = c("participant_id", "stim_id_lo", "stim_id_hi"),
      suffix = c("_aft", "_bef")
    ) %>%
    mutate(move_response = response_aft - response_bef)
  return(tbl_simult_move)
}
