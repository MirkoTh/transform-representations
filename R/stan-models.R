stan_cat <- function() {

  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int n_trials;
  array[n_data] int n_correct;
  array[n_data] int subj;
  matrix[n_data, 2] x; // ic, trial
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  matrix[n_subj, 2] b;
  vector[2] mu;
  vector <lower=0>[2] sigma_subject;
}

transformed parameters {
  array[2] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = inv_logit(b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2]);
  }
}

model {
  for (n in 1:n_data) {
    n_correct[n] ~ binomial(n_trials[n], theta[n]);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }

  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
}

")
  return(stan_logistic)
}

stan_cat_difficulty <- function() {
  
  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int n_trials;
  array[n_data] int n_correct;
  array[n_data] int subj;
  matrix[n_data, 5] x; // ic, trial, cat12, cat13, cat14
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  matrix[n_subj, 5] b;
  vector[5] mu;
  vector <lower=0>[5] sigma_subject;
}

transformed parameters {
  array[5] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  mu_tf[3] = scale_cat * mu[3];
  mu_tf[4] = scale_cat * mu[4];
  mu_tf[5] = scale_cat * mu[5];
  
  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = inv_logit(
      b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + 
      b[subj[n], 3] * x[n, 3] + b[subj[n], 4] * x[n, 4] + b[subj[n], 5] * x[n, 5]
    );
  }
}

model {
  for (n in 1:n_data) {
    n_correct[n] ~ binomial(n_trials[n], theta[n]);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
    b[s, 3] ~ normal(mu_tf[3], sigma_subject[3]);
    b[s, 4] ~ normal(mu_tf[4], sigma_subject[4]);
    b[s, 5] ~ normal(mu_tf[5], sigma_subject[5]);
  }

  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
  mu[3] ~ student_t(1, 0, 1);
  mu[4] ~ student_t(1, 0, 1);
  mu[5] ~ student_t(1, 0, 1);
}

")
  return(stan_logistic)
}


stan_cat_by_category <- function() {
  
  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int n_trials;
  array[n_data] int n_correct;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, trial, category, ia
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  matrix[n_subj, 2] b;
  vector[4] mu;
  vector <lower=0>[2] sigma_subject;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  mu_tf[3] = scale_cat * mu[3];
  mu_tf[4] = scale_cat * mu[4];
  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = inv_logit(b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4]);
  }
}

model {
  for (n in 1:n_data) {
    n_correct[n] ~ binomial(n_trials[n], theta[n]);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }

  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
  mu[3] ~ student_t(1, 0, 1);
  mu[4] ~ student_t(1, 0, 1);
}

")
  return(stan_logistic)
}

stan_sim <- function() {
  
  stan_normal_sim <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] response;
  array[n_data] int subj;
  matrix[n_data, 2] x; // ic, euclidean distance
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  matrix[n_subj, 2] b;
  vector[2] mu;
  vector <lower=0>[2] sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[2] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2];
  }
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }
  
  sigma ~ uniform(0.001, 10);
  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
}

")
  return(stan_normal_sim)
}


stan_cr_rs <- function() {
  
  stan_normal_cr_rs <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] d_closest;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, session, ncat, session x ncat
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
}

parameters {
  matrix[n_subj, 2] b;
  vector[4] mu;
  vector <lower=0>[2] sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2] * scale_cat;
  mu_tf[3] = mu[3] * scale_cat;
  mu_tf[4] = mu[4] * scale_cat;
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4];
  }
}

model {
  for (n in 1:n_data) {
    d_closest[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }
  
  sigma ~ cauchy(0, 1);
  sigma_subject[1] ~ cauchy(0, 1);
  sigma_subject[2] ~ cauchy(0, 1);
  mu[1] ~ cauchy(0, 1);
  mu[2] ~ cauchy(0, 1);
  mu[3] ~ cauchy(0, 1);
  mu[4] ~ cauchy(0, 1);
}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(d_closest[n] | mn[n], sigma);
  }
}

")
  return(stan_normal_cr_rs)
}


stan_cr_ri <- function() {
  
  stan_normal_cr_ri <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] d_closest;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, session, ncat, session x ncat
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
}

parameters {
  matrix[n_subj, 1] b;
  vector[4] mu;
  real <lower=0> sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2] * scale_cat;
  mu_tf[3] = mu[3] * scale_cat;
  mu_tf[4] = mu[4] * scale_cat;
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + mu_tf[2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4];
  }
}

model {
  for (n in 1:n_data) {
    d_closest[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject);
  }
  sigma ~ cauchy(0, 1);
  sigma_subject ~ cauchy(0, 1);
  mu[1] ~ cauchy(0, 1);
  mu[2] ~ cauchy(0, 1);
  mu[3] ~ cauchy(0, 1);
  mu[4] ~ cauchy(0, 1);
}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(d_closest[n] | mn[n], sigma);
  }
}

")
  return(stan_normal_cr_ri)
}

stan_move_mixture_groups <- function() {
  
  write_stan_file("
data {
  int n_data;
  int n_subj;
  int n_groups;
  vector[n_data] d_moved;
  array[n_data] int subj;
  array[n_subj] int group;
}


parameters {
  real<lower=0> mg_mn;
  real<lower=0> mg_sd;
  vector<lower=0>[n_subj] sigma_subject;
  vector[n_subj] b_theta;
  vector<lower=0>[n_groups] sigma_theta;
  vector[n_groups] mu_theta;
}


transformed parameters {
  real<lower=0> shape;
  real<lower=0> rate;
  vector<lower=0,upper=1>[n_subj] theta;

  for (s in 1:n_subj) {
    rate = mg_mn / pow(mg_sd, 2);
    shape = pow(mg_mn, 2) / pow(mg_sd, 2);
    theta[s] = inv_logit(b_theta[s]);
  }
}


model {

  vector[2] lp;

  for (n in 1:n_data) {
    lp[1] = log(1 - theta[subj[n]]) + normal_lpdf(d_moved[n] | 0, sigma_subject[subj[n]]);
    if (d_moved[n] > 0) {
      lp[2] = log(theta[subj[n]]) + gamma_lpdf(d_moved[n] | shape, rate);
      target += log_sum_exp(lp); //lp[1]; //
    }
      
    else {
      target += lp[1];
      }
  }

  mg_mn ~ gamma(2.5, .1);
  mg_sd ~ gamma(1, 1);
  for (s in 1:n_subj) {
      sigma_subject[s] ~ normal(0, 30);
      b_theta[s] ~ normal(mu_theta[group[s]], sigma_theta[group[s]]);
  }
  for (g in 1:n_groups) {
      mu_theta[g] ~ cauchy(0, 1);
      sigma_theta[g] ~ cauchy(0, 1);
  }
  
}


generated quantities {
  vector[n_data] log_lik_pred;
  vector[2] lp_pred;
  vector[n_subj] posterior_prediction;
  vector[n_subj] posterior_gaussian;
  vector[n_subj] posterior_gamma;

  for (n in 1:n_data) {
    lp_pred[1] = log(1 - theta[subj[n]]) + normal_lpdf(d_moved[n] | 0, sigma_subject[subj[n]]);
    if (d_moved[n] > 0) {
      lp_pred[2] = log(theta[subj[n]]) + gamma_lpdf(d_moved[n] | shape, rate);
      log_lik_pred[n] = log_sum_exp(lp_pred); //lp[1]; //
    }
    else {
      log_lik_pred[n] = lp_pred[1];
    }
  }
  
  for (s in 1:n_subj) {
    posterior_gaussian[s] = normal_rng(0, sigma_subject[s]);
    posterior_gamma[s] = gamma_rng(shape, rate);
    posterior_prediction[s] = (1 - theta[s]) * posterior_gaussian[s] +
      theta[s] * posterior_gamma[s];
  }
}

")
}



stan_move_shift_normal <- function() {
  
  write_stan_file("
data {
  int n_data;
  int n_subj;
  int n_groups;
  vector[n_data] d_moved;
  array[n_data] int subj;
  array[n_subj] int group;
}


parameters {
  vector[n_groups] mu; //<lower=0>
  vector<lower=0>[n_groups] sigma;
  vector<lower=0>[n_subj] sigma_subject;
  vector[n_subj] b;

}

model {

  for (n in 1:n_data) {
      d_moved[n] ~ normal(b[subj[n]], sigma_subject[subj[n]]);
  }

  for (s in 1:n_subj) {
      sigma_subject[s] ~ normal(0, 30);
      b[s] ~ normal(mu[group[s]], sigma[group[s]]);
  }
  
  sigma[1] ~ cauchy(0, 1);
  sigma[2] ~ cauchy(0, 1);
  mu[1] ~ cauchy(0, 1);
  mu[2] ~ cauchy(0, 1);

}


generated quantities {
  vector[n_data] log_lik_pred;
  vector[n_subj] posterior_prediction;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(d_moved[n] | b[subj[n]], sigma_subject[subj[n]]);
  }
  
  for (s in 1:n_subj) {
    posterior_prediction[s] = normal_rng(b[subj[s]], sigma_subject[s]);
  }
}

")
}


stan_move_exgaussian <- function() {
  
  write_stan_file("
data {
  int n_data;
  int n_subj;
  int n_groups;
  vector[n_data] d_moved;
  array[n_data] int subj;
  array[n_subj] int group;
}


parameters {
  vector<lower=0>[n_subj] sigma_subject;
  vector<lower=0>[n_subj] tau_subject;
  //vector<lower=0>[n_groups] sigma_tau;
  real<lower=0> sigma_tau;
  vector<lower=0>[n_groups] mu_tau;
  //real<lower=0> mu_tau;
}


model {

  for (n in 1:n_data) {
    d_moved[n] ~ exp_mod_normal(0, sigma_subject[subj[n]], tau_subject[subj[n]]);
  }

  for (s in 1:n_subj) {
      // if sigma_subject is drawn from normal prior with too high mean i.e., 20 or too large sd i.e., 5, model does not converge
      sigma_subject[s] ~ normal(1, .05);
      tau_subject[s] ~ normal(mu_tau[group[s]], sigma_tau);
      //tau_subject[s] ~ normal(mu_tau, sigma_tau);
  }
  sigma_tau ~ normal(0, 1);
  //mu_tau ~ normal(0, 2);
  
  for (g in 1:n_groups) {
      mu_tau[g] ~ normal(0, 2);
      //sigma_tau[g] ~ normal(0, 1);
  }
}


generated quantities {
  vector[n_data] log_lik_pred;
  vector[n_subj] posterior_prediction;


  for (n in 1:n_data) {
      log_lik_pred[n] = exp_mod_normal_lpdf(d_moved[n] | 0, sigma_subject[subj[n]], tau_subject[subj[n]]);
  }
  
  for (s in 1:n_subj) {
    posterior_prediction[s] = exp_mod_normal_rng(0, sigma_subject[s], tau_subject[s]);
  }
}

")
}


stan_simult_rs <- function() {
  
  stan_normal_simult_move_rs <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] move_response;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, comp_pool, ncat, comp_pool x ncat
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
}

parameters {
  matrix[n_subj, 2] b;
  vector[4] mu;
  vector <lower=0>[2] sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2] * scale_cat;
  mu_tf[3] = mu[3] * scale_cat;
  mu_tf[4] = mu[4] * scale_cat;
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4];
  }
}

model {
  for (n in 1:n_data) {
    move_response[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }
  
  sigma ~ cauchy(0, 1);
  sigma_subject[1] ~ cauchy(0, 1);
  sigma_subject[2] ~ cauchy(0, 1);
  mu[1] ~ cauchy(0, 1);
  mu[2] ~ cauchy(0, 1);
  mu[3] ~ cauchy(0, 1);
  mu[4] ~ cauchy(0, 1);
}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(move_response[n] | mn[n], sigma);
  }
}

")
  return(stan_normal_simult_move_rs)
}


stan_simult_ri <- function() {
  
  stan_normal_simult_move_ri <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] move_response;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, comp_pool, ncat, comp_pool x ncat
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
}

parameters {
  matrix[n_subj, 1] b;
  vector[4] mu;
  real<lower=0>sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2] * scale_cat;
  mu_tf[3] = mu[3] * scale_cat;
  mu_tf[4] = mu[4] * scale_cat;
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + mu_tf[2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4];
  }
}

model {
  for (n in 1:n_data) {
    move_response[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject);
  }
  
  sigma ~ cauchy(0, 1);
  sigma_subject ~ cauchy(0, 1);
  mu[1] ~ cauchy(0, 1);
  mu[2] ~ cauchy(0, 1);
  mu[3] ~ cauchy(0, 1);
  mu[4] ~ cauchy(0, 1);
}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(move_response[n] | mn[n], sigma);
  }
}

")
  return(stan_normal_simult_move_ri)
}


stan_cr_2d <- function() {
  
  stan_mv <- write_stan_file("
data {
  int n_data;
  int n_subj;
  matrix[n_data, 2] y;
  array[n_data] int subj;
  matrix[n_data, 2] x; // group, timepoint
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
  vector[2] v_mn = [0, 0]';
}

parameters {
  cholesky_factor_corr[2] L;
  vector<lower=0>[2] L_std;
}

model {
  matrix[2, 2] L_Sigma;
  
  
  L ~ lkj_corr_cholesky(1);
  L_std ~ normal(10, 3);
  
  
  for (n in 1:n_data) {
    target += multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std, L));
  }
}

generated quantities {
  corr_matrix[2] Sigma;
  array[n_data] real log_lik_pred;

  Sigma = multiply_lower_tri_self_transpose(L);

  for (n in 1:n_data) {
    log_lik_pred[n] = multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std, L));
  }
}

")
  return(stan_mv)
}


stan_cr_2d_nested <- function() {
  
  stan_mv <- write_stan_file("
data {
  int n_data;
  int n_subj;
  matrix[n_data, 2] y;
  array[n_data] int subj;
  matrix[n_data, 2] x; // group, timepoint
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
  vector[2] v_mn = [0, 0]';
}

parameters {
  array[n_subj] cholesky_factor_corr[2] L;
  vector<lower=0>[2] mu0;
  array[n_subj] vector<lower=0>[2] b0;
  vector<lower=0>[2] sdsubj;
  vector[2] muGroup; //<lower=0>
  vector[2] muTime; // <lower=0>
  vector[2] muIA; // <lower=0>
}

transformed parameters {
  array[n_data] vector<lower=0>[2] L_std;

  for (n in 1:n_data) {
    L_std[n] = b0[subj[n]] + muGroup * x[n, 1] + muTime * x[n, 2] + muIA * x[n, 1] * x[n, 2];
  }
}

model {

  for (s in 1:n_subj) {
    L[s] ~ lkj_corr_cholesky(1);
    b0[s] ~ normal(mu0, sdsubj);
  }
  
  sdsubj ~ gamma(.05, .1);
  mu0 ~ normal(10, 3);
  muGroup ~ normal(0, 1);
  muTime ~ normal(0, 1);
  muIA ~ normal(0, 1);
  
  for (n in 1:n_data) {
    target += multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std[n], L[subj[n]]));
  }
}

generated quantities {
  array[n_subj] corr_matrix[2] Sigma;
  array[n_data] real log_lik_pred;

  for (s in 1:n_subj) {
    Sigma[s] = multiply_lower_tri_self_transpose(L[s]);
  }

  for (n in 1:n_data) {
    log_lik_pred[n] = multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std[n], L[subj[n]]));
  }
}

")
  return(stan_mv)
}

stan_cr_2d_nested_db <- function() {
  
  stan_mv <- write_stan_file("
data {
  int n_data;
  int n_subj;
  matrix[n_data, 2] y;
  array[n_data] int subj;
  matrix[n_data, 3] x; // group, timepoint, d_boundary_stim_z
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = .5;
  vector[2] v_mn = [0, 0]';
}

parameters {
  array[n_subj] cholesky_factor_corr[2] L;
  vector<lower=0>[2] mu0;
  array[n_subj] vector<lower=0>[2] b0;
  vector<lower=0>[2] sdsubj;
  vector[2] muGroup; //<lower=0>
  vector[2] muTime; // <lower=0>
  vector[2] muIA; // <lower=0>
  vector[2] muBoundary;
}

transformed parameters {
  array[n_data] vector<lower=0>[2] L_std;

  for (n in 1:n_data) {
    L_std[n] = b0[subj[n]] + muGroup * x[n, 1] + muTime * x[n, 2] + muIA * x[n, 1] * x[n, 2] + muBoundary * x[n, 3];
  }
}

model {

  for (s in 1:n_subj) {
    L[s] ~ lkj_corr_cholesky(1);
    b0[s] ~ normal(mu0, sdsubj);
  }
  
  sdsubj ~ gamma(.05, .1);
  mu0 ~ normal(10, 3);
  muGroup ~ normal(0, 1);
  muTime ~ normal(0, 1);
  muIA ~ normal(0, 1);
  muBoundary ~ normal(0, 1);
  
  for (n in 1:n_data) {
    target += multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std[n], L[subj[n]]));
  }
}

generated quantities {
  array[n_subj] corr_matrix[2] Sigma;
  array[n_data] real log_lik_pred;

  for (s in 1:n_subj) {
    Sigma[s] = multiply_lower_tri_self_transpose(L[s]);
  }

  for (n in 1:n_data) {
    log_lik_pred[n] = multi_normal_cholesky_lpdf(y[n] | v_mn, diag_pre_multiply(L_std[n], L[subj[n]]));
  }
}

")
  return(stan_mv)
}


stan_move_by_success <- function() {
  
  stan_normal_move_by_success <- write_stan_file("
data {
  int n_data;
  vector[n_data] response;
  matrix[n_data, 4] x; // ic, category, improvement, category x improvement
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  vector[4] mu;
  real<lower=0> sigma;
}

transformed parameters {

  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = mu[1] * x[n, 1] + mu[2] * x[n, 2] + mu[3] * x[n, 3] + mu[4] * x[n, 4];
  }
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mn[n], sigma);
  }
  
  sigma ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ normal(0, 1);
  mu[3] ~ normal(0, 1);
  mu[4] ~ normal(0, 1);
}

")
  return(stan_normal_move_by_success)
}


stan_move_by_success_square <- function() {
  
  stan_normal_move_by_success_square <- write_stan_file("
data {
  int n_data;
  vector[n_data] response;
  matrix[n_data, 2] x; // ic, improvement
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  vector[2] mu;
  real<lower=0> sigma;
}

transformed parameters {
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = mu[1] * x[n, 1] + mu[2] * x[n, 2];
  }
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mn[n], sigma);
  }
  
  sigma ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ normal(0, 1);
}

")
  return(stan_normal_move_by_success_square)
}




stan_move_e1 <- function() {
  
  write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] d_moved;
  array[n_data] int subj;
  matrix[n_data, 4] x;
}


transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  vector[4] mu; //<lower=0>
  real<lower=0> sigma;
  vector<lower=0>[2] sigma_subject;
  matrix[n_subj, 2] b;

}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cat * mu[2];
  mu_tf[3] = scale_cat * mu[3];
  mu_tf[4] = scale_cat * mu[4];
  vector[n_data] mn;
  
  for (n in 1:n_data) {
      mn[n] = b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + mu_tf[3] * x[n, 3] + mu_tf[4] * x[n, 4];
  }
}


model {

  for (n in 1:n_data) {
      d_moved[n] ~ normal(mn[n], sigma);
  }

  for (s in 1:n_subj) {
      
      b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
      b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
  }
  
  sigma ~ cauchy(0, 1);
  sigma_subject[1] ~ normal(0, 1);
  sigma_subject[2] ~ normal(0, 1);

}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = normal_lpdf(d_moved[n] | mn[n], sigma);
  }
  
}

")
}



stan_sim_city <- function() {
  
  stan_sim_city <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int subj;
  vector[n_data] distance1;
  vector[n_data] distance2;
  vector[n_data] response;
  matrix[n_data, 5] x; // ic, group, category comparison, time point, 3w-ia
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  vector [5] mu;
  vector [n_subj] b0;
  //vector [n_subj] bcc;
  //vector [n_subj] btp;
  
  real<lower=0, upper=1> sigma;
  real<lower=0, upper=1> sigma_w;
  vector <lower=0, upper=1> [1] sigma_subject; // [3] if three random effects

  real<lower=0,upper=1> w_group;
  vector <lower=0,upper=1> [n_subj] w_indiv;
  
}

transformed parameters {
  vector[n_data] mn;
  real reg;

  for (n in 1:n_data) {
    reg = b0[subj[n]] * x[n, 1] + mu[2] * x[n, 2] + mu[3] * x[n, 3] + mu[4] * x[n, 4] + mu[5] * x[n, 5];
    mn[n] = exp(-(reg * (w_indiv[subj[n]] * distance1[n] + (1 - w_indiv[subj[n]]) * distance2[n])));
  }

}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mn[n], sigma);
  }
  
  for (s in 1:n_subj) {
    w_indiv[s] ~ normal(w_group, sigma_w);
    b0[s] ~ normal(mu[1], sigma_subject[1]);
    //bcc[s] ~ normal(mu[3], sigma_subject[2]);
    //btp[s] ~ normal(mu[4], sigma_subject[3]);
    
  }
  
  sigma ~ uniform(0.001, 1);
  sigma_subject ~ uniform(0.001, 1);
  sigma_w ~ uniform(0.001, 1);
  mu[1] ~ normal(3, 1);
  mu[2] ~ normal(0, 1);
  mu[3] ~ normal(0, 1);
  mu[4] ~ normal(0, 1);
  mu[5] ~ normal(0, 1);
  
  w_group ~ beta(1, 1);

}

")
  return(stan_sim_city)
}



