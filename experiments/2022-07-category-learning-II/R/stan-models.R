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

stan_move_mixture <- function() {
  
  write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] d_moved;
  array[n_data] int subj;
}


parameters {
  //vector<lower=0>[n_subj] mg_mn;
  //vector<lower=0>[n_subj] mg_sd;
  real<lower=0> mg_mn;
  real<lower=0> mg_sd;
  vector<lower=0>[n_subj] sigma_subject;
  array[n_subj] simplex[2] theta;
}


transformed parameters {
  //vector<lower=0>[n_subj] shape;
  //vector<lower=0>[n_subj] rate;
  real<lower=0> shape;
  real<lower=0> rate;

  for (s in 1:n_subj) {
    //rate[s] = mg_mn[s] / pow(mg_sd[s], 2);
    //shape[s] = pow(mg_mn[s], 2) / pow(mg_sd[s], 2);
    rate = mg_mn / pow(mg_sd, 2);
    shape = pow(mg_mn, 2) / pow(mg_sd, 2);
  }
}


model {

  vector[2] lp;

  for (n in 1:n_data) {
    lp[1] = log(theta[subj[n]][1]) + normal_lpdf(d_moved[n] | 0, sigma_subject[subj[n]]);
    if (d_moved[n] > 0) {
      lp[2] = log(theta[subj[n]][2]) + gamma_lpdf(d_moved[n] | shape, rate);
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
      //mg_mn[s] ~ gamma(2.5, .1);
      //mg_sd[s] ~ gamma(1, 1);
  }
  
}


generated quantities {
  vector[n_data] log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred[n] = log(d_moved[n]);
  }
}

")
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
  real log_lik_pred;
  vector[2] lp_pred;

  for (n in 1:n_data) {
    lp_pred[1] = log(1 - theta[subj[n]]) + normal_lpdf(d_moved[n] | 0, sigma_subject[subj[n]]);
    if (d_moved[n] > 0) {
      lp_pred[2] = log(theta[subj[n]]) + gamma_lpdf(d_moved[n] | shape, rate);
      log_lik_pred += log_sum_exp(lp_pred); //lp[1]; //
    }
    else {
      log_lik_pred += lp_pred[1];
    }
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
  real<lower=0> mu_delta;
  vector<lower=0>[n_groups] sigma;
  vector<lower=0>[n_subj] sigma_subject;
  vector[n_subj] b;

}


transformed parameters {
  vector[n_groups] mu;
  mu[1] = 0.0;
  mu[2] = mu[1] + mu_delta;

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
  mu_delta ~ cauchy(0, 1);

}


generated quantities {
  real log_lik_pred;

  for (n in 1:n_data) {
    log_lik_pred += normal_lpdf(d_moved[n] | b[subj[n]], sigma_subject[subj[n]]);
  }
}

")
}
