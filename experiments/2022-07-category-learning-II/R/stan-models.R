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

  sigma_subject[1] ~ uniform(0.001, 100);
  sigma_subject[2] ~ uniform(0.001, 100);
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
  sigma_subject[1] ~ uniform(0.001, 100);
  sigma_subject[2] ~ uniform(0.001, 100);
  mu[1] ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
}

")
  return(stan_normal_sim)
}
