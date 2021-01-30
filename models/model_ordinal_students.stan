functions {
  // Bürkner & Charpentier (2018, https://psyarxiv.com/9qkhj/)
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
}
data {
  int<lower = 1> J; // n of students
  int<lower = 1> K; // n of schools
  int<lower = 2> M; // n of response categories
  int<lower = 1> N; // n of observations
  int<lower = 1, upper = J> jj[N]; // index for students
  int<lower = 1, upper = K> kk[N]; // index for schools
  int<lower = 0, upper = 4> x_time[N];
  int<lower = 1, upper = M> y[N];
  int<lower = 0> N_mis;
  int<lower = 0> N_obs;
  int<lower = 1, upper = N> ii_mis[N_mis];
  int<lower = 1, upper = N> ii_obs[N_obs];
  // missing data imputation
  int<lower = 0> J_mis;
  int<lower = 0> J_obs;
  int<lower = 1, upper = J> jj_mis[J_mis];
  int<lower = 1, upper = J> jj_obs[J_obs];
  int<lower = -1, upper = 1> sep_obs[J];
  vector[J_mis] sep_alpha;
  vector[J_mis] sep_beta;
}
parameters {
  ordered[M-1] c;
  real b_time;
  simplex[4] z_time;
  vector[5] b_sep;
  matrix[2, J] b_j_z;
  vector<lower = 0>[2] sigma_j;
  cholesky_factor_corr[2] Lcorr_j;
  vector[K] b_k_z;
  real<lower = 0> sigma_k;
  vector<lower = 0, upper = 1>[J_mis] sep_imp;
}
transformed parameters {
  vector[N] eta;

  // varying effects, non-centred Cholesky parameterization
  matrix[J, 2] b_j = (diag_pre_multiply(sigma_j, Lcorr_j) * b_j_z)';
  vector[J] b_j_1 = b_j[, 1];
  vector[J] b_j_2 = b_j[, 2];
  vector[K] b_k_1 = b_k_z * sigma_k;

  // merge imputed and observed predictor values
  vector<lower = 0, upper = 1>[J] x_sep;
  x_sep[jj_mis] = sep_imp;
  x_sep[jj_obs] = to_vector(sep_obs)[jj_obs];

  for (n in 1:N)
    eta[n] = b_j_1[jj[n]] + b_k_1[kk[n]] + (b_time + b_j_2[jj[n]]) * mo(z_time, x_time[n]);
}
model {
  // priors
  b_time ~ student_t(3, 0, 1);
  z_time ~ dirichlet(rep_vector(1, 4));
  b_sep ~ student_t(3, 0, 1);
  to_vector(b_j_z) ~ normal(0, 1);
  sigma_j[1] ~ student_t(3, 0, 3);
  sigma_j[2] ~ student_t(3, 0, 1);
  Lcorr_j ~ lkj_corr_cholesky(4);
  b_k_z ~ normal(0, 1);
  sigma_k ~ student_t(3, 0, 1);

  // impute missing data
  sep_imp ~ beta(sep_alpha, sep_beta);

  // likelihood for observed data
  for (n in ii_obs)
    target += ordered_logistic_lpmf(y[n] | eta[n] + b_sep[x_time[n] + 1] * x_sep[jj[n]], c);

  // likelihood for missing data
  for (n in ii_mis)
    target += log_mix(
      x_sep[jj[n]],
      ordered_logistic_lpmf(y[n] | eta[n] + b_sep[x_time[n] + 1], c),
      ordered_logistic_lpmf(y[n] | eta[n], c)
    );
}
generated quantities {
  matrix[2, 2] Rho_j = multiply_lower_tri_self_transpose(Lcorr_j);
  matrix[5, M] p[2];
  for (m in 1:M) {
    for (x in 1:5) {
      for (s in 1:2) {
        p[s, x, m] = exp(ordered_logistic_lpmf(m | b_time * mo(z_time, x - 1) + b_sep[x] * (s - 1), c));
      }
    }
  }
}
