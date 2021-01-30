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
  int<lower = 1, upper = 3> x_sep[K];
  int<lower = 0, upper = 4> x_time[N];
  int<lower = 1, upper = M> y[N];
}
parameters {
  ordered[M-1] c;
  vector[2] b_sep_free;
  vector[3] b_sep_time;
  simplex[4] z_time;
  matrix[2, J] b_j_z;
  vector<lower = 0>[2] sigma_j;
  cholesky_factor_corr[2] Lcorr_j;
  matrix[2, K] b_k_z;
  vector<lower = 0>[2] sigma_k;
  cholesky_factor_corr[2] Lcorr_k;
}
transformed parameters {
  // constrain parameter
  vector[3] b_sep = append_row(rep_vector(0, 1), b_sep_free);

  // varying effects, non-centred Cholesky parameterization
  matrix[J, 2] b_j = (diag_pre_multiply(sigma_j, Lcorr_j) * b_j_z)';
  vector[J] b_j_1 = b_j[, 1];
  vector[J] b_j_2 = b_j[, 2];
  matrix[K, 2] b_k = (diag_pre_multiply(sigma_k, Lcorr_k) * b_k_z)';
  vector[K] b_k_1 = b_k[, 1] + b_sep[x_sep];
  vector[K] b_k_2 = b_k[, 2] + b_sep_time[x_sep];
}
model {
  // priors
  b_sep_free ~ student_t(3, 0, 1);
  b_sep_time ~ student_t(3, 0, 1);
  z_time ~ dirichlet(rep_vector(1, 4));
  to_vector(b_j_z) ~ normal(0, 1);
  sigma_j[1] ~ normal(1.57, 0.5);
  sigma_j[2] ~ normal(0.58, 0.5);
  Lcorr_j ~ lkj_corr_cholesky(4);
  to_vector(b_k_z) ~ normal(0, 1);
  sigma_k[1] ~ student_t(3, 0, 1);
  sigma_k[2] ~ student_t(3, 0, 1);
  Lcorr_k ~ lkj_corr_cholesky(4);

  // likelihood for observed data
  for (n in 1:N)
    target += ordered_logistic_lpmf(y[n] | b_j_1[jj[n]] + b_k_1[kk[n]] + (b_j_2[jj[n]] + b_k_2[kk[n]]) * mo(z_time, x_time[n]), c);
}
generated quantities {
  matrix[2, 2] Rho_j = multiply_lower_tri_self_transpose(Lcorr_j);
  matrix[2, 2] Rho_k = multiply_lower_tri_self_transpose(Lcorr_k);
  matrix[5, M] p[3];
  for (m in 1:M) {
    for (x in 1:5) {
      for (s in 1:3) {
        p[s, x, m] = exp(ordered_logistic_lpmf(m | b_sep[s] + (b_sep_time[s]) * mo(z_time, x - 1), c));
      }
    }
  }
}
