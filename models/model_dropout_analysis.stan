data {
  int<lower = 1> J;
  int<lower = 1> K;
  int<lower = 1> N;
  int<lower = 1> M;
  int<lower = 1, upper = 5> ii[N];
  int<lower = 1, upper = J> jj[N];
  int<lower = 1, upper = K> kk[N];
  int<lower = 8, upper = 12> year[N];
  int<lower = 0, upper = 1> y[N];
  matrix[N, M] x;
}
parameters {
  real b_0;
  real b_1;
  vector[M] b_x;
  vector[3] b_year_free;
  vector[J] b_j_z;
  real<lower = 0> sigma_j;
  matrix[4, K] b_k_z;
  vector<lower = 0>[4] sigma_k;
  cholesky_factor_corr[4] L_k;
}
transformed parameters {
  vector[4] b_year = append_row(0, b_year_free);
  vector[J] b_j = b_j_z * sigma_j;
  matrix[K, 4] b_k = transpose(diag_pre_multiply(sigma_k, L_k) * b_k_z);
  vector[N] alpha;
  for (i in 1:N) {
    if (ii[i] == 1) {
      alpha[i] = 999;
    } else {
      alpha[i] = b_0 + b_j[jj[i]] + b_year[year[i] - 8] + b_k[kk[i], year[i] - 8] + b_1 * y[i - 1] + (x[i - 1] * b_x) * (1 - y[i - 1]);
    }
  }
}
model {
  b_0 ~ student_t(3, 0, 1);
  b_1 ~ student_t(3, 0, 1);
  b_x ~ student_t(3, 0, 1);
  b_year_free ~ student_t(3, 0, 1);
  b_j_z ~ std_normal();
  sigma_j ~ student_t(3, 0, 1);
  to_vector(b_k_z) ~ std_normal();
  to_vector(sigma_k) ~ student_t(3, 0, 3);
  L_k ~ lkj_corr_cholesky(2);
  for (i in 1:N) {
    if (ii[i] != 1) {
      y[i] ~ bernoulli_logit(alpha[i]);
    }
  }
}
generated quantities {
  corr_matrix[4] R_k = multiply_lower_tri_self_transpose(L_k);
}
