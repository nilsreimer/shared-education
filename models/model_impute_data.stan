data {
  int<lower = 1> J;
  int<lower = 1> jj[J];
  int<lower = -1, upper = 1> y11[J];
  int<lower = -1, upper = 1> y12[J];
  // Missing
  int<lower = 0, upper = 1> m11[J];
  int<lower = 0, upper = 1> m12[J];
  // Centering
  vector<lower = -1, upper = 1>[J] y11_c;
  vector<lower = -1, upper = 1>[J] y12_c;
  // Schools
  int<lower = 1> K;
  int<lower = 1, upper = K> kk[J];
  int<lower = 1, upper = 3> school_involvement[J];
}
parameters {
  // Year 11 
  real a11;
  real b11_y12;
  real b11_m12;
  vector[3] b11_sep;
  // Year 12
  real a12;
  real b12_y11;
  real b12_m11;
  vector[3] b12_sep;
  // Schools
  vector[3] b11_k[K];
  vector[3] b12_k[K];
  vector<lower = 0>[3] sigma11;
  vector<lower = 0>[3] sigma12;
  cholesky_factor_corr[3] Lcorr11;
  cholesky_factor_corr[3] Lcorr12;
}
transformed parameters {
  vector[J] l11;
  vector[J] l12;
  for (j in 1:J) {
    l11[j] = a11 + b11_k[kk[j],1]*sigma11[1] + (b11_y12 + b11_k[kk[j],2]*sigma11[2])*(1-m12[j])*y12_c[j] + (b11_m12 + b11_k[kk[j],3]*sigma11[3])*m12[j] + b11_sep[school_involvement[j]];
    l12[j] = a12 + b12_k[kk[j],1]*sigma12[1] + (b12_y11 + b12_k[kk[j],2]*sigma12[2])*(1-m11[j])*y11_c[j] + (b12_m11 + b12_k[kk[j],3]*sigma12[3])*m11[j] + b12_sep[school_involvement[j]];
  }
}
model {
  // Likelihood
  for (j in 1:J) {
    if (y11[j] != -1) y11[j] ~ bernoulli_logit(l11[j]);
    if (y12[j] != -1) y12[j] ~ bernoulli_logit(l12[j]);
  }
  // Priors: Year 11
  a11  ~ student_t(3, 0, 1);
  b11_y12 ~ student_t(3, 0, 1);
  b11_m12 ~ student_t(3, 0, 1);
  b11_sep ~ student_t(3, 0, 1);
  // Priors: Year 12
  a12  ~ student_t(3, 0, 1);
  b12_y11 ~ student_t(3, 0, 1);
  b12_m11 ~ student_t(3, 0, 1);
  b12_sep ~ student_t(3, 0, 1);
  // Priors: Schools
  b11_k ~ multi_normal_cholesky(rep_vector(0, 3), Lcorr11);
  b12_k ~ multi_normal_cholesky(rep_vector(0, 3), Lcorr12);
  sigma11 ~ student_t(3, 0, 1);
  sigma12 ~ student_t(3, 0, 1);
  Lcorr11 ~ lkj_corr_cholesky(2);
  Lcorr12 ~ lkj_corr_cholesky(2);
}
generated quantities {
  matrix[3, 3] Rho11 = multiply_lower_tri_self_transpose(Lcorr11);
  matrix[3, 3] Rho12 = multiply_lower_tri_self_transpose(Lcorr12);
  vector<lower = 0, upper = 1>[J] p11 = inv_logit(l11);
  vector<lower = 0, upper = 1>[J] p12 = inv_logit(l12);
  vector<lower = 0, upper = 1>[J] sep;
  vector[J] llk11;
  vector[J] llk12;
  for (j in 1:J) {
    if (y11[j] != -1) {
      p11[j] = to_vector(y11)[j];
      llk11[j] = bernoulli_logit_lpmf(y11[j] | l11[j]);
    }
    if (y12[j] != -1) {
      p12[j] = to_vector(y12)[j];
      llk12[j] = bernoulli_logit_lpmf(y12[j] | l12[j]);
    }
    sep[j] = p11[j]*p12[j] + (1-p11[j])*p12[j] + p11[j]*(1-p12[j]);
  }
}
