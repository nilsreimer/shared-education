rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(glue); library(rstan)
  
  # List outcomes
  outcomes <- c("cq", "oa", "tr", "wc", "em", "ia", "dp", "mb")
  outcome  <- "em"
  
  # Options
  J <- 1000L # n of students
  K <-   10L # n of schools


# Functions ---------------------------------------------------------------

  # Inverse logit (logistic) function
  inv_logit <- function(x) exp(x) / (1 + exp(x))
  
  # Calculate monotonic effects
  mo <- function(scale, i) {
    if (i == 0L) {
      0.0
    } else {
      length(scale) * sum(scale[1:i])
    } 
  }

# Prepare -----------------------------------------------------------------

  # Load results
  fit <- read_rds(glue("results/stanfit/{outcome}_students.rds"))
  
  # Import posterior samples
  ps_y <- read_rds(glue("results/students/{outcome}_ps_y.rds"))
  ps_x <- read_rds(glue("results/students/{outcome}_ps_x.rds"))
  
  # Extract posterior draws
  ps_j <- extract(fit, pars = c("sigma_j", "Rho_j"))
  ps_k <- extract(fit, pars = c("sigma_k"))
  

# Simulate ----------------------------------------------------------------

  # Set seed
  set.seed(1406391)
  
  # Simulate J students
  for (i in 1:4000) {
    Sigma_j <- with(ps_j, sigma_j[i,] * Rho_j[i,,] * rep(sigma_j[i,], each = 2))
    if (i == 1L) {
      b_j <- MASS::mvrnorm(J, c(0, 0), Sigma_j)
    } else {
      b_j <- rbind(b_j, MASS::mvrnorm(J, c(0, 0), Sigma_j))
    }
  }
  d_sim <- tibble(
    .draw = rep(1:4000, each = J), 
    jj = rep(1:J, times = 4000),
    b_j_1 = b_j[,1],
    b_j_2 = b_j[,2]
  )
  
  # Assign students to K schools
  d_sim <- d_sim %>% 
    left_join(
      tibble(jj = 1:J, kk = sample.int(K, J, replace = TRUE)),
      by = "jj"
    )
  
  # Simulate K schools
  d_sim <- crossing(
      nesting(
        .draw = 1:4000,
        sigma_k = ps_k$sigma_k
      ),
      kk = 1:K
    ) %>% 
    mutate(b_k = rnorm(n(), 0, sigma_k)) %>% 
    left_join(d_sim, ., by = c(".draw", "kk")) %>% 
    select(.draw, jj, kk, b_j_1, b_j_2, b_k_1 = b_k)
  
  # Add fixed effects
  d_sim <- d_sim %>% 
    # mutate(b_j_1 = 0, b_j_2 = 0, b_k_1 = 0) %>% 
    crossing(x_time = c(2L, 4L)) %>% 
    left_join(
      ps_x %>% select(-z_time),
      by = c(".draw", "x_time")
    ) %>% 
    left_join(
      ps_x %>% 
        filter(x_time > 0L) %>% 
        group_by(.draw) %>% 
        summarise(z_time = list(z_time)),
      by = c(".draw")
    )
  
  # Add thresholds
  d_sim <- d_sim %>% 
    crossing(y = 1:5) %>% 
    left_join(ps_y, by = c(".draw", "y"))
  
  # Assign participation
  d_sim <- d_sim %>%
    # left_join(
    #   tibble(jj = 1:J, x_sep = sample(rep(0:1, each = J/2))),
    #   by = "jj"
    # )
    crossing(x_sep = 0:1)
  
  # Make predictions
  d_sim <- d_sim %>% 
    mutate(
      mo = map2_dbl(z_time, x_time, mo),
      eta = b_j_1 + b_k_1 + (b_time + b_j_2) * mo + b_sep * x_sep
    )
  
  # Derive probabilities
  d_sim <- d_sim %>% 
    select(.draw, jj, kk, x_time, x_sep, y, c, eta) %>% 
    arrange(.draw, jj, x_time, x_sep, y) %>% 
    mutate(
      p = case_when(
        y == 1L ~ 1 - inv_logit(eta - lead(c)),
        y  < 5L ~ inv_logit(eta - c) - inv_logit(eta - lead(c)),
        y == 5L ~ inv_logit(eta - c)
      )
    )
  

# Calculate ---------------------------------------------------------------

  # Transform
  d_est <- d_sim %>% 
    transmute(.draw, jj, kk, x_time = x_time + 8L, x_sep, y, p) %>% 
    pivot_wider(
      names_from = c(x_time, y),
      names_prefix = "p_",
      values_from = p
    )
  
  # Calculate change probabilities
  d_est <- d_est %>% 
    mutate_at(vars(starts_with("p_")), log) %>% 
    mutate(
      p_higher = 
        exp(p_10_1 + p_12_2) + exp(p_10_1 + p_12_3) + exp(p_10_1 + p_12_4) + exp(p_10_1 + p_12_5) +
        exp(p_10_2 + p_12_3) + exp(p_10_2 + p_12_4) + exp(p_10_2 + p_12_5) +
        exp(p_10_3 + p_12_4) + exp(p_10_3 + p_12_5) +
        exp(p_10_4 + p_12_5),
      p_same = 
        exp(p_10_1 + p_12_1) + 
        exp(p_10_2 + p_12_2) + 
        exp(p_10_3 + p_12_3) +
        exp(p_10_4 + p_12_4) +
        exp(p_10_5 + p_12_5),
      p_lower = 
        exp(p_10_5 + p_12_4) + exp(p_10_5 + p_12_3) + exp(p_10_5 + p_12_2) + exp(p_10_5 + p_12_1) +
        exp(p_10_4 + p_12_3) + exp(p_10_4 + p_12_2) + exp(p_10_4 + p_12_1) +
        exp(p_10_3 + p_12_2) + exp(p_10_3 + p_12_1) +
        exp(p_10_2 + p_12_1)
    ) %>% 
    select(.draw, jj, kk, x_sep, p_higher, p_same, p_lower)
  
  # Summarize
  d_est %>% 
    group_by(.draw, jj, kk) %>% 
    summarise(
      higher = p_higher[2] * (p_same[1] + p_lower[1]) + p_same[2] * p_lower[1],
      same   = p_higher[2] * p_higher[1] + p_same[2] * p_same[1] + p_lower[2] * p_lower[1],
      lower  = p_lower[2] * (p_same[1] + p_higher[1]) + p_same[2] * p_higher[1],
      improved = higher - lower
    ) %>% 
    group_by(.draw) %>% 
    summarise_at(vars(higher:improved), mean) %>% 
    pivot_longer(
      -.draw,
      names_to = "counterfactual",
      values_to = "estimate"
    ) %>% 
    group_by(counterfactual) %>% 
    summarise(
      d_est = median(estimate),
      .lower = quantile(estimate, 0.025),
      .upper = quantile(estimate, 0.975)
    ) %>% 
    mutate_if(is.double, ~ceiling(. * 1000))
