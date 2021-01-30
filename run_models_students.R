rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan)
  
  # Stan options
  n_cores <- 8L
  options(mc.cores = n_cores)
  Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
  rstan_options(auto_write = FALSE)
  seeds <- c(
    6924624, 2265293, 4307307, 3011054, 5129713, 3444155, 8629018, 3905061
  )
  names(seeds) = c("cq", "oa", "tr", "em", "ia", "wc", "dp", "mb")
  
  # Command line arguments
  args <- commandArgs(TRUE)
  
  # Environment
  sessionInfo()
  

# Prepare -----------------------------------------------------------------

  # Import data
  dl <- read_rds("data/dl.rds")

  # Remove missing responses
  dl <- dl %>% filter(!is.na(response))
  
  # Create indices for analyses
  dl <- dl %>% 
    arrange(id, year, outcome, item) %>% 
    group_by(outcome) %>% 
    mutate(
      ii = row_number(),
      jj = as.integer(factor(id)),
      kk = as.integer(factor(school))
    ) %>% 
    ungroup()
  
  # Make data list
  make_dlist <- function(data, variable) {
    
    # Prepare data
    data <- data %>% filter(outcome == variable)
    
    # Compile data list
    data %>% 
      with(., list(
        N = max(ii),
        M = max(response),
        J = max(jj),
        K = max(kk),
        jj = jj,
        kk = kk,
        x_time = year - 8L,
        y = response,
        N_mis = sum(is.na(sep)),
        N_obs = sum(!is.na(sep)),
        ii_mis = data %>% filter(is.na(sep)) %>% pull(ii),
        ii_obs = data %>% filter(!is.na(sep)) %>% pull(ii)
      )) %>% 
      append(
        .,
        data %>% 
          distinct(jj, sep, sep_alpha, sep_beta) %>% 
          with(., list(
            sep_obs = if_else(is.na(sep), -1L, sep),
            sep_alpha = sep_alpha[is.na(sep)],
            sep_beta = sep_beta[is.na(sep)],
            J_mis = sum(is.na(sep)),
            J_obs = sum(!is.na(sep)),
            jj_mis = jj[is.na(sep)],
            jj_obs = jj[!is.na(sep)]
          ))
      ) %>% 
      return()
  }


# Estimate ----------------------------------------------------------------
  
  # Load model
  model <- stan_model("models/model_ordinal_students.stan")

  # Run model
  fit <- sampling(
    model,
    data = make_dlist(dl, args[1]),
    iter = 750 + 4000/n_cores,
    warmup = 750,
    chains = n_cores,
    # control = list(max_treedepth = 15),
    pars = c("eta", "b_j_z", "b_k_z", "sep_imp"),
    include = FALSE,
    seed = seeds[args[1]]
  )
  
  # Save results
  write_rds(fit, glue::glue("results/stanfit/{args[1]}_students.rds"))

  # Inspect inspect
  check_hmc_diagnostics(fit)
  print(
    fit,
    pars = c(
      "c",
      "b_time",
      "z_time",
      "b_sep",
      "sigma_j",
      "Rho_j",
      "sigma_k",
      "p"
    )
  )
