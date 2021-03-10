rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan)
  
  # Options
  outcomes <- c("cq", "oa", "tr", "wc", "em", "ia", "dp", "mb")
  seeds <- c(
    6924624, 2265293, 4307307, 3011054, 5129713, 3444155, 8629018, 4972547
  )
  names(seeds) <- outcomes
  

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
        x_sep = data %>% 
          distinct(kk, school_involvement) %>% 
          arrange(kk) %>% 
          pull(school_involvement),
        x_time = year - 8L,
        y = response
      )) %>% 
      return()
  }


# Estimate ----------------------------------------------------------------

  # Load model
  model <- stan_model("models/model_ordinal_schools.stan")

  # Run models
  for (outcome in outcomes ) {
    
    # Run model
    fit <- sampling(
      model,
      data = make_dlist(dl, outcome),
      iter = 750 + 4000/8,
      warmup = 750,
      chains = 8,
      cores = parallel::detectCores(),
      pars = c("b_j", "b_j_z", "b_k", "b_k_z"),
      include = FALSE,
      seed = seeds[outcome]
    )
    
    # Save results
    write_rds(fit, glue::glue("results/stanfit/{outcome}_schools.rds"))
    
    # Inspect inspect
    check_hmc_diagnostics(fit)
    print(
      fit,
      pars = c(
        "c",
        "b_sep",
        "b_sep_time",
        "z_time",
        "sigma_j",
        "Rho_j",
        "sigma_k",
        "Rho_k",
        "p"
      )
    )

  }
