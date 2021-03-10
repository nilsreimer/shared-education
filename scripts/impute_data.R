rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # This script imputes missing observations of the student-level         #
  # predictor and evaluates the accuracy of the imputation model using    #
  # cross-validation.                                                     #
  #########################################################################

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(rstan)
  library(loo)
  library(tidybayes)


# Functions ---------------------------------------------------------------
  
  # Link function
  inv_logit <- function(v) exp(v) / ( exp(v) + 1 )


# Prepare -----------------------------------------------------------------

  # Import data
  dr <- read_rds("data/raw/dr.rds")
  
  # Calculate treatment variable
  dt <- dr %>% 
    select(id, school, school_involvement, year, sep) %>%
    filter(year >= 11L) %>% 
    pivot_wider(
      names_from = year,
      values_from = sep,
      names_prefix = "y"
    ) %>%
    mutate(
      sep = case_when(
        y11 + y12 == 0L ~ 0L,
        y11 == 1L ~ 1L,
        y12 == 1L ~ 1L,
        TRUE ~ NA_integer_
      )
    )
  
  # Add case with no responses
  dt <- dr %>%
    distinct(id, school, school_involvement) %>%
    anti_join(dt, by = "id") %>%
    bind_rows(dt, .) %>%
    arrange(id)
  
  # Prepare data for analyses
  dt <- dt %>% 
    transmute(
      id,
      jj = as.integer(factor(id)),
      kk = as.integer(factor(school)),
      y11_c = replace_na(y11 - mean(y11, na.rm = TRUE), -1),
      y12_c = replace_na(y12 - mean(y12, na.rm = TRUE), -1),
      y11 = replace_na(y11, -1L),
      y12 = replace_na(y12, -1L),
      m11 = if_else(y11 == -1L, 1L, 0L),
      m12 = if_else(y12 == -1L, 1L, 0L),
      school_involvement,
      sep
    )

  # Compose data list
  dlist <- with(dt, list(
    J = n_distinct(jj),
    K = n_distinct(kk),
    jj = jj,
    kk = kk,
    y11 = y11,
    y12 = y12,
    m11 = m11,
    m12 = m12,
    y11_c = y11_c,
    y12_c = y12_c,
    school_involvement = school_involvement
  ))
  

# Estimate ----------------------------------------------------------------
  
  # Run model
  fit <- stan(
    "models/model_impute_data.stan", 
    data = dlist,
    iter = 1500,
    warmup = 1000,
    chains = 8,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.995),
    seed = 2331574
  )
  
  # Save results
  write_rds(fit, "results/stanfit/imputed_data.rds")


# Evaluate ----------------------------------------------------------------

  # Load results
  fit <- read_rds("results/stanfit/imputed_data.rds")

  # Calculate log likelihood (student-level predictor)
  sep_llk <- fit %>% 
    spread_draws(l11[jj], l12[jj]) %>% 
    ungroup() %>% 
    transmute(
      .chain, .iteration, .draw, jj,
      p11 = inv_logit(l11),
      p12 = inv_logit(l12),
      p_sep = p11 * p12 + (1 - p11) * p12 + p11 * (1 - p12)
    ) %>% 
    left_join(
      dt %>% 
        transmute(
          jj,
          y11 = if_else(y11 == -1L, NA_integer_, y11),
          y12 = if_else(y12 == -1L, NA_integer_, y12),
          sep
        ),
      by = "jj"
    ) %>% 
    filter(!is.na(sep)) %>% 
    mutate(
      sep_llk = case_when(
        sep == 1L ~ log(p_sep),
        sep == 0L ~ log(1 - p_sep)
      )
    ) %>% 
    select(.chain, .iteration, .draw, jj, sep_llk) %>% 
    pivot_wider(
      names_from = jj,
      values_from = sep_llk
    )
    
  # Estimate out-of-sample prediction accuracy (student-level predictor)
  sep_loo <- loo(
    x = sep_llk[,-1:-3] %>% as.matrix(),
    r_eff = relative_eff(sep_llk[,-1:-3] %>% as.matrix(), chain_id = sep_llk$.chain)
  )
  
  # Make confusion matrix
  cmat <- sep_loo$pointwise[,"elpd_loo"] %>% 
    as_tibble(rownames = "jj") %>% 
    transmute(
      jj = as.integer(jj),
      p = exp(value)
    ) %>%
    left_join(
      dt %>% transmute(jj, sep),
      by = "jj"
    ) %>% 
    rename(observed = sep) %>% 
    crossing(
      predicted = 0:1
    ) %>% 
    mutate(
      p = if_else(predicted == observed, p, 1 - p)
    ) %>% 
    group_by(predicted, observed) %>% 
    summarize(
      n = sum(p),
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = observed, values_from = n) %>% 
    select(-predicted) %>% 
    as.matrix()
    
  # Calculate prediction performance
  precision = cmat[2,2] / sum(cmat[2,])
  recall = cmat[2,2] / sum(cmat[,2])
  f1 = 2 * (precision * recall) / (precision + recall)


# Predict -----------------------------------------------------------------

  # Functions
  fit_alpha <- function(p) fitdistrplus::fitdist(p, "beta")[[1]][1]
  fit_beta  <- function(p) fitdistrplus::fitdist(p, "beta")[[1]][2]
  
  # Extract posterior draws
  ps <- fit %>% spread_draws(sep[jj])
  
  # Fit beta distributions
  ps <- ps %>% 
    filter(sep != 0, sep != 1) %>% 
    summarize(
      sep_alpha = fit_alpha(sep),
      sep_beta = fit_beta(sep)
    )
  
  # Merge observed and unobserved data
  dw <- ps %>% 
    full_join(dt %>% select(id, jj, sep), by = "jj") %>% 
    left_join(dr %>% select(-sep), ., by = "id") %>% 
    select(
      id, year, starts_with("school"), 
      sep, sep_alpha, sep_beta, 
      everything()
    )
  
  # Transform to long data
  dl <- dw %>% 
    pivot_longer(
      oa_1:mb_5,
      names_to = c("outcome", "item"),
      names_sep = "_",
      names_transform = list(outcome = as.character, item = as.integer),
      values_to = "response",
      values_transform = list(response = as.integer)
    )


# Export ------------------------------------------------------------------

  # Export as .rds
  write_rds(dw, "data/dw.rds")
  write_rds(dl, "data/dl.rds")
  
  # Export as .csv
  write_rds(dw, "data/csv/dw.csv")
  write_rds(dl, "data/csv/dl.csv")
  