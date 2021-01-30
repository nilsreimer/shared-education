rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(glue); library(rstan)

  # List outcomes
  outcomes <- c("cq", "oa", "tr", "wc", "em", "ia", "dp", "mb")


# Functions ---------------------------------------------------------------

  # Extract posterior samples for fixed parameters
  extract_samples_y <- function(fit) {
    as.data.frame(fit, pars = c("c")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("y"),
        names_pattern = "[:graph:]*\\[([0-9]*)\\]",
        names_ptypes = list(y = integer()),
        values_to = "c"
      ) %>% 
      mutate(y = y + 1L) %>% 
      complete(.draw, y = 1L)
  }

  # Extract posterior samples for time-dependant parameters
  extract_samples_x_time <- function(fit) {
    as.data.frame(fit, pars = c("z_time")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "x_time"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_ptypes = list(parameter = character(), x_time = integer()),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      ) %>% 
      complete(.draw = 1:max(.draw), x_time = 0:4)
  }
  
  # Extract posterior samples for time-dependant parameters
  extract_samples_x_sep <- function(fit) {
    as.data.frame(fit, pars = c("b_sep", "b_sep_time")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "x_sep"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_ptypes = list(parameter = character(), x_sep = integer()),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      )
  }
  
  # Extract posterior samples for student-level parameters
  extract_samples_jj <- function(fit) {
    as.data.frame(fit, pars = c("b_j_1", "b_j_2")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "jj"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_ptypes = list(parameter = character(), jj = integer()),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      )
  }
  
  # Extract posterior samples for school-level parameters
  extract_samples_kk <- function(fit) {
    as.data.frame(fit, pars = c("b_k_1", "b_k_2")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "kk"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_ptypes = list(parameter = character(), kk = integer()),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      )
  }
  
  # Extract predictions based on fixed effects
  extract_samples_p <- function(fit) {
    as.data.frame(fit, pars = c("p")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("x_sep", "x_time", "y"),
        names_pattern = "p\\[([0-9]*),([0-9]*),([0-9]*)\\]",
        names_ptypes = list(
          x_sep = integer(), 
          x_time = integer(),
          y = integer()
        ),
        values_to = "p"
      ) %>% 
      mutate(
        x_sep = x_sep,
        x_time = x_time - 1L
      )
  }
  

# Extract posterior samples -----------------------------------------------
  
  for (outcome in outcomes) {
    
    # Load results
    fit <- read_rds(glue("results/stanfit/{outcome}_schools.rds"))
    
    # Save posterior samples
    extract_samples_y(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_y.rds"))
    extract_samples_x_time(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_x_time.rds"))
    extract_samples_x_sep(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_x_sep.rds"))
    extract_samples_jj(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_jj.rds"))
    extract_samples_kk(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_kk.rds"))
    extract_samples_p(fit) %>%
      write_rds(glue("results/schools/{outcome}_ps_p.rds"))
    
  }
