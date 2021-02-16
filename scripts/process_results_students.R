rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # To meet file size limits, we do not include the files                 #
  # results/stanfit/{outcome}_students.rds in this repository. To run     #
  # this script, rerun run_models_students.R first.                       #
  #########################################################################

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
        names_transform = list(y = as.integer),
        values_to = "c"
      ) %>% 
      mutate(y = y + 1L) %>% 
      complete(.draw, y = 1L) %>% 
    left_join(
      as.data.frame(fit, pars = c("b_time")) %>% 
        rowid_to_column(".draw"),
      by = ".draw"
    )
  }

  # Extract posterior samples for time-dependant parameters
  extract_samples_x_time <- function(fit) {
    as.data.frame(fit, pars = c("z_time", "b_sep")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "x_time"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_transform = list(parameter = as.character, x_time = as.integer),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      ) %>% 
      mutate(
        x_time = x_time - 1L,
        z_time = lag(z_time),
      )
  }
  
  # Extract posterior samples for student-level parameters
  extract_samples_jj <- function(fit) {
    as.data.frame(fit, pars = c("b_j_1", "b_j_2", "x_sep")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("parameter", "jj"),
        names_pattern = "([:graph:]*)\\[([0-9]*)\\]",
        names_transform = list(parameter = as.character, jj = as.integer),
        values_to = "estimate"
      ) %>% 
      pivot_wider(
        names_from = parameter,
        values_from = estimate
      )
  }
  
  # Extract posterior samples for school-level parameters
  extract_samples_kk <- function(fit) {
    as.data.frame(fit, pars = c("b_k_1")) %>% 
      rowid_to_column(".draw") %>% 
      pivot_longer(
        -.draw,
        names_to = c("kk"),
        names_pattern = "[:graph:]*\\[([0-9]*)\\]",
        names_transform = list(kk = as.integer),
        values_to = "b_k_1"
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
        names_transform = list(
          x_sep = as.integer, 
          x_time = as.integer,
          y = as.integer
        ),
        values_to = "p"
      ) %>% 
      mutate(
        x_sep = x_sep - 1L,
        x_time = x_time - 1L
      )
  }
  

# Extract posterior samples -----------------------------------------------
  
  for (outcome in outcomes) {
    
    # Load results
    fit <- read_rds(glue("results/stanfit/{outcome}_students.rds"))
    
    # Save posterior samples
    extract_samples_y(fit) %>%
      write_rds(glue("results/students/{outcome}_ps_y.rds"))
    extract_samples_x_time(fit) %>%
      write_rds(glue("results/students/{outcome}_ps_x.rds"))
    extract_samples_jj(fit) %>%
      write_rds(glue("results/students/{outcome}_ps_jj.rds"))
    extract_samples_kk(fit) %>%
      write_rds(glue("results/students/{outcome}_ps_kk.rds"))
    extract_samples_p(fit) %>%
      write_rds(glue("results/students/{outcome}_ps_p.rds"))
    
  }
