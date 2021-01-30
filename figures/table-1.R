rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)

  
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
  
  # Reformat
  dl <- dl %>% 
    select(ii, jj, kk, year, outcome, response) %>% 
    group_by(jj, kk, year, outcome) %>% 
    summarise(response = mean(response)) %>% 
    ungroup() %>% 
    mutate(
      outcome = factor(
        outcome, 
        levels = c("cq", "oa", "tr", "wc", "em", "ia", "dp", "mb")
      )
    )
  
  # Calculate correlation coefficients (within time points)
  r_within <- full_join(
      dl %>% rename(x_outcome = outcome, x = response),
      dl %>% rename(y_outcome = outcome, y = response),
      by = c("jj", "kk", "year")
    ) %>% 
    group_by(year, x_outcome, y_outcome) %>% 
    summarise(r = cor(x, y, use = "pairwise")) %>%
    group_by(x_outcome, y_outcome) %>% 
    summarise(r = mean(r)) %>% 
    spread(y_outcome, r)  

  # Calculate correlation coefficients (between time points)
  r_between <- dl %>% 
    rename(x_outcome = outcome, x = response, x_year = year) %>% 
    filter(x_year <= 11L) %>% 
    mutate(y_year = x_year + 1L) %>% 
    crossing(y_outcome = unique(dl$outcome)) %>% 
    left_join(
      dl %>% rename(y_outcome = outcome, y = response, y_year = year),
      by = c("jj", "kk", "y_outcome", "y_year")
    ) %>% 
    group_by(x_year, y_year, x_outcome, y_outcome) %>% 
    summarise(r = cor(x, y, use = "pairwise")) %>% 
    group_by(x_outcome, y_outcome) %>% 
    summarise(r = mean(r)) %>% 
    spread(y_outcome, r)
    
    
# Table 1 -----------------------------------------------------------------

  # Correlations
  r_within %>% mutate_if(is.double, round, digits = 2)
    
  # M, SD
  dl %>% 
    group_by(year, outcome) %>% 
    summarise(
      M  = mean(response, na.rm = TRUE),
      SD = sd(response, na.rm = TRUE)
    ) %>% 
    mutate_if(is.double, round, digits = 2) %>% 
    mutate_if(is.double, ~scales::number(., accuracy = 0.01)) %>% 
    mutate(
      table = glue::glue("{M} ({SD})")
    ) %>% 
    select(-M, -SD) %>% 
    spread(outcome, table)
  