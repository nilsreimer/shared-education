rm(list = ls())

# Notes -------------------------------------------------------------------

  #########################################################################
  # This script estimates to what extent whether students dropped out of  #
  # the study depended on the outcome variables measured in this study.   #
  #########################################################################

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(rstan); library(tidybayes)

  # Functions
  inv_logit <- function(alpha) exp(alpha) / (1 + exp(alpha))


# Prepare -----------------------------------------------------------------

  # Import data
  dw <- read_rds("data/dw.rds")
  
  # Add missing cases
  dw <- dw %>% 
    expand(
      id, year
    ) %>% 
    left_join(
      dw %>% distinct(id, school),
      by = "id"
    ) %>% 
    left_join(
      dw,
      by = c("id", "year", "school")
    ) %>% 
    mutate(
      missing = if_all(oa_1:mb_5, is.na)
    ) %>% 
    select(id, school, year, missing, oa_1:mb_5)
  
  # Calculate predictor variables
  dw <- dw %>% 
    rowwise() %>% 
    mutate(
      oa = mean(c(oa_1, oa_2, oa_3, oa_4)),
      tr = mean(c(tr_1, tr_2)),
      em = mean(c(em_1, em_2, em_3, em_4)),
      ia = mean(c(ia_1, ia_2)),
      wc = mean(c(wc_1, wc_2)),
      cq = mean(c(cq_1, cq_2, cq_3, cq_4, cq_5)),
      dp = mean(c(dp_1, dp_2)),
      mb = mean(c(mb_1, mb_2, mb_3, mb_4, mb_5))
    ) %>% 
    ungroup() %>% 
    select(-oa_1:-mb_5) %>% 
    mutate(
      across(oa:mb, ~(. - mean(., na.rm = TRUE))/sd(., na.rm = TRUE))
    )
  
  # Exclude rows before first response 
  dm <- dw %>% 
    group_by(id) %>% 
    filter(
      !(
        (year ==  8L & missing) |
        (year ==  9L & missing & lag(missing, 1L)) |
        (year == 10L & missing & lag(missing, 1L) & lag(missing, 2L)) |
        (year == 11L & missing & lag(missing, 1L) & lag(missing, 2L) & lag(missing, 3L)) |
        (year == 12L & missing & lag(missing, 1L) & lag(missing, 2L) & lag(missing, 3L) & lag(missing, 4L))
      )
    ) %>% 
    ungroup()

  # Create indices for analysis
  dm <- dm %>% 
    arrange(id, year) %>%
    group_by(id) %>% 
    mutate(
      ii = row_number()
    ) %>% 
    ungroup() %>% 
    mutate(
      jj = as.integer(factor(id)),
      kk = as.integer(factor(school)),
      y = as.integer(missing)
    )
  
  # Replace NA with last value
  dm <- dm %>% 
    group_by(id) %>% 
    mutate(
      across(oa:mb, ~if_else(is.na(.), lag(., 1), .)),
      across(oa:mb, ~if_else(is.na(.), lag(., 2), .)),
      across(oa:mb, ~if_else(is.na(.), lag(., 3), .)),
      across(oa:mb, ~if_else(is.na(.), lag(., 4), .)),
      across(oa:mb, ~if_else(is.na(.), 0, .))
    ) %>% 
    ungroup()
  
  # Make data list
  dlist <- with(dm, list(
    J = max(jj),
    K = max(kk),
    N = length(ii),
    M = 8L,
    ii = ii,
    jj = jj,
    kk = kk,
    year = year,
    y = y,
    x = cbind(oa, tr, em, ia, wc, cq, dp, mb)
  ))

# Estimate ----------------------------------------------------------------
  
  # Load model
  model <- stan_model("models/model_dropout_analysis.stan")
  
  # Run model
  fit <- sampling(
    model,
    data = dlist,
    iter = 750 + 4000/8,
    warmup = 750,
    chains = 8,
    cores = parallel::detectCores(),
    pars = c("b_year_free", "b_j_z", "b_k_z", "L_k"), include = FALSE,
    seed = 2385648
  )
  
  # Save results
  write_rds(fit, "results/stanfit/dropout.rds")

  
# Visualize ---------------------------------------------------------------

  # Load results
  fit <- read_rds("results/stanfit/dropout.rds")
  
  # Figure S1
  fit %>% 
    spread_draws(b_x[x]) %>% 
    mutate(
      x = factor(x, levels = c(1:4, 6, 5, 7:8)),
      x = recode_factor(
        x,
        "1" = "Outgroup Attitudes",
        "2" = "Outgroup Trust",
        "3" = "Intergroup Empathy",
        "4" = "Intergroup Anxiety",
        "6" = "Intergroup Contact",
        "5" = "Future Contact",
        "7" = "Deprovincialization",
        "8" = "Multicultural Beliefs"
      ),
      b_x = exp(b_x)
    ) %>% 
    median_qi(b_x) %>%
  ggplot(., aes(x = b_x, y = fct_rev(x))) +
    geom_rect(
      xmin = 1/3.47, xmax = 3.47,
      ymin = -Inf, ymax = Inf,
      fill = "grey96"
    ) +
    geom_rect(
      xmin = 1/1.68, xmax = 1.68,
      ymin = -Inf, ymax = Inf,
      fill = "grey92"
    ) +
    geom_hline(
      yintercept = 1:8,
      colour = "white",
      size = 0.25
    ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      size = 0.25
    ) +
    geom_linerange(
      aes(xmin = .lower, xmax = .upper),
      size = 0.25
    ) +
    geom_point(
      shape = 18,
      size = 1.25
    ) +
    annotate(
      geom = "text",
      x = 1 + 0.68/2, 
      y = 0.75,
      label = "very small",
      size = 9/.pt,
      fontface = "italic"
    ) +
    annotate(
      geom = "text",
      x = 1.68 + 1.79/2, 
      y = 0.75,
      label = "small",
      size = 9/.pt,
      fontface = "italic"
    ) +
    scale_x_continuous(
      limits = c(1/3.47, 3.47),
      breaks = c(1/3.47, 1/1.68, 1, 1.68, 3.47),
      labels = c("1/3.47", "1/1.68", "1", "1.68", "3.47")
    ) +
    theme_bw(base_size = 9, base_line_size = 0.25) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = rel(1), colour = "black"),
      legend.text = element_text(size = rel(1), colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid = element_blank(),
      axis.text = element_text(size = rel(1), colour = "black"),
      axis.title = element_text(size = rel(1), colour = "black"),
      axis.title.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1), colour = "black", hjust = 0)
    ) +
    labs(
      x = "Odds Ratio",
      y = NULL
    )


# Export ------------------------------------------------------------------

  # Save as .pdf
  ggsave(
    "figures/figure-S1.pdf", 
    width = 15.9, height = 15.9/2, units = "cm"
  )
  
  # Save as .png
  ggsave(
    "figures/figure-S1.png",
    width = 15.9, height = 15.9/2, units = "cm",
    dpi = 600, 
    type = "cairo-png"
  )
  