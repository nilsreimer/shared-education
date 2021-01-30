rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)


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
  

# Make posterior predictions ----------------------------------------------
  
  # Select cases
  cases <- c(26L, 481L, 1181L)

  # Make data matrix
  pp <- dl %>% 
    filter(outcome == "cq", jj %in% cases) %>% 
    transmute(
      ii, jj, kk, x_time = year - 8L, y_obs = response
    ) %>% 
    crossing(.draw = 1:4000, y = 1:5)

  # Add posterior draws
  pp <- pp %>% 
    left_join(
      read_rds("results/students/cq_ps_jj.rds") %>% filter(jj %in% cases),
      by = c(".draw", "jj")
    ) %>% 
    left_join(
      read_rds("results/students/cq_ps_kk.rds"),
      by = c(".draw", "kk")
    ) %>% 
    left_join(
      read_rds("results/students/cq_ps_x.rds") %>% select(-z_time),
      by = c(".draw", "x_time")
    ) %>% 
    left_join(
      read_rds("results/students/cq_ps_x.rds") %>% 
        filter(x_time > 0L) %>% 
        group_by(.draw) %>% 
        summarise(z_time = list(z_time)),
      by = c(".draw")
    ) %>% 
    left_join(
      read_rds("results/students/cq_ps_y.rds"),
      by = c(".draw", "y")
    )
  
  # Make predictions
  p_est <- pp %>% 
    mutate(
      mo = map2_dbl(z_time, x_time, mo),
      eta_0 = b_j_1 + b_k_1 + (b_time + b_j_2) * mo,
      eta_1 = eta_0 + b_sep
    ) %>% 
    select(.draw, ii:kk, y_obs, x_time, x_sep, eta_0, eta_1, y, c)
  
  # Derive probabilities
  p_est <- p_est %>% 
    mutate(
      p_0 = case_when(
        y == 1L ~ 1 - inv_logit(eta_0 - lead(c)),
        y  < 5L ~ inv_logit(eta_0 - c) - inv_logit(eta_0 - lead(c)),
        y == 5L ~ inv_logit(eta_0 - c)
      ),
      p_1 = case_when(
        y == 1L ~ 1 - inv_logit(eta_1 - lead(c)),
        y  < 5L ~ inv_logit(eta_1 - c) - inv_logit(eta_1 - lead(c)),
        y == 5L ~ inv_logit(eta_1 - c)
      ),
      p = x_sep * p_1 + (1 - x_sep) * p_0
    )
  
  # Calculate means
  d_pred <- p_est %>% 
    group_by(.draw, ii, jj, kk, y_obs, x_time, x_sep) %>% 
    summarise(y_pred = sample.int(5L, 1L, replace = FALSE, prob = p)) %>% 
    ungroup()
  
  
# Visualize ---------------------------------------------------------------

  # Figure 3
  d_pred %>% 
    mutate(
      x_sep = recode_factor(
        x_sep,
        "1" = "Yes",
        "0" = "No"
      )
    ) %>% 
    group_by(.draw, jj, kk, x_time, x_sep) %>% 
    summarise(m_pred = mean(y_pred)) %>% 
    group_by(jj, kk, x_time, x_sep) %>% 
    summarise(
      .lower = quantile(m_pred, 0.025),
      .upper = quantile(m_pred, 0.975),
      m_pred = median(m_pred)
    ) %>% 
  ggplot(., aes(x = x_time + 8L, y = m_pred, colour = x_sep)) + 
    geom_ribbon(
      aes(ymin = .lower, ymax = .upper, fill = x_sep),
      colour = NA,
      alpha = 0.25
    ) +
    geom_line(size = 0.25) +
    geom_point(aes(shape = x_sep), size = 0.75) +
    geom_line(
      data = d_pred %>% group_by(jj, x_time) %>% summarise(m_obs = mean(y_obs)),
      aes(y = m_obs),
      linetype = "dashed",
      colour = "grey50",
      size = 0.25
    ) +
    geom_point(
      data = d_pred %>% group_by(jj, x_time) %>% summarise(m_obs = mean(y_obs)),
      aes(y = m_obs),
      shape = "plus",
      colour = "grey50",
      size = 0.75
    ) +
    scale_shape_manual(values = c("Yes" = 16, "No" = 15), na.value = 18) +
    scale_colour_manual(values = c("Yes" = "#40B0A6", "No" = "#E1BE6A"), na.value = "grey50") +
    scale_fill_manual(values = c("Yes" = "#40B0A6", "No" = "#E1BE6A"), na.value = "grey50") +
    coord_fixed(1, ylim = c(1, 5)) +
    facet_wrap(vars(jj), nrow = 1) +
    theme_bw(base_size = 9, base_line_size = 0.25) +
    theme(
      legend.position = "bottom",
      legend.justification = "right",
      legend.title = element_text(size = rel(1), colour = "black"),
      legend.text = element_text(size = rel(1), colour = "black"),
      legend.key.width = unit(11, "points"),
      legend.key.height = unit(11, "points"),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = rel(1), colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1), colour = "black", hjust = 0.5)
    ) +
    labs(
      x = "Year",
      y = expression(italic("Mean")),
      shape  = "Shared Education:",
      colour = "Shared Education:",
      fill   = "Shared Education:"
    )
  
  # Export
  ggsave(
    "figures/figure-3.png", 
    width = 15, height = 7, units = "cm",
    dpi = 600, 
    type = "cairo-png"
  )
