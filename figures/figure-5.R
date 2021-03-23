rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(patchwork)

  
# Prepare -----------------------------------------------------------------

  # Import results
  d_est <- bind_rows(
    read_rds("results/schools/cq_ps_p.rds") %>% mutate(outcome = "cq"),
    read_rds("results/schools/oa_ps_p.rds") %>% mutate(outcome = "oa"),
    read_rds("results/schools/tr_ps_p.rds") %>% mutate(outcome = "tr"),
    read_rds("results/schools/em_ps_p.rds") %>% mutate(outcome = "em"),
    read_rds("results/schools/ia_ps_p.rds") %>% mutate(outcome = "ia"),
    read_rds("results/schools/wc_ps_p.rds") %>% mutate(outcome = "wc"),
    read_rds("results/schools/dp_ps_p.rds") %>% mutate(outcome = "dp"),
    read_rds("results/schools/mb_ps_p.rds") %>% mutate(outcome = "mb")
  )
  
  # Calculate estimated means
  d_est <- d_est %>% 
    group_by(.draw, x_sep, x_time, outcome) %>% 
    summarise(m = sum(y * p)) %>% 
    ungroup()

  # Reformat
  d_est <- d_est %>% 
    mutate(
      x_sep = recode_factor(
        x_sep, 
        "3" = "Current involvement",
        "2" = "Past involvement",
        "1" = "No involvement"
      ),
      x_time = x_time + 8L,
      name = recode_factor(
        outcome,
        "oa" = "Outgroup Attitudes",
        "tr" = "Outgroup Trust", 
        "em" = "Intergroup Empathy",
        "ia" = "Intergroup Anxiety",
        "cq" = "Intergroup Contact",
        "wc" = "Future Contact",
        "dp" = "Deprovincialization",
        "mb" = "Multicultural Beliefs"
      )
    )
  
  # Calculate observed standard deviations (at Year 10)
  sds <- read_rds("data/dl.rds") %>% 
    filter(year == 10L, !is.na(response)) %>% 
    group_by(outcome, id) %>% 
    summarise(mean = mean(response)) %>%
    group_by(outcome) %>% 
    summarise(sd = sd(mean))
  

# Figure 5 ----------------------------------------------------------------

  # Figure 5a
  f5a <- d_est %>% 
    group_by(x_sep, x_time, outcome, name) %>% 
    summarise(
      m_est = median(m),
      .lower = quantile(m, 0.025),
      .upper = quantile(m, 0.975)
    ) %>% 
  ggplot(., aes(x = x_time, y = m_est, group = x_sep)) +
    geom_vline(
      xintercept = 10L,
      linetype = "dashed",
      colour = "black",
      size = 0.25
    ) +
    geom_ribbon(
      aes(ymin = .lower, ymax = .upper, fill = x_sep),
      alpha = 0.35
    ) +
    geom_line(
      aes(colour = x_sep),
      size = 0.25
    ) +
    geom_point(
      aes(shape = x_sep, colour = x_sep, size = x_sep),
    ) +
    scale_size_manual(values = c(1.5, 1, 1)) +
    scale_shape_manual(values = c(18, 15, 16)) +
    scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d")) +
    scale_fill_manual(values = c("#018571", "#80cdc1", "#dfc27d")) +
    coord_fixed(1, ylim = c(1, 5)) +
    facet_wrap(vars(name), 2, 4) +
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
      strip.text = element_text(size = rel(1), colour = "black", hjust = 0)
    ) +
    labs(
      tag = "A",
      x = "Year",
      y = expression(italic("Mean")),
      size   = "Shared Education:",
      shape  = "Shared Education:",
      colour = "Shared Education:",
      fill   = "Shared Education:"
    )
  
  # Figure 5b
  f5b <- d_est %>% 
    distinct(name) %>% 
    ggplot(., aes(y = fct_rev(name))) +
    geom_text(
      aes(label = fct_rev(name)),
      x = 1,
      size = 9/.pt,
      hjust = 1
    ) +
    facet_grid(. ~ "") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_bw(base_size = 9, base_line_size = 0.25) +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1)),
      plot.margin = margin(4.5, 2, 4.5, 4.5, unit = "pt")
    ) +
    labs(
      tag = "B"
    )
    
  # Figure 5c
  f5c <- d_est %>% 
    pivot_wider(
      names_from = x_time,
      names_prefix = "y_",
      values_from = "m"
    ) %>% 
    arrange(.draw, name, x_sep) %>% 
    group_by(.draw, outcome, name) %>% 
    summarise(
      d_10_3 = (y_10[1] - y_10[3]),
      d_11_3 = (y_11[1] - y_11[3]) - (y_10[1] - y_10[3]),
      d_12_3 = (y_12[1] - y_12[3]) - (y_10[1] - y_10[3]),
      d_10_2 = (y_10[2] - y_10[3]),
      d_11_2 = (y_11[2] - y_11[3]) - (y_10[2] - y_10[3]),
      d_12_2 = (y_12[2] - y_12[3]) - (y_10[2] - y_10[3])
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      starts_with("d_"),
      names_to = c("comparison", "x_sep"),
      names_pattern = "(d_[0-9]*)_([2-3])",
      values_to = "d"
    ) %>% 
    left_join(sds, by = "outcome") %>% 
    mutate(d = d/sd) %>% 
    group_by(outcome, name, comparison, x_sep) %>% 
    summarise(
      d_est = median(d),
      .lower = quantile(d, 0.025),
      .upper = quantile(d, 0.975)
    ) %>% 
    ungroup() %>% 
    mutate(
      comparison = recode_factor(
        comparison,
        "d_10" = "Year 10",
        "d_11" = "Year 11 (adjusted)",
        "d_12" = "Year 12 (adjusted)"
      ),
      x_sep = recode_factor(
        x_sep, 
        "3" = "Current involvement",
        "2" = "Past involvement",
        "1" = "No involvement"
      ),
      p = sign(.lower) == sign(.upper)
    ) %>% 
  ggplot(., aes(x = fct_rev(name), y = d_est)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      size = 0.25
    ) +
    geom_linerange(
      aes(ymin = .lower, ymax = .upper, colour = x_sep),
      position = position_dodge(width = 0.5),
      size = 0.25
    ) +
    geom_point(
      aes(colour = x_sep, shape = x_sep, size = x_sep),
      position = position_dodge(width = 0.5)
    ) +
    scale_y_continuous(
      breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8)
    ) +
    scale_size_manual(values = c(1.5, 1, 1)) +
    scale_shape_manual(values = c(18, 15, 16)) +
    scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d")) +
    coord_flip(ylim = c(-0.25, 0.6)) +
    facet_wrap(vars(comparison), nrow = 1) +
    theme_bw(base_size = 9, base_line_size = 0.25) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = rel(1), colour = "black"),
      legend.text = element_text(size = rel(1), colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(size = rel(1), colour = "black"),
      axis.title = element_text(size = rel(1), colour = "black"),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1), colour = "black", hjust = 0),
      plot.margin = margin(4.5, 4.5, 4.5, 0, unit = "pt")
    ) +
    labs(
      x = NULL,
      y = expression(Delta~italic("Mean")~("Cohen's"~italic(d)))
    )
  
  # Combine figures
  f5a + f5b + f5c + plot_layout(
    heights = c(10.8, 6.2), 
    design = "
    AAAA
    BCCC
    "
  )
  
# Export ------------------------------------------------------------------
  
  # Save as .pdf
  ggsave(
    "figures/figure-5.pdf", 
    width = 15.9, height = 18, units = "cm",
  )
  
  # Save as .png
  ggsave(
    "figures/figure-5.png", 
    width = 15.9, height = 18, units = "cm",
    dpi = 600, 
    type = "cairo-png"
  )
  