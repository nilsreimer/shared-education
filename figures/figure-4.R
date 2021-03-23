rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse); library(patchwork)

  
# Prepare -----------------------------------------------------------------

  # Import results
  d_est <- bind_rows(
    read_rds("results/students/cq_ps_p.rds") %>% mutate(outcome = "cq"),
    read_rds("results/students/oa_ps_p.rds") %>% mutate(outcome = "oa"),
    read_rds("results/students/tr_ps_p.rds") %>% mutate(outcome = "tr"),
    read_rds("results/students/em_ps_p.rds") %>% mutate(outcome = "em"),
    read_rds("results/students/ia_ps_p.rds") %>% mutate(outcome = "ia"),
    read_rds("results/students/wc_ps_p.rds") %>% mutate(outcome = "wc"),
    read_rds("results/students/dp_ps_p.rds") %>% mutate(outcome = "dp"),
    read_rds("results/students/mb_ps_p.rds") %>% mutate(outcome = "mb")
  )
  
  # Calculate estimated means
  d_est <- d_est %>% 
    group_by(.draw, x_sep, x_time, outcome) %>% 
    summarise(m = sum(y * p)) %>% 
    ungroup()

  # Reformat
  d_est <- d_est %>% 
    mutate(
      x_sep = recode_factor(x_sep, "1" = "Yes", "0" = "No"),
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

  
# Figure 4 ----------------------------------------------------------------

  # Figure 4a
  f4a <- d_est %>% 
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
      aes(shape = x_sep, colour = x_sep),
      size = 0.75
    ) +
    scale_shape_manual(values = c("Yes" = 16, "No" = 15)) +
    scale_colour_manual(values = c("Yes" = "#40B0A6", "No" = "#E1BE6A")) +
    scale_fill_manual(values = c("Yes" = "#40B0A6", "No" = "#E1BE6A")) +
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
      shape  = "Shared Education:",
      colour = "Shared Education:",
      fill   = "Shared Education:"
    )
  
  # Figure 4b
  f4b <- d_est %>% 
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
  
  # Figure 4c
  f4c <- d_est %>% 
    pivot_wider(
      names_from = x_time,
      names_prefix = "y_",
      values_from = "m"
    ) %>% 
    arrange(.draw, name, x_sep) %>% 
    group_by(.draw, outcome, name) %>% 
    summarise(
      d_10 = (y_10[1] - y_10[2]),
      d_11 = (y_11[1] - y_11[2]) - (y_10[1] - y_10[2]),
      d_12 = (y_12[1] - y_12[2]) - (y_10[1] - y_10[2]),
    ) %>% 
    ungroup() %>% 
    pivot_longer(
      starts_with("d_"),
      names_to = "comparison",
      values_to = "d"
    ) %>% 
    left_join(sds, by = "outcome") %>% 
    mutate(d = d/sd) %>% 
    group_by(outcome, name, comparison) %>% 
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
      p = sign(.lower) == sign(.upper)
    ) %>% 
  ggplot(., aes(x = d_est, y = fct_rev(name))) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      size = 0.25
    ) +
    geom_linerange(
      aes(xmin = .lower, xmax = .upper, colour = p),
      size = 0.25
    ) +
    geom_point(
      aes(colour = p),
      shape = 18,
      size = 1.25
    ) +
    scale_x_continuous(
      breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8)
    ) +
    scale_color_grey(start = 0.6, end = 0.0) +
    coord_cartesian(xlim = c(-0.5, 0.35)) +
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
      x = expression(Delta~italic("Mean")~("Cohen's"~italic(d))),
      y = NULL
    )
  
  
  # Combine figures
  f4a + f4b + f4c + plot_layout(
    heights = c(10.8, 6.2), 
    design = "
    AAAA
    BCCC
    "
  )


# Export ------------------------------------------------------------------

  # Save as .pdf
  ggsave(
    "figures/figure-4.pdf", 
    width = 15.9, height = 18, units = "cm"
  )
  
  # Save as .png
  ggsave(
    "figures/figure-4.png", 
    width = 15.9, height = 18, units = "cm",
    dpi = 600, 
    type = "cairo-png"
  )
  