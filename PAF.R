library(dplyr)
library(ggplot2)
library(patchwork)

data <- data.frame(
  outcome = rep(c("Heart Failure", "MACE"), each = 2),
  group = rep(c("Cases", "Controls"), 2),
  PAF = c(22.3, 38.0, 13.7, 11.7),
  ARR = c(453.7, 180.0, 139.4, 65.0),
  ARR_lower = c(40, 60, 10, 15),
  ARR_upper = c(980, 340, 275, 120)
) %>%
  mutate(
    outcome = factor(outcome, levels = c("Heart Failure", "MACE")),
    group = factor(group, levels = c("Cases", "Controls"))
  )


p1 <- ggplot(data, aes(x = outcome, y = PAF, fill = group)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.1f", PAF)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(
    values = c("Cases" = "#5DA5DA",
               "Controls" = "#F15A49")
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Population Attributable Fraction (%)",
    fill = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "#EDF4FB", colour = NA),
    legend.position = "bottom",
    legend.key.width  = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(color = "black")
  )

p2 <- ggplot(
  data,
  aes(x = outcome, y = ARR, fill = group)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    aes(ymin = ARR_lower, ymax = ARR_upper),
    position = position_dodge(width = 0.7),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = sprintf("%.1f", ARR)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c("Cases" = "#5DA5DA",
               "Controls" = "#F15A49")
  ) +
  scale_y_continuous(
    limits = c(0, 1100),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Attributable Rate Reduction (ARR)",
    fill = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "#EDF4FB", colour = NA),
    legend.position = "bottom",
    legend.key.width  = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(color = "black")
  )


figure <- p1 + p2 +
  plot_layout(widths = c(1, 1.2))

figure





library(dplyr)

data <- bind_rows(
  data.frame(
    outcome = "MACE",
    group = c("Cases", "Controls"),
    PAF = 100 * c(PAF_cases, PAF_controls),
    ARR = c(ARR_cases, ARR_controls),
    ARR_lower = c(
      quantile(ARR_boot_cases, 0.025, na.rm = TRUE),
      quantile(ARR_boot_controls, 0.025, na.rm = TRUE)
    ),
    ARR_upper = c(
      quantile(ARR_boot_cases, 0.975, na.rm = TRUE),
      quantile(ARR_boot_controls, 0.975, na.rm = TRUE)
    )
  ),
  data.frame(
    outcome = "HF",
    group = c("Cases", "Controls"),
    PAF = 100 * c(PAF_cases_HF, PAF_controls_HF),
    ARR = c(ARR_cases_HF, ARR_controls_HF),
    ARR_lower = c(
      quantile(ARR_boot_cases_HF, 0.025, na.rm = TRUE),
      quantile(ARR_boot_controls_HF, 0.025, na.rm = TRUE)
    ),
    ARR_upper = c(
      quantile(ARR_boot_cases_HF, 0.975, na.rm = TRUE),
      quantile(ARR_boot_controls_HF, 0.975, na.rm = TRUE)
    )
  )
) %>%
  mutate(
    outcome = factor(outcome, levels = c("HF", "MACE")),
    group = factor(group, levels = c("Cases", "Controls"))
  )
