library(tidyverse)

# Read in data
data = read_csv("data/metrics_all.csv") %>%
  select(id, window, in_range_70_180, CV, below_70, episode_rate, site, trt) %>%
  mutate(trt = factor(trt, levels = c("SAP", "CLC")),
         trt = recode(trt, "SAP" = "Control"))


interaction_estimates = read_csv("data/interaction_estimates.csv") %>%
  mutate(Window = factor(Window, levels = c(7, 14, 30, 60, 90, 182)),
         Metric = case_when(
           Metric == "in_range_70_180" ~ "Time in Range",
           Metric == "CV" ~ "Coefficient of Variation",
           Metric == "below_70" ~ "Time Below 70",
           Metric == "episode_rate" ~ "Episode Rate"
         ))

# Figure 1 - Metric Values Over Time
longer = data |>
  rename("Time in Range" = in_range_70_180,
         "Coefficient of Variation" = CV,
         "Time Below 70" = below_70,
         "Episode Rate" = episode_rate,
         Treatment = trt) |>
  pivot_longer(c("Time in Range", "Coefficient of Variation",
                 "Time Below 70", "Episode Rate"),
                                                       names_to = "metric", values_to = "value")
longer$window = factor(longer$window, levels = c("baseline", 7, 14, 30, 60, 90, 182))

values_over_time = ggplot(longer, aes(x = window, y = value, color = Treatment)) +
  geom_boxplot() +
  labs(x = "Window Length",
       y = "Measure") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_manual(values = c("#3498DB", "#9B59B6")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# Figure 2 - Interaction Estimates
interaction_effect = interaction_estimates %>%
  ggplot(aes(x = Window, y = Estimate, color = Metric)) +
  geom_point() +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2) +
  labs(x = "Window Length",
       y = "Estimate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_color_manual(values = c("#1ABC9C", "#3498DB", "#E74C3C", "#9B59B6")) +
  theme_bw() +
  theme(legend.position = "none")


# Save out figures
ggsave("docs/Figure_1.jpeg", values_over_time, width = 15, height = 8, dpi = 1200)
ggsave("docs/Figure_2.jpeg", interaction_effect, width = 15, height = 8, dpi = 1200)

