library(tidyverse)

# Read in data
data = read_csv("data/metrics_all.csv") %>%
  select(id, window, in_range_70_180, CV, below_70, tot_hyper_ep, site, trt) %>%
  mutate(trt = factor(trt, levels = c("SAP", "CLC")))

treatment_effect = read_csv("data/treatment_effect.csv") %>%
  mutate(length = factor(length, levels = c(7, 14, 30, 60, 90, 182)))


# Figure 1 - Metric Values Over Time

longer = pivot_longer(data, c(in_range_70_180, CV, below_70, tot_hyper_ep),
                      names_to = "metric", values_to = "value")
longer$window = factor(longer$window, levels = c("baseline", 7, 14, 30, 60, 90, 182))

values_over_time = ggplot(longer, aes(x = window, y = value, color = trt)) +
  geom_boxplot() +
  labs(x = "Window Length",
       y = "Estimate",
       title = "Estimates by Treatment Group and Window Length \n for Each Metric") +
  facet_wrap(~ metric, scales = "free_y")


# Figure 2 - Treatment Effect Over Time
effect_over_time = ggplot(treatment_effect, aes(x = length, y = estimate, color = metric)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0.2) +
  labs(x = "Window Length",
       y = "Estimate",
       title = "Model Estimates by Metric and Window Length") +
  facet_wrap(~ metric, scales = "free_y")




# Save out figures
ggsave("figures/values_over_time.jpeg", values_over_time, width = 15, height = 8, dpi = 1200)
ggsave("figures/effect_over_time.jpeg", effect_over_time, width = 15, height = 8, dpi = 1200)
