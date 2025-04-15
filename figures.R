library(tidyverse)

# Read in data
data = read_csv("data/metrics_all.csv") %>%
  select(id, window, in_range_70_180, CV, below_70, tot_hyper_ep, site, trt) %>%
  mutate(trt = factor(trt, levels = c("SAP", "CLC")),
         trt = recode(trt, "SAP" = "Control"))

treatment_effect = read_csv("data/treatment_effect.csv") %>%
  mutate(length = factor(length, levels = c(7, 14, 30, 60, 90, 182)),
         metric = case_when(
           metric == "in_range_70_180" ~ "Time in Range",
           metric == "CV" ~ "Coefficient of Variation",
           metric == "below_70" ~ "Time Below 70",
           metric == "tot_hyper_ep" ~ "Total Hyper Epi"
         )) |> 
  rename(Metric = metric)


# Figure 1 - Metric Values Over Time

longer = data |> 
  rename("Time in Range" = in_range_70_180,
         "Coefficient of Variation" = CV,
         "Time Below 70" = below_70,
         "Total Hyper Epi" = tot_hyper_ep,
         Treatment = trt) |> 
  pivot_longer(c("Time in Range", "Coefficient of Variation",
                 "Time Below 70", "Total Hyper Epi"),
                                                       names_to = "metric", values_to = "value")
longer$window = factor(longer$window, levels = c("baseline", 7, 14, 30, 60, 90, 182))

values_over_time = ggplot(longer, aes(x = window, y = value, color = Treatment)) +
  geom_boxplot() +
  labs(x = "Window Length",
       y = "Estimate",
       title = "Metric Distributions") +
  facet_wrap(~ metric, scales = "free_y") + 
  theme(plot.title = element_text(hjust = 0.5))


# Figure 2 - Treatment Effect Over Time
effect_over_time = ggplot(treatment_effect, aes(x = length, y = estimate, color = Metric)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0.2) +
  labs(x = "Window Length",
       y = "Estimate",
       title = "Treatment Effect Estimates by Metric and Window Length") +
  facet_wrap(~ Metric, scales = "free_y") + 
  theme(legend.position = "none") 




# Save out figures
ggsave("output/Figure_1.jpeg", values_over_time, width = 15, height = 8, dpi = 1200)
ggsave("output/Figure_2.jpeg", effect_over_time, width = 15, height = 8, dpi = 1200)
