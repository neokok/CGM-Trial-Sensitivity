library(tidyverse)
library(iglu)

##### Loading data and coercing variable types #####
data <- read_csv("data/cleaned_data.csv")

data <- data |>
  mutate(id = as.factor(id),
         site = as.factor(site),
         trt = as.factor(trt),
         phase = as.factor(phase))

###### Filtering into analysis windows and calculating metrics ######

baseline_data <- data |> filter(phase == "baseline")
baseline_metrics <- cbind(in_range_percent(baseline_data,
                                           target_ranges = list(c(70,180))),
                          cv_glu(baseline_data)[,2],
                          below_percent(baseline_data, targets_below = c(70))[,2],
                          episode_calculation(baseline_data) |>
                            filter(type == "hypo", level == "lv1") |>
                            select(total_episodes),
                          window = rep("baseline", length(unique(baseline_data$id))))

trial_data <- data |> filter(phase == "trial")

trial_metrics <- function(data, window){

  data <- data |> filter(time >= date(randomization) & time <= date(randomization) + days(window))

  metrics <- cbind(in_range_percent(data, target_ranges = list(c(70,180))),
        cv_glu(data)[,2],
        below_percent(data, targets_below = c(70))[,2],
        episode_calculation(data) |>
          filter(type == "hypo", level == "lv1") |>
          select(total_episodes),
        window = rep(windows[i], length(unique(data$id))))

  print(paste0("Window: ", window, " days"))

  return(metrics)
  }

windows <- c(7,14,30,60,90,182)

for(i in seq_along(windows)){
  assign(paste0("trial_",windows[i]), trial_metrics(trial_data, windows[i]))
}

###### Combining Data Frames ######
metrics_all <- rbind(baseline_metrics,
                     trial_7,
                     trial_14,
                     trial_30,
                     trial_60,
                     trial_90,
                     trial_182)

metrics_all <- metrics_all |>
  left_join(data |>
              group_by(id) |>
              summarise(b_end = max(time), b_start = min(time),
                        site = first(site), trt = first(trt),
                        randomization = first(randomization),
                        screening = first(screening),
                        .groups = "drop" ) |>
              select(id, site, trt, b_end, b_start, randomization, screening), by = "id") |>
  rename(tot_hyper_ep = total_episodes) |>
  mutate(randomization_days = case_when(window != "baseline" ~ as.numeric(window),
                                   window == "baseline" ~ round(as.numeric(difftime(randomization, b_start, units = "days")))),
         episode_rate = tot_hyper_ep/randomization_days) |>
  select(-randomization_days)

write_csv(metrics_all, "data/metrics_all.csv")
