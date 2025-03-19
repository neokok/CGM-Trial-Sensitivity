library(tidyverse)
library(iglu)

##### Loading data and coercing variable types #####
data <- read_csv("../cleaned_data.csv")

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
                          episode_calculation(baseline_data) |> 
                            filter(type == "hypo", level == "lv1") |> 
                            select(total_episodes),
                          window = rep("baseline", length(unique(baseline_data$id))))

trial_data <- data |> filter(phase == "trial")

trial_metrics <- function(data, window){

  data <- data |> filter(time >= date(randomization) & time <= date(randomization) + days(window))
  
  metrics <- cbind(in_range_percent(trial_data,target_ranges = list(c(70,180))),
        cv_glu(trial_data)[,2],
        episode_calculation(trial_data) |> 
          filter(type == "hypo", level == "lv1") |> 
          select(total_episodes),
        window = rep(windows[i], length(unique(trial_data$id))))
  
  print(paste0("Window: ", window, " days"))
  
  return(metrics)
  }

windows <- c(7,14,30,60,90,180)

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
                     trial_180) 

metrics_all <- metrics_all |> 
  rename(tot_hyper_ep = total_episodes)

write_csv(metrics_all, "data/metrics_all.csv")

