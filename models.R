library(lme4)
library(tidyverse)

# Read in and re-format data for easier use
data = read_csv("data/metrics_all.csv") %>% select(id, window, in_range_70_180, CV, below_70, tot_hyper_ep, site, trt)
metrics = data %>% select(id, window, in_range_70_180, CV, below_70, tot_hyper_ep)
wide_data = pivot_wider(metrics, names_from = window, values_from = c(in_range_70_180, CV, below_70, tot_hyper_ep))
data$trt = factor(data$trt, levels = c("SAP", "CLC"))
screeningData <- read.table(paste0(data_folder, "/Data Files/DiabScreening_a.txt"),
                            sep = "|", header = TRUE, fill = TRUE, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE, na.strings = "") %>%
  select(id = PtID, InsModPump, CGMUseStat, AgeAtEnrollment) %>%
  mutate(InsModPump = ifelse(is.na(InsModPump), "Injection", "Pump"),
         CGMUseStat = ifelse(CGMUseStat == "Current", "Yes", "No"))
combined = left_join(wide_data, select(data, id, site, trt), by = c("id")) %>% left_join(screeningData, by = c("id")) %>% distinct()


# Function to run random effects model based on paper
run_model = function(data, window, metric){
  formula = paste0(paste0(metric, "_", window), " ~ trt +", paste0(metric, "_baseline"), "+ AgeAtEnrollment + InsModPump + CGMUseStat + (1|site)")
  model = lmer(as.formula(formula), data = data)
  return(model)
}

# Initialize results dataframe
results = as.data.frame(matrix(ncol = 5))
colnames(results) = c("metric", "length", "estimate", "se", "t")

# Run models for each metric & length combination and save treatment effect
for(m in colnames(data)[3:6]){
  for(l in unique(data$window)[-1]){
    model = summary(run_model(combined, l, m))$coef[2,]
    new_row = data.frame(metric = m,
                          length = l,
                          estimate = model[1],
                          se = model[2],
                          t = model[3])

    results = rbind(results, new_row)
  }
}

# Reformat and save out treatment effect data
results = results[-1,]
rownames(results) = c()
write_csv(results, "data/treatment_effect.csv")



