library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom)

set.seed(42)

# Read in and re-format data for easier use
data = read_csv("data/metrics_all.csv") %>% select(id, window, in_range_70_180, CV, below_70, episode_rate, site, trt)
metrics = data %>% select(id, window, in_range_70_180, CV, below_70, episode_rate)
wide_data = pivot_wider(metrics, names_from = window, values_from = c(in_range_70_180, CV, below_70, episode_rate))
data$trt = factor(data$trt, levels = c("SAP", "CLC"))
screeningData = read.table("data/DCLP3 Public Dataset - Release 3 - 2022-08-04/Data Files/DiabScreening_a.txt",
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

# Create function for creating full and reduced models
full_model = function(data, metric){

  # Reformat data to create a variable for metric calculated at baseline and add demographic data
  combined_data = data %>%
    left_join(select(screeningData, id, InsModPump, CGMUseStat, AgeAtEnrollment), by = c("id")) %>% distinct()
  full_model_baseline = combined_data %>%
    filter(window == "baseline") %>%
    select(id, !!sym(metric)) %>%
    rename(!!paste0(metric, "_baseline") := !!sym(metric))
  full_model_data = combined_data %>%
    filter(window != "baseline") %>%
    left_join(full_model_baseline, by = c("id")) %>%
    mutate(window = factor(window, levels = c("182", "90", "60", "30", "14", "7")))

# If metric does not need to be transformed
  if(metric %in% c("in_range_70_180", "CV")){
    # Initialize formula
  formula = paste0(metric, " - ", metric, "_baseline",
                   " ~ trt * window + ", paste0(metric, "_baseline"),
                   "+ AgeAtEnrollment + InsModPump + CGMUseStat+ (1|id)")
  # Create full model
  model = lmer(as.formula(formula), data = full_model_data)

  # Initialize second formula for reduced model
  formula2 = paste0(metric, " - ", metric, "_baseline",
                    " ~ trt + ", paste0(metric, "_baseline"),
                    "+ AgeAtEnrollment + window + InsModPump + CGMUseStat + (1|id)")
  # Create reduced model for later anova testing
  model2 = lmer(as.formula(formula2), data = full_model_data)
  return(list(model, model2))
  } else{ # Repeat the process for log-transformed models
    formula = paste0("log(1 + ", metric, ") - ",  paste0("log(1 + ", metric, "_baseline)"),
                     " ~ trt * window + ", paste0("log(1 + ", metric, "_baseline)"),
                     "+ AgeAtEnrollment + InsModPump + CGMUseStat + (1|id)")
    model = lmer(as.formula(formula), data = full_model_data)
    formula2 = paste0("log(1 + ", metric, ") - ",  paste0("log(1 + ", metric, "_baseline)"),
                      " ~ trt + ", paste0("log(1 + ", metric, "_baseline)"),
                      "+ AgeAtEnrollment + window + InsModPump + CGMUseStat + (1|id)")
    model2 = lmer(as.formula(formula2), data = full_model_data)
    return(list(model, model2))
  }


}

# Initialize ANOVA dataframe
anova_results = data.frame(
  Metric = character(),
  F_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

interaction_results = data.frame(
  Metric = character(5),
  Window = numeric(5),
  Estimate = numeric(5),
  LB = numeric(5),
  UB = numeric(5),
  p = numeric(5)
)

interaction_results = data.frame(
  Metric = character(),
  Window = numeric(),
  Estimate = numeric(),
  LB = numeric(),
  UB = numeric(),
  p = numeric()
)

# Loop through each metric to save out ANOVA and model output
for(metric in colnames(metrics)[3:6]){

  # Create full and reduced models for given metric
  model = (full_model(data, metric))[[1]]
  model_reduced = (full_model(data, metric))[[2]]

  interaction = data.frame(
    Metric = character(5),
    Window = numeric(5),
    Estimate = numeric(5),
    LB = numeric(5),
    UB = numeric(5),
    p = numeric(5)
  )

  # Save interaction estimates
  interaction$Metric = metric
  interaction$Window = c(90, 60, 30, 14, 7)
  interaction$Estimate[1] = summary(model)$coef[12]
  interaction$Estimate[2] = summary(model)$coef[13]
  interaction$Estimate[3] = summary(model)$coef[14]
  interaction$Estimate[4] = summary(model)$coef[15]
  interaction$Estimate[5] = summary(model)$coef[16]
  interaction$LB[1] = summary(model)$coef[12] - 1.96 * summary(model)$coef[28]
  interaction$LB[2] = summary(model)$coef[13] - 1.96 * summary(model)$coef[29]
  interaction$LB[3] = summary(model)$coef[14] - 1.96 * summary(model)$coef[30]
  interaction$LB[4] = summary(model)$coef[15] - 1.96 * summary(model)$coef[31]
  interaction$LB[5] = summary(model)$coef[16] - 1.96 * summary(model)$coef[32]
  interaction$UB[1] = summary(model)$coef[12] + 1.96 * summary(model)$coef[28]
  interaction$UB[2] = summary(model)$coef[13] + 1.96 * summary(model)$coef[29]
  interaction$UB[3] = summary(model)$coef[14] + 1.96 * summary(model)$coef[30]
  interaction$UB[4] = summary(model)$coef[15] + 1.96 * summary(model)$coef[31]
  interaction$UB[5] = summary(model)$coef[16] + 1.96 * summary(model)$coef[32]
  interaction$p[1] = summary(model)$coef[76]
  interaction$p[2] = summary(model)$coef[77]
  interaction$p[3] = summary(model)$coef[78]
  interaction$p[4] = summary(model)$coef[79]
  interaction$p[5] = summary(model)$coef[80]

  interaction_results = rbind(interaction_results, interaction)

  # Compute the ANOVA comparing the reduced and full models
  anova_res = anova(model_reduced, model)

  # Clean up ANOVA output
  tidy_anova = tidy(anova_res)
  comparison = tidy_anova[2, ]

  # Add ANOVA output to table
  anova_results = rbind(anova_results,
                         data.frame(
                           Metric = metric,
                           F_value = comparison$statistic,
                           p_value = comparison$p.value,
                           stringsAsFactors = FALSE
                         ))

  # Save out full model summary output
  tab_model(model, file = paste0("docs/", metric, "_model_summary.html"))
}

# Save out combined ANOVA table
tab_df(anova_results, file = "docs/anova_results_summary.html", digits = 6)

write_csv(interaction_results, "data/interaction_estimates.csv")


