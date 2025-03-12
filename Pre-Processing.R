library(tidyverse)
library(haven)
library(lubridate)

# The dataset is downloaded as a folder containing multiple data tables and forms.
# First, download the entire dataset. Do not rename the downloaded folder.

# Data folder is named "DCLP3 Public Dataset - Release 3 - 2022-08-04" - rename if downloaded file name changes

# Differences in folder name due to future releases should be accounted for:
main_folder = list.dirs(full.names = TRUE, recursive = FALSE)
data_folder = main_folder[grepl("DCLP3 Public Dataset - Release", main_folder)]

# Read in necessary data
cgmData <- read.table(paste0(data_folder, "/Data Files/cgm.txt"), sep = "|", header = TRUE)
screeningData <- read.table(paste0(data_folder, "/Data Files/DiabScreening_a.txt"),
                            sep = "|", header = TRUE, fill = TRUE, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE, na.strings = "")

# Merge demographic data with CGM data
merged_data <- cgmData %>%
  left_join(screeningData, by = "PtID")

# Add additional variables: specify the dataset, subject type, device used, and placeholder values
final_data <- merged_data %>%
  mutate(
    id = PtID,
    time = as.POSIXct(dmy_hms(DataDtTm), format = "%Y-%m-%d %H:%M:%S"),
    gl = as.numeric(CGM),
    age = AgeAtEnrollment,
    sex = Gender,
    insulinModality = as.numeric(1),
    type = as.numeric(1),
    device = "Dexcom G6",
    dataset = "Brown"
  ) %>%
  # Remove NA times and gl values
  filter(!is.na(time), !is.na(gl)) %>%
  group_by(id) %>%
  # Ensure that time is in order
  arrange(time) %>%
  # Ungroup the dataset after creating pseudoID
  ungroup() %>%
  # Select necessary variables
  select(id, time, gl, age, sex, insulinModality, type, device, dataset)

# Check if 'csv_data' folder exists, create if not
if (!dir.exists("csv_data")) {
  dir.create("csv_data")
}

# Save the processed dataset to a CSV file in the 'csv_data' folder
#final_data %>% write.csv("csv_data/brown2021.csv", row.names = FALSE)

roster = as.data.frame(
  read.table(paste0("DCLP3 Public Dataset - Release 3 - 2022-08-04/Data Files/PtRoster_a.txt"),
             sep = "|", header = TRUE, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE, na.strings = "") %>% rename(id = PtID)
)

visit = as.data.frame(
  read.table(paste0("DCLP3 Public Dataset - Release 3 - 2022-08-04/Data Files/VisitInfo_a.txt"),
             sep = "|", header = TRUE, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE, na.strings = "") %>% rename(id = PtID)
)
# Separate randomization and screening visits
visit_randomization = visit %>% filter(Visit == "Randomization") %>% select(id, randomization = VisitDt)
visit_screening = visit %>% filter(Visit == "Run-in Initiation") %>% select(id, screening = VisitDt)

data = final_data %>% left_join(roster, by = c("id")) %>% # Join "roster" df to identify treatment type
  select(id, time, gl, screening = EnrollDt, randomization = RandDt, site = SiteID, trt = trtGroup) %>% # Select important variables
  mutate(screening = as.POSIXct(screening),
         randomization = as.POSIXct(randomization)) # Change screening and randomization into date format

phase_trial = data %>% mutate(phase = case_when(time < randomization & time >= randomization - weeks(2) ~ "baseline",
                                                time >= randomization & time < randomization + weeks(26) ~ "trial")) # Identify last 2 weeks of baseline and first 13 weeks of trial

df = phase_trial %>% filter(phase == "trial")
df_base = phase_trial %>% filter(phase == "baseline")

comparison_trial = df_base %>% filter(phase == "baseline") %>% # Only include trial phase
  in_range_percent() %>% left_join(data, by = c("id")) %>% # Calculate TIR and merge back with data df to get all variables
  group_by(trt) %>% summarise(mean = mean(in_range_70_180), sd = sd(in_range_70_180), n = length(unique(id))) # group by treatment and summarise



write_csv(phase_trial, "cleaned_data.csv")
