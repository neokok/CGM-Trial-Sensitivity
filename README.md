# Assessing the Impact of Shortened Follow-up Time in Continuous Glucose Monitor (CGM) Clinical Trials

This repository hosts the complete, reproducible workflow for the manuscript\
**“Assessing the Impact of Shortened Follow-up Time in Continuous Glucose Monitor (CGM) Clinical Trials”** by Neo Kok, Walter Williamson, and Will Tackett.

------------------------------------------------------------------------

## Data

### 1. Download

1.  Visit the JAEB Center for Health Research: <https://public.jaeb.org>.\
2.  Download and unzip **“DCLP3 Public Dataset – Release 3 – 2022-08-04”**.\
3.  Move the unzipped folder into this repository’s `data/` directory.

For details on the original extraction steps, see the Awesome-CGM wiki:\
<https://github.com/IrinaStatsLab/Awesome-CGM/wiki>.

### 2. Pre-processing

From the `code/` directory, run:

```         
pre-processing.R
```

The script (adapted from Awesome-CGM) cleans the raw files and saves **`cleaned_data.csv`** in `data/`.

## Analysis

### 1. Metric Calculation

From the `code/` directory, run:

```         
analysis.R
```

Using cleaned_data.csv, this script computes all CGM metrics and writes metrics_all.csv to data/.

### 2. Model Fitting

```         
models.R
```

This script fits the linear mixed-effects models described in the paper with metrics_all.csv and:

-   saves four model summaries (one per metric) to `docs/`,

-   writes interaction_estimates.csv to `docs/`,

-   saves an aggregated ANOVA table to `docs/`.

### 3. Figure Generation

```         
figures.R
```

Using `metrics_all.csv` and `interaction_estimates.csv`, this script creates Figure 1 and Figure 2 and stores them in `docs/`.

## Final Report

The manuscript is written in Quarto (`final-project.qmd`). Render the PDF with:

```         
quarto render report.qmd
```

All analyses were conducted using R version 4.4.1.
