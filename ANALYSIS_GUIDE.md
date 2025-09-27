# Analysis Guide

This guide provides step-by-step instructions for reproducing the analyses in the Queens-Thesis project.

## Prerequisites

### Software Requirements
1. **R** (version 4.0 or higher)
2. **RStudio** (recommended)
3. **LaTeX** distribution (for PDF compilation)
4. **Git** (for version control)

### R Package Installation
```r
# Install required packages
install.packages(c(
  "papaja", "rstanarm", "psycho", "tidyverse", "ggplot2",
  "rio", "dplyr", "readxl", "Hmisc", "lmerTest", 
  "corrplot", "bayesplot", "jtools", "magrittr",
  "devtools", "knitr", "rmarkdown"
))

# For EEG analysis (optional)
install.packages(c(
  "eegkit", "eegkitdata", "bigsplines", "quadprog",
  "ica", "signal", "rgl"
))
```

## Data Preparation

### 1. Load Main Dataset
```r
# Load the main dataset
RAW_data <- rio::import('Raw data/RAW_data.xlsx', na="N/A")
RAW <- RAW_data

# Create condition subsets
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
NAP_SWS <- RAW[which(RAW$percentSWS != 0), ]
NAP_noSWS <- RAW[which(RAW$percentSWS == 0), ]
```

### 2. Calculate Performance Measures
```r
# Declarative memory measures
RAW$TPR <- RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
RAW$TNR <- RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
RAW$GMean <- sqrt(RAW$TPR * RAW$TNR)

# Non-declarative memory measures
RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median`
```

### 3. Create Analysis Datasets
```r
# Exclude outliers and create analysis datasets
RAW_declr <- RAW[-c(34,39,25,13,46),]  # Declarative memory
RAW_nondeclr <- RAW[-c(7,34,39,25,13,44,49,64),]  # Non-declarative memory
```

## Statistical Analyses

### 1. Declarative Memory Analysis
```r
# Bayesian GLM for declarative memory
fit_GMean <- rstanarm::stan_glm(GMean ~ Condition, data=RAW_declr)

# Analyze results
results <- psycho::analyze(fit_GMean)
summary(results, round = 2)

# Get contrasts and means
contrasts <- psycho::get_contrasts(fit_GMean, "Condition")
means <- psycho::get_means(fit_GMean, "Condition")
```

### 2. Non-declarative Memory Analysis
```r
# Load repeated measures data
nonSWS_repeated <- rio::import("Raw data/Repeated measures/nonSWS_repeated.xlsx", na = "N/A")
SWS_repeated <- rio::import("Raw data/Repeated measures/SWS_repeated.xlsx", na = "N/A")

# Bayesian mixed model for non-SWS participants
fit_Bayes_RM_nonSWS <- rstanarm::stan_lmer(MedianScores ~ Condition + (1|Code), data=nonSWS_repeated)

# Analyze results
results <- psycho::analyze(fit_Bayes_RM_nonSWS)
summary(results, round = 2)
```

### 3. Sleep Stage Analysis
```r
# Compare SWS vs non-SWS nappers
RAW_declr$SWS[RAW_declr$percentSWS == 0] <- "Absent"
RAW_declr$SWS[RAW_declr$percentSWS != 0] <- "Present"
RAW_NAP <- RAW_declr[1:23,]

fit_GMean_NAP <- rstanarm::stan_glm(GMean ~ SWS, data=RAW_NAP)
results <- psycho::analyze(fit_GMean_NAP)
```

## Visualization

### 1. Performance Distribution Plots
```r
# Declarative memory distribution
ggplot(df_GMean, aes(x = Condition, y = GeometricMean, color = Condition)) +
  geom_point(size = 4, alpha = 0.7, position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, color = "black", size = 4) +
  stat_summary(fun.ymin = function(x) (mean(x) - sd(x)),
               fun.ymax = function(x) (mean(x) + sd(x)),
               geom = "errorbar", width = 0.1) +
  theme_apa()
```

### 2. Bayesian Results Plots
```r
# Plot Bayesian means with credible intervals
ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("G-Mean") +
  xlab("Condition") +
  theme_apa()
```

## Document Compilation

### 1. Compile Individual Sections
```r
# Compile Introduction
rmarkdown::render("Introduction.Rmd")

# Compile Methods
rmarkdown::render("Methods.Rmd")

# Compile Results
rmarkdown::render("Results.Rmd")

# Compile Appendix
rmarkdown::render("Appendix.Rmd")
```

### 2. Compile Complete Thesis
```r
# Compile main thesis document
rmarkdown::render("Thesis.Rmd")
```

## Troubleshooting

### Common Issues

1. **Package Installation Errors**
   - Update R to latest version
   - Install system dependencies for LaTeX packages
   - Use `install.packages()` with `dependencies = TRUE`

2. **Data Loading Issues**
   - Check file paths are correct
   - Ensure Excel files are not open in another program
   - Verify file permissions

3. **Compilation Errors**
   - Check LaTeX installation
   - Install missing LaTeX packages
   - Verify all required files are present

4. **Memory Issues**
   - Increase R memory limit: `memory.limit(size = 8000)`
   - Use `gc()` to free memory
   - Process data in smaller chunks

### Performance Tips

1. **Caching**
   - Use `cache = TRUE` in R chunks for expensive computations
   - Set `cache.extra = knitr::rand_seed` for reproducibility

2. **Parallel Processing**
   - Use `cores = 4` in `stan_glm()` for faster MCMC sampling
   - Consider `future` package for parallel data processing

3. **Data Management**
   - Use `data.table` for large datasets
   - Implement efficient data filtering and subsetting

## Reproducibility

### Setting Random Seeds
```r
# Set seed for reproducibility
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
```

### Session Information
```r
# Record session info for reproducibility
sessionInfo()
```

### Version Control
- Use Git to track changes
- Tag releases with version numbers
- Document any changes to analysis pipeline

## Additional Resources

- [papaja package documentation](https://github.com/crsh/papaja)
- [rstanarm documentation](https://mc-stan.org/rstanarm/)
- [psycho package documentation](https://github.com/neuropsychology/psycho.R)
- [Bayesian Analysis with R](https://www.r-bloggers.com/2019/08/bayesian-analysis-with-r/)

For questions or issues, please open an issue on the GitHub repository.
