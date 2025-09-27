# Reproducibility Guide

This document provides comprehensive instructions for reproducing the analyses and results presented in the Queens-Thesis project.

## Overview

This research project investigates the effects of napping and meditation on memory consolidation using Bayesian statistical analysis. The study compares three experimental conditions (meditation, napping, wakefulness) across declarative and non-declarative memory tasks.

## System Requirements

### Software
- **R** (version 4.0 or higher)
- **RStudio** (recommended IDE)
- **LaTeX** distribution (for PDF compilation)
- **Git** (for version control)

### Hardware
- **RAM**: Minimum 8GB (16GB recommended for Bayesian analyses)
- **Storage**: 2GB free space
- **CPU**: Multi-core processor recommended for MCMC sampling

## Installation

### 1. Clone Repository
```bash
git clone https://github.com/mohdasti/Queens-Thesis.git
cd Queens-Thesis
```

### 2. Install R Packages
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

### 3. Verify Installation
```r
# Check package installation
required_packages <- c("papaja", "rstanarm", "psycho", "tidyverse")
all_installed <- all(sapply(required_packages, require, character.only = TRUE))
if (all_installed) {
  cat("All required packages installed successfully!\n")
} else {
  cat("Some packages failed to install. Check error messages above.\n")
}
```

## Data Preparation

### 1. Load Main Dataset
```r
# Load the main dataset
RAW_data <- rio::import('Raw data/RAW_data.xlsx', na="N/A")
RAW <- RAW_data

# Verify data structure
str(RAW)
summary(RAW)
```

### 2. Create Analysis Subsets
```r
# Create condition subsets
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
NAP_SWS <- RAW[which(RAW$percentSWS != 0), ]
NAP_noSWS <- RAW[which(RAW$percentSWS == 0), ]

# Verify subset sizes
cat("Meditation:", nrow(MED), "participants\n")
cat("Wake:", nrow(WAKE), "participants\n")
cat("Nap:", nrow(NAP), "participants\n")
```

### 3. Calculate Performance Measures
```r
# Declarative memory measures
RAW$TPR <- RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
RAW$TNR <- RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
RAW$GMean <- sqrt(RAW$TPR * RAW$TNR)

# Non-declarative memory measures
RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median`

# Verify calculations
summary(RAW$GMean)
summary(RAW$Median.Diff)
```

## Statistical Analyses

### 1. Declarative Memory Analysis
```r
# Set random seed for reproducibility
set.seed(42)

# Create analysis dataset (excluding outliers)
RAW_declr <- RAW[-c(34,39,25,13,46),]

# Bayesian GLM for declarative memory
fit_GMean <- rstanarm::stan_glm(
  GMean ~ Condition, 
  data = RAW_declr,
  cores = 4,  # Use multiple cores for faster sampling
  prior = normal(0, 0.28)  # Weakly informative prior
)

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
fit_Bayes_RM_nonSWS <- rstanarm::stan_lmer(
  MedianScores ~ Condition + (1|Code), 
  data = nonSWS_repeated,
  cores = 4
)

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

fit_GMean_NAP <- rstanarm::stan_glm(
  GMean ~ SWS, 
  data = RAW_NAP,
  cores = 4
)

results <- psycho::analyze(fit_GMean_NAP)
summary(results, round = 2)
```

## Visualization

### 1. Performance Distribution Plots
```r
# Create data frame for plotting
NAP_GMean <- data.frame(Condition = "NAP", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'NAP']))
MED_GMean <- data.frame(Condition = "MED", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'MED']))
WAKE_GMean <- data.frame(Condition = "WAKE", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'WAKE']))
df_GMean <- rbind(NAP_GMean, MED_GMean, WAKE_GMean)

# Create plot
library(ggplot2)
p1 <- ggplot(df_GMean, aes(x = Condition, y = GeometricMean, color = Condition)) +
  geom_point(size = 4, alpha = 0.7, position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, color = "black", size = 4) +
  stat_summary(fun.ymin = function(x) (mean(x) - sd(x)),
               fun.ymax = function(x) (mean(x) + sd(x)),
               geom = "errorbar", width = 0.1) +
  scale_color_manual(values = c("MED" = "#2E8B57", "NAP" = "#4682B4", "WAKE" = "#CD5C5C")) +
  theme_apa() +
  labs(
    title = "Declarative Memory Performance Across Conditions",
    x = "Experimental Condition",
    y = "Geometric Mean Score"
  )

print(p1)
```

### 2. Bayesian Results Plots
```r
# Plot Bayesian means with credible intervals
p2 <- ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  size = 1.2, color = "#2E8B57") +
  geom_point(size = 3, color = "#2E8B57") +
  ylab("G-Mean") +
  xlab("Condition") +
  theme_apa() +
  labs(
    title = "Bayesian Analysis: Declarative Memory Performance",
    subtitle = "Error bars show 90% credible intervals"
  )

print(p2)
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

## Expected Results

### Declarative Memory
- **Meditation vs Wake**: 97.42% probability of difference
- **Meditation vs Nap**: 69.80% probability of difference
- **Effect sizes**: Small to medium

### Non-declarative Memory
- **SWS effects**: 96.23% probability of adverse effect
- **Meditation benefits**: Superior to both SWS and wake conditions
- **Gender differences**: Significant in both memory systems

## Troubleshooting

### Common Issues

1. **Package Installation Errors**
   ```r
   # Update R to latest version
   # Install system dependencies for LaTeX packages
   # Use install.packages() with dependencies = TRUE
   ```

2. **Memory Issues**
   ```r
   # Increase R memory limit
   memory.limit(size = 8000)
   
   # Use garbage collection
   gc()
   ```

3. **MCMC Convergence Issues**
   ```r
   # Increase iterations
   fit <- rstanarm::stan_glm(..., iter = 4000, warmup = 2000)
   
   # Check convergence
   summary(fit)
   ```

4. **LaTeX Compilation Errors**
   - Check LaTeX installation
   - Install missing LaTeX packages
   - Verify all required files are present

### Performance Optimization

1. **Parallel Processing**
   ```r
   # Use multiple cores for MCMC
   fit <- rstanarm::stan_glm(..., cores = 4)
   ```

2. **Caching**
   ```r
   # Use caching for expensive computations
   knitr::opts_chunk$set(cache = TRUE)
   ```

3. **Memory Management**
   ```r
   # Clear workspace periodically
   rm(list = ls())
   gc()
   ```

## Validation

### Reproducibility Checks
1. **Random Seed**: Ensure `set.seed(42)` is used
2. **Package Versions**: Record with `sessionInfo()`
3. **Data Integrity**: Verify no changes to original data
4. **Statistical Results**: Compare with published results

### Quality Assurance
1. **Code Review**: Check for errors and inconsistencies
2. **Documentation**: Ensure all steps are documented
3. **Testing**: Run analyses on subset of data first
4. **Validation**: Compare results with original analysis

## Support

### Getting Help
- **GitHub Issues**: Open an issue for bugs or questions
- **Documentation**: Check existing guides and documentation
- **Community**: Consult R and statistics communities

### Reporting Problems
When reporting issues, include:
- R version and operating system
- Package versions
- Error messages
- Steps to reproduce
- Expected vs. actual behavior

## Citation

If you use this code or data, please cite:

```bibtex
@thesis{dastgheib2021napping,
  title={Napping and Meditation on Memory Consolidation: A Bayesian Analysis of Sleep and Wake States},
  author={Dastgheib, Mohammad},
  year={2021},
  school={Queen's University},
  type={Master's Thesis}
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
