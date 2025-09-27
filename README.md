# 🧠 Napping and Meditation on Memory Consolidation

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![LaTeX](https://img.shields.io/badge/LaTeX-Required-green.svg)](https://www.latex-project.org/)

## 📋 Project Overview

This repository contains the complete thesis project investigating the effects of napping and meditation on memory consolidation. The study examines whether meditation, as a state of wakefulness with sleep-like characteristics, can facilitate memory consolidation similar to sleep.

### 🎯 Research Question

**Primary Objective:** Assess whether meditation, a state of wakefulness that encompasses some sleep-related features, exerts an effect similar to that described for sleep to facilitate the consolidation of memories in the human brain.

## 🔬 Study Design

The study compares memory (explicit and implicit) consolidation among three experimental conditions:

- **🧘 Meditation**: Self-guided meditation session
- **😴 Napping**: 60-minute sleep opportunity  
- **🎬 Wake**: Watching documentary (control condition)

### Memory Tasks

- **Declarative Memory**: Paired-associate word learning task
- **Non-declarative Memory**: Marble maze visuomotor task

## 📊 Key Findings

- Meditation showed superior performance in declarative memory consolidation compared to wakefulness
- Sleep stage analysis revealed differential effects of slow-wave sleep (SWS) on memory consolidation
- Gender differences were observed in both memory systems
- EEG spectral analysis confirmed meditative states with alpha-theta oscillations

## 📁 Repository Structure

```
Queens-Thesis/
├── 📄 Thesis.Rmd              # Main thesis document
├── 📄 Introduction.Rmd        # Literature review and background
├── 📄 Methods.Rmd            # Study methodology
├── 📄 Results.Rmd            # Statistical analysis and results
├── 📄 Appendix.Rmd           # Supplementary materials
├── 📊 Analysis_Bayesian.R    # Bayesian statistical analysis
├── 📁 Raw data/              # Original experimental data
├── 📁 Figures/               # All figures and visualizations
├── 📁 Appendix/              # Consent forms, questionnaires, etc.
└── 📚 References.bib         # Bibliography
```

## 🛠️ Technical Details

### Software Requirements
- **R** (4.0+): Statistical analysis and data visualization
- **RStudio**: Development environment
- **LaTeX**: Document compilation
- **R Packages**: `papaja`, `rstanarm`, `psycho`, `ggplot2`, `tidyverse`

### Data Analysis
- **Statistical Framework**: Bayesian inference using MCMC
- **Memory Measures**: Geometric Mean (declarative), Median differences (non-declarative)
- **EEG Analysis**: Spectral analysis for meditative states

## 📈 Results Summary

### Declarative Memory
- Meditation > Wake (97.42% probability)
- No significant difference between Meditation and Napping

### Non-declarative Memory  
- SWS had adverse effects on procedural memory
- Meditation showed benefits over both SWS and wake conditions

## 🎓 Academic Context

**Institution**: Queen's University  
**Program**: Master's Thesis in Psychology  
**Supervisor**: Dr. Hans Dringenberg
**Year**: 2017-2020

## 📖 How to Use This Repository

1. **Clone the repository**
   ```bash
   git clone https://github.com/mohdasti/Queens-Thesis.git
   cd Queens-Thesis
   ```

2. **Install R dependencies**
   ```r
   install.packages(c("papaja", "rstanarm", "psycho", "ggplot2", "tidyverse"))
   ```

3. **Compile the thesis**
   ```r
   rmarkdown::render("Thesis.Rmd")
   ```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 👥 Contributing

This is a completed thesis project. For questions or clarifications, please open an issue.

## 📧 Contact

**Mohammad Dastgheib**  
Email: m.dastgheib@gmail.com 
GitHub: [@mohdasti](https://github.com/mohdasti)

---

*This research contributes to our understanding of memory consolidation mechanisms and the potential therapeutic applications of meditation in cognitive enhancement.*
