# Data Dictionary

This document describes the variables and data structure used in the Queens-Thesis project.

## Main Dataset: `RAW_data.xlsx`

### Participant Information
- **Code**: Unique participant identifier
- **Age**: Participant age in years
- **Gender**: Participant gender (M/F)
- **Handedness**: Handedness (R/L)
- **Condition**: Experimental condition (MED/NAP/WAKE)

### Memory Performance Measures

#### Declarative Memory (Paired Associates Task)
- **Hit ratio**: Proportion of correctly identified old word pairs
- **Miss ratio**: Proportion of missed old word pairs  
- **False Alarm ratio**: Proportion of incorrectly identified new word pairs
- **Correct Rejection ratio**: Proportion of correctly rejected new word pairs
- **TPR**: True Positive Rate (Hit ratio / (Hit ratio + False Alarm ratio))
- **TNR**: True Negative Rate (Correct Rejection ratio / (Correct Rejection ratio + Miss ratio))
- **GMean**: Geometric Mean (√(TPR × TNR)) - overall performance measure

#### Non-declarative Memory (Marble Maze Task)
- **Trial 91-100 median**: Median score of last 10 training trials
- **Median 1-10**: Median score of first 10 test trials
- **Median.Diff**: Difference between test and training medians (retention measure)

### Sleep Parameters (NAP condition only)
- **Sleep latency**: Time to fall asleep (minutes)
- **Stage 1**: Duration in Stage 1 sleep (minutes)
- **Stage 2**: Duration in Stage 2 sleep (minutes)
- **SWS**: Duration in Slow Wave Sleep (minutes)
- **REM sleep**: Duration in REM sleep (minutes)
- **Total sleep time**: Total time asleep (minutes)
- **Number of arousals**: Count of sleep interruptions
- **percentSWS**: Percentage of total sleep time in SWS

### Self-Report Measures
- **ESS_Pre**: Epworth Sleepiness Scale score before intervention
- **ESS_Post**: Epworth Sleepiness Scale score after intervention
- **FMI**: Freiburg Mindfulness Inventory score
- **ESS_diff**: Difference in ESS scores (Post - Pre)

### Meditation Parameters (MED condition only)
- **Minutes in meditative state**: Duration of alpha-theta EEG activity
- **Months of practice**: Self-reported meditation experience

## Repeated Measures Datasets

### `nonSWS_repeated.xlsx`
- **Code**: Participant identifier
- **Condition**: Experimental condition
- **Trial**: Trial number (1-150)
- **MedianScores**: Median score for each trial block

### `SWS_repeated.xlsx`
- **Code**: Participant identifier  
- **Condition**: Experimental condition
- **Trial**: Trial number (1-150)
- **MedianScores**: Median score for each trial block

## Acquisition Curve Data

### `Acquisition_curve_overall.xlsx`
- **Trials**: Trial block identifier (1-10, 11-20, etc.)
- **Condition**: Experimental condition
- **Sum_Scores**: Average scores across participants

## Data Exclusions

### Declarative Memory Analysis
Participants excluded: 34, 39, 25, 13, 46
- Reasons: Outliers beyond ±2 SD, non-compliance with experimental conditions

### Non-declarative Memory Analysis  
Participants excluded: 7, 34, 39, 25, 13, 44, 49, 64
- Reasons: Outliers beyond ±2 SD, non-compliance with experimental conditions

## Missing Data
- **N/A**: Indicates missing or not applicable data
- **0**: Used for sleep parameters when no sleep occurred
- **NA**: R's standard missing value indicator

## Data Quality Notes
- All behavioral data underwent outlier detection (±2 SD criterion)
- EEG data was visually inspected for artifacts
- Sleep scoring followed standard Rechtschaffen & Kales criteria
- Meditation state detection based on alpha-theta power spectral analysis

## Statistical Analysis Notes
- Bayesian inference used for all primary analyses
- MCMC sampling: 4 chains, 2000 iterations, 1000 warmup
- Credible intervals reported at 90% level
- Maximum Probability of Effect (MPE) used for significance testing
