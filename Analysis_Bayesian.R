#Loading required packages
required_packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rio","readxl", "tidyverse","devtools","car","lsr","dplyr","ggplot2","magrittr","Hmisc","psycho","lmerTest","rstanarm","jtools","bayesplot")
required_packages(packages)

# importing the main excel file from Github repository (mohdasti)
RAW_data <- rio::import('https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/RAW_data.xlsx?raw=true', na="N/A")
View(RAW_data)
RAW <- RAW_data # rename for simplicity

#creating subsets of data 
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
NAP_SWS <- RAW[which(RAW$percentSWS != 0), ]
NAP_noSWS <- RAW[which(RAW$percentSWS == 0), ]

# Calculating Geometric Mean and Difference of Medians (two main measures of performance for the declarative and non-declarative memory) ----
# True Positive rate
RAW$TPR <-
  RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
#True Negative rate
RAW$TNR <-
  RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
#calculating the G-Mean
RAW$GMean <- 
  sqrt(RAW$TPR * RAW$TNR)
# Difference of the medians
RAW$Median.Diff <- 
  RAW$`Median 1-10` - RAW$`Trial 91-100 median` 

#Excluding outliers - creating modified datasets ----
## any data points that is beyond ±2 SD - these exculusion are specific to each task (one participant may be excluded from declarative task (for being over ±2 SD), but its score for non-declarative memory may be used)
## those who failed to meet the general criteria of their treatment condition (i.e. falling asleep in MED)
## those who their number of arousals were beyond ±2 SD 
## in addition, I excluded LN7 from non-declarative because it was acting like an outlier in the regression line of the SWSpercent graph

MED$Median.Diff[abs(scale(MED$Median.Diff)) > 2]
MED$GMean[abs(scale(MED$GMean)) > 2]
WAKE$Median.Diff[abs(scale(WAKE$Median.Diff)) > 2]
WAKE$GMean[abs(scale(WAKE$GMean)) > 2]
NAP_SWS$Median.Diff[abs(scale(NAP_SWS$Median.Diff)) > 2]
NAP_noSWS$Median.Diff[abs(scale(NAP_noSWS$Median.Diff)) > 2]
NAP_SWS$GMean[abs(scale(NAP_SWS$GMean)) > 2]
NAP_noSWS$GMean[abs(scale(NAP_noSWS$GMean)) > 2]

#creating two separate datasets based on the memory task
RAW_declr <- RAW[-c(34,39,25,13,46),]
RAW_nondeclr <- RAW[-c(7,34,39,25,13,44,49,64),]

## DECLARATIVE MEMORY ----

#overall pattern of distribution
NAP_GMean <-
  data.frame(Condition = "NAP", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'NAP']))
MED_GMean <-
  data.frame(Condition = "MED", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'MED']))
WAKE_GMean <-
  data.frame(Condition = "WAKE", GeometricMean = c(RAW_declr$GMean[RAW_declr$Condition == 'WAKE']))
df_GMean <- rbind(NAP_GMean, MED_GMean, WAKE_GMean)

ggplot(df_GMean, aes(x = Condition, y = GeometricMean, color = Condition)) +
  geom_point(size = 4,
             alpha = 0.7,
             position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    shape = 23,
    color = "black",
    aes(fill = Condition),
    size = 4
  ) +
  stat_summary(
    fun.ymin = function(x)
      (mean(x) - sd(x)
      ),
    fun.ymax=function(x)(mean(x)+sd(x)),
    geom="errorbar", width=0.1) +
    theme_apa()

#Compare across all treatment conditions
fit_GMean <- rstanarm::stan_glm(GMean ~ Condition, data=RAW_declr)
results <- psycho::analyze(fit_GMean)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean, "Condition")
means <- psycho::get_means(fit_GMean, "Condition")

#plotting the bayesian comparison
ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("GMean") +
  xlab("Condition") +
  theme_apa()

# plotting Central posterior uncertainty intervals
posterior_GMean <- as.array(fit_GMean)
dim(posterior_GMean)
dimnames(posterior_GMean)
color_scheme_set("red")
mcmc_intervals(posterior_GMean, pars = c("(Intercept)", "ConditionNAP", "ConditionWAKE", "sigma"))

#for nappers - comparing the GMean for sws and non-sws
RAW_declr$SWS[RAW_declr$percentSWS == 0] <- "Present"
RAW_declr$SWS[RAW_declr$percentSWS != 0] <- "Absent"

RAW_NAP <- RAW_declr[1:24,]
fit_GMean_NAP <- rstanarm::stan_glm(GMean ~ SWS , data=RAW_NAP)
results <- psycho::analyze(fit_GMean_NAP)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean_NAP, "SWS")
means <- psycho::get_means(fit_GMean_NAP, "SWS")

#plotting the sws vs. nonsws performace in the declarative memory
ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Geometric Mean") +
  xlab("SWS type") +
  theme_apa()

## NON-DECLARATIVE MEMORY ----
#importing thier separate dataset - we are performing a repated-measures task
nonSWS_repeated <- rio::import("https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/Repeated%20measures/nonSWS_repeated.xlsx?raw=true",na = "N/A")

nonSWS_repeated <- nonSWS_repeated[-c(19,20,57,58,87,88),]

SWS_repeated_measures <- rio::import("https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/Repeated%20measures/SWS_repeated.xlsx?raw=true", 
                                     na = "N/A")
SWS_repeated <- SWS_repeated_measures[-c(11,12,65,66,95,96),]

# overall pattern of distribution - nothing significant
ggplot(RAW_nondeclr,
       aes(
         x = Condition,
         y = Median.Diff,
         color = Condition,
         label = Code
       )) +
  geom_point(size = 4,
             alpha = 0.7,
             position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    shape = 23,
    color = "black",
    aes(fill = Condition),
    size = 4
  ) +
  stat_summary(
    fun.ymin = function(x)
      (mean(x) - sd(x)
      ),
    fun.ymax=function(x)(mean(x)+sd(x)),
    geom="errorbar", width=0.1) +
    theme_apa()

#Compare across all treatment conditions

#with nonSWS nappers
fit_Bayes_RM_nonSWS <- rstanarm::stan_lmer(MedinScores ~ Condition + (1|Code), data=nonSWS_repeated)

fit.mean <- as.matrix(fit_Bayes_RM_nonSWS)[,1:3]
fit.mean[,2] <- fit.mean[,1] + fit.mean[,2]
fit.mean[,3] <- fit.mean[,1] + fit.mean[,3]


fit.pf <- cbind(colMeans(fit.mean), 
                posterior_interval(fit.mean))
fit.pf <- as.data.frame(fit.pf)
fit.pf <- fit.pf
names(fit.pf) <- c('Mean', 'Lower', 'Upper')
fit.pf$Condition = c('Meditation', 'Nap', 'Wake')

results <- psycho::analyze(fit_Bayes_RM_nonSWS)
summary(results, round = 2)
print(results)

ggplot(fit.pf, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  ylab("Median of Scores - non SWS") +
  xlab("Condition") +
  theme_apa()

# plotting Central posterior uncertainty intervals
posterior_Maze_nonSWS <- as.array(fit_Bayes_RM_nonSWS)
dim(posterior_Maze_nonSWS)
dimnames(posterior_Maze_nonSWS)
color_scheme_set("red")
mcmc_intervals(posterior_Maze_nonSWS, pars = c("(Intercept)", "ConditionNAP", "ConditionWAKE", "sigma"))


## with SWS
fit_Bayes_RM_SWS <- rstanarm::stan_lmer(MedinScores ~ Condition + (1|Code), data=SWS_repeated)

fit.mean <- as.matrix(fit_Bayes_RM_SWS)[,1:3]
fit.mean[,2] <- fit.mean[,1] + fit.mean[,2]
fit.mean[,3] <- fit.mean[,1] + fit.mean[,3]


fit.pf <- cbind(colMeans(fit.mean), 
                posterior_interval(fit.mean))
fit.pf <- as.data.frame(fit.pf)
fit.pf <- fit.pf
names(fit.pf) <- c('Mean', 'Lower', 'Upper')
fit.pf$Condition = c('Meditation', 'Nap', 'Wake')

results <- psycho::analyze(fit_Bayes_RM_SWS)
summary(results, round = 2)
print(results)

ggplot(fit.pf, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  ylab("Median of Scores - SWS") +
  xlab("Condition") +
  theme_apa()

# plotting Central posterior uncertainty intervals
posterior_Maze_SWS <- as.array(fit_Bayes_RM_SWS)
dim(posterior_Maze_SWS)
dimnames(posterior_Maze_SWS)
color_scheme_set("red")
plot_title_Maze_SWS <- ggtitle("Central posterior uncertainty intervals", "median and 50% intervals (the thick segments) and 90% intervals (the thinner outer lines)")
mcmc_intervals(posterior_Maze_SWS, pars = c("(Intercept)", "ConditionNAP", "ConditionWAKE", "sigma")) + plot_title_Maze_SWS

# plot of SWS percent and the time of sleep
RAW_nondeclr$percentSWS <- 100*RAW_nondeclr$percentSWS #to make them look like percent!
RAW_nondeclr$percentSWS <- ceiling(RAW_nondeclr$percentSWS) #keeping the integers only
RAW_nondeclr$total.sleep.time <- RAW_nondeclr$total.sleep.time/60 #converting them to minutes
RAW_nondeclr$total.sleep.time <- ceiling(RAW_nondeclr$total.sleep.time) #keeping the integers only
RAW_nondeclr$SWS[RAW_nondeclr$percentSWS == 0] <- "SWS Present"
RAW_nondeclr$SWS[RAW_nondeclr$percentSWS != 0] <- "SWS Absent"
NAP.performance.nosws <- filter(RAW_nondeclr, percentSWS == 0)
NAP.performance.sws <-  filter(RAW_nondeclr, percentSWS != 0)
df2.median <-rbind(NAP.performance.nosws, NAP.performance.sws)
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, colour = SWS )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "blue") + geom_smooth(method = lm, se= FALSE, fullrange=TRUE) + theme_apa() 


## EXPLORATORY ANALYSIS -----
### role of gender on the DECLARATIVE memory

RAW_declr$Binary_gender <- ifelse(RAW_declr$Gender == 'M', 1, 0) #create dummy variable for gender

fit_GMean_gender <- rstanarm::stan_glm(Binary_gender ~ GMean, data=RAW_declr, family = "binomial")

# Generate a new refgrid
refgrid <- RAW_declr %>% 
  dplyr::select(GMean) %>% 
  psycho::refdata(length.out=10)

# Get predictions and keep iterations
predicted_GMean <- psycho::get_predicted(fit_GMean_gender, newdata=refgrid, keep_iterations=TRUE)

# Reshape this dataframe to have iterations as factor
predicted_GMean <- predicted_GMean %>% 
  tidyr::gather(Iteration, Iteration_Value, starts_with("iter"))

# Plot all iterations with the median prediction
ggplot(predicted_GMean, aes(x=GMean)) +
  geom_line(aes(y=Iteration_Value, group=Iteration), size=0.3, alpha=0.01) +
  geom_line(aes(y=Binary_gender_Median), size=1) + 
  ylab("Probability of being a man\n") +
  theme_apa()
## role of gender on the NON-DECLARATIVE memory
# modeling the effect of gender
#non SWS
fit_Bayes_RM_nonSWS_gender <- rstanarm::stan_lmer(scale(MedinScores) ~ Condition * Gender + (1|Code), data=nonSWS_repeated, cores = 4, prior = normal(0,.5))
fit.mean <- as.matrix(fit_Bayes_RM_nonSWS_gender)[,1:3]
fit.mean[,2] <- fit.mean[,1] + fit.mean[,2]
fit.mean[,3] <- fit.mean[,1] + fit.mean[,3]


fit.pf <- cbind(colMeans(fit.mean), 
                posterior_interval(fit.mean))
fit.pf <- as.data.frame(fit.pf)
fit.pf <- fit.pf
names(fit.pf) <- c('Mean', 'Lower', 'Upper')
fit.pf$Condition = c('Meditation', 'Nap', 'Wake')

results <- psycho::analyze(fit_Bayes_RM_nonSWS_gender)
summary(results, round = 2)
print(results)

ggplot(fit.pf, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  ylab("Median of Scores - non SWS") +
  xlab("Condition") +
  theme_apa()

# plotting Central posterior uncertainty intervals
posterior_Maze_nonSWS_gender <- as.array(fit_Bayes_RM_nonSWS_gender)
dim(posterior_Maze_nonSWS_gender)
dimnames(posterior_Maze_nonSWS_gender)
color_scheme_set("red")
plot_title_Maze_nonSWS_gender <- ggtitle("Central posterior uncertainty intervals", "median and 50% intervals (the thick segments) and 90% intervals (the thinner outer lines)")
mcmc_intervals(posterior_Maze_nonSWS_gender, pars = c("(Intercept)", "ConditionNAP", "ConditionWAKE", "sigma")) + plot_title_Maze_nonSWS_gender

#SWS
fit_Bayes_RM_SWS_gender <- rstanarm::stan_lmer(MedinScores ~ Condition * Gender + (1|Code), data=SWS_repeated, cores = 4, prior = normal(0,.5))
fit.mean <- as.matrix(fit_Bayes_RM_SWS_gender)[,1:3]
fit.mean[,2] <- fit.mean[,1] + fit.mean[,2]
fit.mean[,3] <- fit.mean[,1] + fit.mean[,3]


fit.pf <- cbind(colMeans(fit.mean), 
                posterior_interval(fit.mean))
fit.pf <- as.data.frame(fit.pf)
fit.pf <- fit.pf
names(fit.pf) <- c('Mean', 'Lower', 'Upper')
fit.pf$Condition = c('Meditation', 'Nap', 'Wake')

results <- psycho::analyze(fit_Bayes_RM_SWS_gender)
summary(results, round = 2)
print(results)

ggplot(fit.pf, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  ylab("Median of Scores - SWS") +
  xlab("Condition") +
  theme_apa()

# plotting Central posterior uncertainty intervals
posterior_Maze_SWS_gender <- as.array(fit_Bayes_RM_SWS_gender)
dim(posterior_Maze_SWS_gender)
dimnames(posterior_Maze_SWS_gender)
color_scheme_set("red")
plot_title_Maze_SWS_gender <- ggtitle("Central posterior uncertainty intervals", "median and 50% intervals (the thick segments) and 90% intervals (the thinner outer lines)")
mcmc_intervals(posterior_Maze_SWS_gender, pars = c("(Intercept)", "ConditionNAP", "ConditionWAKE", "sigma")) +plot_title_Maze_SWS_gender

# Correlation of brain oscillations ----