## check the following lines: 110,111
#                             and why the results of the lines 123 and 130 are identical? they must arise from different data frames 
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rio","readxl", "tidyverse","devtools","car","lsr","dplyr","ggplot2","magrittr","Hmisc","psycho","lmerTest")
ipak(packages)
library(rio)
# importing the main excel file from Githul repository 
RAW_Jan_2019 <- rio::import('https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/RAW_data.xlsx?raw=true', na="N/A")
View(RAW_Jan_2019)
RAW <- RAW_Jan_2019  #should be changed everytime I update the main excel file
RAW<- RAW[-c(7,34,39,44),] #Removing outlier participants - LN7: too many arousals - MM4&MM5: they slept through meditation session - AM3: Hans advised me to remove it
#Subsetting the dataset based on the condition
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]

# Dummy codings/contrast codings ----
#Dummy variables for 'gender' and 'handedness' 
RAW$binary_gender <-
  ifelse(RAW$Gender == 'M', 1, 0) #create dummy variable for gender
RAW$binary_handedness <-
  ifelse(RAW$Gender == 'R', 1, 0) #create dummy variable for handedness

# Contrast coding based on the conditions and apriori hyp
RAW$NAPMEDvsWAKE <-
  ifelse(RAW$Condition == "MED",.33,
         ifelse(RAW$Condition == "NAP", .33,-.67))
RAW$NAPvsMED <-
  ifelse(RAW$Condition == "MED", .5, ifelse(RAW$Condition == "NAP",-.5, 0))
#centering those values to avoid problems of multicollinearity
RAW$cNAPMEDvsWAKE <- scale(RAW$NAPMEDvsWAKE, center = TRUE, scale = FALSE)[,]
RAW$cNAPvsMED <- scale(RAW$NAPvsMED, center = TRUE, scale = FALSE)[,]
## DECLARATIVE MEMORY task ##----
# Calculating True Postive Rate, True Negative Rate, BA, and Geometric Mean
RAW$TPR <-
  RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
RAW$TNR <-
  RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
#calculating the G-Mean
RAW$GMean <- sqrt(RAW$TPR * RAW$TNR)
#calculating Balanced Accuracy
RAW$BA <- 0.5 * (RAW$TPR + RAW$TNR)
# plotting  categorical scatter plot of G-Mean for each condition ----
NAP_GMean <-
  data.frame(Condition = "NAP", GeometricMean = c(RAW$GMean[RAW$Condition == 'NAP']))
MED_GMean <-
  data.frame(Condition = "MED", GeometricMean = c(RAW$GMean[RAW$Condition == 'MED']))
WAKE_GMean <-
  data.frame(Condition = "WAKE", GeometricMean = c(RAW$GMean[RAW$Condition == 'WAKE']))
df <- rbind(NAP_GMean, MED_GMean, WAKE_GMean)
library(ggplot2)
ggplot(df, aes(x = Condition, y = GeometricMean, color = Condition)) +
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
    geom="errorbar", width=0.1)
# plotting the BA scatter plot(for the sake of comparison with GMean) => no obvious difference. I'll stick with GMean
# NAP.BA <- data.frame(group="NAP", value=c(RAW$BA[1:15])) #change the values if new data is added
# MED.BA <- data.frame(group="MED", value=c(RAW$BA[16:28])) #change the values if new data is added
# WAKE.BA <- data.frame(group="WAKE", value=c(RAW$BA[29:38])) #change the values if new data is added
# df2<- rbind(NAP.BA,MED.BA,WAKE.BA)
# library(ggplot2)
# ggplot(df2, aes(x=group, y=value, color=group)) +
#   geom_point(size=4, alpha=0.7, position=position_jitter(w=0.1, h=0)) +
#   stat_summary(fun.y=mean, geom="point", shape=23, color="black", aes(fill=group), size=4) +
#   stat_summary(fun.ymin=function(x)(mean(x)-sd(x)),
#                fun.ymax=function(x)(mean(x)+sd(x)),
#                geom="errorbar", width=0.1)
#it's good to calculate BA (balanced accuracy) as well, and compare it to G-Mean for my own reference.
#No noticeable difference between BA and GMean was observed.
#look for those who napped and went SWS and look for covariates of sleep duration
# comparing Geometric mean with contrast-coding and ANOVA 
boxplot(GMean ~ Condition, data = RAW, main = "Geometric mean for word-pair associates across treatment conditions") #no outlier, but huge variation for nap condition. could be caused by sleep inertia.
qqnorm(RAW$GMean)
qqline(RAW$GMean) # there was not 'significant' departures from the line
shapiro.test(RAW$GMean)  #not significant, meaning that no violation of normality
#GMean_contrastcoding_aov <- lm((RAW$GMean) ~ cNAPMEDvsWAKE + cNAPvsMED, RAW)
# decided to use psycho package to interpret the linear mixed model
#Anova(GMean_contrastcoding_aov, type = 3) #unnecessary
# Calculating the Effect Size
library(lsr)
etaSquared(GMean_contrastcoding_aov, type = 3)
#pairwise-comparison with Holm correction
pairwise.t.test(RAW$GMean, RAW$Condition, p.adjust.method = "holm")
##how to write interaction plot? -- wait for the rest of Asvini's data, then run a full analysis
##for quantitative explanatory variables, which test is used to check for the homogeneity of variance??
# Running factorial ANOVA for GMean across all conditions ----
# I found lmerTest function
GMean_lm<- lm(GMean ~ 1 + as.factor(Condition), data = RAW)
#Running factorial ANOVA for GMean across all conditions
#Checking for assumptions
#outliers - ** so far, there is none - will keep that in case something showed up **
# boxplot(GMean ~ Condition, data = RAW, main = "GMean of Word Associates across treatment conditions")
# boxplot(
#   Median.Diff ~ Condition,
#   data = RAW,
#   main = "Median differences of Marble Maze Task across treatment conditions",
#   outline = FALSE
# ) # there was one outlier in MED
# Capped.med.diff <- RAW$Median.Diff
# qnt <- quantile(Capped.med.diff, probs = c(.25, .75), na.rm = T)
# caps <- quantile(Capped.med.diff, probs = c(.05, .95), na.rm = T)
# H <- 1.5 * IQR(Capped.med.diff, na.rm = T)
# Capped.med.diff[Capped.med.diff < (qnt[1] - H)] <- caps[1]
# Capped.med.diff[Capped.med.diff > (qnt[2] + H)] <- caps[2]
# RAW$Capped.Median.diff <-
#   Capped.med.diff  ### use this variable for your analysis instread of RAW$Median.Diff
# checking for Normality
qqnorm(RAW$GMean)
qqline(RAW$GMean) # there was not 'significant' departures from the line
shapiro.test(RAW$GMean) #failed to reject the hypothesis that the sample comes from a normal distribution. So, no violation of normality
#Running ANOVA for GMean
RAW$Cond.factor <-
  factor(RAW$Condition, labels = c("NAP", "MED", "WAKE"))
library(car)
anova1 <- lm(RAW$GMean ~ RAW$Condition)
Anova(anova1, type = 3)
# Calculating the Effect Size
library(lsr)
etaSquared(anova1, type = 3)
# checking for homogeneity of variance
leveneTest(anova1) #Brown-Forsyth Test
leveneTest(anova1, center = mean) #Levene's Test  #there was no violation of homogeneity of variance assumption

# other explorartory analyses - main effect of gender or handedness -----
# it should be noted that since the gender is not balanced in my study, there are very few men in some conditions
#main effect of gender
menonly <- filter(RAW, Gender == "M")
femaleonly <- filter(RAW, Gender == "F")
#main effect of handedness
righthanded <-
  filter(RAW, Handedness == "R") # I did not include ambidextrous participants
lefthanded <- filter(RAW, Handedness == "L")
#I will check that for both contrast-coding and without contrast-coding. I should later check which one is more apporopriate
Men_GMean_contrastcoding_aov <-
  lm((menonly$GMean) ~ menonly$cNAPMEDvsWAKE + menonly$cNAPvsMED, menonly) #with contrast-coding
Anova(Men_GMean_contrastcoding_aov, type = 3)
Men_GMean_without_contrastcoding_aov <-
  lm(menonly$GMean ~ menonly$Condition, menonly) #without contrast-coding
Anova(Men_GMean_without_contrastcoding_aov, type = 3)
#checking for simple main effects of Females
Fem_GMean_contrastcoding_aov <-
  lm((femaleonly$GMean) ~ femaleonly$cNAPMEDvsWAKE + femaleonly$cNAPvsMED, femaleonly) #with contrast-coding
Anova(Fem_GMean_contrastcoding_aov, type = 3)
Fem_GMean_without_contrastcoding_aov <-
  lm(femaleonly$GMean ~ femaleonly$Condition, femaleonly) #without contrast-coding
Anova(Fem_GMean_without_contrastcoding_aov, type = 3)
# I should check them again, when I got the full records of Asvini
# I might need to check for interactions. Read about them

## NON-DECLARATIVE task ##
RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median` #new column for the difference of medians
RAW$percentSWS <- 100*RAW$percentSWS #to make them look like percent!
RAW$percentSWS <- ceiling(RAW$percentSWS) #keeping the integers only
RAW$total.sleep.time <- RAW$total.sleep.time/60 #converting them to minutes
RAW$total.sleep.time <- ceiling(RAW$total.sleep.time) #keeping the integers only
RAW$median.pre <-RAW$`Trial 91-100 median`
RAW$median.post <- RAW$`Median 1-10`

library(ggplot2) #This plot labels the percent SWS over data-points. Not pretty but helps us to detect outlier
ggplot(RAW,
       aes(
         x = Condition,
         y = Median.Diff,
         color = Condition,
         label = Code
       )) +
  geom_point(size = 4,
             alpha = 0.7,
             position = position_jitter(w = 0.1, h = 0)) +
  geom_text(aes(label = ifelse(Median.Diff > 2.5 | Median.Diff < -2.5  , as.character(Code), '')), hjust = 0, vjust = 0) +
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
    geom="errorbar", width=0.1)

library(ggplot2) #looking at the percent SWS
median.plot <- ggplot(RAW,
                      aes(
                        x = Condition,
                        y = Median.Diff,
                        color = Condition,
                        label = percentSWS
                      )) +
  geom_point(size = 4,
             alpha = 0.7,
             position = position_jitter(w = 0.1, h = 0))
median.plot + geom_label() +
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
    geom="errorbar", width=0.1)

library(ggplot2) #checking for total sleep time
median.plot <- ggplot(RAW,
                      aes(
                        x = Condition,
                        y = Median.Diff,
                        color = Condition,
                        label = total.sleep.time
                      )) +
  geom_point(size = 4,
             alpha = 0.7,
             position = position_jitter(w = 0.1, h = 0))
median.plot + geom_label() +
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
    geom="errorbar", width=0.1)

library(dplyr)
RAW$swstype[RAW$percentSWS == 0] <- "zero"
RAW$swstype[RAW$percentSWS != 0] <- "non-zero"
NAP.performance.nosws <- filter(RAW, percentSWS == 0)
NAP.performance.sws <-  filter(RAW, percentSWS != 0)
df2.median <-rbind(NAP.performance.nosws, NAP.performance.sws)
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "red") #PercentSWSgraph - Black and White
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = Code )) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(label = paste(percentSWS, "^(", Code, ")", sep = "")), parse = TRUE,position = position_jitter(w = 0.3, h = 0.1)) #PercentSWSCode along each data point 
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = percentSWS )) + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(colour = percentSWS), size = 5)  #PercentSWSnoCode - with gradient color indicating the percentage of SWS
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = percentSWS )) + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(colour = percentSWS), size = 5) + geom_smooth(method = lm) #PercentSWSnoCode - looking at the overall regression line, without the separation of SWS and non-SWS
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, colour = swstype )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "blue") + geom_smooth(method = lm, se= FALSE) #+ theme_apa() #PercentSWSgraph with regression lines}}

# Calculating with Bayesian Framework ----
# Across all conditions - comparing the GMean 
RAW$TPR <-
  RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
RAW$TNR <-
  RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
#calculating the G-Mean
RAW$GMean <- sqrt(RAW$TPR * RAW$TNR)
fit_GMean <- rstanarm::stan_glm(GMean ~ Condition, data=RAW)
results <- psycho::analyze(fit_GMean)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean, "Condition")
means <- psycho::get_means(fit_GMean, "Condition")

ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("GMean") +
  xlab("Condition")
-----------
  #for nappers - comparing the GMean for sws and non-sws
  RAW_for_NAP <- RAW[1:24,]
fit_GMean_NAP <- rstanarm::stan_glm(GMean ~ swstype , data=RAW_for_NAP)
results <- psycho::analyze(fit_GMean_NAP)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean_NAP, "swstype")
means <- psycho::get_means(fit_GMean_NAP, "swstype")

ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("GMean") +
  xlab("swstype")
-------------
  #for nappers - comparing the median of differences in maze task for sws and non-sws
  RAW_for_NAP <- RAW[1:24,]
fit_Maze_NAP <- rstanarm::stan_glm(Median.Diff ~ swstype , data=RAW_for_NAP)
results <- psycho::analyze(fit_Maze_NAP)
print(results)
contrasts <- psycho::get_contrasts(fit_Maze_NAP, "swstype")
means <- psycho::get_means(fit_Maze_NAP, "swstype")


ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Median.Diff") +
  xlab("swstype")  

# Calculating repeated-measures ANOVA based on swstype ----
# for Motor-task:
#make sure to check for those outliers that I have selected above. These excel files may still contain those.
library(readxl)

nonSWS_repeated <- rio::import("https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/Repeated%20measures/nonSWS_repeated.xlsx?raw=true",na = "N/A")
View(nonSWS_repeated)

SWS_repeated_measures <- rio::import("https://github.com/mohdasti/Queens-Thesis/blob/master/Raw%20data/Repeated%20measures/SWS_repeated.xlsx?raw=true", 
                                     na = "N/A")
View(SWS_repeated_measures)
# repeated measures ANOVA for nonSWS nappers and other conditions for maze task
library(psycho)
library(rstanarm)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(Matrix)
library(lmerTest)

## note that these ARE NOT bayesian inferences

fit_RM_nonSWS <- lmer(MedinScores ~ Condition + (1|Code), data=nonSWS_repeated)
anova(fit_RM_nonSWS)
print(analyze(fit_RM_nonSWS))

results_RM_nonSWS <- get_contrasts(fit_RM_nonSWS, "Condition")
contrasts_RM_nonSWS <- psycho::get_contrasts(fit_RM_nonSWS, "Condition")
means_RM_nonSWS <- psycho::get_means(fit_RM_nonSWS, "Condition")
print(contrasts_RM_nonSWS)
print(means_RM_nonSWS)

ggplot(means_RM_nonSWS, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Median of Scores - nonSWS") +
  xlab(" Condition") +
  theme_bw()
##
fit_RM_SWS <- lmer(MedinScores ~ Condition + (1|Code), data=SWS_repeated_measures)
anova(fit_RM_SWS)
print(analyze(fit_RM_SWS))

results_RM_SWS <- psycho::get_contrasts(fit_RM_SWS, "Condition")
means_RM_SWS <- psycho::get_means(fit_RM_SWS, "Condition")
contrasts_RM_SWS <- psycho::get_contrasts(fit_RM_SWS, "Condition")
print(contrasts_RM_SWS)
print(means_RM_SWS)

ggplot(means_RM_SWS, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Median of Scores - SWS") +
  xlab(" Condition") +
  theme_bw()

#now, running them in Bayesian
#for nonSWS
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
ggplot(fit.pf, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) +
  ylab("Median of Scores - SWS") +
  xlab(" Condition") +
  theme_bw()


results <- psycho::analyze(fit_Bayes_RM_nonSWS)
summary(results, round = 2)
print(results)

#now, running additional factors.
# checking for the main effects of gender or handedness:
#Gender
#Gender for nonSWS
fit_RM_nonSWS_Sex <- lmer(MedinScores ~ Condition * Gender + (1|Code), data=nonSWS_repeated)
anova(fit_RM_nonSWS_Sex)
print(analyze(fit_RM_nonSWS_Sex))

results_Sex <- get_contrasts(fit_RM_nonSWS_Sex, "Condition * Gender")
print(results_Sex$means)

ggplot(results_Sex$means, aes(x=Condition, y=Mean, color=Gender, group=Gender)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  position = position_dodge(.3)) +
  ylab("Median of Scores - nonSWS") +
  xlab("Condition") +
  theme_bw()
#Gender for SWS
fit_RM_SWS_Sex <- lmer(MedinScores ~ Condition * Gender + (1|Code), data=SWS_repeated_measures)
anova(fit_RM_SWS_Sex)
print(analyze(fit_RM_SWS_Sex))

results_Sex <- get_contrasts(fit_RM_SWS_Sex, "Condition * Gender")
print(results_Sex$means)

ggplot(results_Sex$means, aes(x=Condition, y=Mean, color=Gender, group=Gender)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  position = position_dodge(.3)) +
  ylab("Median of Scores - nonSWS") +
  xlab("Condition") +
  theme_bw()
####
#Handedness
#Handedness for nonSWS
fit_RM_nonSWS_Hand <- lmer(MedinScores ~ Condition * Handedness + (1|Code), data=nonSWS_repeated)
anova(fit_RM_nonSWS_Hand)
print(analyze(fit_RM_nonSWS_Hand))

results_Hand <- get_contrasts(fit_RM_nonSWS_Hand, "Condition * Handedness")
print(results_Hand$means)

ggplot(results_Hand$means, aes(x=Condition, y=Mean, color=Handedness, group=Handedness)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  position = position_dodge(.3)) +
  ylab("Median of Scores - nonSWS") +
  xlab("Condition") +
  theme_bw()
#Handedness for SWS
fit_RM_SWS_Hand <- lmer(MedinScores ~ Condition * Handedness + (1|Code), data=SWS_repeated_measures)
anova(fit_RM_SWS_Hand)
print(analyze(fit_RM_SWS_Hand))

results_Hand <- get_contrasts(fit_RM_SWS_Hand, "Condition * Handedness")
print(results_Hand$means)

ggplot(results_Hand$means, aes(x=Condition, y=Mean, color=Handedness, group=Handedness)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  position = position_dodge(.3)) +
  ylab("Median of Scores - nonSWS") +
  xlab("Condition") +
  theme_bw()
####
# FMI or Age... I think it does not make sense to calculate those. 
# even, I think there is no particular advantage in doing "Repeated-Measures ANOVA". They are significant, so I can 'Median.Diff'
# Calculaitng logistic regressions of gender and Handedness ----
RAW_noSWS_and_MED_WAKE$binary.gender <-
  ifelse(RAW_noSWS_and_MED_WAKE$Gender == 'M', 1, 0) #create dummy variable for gender

fit_LogReg_Sex <- rstanarm::stan_glm(binary.gender ~ Median.Diff, data= RAW_noSWS_and_MED_WAKE, family = "binomial")
results_LogReg_Sex <- psycho::analyze(fit_LogReg_Sex)
summary(results_LogReg_Sex, round = 2)

refgrid <- RAW_noSWS_and_MED_WAKE %>% 
  select(Median.Diff) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit_LogReg_Sex, newdata=refgrid)

ggplot(predicted, aes(x=Median.Diff, y=binary.gender_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=binary.gender_CI_5, 
                  ymax=binary.gender_CI_95), 
              alpha=0.1) +
  ylab("Probability of being a male")
#running logistic regression for handedness
RAW_noSWS_and_MED_WAKE$binary.handedness <-
  ifelse(RAW_noSWS_and_MED_WAKE$Handedness == 'R', 1, 0) #create dummy variable for handedness

fit_LogReg_Hand <- rstanarm::stan_glm(binary.handedness ~ Median.Diff, data= RAW_noSWS_and_MED_WAKE, family = "binomial")
results_LogReg_Hand <- psycho::analyze(fit_LogReg_Hand)
summary(results_LogReg_Hand, round = 2)

refgrid <- RAW_noSWS_and_MED_WAKE %>% 
  select(Median.Diff) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit_LogReg_Hand, newdata=refgrid)

ggplot(predicted, aes(x=Median.Diff, y=binary.handedness_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=binary.handedness_CI_5, 
                  ymax=binary.handedness_CI_95), 
              alpha=0.1) +
  ylab("Probability of being Right Handed")
# checking the effect of gender of GMean - here, there is no need to look at non-SWS specifically
RAW.without.LN7$binary.gender <-
  ifelse(RAW.without.LN7$Gender == 'M', 1, 0) #create dummy variable for gender

fit_LogReg_GMean_Sex <- rstanarm::stan_glm(binary.gender ~ GMean, data= RAW.without.LN7, family = "binomial")
results_LogReg_GMean_Sex <- psycho::analyze(fit_LogReg_GMean_Sex)
summary(results_LogReg_GMean_Sex, round = 2)

refgrid <- RAW.without.LN7 %>% 
  select(GMean) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit_LogReg_GMean_Sex, newdata=refgrid)

ggplot(predicted, aes(x=GMean, y=binary.gender_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=binary.gender_CI_5, 
                  ymax=binary.gender_CI_95), 
              alpha=0.1) +
  ylab("Probability of being a male") # it was interesting!
# Calculating the Moderated Regression ----
# FMI
# for Motor-task:
other.than.NAP <- filter(RAW.without.LN7, Condition != 'NAP' )
RAW_noSWS_and_MED_WAKE <- rbind(NAP.performance.nosws,other.than.NAP)

fit_MODREG_FMI <- lm(Median.Diff ~ Condition + FMI + Condition*FMI, data = RAW_noSWS_and_MED_WAKE)
summary(fit_MODREG_FMI)
library(QuantPsyc)
sim.slopes(fit_MODREG_FMI, RAW_noSWS_and_MED_WAKE$FMI)
# calculating the linear regression of sws percentage and median difference ----
fit_Maze_linear_reg <- rstanarm::stan_glm(Median.Diff ~ percentSWS, data = NAP.performance.sws)
results <- psycho::analyze(fit_Maze_linear_reg)
summary(results, round = 2)
print(results)
refgrid <- NAP.performance.sws %>% 
  select(percentSWS) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit_Maze_linear_reg, newdata=refgrid)

ggplot(predicted, aes(x=percentSWS, y=Median.Diff_Median)) +
  geom_line() + 
  geom_ribbon(aes(ymin=Median.Diff_CI_5, 
                  ymax=Median.Diff_CI_95), 
              alpha=0.1)
#plotting the posterior distribution for percent SWS
posterior <- as.matrix(fit_Maze_linear_reg)
plot_title <- ggtitle("Posterior distributions", "median and 80% intervals")
mcmc_areas(posterior, pars = c("percentSWS"),prob = .8)+plot_title
# Diagnosing convergence with traceplot
mcmc_trace(posterior,pars = c("percentSWS"))
#pairs plot for determining if we have any highly correlated parameters.
posterior_chains <- as.array(fit_Maze_linear_reg)
pairs <- posterior_chains %>%
  mcmc_pairs(pars = c("(Intercept)","percentSWS"))
#posterior predictive density
library("rstanarm")
ppd <- posterior_predict(fit_Maze_linear_reg, draws= 500)
ppd %>% ppc_dens_overlay(y= fit_Maze_linear_reg$y, yrep = .)
#-
ppd %>%
  ppc_stat_grouped(y=NAP.performance.sws$percentSWS, group = NAP.performance.sws$Median.Diff,
                   stat = "median", binwidth = .5)

#misc. ----
# to cite any package
#citation(package = "dplyr") 
#just change the name of the package in the ""

# Sleepiness----------
# A new column for difference in pre-post Epworth scale for further use
RAW$ESS_diff <- (RAW$ESS_Post - RAW$ESS_Pre)
#filtering napper based on their sleep profile into a separate dataframe for further use
SWS_nappers <- filter(RAW, percentSWS != 0.0) #nappers who showed SWS
#running repeated-measures t-test on ESS within-subject in different conditions
MED_ESSpre <- c(MED$ESS_Pre) 
MED_ESSpost <- c(MED$ESS_Post)
t.test(MED_ESSpre, MED_ESSpost, paired = TRUE)
NAP_ESSpre <- c(NAP$ESS_Pre) 
NAP_ESSpost <- c(NAP$ESS_Post)
t.test(NAP_ESSpre, NAP_ESSpost, paired = TRUE)
WAKE_ESSpre <- c(WAKE$ESS_Pre) 
WAKE_ESSpost <- c(WAKE$ESS_Post)
t.test(WAKE_ESSpre, WAKE_ESSpost, paired = TRUE)
#running ANOVA on ESS across all conditions
boxplot(ESS_diff ~ Condition, data = RAW, main = "Epworth pre-post difference across treatment conditions") 
#no outlier, but huge variation for nap condition. could be caused by sleep inertia.
qqnorm(RAW$ESS_diff)
qqline(RAW$ESS_diff) # there was not 'significant' departures from the line
shapiro.test(RAW$ESS_diff) #not significant, meaning that no violation of normality
library(car)
anova_ESS_diff <- lm(RAW$ESS_diff ~ RAW$Condition)
Anova(anova_ESS_diff, type = 3) #it was marginally significant
# Calculating the Effect Size
library(lsr)
etaSquared(anova_ESS_diff, type = 3)
#pairwise-comparison with Holm correction
pairwise.t.test(RAW$ESS_diff, RAW$Condition, p.adjust.method = "holm")
# checking for homogeneity of variance
leveneTest(anova_ESS_diff) #Brown-Forsyth Test
leveneTest(anova_ESS_diff, center = mean) #Levene's Test
## end of Epworth analysis


