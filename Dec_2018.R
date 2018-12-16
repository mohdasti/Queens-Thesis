<<<<<<< HEAD
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rio","readxl", "tidyverse","devtools","car","lsr","dplyr","ggplot2","magrittr")
ipak(packages)
#my_packages <- c("rio","readxl", "tidyverse","devtools","car","lsr","dplyr","ggplot2","magrittr")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
=======
my_packages <- c("rio","readxl", "tidyverse","devtools","car","lsr","dplyr","ggplot2","magrittr")
install.packages(my_packages, repos = "http://cran.rstudio.com")
>>>>>>> cc602b81e1256058cbffdf2a047a6a7e29e22fe9
library(rio)
RAW_Dec_2018 <- rio::import('https://github.com/mohdasti/Queens-Thesis/blob/bff30a97de4611da7172295f0b25c090770e41ab/Oct%202018.xlsx?raw=true', na="N/A")
View(RAW_Dec_2018)
RAW <- RAW_Dec_2018  #should be changed everytime I update the main excel file
#RAW <- RAW[-c(7),] #Removing one participant - LN7
# Subsetting based on the condition ---------------------------------------
#subsetting the main dataset into three treatment conditions (if needed)
##IMPORTANT: needs to be changed everytime we update the main RAW file##
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]
# summarytools packages ====
#install.packages("summarytools") #simply summarizng my dataset (if needed)
#library(summarytools)
#view(dfSummary(RAW))  # in general, but it's more helpful to see that within each treatment condition
#view(dfSummary(MED))
#view(dfSummary(NAP))
#view(dfSummary(WAKE))
# Sleepiness----------
#adding a new column for difference in pre-post Epworth scale for further use
RAW$ESSdifference <- (RAW$`ESS Post` - RAW$`ESS Pre`)
#running repeated-measures t-test on ESS within-subject in different conditions
MED.ESSpre <- c(MED$`ESS Pre`) #meditators
MED.ESSpost <- c(MED$`ESS Post`)
t.test(MED.ESSpre, MED.ESSpost, paired = TRUE)
NAP.ESSpre <- c(NAP$`ESS Pre`) #nappers
NAP.ESSpost <- c(NAP$`ESS Post`)
t.test(NAP.ESSpre, NAP.ESSpost, paired = TRUE)
WAKE.ESSpre <- c(WAKE$`ESS Pre`) #wake
WAKE.ESSpost <- c(WAKE$`ESS Post`)
t.test(WAKE.ESSpre, WAKE.ESSpost, paired = TRUE)
#running ANOVA on ESS across all conditions
boxplot(ESSdifference ~ Condition, data = RAW, main = "Epworth pre-post difference across treatment conditions") #no outlier, but huge variation for nap condition. could be caused by sleep inertia.
qqnorm(RAW$ESSdifference)
qqline(RAW$ESSdifference) # there was not 'significant' departures from the line
shapiro.test(RAW$ESSdifference) #not significant, meaning that no violation of normality
RAW$Cond.factor <-
  factor(RAW$Condition, labels = c("NAP", "MED", "WAKE"))
library(car)
anova1 <- lm(RAW$ESSdifference ~ RAW$Condition)
Anova(anova1, type = 3) #it was significant
# Calculating the Effect Size
library(lsr)
etaSquared(anova1, type = 3)
#pairwise-comparison with Holm correction
pairwise.t.test(RAW$ESSdifference, RAW$Condition, p.adjust.method = "holm")
# checking for homogeneity of variance
leveneTest(anova1) #Brown-Forsyth Test
leveneTest(anova1, center = mean) #Levene's Test
#filtering napper based on their sleep profile
library(dplyr)
SWS_nappers <- filter(RAW, `percent SWS` != 0.0) #nappers who showed SWS
# Dummy variables for gender and handedness ----
RAW$binary.gender <-
  ifelse(RAW$Gender == 'M', 1, 0) #create dummy variable for gender
RAW$binary.handedness <-
  ifelse(RAW$Gender == 'R', 1, 0) #create dummy variable for handedness
# Contrast coding based on the conditions and apriori hyp. ----
RAW$cNAPMEDvsWAKE <-
  ifelse(RAW$Condition == "MED",
         .33,
         ifelse(RAW$Condition == "NAP", .33,-.67))
RAW$cNAPvsMED <-
  ifelse(RAW$Condition == "MED", .5, ifelse(RAW$Condition == "NAP",-.5, 0))
# Calculating True Postive Rate, True Negative Rate, BA, and Geometric Mean----
RAW$TPR <-
  RAW$`Hit ratio` / (RAW$`Hit ratio` + RAW$`False Alarm ratio`)
RAW$TNR <-
  RAW$`Correct Rejection ratio` / (RAW$`Correct Rejection ratio` + RAW$`Miss ratio`)
#calculating the G-Mean
RAW$GMean <- sqrt(RAW$TPR * RAW$TNR)
#calculating Balanced Accuracy
RAW$BA <- 0.5 * (RAW$TPR + RAW$TNR)
# plotting  categorical scatter plot of G-Mean for each condition ----
NAP.GMean <-
  data.frame(Condition = "NAP", GeometricMean = c(RAW$GMean[RAW$Condition == 'NAP']))
MED.GMean <-
  data.frame(Condition = "MED", GeometricMean = c(RAW$GMean[RAW$Condition == 'MED']))
WAKE.GMean <-
  data.frame(Condition = "WAKE", GeometricMean = c(RAW$GMean[RAW$Condition == 'WAKE']))
df <- rbind(NAP.GMean, MED.GMean, WAKE.GMean)
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
#plotting the BA scatter plot(for the sake of comparison with GMean) => no obvious difference. I'll stick with GMean
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
# comparing Geometric mean with contrast-coding and ANOVA ----
boxplot(GMean ~ Condition, data = RAW, main = "Geometric mean for word-pair associates across treatment conditions") #no outlier, but huge variation for nap condition. could be caused by sleep inertia.
qqnorm(RAW$GMean)
qqline(RAW$GMean) # there was not 'significant' departures from the line
shapiro.test(RAW$GMean)
RAW$cNAPMEDvsWAKE <-
  ifelse(RAW$Condition == "MED",.33,
         ifelse(RAW$Condition == "NAP", .33,-.67))
RAW$cNAPvsMED <-
  ifelse(RAW$Condition == "MED", .5, ifelse(RAW$Condition == "NAP",-.5, 0))
GMean.contrastcoding.aov <-
  lm((RAW$GMean) ~ cNAPMEDvsWAKE + cNAPvsMED, RAW)
Anova(GMean.contrastcoding.aov, type = 3)
#for quantitative explanatory variables, which test is used to check for the homogeneity of variance??
# other explorartory analyses - main effect of gender or handedness -----
#main effect of gender
library(dplyr)
menonly <- filter(RAW, Gender == "M")
femaleonly <- filter(RAW, Gender == "F")
#main effect of handedness
righthanded <-
  filter(RAW, Handedness == "R") # I did not include ambidextrous participants
lefthanded <- filter(RAW, Handedness == "L")
#I will check that for both contrast-coding and without contrast-coding. I should later check which one is more apporopriate
Men.GMean.contrastcoding.aov <-
  lm((RAW$GMean) ~ RAW$cNAPMEDvsWAKE + RAW$cNAPvsMED, menonly) #with contrast-coding
Anova(Men.GMean.contrastcoding.aov, type = 3)
Men.GMean.contrastcoding.aov <-
  lm(RAW$GMean ~ RAW$Condition, menonly) #without contrast-coding
Anova(Men.GMean.contrastcoding.aov, type = 3)
#checking for simple main effects of Females
Fem.GMean.contrastcoding.aov <-
  lm((RAW$GMean) ~ RAW$cNAPMEDvsWAKE + RAW$cNAPvsMED, menonly) #with contrast-coding
Anova(Fem.GMean.contrastcoding.aov, type = 3)
Fem.GMean.contrastcoding.aov <-
  lm(RAW$GMean ~ RAW$Condition, menonly) #without contrast-coding
Anova(Fem.GMean.contrastcoding.aov, type = 3)
# Running factorial ANOVA for GMean across all conditions ----
RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median`
#Running factorial ANOVA for GMean across all conditions
#Checking for assumptions
#outliers
boxplot(GMean ~ Condition, data = RAW, main = "GMean of Word Associates across treatment conditions")
boxplot(
  Median.Diff ~ Condition,
  data = RAW,
  main = "Median differences of Marble Maze Task across treatment conditions",
  outline = FALSE
) # there was one outlier in MED
Capped.med.diff <- RAW$Median.Diff
qnt <- quantile(Capped.med.diff, probs = c(.25, .75), na.rm = T)
caps <- quantile(Capped.med.diff, probs = c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Capped.med.diff, na.rm = T)
Capped.med.diff[Capped.med.diff < (qnt[1] - H)] <- caps[1]
Capped.med.diff[Capped.med.diff > (qnt[2] + H)] <- caps[2]
RAW$Capped.Median.diff <-
  Capped.med.diff  ### use this variable for your analysis instread of RAW$Median.Diff
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
## running factorial anova

# running Signal Detection Theory Indices on Paired Association Task (PAL)----
# #we need Hit, Correct Rejection, Miss, and False Alarm
# #install.packages("psycho")
# #library(psycho)
# #install.packages("dplyr")
# library(dplyr)
# install.packages("tidyr")
# library(tidyr)
# #for meditation:
# MED.PAL <- data.frame(Participant = c(1:13), #be careful when you add more observations, these numbers also changes
#                       n_hit = c(MED$`Hit ratio`),
#                       n_fa = c(MED$`False Alarm ratio`),
#                       n_miss = c(MED$`Miss ratio`),
#                       n_cr = c(MED$`Correct Rejection ratio`))
#
# indices <- psycho::dprime(MED.PAL$n_hit, MED.PAL$n_fa, MED.PAL$n_miss, MED.PAL$n_cr)
# MED.PAL <- cbind(MED.PAL, indices)
# #for nap:
# NAP.PAL <- data.frame(Participant = c(1:15),
#                       n_hit = c(NAP$`Hit ratio`),
#                       n_fa = c(NAP$`False Alarm ratio`),
#                       n_miss = c(NAP$`Miss ratio`),
#                       n_cr = c(NAP$`Correct Rejection ratio`))
#
# indices <- psycho::dprime(NAP.PAL$n_hit, NAP.PAL$n_fa, NAP.PAL$n_miss, NAP.PAL$n_cr)
# NAP.PAL <- cbind(NAP.PAL, indices)
# #for wake:
# WAKE.PAL <- data.frame(Participant = c(1:10),
#                        n_hit = c(WAKE$`Hit ratio`),
#                        n_fa = c(WAKE$`False Alarm ratio`),
#                        n_miss = c(WAKE$`Miss ratio`),
#                        n_cr = c(WAKE$`Correct Rejection ratio`))
#
# indices <- psycho::dprime(WAKE.PAL$n_hit, WAKE.PAL$n_fa, WAKE.PAL$n_miss, WAKE.PAL$n_cr)
# WAKE.PAL <- cbind(WAKE.PAL, indices)
# #by looking at those numbers, we can find possible outliers(?)
# #average of meditators' responses:
# MED_PALsum <- select(MED.PAL, Participant, dprime, c) %>%  # Select these variables only
#   gather(parameter, value, -Participant) %>%  # Convert data to long format
#   group_by(parameter) %>%  # Prepare to summarise on these grouping variables
#   # Calculate summary statistics for grouping variablees
#   summarise(n=n(), mu=mean(value), sd=sd(value), se=sd/sqrt(n))
# MED_PALsum
# #average of nappers' responses:
# NAP_PALsum <- select(NAP.PAL, Participant, dprime, c) %>%  # Select these variables only
#   gather(parameter, value, -Participant) %>%  # Convert data to long format
#   group_by(parameter) %>%  # Prepare to summarise on these grouping variables
#   # Calculate summary statistics for grouping variables
#   summarise(n=n(), mu=mean(value), sd=sd(value), se=sd/sqrt(n))
# NAP_PALsum
# #average of wakes' responses:
# WAKE_PALsum <- select(WAKE.PAL, Participant, dprime, c) %>%  # Select these variables only
#   gather(parameter, value, -Participant) %>%  # Convert data to long format
#   group_by(parameter) %>%  # Prepare to summarise on these grouping variables
#   # Calculate summary statistics for grouping variables
#   summarise(n=n(), mu=mean(value), sd=sd(value), se=sd/sqrt(n))
# WAKE_PALsum
## ## ## 
# I decided to use Matthews correlation coefficient (MCC) to compare the performance of participants on paired associate task
# I'm not 100% sure, but I think mcc is able to be used as a sole indicator of the overall performance
###Update###: What we have, is an 'imbalanced class problem', therefore, we need metrics that are non-sensitive to class imbalance. Refer to Straube & Krell paper.
## we need to calculate the True Positive Rate, as well as True Negative Rate, to see how apart they are from each other.
#then, I think I need to compute both G-Mean and d'

# mcc <- function (actual, predicted)
# {
#   # actual = vector of true outcomes, 1 = Positive, 0 = Negative
#   # predicted = vector of predicted outcomes, 1 = Positive, 0 = Negative
#   # function returns MCC
#
#   #TP <- sum(actual == 1 & predicted == 1)
#   #TN <- sum(actual == 0 & predicted == 0)
#   #FP <- sum(actual == 0 & predicted == 1)
#   #FN <- sum(actual == 1 & predicted == 0)
#   #TP;TN;FP;FN # for debugging
#   sum1 <- RAW$`hits (/20)`+RAW$`false alarms (/40)`; sum2 <-RAW$`hits (/20)`+(20-RAW$`hits (/20)`) ; sum3 <-(40-RAW$`false alarms (/40)`)+RAW$`false alarms (/40)` ; sum4 <- (40-RAW$`false alarms (/40)`)+(20-RAW$`hits (/20)`);
#   denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
#   if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
#     denom <- 1
#   }
#   mcc <- ((RAW$`hits (/20)`*(40-RAW$`false alarms (/40)`))-(RAW$`false alarms (/40)`*(20-RAW$`hits (/20)`))) / sqrt(denom)
#   return(mcc)
# }
# RAW$Matthews.coeff <- mcc(RAW) #calculating a separate mcc value for every participant
## # ##
#categorical scatter plot of the mcc values for each condition
#NAP.mcc <- data.frame(group="NAP", value=c(RAW$Matthews.coeff[1:15])) #change the values if new data is added
#MED.mcc <- data.frame(group="MED", value=c(RAW$Matthews.coeff[16:28])) #change the values if new data is added
#WAKE.mcc <- data.frame(group="WAKE", value=c(RAW$Matthews.coeff[29:38])) #change the values if new data is added
#df<- rbind(NAP.mcc,MED.mcc,WAKE.mcc)
#library(ggplot2)
#ggplot(df, aes(x=group, y=value, color=group)) +
#  geom_point(size=4, alpha=0.7, position=position_jitter(w=0.1, h=0)) +
#  stat_summary(fun.y=mean, geom="point", shape=23, color="black", aes(fill=group), size=4) +
#  stat_summary(fun.ymin=function(x)(mean(x)-sd(x)),
#              fun.ymax=function(x)(mean(x)+sd(x)),
#               geom="errorbar", width=0.1)
#+theme_bw() #if I wanted to remove the gray background of the plot
#I used this scatter to simply visaulize the overall performance of each condition.
#Apparently, NAP > MED > WAKE; however, their median difference is not huge and there is a huge variability in NAP (we might need to find some outliers)
##UPDATE##it is needed to plot similar graphs for G-Mean and d'
# Plotting the difference of medians across all conditions ----
#this plot indicates the participant codes of whom who had + 2.5 or -2.5
RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median`
RAW$percentSWS <- 100*RAW$percentSWS
RAW$percentSWS <- ceiling(RAW$percentSWS)
RAW$total.sleep.time <- RAW$total.sleep.time/60
RAW$total.sleep.time <- ceiling(RAW$total.sleep.time)
RAW$median.pre <-RAW$`Trial 91-100 median`
RAW$median.post <- RAW$`Median 1-10`
library(ggplot2)
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
#This plot labels the percent SWS over data-points. Not pretty but helps us to detect outlier
library(ggplot2)
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

#checking for total sleep time
library(ggplot2)
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

#Looking at the NAPPERS only 
# library(nlme) ## the graph was ugly and can be made in other ways. I made some below
# RAW$median.pre <-RAW$`Trial 91-100 median`
# RAW$median.post <- RAW$`Median 1-10`
# RAW$percentSWS <- 100*RAW$percentSWS
# RAW$percentSWS <- ceiling(RAW$percentSWS)
# Before.NAP <- data.frame(Condition = "Before", Median.Score = c(RAW$median.pre[RAW$Condition == 'NAP']), Subject = c(RAW$Code[RAW$Condition == 'NAP']), percentSWS = c(RAW$percentSWS[RAW$Condition == 'NAP']))
# After.NAP <- data.frame(Condition = "After", Median.Score = c(RAW$median.post[RAW$Condition == 'NAP']), Subject = c(RAW$Code[RAW$Condition == 'NAP']), percentSWS = c(RAW$percentSWS[RAW$Condition == 'NAP']))
# df.median <- rbind(Before.NAP, After.NAP)
# library(ggplot2)
# #ggplot(df.median,aes(Condition, Median.Score)) + geom_point() + geom_line(aes(group=Subject))
# ggplot(df.median,aes(x = Condition, y = Median.Score, label = Subject)) + geom_point( aes(colour=Subject)) + geom_line(aes(group=Subject), show.legend = TRUE) + geom_text (check_overlap = TRUE, position = position_jitter(w = 0.2, h = 0.1),aes(colour = Subject))
# ##
# library(dplyr)
# RAW$Median.Diff <- RAW$`Median 1-10` - RAW$`Trial 91-100 median`
# RAW$percentSWS <- 100*RAW$percentSWS
# RAW$percentSWS <- ceiling(RAW$percentSWS)
# RAW$total.sleep.time <- RAW$total.sleep.time/60
# RAW$total.sleep.time <- ceiling(RAW$total.sleep.time)
# NAP.performance.nosws <- filter(RAW, percentSWS == 0)
# NAP.performance.sws <-  filter(RAW, percentSWS != 0)
# df2.median <-rbind(NAP.performance.nosws, NAP.performance.sws)
# ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "red") #PercentSWSgraph
# ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = Code )) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(label = paste(percentSWS, "^(", Code, ")", sep = "")), parse = TRUE,position = position_jitter(w = 0.3, h = 0.1)) #PercentSWSCode 
# ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = percentSWS )) + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(colour = percentSWS), size = 5)  #PercentSWSnoCode
### we found that there was one outlier here, so I commented-out all those lines and will correct the RAW files by removing that data point, and then repeat above codes below
#now I'm trying to remove the LN7 that looks like an outlier and plot it again, and draw a correlation line
library(dplyr)
RAW$median.pre <-RAW$`Trial 91-100 median`
RAW$median.post <- RAW$`Median 1-10`
RAW.without.LN7 <- RAW[-c(7),] #removing the LN7 and renaming RAW 
RAW.without.LN7$Median.Diff <- RAW.without.LN7$`Median 1-10` - RAW.without.LN7$`Trial 91-100 median`
RAW.without.LN7$percentSWS <- 100*RAW.without.LN7$percentSWS
RAW.without.LN7$percentSWS <- ceiling(RAW.without.LN7$percentSWS)
RAW.without.LN7$total.sleep.time <- RAW.without.LN7$total.sleep.time/60
RAW.without.LN7$total.sleep.time <- ceiling(RAW.without.LN7$total.sleep.time)
RAW.without.LN7$swstype[RAW.without.LN7$percentSWS == 0] <- "zero"
RAW.without.LN7$swstype[RAW.without.LN7$percentSWS != 0] <- "non-zero"
NAP.performance.nosws <- filter(RAW.without.LN7, percentSWS == 0)
NAP.performance.sws <-  filter(RAW.without.LN7, percentSWS != 0)
df2.median <-rbind(NAP.performance.nosws, NAP.performance.sws)
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "red") #PercentSWSgraph
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = Code )) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(label = paste(percentSWS, "^(", Code, ")", sep = "")), parse = TRUE,position = position_jitter(w = 0.3, h = 0.1)) #PercentSWSCode 
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = percentSWS )) + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(colour = percentSWS), size = 5)  #PercentSWSnoCode
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, label = percentSWS )) + geom_hline(yintercept=0, linetype="dashed", color = "red") + geom_text(aes(colour = percentSWS), size = 5) + geom_smooth(method = lm) #PercentSWSnoCode
ggplot(df2.median, aes(x = total.sleep.time, y=Median.Diff, colour = swstype )) + geom_point(aes(size = percentSWS)) + geom_hline(yintercept=0, linetype="dashed", color = "blue") + geom_smooth(method = lm, se= FALSE) #+ theme_apa() #PercentSWSgraph with regression lines}}

# Calculating ANOVA with Bayesian Framework ----
# Across all conditions - comparing the GMean 
RAW.without.LN7$TPR <-
  RAW.without.LN7$`Hit ratio` / (RAW.without.LN7$`Hit ratio` + RAW.without.LN7$`False Alarm ratio`)
RAW.without.LN7$TNR <-
  RAW.without.LN7$`Correct Rejection ratio` / (RAW.without.LN7$`Correct Rejection ratio` + RAW.without.LN7$`Miss ratio`)
#calculating the G-Mean
RAW.without.LN7$GMean <- sqrt(RAW.without.LN7$TPR * RAW.without.LN7$TNR)
fit_GMean <- rstanarm::stan_glm(GMean ~ Condition, data=RAW.without.LN7)
results <- psycho::analyze(fit_GMean)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean, "Condition")
contrasts$means
contrasts$contrasts

ggplot(contrasts$means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("GMean") +
  xlab("Condition")
-----------
  #for nappers - comparing the GMean for sws and non-sws
  RAW.for.NAP <- RAW.without.LN7[1:15,]
fit_GMean_NAP <- rstanarm::stan_glm(GMean ~ swstype , data=RAW.for.NAP)
results <- psycho::analyze(fit_GMean_NAP)
print(results)
contrasts <- psycho::get_contrasts(fit_GMean_NAP, "swstype")
contrasts$means
contrasts$contrasts

ggplot(contrasts$means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("GMean") +
  xlab("swstype")
-------------
  #for nappers - comparing the median of differences in maze task for sws and non-sws
  RAW.for.NAP <- RAW.without.LN7[1:15,]
fit_Maze_NAP <- rstanarm::stan_glm(Median.Diff ~ swstype , data=RAW.for.NAP)
results <- psycho::analyze(fit_Maze_NAP)
print(results)
contrasts <- psycho::get_contrasts(fit_Maze_NAP, "swstype")
contrasts$means
contrasts$contrasts

ggplot(contrasts$means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Median.Diff") +
  xlab("swstype")  
# Calculating repeated-measures ANOVA based on swstype ----
# for Motor-task:
#make sure to check for those outliers that I have selected above. These excel files may still contain those.
library(readxl)
nonSWS_repeated <- read_excel("Downloads/Sleep & Meditation & Memory articles/Analysis/R analysis/Oct 2018/Rep.Mes.ANOVAs/nonSWS repeated.xlsx", 
                              na = "N/A")
View(nonSWS_repeated)

SWS_repeated_measures <- read_excel("Downloads/Sleep & Meditation & Memory articles/Analysis/R analysis/Oct 2018/Rep.Mes.ANOVAs/SWS repeated measures.xlsx", 
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


fit_RM_nonSWS <- lmer(MedinScores ~ Condition + (1|Code), data=nonSWS_repeated)
anova(fit_RM_nonSWS)
print(analyze(fit_RM_nonSWS))

results_RM_nonSWS <- get_contrasts(fit_RM_nonSWS, "Condition")
print(results_RM_nonSWS$contrasts)
results_RM_SWS$means

ggplot(results_RM_nonSWS$means, aes(x=Condition, y=Mean, group=1)) +
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
print(results_RM_SWS$contrasts)
results_RM_SWS$means

ggplot(results_RM_SWS$means, aes(x=Condition, y=Mean, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Median of Scores - SWS") +
  xlab(" Condition") +
  theme_bw()
#now, running them in Bayesian
#for nonSWS
fit_Bayes_RM_nonSWS <- rstanarm::stan_lmer(MedinScores ~ Condition + (1|Code), data=nonSWS_repeated)
results <- psycho::analyze(fit_Bayes_RM_nonSWS)
summary(results, round = 2)
print(results)
#I was not able to plot for this. just explanation, if it is necessary

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

fit_MODREG_FMI <- lm(Median.Diff ~ Condition + FMI + Condition:FMI, data = RAW_noSWS_and_MED_WAKE)
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

##IMPORTANT: remove those meditators who slept. MM5 and MM4 probably