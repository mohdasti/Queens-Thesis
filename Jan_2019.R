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
library(rio)
# importing the main excel file from Githul repository 
RAW_Jan_2019 <- rio::import('https://github.com/mohdasti/Queens-Thesis/raw/master/Jan_2019.xlsx', na="N/A")
View(RAW_Jan_2019)
RAW <- RAW_Jan_2019  #should be changed everytime I update the main excel file
RAW<- RAW[-c(7,25,30),] #Removing outlier participants - LN7: too many arousals - MM4&MM5: they slept through meditation session
#Subsetting the dataset based on the condition
NAP <- RAW[which(RAW$Condition == 'NAP'), ]
MED <- RAW[which(RAW$Condition == 'MED'), ]
WAKE <- RAW[which(RAW$Condition == 'WAKE'), ]

# Sleepiness----------
#adding a new column for difference in pre-post Epworth scale for further use
RAW$ESS_diff <- (RAW$ESS_Post - RAW$ESS_Pre)
#running repeated-measures t-test on ESS within-subject in different conditions
MED.ESSpre <- c(MED$ESS_Pre) #meditators
MED.ESSpost <- c(MED$ESS_Post)
t.test(MED.ESSpre, MED.ESSpost, paired = TRUE)
NAP.ESSpre <- c(NAP$ESS_Pre) #nappers
NAP.ESSpost <- c(NAP$ESS_Post)
t.test(NAP.ESSpre, NAP.ESSpost, paired = TRUE)
WAKE.ESSpre <- c(WAKE$ESS_Pre) #wake
WAKE.ESSpost <- c(WAKE$ESS_Post)
t.test(WAKE.ESSpre, WAKE.ESSpost, paired = TRUE)
#running ANOVA on ESS across all conditions
boxplot(ESS_diff ~ Condition, data = RAW, main = "Epworth pre-post difference across treatment conditions") #no outlier, but huge variation for nap condition. could be caused by sleep inertia.
qqnorm(RAW$ESS_diff)
qqline(RAW$ESS_diff) # there was not 'significant' departures from the line
shapiro.test(RAW$ESS_diff) #not significant, meaning that no violation of normality
RAW$Cond.factor <-
  factor(RAW$Condition, labels = c("NAP", "MED", "WAKE"))
library(car)
anova1 <- lm(RAW$ESS_diff ~ RAW$Condition)
Anova(anova1, type = 3) #it was significant
# Calculating the Effect Size
library(lsr)
etaSquared(anova1, type = 3)
#pairwise-comparison with Holm correction
pairwise.t.test(RAW$ESS_diff, RAW$Condition, p.adjust.method = "holm")
# checking for homogeneity of variance
leveneTest(anova1) #Brown-Forsyth Test
leveneTest(anova1, center = mean) #Levene's Test
#filtering napper based on their sleep profile
SWS_nappers <- filter(RAW, `percent SWS` != 0.0) #nappers who showed SWS