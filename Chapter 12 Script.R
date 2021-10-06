# Chapter 12 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("SpecialCases.Rdata")

# Save R History file

save_history <- savehistory("SpecialCases.Rhistory")


# Packages Needed for Chapter 12

install.packages("effectsize")
library(effectsize)

install.packages("effsize")
library(effsize)

install.packages("emmeans")
library(emmeans)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("lsr")
library(lsr)

install.packages("psych")
library(psych)

install.packages("reshape2")
library(reshape2)

# Load SE_MPMA.R script file

# 12.2.1. One-Sample t-test

library(psych)

describe(SE_MPMA$MusicPerceptionScores)

library(ggplot2)
library(ggpubr)

ggline(SE_MPMA, y = "MusicPerceptionScores",
       add = c("mean_se", "jitter"),
       ylab = "Music Perception Score",
       xlab = "",
       title = "Music Perception Scores") +
  grids(linetype = "dashed") +
  scale_x_discrete(labels = c("")) +
  geom_hline(yintercept = 0.001466667, linetype = "dashed")

one_sample_t_regression <- lm(SE_MPMA$MusicPerceptionScores ~ 1)
summary(one_sample_t_regression)

one_sample_t <- t.test(SE_MPMA$MusicPerceptionScores)
one_sample_t

library(effsize)

cohen.d(SE_MPMA$MusicPerceptionScores ~ 1)

t.test(SE_MPMA$MusicPerceptionScores, mu = 3.00)

cohen.d(SE_MPMA$MusicPerceptionScores ~ 1, mu = 3.00)

# 12.2.2. Two-Sample t-test

# Load PerformanceAnxiety.csv

PerformanceAnxiety <- read.csv(file.choose(), header = TRUE)

str(PerformanceAnxiety)

PerformanceAnxiety$SexType <- factor(PerformanceAnxiety$SexType, levels = c("Female", "Male"))

str(PerformanceAnxiety)

contrasts(PerformanceAnxiety$SexType)

PerformanceAnxiety$SexType <- factor(PerformanceAnxiety$SexType,
                                     levels=c("Male","Female")) 

colnames(contrasts(PerformanceAnxiety$SexType)) <- c("Female_compared_to_Male")

contrasts(PerformanceAnxiety$SexType)

library(psych)

describeBy(PerformanceAnxiety$PerformanceAnxiety, group = PerformanceAnxiety$SexType)

library(ggplot2)
library(ggpubr)

ggline(PerformanceAnxiety, x = "SexType", y = "PerformanceAnxiety",
       add = c("mean_se", "jitter"),
       order = c("Male", "Female"),
       ylab = "Performance Anxiety Scores",
       xlab = "Sex-Type",
       connect.lines = TRUE,
       title = "Performance Anxiety Scores by Sex-Type") +
  grids(linetype = "dashed") +
  scale_x_discrete(labels = c("Male", "Female"))

two_sample_t_regression <- lm(PerformanceAnxiety$PerformanceAnxiety ~ PerformanceAnxiety$SexType)
summary(two_sample_t_regression)

two_sample_t <- t.test(PerformanceAnxiety$PerformanceAnxiety ~ 1 + PerformanceAnxiety$SexType)
two_sample_t

library(effsize)

cohen.d(PerformanceAnxiety$PerformanceAnxiety ~ PerformanceAnxiety$SexType)


# 12.2.3. Paired Samples t-test

# Load PerformanceConfidence.csv

PerformanceConfidence <- read.csv(file.choose(), header = TRUE)

str(PerformanceConfidence)

head(PerformanceConfidence)

PerformanceConfidence_long <- PerformanceConfidence

Participant_ID <- 1:150
PerformanceConfidence_long$Participant_ID <- Participant_ID
PerformanceConfidence_long <- PerformanceConfidence_long[, c(3, 1, 2)] 

head(PerformanceConfidence_long)

library(reshape2)

PerformanceConfidence_long <- melt(PerformanceConfidence_long,
                                   measure.vars = c("PerfConfidence_pre", "PerfConfidence_post"),
                                   variable.name = "Condition",
                                   value.name = "PerformanceConfidence")

head(PerformanceConfidence_long)
tail(PerformanceConfidence_long)

library(psych)

describeBy(PerformanceConfidence_long$PerformanceConfidence, group = PerformanceConfidence_long$Condition)

library(ggplot2)
library(ggpubr)

ggline(PerformanceConfidence_long, x = "Condition", y = "PerformanceConfidence",
       add = c("mean_se", "jitter"),
       order = c("PerfConfidence_pre", "PerfConfidence_post"),
       ylab = "Performance Confidence Scores",
       xlab = "Condition",
       connect.lines = TRUE,
       title = "Performance Confidence Scores by Condition") +
  grids(linetype = "dashed") +
  scale_x_discrete(labels = c("Pretest", "Posttest"))

paired_samples_t_regression <- lm(PerfConfidence_post - PerfConfidence_pre ~ 1, data = PerformanceConfidence)

summary(paired_samples_t_regression)

t.test(PerformanceConfidence$PerfConfidence_post, PerformanceConfidence$PerfConfidence_pre, paired = TRUE)

t.test(PerformanceConfidence_long$PerformanceConfidence ~ PerformanceConfidence_long$Condition, paired = TRUE)

library(effsize)

cohen.d(PerfConfidence_post - PerfConfidence_pre ~ 1, data = PerformanceConfidence, paired = TRUE)


# 12.3.1. One-Way Between-Subjects Analysis of Variance

# Load EarTraining.csv

EarTraining <- read.csv(file.choose(), header = TRUE)

str(EarTraining)

EarTraining$instrument <- factor(EarTraining$instrument, levels = c("brass", "piano", "woodwinds"))

str(EarTraining)

library(psych)

describeBy(EarTraining$eartraining, group = EarTraining$instrument)

library(ggplot2)
library(ggpubr)

ggline(EarTraining, x = "instrument", y = "eartraining",
       add = c("mean_se", "jitter"),
       order = c("brass", "piano", "woodwinds"),
       ylab = "Ear Training Scores",
       xlab = "Instrument",
       connect.lines = TRUE,
       title = "Ear Training Scores by Instrument") +
  grids(linetype = "dashed")


one_way_between_regression <- lm(eartraining ~ 1 + instrument, data = EarTraining)
summary(one_way_between_regression)

one_way_aov_between <- aov(EarTraining$eartraining ~ EarTraining$instrument)
summary(one_way_aov_between)

library(lsr)

etaSquared(one_way_aov_between)  

model.tables(one_way_aov_between, "means")

TukeyHSD(one_way_aov_between)


# 12.3.2. One-Way Within-Subjects Analysis of Variance

# Load Recall.csv

recall <- read.csv(file.choose(), header = TRUE)

str(recall)

recall$TimeSpan <- factor(recall$TimeSpan, levels = c("NextDay", "Week", "Month"))

str(recall)

head(recall)

levels(recall$TimeSpan)

colnames(contrasts(recall$TimeSpan)) <- c("Week_compared_to_NextDay", "Month_compared_to_NextDay")

contrasts(recall$TimeSpan)

library(psych)

describeBy(recall$Recall, group = recall$TimeSpan)

library(ggplot2)
library(ggpubr)

ggline(recall, x = "TimeSpan", y = "Recall",
       add = c("mean_se", "jitter"),
       order = c("NextDay", "Week", "Month"),
       ylab = "Recall Scores",
       xlab = "Time Span",
       connect.lines = TRUE,
       title = "Recall Scores by Time Span") +
  grids(linetype = "dashed")

oneway_aov_within <- aov(Recall ~ TimeSpan + Error(Participant_ID/TimeSpan), data = recall)
summary(oneway_aov_within)

library(effectsize)

eta_squared(oneway_aov_within)

library(emmeans)

pairwise <- emmeans(oneway_aov_within, ~ TimeSpan)

pairwise

pairs(pairwise)

# 12.3.3. Two-Way Between-Subjects Analysis of Variance

# Load PerformanceAchievement.csv

PerformanceAchievement <- read.csv(file.choose(), header = TRUE)

str(PerformanceAchievement)

PerformanceAchievement$HomeMusic <- factor(PerformanceAchievement$HomeMusic, levels = c("Much", "Some", "None"))
PerformanceAchievement$PrivateLessons <- factor(PerformanceAchievement$PrivateLessons, levels = c("No", "Yes"))

str(PerformanceAchievement)

levels(PerformanceAchievement$HomeMusic)

contrasts(PerformanceAchievement$HomeMusic)

PerformanceAchievement$HomeMusic <- factor(PerformanceAchievement$HomeMusic, levels = c("None", "Some", "Much"))

colnames(contrasts(PerformanceAchievement$HomeMusic)) <- c("Some_compared_to_None", "Much_compared_to_none")
colnames(contrasts(PerformanceAchievement$PrivateLessons)) <- c("Yes_compared_to_No")

contrasts(PerformanceAchievement$HomeMusic)
contrasts(PerformanceAchievement$PrivateLessons)

PerformanceAchievement <- data.frame(PerformanceAchievement$PerformanceAchievement, PerformanceAchievement$HomeMusic, PerformanceAchievement$PrivateLessons)

colnames(PerformanceAchievement) <- c("PerformanceAchievement", "HomeMusic", "PrivateLessons")


library(psych)

describeBy(PerformanceAchievement$PerformanceAchievement, group = PerformanceAchievement$HomeMusic)

describeBy(PerformanceAchievement$PerformanceAchievement, group = PerformanceAchievement$PrivateLessons)

library(ggplot2)
library(ggpubr)

ggline(PerformanceAchievement, x = "HomeMusic", y = "PerformanceAchievement", color = "PrivateLessons",
       add = c("mean_se", "jitter"),
       order = c("None", "Some", "Much"),
       ylab = "Performance Achievement Scores",
       xlab = "Home Music Participation",
       connect.lines = TRUE,
       title = "Performance Achievement Scores by Home Music and Private Lesson Participation") +
  grids(linetype = "dashed")

two_way_between_regression <- lm(PerformanceAchievement ~ HomeMusic + PrivateLessons + HomeMusic*PrivateLessons, data = PerformanceAchievement)
summary(two_way_between_regression)

two_way_aov_between <- aov(PerformanceAchievement ~ HomeMusic + PrivateLessons + HomeMusic*PrivateLessons, data = PerformanceAchievement)
summary(two_way_aov_between)

library(lsr)

etaSquared(two_way_aov_between)

model.tables(two_way_aov_between, "means")

TukeyHSD(two_way_aov_between)

# 12.3.4. Two-Way Within Subjects Analysis of Variance

# Load recall_twoway_within.csv

recall <- read.csv(file.choose(), header = TRUE)

str(recall)

head(recall)

library(reshape2)

recall_long <- melt(recall,
                    id = "Participant_ID",
                    measured = c("NextDay_ear", "Week_ear", "Month_ear", 
                                 "NextDay_written", "Week_written", "Month_written"))

head(recall_long)

colnames(recall_long) <- c("Participant_ID", "Condition", "RecallScore")

head(recall_long)

recall_long$TimeSpan <- c(rep("NextDay", 50), rep("Week", 50), rep("Month", 50), 
                          rep("NextDay", 50), rep("Week", 50), rep("Month", 50))

recall_long$Method <- c(rep("Ear", 150), rep("Written", 150))

head(recall_long)

class(recall_long$TimeSpan)
class(recall_long$Method)

recall_long$TimeSpan <- as.factor(recall_long$TimeSpan)
recall_long$Method <- as.factor(recall_long$Method)

class(recall_long$TimeSpan)
class(recall_long$Method)

levels(recall_long$TimeSpan)

recall_long$TimeSpan <- factor(recall_long$TimeSpan, levels = c("NextDay", "Week", "Month"))
contrasts(recall_long$TimeSpan)

library(psych)

describeBy(recall_long$RecallScore, group = recall_long$TimeSpan)

describeBy(recall_long$RecallScore, group = recall_long$Method)

library(ggplot2)
library(ggpubr)

ggline(recall_long, x = "TimeSpan", y = "RecallScore", color = "Method",
       add = c("mean_se", "jitter"),
       order = c("NextDay", "Week", "Month"),
       ylab = "Recall Scores",
       xlab = "Time Span",
       connect.lines = TRUE,
       title = "Recall Scores by Time Span and Method") +
  grids(linetype = "dashed"
  )

twoway_aov_within <- aov(RecallScore ~ TimeSpan + Method + TimeSpan*Method + Error(Participant_ID/(TimeSpan + Method + TimeSpan*Method)), data = recall_long)
summary(twoway_aov_within)

library(effectsize)

eta_squared(twoway_aov_within)

library(emmeans)

pairwise <- emmeans(twoway_aov_within, ~ Method | TimeSpan)

pairwise

pairs(pairwise, adjust = "tukey")

# 12.4. Closing Out the Chapter

save_workspace
save_history
