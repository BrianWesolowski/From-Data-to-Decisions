# Chapter 5 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("SummaryOperations.Rdata")

# Save R History file

save_history <- savehistory("SummaryOperations.Rhistory")

# Packages Needed for Chapter 5

install.packages("CTT")
library(CTT)

install.packages("cocron")
library(cocron)

install.packages("DescTools")
library(DescTools)

install.packages("gmodels")
library(gmodels)

install.packages("lsr")
library(lsr)

install.packages("moments")
library(moments)

install.packages("psych")
library(psych)

install.packages("psychometric")
library(psychometric)

install.packages("sjstats")
library(sjstats)

# Load SE_MPMA.R script file

str(SE_MPMA)

# 5.2. The summary() Function

summary(SE_MPMA)

# 5.3. Univariate Data
# 5.3.1. Descriptive Statistics for Univariate Categorical Data
# 5.3.1.1. Frequency

summary(SE_MPMA$Item1)
table(SE_MPMA$Item1)

Item1_summary <- summary(SE_MPMA$Item1)
Item1_table <- table(SE_MPMA$Item1)

class(Item1_summary)
class(Item1_table)

Item1_freq_matrix <- cbind(Frequency = Item1_table)
Item1_freq_matrix

# 5.3.1.2. Proportional Frequency

Item1_prop_freq <- prop.table(Item1_table)
Item1_prop_freq

Item1_prop_freq_round <- round(prop.table(Item1_table), digits = 2)
Item1_prop_freq_round

Item1_freq_matrix <- cbind(Frequency = Item1_table, Prop_Frequency = Item1_prop_freq_round)
Item1_freq_matrix

# 5.3.1.3. Cumulative Frequency

Item1_cum_freq <- cumsum(Item1_table)
Item1_cum_freq

Item1_freq_matrix <- cbind(Frequency = Item1_table, Prop_Frequency = Item1_prop_freq_round, Cum_Frequency = Item1_cum_freq)
Item1_freq_matrix

# 5.3.1.4. Cumulative Proportional Frequency

cumsum(Item1_table)
nrow(SE_MPMA)

Item1_cum_rel_freq <- round(cumsum(Item1_table)/nrow(SE_MPMA), digits = 2)
Item1_cum_rel_freq

Item1_freq_matrix <- cbind(Frequency = Item1_table, Prop_Freqency = Item1_prop_freq_round, Cum_Frequency = Item1_cum_freq, Cum_Prop_Frequency = Item1_cum_rel_freq)
Item1_freq_matrix

# 5.3.2. Descriptive Statistics for Bivariate Categorical Data
# 5.3.2.1. Frequency

table(SE_MPMA$Item1, SE_MPMA$YearsParticipating)

# 5.3.2.2. Proportional Frequency

Item1_YearsPart_crossclass <- table(SE_MPMA$Item1, SE_MPMA$YearsParticipating)
round(prop.table(Item1_YearsPart_crossclass), digits = 2)

# 5.3.2.3. Marginals
# 5.3.2.3.1. Frequency Marginals

margin.table(Item1_YearsPart_crossclass, 1)
margin.table(Item1_YearsPart_crossclass, 2)

# 5.3.2.3.2. Proportional Marginals

round(prop.table(Item1_YearsPart_crossclass, margin = 1), digits = 2)
round(prop.table(Item1_YearsPart_crossclass, margin = 2), digits = 2)

# 5.3.2.4. The CrossTable() Function

library(gmodels)

CrossTable(SE_MPMA$Item1, SE_MPMA$YearsParticipating)

# 5.3.3. Descriptive Statistics for Univariate Continuous Data

# 5.3.3.1. Sampling Theory and the Gaussian Distribution

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)
pnorm(2, mean = 0, sd = 1) - pnorm(-2, mean = 0, sd = 1)
pnorm(3, mean = 0, sd = 1) - pnorm(-3, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)
pnorm(-1, mean = 0, sd = 1)
qnorm(0.90, mean = 0, sd = 1)
qnorm(0.025, mean = 0,sd = 1)
qnorm(0.975, mean = 0,sd = 1)

# 5.3.3.2. Measures of Central Tendency

# 5.3.3.2.1. Mean

values <- c(0.89, 0.69, 0.64, 0.99, 0.66, 0.71, 0.54, 0.59, 0.29, 0.15, 0.96, 0.90, 0.69, 0.80, 0.02, 0.48, 0.76, 0.22, 0.32, 0.23)
values

n <- length(values)
n
sum(values) / n
mean(values)

# 5.3.3.2.2. Trimmed Mean

n_remove <- floor(.10 * n)
n_remove

trim <- function(x) sort(x)[-c(1, 2, length(x) -1, length(x))]

values_trimmed <- trim(values)

length(values)
length(values_trimmed)

head(sort(values), 4)
head(sort(values_trimmed), 4)
tail(sort(values), 4)
tail(sort(values_trimmed), 4)

mean(values_trimmed)
mean(values, trim = .10)

# 5.3.3.2.3. Median

x <- c(0.09, 0.21, 0.73, 0.85, 0.79)
x

x_sorted <- sort(x)
x_sorted

length(x_sorted)

(length(x_sorted) + 1) / 2

x_sorted[3]
median(x)

values

values_sorted <- sort(values)
length(values_sorted)
(length(values_sorted) + 1) / 2

values_sorted[10]
values_sorted[11]

mean(values_sorted[10:11])

median(values)

# 5.3.3.2.4. Mode

mode(values)
MP_mode_table <- table(SE_MPMA$MusicPerceptionScores)
MP_mode_table
names(MP_mode_table)[which(MP_mode_table == max(MP_mode_table))]

library(DescTools)

Mode(SE_MPMA$MusicPerceptionScores)
Mode(SE_MPMA$Instrument)

# 5.3.3.3. Measures of Dispersion

# 5.3.3.3.1. Population Variance

mean(SE_MPMA$MusicPerceptionScores)
str((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)
sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)
sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)/(length(SE_MPMA$MusicPerceptionScores))

library(sjstats)

var_pop(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.2. Sample Variance

sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)/(length(SE_MPMA$MusicPerceptionScores) - 1)
var(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.3. Population Standard Deviation

sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)/(length(SE_MPMA$MusicPerceptionScores))
sqrt(sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)/(length(SE_MPMA$MusicPerceptionScores)))

library(sjstats)

sd_pop(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.4. Sample Standard Deviation

sqrt(sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)/(length(SE_MPMA$MusicPerceptionScores) - 1))
sd(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.5. Average Absolute Deviation

mean(SE_MPMA$MusicPerceptionScores)
str(abs((SE_MPMA$MusicPerceptionScores) - (mean(SE_MPMA$MusicPerceptionScores))))
sum(abs((SE_MPMA$MusicPerceptionScores) - (mean(SE_MPMA$MusicPerceptionScores))))
(sum(abs((SE_MPMA$MusicPerceptionScores) - (mean(SE_MPMA$MusicPerceptionScores)))))/length(SE_MPMA$MusicPerceptionScores)

library(lsr)

aad(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.6. Median Absolute Deviation

median(SE_MPMA$MusicPerceptionScores)
SE_MPMA$MusicPerceptionScores - median(SE_MPMA$MusicPerceptionScores)
abs(SE_MPMA$MusicPerceptionScores - median(SE_MPMA$MusicPerceptionScores))
median(abs(SE_MPMA$MusicPerceptionScores - median(SE_MPMA$MusicPerceptionScores)))

mad(SE_MPMA$MusicPerceptionScores)

median(abs(SE_MPMA$MusicPerceptionScores - median(SE_MPMA$MusicPerceptionScores))) * 1.4826

# 5.3.3.3.7. Range

min(SE_MPMA$MusicPerceptionScores)
max(SE_MPMA$MusicPerceptionScores)
range(SE_MPMA$MusicPerceptionScores)
max(SE_MPMA$MusicPerceptionScores) - min(SE_MPMA$MusicPerceptionScores)

library(DescTools)

Range(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.8. Skewness

library(psych)
skew(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.9. Kurtosis

library(psych)
kurtosi(SE_MPMA$MusicPerceptionScores)

library(moments)
kurtosis(SE_MPMA$MusicPerceptionScores)

# 5.3.3.3.10. Standard Error of the Mean

sd(SE_MPMA$MusicPerceptionScores)
sqrt(length(SE_MPMA$MusicPerceptionScores))
sd(SE_MPMA$MusicPerceptionScores)/sqrt(length(SE_MPMA$MusicPerceptionScores))

library(DescTools)

MeanSE(SE_MPMA$MusicPerceptionScores)

# 5.3.3.4. Five-Number Summaries

round(summary(SE_MPMA$MusicPerceptionScores), digits = 2)

# 5.3.3.4.1. Quartiles

round(summary(SE_MPMA$MusicPerceptionScores), digits = 2)
fivenum(SE_MPMA$MusicPerceptionScores)

odd <- seq(1,99, by = 2)
summary(odd)
fivenum(odd)

# 5.3.3.4.2. Quantiles and Percentile Ranks

quantile(SE_MPMA$MusicPerceptionScores, probs = .33)
quantile(SE_MPMA$MusicPerceptionScores, probs = c(.00, .25, .50, .75, 1.00))

library(CTT)

MP_transform <- score.transform(SE_MPMA$MusicPerceptionScores)

head(SE_MPMA$MusicPerceptionScores)
round(head(MP_transform$new.scores), digits = 2)
round(head(MP_transform$p.scores), digits = 2)

quantile(SE_MPMA$MusicPerceptionScores, probs = c(seq(0, 1, by = .01 )))
head(round(quantile(SE_MPMA$MusicPerceptionScores, probs = c(seq(0, 1, by = .01 ))), digits = 2))  

# 5.3.3.4.3. The describe() Function

library(psych)

describe(SE_MPMA$MusicPerceptionScores)

# 5.3.4. Descriptive Statistics for a Continuous Variable Stratified by a Categorical Variable

library(psych)

describeBy(SE_MPMA$MusicPerceptionScores, group= SE_MPMA$YearsParticipating)

do.call("rbind", tapply(SE_MPMA$MusicPerceptionScores, SE_MPMA$YearsParticipating, quantile))
round(do.call("rbind", tapply(SE_MPMA$MusicPerceptionScores, SE_MPMA$YearsParticipating, quantile)), digits = 2)

# 5.4. Coefficient Alpha (Cronbach’s Alpha)

library(psychometric)

Item1_numeric <- as.numeric(SE_MPMA$Item1)
Item2_numeric <- as.numeric(SE_MPMA$Item2)
Item3_numeric <- as.numeric(SE_MPMA$Item3)

class(Item1_numeric)
class(Item2_numeric)
class(Item3_numeric)

items_numeric <- data.frame(Item1_numeric, Item2_numeric, Item3_numeric)

alpha(items_numeric)

person_mean <- rowMeans(items_numeric)
person_sd <- sd(person_mean)
SE.Meas(person_sd, alpha(items_numeric))

library(cocron)

alpha(items_numeric)
nrow(items_numeric)
ncol(items_numeric)

cronbach.alpha.CI(alpha(items_numeric), nrow(items_numeric), ncol(items_numeric))

# 5.5. Closing Out the Chapter

save_workspace
save_history
