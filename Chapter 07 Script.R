# Chapter 7 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("Normality.Rdata")

# Save R History file

save_history <- savehistory("Normality.Rhistory")

# Packages Needed for Chapter 7

install.packages("car")
library(car)

install.packages("EnvStats")
library(EnvStats)

install.packages("moments")
library(moments)

install.packages("nortest")
library(nortest)

install.packages("outliers")
library(outliers)

install.packages("psych")
library(psych)


# 7.4. Conducting Univariate Normality Tests

mean(SE_MPMA$MusicPerceptionScores)
sd(SE_MPMA$MusicPerceptionScores)

library(psych)

describe(SE_MPMA$MusicPerceptionScores)


# 7.4.1. Shapiro-Wilk Test 

shapiro.test(SE_MPMA$MusicPerceptionScores)

# 7.4.2. Anderson-Darling Test     

library(nortest)

ad.test(SE_MPMA$MusicPerceptionScores)

# 7.4.3. Jarque-Bera Test   

library(moments)

jarque.test(SE_MPMA$MusicPerceptionScores)

# 7.4.4. Anscombe-Glynn Test

library(moments)

anscombe.test(SE_MPMA$MusicPerceptionScores)

# 7.4.5. Geary and Bonnet-Seier Tests 

sqrt(2/pi)

library(moments)

geary(SE_MPMA$MusicPerceptionScores)

bonett.test(SE_MPMA$MusicPerceptionScores)

# 7.4.6. D’Agostino Test

describe(SE_MPMA$MusicAptitudeScores)

library(moments)

agostino.test(SE_MPMA$MusicAptitudeScores)

# 7.4.7. Kolmogorov-Smirnov Test

mean(SE_MPMA$MusicPerceptionScores)
sd(SE_MPMA$MusicPerceptionScores)

ks.test(SE_MPMA$MusicPerceptionScores, "pnorm", mean(SE_MPMA$MusicPerceptionScores), sd(SE_MPMA$MusicPerceptionScores))

# 7.2. Univariate Outlier Identification Techniques

# 7.2.1. Plotting Data to Identify Outliers

# Set plot parameters

par(mfrow = c(2,2))

# Plot histogram with rug plot overlay

hist(SE_MPMA$MusicPerceptionScores, breaks = 70)
rug(SE_MPMA$MusicPerceptionScores)

# Install and load car library

library(car)

# Plot boxplot 

Boxplot(SE_MPMA$MusicPerceptionScores)

#Plot Q-Q Plot

library(car)
qqPlot(SE_MPMA$MusicPerceptionScores)

#Plot EDCF plot

ecdf(SE_MPMA$MusicPerceptionScores)
ecdf <- ecdf(SE_MPMA$MusicPerceptionScores)
plot.ecdf(ecdf(SE_MPMA$MusicPerceptionScores))

library(car)

par(mfrow = c(1,1))

qqPlot(SE_MPMA$MusicPerceptionScores, id = list(n = 3))

SE_MPMA$MusicPerceptionScores[1]

SE_MPMA$MusicPerceptionScores[29]

SE_MPMA$MusicPerceptionScores[83]

# 7.2.2. Descriptive Statistics Rules-of-Thumb to Identify Outliers

# 7.2.2.1. Z-scores

MP_zscore <- scale(SE_MPMA$MusicPerceptionScores)

sort(MP_zscore)

head(sort(MP_zscore))
tail(sort(MP_zscore))

sort(unique(MP_zscore[MP_zscore > 3.00]))
sort(unique(MP_zscore[MP_zscore < -3.00]))

MP_zscore[73]
MP_zscore[1]

# 7.2.2.2. Interquartile Range

IQR(SE_MPMA$MusicPerceptionScores)

MP_quartile1 <- quantile(SE_MPMA$MusicPerceptionScores, prob = .25)
MP_quartile3 <- quantile(SE_MPMA$MusicPerceptionScores, prob = .75)

MP_quartile3 - MP_quartile1

IQR <- IQR(SE_MPMA$MusicPerceptionScores)
LT <- MP_quartile1 - (1.5*IQR)
UT <- MP_quartile3 + (1.5*IQR)

IQR
LT
UT

sort(unique(SE_MPMA$MusicPerceptionScores[SE_MPMA$MusicPerceptionScores > UT]))

sort(unique(SE_MPMA$MusicPerceptionScores[SE_MPMA$MusicPerceptionScores < LT]))


match(0.45, SE_MPMA$MusicPerceptionScores)
match(0.53, SE_MPMA$MusicPerceptionScores)
match(-0.67, SE_MPMA$MusicPerceptionScores)

# 7.2.3. Formal Hypothesis Testing to Identify Outliers

# 7.2.3.1. Grubbs’ Outlier Test

library(outliers)

grubbs.test(SE_MPMA$MusicPerceptionScores)

match(-0.67, SE_MPMA$MusicPerceptionScores)

SE_MPMA$MusicPerceptionScores[1]

# 7.2.3.2. Rosner’s Outlier Test

library(EnvStats)

rosnerTest(SE_MPMA$MusicPerceptionScores)

# 7.3. Closing Out the Chapter

save_workspace
save_history
