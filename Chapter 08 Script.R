# Chapter 8 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("DataReExpression.Rdata")

# Save R History file

save_history <- savehistory("DataReExpression.Rhistory")

# Packages Needed for Chapter 8

install.packages("CTT")
library(CTT)

install.packages("psych")
library(psych)

install.packages("scales")
library(scales)

# Load SE_MPMA.R script file

# Load normal.csv

normal <- read.csv(file.choose(), header = TRUE)  

# Load left_skew.csv

left_skew <- read.csv(file.choose(), header = TRUE)

# Load right_skew.csv

right_skew <- read.csv(file.choose(), header = TRUE)

# Load example_distribution.csv

example_distribution <- read.csv(file.choose(), header = TRUE)

normal <- as.vector(normal$normal)
left_skew <- as.vector(left_skew$left_skew)
right_skew <- as.vector(right_skew$right_skew)
example_distribution <- as.vector(example_distribution$example_distribution)

class(normal)
class(left_skew)
class(right_skew)
class(example_distribution)

# 8.2. Clarifying Terminology

plot(density(example_distribution),
     main = "example_distribution",
     xlab = '',
     ylab = '')

# 8.2.1. Shifting 

library(psych)

describe(example_distribution)
summary(example_distribution)

example_distribution_shifted <- (example_distribution) + 2

describe(example_distribution_shifted)
summary(example_distribution_shifted)

plot(density(example_distribution_shifted),
     main = "example_distribution_shifted",
     xlab = '',
     ylab = '')

# 8.2.2. Centering

library(psych)

describe(example_distribution)
summary(example_distribution)

example_distribution_meancentered <- scale(example_distribution, scale = FALSE)

describe(example_distribution_meancentered)
summary(example_distribution_meancentered)

plot(density(example_distribution_meancentered),
     main = "example_distribution_meancentered",
     xlab = '',
     ylab = '')

# 8.2.3. Scaling

library(psych)

describe(example_distribution)
summary(example_distribution)

range_dif <- max(example_distribution) - min(example_distribution)

example_distribution_rangescaled <- example_distribution/range_dif

describe(example_distribution_rangescaled)
summary(example_distribution_rangescaled)

plot(density(example_distribution_rangescaled),
     main = "example_distribution_rangescaled",
     xlab = "",
     ylab = "")

# 8.2.4. Normalization

library(psych)

describe(example_distribution)
summary(example_distribution)

example_distribution_normalized <- rescale(example_distribution, mean = 100, sd = 15, df = FALSE)

describe(example_distribution_normalized)
summary(example_distribution_normalized)

plot(density(example_distribution_normalized),
     main = "example_distribution_normalized",
     xlab = '',
     ylab = '')

# 8.2.5. Standardization

library(psych)

describe(example_distribution)
summary(example_distribution)

example_distribution_standardized <- scale(example_distribution)

describe(example_distribution_standardized)
summary(example_distribution_standardized)

plot(density(example_distribution_standardized),
     main = "example_distribution_standardized",
     xlab = '',
     ylab = '')

# 8.2.6. Transformation

library(psych)

describe(example_distribution)
summary(example_distribution)

example_distribution_transformed <- (example_distribution) ^ 2

describe(example_distribution_transformed)
summary(example_distribution_transformed)

plot(density(example_distribution_transformed),
     main = "example_distribution_transformed",
     xlab = '',
     ylab = '')

# 8.3. Selecting a Re-Expression Method

# 8.3.1. Communicating Variables to Audiences

# 8.3.1.1. Percentile Ranks

library(CTT)

MP_transform <- score.transform(SE_MPMA$MusicPerceptionScores)
MP_percentiles <- round(MP_transform$p.scores, digits = 2)
head(MP_percentiles)

MP_percentile_values <- quantile(SE_MPMA$MusicPerceptionScores, probs = c(seq(0, 1, by = .01)))
MP_percentile_values <- round(MP_percentile_values, digits = 2)
head(MP_percentile_values, 10)

# 8.3.2. Comparing Variables

# 8.3.2.1. Z-Score Normalization

library(CTT)

MP_norm <- score.transform(SE_MPMA$MusicPerceptionScores)
MP_zscores <- round(MP_norm$new.scores, digits = 2)
head(MP_zscores)

library(psych)

describe(MP_zscores)

plot(density(MP_zscores),
     main = "Z-Score Normalization of Music Perception Scores",
     xlab = '',
     ylab = '')

# 8.3.2.2. SS-Score Normalization

MP_SS_score <- rescale(SE_MPMA$MusicPerceptionScores, mean = 50, sd = 10, df = FALSE)
class(MP_SS_score)

MP_SS_score <- as.vector(MP_SS_score)
class(MP_SS_score)
head(MP_SS_score)

library(psych)

describe(MP_SS_score)

plot(density(MP_SS_score),
     main = "SS-Score Normalization of Music Perception Scores",
     xlab = '',
     ylab = '')

# 8.3.3. Equality of Variables

# 8.3.3.1. [0 , 1] Min-Max Scaling

library(scales)

MP_minmax_rescale <- rescale(SE_MPMA$MusicPerceptionScores, to = c(0, 1))

library(psych)

describe(MP_minmax_rescale)

plot(density(MP_minmax_rescale),
     main = "Min-Max [0, 1] Scaling of Music Perception Scores",
     xlab = '',
     ylab = '')

# 8.3.3.2. Arbitrary Min-Max Scaling

library(scales)

MP_minmax_arbitrary <- rescale(SE_MPMA$MusicPerceptionScores, to = c(-1, 1))

library(psych)

describe(MP_minmax_arbitrary)

plot(density(MP_minmax_arbitrary),
     main = "Min-Max [-1, 1] Scaled",
     xlab = '',
     ylab = '')

# 8.3.4. Normality

# 8.3.4.1. Addressing Skewness

par(mfrow = c(3, 1))

hist(left_skew, main = "Left Skew", xlab = '', xlim = c(0.0,1.0))
hist(normal, main = "Approximately Normal",xlab = '', xlim = c(0.0,1.0))
hist(right_skew, main = "Right Skew", xlab = '', xlim = c(0.0,1.0))

library(psych)

describe(normal)
describe(left_skew)
describe(right_skew)

# 8.3.4.1.1. Reducing Right Skewness

# 8.3.4.1.1.1. Logarithmic Transformation

range(right_skew)

log_e_right_skew <- log(right_skew + 1)
log_10_right_skew <- log10(right_skew + 1)

par(mfrow = c(1, 1))

hist(log_e_right_skew, main = "Natural Log Transformation")
hist(log_10_right_skew, main = "Base-10 Log Transformation")

describe(right_skew)
describe(log_e_right_skew)
describe(log_10_right_skew)

# 8.3.4.1.1.2. Reciprocal Transformation

right_skew_recip <- 1/(right_skew + 1)
right_skew_neg_recip <- -1/(right_skew + 1)

hist(right_skew_recip, main = "Reciprocal Transformation", ylim = c(0, 2000), xlab = '')
hist(right_skew_neg_recip, main = "Negative Reciprocal Transformation", ylim = c(0, 2000), xlab = '')

describe(right_skew)
describe(right_skew_recip)
describe(right_skew_neg_recip)

# 8.3.4.1.1.3. Cube Root Transformation

right_skew_cuberoot <- (right_skew)^(1/3)

hist(right_skew_cuberoot, main = "Cube Root Transformation", xlab = '')

describe(right_skew)
describe(right_skew_cuberoot)

# 8.3.4.1.2. Reducing Left Skewness

# 8.3.4.1.2.1. Squares Transformation 

left_skew_squared <- left_skew ^ 2

hist(left_skew, main = "Left Skew", xlab = '')
hist(left_skew_squared, main = "Squares Transformation", xlab = '')

describe(left_skew)
describe(left_skew_squared)

# 8.3.4.1.2.2. Cubes Transformation

left_skew_cubes <- left_skew ^ 3

hist(left_skew_cubes, main = "Cubes Transformation", xlab = '')

describe(left_skew)
describe(left_skew_cubes)

# 8.4. Closing Out the Chapter

save_workspace
save_history
