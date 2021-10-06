# Chapter 10 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("Simple Regression.Rdata")

# Save R History file

save_history <- savehistory("Simple Regression.Rhistory")

# Packages Needed for Chapter 10

install.packages("DescTools")
library(DescTools)

install.packages("effectsize")
library(effectsize)

install.packages("psych")
library(psych)

install.packages("Rmisc")
library(Rmisc)

# Load SE_MPMA Sript file


# 10.2. Beyond Correlation

cor(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores) == cor(SE_MPMA$MusicAptitudeScores, SE_MPMA$MusicPerceptionScores)

# 10.3. Modeling One Variable: The Mean Model

length(SE_MPMA$MusicPerceptionScores)
mean(SE_MPMA$MusicPerceptionScores)

plot(SE_MPMA$MusicPerceptionScores,
     main = "Plot of Music Perception Scores by Observation",
     ylab = "Music Perception Scores",
     xlab = "Observation IDs",
     pch = 19,
     col = "black",
     cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
text(SE_MPMA$MusicPerceptionScores, cex = .5, pos = 3)
grid()

# 10.3.3. Calculating Residuals

SE_MPMA$MusicPerceptionScores[1]
mean(SE_MPMA$MusicPerceptionScores)
SE_MPMA$MusicPerceptionScores[1] - mean(SE_MPMA$MusicPerceptionScores)

SE_MPMA$MusicPerceptionScores[2]
mean(SE_MPMA$MusicPerceptionScores)
SE_MPMA$MusicPerceptionScores[2] - mean(SE_MPMA$MusicPerceptionScores)

mmodel_residuals <- SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)

# 10.3.3.1. Summing the Residuals

mean(mmodel_residuals)
mmodel_sq_residuals <- mmodel_residuals ^ 2
mmodel_RSS <- sum(mmodel_sq_residuals)
mmodel_RSS


# 10.3.4. Distribution of the Residuals 

summary(mmodel_residuals)

par(mfrow = c(2, 1))

hist(mmodel_residuals, 
     main = "Histogram of Mean Model Residuals",
     xlab = "Residual Values",
     ylab = "Frequency (N = 150)")

plot(mmodel_residuals,
     main = "Scatterplot of Mean Model Residuals",
     xlab = "Observations",
     ylab = "Residual Values",
     pch = 19,
     cex = .6)

# 10.3.5. Standard Error of the Mean

library(DescTools)

mmodel_SEM <- MeanSE(SE_MPMA$MusicPerceptionScores)
mmodel_SEM

# 10.3.6. Confidence Intervals

mmodel_df <- length(SE_MPMA$MusicPerceptionScores) - 1
mmodel_df

mmodel_Alpha <- (1-.95) / 2
mmodel_Alpha_lower <- 0 + mmodel_Alpha
mmodel_Alpha_upper <- 1 - mmodel_Alpha

mmodel_Alpha
mmodel_Alpha_lower
mmodel_Alpha_upper

mmodel_t_crit_lower <- qt(mmodel_Alpha_lower, mmodel_df)
mmodel_t_crit_upper <- qt(mmodel_Alpha_upper, mmodel_df)

mmodel_t_crit_lower
mmodel_t_crit_upper

mmodel_lower_CI <- mean(SE_MPMA$MusicPerceptionScores) + (mmodel_SEM*mmodel_t_crit_lower)
mmodel_upper_CI <- mean(SE_MPMA$MusicPerceptionScores) + (mmodel_SEM*mmodel_t_crit_upper)

mmodel_lower_CI 
mmodel_upper_CI

library(Rmisc)

CI(SE_MPMA$MusicPerceptionScores, ci = 0.95)

par(mfrow = c(1, 1))

plot(SE_MPMA$MusicPerceptionScores,
     main = "Plot of Music Perception Scores by Observation",
     ylab = "Music Perception Scores",
     xlab = "Observation IDs",
     pch = 19,
     col = "black",
     cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
text(SE_MPMA$MusicPerceptionScores, cex = .5, pos = 3)
grid()
abline(h = mmodel_upper_CI)
abline(h = mmodel_lower_CI)


# 10.3.7. Mean Square Error and Root Mean Square Error

mmodel_MSE <- mean(mmodel_sq_residuals)
mmodel_MSE

mmodel_RSME <- sqrt(mmodel_MSE)
mmodel_RSME


# 10.3.9. Specifying the Mean Model in R

mmodel <- lm(MusicPerceptionScores ~ 1, data = SE_MPMA)
mmodel

head(model.matrix(mmodel))

anova(mmodel)
anova(mmodel)[1]
anova(mmodel)[2]
anova(mmodel)[3]

summary(mmodel)
attributes(summary(mmodel))
mmodel$coefficients
str(summary(mmodel))

summary(mmodel)$df
quantile(mmodel$residuals)
head(mmodel$residuals)
summary(mmodel)$coefficients
summary(mmodel)$coefficients[1]
summary(mmodel)$coefficients[2]
summary(mmodel)$coefficients[3]
summary(mmodel)$coefficients[4]
summary(mmodel)$sigma
summary(mmodel)$df[2]

# 10.3.9.1. Calculating Confidence Intervals for b0

confint(mmodel, level = 0.95)

# 10.4.1.1. Calculating Ordinary Least Squares

par(mfrow = c(1, 2))

stripchart(SE_MPMA$MusicPerceptionScores,
           main = " Plot A \n Mean Model: Music Perception",
           ylab = "Music Perception",
           vertical = TRUE,
           pch = 19,
           cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()

plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = " Plot B \n Bivariate Model: Music Perception as a Function of Music Aptitude",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()


mmodel_RSS

sum_XX <- sum((SE_MPMA$MusicAptitudeScores - mean(SE_MPMA$MusicAptitudeScores)) ^ 2)
sum_XX

sum_XY <- sum((SE_MPMA$MusicAptitudeScores - mean(SE_MPMA$MusicAptitudeScores)) * (SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)))
sum_XY

bvmodel_slope <- sum_XY/sum_XX
bvmodel_slope

bvmodel_intercept <- mean(SE_MPMA$MusicPerceptionScores) - (bvmodel_slope * mean(SE_MPMA$MusicAptitudeScores))
bvmodel_intercept

bvmodel <- lm(MusicPerceptionScores ~ MusicAptitudeScores, data = SE_MPMA)
bvmodel

par(mfrow = c(1,1))

plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = "Scatterplot of Music Aptitude and Music Perception (N = 150)",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
abline(bvmodel, lty = 1)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()

# 10.4.1.2. Expressing the Least Squares Line of Best Fit

range(SE_MPMA$MusicAptitudeScores)
range(SE_MPMA$MusicPerceptionScores)

mean(SE_MPMA$MusicAptitudeScores)
mean(SE_MPMA$MusicPerceptionScores)

# 10.4.2. Decomposing Residuals of the Bivariate Model

# 10.4.2.1. Unexplained Variance: Residual Sum of Squares (RSS)

head(SE_MPMA$MusicPerceptionScores)
head(mmodel$fitted.values)

head(SE_MPMA$MusicPerceptionScores)
head(bvmodel$fitted.values)

bvmodel_RSS <- sum((SE_MPMA$MusicPerceptionScores - bvmodel$fitted.values) ^ 2)
bvmodel_RSS

par(mfrow = c(2,1))

plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = "Residual Sum of Squares (RSS): \n Difference Between Observed Scores (y) and Line of Best Fit (y-hat)",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
abline(bvmodel, lty = 1)
grid()

plot(SE_MPMA$MusicPerceptionScores,
     main = "Residual Sum of Squares (RSS): \n Difference Between Observed Scores (y) and Fitted Scores (y-hat)",
     xlab = "Observations",
     ylab = "y and y-hat values",
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
points(bvmodel$fitted.values, 
       pch = 19,
       col="gray",
       lwd = 2,
       cex = .6)
grid()

# 10.4.2.2. Explained Variance: Explained Sum of Squares (ESS)

bvmodel_ESS <- sum((bvmodel$fitted.values - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)
bvmodel_ESS

par(mfrow = c(2,1))

plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = "Scatterplot of Music Perception as a Function of Music Aptitude",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
abline(bvmodel, lty = 1)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()

plot(bvmodel$fitted.values,
     main = "RSS: Fitted Values (y-hat)",
     ylab = "Music Perception (y-hat)",
     xlab = "Observation IDs",
     pch = 19,
     col = "gray",
     cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()

# 10.4.2.3. Total Variance: Total Sum of Squares (TSS)

bvmodel_TSS <- sum((SE_MPMA$MusicPerceptionScores - mean(SE_MPMA$MusicPerceptionScores)) ^ 2)
bvmodel_TSS

bvmodel_ESS + bvmodel_RSS

mmodel_RSS
bvmodel_TSS

# 10.4.3. Quantifying the Quality of the Line of Best Fit: R2 and Adjusted R2

mmodel_RSS
bvmodel_RSS

bvmodel_SSR <- mmodel_RSS - bvmodel_RSS 
bvmodel_SSR

bvmodel_SSR 
bvmodel_ESS

bvmodel_Rsq_a <- bvmodel_ESS/mmodel_RSS
bvmodel_Rsq_b <- bvmodel_ESS/bvmodel_TSS

bvmodel_Rsq_a
bvmodel_Rsq_b

cor(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)

(cor(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)) ^ 2

bvmodel_Rsq_adj <- 1 - (((1 - bvmodel_Rsq_a) * (length(SE_MPMA$MusicPerceptionScores) - 1)) / (length(SE_MPMA$MusicPerceptionScores) - 1 - 1))
bvmodel_Rsq_adj

# 10.4.4. Examining the Distribution of the Residuals 

summary(bvmodel$residuals)

par(mfrow = c(2, 1))

hist(bvmodel$residuals,
     main = "Histogram of Bivariate Model Residuals",
     xlab = "Residual Values",
     ylab = "Frequency (N = 150)")

plot(bvmodel$residuals,
     main = "Scatterplot of Bivariate Model Residuals",
     xlab = "Observations",
     ylab = "Residual Values",
     pch = 19,
     cex = .6)

mean(bvmodel$residuals)
sd(bvmodel$residuals)

# 10.4.5. Examining the Precision of the Model: Standard Errors

bvmodel_SEE <- (sd(bvmodel$residuals)/sqrt(sum_XX))
bvmodel_SEE

bvmodel_MSE <- bvmodel_RSS/(length(SE_MPMA$MusicPerceptionScores) - 1 - 1)
bvmodel_MSE

bvmodel_RSME <- sqrt(bvmodel_MSE)
bvmodel_RSME

bvmodel_SEI <- sqrt(bvmodel_MSE * ((1/length(SE_MPMA$MusicAptitudeScores)) + ((mean(SE_MPMA$MusicAptitudeScores)^2) /bvmodel_ESS)))
bvmodel_SEI

# 10.4.6. Statistical Significance: F-tests and p-values

mmodel_RSS
bvmodel_TSS
bvmodel_RSS
bvmodel_SSR
bvmodel_ESS

bvmodel_Fstatistic <- bvmodel_SSR/bvmodel_MSE
bvmodel_Fstatistic

bvmodel_Fcritical <- qf(p = .05, df1 = 1, df2 = 148)
bvmodel_Fcritical

bvmodel_F_pvalue <- pf(bvmodel_Fstatistic, df1 = 1, df2 = 148, lower.tail = FALSE)
bvmodel_F_pvalue

n <- nrow(SE_MPMA)
MPMA_cortest <- cor.test(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)
bvmodel_t_statistic <- (MPMA_cortest$estimate * sqrt(n - 2))/sqrt(1 - (MPMA_cortest$estimate ^ 2))
bvmodel_t_statistic

bvmodel_pval <- 2 * min(pt(MPMA_cortest$statistic, MPMA_cortest$parameter), pt(MPMA_cortest$statistic, MPMA_cortest$parameter, lower.tail = FALSE))
bvmodel_pval

# 10.4.7. Specifying the Simple Linear Regression Model in R 

bvmodel <- lm(MusicPerceptionScores ~ MusicAptitudeScores, data = SE_MPMA)
bvmodel

anova(bvmodel)
anova(bvmodel)[1,1]
anova(bvmodel)[1,2]
anova(bvmodel)[1,3]
anova(bvmodel)[1,4]
anova(bvmodel)[1,5]
anova(bvmodel)[2,1]
anova(bvmodel)[2,2]
anova(bvmodel)[2,3]

summary(bvmodel)

quantile(bvmodel$residuals)
head(bvmodel$residuals)
summary(bvmodel$residuals)
summary(bvmodel)$coefficients
summary(bvmodel)$coefficients[1,1]
summary(bvmodel)$coefficients[1,2]
summary(bvmodel)$coefficients[3]
summary(bvmodel)$coefficients[4]
summary(bvmodel)$coefficients[2,1]
head(bvmodel$fitted.values)
summary(bvmodel)$coefficients[2,2]
summary(bvmodel)$coefficients[2,3]
summary(bvmodel)$coefficients[2,4]
summary(bvmodel)$sigma
summary(bvmodel)$df
summary(bvmodel)$r.squared
summary(bvmodel)$adj.r.squared
summary(bvmodel)$fstatistic[1]
summary(bvmodel)$fstatistic[2]
summary(bvmodel)$fstatistic[3]

# 10.4.7.1. Calculating Confidence Intervals for b0 and b1

summary(bvmodel)$coefficients

b0 <- summary(bvmodel)$coefficients[1,1]
SEb0 <- summary(bvmodel)$coefficients[1,2]

b0
SEb0

cbind(LowerCI = ((b0) - 1.96 * (SEb0)), UpperCI = ((b0) + 1.96 * (SEb0)))

b1 <- summary(bvmodel)$coefficients[2,1]
SEb1 <- summary(bvmodel)$coefficients[2,2]

b1
SEb1

cbind(LowerCI = ((b1) - 1.96 * (SEb1)), UpperCI = ((b1) + 1.96 * (SEb1)))
confint(bvmodel, level = 0.95)

# 10.4.8. Forecasting

MA_predict_value <- data.frame(MusicAptitudeScores = 1.25)
predict(bvmodel, MA_predict_value, interval = "predict", se.fit = TRUE)

# Load New_MA_Data.csv

New_MA_Data <- read.csv(file.choose(), header = TRUE)

str(New_MA_Data)

bvmodel$call
names(New_MA_Data)

colnames(New_MA_Data)[1] <- "MusicAptitudeScores"
names(New_MA_Data)

prediction <- predict(object = bvmodel, newdata = New_MA_Data, se.fit = TRUE)
head(predict(object = bvmodel, newdata = New_MA_Data), 5)

head(prediction$fit)
head(prediction$se.fit)

predict(bvmodel, New_MA_Data, interval = "predict")
head(predict(bvmodel, New_MA_Data, interval = "predict"), 5)

new_xvalues <- seq(min(SE_MPMA$MusicAptitudeScores), max(SE_MPMA$MusicAptitudeScores), length.out = 150)

CIs <- predict(bvmodel, newdata = data.frame(MusicAptitudeScores = new_xvalues), interval = 'confidence')

PIs <- predict(bvmodel, newdata=data.frame(MusicAptitudeScores = new_xvalues), interval="prediction", level = 0.95)

par(mfrow = c(1,1))

plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = "Scatterplot of Music Aptitude and Music Perception (N = 150)",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "black",
     lwd = 2,
     cex = .6)
grid()
abline(lm(SE_MPMA$MusicPerceptionScores ~ SE_MPMA$MusicAptitudeScores), lty = 1)
polygon(c(rev(new_xvalues), new_xvalues), c(rev(CIs[ ,3]), CIs[ ,2]), col=gray(.3, .2), border = NA)
lines(new_xvalues, CIs[ ,3], lty = 2, col = 'black')
lines(new_xvalues, CIs[ ,2], lty = 2, col = 'black')
lines(new_xvalues, PIs[ ,2], lty = 2, col="gray70")
lines(new_xvalues, PIs[ ,3], lty = 2, col="gray70")

# 10.4.9. Calculating Standardized Coefficients and Effect Size

bvmodel_slope * (sd(SE_MPMA$MusicAptitudeScores) / sd(SE_MPMA$MusicPerceptionScores))

bvmodel_standardized <- lm(scale(MusicPerceptionScores) ~ scale(MusicAptitudeScores), data = SE_MPMA)
bvmodel_standardized

library(effectsize)

standardize_parameters(bvmodel)

# 10.5. Closing Out the Chapter

save_workspace
save_history
