# Chapter 13 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("Diagnostics.Rdata")

# Save R History file

save_history <- savehistory("Diagnostics.Rhistory")

# Packages Needed for Chapter 13

install.packages("car")
library(car)

install.packages("corpcor")
library(corpcor)

install.packages("ez")
library(ez)

install.packages("gvlma")
library(gvlma)

install.packages("Hmisc")
library(Hmisc)

install.packages("lmtest")
library(lmtest)

install.packages("MASS")
library(MASS)

install.packages("mctest")
library(mctest)

install.packages("nortest")
library(nortest)

install.packages("olsrr")
library(olsrr)

install.packages("ppcor")
library(ppcor)

# Load SE_MPMA.R script file

bvmodel <- lm(MusicPerceptionScores ~ MusicAptitudeScores, data = SE_MPMA)

MusicPerceptionScores <- SE_MPMA$MusicPerceptionScores
MusicAptitudeScores <- SE_MPMA$MusicAptitudeScores
Fit <- fitted(bvmodel)
RawResiduals <- residuals(bvmodel)

library(MASS)
SzdResiduals <- stdres(bvmodel)

library(MASS)
StdResiduals <- studres(bvmodel)

Hat <- hatvalues(bvmodel)
Sigma <- sigma(bvmodel)
CooksDistance <- cooks.distance(bvmodel)

model_metrics <- data.frame(MusicPerceptionScores, MusicAptitudeScores, Fit, RawResiduals, SzdResiduals, StdResiduals, Hat, Sigma, CooksDistance)
str(model_metrics)

par(mfrow = c(2, 2))

diagnostic_plots <- plot(bvmodel)

# 13.2. Assumption: The Relationship Between the Predictor and Outcome Variables is Linear 

par(mfrow = c(1, 1))

plot(bvmodel, 1)

library(car)
crPlots(bvmodel)

cor.test(model_metrics$RawResiduals, model_metrics$Fit)

# 13.3. Assumption: Independence of Residuals’ Error Terms

# 13.3.1. Identifying Outlier Cases

library(car)

outlierTest(bvmodel)

bvmodel_modified <- lm(MusicPerceptionScores ~ MusicAptitudeScores, data = SE_MPMA[-c(1), ])
outlierTest(bvmodel_modified)

# 13.3.2. Identifying Cases with High Leverage Values

p <- length(coefficients(bvmodel))
n <- length(fitted(bvmodel))

p
n

average_hat <- p / n
average_hat

which(model_metrics$Hat > (2*average_hat))
length(which(model_metrics$Hat > (2*average_hat)))

which(model_metrics$Hat > (3*average_hat))
length(which(model_metrics$Hat > (3*average_hat)))

high_hat <- model_metrics[order(-model_metrics$Hat), ]
high_hat <- subset(high_hat, Hat > (2*average_hat))

plot(model_metrics$Hat,
     main = "Plot of Leverage Values Across Cases (N = 150)",
     ylab = "Leverage Values",
     xlab = "Case",
     pch = 19,
     cex = .6)
abline(h = 2*average_hat, lty = 2)
abline(h = 3*average_hat, lty = 2)
text(model_metrics$Hat, cex = .5, pos = 3)

# 13.3.3. Identifying Influential Cases

CooksCutoff <- 4/(nrow(SE_MPMA) - length(bvmodel$coefficients) - 2)
CooksCutoff

which(model_metrics$CooksDistance > CooksCutoff)
length(which(model_metrics$CooksDistance > CooksCutoff))

high_cooksd <- model_metrics[order(-model_metrics$CooksDistance), ]
high_cooksd <- subset(high_cooksd, CooksDistance > CooksCutoff)

library(olsrr)

ols_plot_cooksd_chart(bvmodel)

plot(bvmodel, 5)

library(car)

leveragePlots(bvmodel)

library(car)

influencePlot(bvmodel)

library(car)

durbinWatsonTest(bvmodel)

acf(model_metrics$RawResiduals,
    main = "Autocorrelation Estimates for Model Residuals")

# 13.4. Assumption: Residual Errors are Normally Distributed

plot(bvmodel, 2)

library(car)

qqPlot(bvmodel)

par(mfrow = c(1, 1))

hist(model_metrics$StdResiduals, freq = FALSE, main= "Distribution of Studentized Residuals")
xfit <- seq(min(model_metrics$StdResiduals), max(model_metrics$StdResiduals), length = 150)
yfit <- dnorm(xfit)
lines(xfit, yfit)
rug(jitter(model_metrics$StdResiduals))

which(model_metrics$StdResiduals < -4.00)

par(mfrow = c(1, 1))

hist(model_metrics$StdResiduals[-c(1)], freq = FALSE, main= "Distribution of Studentized Residuals")
xfit <- seq(min(model_metrics$StdResiduals[-c(1)]), max(model_metrics$StdResiduals[-c(1)]), length = 150)
yfit <- dnorm(xfit)
lines(xfit, yfit)
rug(jitter(model_metrics$StdResiduals[-c(1)]))

mean(model_metrics$RawResiduals)
mean(model_metrics$SzdResiduals)
mean(model_metrics$StdResiduals)

library(nortest)

ad.test(model_metrics$SzdResiduals)


# 13.5. Assumption: Homogeneity of Residuals’ Variance (i.e., Homoscedasticity)

cor.test(model_metrics$RawResiduals, model_metrics$Fit)
cor.test(model_metrics$StdResiduals, model_metrics$Fit)
cor.test(model_metrics$SzdResiduals, model_metrics$Fit)

plot(bvmodel, 3)

library(car)

spreadLevelPlot(bvmodel)

library(car)

ncvTest(bvmodel)

library(lmtest)

bptest(bvmodel)
bptest(bvmodel, studentize = FALSE)


# 13.6. Global Test of Model Assumptions

library(gvlma)

model_assessment <- gvlma(bvmodel)
model_assessment

gvlma(bvmodel_modified)

# 13.7. Assumption: Multicollinearity Does Not Exist Between Predictor Variables

# Load SelfEfficacy.csv

SelfEfficacy <- read.csv(file.choose(), header = TRUE)

str(SelfEfficacy)

SelfEfficacy_matrix <- as.matrix(SelfEfficacy)
class(SelfEfficacy_matrix)

library(Hmisc)

predictor_cormatrix <- rcorr(SelfEfficacy_matrix[, 1:3])
round(predictor_cormatrix$r, digits = 2)

library(corpcor)

round(cor2pcor(predictor_cormatrix$r), digits = 2)

SelfEfficacy_model <- lm(PhysiologicalFeedback ~ PerformanceOutcomes + VicariousExperiences + VerbalPersuasion, data = SelfEfficacy)

library(olsrr)

ols_vif_tol(SelfEfficacy_model)

library(olsrr)

round(ols_eigen_cindex(SelfEfficacy_model), digits = 2)

library(olsrr)

ols_coll_diag(SelfEfficacy_model)

library(mctest)

omcdiag(SelfEfficacy_model)

library(olsrr)

imcdiag(SelfEfficacy_model)

library(ppcor)

pcor(SelfEfficacy[, 1:3], method = "pearson")

# 13.8. Assumption: Homogeneity of Variances

# 13.8.1. Two Levels

# Load PerformanceAnxiety.csv

PerformanceAnxiety <- read.csv(file.choose(), header = TRUE) 

PerformanceAnxiety$SexType <- factor(PerformanceAnxiety$SexType, levels = c("Female", "Male"))

var.test(PerformanceAnxiety ~ SexType, data = PerformanceAnxiety)

aggregate(PerformanceAnxiety ~ SexType, data = PerformanceAnxiety, var)


# 13.8.2. Three or More Levels

# Load eartraining.csv

EarTraining <- read.csv(file.choose(), header = TRUE)

EarTraining$instrument <- factor(EarTraining$instrument, levels = c("brass", "piano", "woodwinds"))

bartlett.test(eartraining ~ instrument, data = EarTraining)

library(car)

leveneTest(eartraining ~ instrument, data = EarTraining)

boxplot(eartraining ~ instrument, data = EarTraining)

aggregate(eartraining ~ instrument, data = EarTraining, var)

# 13.8.3. Within-Subjects Designs

# Load recall.csv

recall <- read.csv(file.choose(), header = TRUE)

recall$TimeSpan <- factor(recall$TimeSpan, levels = c("NextDay", "Week", "Month"))

library(ez)

Mauchly <- ezANOVA(data = recall,
                   wid = Participant_ID,
                   within = TimeSpan,
                   dv = Recall)

Mauchly$`Mauchly's Test for Sphericity`

# Closing Out the Chapter

save_workspace
save_history

