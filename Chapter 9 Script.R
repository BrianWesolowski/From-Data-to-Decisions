# Chapter 9 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("Relationships.Rdata")

# Save R History file

save_history <- savehistory("Relationships.Rhistory")

# Packages Needed for Chapter 9

install.packages("corrplot")
library(corrplot)

install.packages("Hmisc")
library(Hmisc)

install.packages("psych")
library(psych)

# Load SE_MPMA.R Script file

# 9.2. Covariance

var(SE_MPMA$MusicPerceptionScores)

stripchart(SE_MPMA$MusicPerceptionScores,
           main = "Distribution of Music Perception Scores Compared to Mean \n Mean = -0.001466667",
           ylab = "Music Perception",
           vertical = TRUE,
           pch = 19,
           cex = .6)
abline(h = mean(SE_MPMA$MusicPerceptionScores), col = "black", lty = 2)
grid()

# 9.3. Assumptions Based on the Design of the Stored Data

length(SE_MPMA$MusicAptitudeScores)
length(SE_MPMA$MusicPerceptionScores)

SE_MPMA$MusicAptitudeScores[1]
SE_MPMA$MusicPerceptionScores[1]

SE_MPMA$MusicAptitudeScores[2]
SE_MPMA$MusicPerceptionScores[2]

# 9.4. Examining Covariance

par(mfrow = c(1,1))
plot(MusicPerceptionScores ~ MusicAptitudeScores,
     main = "Scatterplot of Music Aptitude and Music Perception (N = 150)",
     xlab = "Music Aptitude",
     ylab = "Music Perception",
     data = SE_MPMA,
     pch = 19,
     col = "gray",
     lwd = 2,
     cex = 1.2)
text(MusicPerceptionScores ~ MusicAptitudeScores, labels = rownames(SE_MPMA), data = SE_MPMA, cex = 0.3, font = 2)
grid()

n <- nrow(SE_MPMA)
n

dif_MP <- (SE_MPMA$MusicPerceptionScores) - mean(SE_MPMA$MusicPerceptionScores)
head(dif_MP)

dif_MA <- (SE_MPMA$MusicAptitudeScores) - mean(SE_MPMA$MusicAptitudeScores)
head(dif_MA)

mult_MP_MA <- (dif_MP * dif_MA)
head(mult_MP_MA)

mult_MP_MA_add <- sum(mult_MP_MA)
mult_MP_MA_add

cov_MPMA <- mult_MP_MA_add/(n - 1)
cov_MPMA

cov(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)

# 9.5. Correlation

cov_MPMA

SDMP_by_SDMA <- (sd(SE_MPMA$MusicPerceptionScores)) * (sd(SE_MPMA$MusicAptitudeScores))
SDMP_by_SDMA

cov_MPMA/SDMP_by_SDMA

cor(SE_MPMA$MusicPerceptionScores,SE_MPMA$MusicAptitudeScores)

# 9.6. Properties of the Correlation Coefficient

cor.test(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)

# 9.7. Attributes and Structure of the cor.test() Function

MPMA_cortest <- cor.test(SE_MPMA$MusicPerceptionScores, SE_MPMA$MusicAptitudeScores)
attributes(MPMA_cortest)

str(MPMA_cortest)

# 9.8. Test of Significance Using the t-Statistic

n

MPMA_cortest$estimate

t_statistic <- (MPMA_cortest$estimate * sqrt(n - 2)) / sqrt(1 - (MPMA_cortest$estimate ^ 2))
t_statistic

MPMA_cortest$statistic

t_critical <- qt(c(.025, .975), df = n - 2)
t_critical

qt(p = .05, df = n - 2, lower.tail = TRUE)

qt(p = .05, df = n - 2, lower.tail = FALSE)

MPMA_cortest$statistic > t_critical[2]

# 9.9. Inference and Correlation: Test of Significance Using p-values

MPMA_cortest$p.value

pval <- 2 * min(pt(MPMA_cortest$statistic, MPMA_cortest$parameter), pt(MPMA_cortest$statistic, MPMA_cortest$parameter, lower.tail = FALSE))
pval

pval <- 2 * pt(q = MPMA_cortest$statistic, df = MPMA_cortest$parameter, lower.tail = FALSE)
pval

# 9.10 Confidence Intervals for Population Pearson’s ρ 

MPMA_cortest$conf.int

library(psych)

fisherz <- fisherz(MPMA_cortest$estimate)
fisherz

r.con(MPMA_cortest$estimate, n, p = .95, twotailed = TRUE)

# 9.11. Multiple Correlation

# Load SelfEfficacy.csv

SelfEfficacy <- read.csv(file.choose(), header = TRUE)

str(SelfEfficacy)

head(SelfEfficacy)

library(psych)

describe(SelfEfficacy)

plot(SelfEfficacy)

SelfEfficacy_cov <- round(cov(SelfEfficacy), digits = 2)
SelfEfficacy_cov

SelfEfficacy_cor <- round(cor(SelfEfficacy), digits = 2)
SelfEfficacy_cor

library(psych)

pairs.panels(SelfEfficacy)

library(Hmisc)

SelfEfficacy_matrix <- as.matrix(SelfEfficacy)
class(SelfEfficacy_matrix)

rcorr(SelfEfficacy_matrix)

SelfEfficacy_rp <- rcorr(SelfEfficacy_matrix)

round(SelfEfficacy_rp$r, digits = 2)
round(SelfEfficacy_rp$P, digits = 2)

cor.test(SelfEfficacy$PerformanceOutcomes, SelfEfficacy$PhysiologicalFeedback)

library(corrplot)

SelfEfficacy_p_CI <- cor.mtest(SelfEfficacy, conf.level = .95)
SelfEfficacy_p_CI

corrplot(SelfEfficacy_cor, method = "circle")

corrplot(SelfEfficacy_cor, method = "circle", type = "full")
corrplot(SelfEfficacy_cor, method = "circle", type = "upper")
corrplot(SelfEfficacy_cor, method = "circle", type = "lower")

par(mfrow = c(1, 1))
corrplot(SelfEfficacy_cor, p.mat = SelfEfficacy_p_CI$p, insig = "blank")

# Create labels

conf <- paste0("[", format(SelfEfficacy_p_CI$lowCI, digits = 1), ":", format(SelfEfficacy_p_CI$uppCI, digits = 2), "]")
conf

# Plot positions

row <- row(SelfEfficacy_p_CI$p)
col <- (ncol(SelfEfficacy_p_CI$p) + 1) - col(SelfEfficacy_p_CI$p)
corrplot(SelfEfficacy_cor, method = "number")
text(row, col, conf, pos = 1, cex = 0.6)  

# 9.12. Closing Out the Chapter

save_workspace
save_history
