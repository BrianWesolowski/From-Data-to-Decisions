# Chapter 11 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("MultipleRegression.Rdata")

# Save R History file

save_history <- savehistory("MultipleRegression.Rhistory")

# Packages Needed for Chapter 11

install.packages("car")
library(car)

install.packages("effects")
library(effects)

install.packages("effectsize")
library(effectsize)

install.packages("emmeans")
library(emmeans)

install.packages("ggeffects")
library(ggeffects)

install.packages("ggplot2")
library(ggplot2)

install.packages("interactions")
library(interactions)

install.packages("interplot")
library(interplot)

install.packages("moderndive")
library(moderndive)

install.packages("psych")
library(psych)

install.packages("sandwich")
library(sandwich)

# 11.2. Continuous by Continuous Multiple Regression

# Load PracticeMotivation.csv

PracticeMotivation <- read.csv(file.choose(), header = TRUE)

str(PracticeMotivation)

library(psych)

describe(PracticeMotivation)

# 11.2.2. The Additive Multiple Linear Regression Model

PracticeMotivation_additive <- lm(PracticeMotivation ~ SelfRegulation + Desire, data = PracticeMotivation)

summary(PracticeMotivation_additive)

PracticeMotivation_additive

round(confint(PracticeMotivation_additive, level = 0.95), digits = 2)

library(effectsize)

standardize_parameters(PracticeMotivation_additive)

# 11.2.2.1. Analysis of Residuals

anova(PracticeMotivation_additive)

# 11.2.2.2. Plotting the Main Effects

library(ggeffects)
library(ggplot2)

SelfRegulation_meffect <- ggpredict(PracticeMotivation_additive, terms = "SelfRegulation")

plot(SelfRegulation_meffect, add.data = TRUE) +
  labs(
    x = "Self-Regulation",
    y = "Practice Motivation",
    title = "Main Effect of Self-Regulation on Practice Motivation"
  )

Desire_meffect <- ggpredict(PracticeMotivation_additive, terms = "Desire")

plot(Desire_meffect, add.data = TRUE) +
  labs(
    x = "Desire for Achievement",
    y = "Practice Motivation",
    title = "Main Effect of Desire for Achievement on Practice Motivation"
  )

# 11.2.2.3. Forecasting

Motivation_pred <- data.frame(SelfRegulation = 2.75, Desire = 12.03)

predict(PracticeMotivation_additive, Motivation_pred, interval = "predict", se.fit = TRUE)


# 11.3. Continuous by Continuous Multiple Regression with a Moderator Variable

# 11.3.1. Preprocessing the Predictor Variables

SelfRegulation_meancentered <- scale(PracticeMotivation$SelfRegulation, scale = FALSE)

Desire_meancentered <- scale(PracticeMotivation$Desire, scale = FALSE)

class(SelfRegulation_meancentered)

class(Desire_meancentered)

SelfRegulation_meancentered <- SelfRegulation_meancentered[ , 1]

Desire_meancentered <- Desire_meancentered[ , 1]

class(SelfRegulation_meancentered)
class(Desire_meancentered)

library(psych)

describe(PracticeMotivation$SelfRegulation)
describe(SelfRegulation_meancentered)
describe(PracticeMotivation$Desire)

PracticeMotivation_meancentered <- data.frame(PracticeMotivation$PracticeMotivation, SelfRegulation_meancentered, Desire_meancentered)
names(PracticeMotivation_meancentered)
colnames(PracticeMotivation_meancentered) <- c("PracticeMotivation", "SelfRegulation", "Desire")
names(PracticeMotivation_meancentered)

# 11.3.2. The Multiplicative Multiple Linear Regression Model

PracticeMotivation_interaction <- lm(PracticeMotivation ~ SelfRegulation + Desire + SelfRegulation*Desire, data = PracticeMotivation_meancentered) 
summary(PracticeMotivation_interaction)

PracticeMotivation_interaction

round(confint(PracticeMotivation_interaction), digits = 2)

library(effectsize)

standardize_parameters(PracticeMotivation_interaction)

# 11.3.2.1. Analysis of Residuals

anova(PracticeMotivation_interaction)

# 11.3.2.2. Plotting the Interaction Effect

library(interplot)

interplot(m = PracticeMotivation_interaction, var1 = "SelfRegulation", var2 = "Desire", hist = TRUE) +
  xlab("Desire for Achievement") +
  ylab("Self-Regulation") +
  ggtitle("Interaction Effect Between Self-Regulation and Desire for Achievement") +
  geom_hline(yintercept = 0.00, linetype = "dashed")

# 11.3.2.3. Plotting the Simple Slopes

simpleslopes_plot <- ggpredict(PracticeMotivation_interaction, c("SelfRegulation", "Desire"))
simpleslopes_plot

plot(simpleslopes_plot, ci=TRUE, add.data = TRUE) +
  labs(x = "Self-Regulation",
       y = "Practice Motivation",
       title = "Self-Regulation and Desire for Achievement as Predictors of Practice Motivation",
       color = "Desire for Achievement +/- 1 SD")  

library(effects)

simple_slopes <- effect('SelfRegulation * Desire', PracticeMotivation_interaction,
                        xlevels = list(Desire = c(-1, 0, 1),
                                       SelfRegulation = c(-3, -2, -1, 0, 1, 2, 3)),
                        se = TRUE, confidence.level=.95, typical = mean)

simple_slopes <- as.data.frame(simple_slopes)
simple_slopes

# 11.3.2.4. Testing the Simple Slopes

library(interactions)
library(sandwich)

sim_slopes(PracticeMotivation_interaction, pred = SelfRegulation, modx = "Desire", cond.int = TRUE)

johnson_neyman(PracticeMotivation_interaction, pred = SelfRegulation, modx = Desire, alpha = .05)


# 11.4. Continuous by Categorical Multiple Regression

# Load PerformanceAnxiety.csv

PerformanceAnxiety <- read.csv(file.choose(), header = TRUE)

str(PerformanceAnxiety)

PerformanceAnxiety$SexType <- factor(PerformanceAnxiety$SexType, levels = c("Female", "Male"))

str(PerformanceAnxiety)

library(psych)

describe(PerformanceAnxiety)

describeBy(PerformanceAnxiety, group = PerformanceAnxiety$SexType)

# 11.4.1. Preprocessing the Predictor Variables

class(PerformanceAnxiety$SexType)
contrasts(PerformanceAnxiety$SexType)
levels(PerformanceAnxiety$SexType)

PerformanceAnxiety$SexType <- factor(PerformanceAnxiety$SexType,
                                     levels = c("Male","Female")) 

levels(PerformanceAnxiety$SexType)
contrasts(PerformanceAnxiety$SexType)

colnames(contrasts(PerformanceAnxiety$SexType)) <- c("Female_compared_to_Male")
contrasts(PerformanceAnxiety$SexType)
head(PerformanceAnxiety$SexType, 5)

library(psych)

SexType_dummy <- dummy.code(PerformanceAnxiety$SexType)
class(SexType_dummy)
head(SexType_dummy)

# 11.4.2. The Additive Multiple Linear Regression Model

PerformanceAnxiety_additive <- lm(PerformanceAnxiety ~ Perfectionism + SexType, data = PerformanceAnxiety)
summary(PerformanceAnxiety_additive)

PerformanceAnxiety_additive

round(confint(PerformanceAnxiety_additive, level = 0.95), digits = 2)

library(effectsize)

standardize_parameters(PerformanceAnxiety_additive)

# 11.4.2.1. Analysis of Residuals

table(PerformanceAnxiety$SexType)

library(car)

Anova(PerformanceAnxiety_additive, type = 2)

# 11.4.2.2. Plotting the Main Effects

library(ggeffects)
library(ggplot2)

Perfectionism_meffect <- ggpredict(PerformanceAnxiety_additive, terms = "Perfectionism")

plot(Perfectionism_meffect, add.data = TRUE) +
  labs(
    x = "Perfectionism",
    y = "Performance Anxiety",
    title = "Main Effect of Perfectionism on Performance Anxiety" 
  )

SexType_meffect <- ggpredict(PerformanceAnxiety_additive, terms = "SexType")

plot(SexType_meffect, add.data = TRUE, connect.lines = TRUE) +
  labs(
    x = "Sex-Type",
    y = "Performance Anxiety",
    title = "Main Effect of Sex-Type on Performance Anxiety"
  )

library(emmeans)

emmeans(PerformanceAnxiety_additive, pairwise ~ SexType, contr = TRUE)

library(ggplot2)
library(moderndive)

ggplot(PerformanceAnxiety, aes(x = Perfectionism, y = PerformanceAnxiety, color = SexType)) +
  labs(x = "Perfectionism",
       y = "Performance Anxiety",
       title = "Parallel Slopes for Sex-Type") +
  geom_point() +
  geom_parallel_slopes(se = FALSE) +
  theme_bw() +
  theme(panel.border = element_blank())

# 11.4.2.3. Forecasting

Anxiety_pred <- data.frame(Perfectionism = 0.75, SexType = "Male")

predict(PerformanceAnxiety_additive, Anxiety_pred, interval = "predict", se.fit = TRUE)


# 11.5. Continuous by Categorical Multiple Regression with a Moderator Variable

Perfectionism_meancentered <- scale(PerformanceAnxiety$Perfectionism, scale = FALSE)

library(psych)

describe(Perfectionism_meancentered)

class(Perfectionism_meancentered)

Perfectionism_meancentered <- Perfectionism_meancentered[ , 1]

class(Perfectionism_meancentered)

PerformanceAnxiety_meancentered <- data.frame(PerformanceAnxiety$PerformanceAnxiety, Perfectionism_meancentered, PerformanceAnxiety$SexType)
names(PerformanceAnxiety_meancentered)
colnames(PerformanceAnxiety_meancentered) <- c("PerformanceAnxiety", "Perfectionism", "SexType")
names(PerformanceAnxiety_meancentered)

# 11.5.2. The Multiplicative Multiple Linear Regression Model

PerformanceAnxiety_Interaction <- lm(PerformanceAnxiety ~ Perfectionism + SexType + Perfectionism*SexType, data = PerformanceAnxiety_meancentered)
summary(PerformanceAnxiety_Interaction)

round(confint(PerformanceAnxiety_Interaction), digits = 2)

library(effectsize)

standardize_parameters(PerformanceAnxiety_Interaction)


# 11.5.2.1. Analysis of Residuals

library(car)

Anova(PerformanceAnxiety_Interaction, type = 2)


# 11.5.2.2. Plotting the Interaction Effect

PerformanceAnxiety_numeric <- data.frame(PerformanceAnxiety$PerformanceAnxiety, Perfectionism_meancentered, SexType_dummy[,1])
colnames(PerformanceAnxiety_numeric) <- c("PerformanceAnxiety", "Perfectionism", "SexType")
PerformanceAnxiety_Interaction_numeric <- lm(PerformanceAnxiety ~ Perfectionism * SexType, data = PerformanceAnxiety_numeric)

library(interplot)

interplot(PerformanceAnxiety_Interaction_numeric, var1 = "Perfectionism", var2 = "SexType") +
  xlab("Sex-Type") +
  ylab("Conditional Effect of Perfectionism") +
  ggtitle("Interaction Effect Between Perfectionism and Sex-Type")+
  geom_hline(yintercept = 0.00, linetype = "dashed")

# 11.5.2.3. Plotting the Simple Effects

library(emmeans)

emtrends(PerformanceAnxiety_Interaction, ~ SexType, var = "Perfectionism")

library(ggeffects)
library(ggplot2)

simpleeffects_plot <- ggpredict(PerformanceAnxiety_Interaction, c("Perfectionism", "SexType"))

simpleeffects_plot

plot(simpleeffects_plot, ci = TRUE, add.data = TRUE) +
  labs(x = "Perfectionism",
       y = "Performance Anxiety",
       title = "Simple Effects of Sex-Type",
       color = "Sex-Type")  

library(effects)

simple_effects <- effect('Perfectionism * SexType', PerformanceAnxiety_Interaction,
                         xlevels = list(SexType = c("Male", "Female"),
                                        Perfectionism = c(-3, -2, -1, 0, 1, 2, 3)),
                         se = TRUE, confidence.level = .95, typical = mean)

simple_effects <- as.data.frame(simple_effects)
simple_effects

# 11.5.2.4. Testing the Simple Effects

library(emmeans)

emtrends(PerformanceAnxiety_Interaction, pairwise ~ SexType, var = "Perfectionism")


# 11.6. Categorical by Categorical Multiple Regression

# Load PerformanceAchievement.csv

PerformanceAchievement <- read.csv(file.choose(), header = TRUE)

str(PerformanceAchievement)

PerformanceAchievement$HomeMusic <- factor(PerformanceAchievement$HomeMusic, levels = c("Much", "Some", "None"))
PerformanceAchievement$PrivateLessons <- factor(PerformanceAchievement$PrivateLessons, levels = c("No", "Yes"))

str(PerformanceAchievement)

library(psych)

describe(PerformanceAchievement)
describeBy(PerformanceAchievement, group = PerformanceAchievement$HomeMusic)
describeBy(PerformanceAchievement, group = PerformanceAchievement$PrivateLessons)

# 11.6.1. Preprocessing the Predictor Variables

contrasts(PerformanceAchievement$HomeMusic)
contrasts(PerformanceAchievement$PrivateLessons)

PerformanceAchievement$HomeMusic <- factor(PerformanceAchievement$HomeMusic, levels = c("None", "Some", "Much"))

levels(PerformanceAchievement$HomeMusic)
contrasts(PerformanceAchievement$HomeMusic)

colnames(contrasts(PerformanceAchievement$HomeMusic)) <- c("Some_compared_to_None", "Much_compared_to_none")
contrasts(PerformanceAchievement$HomeMusic)

colnames(contrasts(PerformanceAchievement$PrivateLessons)) <- c("Yes_compared_to_No")
contrasts(PerformanceAchievement$PrivateLessons)

library(psych)

PrivateLessons_dummy <- dummy.code(PerformanceAchievement$PrivateLessons)

head(PrivateLessons_dummy)

HomeMusic_dummy <- dummy.code(PerformanceAchievement$HomeMusic)

head(HomeMusic_dummy)

PerformanceAchievement <- data.frame(PerformanceAchievement$PerformanceAchievement, PerformanceAchievement$HomeMusic, PerformanceAchievement$PrivateLessons)
colnames(PerformanceAchievement) <- c("PerformanceAchievement", "HomeMusic", "PrivateLessons")

# 11.6.2. The Additive Multiple Linear Regression Model

PerformanceAchievement_additive <- lm(PerformanceAchievement ~ HomeMusic + PrivateLessons, data = PerformanceAchievement)

summary(PerformanceAchievement_additive)

round(confint(PerformanceAchievement_additive, level = 0.95), digits = 2)

library(effectsize)

standardize_parameters(PerformanceAchievement_additive)


# 11.6.2.1. Analysis of Residuals

library(car)

Anova(PerformanceAchievement_additive, type = 2)

# 11.6.2.2. Plotting and Testing the Effects

library(ggeffects)
library(ggplot2)

HomeMusic_effects <- ggpredict(PerformanceAchievement_additive, terms = "HomeMusic")

plot(HomeMusic_effects, add.data = TRUE, connect.lines = TRUE) +
  labs(
    x = "Home Music Participation",
    y = "Performance Achievement",
    title = "Effects of Home Music on Performance Achievement"
  )

PrivateLessons_effects <- ggpredict(PerformanceAchievement_additive, terms = "PrivateLessons")

plot(PrivateLessons_effects, add.data = TRUE, connect.lines = TRUE) +
  labs(
    x = "Private Lesson Participation",
    y = "Performance Achievement Scores",
    title = "Effects of Private Lesson Participation on Performance Achievement"
  )

library(emmeans)

emmeans(PerformanceAchievement_additive, pairwise ~ HomeMusic, contr = TRUE)

# 11.6.2.3. Forecasting

library(emmeans)

emmeans(PerformanceAchievement_additive, ~ HomeMusic * PrivateLessons)

# 11.7. Categorical by Categorical Multiple Regression with a Moderator Variable

# 11.7.1. The Multiplicative Multiple Regression Model

PerformanceAchievement_interaction <- lm(PerformanceAchievement ~ HomeMusic + PrivateLessons + HomeMusic*PrivateLessons, data = PerformanceAchievement)
summary(PerformanceAchievement_interaction)

round(confint(PerformanceAchievement_interaction), digits = 2)

library(effectsize)

standardize_parameters(PerformanceAchievement_interaction)

# 11.7.1.1. Analysis of Residuals

library(car)

Anova(PerformanceAchievement_interaction, type = 2)

# 11.7.1.2. Plotting Interaction Effect

interaction_plot <- ggpredict(PerformanceAchievement_interaction, c("HomeMusic", "PrivateLessons"))
interaction_plot

plot(interaction_plot, ci=TRUE, add.data = TRUE, connect.lines = TRUE) +
  labs(x = "Home Music Scores",
       y = "Performance Achievement Scores",
       title = "Home Music and Private Lesson Participation as Predictors of Performance Achievement",
       color = "Private Lesson Engagement")

int_effects <- effect('HomeMusic * PrivateLessons', PerformanceAchievement_interaction,
                      xlevels = list(PrivateLessons = c("No", "Yes"),
                                     HomeMusic = c("None", "Some", "Music")),
                      se = TRUE, confidence.level = .95, typical = mean)

int_effects <- as.data.frame(int_effects)
int_effects

library(emmeans)

emmeans(PerformanceAchievement_interaction, ~ HomeMusic*PrivateLessons)

# 11.7.1.3. Testing the Effects

library(emmeans)

Performance_Achievement_emmeans <- emmeans(PerformanceAchievement_interaction, ~ HomeMusic*PrivateLessons)

contrast(Performance_Achievement_emmeans, "pairwise", by = "PrivateLessons")

# Closing Out the Chapter

save_workspace
save_history




