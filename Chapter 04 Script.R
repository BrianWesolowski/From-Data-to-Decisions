# Chapter 4 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("ExtendingDataFrames.Rdata")

# Save R History file

save_history <- savehistory("ExtendingDataFrames.Rhistory")

# Packages Needed for Chapter 4

install.packages("plyr")
library(plyr)

install.packages("reshape2")
library(reshape2)

install.packages("stringr")
library(stringr)

# Run SE_processed.R script file

# Load New_SE_Data.csv

SE_newdata <- read.csv(file.choose(), header = TRUE)

str(SE_processed)

str(SE_newdata)

SE_newdata_processed <- within(SE_newdata, rm(Q2_1, Q2_2, Q2_3))

ncol(SE_newdata_processed)

ncol(SE_processed)

names(SE_newdata_processed) <- c("ChildAge", "YearsParticipating", "Instrument", "Item1", "Item2", "Item3")

names(SE_newdata_processed)

names(SE_processed)

SE_newdata_processed <- SE_newdata_processed[ , c("YearsParticipating", "ChildAge", "Instrument", "Item1", "Item2", "Item3")]

str(SE_newdata_processed)

str(SE_processed)


SE_newdata_processed$YearsParticipating <- factor(SE_newdata_processed$YearsParticipating, ordered = TRUE, levels = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"))
library(stringr)
SE_newdata_processed$ChildAge <- str_remove_all(SE_newdata_processed$ChildAge, "[years old]")
SE_newdata_processed$ChildAge <- as.numeric(SE_newdata_processed$ChildAge)
SE_newdata_processed$Instrument <- factor(SE_newdata_processed$Instrument, levels = c("Bass","Drums","Guitar","Piano/keyboard","Vocal","Other"))
SE_newdata_processed$Item1 <- factor(SE_newdata_processed$Item1, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
SE_newdata_processed$Item2 <- factor(SE_newdata_processed$Item2, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
SE_newdata_processed$Item3 <- factor(SE_newdata_processed$Item3, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

str(SE_newdata_processed)
str(SE_processed)

SE_merged <- rbind(SE_processed, SE_newdata_processed)
dim(SE_merged)

# 4.2.2. Merging Columns to a Data Frame with an Equivalent Observation Structure

# Load MAMP1.csv

MAMP1 <- read.csv(file.choose(), header = TRUE)  

# Load MAMP2.csv

MAMP2 <- read.csv(file.choose(), header = TRUE)   

# Load MAMP3.csv

MAMP3 <- read.csv(file.choose(), header = TRUE)   

# 4.2.2.1. Scenario 1: Merging on Row Numbers

str(SE_processed)
str(MAMP1)

head(SE_processed, 3)
head(MAMP1, 3)

SE_MAMP1_merge1 <- merge(SE_processed, MAMP1, by = 0)
str(SE_MAMP1_merge1)
SE_MAMP1_merge1 <- within(SE_MAMP1_merge1, rm(Row.names))
names(SE_MAMP1_merge1)

SE_MAMP1_merge_1a <- transform(merge(SE_processed, MAMP1, by = 0), Row.names = NULL)
names(SE_MAMP1_merge_1a)

# 4.2.2.2. Scenario 2: Merging on Columns with the Same Name

str(SE_processed)
str(MAMP2)

Participant_ID <- as.integer(1:150)
SE_processed <- cbind(SE_processed, Participant_ID = Participant_ID)
SE_processed <- SE_processed[ , c(7, 1:6)]
str(SE_processed)

SE_MAMP2_merged2 <- merge(SE_processed, MAMP2, by = "Participant_ID")
dim(SE_MAMP2_merged2)

# 4.2.2.3. Scenario 3: Merging on Columns with a Different Name

str(SE_processed)
str(MAMP3)

SE_MAMP3_merged3 <- merge(x = SE_processed, y = MAMP3, by.x = "Participant_ID", by.y = "Student_ID")
str(SE_MAMP3_merged3)

# 4.2.3. Adding Observations with Non-Equivalent Column Structures  

SE_10rows <- SE_processed[1:10, ]
dim(SE_10rows)

# Load SE_6items_processed.R Script file

str(SE_10rows)
str(SE_6Items_processed)

library(plyr)

SE_10rows_SE_6Items_processed_merged <- rbind.fill(SE_10rows, SE_6Items_processed)
View(SE_10rows_SE_6Items_processed_merged)

# 4.2.4. Adding Columns with Non-Equivalent Observation Structures

# Load MAMP4.csv

MAMP4 <- read.csv(file.choose(), header = TRUE) 

str(SE_10rows)
str(MAMP4)

SE_10rows$Participant_ID
MAMP4$Participant_ID

SE_10rows_MAMP4_merged <- merge(SE_10rows, MAMP4, by = "Participant_ID")
View(SE_10rows_MAMP4_merged)

SE_MAMP4_merged_allX <- merge(SE_10rows, MAMP4, by = "Participant_ID", all.x = TRUE)
View(SE_MAMP4_merged_allX)

SE_MAMP4_merged_allY <- merge(SE_10rows, MAMP4, by = "Participant_ID", all.y = TRUE)
View(SE_MAMP4_merged_allY)

SE_MAMP4_merged_allX_allY <- merge(SE_10rows, MAMP4, by = "Participant_ID", all.x = TRUE, all.y = TRUE)
View(SE_MAMP4_merged_allX_allY)

# 4.3. Reshaping Data: Long Versus Wide Formats

# Load BandEnrollment.csv

BandEnrollment <- read.csv(file.choose(), header = TRUE)
BandEnrollment

# 4.3.1. Wide to Long Format

library(reshape2)

BandEnrollment_long <- melt(BandEnrollment, 
                            id.vars = "School",
                            measure.vars = c("Enrollment_2019", "Enrollment_2020",   
                                             "Enrollment_2021"),
                            variable.name = "Enrollment_Year",
                            value.name= "Students")

head(BandEnrollment_long, 5)

levels(BandEnrollment_long$Enrollment_Year)[levels(BandEnrollment_long$Enrollment_Year)=="Enrollment_2019"] <- "2019"
levels(BandEnrollment_long$Enrollment_Year)[levels(BandEnrollment_long$Enrollment_Year)=="Enrollment_2020"] <- "2020"
levels(BandEnrollment_long$Enrollment_Year)[levels(BandEnrollment_long$Enrollment_Year)=="Enrollment_2021"] <- "2021"

head(BandEnrollment_long, 5)

# 4.3.2. Long to Wide Format

library(reshape2)

BandEnrollment_wide <- dcast(BandEnrollment_long, School ~ Enrollment_Year, value.var = "Students")
BandEnrollment_wide

names(BandEnrollment_wide)[names(BandEnrollment_wide)=="2019"] <- "Enrollment_2019"
names(BandEnrollment_wide)[names(BandEnrollment_wide)=="2020"] <- "Enrollment_2020"
names(BandEnrollment_wide)[names(BandEnrollment_wide)=="2021"] <- "Enrollment_2021"

head(BandEnrollment_wide)

# 4.4. Closing Out the Chapter

save_workspace
save_history
