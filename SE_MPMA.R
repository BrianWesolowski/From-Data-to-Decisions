######## Script file to create SE_MPMA data frame ######## 


# Load SelfEvaluation.csv file
SE <- read.csv(file.choose(), header = TRUE)

# Load MAMP2.csv file 

MAMP2 <- read.csv(file.choose(), header = TRUE)

# Replace blank cells with NA
SE[SE == ''] <- NA

# Replace 99s with NAs
SE[SE == 99] <- NA

# Remove NAs
SE_complete <- na.omit(SE)

# Change miskeyed elements

SE_complete$Q1_8[SE_complete$Q1_8 == "disagree"] <- "Disagree"
SE_complete$Q1_8[SE_complete$Q1_8 == "SA"] <- "Strongly Agree"

#Explicit Coercion of Variables
Q30_ordered <- factor(SE_complete$Q30, ordered = TRUE, levels = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"))

library(stringr)
Q9_char_removed <- str_remove_all(SE_complete$Q9, "[years old]")
Q9_numeric <- as.numeric(Q9_char_removed)

Q7_ordered <- factor(SE_complete$Q7, levels = c("Bass","Drums","Guitar","Piano/keyboard","Vocal","Other"))

Q1_6_ordered <- factor(SE_complete$Q1_6, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

Q1_7_ordered <- factor(SE_complete$Q1_7, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

Q1_8_ordered <- factor(SE_complete$Q1_8, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

# Create new data frame

SE_processed <- data.frame(Q30_ordered, Q9_numeric, Q7_ordered, Q1_6_ordered, Q1_7_ordered, Q1_8_ordered)

names(SE_processed) <- c("YearsParticipating", "ChildAge", "Instrument", "Item1", "Item2", "Item3")


# Add participant ID variable

Participant_ID <- as.integer(1:150)

SE_processed <- cbind(SE_processed, Participant_ID = Participant_ID)

SE_processed  <- SE_processed[ , c(7, 1:6)]


# Merge SE_processed and MAMP2

SE_MAMP2_merged2 <- merge(SE_processed, MAMP2, by = "Participant_ID")

# Rename data frame to SE_MPMA

SE_MPMA <- SE_MAMP2_merged2


# Remove all but SE_processed data frame from workspace

rm(Participant_ID, Q1_6_ordered, Q1_7_ordered, Q1_8_ordered, Q30_ordered, Q7_ordered, Q9_char_removed, Q9_numeric, MAMP2, SE, SE_complete, SE_MAMP2_merged2, SE_processed)

