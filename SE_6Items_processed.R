######## Script file to recreate SE_newdata_processed data frame from Chapter 4 ######## 

# Load New_SE_Data.csv

SE_6Items <- read.csv(file.choose(), header = TRUE)



names(SE_6Items) <- c("ChildAge", "YearsParticipating", "Instrument", "Item1", "Item2", "Item3", "Item4", "Item5", "Item6")

SE_6Items_processed <- SE_6Items[ , c("YearsParticipating", "ChildAge", "Instrument", "Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]

SE_6Items_processed$YearsParticipating <- factor(SE_6Items_processed$YearsParticipating, ordered = TRUE, levels = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"))

library(stringr)

SE_6Items_processed$ChildAge <- str_remove_all(SE_6Items_processed$ChildAge, "[years old]")

SE_6Items_processed$ChildAge <- as.numeric(SE_6Items_processed$ChildAge)

SE_6Items_processed$Instrument <- factor(SE_6Items_processed$Instrument, levels = c("Bass","Drums","Guitar","Piano/keyboard","Vocal","Other"))

SE_6Items_processed$Item1 <- factor(SE_6Items_processed$Item1, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_6Items_processed$Item2 <- factor(SE_6Items_processed$Item2, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_6Items_processed$Item3 <- factor(SE_6Items_processed$Item3, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_6Items_processed$Item4 <- factor(SE_6Items_processed$Item4, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_6Items_processed$Item5 <- factor(SE_6Items_processed$Item5, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_6Items_processed$Item6 <- factor(SE_6Items_processed$Item6, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

rm(SE_6Items)

