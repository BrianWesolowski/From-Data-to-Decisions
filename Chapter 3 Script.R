# Chapter 3 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("SelfEvaluation.Rdata")

# Save R History file

save_history <- savehistory("SelfEvaluation.Rhistory")

# Packages Needed for Chapter 3

install.packages("plyr")
library(plyr)

install.packages("readxl")
library(readxl)

install.packages("skimr")
library(skimr)

install.packages("stringr")
library(stringr)

# 3.2.1.1. Files with .csv Extensions

# Load SelfEvaluation.csv

SE1 <- read.csv(file.choose(), header = TRUE)

# Load SelfEvaluation.csv

SE1a <- read.table(file.choose(), header = TRUE, sep = ',')

# 3.2.1.2. Files with .txt Extensions

# Load SelfEvaluation.txt

SE2 <- read.delim(file.choose(), header = TRUE)

# Load SelfEvaluation.txt

SE2a <- read.table(file.choose(), header = TRUE, sep = "\t")

# 3.2.1.3. Files with .xlsx or .xls Extensions

library(readxl)
SE3 <- read_excel("")
View(SE3)

# Load SelfEvaluation.xlsx

library(readxl)
SE3 <- read.table(file.choose(), header = TRUE, sep = "\t")

rm(SE1a, SE2, SE2a, SE3)

# 3.2.2. Examining the Landscape of the Data

class(SE1)
dim(SE1)
nrow(SE1)
ncol(SE1)
names(SE1)
str(SE1)
head(SE1)
head(SE1$Q7)

Q7

attach(SE1)
head(Q7)

detach(SE1)

# 3.2.2.1. Missing and Inconsistent Data Elements	

any(is.na(SE1))

View(SE1)

SE1[SE1 == ''] <- NA
any(is.na(SE1))

which(SE1$Q1_6 == 99)
which(SE1$Q1_7 == 99)
which(SE1$Q1_8 == 99)

SE1[SE1 == 99] <- NA

which(SE1$Q1_6 == 99)
which(SE1$Q1_7 == 99)
which(SE1$Q1_8 == 99)

sum(is.na(SE1))
colSums(is.na(SE1))

is.na(SE1)
SE1[!complete.cases(SE1), ]

SE_complete <- na.omit(SE1)
SE_complete[!complete.cases(SE_complete), ]

dim(SE_complete)

unique(SE_complete$Q1_8)

SE_complete$Q1_8[SE_complete$Q1_8 == "disagree"] <- "Disagree"
SE_complete$Q1_8[SE_complete$Q1_8 == "SA"] <- "Strongly Agree"

# 3.2.2.2. Explicit Coercion of Variables

unique(SE_complete$Q30)

Q30_ordered <- factor(SE_complete$Q30, ordered = TRUE, levels = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"))

str(Q30_ordered)

unique(SE_complete$Q9)

Q9_ordered <- factor(SE_complete$Q9, ordered = TRUE, levels = c("8 years old", "9 years old", "10 years old", "11 years old", "12 years old", "13 years old", "14 years old", "15 years old", "16 years old", "17 years old"))

library(stringr)

Q9_char_removed <- str_remove_all(SE_complete$Q9, "[years old]")

head(Q9_char_removed)

Q9_numeric <- as.numeric(Q9_char_removed)

head(Q9_numeric)

class(Q9_numeric)

unique(SE_complete$Q7)

Q7_ordered <- factor(SE_complete$Q7, levels = c("Bass","Drums","Guitar","Piano/keyboard","Vocal","Other"))

levels(Q7_ordered)

unique(SE_complete$Q1_6)
unique(SE_complete$Q1_7)
unique(SE_complete$Q1_8)

Q1_6_ordered <- factor(SE_complete$Q1_6, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
Q1_7_ordered <- factor(SE_complete$Q1_7, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
Q1_8_ordered <- factor(SE_complete$Q1_8, ordered = TRUE, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

SE_processed <- data.frame(Q30_ordered, Q9_numeric, Q7_ordered, Q1_6_ordered, Q1_7_ordered, Q1_8_ordered)

names(SE_processed) <- c("YearsParticipating", "ChildAge", "Instrument", "Item1", "Item2", "Item3")

str(SE_processed)

library(skimr)

skim(SE_processed)


# 3.3. Data Manipulation Techniques

head(SE_processed, 15)
SE_processed[11, 3]
SE_processed[11, "Instrument"]
Inst_11 <- SE_processed[11, "Instrument"] 
Inst_11
SE_processed[9, ]
head(SE_processed[ , "ChildAge"])
SE_processed[c(2, 3, 4), c("Instrument", "ChildAge")]

# 3.3.2. Subsets

drums <- subset(SE_processed, Instrument == "Drums")
class(drums)
head(drums, 15)
dim(drums)

drums_1_2yrs <- subset(SE_processed, Instrument == "Drums" & YearsParticipating == "1-2 years")
drums_1_2yrs
dim(drums_1_2yrs)

drums_1_2yrs_14plus <- subset(SE_processed, Instrument == "Drums" & YearsParticipating == "1-2 years" & ChildAge >= 14)
drums_1_2yrs_14plus
dim(drums_1_2yrs_14plus)

# 3.3.3. Sorting and Ordering

sort(drums$Item1)

sort(drums$ChildAge)
sort(drums$ChildAge, decreasing = TRUE)

example <- c(15, 3, 45, 23, 17)
sort(example)
order(example)

drums_ChildAge <-  drums[order(drums$ChildAge), ]
head(drums_ChildAge, 15)

drums_ChildAge_descending <- drums[order(-drums$ChildAge), ]
head(drums_ChildAge_descending, 15)

drums_ChildAge_YearsParticipating <- drums[order(drums$ChildAge, drums$YearsParticipating), ]
head(drums_ChildAge_YearsParticipating, 15)

# 3.3.4. Removing Rows from a Data Frame

SE1_removed <- SE_processed[-1, ]
head(SE1_removed, 3)

SE_1_4_removed <- SE_processed[-c(1:4), ]
head(SE_1_4_removed, 3)

# 3.3.5. Removing Rows by Condition

SE_processed_no_piano_keyboard <- subset(SE_processed, Instrument != "Piano/keyboard")
dim(SE_processed_no_piano_keyboard)

range(SE_processed$ChildAge)
SE_processed_16plus_10minus <- subset(SE_processed, ChildAge >= 16 | ChildAge <= 10)
dim(SE_processed_16plus_10minus)

# 3.3.6. Creating New Vectors: Collapsing Data and Explicitly Coercing Vectors 
# 3.3.6.1. Categorical to Logical

piano_keyboard <- SE_processed$Instrument == "Piano/keyboard" 
head(piano_keyboard, 10)

# 3.3.6.2. Categorical to Categorical

library(plyr)

Item1_dichotomous <- revalue(SE_processed$Item1, c("Strongly Disagree" = "Disagree", "Disagree" = "Disagree", "Agree" = "Agree", "Strongly Agree" = "Agree"))
head(Item1_dichotomous)

# 3.3.6.3. Numeric to Categorical 

range(SE_processed$ChildAge)

breaks <- c(6, 11, 13, 18)  

ChildAge_Categorical <- cut(SE_processed$ChildAge, ordered = TRUE, breaks = c(6, 11, 13, 18), labels = c("Elementary", "Middle", "High"))
head(ChildAge_Categorical)

ChildAge_Categorical_ROpen <- cut(SE_processed$ChildAge, ordered = TRUE, breaks = c(6, 11, 13, 18), labels = c("Elementary", "Middle", "High"), right = FALSE)
head(ChildAge_Categorical_ROpen)

# 3.3.7. Adding Vectors to Data Frames

SE_processed1 <- SE_processed
SE_processed2 <- SE_processed
SE_processed3 <- SE_processed

SE_processed1$PK_logical <- piano_keyboard
names(SE_processed1)

# 3.3.9. Bracket Notation

SE_processed2["PK_logical"] <- piano_keyboard
names(SE_processed2)

# 3.3.10. Using the Column Bind Function

SE_processed3 <- cbind(SE_processed,PK_logical = piano_keyboard)
names(SE_processed3)

# 3.3.11. Reordering Columns in a Data Frame 
# 3.3.11.1. Indexing Columns by Position Number or Variable Name

SE_processed_ordered1 <- SE_processed1[ , c(1:3, 7, 4:6)]
names(SE_processed_ordered1)

# 3.3.11.2. Subset

SE_processed_ordered2 <- subset(SE_processed1, select = c(1:3, 7, 4:6))
names(SE_processed_ordered2)

# 3.3.12. Removing Columns from a Data Frame

SE_processed_PKremoved <- within(SE_processed1, rm(PK_logical))
names(SE_processed_PKremoved)

SE_processed_Likert_removed <- within(SE_processed_PKremoved, rm(Item1, Item2, Item3))
names(SE_processed_Likert_removed)

# 3.4. Closing Out the Chapter

save_workspace
save_history
