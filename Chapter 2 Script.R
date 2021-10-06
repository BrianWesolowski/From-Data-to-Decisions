# Chapter 2 Script File

# 2.1.1. Character Classes

instrument <- "clarinet"
class(instrument)
instrument <- clarinet

# 2.1.2. Numeric Classes

pie <- 3.14
class(pie)

# 2.1.3. Integer Classes

students <- 15
class(students)

students <- 15L
class(students)

# 2.1.4. Complex Classes

complex <- 5 + 2i
class(complex)

# 2.1.5. Logical Classes

type <- TRUE
class(type)

# 2.2. Relational and Logical Operators

10 < 4
11 > 8
8 <= 8
9 >= 14
8 == 8
8 != 8

x <- 7

x > 3 & x < 10
x > 8 & x < 10
x > 4 & x < 6

x > 3 | x < 10
x > 8 | x < 10
x > 4 | x < 6
x > 8 | x < 6

x > 7
!(x > 7)

# 2.4. Working with Vectors

age <- c(15, 18, 22, 17, 17)
age
class(age)

participant_ID <- c(1L, 2L, 3L, 4L, 5L)
participant_ID
class(participant_ID)

instrument <- c("clarinet", "saxophone", "trumpet", "sax", "flute")
instrument
class(instrument)

under_18 <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
under_18
class(under_18)

class(age)
str(age)

# 2.4.1. Vector Operations

x <- c(1, 2, 3, 4, 5, 6)
x

x + 1
x

y <- x + 1
y

x
y
z <- x + y
z

z_sqrt <- sqrt(z)
z_sqrt

fun <- c(7, 2, 6, 9, 4, 1, 3)
fun

fun_sorted <- sort(fun)
fun_sorted

length(fun)

min(fun)
max(fun)
sum(fun)
prod(fun)
mean(fun)

rep(1, 5)

student_IDs <- seq(5)
student_IDs

seq(5, 10)

5:10

seq(10, 20, 2)

# 2.4.2. Explicit Coercion of Atomic Class Types

x <- c(1, 2, 3, 4, 5)
class(x)

y <- 6:10
class(y)

class(x)
x_integer <- as.integer(x)
class(x_integer)

class(y)
y_numeric <- as.numeric(y)
class(y_numeric)

under_18
class(under_18)
under_18_character <- as.character(under_18)
class(under_18_character)
under_18_character

under_18_character
class(under_18_character)
under_18_numeric <- as.numeric(under_18_character)
class(under_18_numeric)
under_18_numeric

# 2.4.3. Nominal Factor Vectors

musical_instrument <- c("oboe", "clarinet", "saxophone", "saxophone", "flute")
class(musical_instrument)

musical_instrument_factor <- factor(musical_instrument)
musical_instrument_factor
class(musical_instrument_factor)
str(musical_instrument_factor)

# 2.4.4. Ordered Factor Vectors

enjoyment <- c("strongly agree", "agree", "agree", "strongly disagree", "disagree")
class(enjoyment)

enjoyment_factor <- factor(enjoyment)
enjoyment_factor

enjoyment_factor_ordered <- factor(enjoyment, ordered = TRUE, levels = c("strongly disagree", "disagree", "agree", "strongly agree"))
enjoyment_factor_ordered
class(enjoyment_factor_ordered)
str(enjoyment_factor_ordered)

# 2.4.5. Naming Factor Vector Levels: From Integers to Labels

enjoyment_numeric <- c(4, 3, 3, 1, 2)
class(enjoyment_numeric)

enjoyment_factor <- as.factor(enjoyment_numeric)
enjoyment_factor
class(enjoyment_factor)

enjoyment_factor_named <- factor(enjoyment_factor, labels = c("strongly disagree", "disagree", "agree", "strongly agree"))
enjoyment_factor_named
class(enjoyment_factor_named)

enjoyment_factor_named_ordered <- factor(enjoyment_factor_named, ordered = TRUE)
enjoyment_factor_named_ordered
class(enjoyment_factor_named_ordered)

# 2.4.6. Reverse Coding Numeric Vectors

library(car)

item2 <- c(1, 3, 4, 2, 3)
class(item2)

item2_reverse <- recode(item2, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
item2
item2_reverse

# 2.4.7. Reverse Coding Character Vectors

item2 <- c("strongly disagree", "agree", "strongly agree", "disagree", "agree")
class(item2)

item2_factor <- factor(item2, levels = c("strongly disagree", "disagree", "agree", "strongly agree"))
class(item2_factor)
str(item2_factor)

library(car)

item2_factor_reverse <- recode(item2_factor, "'strongly disagree' = 'strongly agree'; 'disagree' = 'agree'; 'agree' = 'disagree'; 'strongly agree' = 'strongly disagree'")
item2_factor_reverse

item2_factor_reverse_ordered <- factor(item2_factor_reverse, ordered = TRUE, levels = c("strongly disagree", "disagree", "agree", "strongly agree"))
item2_factor_reverse_ordered

# 2.5.1. Creating Vectors

participant_ID <- c(1:5)
str(participant_ID)

years_participation <- c(2, 3, 1, 5, 3)
str(years_participation)

item1 <- c(1, 3, 4, 2, 3)
item2 <- c(4, 1, 2, 3, 1)

str(item1)
str(item2)

item1_factor <- as.factor(item1)
item2_factor <- as.factor(item2)

str(item1_factor)
str(item2_factor)

item1_factor_named <- factor(item1_factor, labels = c("strongly disagree", "disagree", "agree", "strongly agree"))
item2_factor_named <- factor(item2_factor, labels = c("strongly disagree", "disagree", "agree", "strongly agree"))

str(item1_factor_named)
str(item2_factor_named)

item1_factor_named_ordered <- factor(item1_factor_named, ordered = TRUE)
item2_factor_named_ordered <- factor(item2_factor_named, ordered = TRUE)

item1_factor_named_ordered
item2_factor_named_ordered

# 2.5.2. Creating the Data Frame

goal_orientation <- data.frame(participant_ID, years_participation, item1_factor_named_ordered, item2_factor_named_ordered)
str(goal_orientation)

names(goal_orientation) <- c("ID", "Years Participating", "Item1", "Item2")
names(goal_orientation)

# 2.5.3. Accessing Variables in a Data Frame

goal_orientation$Item1

# 2.6. Exporting Data Structures to .csv

write.csv(goal_orientation, file = “goal_orientation.csv”)