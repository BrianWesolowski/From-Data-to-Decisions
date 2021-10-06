# Chapter 1 Script File


# 1.3. First Commands in R: A Fancy Calculator

4 + 6

9 - 6

9 * 6

50 / 2

3 ^ 2

# 1.4. Garbage In, Garbage Out

(3 + 5) ^ 2 / 17 + 21

(3 + 5) ^ 2 / (17 + 21)


# 1.6. Assigning Values to Objects: Object-Oriented Programming


x <- 5

x

print(x)

x + 5

y <- 2

y

x + y

ls()

rm(x)

ls()


x

# 1.7. Working in the Syntax Pane and Creating Scripts

5 + 5

# Here I am adding 5 + 5

5 + 5

# 1.8. An Example of Computational Thinking with Object-Oriented Programming

total_ME_faculty <- 4
total_TAs <- 5
TA_hours <- 14

total_TA_hours <- total_TAs * TA_hours
total_TA_hours

distributed_hours <- total_TA_hours / total_ME_faculty
distributed_hours

total_ME_faculty <- 4
total_TAs <- 6
TA_hours <- 14

total_TA_hours <- total_TAs * TA_hours
total_TA_hours

distributed_hours <- total_TA_hours / total_ME_faculty
distributed_hours


# 1.9. Functions

rm


# 1.10. Setting Up a Working Directory

getwd()
setwd("")
setwd("")
getwd()

# Set working directory

working_directory <- setwd("")
working_directory

# 1.11. Saving Files

working_directory <- setwd("")
working_directory

# Assign the value 2 to the object X

x <- 2

# Assign the value 5 to the object y

y <- 5

# Sum the objects x and y and assign to the object z

z <- x + y

# Save workspace

save.image("my_first_R_project.Rdata")

# Save history

savehistory("my_first_R_project.Rhistory")


# 1.12. Recap of Starting an R Session

Title of project or script purpose

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("my_first_R_project.Rdata")

# Save R History file

save_history <- savehistory("my_first_R_project.Rhistory")


# 1.13. Closing Down an R Session

(q)

# 1.14. Reopening an R Session


load(“my_first_R_project.Rdata”)

loadhistory(“my_first_R_project.Rhistory”)


# 1.15. Installing and Loading Packages

install.packages("car")

library(car)

# Title of project or script purpose

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("my_first_R_project.Rdata")

# Save R History file

save_history <- savehistory("my_first_R_project.Rhistory")

# Packages needed

install.packages(“car”)
library(car)

install.packages(“psych”)
library(psych)

install.packages(“interplot”)
library(interplot)

update.packages(oldPkgs = "cars")

update.packages(oldPkgs = c("cars", "psych"))

detach(“car”, unload = TRUE)

remove.packages(“car”)