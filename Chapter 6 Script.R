# Chapter 6 Script File

# Set working directory

working_directory <- setwd("")

# Save R Workspace file

save_workspace <- save.image("DataVisualization.Rdata")

# Save R History file

save_history <- savehistory("DataVisualization.Rhistory")

# Packages Needed for Chapter 6

install.packages("car")
library(car)

install.packages("lattice")
library(lattice)

install.packages("plyr")
library(plyr)

install.packages("psych")
library(psych)

install.packages("qcc")
library(qcc)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("vioplot")
library(vioplot)

install.packages("viridis")
library(viridis)

install.packages("yarrr")
library(yarrr)


# 6.2. A Data Visualization Primer in R
# 6.2.1. Creating Multiple Plot Spaces

barplot(1:3)

par(mfrow = c(2,3))

barplot(1:3)
barplot(1:4) 
barplot(1:5) 
barplot(1:6)
barplot(1:7) 
barplot(1:8)

par(mfrow = c(3,2))

barplot(1:3)
barplot(1:4) 
barplot(1:5) 
barplot(1:6)
barplot(1:7) 
barplot(1:8)

par(mfrow = c(1,1))

# 6.2.2. Color Considerations 
# 6.2.2.1. R Colors

colors()
head(colors())

barplot(1:5, col = "gray53")

barplot(1:5, col = c("gray90", "gray70", "gray50", "gray30", "gray10"))

barplot(1:20, col = gray.colors(20, 0, 1))

barplot(1:20, col = gray.colors(20, 0.3, 0.8))   # Plot A
barplot(1:20, col = gray.colors(20, 0.7, 0.9))   # Plot B
barplot(1:20, col = gray.colors(20, 0.5, 0.8))   # Plot C
barplot(1:20, col = gray.colors(20, 0.1, 0.9))   # Plot D

gray.colors(20, 0.3, 0.8)

# 6.2.2.2. Hexadecimal Color Codes

barplot(1:5, col = "#4D4D4D")

# 6.2.2.3. RGB Values

crgb <- col2rgb(cc <- colors())
t(crgb)

head(t(crgb))

# 6.2.2.4. Converting Between R Colors, Hexadecimal Colors, and RGB Values

rgbtohex <- function(r, g, b) sprintf('#%s', paste(as.hexmode(c(r, g, b)), collapse = ''))
rgbtohex(186, 12, 47)

x <- "#ba0c2f"
paste(as.vector(col2rgb(x)), collapse = " ")

col2rgb('darkgreen')

# 6.2.2.5. Color Palettes

library(viridis)

barplot(1:20, col = rainbow(20))

par(mfrow = c(2,2))

barplot(1:20, col = rainbow(10))  
barplot(1:20, col = rainbow(20))  
barplot(1:20, col = rainbow(30))  
barplot(1:20, col = rainbow(40))

par(mfrow = c(1,1))

library(RColorBrewer)

display.brewer.all()

display.brewer.all(n = 5, type = "seq", select = NULL)

display.brewer.all(n = NULL, type = "all", select = NULL, colorblindFriendly = TRUE)

display.brewer.pal(n = 9, name= "Greys")

barplot(1:20, col = brewer.pal(n = 20, name = "Greys"))

# 6.3. Visualizing Univariate Data
# 6.3.1. Plotting Univariate Categorical Data 
# 6.3.1.1. Bar Plot

Item1_table <- table(SE_MPMA$Item1)
barplot(Item1_table)
barplot(Item1_table, horiz = TRUE)

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)")                      

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1")                   

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1: I ask for help when I do not understand something I am practicing")

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1: \n I ask for help when I do not understand something I am practicing")

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1: \n I ask for help when I do not understand something I am practicing",
        ylim = c(0, 70))

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1: \n I ask for help when I do not understand something I am practicing",
        ylim = c(0, 70),
        las = 1)

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                     
        main = "Response Distribution for Item 1: \n I ask for help when I do not understand something I am practicing",
        ylim = c(0, 70), 
        las = 1,                                                     
        col = gray.colors(4, start = 0.1, end = 0.9))                

color1 <- c("gray25", "gray75", "gray75", "gray75")
color2 <- c("gray75", "gray25", "gray75", "gray75")
color3 <- c("gray75", "gray75", "gray25", "gray75")
color4 <- c("gray75", "gray75", "gray75", "gray25")

par(mfrow = c(2,2))

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                    
        main = "Response Category 1 Using Color1",                                         
        ylim = c(0, 70),                                             
        col = color1)              

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                    
        main = "Response Category 2 Using Color2",                                         
        ylim = c(0, 70),                                             
        col = color2)   

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                    
        main = "Response Category 3 Using Color3",                                          
        ylim = c(0, 70),                                             
        col = color3) 

barplot(Item1_table,
        ylab = "Raw Frequency of Responses (N)",                    
        main = "Response Category 4 Using Color4",                                          
        ylim = c(0, 70),                                             
        col = color4)

par(mfrow = c(1,1))

prop.table(Item1_table)

Item1_proptable <- prop.table(Item1_table)

barplot(Item1_proptable,
        ylab = "Proportion of Responses (%)",
        main = "Proportional Response Distribution for Item 1: \n I ask for help when I do not understand something I am practicing",
        col = color3,
        ylim = c(0.0, 0.5),
        las = 1)

# 6.3.1.2. Pareto Chart

library(qcc)

pareto.chart(table(SE_MPMA$Item1))

qcc.options(bg.margin = "white")                             

pareto.chart(table(SE_MPMA$Item1), 
             col="gray",                                     
             main = "Pareto Chart for Item 1 Responses")  

# 6.3.2. Plotting of Univariate Continuous Data 

# 6.3.2.1. Empirical Cumulative Distribution Function

ecdf(SE_MPMA$MusicPerceptionScores)
ecdf <- ecdf(SE_MPMA$MusicPerceptionScores)
ecdf(SE_MPMA$MusicPerceptionScores)

plot.ecdf(ecdf,
          main = "Empirical Cumulative Distribution for Music Perception Scores (N = 150)",
          xlab = "Music Perception Scores",
          ylab = "Cumulative Percentage of Student Scores")

yticks <- seq(0.0, 1.0, .1)

plot.ecdf(ecdf,
          main = "Empirical Cumulative Distribution for Music Perception Scores (N = 150)",
          xlab = "Music Perception Scores",
          ylab = "Cumulative Percentage of Student Scores",
          axes = FALSE)

plot.ecdf(ecdf,
          main = "Empirical Cumulative Distribution for Music Perception Scores (N = 150)",
          xlab = "Music Perception Scores",
          ylab = "Cumulative Percentage of Student Scores",
          axes = FALSE)
axis(1)

plot.ecdf(ecdf,
          main = "Empirical Cumulative Distribution for Music Perception Scores (N = 150)",
          xlab = "Music Perception Scores",
          ylab = "Cumulative Percentage of Student Scores",
          axes = FALSE)
axis(1)
axis(2, at = yticks)


# 6.3.2.2. Box Plot

round(summary(SE_MPMA$MusicPerceptionScores), digits = 2)

boxplot(SE_MPMA$MusicPerceptionScores)

boxplot(SE_MPMA$MusicPerceptionScores,
        main = "Box Plot Distribution of Music Perception Scores",  
        ylab = "Music Perception Scores",                           
        ylim = c(-.8, .6),                                          
        las = 1,                                                    
        col = "grey",                                               
        frame.plot = FALSE, 
        boxwex = .7)                                                

# 6.3.2.2.1. Box Plot with Strip Chart Overlay

boxplot(SE_MPMA$MusicPerceptionScores,
        main = "Box Plot Distribution of Music Perception Scores",  
        ylab = "Music Perception Scores",                           
        ylim = c(-.8, .6),                                          
        las = 1,                                                    
        col = "grey",                                               
        frame.plot = FALSE, 
        boxwex = 0.7) 
stripchart(SE_MPMA$MusicPerceptionScores,                   
           vertical = TRUE,                                         
           method = "jitter",                                       
           add = TRUE,                                              
           pch = 20,                                                
           cex = .6)                                                

# 6.3.2.3. Stem-and-Leaf Plot

stem(SE_MPMA$MusicPerceptionScores)

range(SE_MPMA$MusicPerceptionScores)

sort(SE_MPMA$MusicPerceptionScores)[2:3]

stem(SE_MPMA$MusicPerceptionScores, scale = 2)

# 6.3.2.4. Histogram with Frequency Scale

hist(SE_MPMA$MusicPerceptionScores)

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray")                                                   

MP_hist_values <- hist(SE_MPMA$MusicPerceptionScores)
MP_hist_values

# 6.3.2.4.1. Overlays

# 6.3.2.4.1.1. Frequency Count Overlay

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray")                                                   
text(MP_hist_values$mids,                                               
     MP_hist_values$counts,                                             
     MP_hist_values$counts,
     cex = .8,
     adj = c(.5, -.7))  

# 6.3.2.4.1.2. Mean and Standard Deviation Overlay

MP_mean <- mean(SE_MPMA$MusicPerceptionScores)
MP_SD <- sd(SE_MPMA$MusicPerceptionScores)

MP_mean
MP_SD

pos1_SD <- MP_mean + MP_SD
neg1_SD <- MP_mean - MP_SD

pos1_SD 
neg1_SD

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray")                                                       
abline(v = MP_mean, col = "black", lwd = 1.3, lty = 6)                  
abline(v = pos1_SD, col = "gray20", lwd = 1.3, lty = 3)                 
abline(v = neg1_SD, col = "gray20", lwd = 1.3, lty = 3)
text(-.135, 37, "-1 SD")
text(.045, 37, "Mean")
text(.215, 37, "+1 SD")  

# 6.3.2.4.1.3. Rug Overlay

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray") 
rug(SE_MPMA$MusicPerceptionScores, col = "gray20", lwd = 1.3)  

# 6.3.2.4.1.4. Line Graph Overlay

MP_hist_values$mids
MP_hist_values$counts

xcoords <- MP_hist_values$mids
ycoords <- MP_hist_values$counts

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray") 
lines(xcoords, ycoords, type = "b", pch = 20, lwd = 1.3)  

# 6.3.2.4.1.5. Normal Curve Overlay

xfit <- seq(min(SE_MPMA$MusicPerceptionScores), max(SE_MPMA$MusicPerceptionScores), length = 50)
head(sort(xfit))

yfit <- dnorm(xfit, mean = mean(SE_MPMA$MusicPerceptionScores), sd = sd(SE_MPMA$MusicPerceptionScores))
head(yfit)

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray") 
lines(xfit, yfit) 

yfit_prob <- dnorm(xfit, mean = mean(SE_MPMA$MusicPerceptionScores), sd = sd(SE_MPMA$MusicPerceptionScores))
rm(yfit)

yfit_freq <- (yfit_prob * diff(MP_hist_values$mids[1:2])) * length(SE_MPMA$MusicPerceptionScores)

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Frequency of Scores",                                   
     ylim = c(0, 40),                                                
     las = 1,                                                        
     col = "gray") 
lines(xfit, yfit_freq) 

# 6.3.2.5. Histogram with Probability Density Function Scale

MP_hist_values$density

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",           
     xlab = "Music Perception Scores",                                  
     ylab = "Frequency of Scores",                                      
     ylim = c(0, 35),                                                   
     las = 1,                                                           
     col = "gray",                                                      
     xaxt = 'n',                                                        
     prob = TRUE) 

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Probability Density Function (PDF)",                                   
     ylim = c(0, 2.5),                                                
     las = 1,                                                        
     col = "gray",
     prob = TRUE)  

# 6.3.2.5.1. Overlays

# 6.3.2.5.1.1. Normal Curve Overlay

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Probability Density Function (PDF)",                                   
     ylim = c(0, 2.5),                                                
     las = 1,                                                        
     col = "gray",
     prob = TRUE) 
curve(dnorm(x, mean = mean(SE_MPMA$MusicPerceptionScores), 
            sd = sd(SE_MPMA$MusicPerceptionScores)), add = TRUE)   

# 6.3.2.5.1.2. Kernel Probability Density Overlay

hist(SE_MPMA$MusicPerceptionScores,
     main = "Histogram of Music Perception Scores (N = 150)",        
     xlab = "Music Perception Scores",                               
     ylab = "Probability Density Function (PDF)",                                   
     ylim = c(0, 2.5),                                                
     las = 1,                                                        
     col = "gray",
     prob = TRUE) 
lines(density(SE_MPMA$MusicPerceptionScores))   

# 6.3.2.6. Kernel Density Plot

plot(density(SE_MPMA$MusicPerceptionScores))

plot(density(SE_MPMA$MusicPerceptionScores),
     xlim = c(-.7, .6),                                                  
     ylim = c(0.0, 2.5),                                                 
     las = 1,                                                            
     xlab = "Music Perception Scores",                                   
     ylab = "Probability Density Function (PDF)",                        
     main = "Kernel Density Plot of Music Perception Scores (N = 150)",  
     bty = 'n')                                                          

plot(density(SE_MPMA$MusicPerceptionScores),
     xlim = c(-.7, .6),                                                  
     ylim = c(0.0, 2.5),                                                 
     las = 1,                                                            
     xlab = "Music Perception Scores",                                   
     ylab = "Probability Density Function (PDF)",                        
     main = "Kernel Density Plot of Music Perception Scores (N = 150)",  
     bty = 'n')                                                          
polygon(density(SE_MPMA$MusicPerceptionScores), col = "gray") 

# 6.3.2.7. Violin Plot

library(vioplot)

vioplot(SE_MPMA$MusicPerceptionScores)

vioplot(SE_MPMA$MusicPerceptionScores,
        main = "Violin Plot of Music Perception Scores (N = 150)", 
        ylab = "Distribution of Music Perception Scores",          
        names = "",                                                
        ylim = c(-0.8, 0.6),                                       
        las = 1,                                                   
        col = "Gray54",                                            
        lineCol = "white",                                         
        rectCol = "Gray74",                                        
        colMed = "black",                                          
        border = "black")                                          

# 6.3.2.8. Quantile-Quantile (Q-Q) Plot   

library(car)

qqPlot(SE_MPMA$MusicPerceptionScores)

SE_MPMA$MusicPerceptionScores[1]
SE_MPMA$MusicPerceptionScores[29]

qqPlot(SE_MPMA$MusicPerceptionScores, 
       id = list(n = 4),                                                  
       main = "Quantile-Quantile (Q-Q) Plot of Music Perception Scores",    
       ylab = "Music Perception Score Quantiles",                         
       xlab = "Normal Theoretical Quantiles",
       las = 1)                             

# 6.4. Visualizing Bivariate Data

# 6.4.1. Categorical and Continuous Variables

# 6.4.1.1. Stratified Boxplot

boxplot(SE_MPMA$MusicPerceptionScores ~ SE_MPMA$YearsParticipating)

boxplot(SE_MPMA$MusicPerceptionScores ~ SE_MPMA$YearsParticipating,
        main = "Boxplot of Music Perception Scores by Years Participating (N = 150)",
        xlab = "Years Participating",
        ylab = "Music Perception Scores",
        las = 1,
        cex.axis = .8,
        col = "gray")

# 6.4.1.2. Stratified Lattice Histograms and Kernel Density Plots

library(lattice)

histogram(~ SE_MPMA$MusicPerceptionScores | SE_MPMA$YearsParticipating,
          main = "Distribution of Music Perception Scores by Years Participating (N = 150)",
          xlab = "Music Perception Scores",
          ylab = "Relative Frequency (%)",
          col = gray.colors(8, 0, 1),
          strip = strip.custom(bg = "gray96"),
          index.con = list(c(3, 4, 5, 1, 2)))

densityplot(~ SE_MPMA$MusicPerceptionScores | SE_MPMA$YearsParticipating,
            main = "Distribution of Music Perception Scores by Years Participating (N = 150)",
            xlab = "Music Perception Scores",
            ylab = "Relative Frequency (%)",
            col = gray.colors(8, 0, 1),
            strip = strip.custom(bg = "gray96"),
            index.con = list(c(3, 4, 5, 1, 2)),
            plot.points = FALSE,
            adjust = 2)

# 6.4.1.3. Stratified Kernel Density Plots Overlapped

MP_split <- split(SE_MPMA$MusicPerceptionScores, as.factor(SE_MPMA$YearsParticipating))
MP_split

YP1 <- MP_split$`Less than 1 year`
YP2 <- MP_split$`1-2 years`
YP3 <- MP_split$`2-3 years`
YP4 <- MP_split$`3-5 years`
YP5 <- MP_split$`More than 5 years`

plot(density(YP1),
     adjust = 2,
     ylim = c(0.0, 12.0),
     las = 1,
     bty = "n",
     main = "Kernel Density Distributions of Music Perception Scores \n by Years Participating (N = 150)",
     xlab = "Music Perception Scores",
     ylab = "Probability Density Function (PDF)")
lines(density(YP2, adjust = 2))
lines(density(YP3, adjust = 2))
lines(density(YP4, adjust = 2))
lines(density(YP5, adjust = 2))


plot(density(YP1, adjust = 2),
     ylim = c(0.0, 12.0),
     las = 1,
     bty = "n",
     main = "Kernel Density Distributions of Music Perception Scores \n by Years Participating (N = 150)",
     xlab = "Music Perception Scores",
     ylab = "Probability Density Function (PDF)",
     lty = 1)
lines(density(YP2, adjust = 2), lty = 2)
lines(density(YP3, adjust = 2), lty = 3)
lines(density(YP4, adjust = 2), lty = 4)
lines(density(YP5, adjust = 2), lty = 5)
legend("topright",
       inset = c(0.1, 0.1),
       legend = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"),
       col = "black", lty = 1:5)


plot(density(YP1, adjust = 2),
     ylim = c(0.0, 12.0),
     las = 1,
     bty = "n",
     main = "Kernel Density Distributions of Music Perception Scores \n by Years Participating (N = 150)",
     xlab = "Music Perception Scores",
     ylab = "Probability Density Function (PDF)")
polygon(density(YP1, adjust = 2), col = "gray98")
polygon(density(YP2, adjust = 2), col = "gray88")
polygon(density(YP3, adjust = 2), col = "gray78")
polygon(density(YP4, adjust = 2), col = "gray68")
polygon(density(YP5, adjust = 2), col = "gray58")
legend("topright",
       inset = c(0.1, 0.1),
       legend = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"),
       fill = c("gray98", "gray88", "gray78", "gray68", "gray58"))

library(yarrr)

plot(density(YP1),
     ylim = c(0.0, 12.0),
     las = 1,
     bty = "n",
     main = "Kernel Density Distributions of Music Perception Scores \n by Years Participating (N = 150)",
     xlab = "Music Perception Scores",
     ylab = "Probability Density Function (PDF)")
polygon(density(YP1), col = transparent(orig.col = "gray98", trans.val = .8))
polygon(density(YP2), col = transparent(orig.col = "gray88", trans.val = .8))
polygon(density(YP3), col = transparent(orig.col = "gray78", trans.val = .8))
polygon(density(YP4), col = transparent(orig.col = "gray68", trans.val = .8))
polygon(density(YP5), col = transparent(orig.col = "gray58", trans.val = .8))
legend("topright",
       inset = c(0.1, 0.1),
       legend = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"),
       fill = c("gray98", "gray88", "gray78", "gray68", "gray58"))


# 6.4.1.4. Stratified Violin Plots

library(vioplot)

vioplot(YP1, YP2, YP3, YP4, YP5,
        names = c("Less than 1 year", "1-2 years", "2-3 years", "3-5 years", "More than 5 years"),
        main = "Violin Plots of Music Perceptions Scores by Years Participating (N = 150)",
        ylab = "Music Perception Scores",
        las = 1,
        ylim = c(-0.8, 0.6),
        col = gray.colors(5))


# 6.4.2. Plotting Bivariate Continuous Data

# 6.4.2.1. Scatterplot

plot(SE_MPMA$MusicAptitudeScores, SE_MPMA$MusicPerceptionScores)

plot(SE_MPMA$MusicAptitudeScores, SE_MPMA$MusicPerceptionScores,
     main = "Scatterplot of Music Perception and Music Aptitude Scores (N = 150)",
     xlab = "Music Aptitude Scores",
     ylab = "Music Perception Scores",
     las = 1,
     pch = 20,
     col = "black")
abline(lm(SE_MPMA$MusicAptitudeScores ~ SE_MPMA$MusicPerceptionScores))

# 6.5. Closing Out the Chapter

save_workspace
save_history

