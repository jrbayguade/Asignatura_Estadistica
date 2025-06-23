# This script demonstrates basic descriptive statistics on numeric data,
# including central tendency (mean, mode, median, quantiles),
# dispersion measures (range, variance, standard deviation, IQR, coefficient 
# of variation), and skewness to assess data asymmetry.

# It starts by analyzing a sample numeric vector `x`,
# then applies similar central tendency calculations to all numeric columns 
# of the built-in `mtcars` dataset.

# The code also illustrates the concept of kurtosis by generating three 
# sample distributions:
# mesokurtic (normal), leptokurtic (heavy-tailed), and platykurtic 
# (light-tailed), calculating and interpreting their kurtosis, excess kurtosis,
# and skewness, and visualizing them with histograms.

# Finally, it begins working on grouped data to manually calculate grouped 
# mean and median as an example of descriptive statistics for grouped 
# frequency distributions.


# *****************************************************
# INTERPRETATION OF SKEWNESS:
# Skewness measures the asymmetry of the distribution.
# - If skewness > 0: distribution is right-skewed (positive asymmetry)
#   => tail is longer on the right side.
# - If skewness < 0: distribution is left-skewed (negative asymmetry)
#   => tail is longer on the left side.
# - If skewness ≈ 0: distribution is approximately symmetric.

# *****************************************************
# INTERPRETATION OF KURTOSIS:
# Kurtosis measures the "tailedness" or peakedness of the distribution.
# - Kurtosis = 3 (Excess kurtosis = 0): Mesokurtic distribution
#   => similar to normal distribution, moderate tails and peak.
# - Kurtosis > 3 (Excess kurtosis > 0): Leptokurtic distribution
#   => sharper peak and heavier tails (more outliers).
# - Kurtosis < 3 (Excess kurtosis < 0): Platykurtic distribution
#   => flatter peak and lighter tails (less extreme outliers).

x <- c(3, 7, 5, 9, 12, 7, 5, 6, 4, 10)

# *****************************************************
# CENTRAL TENDENCY STATISTICS

# MEAN
x_mean <- mean(x)
cat("mean: ", x_mean)

# MODE
library(DescTools)
x_mode <- Mode(x)
cat("mode: ", x_mode)

# MEDIAN
x_median = median(x)
cat("median: ", x_median)

# QUANTILES
x_q25 = quantile(x, 0.25)
cat("quantile: ", x_q25)

# *****************************************************
# DISPERSION STATISTICS

# RANGE
x_range <- range(x)
cat("range: ", x_range)

# VARIANCE
x_variance <- var(x)
cat("variance: ", x_variance)

# STANDARD DEVIATION
x_sd <- sd(x)
cat("standard deviation: ", x_sd)

# INTERQUANTILE RANGE
x_iqr <- IQR(x)
cat("interquartile range: ", x_iqr)

# MEDIAN VARIATION COEFFICIENT
x_medvar <- sd(x)/mean(x)*100
cat("interquartile range: ", x_iqr)

# *****************************************************
# SKEWNESS
library(moments)
x_skew <- skewness(x)
# if Skewness > 0 => positive asymmetry -> positive skew -> right-skewed
# if Skewness < 0 => negative asymmetry -> negative skew -> left-skewed  
# if Skewness ≈ 0 => symmetric distribution (no skew)

# *****************************************************
# More central tendency measurements with MTCARS 
head(mtcars)
str(mtcars)

# Mean for all its numeric columns
mean_mpg <- mean(mtcars$mpg)
mean_cyl <- mean(mtcars$cyl)
mean_disp <- mean(mtcars$disp)
mean_hp <- mean(mtcars$hp)
mean_drat <- mean(mtcars$drat)
mean_wt <- mean(mtcars$wt)
mean_qsec <- mean(mtcars$qsec)
mean_vs <- mean(mtcars$vs)
mean_am <- mean(mtcars$am)
mean_gear <- mean(mtcars$gear)
mean_carb <- mean(mtcars$carb)

mode_mpg <- Mode(mtcars$mpg)
mode_cyl <- Mode(mtcars$cyl)
mode_disp <- Mode(mtcars$disp)
mode_hp <- Mode(mtcars$hp)
mode_drat <- Mode(mtcars$drat)
mode_wt <- Mode(mtcars$wt)
mode_qsec <- Mode(mtcars$qsec)
mode_vs <- Mode(mtcars$vs)
mode_am <- Mode(mtcars$am)
mode_gear <- Mode(mtcars$gear)
mode_carb <- Mode(mtcars$carb)

median_mpg <- median(mtcars$mpg)
median_cyl <- median(mtcars$cyl)
median_disp <- median(mtcars$disp)
median_hp <- median(mtcars$hp)
median_drat <- median(mtcars$drat)
median_wt <- median(mtcars$wt)
median_qsec <- median(mtcars$qsec)
median_vs <- median(mtcars$vs)
median_am <- median(mtcars$am)
median_gear <- median(mtcars$gear)
median_carb <- median(mtcars$carb)

quantile_mpg <- quantile(mtcars$mpg, 0.25)
quantile_cyl <- quantile(mtcars$cyl, 0.25)
quantile_disp <- quantile(mtcars$disp, 0.25)
quantile_hp <- quantile(mtcars$hp, 0.25)
quantile_drat <- quantile(mtcars$drat, 0.25)
quantile_wt <- quantile(mtcars$wt, 0.25)
quantile_qsec <- quantile(mtcars$qsec, 0.25)
quantile_vs <- quantile(mtcars$vs, 0.25)
quantile_am <- quantile(mtcars$am, 0.25)
quantile_gear <- quantile(mtcars$gear, 0.25)
quantile_carb <- quantile(mtcars$carb, 0.25)

# *****************************************************
# Kurtosis types
# We need to load this library to calculate kurtosis
library(moments)

# Generar dades per mostrar els 3 tipus de kurtosis
set.seed(123)  # Per resultats reproduïbles

# 1. MESOKÚRTICA (kurtosis ≈ 3, excess kurtosis ≈ 0)
# Normal distribution
mesokurtic <- rnorm(1000, mean = 0, sd = 1)

# 2. LEPTOKÚRTICA (kurtosis > 3, excess kurtosis > 0)
# Distribució amb cues més pesades - més punxaguda al centre
# We use a T distribution with little degrees of freedom
leptokurtic <- rt(1000, df = 3)

# 3. PLATIKÚRTICA (kurtosis < 3, excess kurtosis < 0)
# Distribució amb cues més lleugeres - més plana
# We use a uniform distribution
platykurtic <- runif(1000, min = -2, max = 2)

# Calculate the sknewness of each distribution:
snewness_meso <- skewness(mesokurtic)   # tends to zero, symmetric
snewness_lepto <- skewness(leptokurtic) # symmetric, very slightly right-skewed
snewness_platy <- skewness(platykurtic) # almost zero, symmetric

# Calculate kurtosis for each type
kurt_meso <- kurtosis(mesokurtic)
kurt_lepto <- kurtosis(leptokurtic)
kurt_platy <- kurtosis(platykurtic)

# Calculate excess of Kurtosis (kurtosis - 3)
excess_meso <- kurt_meso - 3
excess_lepto <- kurt_lepto - 3
excess_platy <- kurt_platy - 3

# Show the results
cat("=== TYPES OF KURTOSIS ===\n\n")

cat("1. MESOKÚRTICA (Normal):\n")
cat("   Kurtosis:", round(kurt_meso, 3), "\n")
cat("   Excess Kurtosis:", round(excess_meso, 3), "\n")
cat("   Interpretació: Distribució normal, referència estàndard\n\n")

cat("2. LEPTOKÚRTICA (Punxaguda):\n")
cat("   Kurtosis:", round(kurt_lepto, 3), "\n")
cat("   Excess Kurtosis:", round(excess_lepto, 3), "\n")
cat("   Interpretació: Més punxaguda al centre, cues més pesades\n\n")

cat("3. PLATIKÚRTICA (Plana):\n")
cat("   Kurtosis:", round(kurt_platy, 3), "\n")
cat("   Excess Kurtosis:", round(excess_platy, 3), "\n")
cat("   Interpretació: Més plana, cues més lleugeres\n\n")

# Descriptive statistics
cat("=== COMPARACIÓ ESTADÍSTIQUES ===\n")
cat("\nMesokúrtica (Normal):\n")
cat("Mean:", round(mean(mesokurtic), 3), "| SD:", round(sd(mesokurtic), 3), "\n")

cat("\nLeptokúrtica (t-distribution):\n")
cat("Mean:", round(mean(leptokurtic), 3), "| SD:", round(sd(leptokurtic), 3), "\n")

cat("\nPlatikúrtica (Uniforme):\n")
cat("Mean:", round(mean(platykurtic), 3), "| SD:", round(sd(platykurtic), 3), "\n")

# Interpretation rules
cat("\n=== INTERPRETATION RULES ===\n")
cat("• Kurtosis = 3 (Excess = 0): Mesokúrtica\n")
cat("• Kurtosis > 3 (Excess > 0): Leptokúrtica\n")
cat("• Kurtosis < 3 (Excess < 0): Platikúrtica\n")

# Visualize through histograms
par(mfrow = c(1, 3))

hist(mesokurtic, main = "Mesokúrtica\n(Normal)", 
     xlab = "Valors", col = "lightblue", breaks = 30)

hist(leptokurtic, main = "Leptokúrtica\n(t-distribution)", 
     xlab = "Valors", col = "lightcoral", breaks = 30)

hist(platykurtic, main = "Platikúrtica\n(Uniforme)", 
     xlab = "Valors", col = "lightgreen", breaks = 30)

par(mfrow = c(1, 1))  # Restore layout

# *****************************************************
# Activities (assignments)
# Given the following grouped data:
grouped_data <- data.frame(
  interval_min = seq(50, 160, by = 10),
  interval_max = seq(60, 170, by = 10),
  freq = c(1, 3, 6, 7, 56, 12, 10, 8, 6, 4, 2, 1)
)


# 1) With the grouped data, calculate the mean, the median and the modal class.
  # I will do the calculations "manually" first, step by step, later on using
  # built-in R functions
  # add the calculated xi (to get the mean) column to the data frame
grouped_data$xi <- with(grouped_data, 
                        grouped_data$interval_min + (grouped_data$interval_max 
                        - grouped_data$interval_min) / 2)

grouped_data$fi_xi <- with(grouped_data, grouped_data$freq * grouped_data$xi)

  # Grouped mean calculation *****************
grouped_mean <- sum(grouped_data$fi_xi) / sum(grouped_data$freq)
cat("gropued mean: ", grouped_mean)

  # Grouped median ***************************
  # we need the accumulated frequency as a column (F)
grouped_data$F <- with(grouped_data, cumsum(grouped_data$freq))
  
  # Total frequency
N <- sum(grouped_data$freq)
  
  # Find the median class: where cumulative frequency >= N/2
median_class_index <- which(grouped_data$F >= N/2)[1]
  
  # Get components of the formula
L <- grouped_data$interval_min[median_class_index]
F_prev <- if (median_class_index == 1) 0 else grouped_data$F[median_class_index - 1]
f_m <- grouped_data$freq[median_class_index]
h <- grouped_data$interval_max[median_class_index] - grouped_data$interval_min[median_class_index]

  # Apply the formula
grouped_median <- L + ((N/2 - F_prev) / f_m) * h

cat("Grouped median:", grouped_median)

  # Modal class*********************************
  # Find index of the modal class
modal_class_index <- which.max(grouped_data$freq)
  
  # Get the modal class interval
modal_class <- grouped_data[modal_class_index, c("interval_min", "interval_max")]

cat("Modal class interval: [", modal_class$interval_min, ", ", modal_class$interval_max, "]\n")

# 2) Determine the interquartile range and the interdecile range.
  # IQR ****************************************
  # Total frequency
N <- sum(grouped_data$freq)
  
  # Class width (assuming uniform width)
h <- grouped_data$interval_max[1] - grouped_data$interval_min[1]

  # Q1 --------------------------------
Q1_index <- which(grouped_data$F >= N/4)[1]
L1 <- grouped_data$interval_min[Q1_index]
F1 <- if (Q1_index == 1) 0 else grouped_data$F[Q1_index - 1]
f1 <- grouped_data$freq[Q1_index]
Q1 <- L1 + ((N/4 - F1) / f1) * h
  
  # Q3 --------------------------------
Q3_index <- which(grouped_data$F >= 3*N/4)[1]
L3 <- grouped_data$interval_min[Q3_index]
F3 <- if (Q3_index == 1) 0 else grouped_data$F[Q3_index - 1]
f3 <- grouped_data$freq[Q3_index]
Q3 <- L3 + ((3*N/4 - F3) / f3) * h

  # IQR
IQR_grouped <- Q3 - Q1

  # Output
cat("Q1:", Q1, "\n")
cat("Q3:", Q3, "\n")
cat("Grouped IQR:", IQR_grouped, "\n")

# 3) Calculate the variance and the standard deviation.
# 4) Calculate the coefficients of skewness and kurtosis.
  # Skewness **********************************
  # Pearson skewness. I'll use an approximation to the data or will die old here
approx_data <- rep(grouped_data$xi, grouped_data$freq)

  # Calculate mean, median, standard deviation and skewness
mean_est <- mean(approx_data)
var_est <- var(approx_data)
median_est <- median(approx_data)
sd_est <- sd(approx_data)
skew_est <- skewness(approx_data)
kurt_est <- kurtosis(approx_data)

  # Results
cat("Grouped mean (est.):", mean_est, "\n")
cat("Grouped variance (est.):", var_est, "\n")
cat("Grouped median (est.):", median_est, "\n")
cat("Grouped standard deviation (est.):", sd_est, "\n")
cat("Skewness (est.):", skew_est, "\n")
cat("Kurtosis (est.):", kurt_est, "\n")

hist(approx_data)

# Interpretation: as confirmed by the histogram and the skewness value,
# the distribution is approximately symmetric with a slight positive skew.
# The kurtosis excess of 0.88 indicates a leptokurtic distribution,
# meaning it has a sharper peak and heavier tails than a normal distribution.


# *****************************************************
# Problem: how to describe and resume a data set to understand its main
# characteristics? Assume we have the following data: 2, 5, 9, 3, 7, 1, 8

# We put the data in a proper dataframe
data_problem <- c(2, 5, 9, 3, 7, 1, 8)

summary(data_problem_IQR)

# Central tendency statistics:
# mean, median, mode, percentiles
data_problem_mean <- mean(data_problem)
data_problem_median <- median(data_problem)
library(DescTools)
data_problem_mode <- Mode(data_problem) # there is no mode
data_problem_p25 <- quantile(data_problem, 0.25)
data_problem_p50 <- quantile(data_problem, 0.50)
data_problem_p75 <- quantile(data_problem, 0.75)
data_problem_IQR <- IQR(data_problem)

# dispersion statistics (variance, standard deviation)
data_problem_range <- range(data_problem)
data_problem_variance <- var(data_problem)
data_problem_std <- sd(data_problem)

# visualization: distribution (histogram)
hist(data_problem, breaks = 0:10)

# Histogram with normal curve test ************************************
# We add a normal curve with x <- mtcars$mpg
h<-hist(x, breaks=10,  
        xlab="Miles Per Gallon", main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) # fix frequency vs density
lines(xfit, yfit, lwd=2)

# The same with GGPlot
library(ggplot2)
ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10) +
  stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)), 
                 size = 1)

# Boxplot example ******************************************************
# Boxplot of MPG by Car Cylinders 
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

# Simple scatter plot example ******************************************
plot(mtcars$wt, mtcars$mpg, main="Scatterplot Example", 
     xlab="Car Weight", ylab="Miles Per Gallon", pch=19)

# High Density Scatterplot with Binning ********************************
library(hexbin) 
x <- rnorm(1000) 
y <- rnorm(1000)
bin<-hexbin(x, y, xbins=30) 
plot(bin, main="Hexagonal Binning")


# 3D Scatterplot 
library(scatterplot3d) 
scatterplot3d(mtcars$wt,mtcars$disp,mtcars$mpg, main="3D Scatterplot")

# Dotplot: Grouped Sorted and Colored 
# Sort by mpg, group and color by cylinder 
x <- mtcars[order(mtcars$mpg),] # sort by mpg 
x$cyl <- factor(x$cyl) # (convert from numeric to categorical)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue" 
x$color[x$cyl==8] <- "darkgreen" 
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl, 
         main="Gas Milage for Car Models\ngrouped by cylinder", 
         xlab="Miles Per Gallon", gcolor="black", color=x$color)


# Assignments
# ************