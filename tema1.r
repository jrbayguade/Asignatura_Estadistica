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