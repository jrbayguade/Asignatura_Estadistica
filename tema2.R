# Create a frequency table ****************************************
# values in the data set
xi <- c(3, 1, 2, 1, 2, 1, 2, 4, 1, 3, 5, 2, 2, 5, 4, 4, 4, 5, 1, 2)

# create a frequency table
freq_table <- as.data.frame(table(xi))

# Give the columns good names
names(freq_table) <- c("xi", "fi")

# Add accumulated frequency column
freq_table$Fi <- cumsum(freq_table$fi)

# Add relative frequency column (hi or fr)
freq_table$hi <- freq_table$fi/sum(freq_table$fi)

# Add accumulated relative frequency column (Hi)
freq_table$Hi <- cumsum(freq_table$hi)

# Show histogram
hist(xi, breaks = length(xi))

# Print the frequency table
print(freq_table)

# Create a binned frequency table: continuous data ******************
# It is unpractical to create a frequency table with continuous data
# since there is almost no repetition. For that purpose we can
# "discretize" the data by putting it into bins

values_continuous <- c(2.87, 2.44, 3.49, 3.83, 3.97, 4.69, 3.35, 1.89, 3.90, 3.55,
            4.69, 3.03, 3.00, 4.96, 3.10, 1.84, 2.23, 3.64, 1.96, 4.39,
            3.15, 3.61, 4.43, 2.96, 2.04, 2.62, 3.96, 2.41, 4.03, 4.70,
            5.33, 3.19, 3.19, 5.03, 3.92, 1.93, 2.74, 2.83, 3.03, 2.64,
            3.70, 1.41, 3.87, 1.04, 2.43, 2.87, 3.44, 0.92, 4.22, 2.88)

# Break within the intervals
n <- length(values_continuous)
num_intervals <- round(sqrt(n))
amplitude <- diff(range(values_continuous)) / num_intervals


breaks_manual <- seq(from = min(values_continuous), 
                     to = max(values_continuous) + 0.001, 
                     by = amplitude)

intervals <- cut(values_continuous, breaks = breaks_manual, 
                 right = FALSE, include.lowest = TRUE)

# Create a beautiful frequency table
freq_table_intervals <- as.data.frame(table(intervals))

# Give the columns good names
names(freq_table_intervals) <- c("xi", "fi")

# Add accumulated frequency column
freq_table_intervals$Fi <- cumsum(freq_table_intervals$fi)

# Add relative frequency column (hi or fr)
freq_table_intervals$hi <- freq_table_intervals$fi/sum(freq_table_intervals$fi)

# Add accumulated relative frequency column (Hi)
freq_table_intervals$Hi <- cumsum(freq_table_intervals$hi)

# Show histogram
hist(xi, breaks = length(xi))

# Show ft
print(freq_table_intervals)

# Data exploration with charts ***************************************
library("readxl")
df <- read_excel("tortugas.xlsx")
head(df)

# we create a sub dataset withthe columns we want
n_df <- df[,c('Año', 'peso', 'Huevos', 'playa', 'profNido', 'crias_Emerg')]
head(n_df)
summary(n_df)
str(n_df)

# Factor categorical variables (playa / beach)
n_df$playa <- factor(n_df$playa)

# Check for NaN / empty rows
complete.cases(n_df)

# there are some, let's quickly clean them up (omit)
n_df <- na.omit(n_df)

# let's check there are no nans now -> FALSE = success
anyNA(n_df)

# Check for NaN / empty rows
complete.cases(n_df)

library("ggplot2")
# Turtles weight histogram
p <- ggplot(n_df, aes(x = peso))
p + geom_histogram()

# Boxplot of weight per beach
p <- ggplot(n_df, aes(x = playa, y = peso))
p + geom_boxplot()

# Scatter plot with depth and baby turtles emerged
p <- ggplot(n_df, aes(x = profNido, y = crias_Emerg))
p + geom_point()

# Barplot with the mean of turtles per year
p <- ggplot(n_df, aes(x = factor(Año), y = crias_Emerg))
p + stat_summary(fun = mean, geom = "bar")

# Pie chart tests with plotrix**********************************************
library(plotrix) 
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France") 
pie3D(slices,labels=lbls,explode=0.1, main="Pie Chart of Countries ")

# Bars chart test with plotrix
counts <- table(mtcars$vs, mtcars$gear) 
counts
barplot(counts, main="Car Distribution by Gears and VS", xlab="Number of Gears", col=c("darkblue","red"),
legend = rownames(counts), beside=TRUE) 
barplot(counts, main="Car Distribution by Gears and VS", xlab="Number of Gears", col=c("darkblue","red"),
legend = rownames(counts))

# Assignments
# ***********************************************************************
# 1) In this example, the average time in minutes is collected for any given day
# that men and women dedicate to household tasks. Could you represent this data
# in a bar chart, drawing different bars for men and women?

# create a data frame
df_ass1 <- data.frame(
  task = c("Childcare", "Kitchen", "Grocery", "Cleaning", "Ironing"),
  male = c(30, 25, 15, 10, 10),
  female = c(70, 80, 40, 90, 45)
)
df_ass1$task <- factor(df_ass1$task)

# check it looks good
print(df_ass1)

# use the sexiest library for charts on eaRth
library(tidyr)
df_long <- pivot_longer(df_ass1, cols = c(male, female), 
                        names_to = "gender", values_to = "time")

# Stacked bar chart
ggplot(df_long, aes(x = task, y = time, fill = gender)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("male" = "#d4d64f", "female" = "#4f47bf")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2 ) The number of errors that a factory sensor has experienced over the course 
# of a month has been: 2, 1, 5, 3, 4, 1, 0, 1, 2, 3, 4, 3, 4, 0, 2, 4, 3, 
# 5, 6, 1, 2, 3, 4, 3, 2, 4, 1, 3, 4, 3.

# Graphically represent the frequency histogram of the number of errors.

# put the data in a vector
ds_errors <- c(2, 1, 5, 3, 4, 1, 0, 1, 2, 3, 4, 3, 4, 0, 2, 4, 3, 5, 6, 1, 
            2, 3, 4, 3, 2, 4, 1, 3, 4, 3)

# show built in histogram. quick and easy
hist(ds_errors)

# just for the sake of learning... I will make it using ggplot as well
df_long <- data.frame(errors = ds_errors)

ggplot(df_long, aes(x = errors)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Frequency histogram of number of errors",
       x = "number of errors",
       y = "frequency")  +
  scale_x_continuous(breaks = 0:6)

# 3) given the data set, draw a pie chart with the distribution
# create a vector
sports_vector <- c(soccer = 20, basket = 15, cycling = 12, swimming = 10, 
                   others = 3)
# Simple pie chart
pie(sports_vector)

# for the sake of learning, let's use ggplot now
# unfortunately we need a data frame for this purpose:
df_sports <- data.frame(
  sport = names(sports_vector),
  count = as.numeric(sports_vector)
)

# strike! (ugly as hell, but still...)
ggplot(df_sports, aes(x = "", y = count, fill = sport)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void()


# 3) A library has received the following phone calls over a determined period: 
# 8, 5, 9, 2, 0, 11, 13, 8, 9, 14, 3, 1, 16, 4, 8, 9, 11, 5, 2, 19, 9, 13,
# 17, 10, 4, 0, 3, 7, 18, 6.

# Create a distribution table with absolute and cumulative frequencies,
# grouping the data in intervals with amplitude 5 and indicating the class mark

# Represent the distribution of absolute frequencies through a histogram.
ass_3_values <- c(8, 5, 9, 2, 0, 11, 13, 8, 9, 14, 3, 1, 16, 4, 8, 9, 11, 5, 
                  2, 19, 9, 13, 17, 10, 4, 0, 3, 7, 18, 6)

n <- length(ass_3_values)
num_intervals <- round(sqrt(n))
amplitude <- 5

breaks_manual <- seq(from = min(ass_3_values), 
                     to = max(ass_3_values) + 0.001, 
                     by = amplitude)

intervals <- cut(ass_3_values, breaks = breaks_manual, 
                 right = FALSE, include.lowest = TRUE)

# Create a beautiful frequency table
freq_table_intervals <- as.data.frame(table(intervals))

# Give the columns good names
names(freq_table_intervals) <- c("xi", "fi")

# Add accumulated frequency column
freq_table_intervals$Fi <- cumsum(freq_table_intervals$fi)

# Add relative frequency column (hi or fr)
freq_table_intervals$hi <- freq_table_intervals$fi/sum(freq_table_intervals$fi)

# Add accumulated relative frequency column (Hi)
freq_table_intervals$Hi <- cumsum(freq_table_intervals$hi)

# Show histogram
hist(xi, breaks = length(xi))

# Show ft
print(freq_table_intervals)

# Extra, more on frequency tables ****************************************
# Frequency tables are important... so let's get more practice on the topic

data_vector <- c(3, 3, 8, 9, 3, 1, 3, 5, 4, 6, 6, 3, 2, 9, 8, 9, 7, 7, 7, 7)

absolute_freq <- table(data_vector)        # get absolute frequency
relative_freq <- prop.table(absolute_freq) # get relative frequency

# combine into a data frame
frequency_table <- data.frame(
  value = as.numeric(names(absolute_freq)),
  absolute = as.vector(absolute_freq),
  relative = as.vector(relative_freq)
)
print(frequency_table)

# histogram
hist(data_vector)

# with Ggplot
ggplot(frequency_table, aes(x = value, y = absolute)) +
  geom_col()


# Exercises  *************************************************************
# 1) Consider the following data set containing information about sex, age, 
# height, score and grade of 20 students:

# a) Create an absolute frequency table and a relative frequency table for 
# the variable Grade. Store the previous tables in two variables and call 
# them absolutes and relatives.

estudiantes <- data.frame(
  Sex = c("Mujer", "Hombre", "Mujer", "Mujer", "Mujer", "Hombre", "Mujer", 
           "Hombre", "Hombre", "Mujer", "Mujer", "Hombre", "Hombre", "Mujer", 
           "Mujer", "Hombre", "Mujer", "Mujer", "Mujer", "Mujer"),
  Age = c(21, 19, 18, 20, 23, 22, 22, 20, 21, 21, 22, 20, 22, 19, 19, 21, 
           20, 21, 22, 23),
  Height = c(1.82, 1.83, 1.78, 1.79, 1.80, 1.90, 1.79, 1.83, NA, 1.65, 
               1.73, 1.79, 1.80, 1.77, 1.69, 1.75, 1.66, NA, 1.79, 1.80),
  Score = c(5, 6, 7, 5, 9, 7, 8, 3, 9, 2, 5, 8, 7, 6, 3, 4, 5, 6, 2, 8),
  Grade = c("Aprobado", "Aprobado", "Notable", "Aprobado", "Sobresaliente",
                   "Notable", "Notable", "Suspenso", "Sobresaliente", "Suspenso",
                   "Aprobado", "Notable", "Notable", "Aprobado", "Suspenso",
                   "Suspenso", "Aprobado", "Aprobado", "Suspenso", "Notable")
)

absolutes <- table(estudiantes$Grade)
relatives <- prop.table(absolutes)

# b) Represent the Grade variable using a bar chart and a pie chart. 
# Include an appropriate title for each graph and color the bars and 
# sectors with different colors.

desired_order <- c("Suspenso", "Aprobado", "Notable", "Sobresaliente")

barplot(absolutes[desired_order], 
        col=c("red", "darkgreen", "green", "lightgreen"),
        main = "Grade",
        cex.names = 0.8)

pie(absolutes[desired_order],
    col=c("red", "darkgreen", "green", "lightgreen"),
    main = "Grade",
    cex.names = 0.8)

# c) For the Age variable, create a histogram and a box plot considering 
# the option range = 1.5. Include an appropriate title for each graph and 
# color the histogram bars yellow. Is there any outlier in this variable? 
# Reduce the range argument value to 0.5. Do the conclusions vary?

hist(estudiantes$Age, main = "Student's age histogram", col = "yellow",
     xlab = "Ages", ylab ="Frequency")
boxplot(estudiantes$Age, main = "Student's age boxplot", range = 1.5)
boxplot(estudiantes$Age, main = "Student's age boxplot", range = 0.5)

#  -> the range controls the reach of the whiskers, anything further away
#     is considered an outlier

# d) Create a summary of the Score variable using the summary command. Verify 
# that the measures provided by summary match the measures calculated 
# individually using their specific function.

summary(estudiantes$Score)
score_min <- min(estudiantes$Score)
score_max <- max(estudiantes$Score)
score_q25 <- quantile(estudiantes$Score, 0.25)
score_med <- quantile(estudiantes$Score, 0.5)
score_q75 <- quantile(estudiantes$Score, 0.75)
score_mean <- mean(estudiantes$Score)


# Problems given by the teacher through the class ************************
# ************************************************************************
# Communicate information from a dataset visually. Is it possible?

# We will analyze the R mtcars dataset by doing a Basic Descriptive Summary

# Mean: Average value of each variable.
# Median: Value that divides the data into two equal parts.
# Standard deviation: Measure of data dispersion with respect to the mean.
# Minimum/Maximum: Extreme values of the variables.

# Let's see if we can communicate the information that this dataset provides 
# to us visually.

head(mtcars)

# Summarize the data
summary(mtcars)

# Wider description of the data thanks to psych
library(psych)
describe(mtcars)

# Correlation matrix
cor(mtcars)

# Correlation matrix on a heat map 
library(corrplot)
corrplot(cor(mtcars), 
         method="color", 
         title ="Correlation matrix",
         number.cex = 0.7,
         addCoef.col = "black")

library(ggplot2)

# boxplot showing HP distribution by number of cylinders
ggplot(data = mtcars, mapping = aes(x = factor(cyl), y = hp)) +
  geom_boxplot(outlier.color = "red")

# Distribution of disp (whatever disp means)
ggplot(data = mtcars, mapping = aes(x = disp)) + 
  geom_histogram()

# Scatter plot showing negative correlation between mpg (miles per gallon) 
# and wt (weight).We could see -0.87 in the correlation matrix btween the vars
ggplot(data = mtcars, mapping = aes(x = mpg, y = wt)) +
  geom_point() +
  stat_smooth(method = lm)

# show a bar chart of number of cars per cylinder type
ggplot(data = mtcars, mapping = aes(x = cyl)) +
  geom_bar()

# probability density plot along with the histogram in HP
ggplot(data = mtcars, mapping = aes(x = hp)) +
  geom_histogram(bins=10, aes(y = after_stat(density))) +
  geom_density(colour = "blue", size = 2) 



# Simple Pareto Analysis Example in R ****************************************
# Sample data: defect types and their frequencies
defects <- c("A" = 50, "B" = 30, "C" = 15, "D" = 5)

# Sort in descending order
defects_sorted <- sort(defects, decreasing = TRUE)

# Calculate cumulative percentages
cumulative_freq <- cumsum(defects_sorted)
cumulative_pct <- cumulative_freq / sum(defects_sorted) * 100

df_defects <- data.frame(
  category = names(defects_sorted),
  frequency = as.numeric(defects_sorted),
  cumulative_freq,
  cumulative_pct
)

# Pareto chart using GGPlot ********************************************
library(ggplot2)
ggplot(data = df_defects, mapping = aes(x = category, y = frequency)) +
  geom_col() +
  geom_line(mapping = aes(x = category, y = cumulative_pct), group = 1, 
            colour = "red", size = 3) +
  xlab("Category") +
  ylab("Frequency")
  