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
                                                                                                             main="Pie Chart of Countries ")
