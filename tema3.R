#----------------------------------------
# Discrete and Continuous Random Variables
# Probability Mass/Density Functions (PMF/PDF) 
# Cumulative Distribution Functions (CDF)
#----------------------------------------

# Split the plotting window into 2 rows and 3 columns
par(mfrow = c(2, 3))

### DISCRETE CASE: POISSON DISTRIBUTION (λ = 1) ###
lambda <- 1  

# 1. Empirical Histogram (Simulated Data)
Y_pois <- rpois(1000, lambda)  # Generate 1000 Poisson random values
hist(Y_pois, 
     main = "Histogram: Simulated Poisson(λ=1)", 
     xlab = "Values", 
     col = "lightblue",
     border = "white")

# 2. Theoretical Probability Mass Function (PMF)
x_pois <- seq(-0.01, 5, 0.01)  # Small-step sequence for smooth plotting
plot(x_pois, dpois(x_pois, lambda), 
     type = "s",  # Step-like plot for discrete distributions
     ylab = "P(X = x)", 
     main = "Theoretical PMF: Poisson(λ=1)")

# 3. Cumulative Distribution Function (CDF)
plot(x_pois, ppois(x_pois, lambda), 
     type = "s", 
     ylab = "F(x) = P(X ≤ x)", 
     main = "CDF: Poisson(λ=1)")

### CONTINUOUS CASE: NORMAL DISTRIBUTION (μ = 0, σ = 1) ###

# 4. Empirical Histogram (Simulated Data)
Y_norm <- rnorm(1000, 0, 1)  # Generate 1000 Normal random values
hist(Y_norm,
     main = "Histogram: Simulated N(0,1)", 
     xlab = "Values", 
     col = "lightgreen",
     border = "white")

# 5. Theoretical Probability Density Function (PDF)
curve(dnorm(x, 0, 1), 
      xlim = c(-5, 5), 
      ylab = "f(x)", 
      main = "PDF: Standard Normal(0,1)")

# 6. Cumulative Distribution Function (CDF)
curve(pnorm(x, 0, 1), 
      xlim = c(-5, 5), 
      ylab = "F(x) = P(X ≤ x)", 
      main = "CDF: Standard Normal(0,1)")


# *******************************************************************************
#----------------------------------------
# Normal distributions generation of different sample sizes
#----------------------------------------

# We simulate n values of a Normal distribution
n <- 50
title <- paste("Histogram with n=", n)

# Screen partition for the chart
par(mfrow = c(1, 1))
X = rnorm(n, 0, 1)

hist(X, freq = FALSE, col="lightsalmon", main = title, sub = "Datos simulados de una N(0,1)")
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = "blue", lwd = 2, add = TRUE)

# We create a chart with multiple plots with a bubble
# and generate N(0,1) with different growing sample sizes
par(mfrow=c(2,2))

cases <- c(50, 100, 1000, 10000)

for (i in 1:length(cases)){
  n <- cases[i]
  title <- paste("Histogram with n=", n)
  X <- rnorm(n, 0, 1)
  hist(X, freq=FALSE, col="lightsalmon", main = title, sub = "Simulated N(0,1)")
  curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = "blue", lwd = 2, add = TRUE)
}


# *******************************************************************************
#----------------------------------------
# Central Limit Theorem
#----------------------------------------

# Create N distributions of size n, calculate the mean of each and calculate 
# the mean of all of them after
N = 1000 
n = 100
Y = 0

for (i in 1:N) { 
  X=rnorm(n, 0, 1) 
  media.X=mean(X)
  print(paste('The mean for the iteration ',i,' is ',
              round(media.X,3),' variance = ',round(var(X),3)))
  Y <- c(Y,media.X) # store the means in a variable
}

Y <- Y[-1] # Remove the initial element
Y

# Calculate the mean of the means vector
mean(Y)

# Calculate the standard deviation of the means vector
sd(Y)

# Calculate the variance of the means vector 
var(Y)
1/sqrt(n) 

# we check that the means vector follows a normal distribution N(0,1/sqr(n))
titulo=paste('Histograma de la variable construida con las ',N,'medias')
hist(Y,freq=FALSE,col="lightsalmon",main=titulo,sub="Histograma de medias")
summary(Y) 
describe(Y)


#--------------------------------
# Teorema central del limite (mu, sigma) # Normalización
#--------------------------------
library(psych)
# Sean Xi 1,...N, variables aleatorias con media 20 y varianza 9, por ejemplo N(20,3)
N = 1000 
n = 500
Y = 0 
Z = 0

for (i in 1:N) 
{ 
  X=rnorm(n, 20, 3) 
  media.X=mean(X) 
  varianza.X=var(X) 
  normalizada=sqrt(n)*(media.X-20)/3 
  print(paste('La media para la iteración ',i,' es ',round(media.X,3),' varianza = ',round(var(X),3)))
  
  Y <- c(Y,media.X) # almacenamos las medias en una variable 
  Z <- c(Z,normalizada) # almacenamos la variable normalizada 
}

Y <- Y[-1] # Eliminamos el elemento inicial
Z <- Z[-1] 

Y
Z

# Calculamos la media del vector de medias 
mean(Y)
mean(Z) 

# Calculamos la desviación estándar del vector de medias 
sd(Y)
sd(Z) 

# Calculamos la varianza del vector de medias 
var(Y)
var(Z) 
3/sqrt(n) 
par(mfrow=c(1,2)) 

# podemos comprobar que el vector de medias Y sigue una distribución N(0,1/raíz(n))
titulo=paste('Histograma de las medias') 
hist(Y,freq=FALSE,col="lightsalmon", main=titulo, sub="Histograma de medias")
summary(Y) 
describe(Y) 
hist(Z,freq=FALSE,col="lightsalmon",main=titulo,sub="Histograma de la variable normalizada")
summary(Z) 
describe(Z)
  