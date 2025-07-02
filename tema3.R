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

# ******************************************************************************
#-----------------------------------------------------------------------------
# Vizualització concepte de variables aleatories, funció de probabilitat, 
# funció de densitat i funció de distribució acumulada
#-----------------------------------------------------------------------------
# Variable Contínua: Distribució Normal  
# Explicació:
#   dnorm: Calcula la PDF (densitat) en cada punt.
#   pnorm: Calcula la CDF (probabilitat acumulada fins a x).
#   Probabilitat P(0≤X≤1)=F(1)−F(0).

# Configuració
set.seed(123)
x <- seq(-4, 4, length.out = 1000)
mu <- 0    # Mitjana
sigma <- 1 # Desviació estàndard

# PDF (Densitat)
pdf_normal <- dnorm(x, mean = mu, sd = sigma)

# CDF (Distribució acumulada)
cdf_normal <- pnorm(x, mean = mu, sd = sigma)

# Probabilitat P(0 ≤ X ≤ 1)
prob <- pnorm(1, mean = mu, sd = sigma) - pnorm(0, mean = mu, sd = sigma)
cat("P(0 ≤ X ≤ 1) =", prob, "\n")  # Resultat: ~0.3413

# Gràfics
par(mfrow = c(1, 2))
plot(x, pdf_normal, type = "l", col = "blue", main = "PDF (Normal)", xlab = "x", ylab = "Densitat")
abline(v = c(0, 1), col = "red", lty = 2)
plot(x, cdf_normal, type = "l", col = "green", main = "CDF (Normal)", xlab = "x", ylab = "Probabilitat acumulada")
abline(v = c(0, 1), col = "red", lty = 2)

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Variable discreta: Distribució binomial
# Explicació:
#   dbinom: Calcula la PMF (probabilitat exacta per a cada k).
#   pbinom: Calcula la CDF (probabilitat acumulada).
#   Probabilitat:P(3≤X≤5) = F(5) − F(2) = F(5) − F(2)

# Configuració
n <- 10   # Nombre d'intents
p <- 0.5  # Probabilitat d'èxit
k <- 0:n  # Valors possibles

# PMF (Probabilitats puntuals)
pmf_binom <- dbinom(k, size = n, prob = p)

# CDF (Probabilitat acumulada)
cdf_binom <- pbinom(k, size = n, prob = p)

# Probabilitat P(3 ≤ X ≤ 5)
prob <- pbinom(5, size = n, prob = p) - pbinom(2, size = n, prob = p)
cat("P(3 ≤ X ≤ 5) =", prob, "\n")  # Resultat: ~0.568

# Gràfics
par(mfrow = c(1, 2))
plot(k, pmf_binom, type = "h", lwd = 2, col = "purple", main = "PMF (Binomial)", xlab = "k", ylab = "P(X = k)")
points(k, pmf_binom, col = "purple", pch = 16)
plot(k, cdf_binom, type = "s", col = "orange", main = "CDF (Binomial)", xlab = "k", ylab = "P(X ≤ k)")
abline(v = c(3, 5), col = "red", lty = 2)

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Comparació gràfica

# Torna a configurar els gràfics
par(mfrow = c(2, 2))

# Normal: PDF i CDF
plot(x, pdf_normal, type = "l", col = "blue", main = "PDF (Normal)")
plot(x, cdf_normal, type = "l", col = "green", main = "CDF (Normal)")

# Binomial: PMF i CDF
plot(k, pmf_binom, type = "h", lwd = 2, col = "purple", main = "PMF (Binomial)")
points(k, pmf_binom, col = "purple", pch = 16)
plot(k, cdf_binom, type = "s", col = "orange", main = "CDF (Binomial)")

# ******************************************************************************
#----------------------------------------
# Cuaderno actividades T4
#----------------------------------------

# Ejercicio 1
#
# Sea X una variable aleatoria que toma los valores 30, 40, 50 y 60 con
# probabilidades 0,4; 0,2; 0,1 y 0,3.
#
# Se pide: representar la tabla con la función de probabilidad,
# P(X = x) = P(xi) = Pi y la función de distribución de probabilidad,
# F(X) = P(X≤x), y calcular las siguientes probabilidades:
#
# P(X≤25), P(X≥60), P(X < 40), P(30≤X≤60), P(30 < X≤60)
#
# Calcular también la esperanza y varianza de X.

# Crear vectores con los datos del problema
xi <- c(30, 40, 50, 60)
pi <- c(0.4, 0.2, 0.1, 0.3)

# Crear tabla de función de probabilidad
df <- data.frame(xi = xi, pi = pi)

# Función de distribución (probabilidades acumuladas)
df$Fi <- cumsum(df$pi)

# Verificar que las probabilidades suman 1
sum(df$pi)

df

# P(X≤25)
sum(df$pi[df$xi <= 25])

# P(X≥60)
sum(df$pi[df$xi >= 60])

# P(X < 40)
sum(df$pi[df$xi < 40])

# P(X > 40)
sum(df$pi[df$xi > 40])

# P(30≤X≤60)
sum(df$pi[df$xi >= 30 & df$xi <= 60])

# P(30 < X≤ 60)
sum(df$pi[df$xi > 30 & df$xi <= 60])

# Esperanza E(X) = Σ xi * pi
esperanza <- sum(df$xi * df$pi)
esperanza

# Varianza Var(X) = E(X²) - [E(X)]²
# Primero calculamos E(X²)
esperanza_x2 <- sum(df$xi^2 * df$pi)
varianza <- esperanza_x2 - esperanza^2
varianza


# Ejercicio 2
#
# Sea X una variable aleatoria, cuya función de probabilidad está determinada
# por: X = 10, 12, 14, 15, 17, 20 y P(X) = 0,1; 0,3; 0,25; 0,14; 0,06; 0,15.
#
# Calcular la esperanza y varianza de la variable aleatoria X.
#
# Calcular la función de distribución de probabilidad y determinar los valores
# de: F(33), F(14,5), F(3), P(10,5 < X≤17,5).

# Creamos vectores con los datos del problema
xi <- c(10, 12, 14, 15, 17, 20)
pi <- c(0.1, 0.3, 0.25, 0.14, 0.06, 0.15)

# Crear tabla de función de probabilidad
df <- data.frame(xi = xi, pi = pi)

# Añadir distribución acumulada
df$Fi <- cumsum(df$pi)

# Visualizamos data frame
df

# Verificar que las probabilidades suman 1
sum(df$pi)

# F(33) = F(X<=33)
sum(df$pi[df$xi<=33])

# F(14.5) = F(X<=14.5)
sum(df$pi[df$xi<=14.5])

# F(3) = F(X<=3)
sum(df$pi[df$xi<=3])

# P(10.5 < X≤17.5)
sum(df$pi[df$xi>10.5 & df$xi<=17.5])

# Esperanza E(X) = Σ xi * pi
esperanza <- sum(df$xi * df$pi)
esperanza

# Varianza Var(X) = E(X²) - [E(X)]²
# Primero calculamos E(X²)
esperanza_x2 <- sum(df$xi^2 * df$pi)
varianza <- esperanza_x2 - esperanza^2
varianza


# Ejercicio 3
#
# Las ventas mensuales realizadas por un periódico (en miles) siguen un modelo
# normal de media 200 y desviación típica 40. Calcular:
#
# - La probabilidad de que las ventas de un mes sean superiores a 300.
# - La probabilidad de que las ventas de un mes se encuentren entre 160 y 240.
# - La probabilidad de que las ventas de un mes no superen 150.
# - La probabilidad de que las ventas de un mes superen 3000.

# En esta ocasión estamos ante una distribución contínua *****

# Parámetros de la distribución N(200, 40)
media <- 200
sd <- 40

# P(X > 300)
prob1 <- 1 - pnorm(300, mean = media, sd = sd)
prob1

# P(160 ≤ X ≤ 240)  
prob2 <- pnorm(240, mean = media, sd = sd) - pnorm(160, mean = media, sd = sd)
prob2

# P(X ≤ 150)
prob3 <- pnorm(150, mean = media, sd = sd)
prob3

# P(X > 3000)
prob4 <- 1 - pnorm(3000, mean = media, sd = sd)
prob4
