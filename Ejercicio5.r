# Ejemplo a) Normal
x <- seq(-3, 9, 0.1)

y1 <- dnorm(x, mean = 3, sd = sqrt(7))
y2 <- dnorm(x, mean = 3, sd = 1)

max_y <- max(y1, y2)  

plot(x, y1, type = "l", main = "Gráfico de densidad de X ∼ N(3,7) e Y ∼ N(3,1)", 
     xlab = "Valores", ylab = "Densidad", lwd = 2, col = "blue", 
     ylim = c(0, max_y * 1.1))  

lines(x, y2, col = "red", lwd = 2)


#Ejemplo a) Gamma
x <- seq(0, 9, 0.1)

y3 <- dgamma(x,shape=2,scale=1)
y4 <- dgamma(x,shape=0.1, scale=1/5)

y4[!is.finite(y4)] <- NA 

max_y <- max(y3, y4, na.rm = TRUE) 

plot(x, y3, type = "l", main = "Gráfico de densidad de X ∼ gamma(2,1) e Y ∼ gamma(0.1,5)", 
     xlab = "Valores", ylab = "Densidad", lwd = 2, col = "yellow", 
     ylim = c(0, max_y * 1.1))  

lines(x, y4, col = "green", lwd = 2)

#Ejemplo B) 
#P(-0,5<=Z<=1)
probabilidad1 <- pnorm(1) - pnorm(-0.5)
probabilidad1
x <- seq(-2, 2, by = 0.05) 
z1 <- dnorm(x, 0, 1) 
plot(x, z1, type = "l", main = "Gráfico de densidad de Z ∼ N(0,1)", 
     xlab = "Valores", ylab = "Densidad", lwd = 2, col = "red")
x2 <- seq(-0.5, 1, 0.01)
x2 = c(-0.5, x2, 1)
y1 <- dnorm(x2) 
y1 = c(0, y2, 0)
polygon (x2, y2, col="lightblue", border=NA)

#P(Z<= -10)
probabilidad2 <- pnorm(-10)
probabilidad2

#P(x>=1,2)
probabilidad3 <- 1 - pnorm(1.2, mean = 5, sd = sqrt(2))
probabilidad3
x3 <- seq(-1, 12, by = 0.05) 
y3 <- dnorm(x3, mean = 5, sd = sqrt(2))
plot(x3, y3, type = "l", main = "Gráfico de densidad de X ∼ N(5,2)", 
     xlab = "Valores", ylab = "Densidad", lwd = 2, col = "orange")
x4 <- seq(1.2, 20, 0.01)
y4 <-  dnorm(x4, mean = 5, sd = sqrt(2))
x4 = c(1.2, x4, 20)
y4 = c(0, y4, 0)
polygon(x4, y4, col = "lightblue", border=NA)

#Ejemplo c)
#i) P(Y>=z_0)
mu_Y <- 2
sigma_Y <- 4
p <- 0.117
z0_Y <- qnorm(p, mean = mu_Y, sd = sigma_Y, lower.tail = FALSE)
z0_Y 
curve(dnorm(x, mean = mu_Y, sd = sigma_Y), from = -15, to = 15, col = "blue", lwd = 2, 
      ylab = "Densidad", xlab = "Y", main = "Distribución Normal de Y")
abline(v = z0_Y, col = "red", lty = 2)
text(z0_Y, 0.02, labels = paste("z_0 =", round(z0_Y, 2)), pos = 4, col = "red")

#ii) P(-z_0 <= Z <= z_0) -------------
# calcular el cuantil para P(-z0 ≤ Z ≤ z0)
p <- 0.9 + (0.10 / 2)
z0_Z <- qnorm(p)
cat("El valor de z0 es:", z0_Z, "\n")

# crear una secuencia de valores z para la gráfica
z_vals <- seq(-4, 4, length = 1000)

# calculamos la función de densidad de probabilidad (normal estándar)
densidad <- dnorm(z_vals, mean = 0, sd = 1)

# graficamos
plot(z_vals, densidad, type = "l", lwd = 2, col = "blue",
     xlab = "Valores de Z", ylab = "Densidad",
     main = "Distribución Normal Estándar de Z")

# rellenamos el área bajo la curva entre -z0 y z0
polygon(c(-z0_Z, z_vals[z_vals >= -z0_Z & z_vals <= z0_Z], z0_Z),
        c(0, densidad[z_vals >= -z0_Z & z_vals <= z0_Z], 0),
        col = "lightblue", border = NA)
# agregamos líneas verticales en -z0 y z0
abline(v = c(-z0_Z, z0_Z), col = "red", lwd = 2, lty = 2)

# Ponemos un texto en los puntos en -z0 y z0
text(-z0_Z, 0.05, paste("-z0 =", round(-z0_Z, 2)), pos = 4, col = "red")
text(z0_Z, 0.05, paste("z0 =", round(z0_Z, 2)), pos = 4, col = "red")

#iii) P(-z_0 <= X <= z_0) = P(-z_0/0.1 <= Z <= z_0/0.1)
# parámetros de la normal X
mean_X <- 0
sd_X <- sqrt(0.01)  # Desviación estándar

# Calcular el cuantil para P(-z0 ≤ X ≤ z0)
p <- 0.9 + (0.10 / 2)
z0_X <- qnorm(p) * sd_X
cat("El valor de z0 para X es:", z0_X, "\n")

# creamos una secuencia de valores para la gráfica de X
x_vals <- seq(-0.5, 0.5, length = 1000)

# calculamos la función de densidad de probabilidad para X
densidad_X <- dnorm(x_vals, mean = mean_X, sd = sd_X)

# graficamos
plot(x_vals, densidad_X, type = "l", lwd = 2, col = "blue",
     xlab = "Valores de X", ylab = "Densidad",
     main = "Distribución Normal para X")

# rellenamos el área bajo la curva entre -z0 y z0
polygon(c(-z0_X, x_vals[x_vals >= -z0_X & x_vals <= z0_X], z0_X),
        c(0, densidad_X[x_vals >= -z0_X & x_vals <= z0_X], 0),
        col = "lightblue", border = NA)

# agregamos líneas verticales en -z0 y z0
abline(v = c(-z0_X, z0_X), col = "red", lwd = 2, lty = 2)

# ponemos texto en los puntos en -z0 y z0 para representarlos
text(-z0_X, 0.05, paste("-z0 =", round(-z0_X, 2)), pos = 4, col = "red")
text(z0_X, 0.05, paste("z0 =", round(z0_X, 2)), pos = 4, col = "red")

#Ejemplo D)
# Calcular los percentiles
z0 <- qgamma(0.16, shape = 4, scale = 2)
z1 <- qgamma(0.84, shape = 4, scale = 2)

probabilidad <- pgamma(z1, shape = 4, scale = 2) - pgamma(z0, shape = 4, scale = 2)
probabilidad

# Mostrar los resultados
cat("El percentil 16% (z0) es:", z0, "\n")
cat("El percentil 84% (z1) es:", z1, "\n")

#Ejercicio 10
# Límites de interés
a = 8
b = 10

# Calcular las probabilidades acumuladas en a y b
P_a = pgamma(a, shape=9, scale=1/0.75)
P_b = pgamma(b, shape=9, scale=1/0.75)

# Probabilidad de que X esté entre 8 y 10
probabilidad = P_b - P_a
probabilidad


#Ejercicio 6
#a)
# Calcular la probabilidad de que X > 100
proba <- 1 - pnorm(100, mean = 85.3, sd = 2.7)

# Mostrar el resultado
proba

#b) 
# Calculamos el x_0 donde la probabilidad de que X < x_0 = 0.95
x_0 <- qnorm(0.95, mean=85.3, sd=2.7)
x_0
proba <- pnorm(x_0, mean=85.3, sd=2.7)
proba

#c)
probabilidad2 <- pnorm (88, mean=85.3, sd= 2.7) - pnorm (82.6, mean=85.3, sd= 2.7)
probabilidad2 <- probabilidad2 * 100
probabilidad2 

