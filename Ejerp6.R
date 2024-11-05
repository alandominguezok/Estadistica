#Ejercicio 1)

#a)
alpha = (37.5/21.6)^2
lambda = (37.5/((21.6)^2))
x = seq(0, 110, 0.01)

densidad = dgamma (x, shape = alpha, scale = 1/lambda)

plot (x, densidad, type = "l" , xlab = "X", ylab = "Densidad", col = "blue")


#b)
qgamma (.75, shape = alpha, scale = 1/lambda)

#c)
R <- 800
n <- 25

cuantiles_75 <- rep(0,R)
set.seed(34)

for(i in 1:R) {
cuantiles_75[i] <- quantile( rgamma(n, shape = alpha, scale = 1/lambda), .75)
}

mean(cuantiles_75)
hist(cuantiles_75, ylab="Frecuencia")

#e)
errorEstandar <- sd(cuantiles_75);
errorEstandar
cat("El error estandar es de ", errorEstandar, " min y, dado que la media es de ", mean(cuantiles_75), "min, representa un 14,7% por lo tanto el error sigue siendo una fracción significativa de tita sombrero")

#Ejercicio 7 a)
qnorm(0.96)

extremoIzq = 16.2 -  1.750686 * (3.6/(sqrt(120)))
extremoIzq

extremoDer = 16.2 +  1.750686 * (3.6/(sqrt(120)))
extremoDer

#Ejercicio 6)
z_0 = qnorm(0.975)
z_0

#Ejercicio 5)
z_005 = qnorm(0.95)
z_005

#Ejercicio 4) 
z_0025 = qnorm(0.975)
z_0025