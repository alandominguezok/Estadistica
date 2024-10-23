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
R <- 10000
n <- 100

cuantil <- rep(0,R)
set.seed(34)

for(i in 1:R) {
cuantil[i] <- quantile( rgamma(n, shape = alpha, scale = 1/lambda), .75)
}

mean(cuantil)
hist(cuantil, ylab="Frecuencia")
