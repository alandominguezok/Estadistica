f <- function(x) {
    (3/32)*(1-x)*(x-5)
}

encontrarK <- function(k) {
    resultado <- integrate(f, lower = 1, upper = 5, k = k)
    return (resultado$value - 1)
}
# Graficar la función f(x)
curve(f, from = -20, to = 10, col = "blue", lwd = 1, 
      main = "Gráfico de f(x) con k = 3/32", xlab = "x", ylab = "f(x)")

computarPa <- function(a,b,c){
    da = 0.9
    db = 0.8
    dc = 0.85
    d = da*a+db*b+dc*c
    ad = (da * a)/d
    return (ad)
}

print(computarPa(0.2,0.4,0.4))