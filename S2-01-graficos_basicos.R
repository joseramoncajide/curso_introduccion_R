##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R
##########################################################################

head(cars) 

pairs(gap)

stripchart(cars$speed)

# Scatter Plot: Visualizar relaciones
# 
plot(cars)

plot(cars$speed, type="l")
plot(cars$speed, type="h")
plot(cars$speed, type="s")

plot(x= cars$speed, y = cars$dist)

plot(x= cars$speed, y = cars$dist, main = "Cars", sub = "Gráfico de dispersión", bty="n", cex=0.5, cex.axis=0.6, pch=19, xlab="Velocidad", ylab="Distancia de frenado")
abline( lm(cars$dist ~ cars$speed ), col="red")

scatter.smooth(x = cars$speed, y = cars$dist, main="Dist ~ Speed")


# Histograma: Frecuencias
par(mfrow=c(1, 2)) 
hist(cars$speed)
hist(cars$dist)
par(mfrow=c(1, 1)) 

hist(cars$speed, breaks = 12, main = "Histograma", xlab = "Velocidad")

# Box plot: Comprobar outliers

par(mfrow=c(1, 2))  
boxplot(cars$speed, main="Velocidad", sub=paste("Observación con outliers: ", boxplot.stats(cars$speed)$out)) 
boxplot(cars$speed, main="Distancia de frenado", sub=paste("Observación con outliers: ", boxplot.stats(cars$dist)$out))
par(mfrow=c(1, 1)) 


# Gráficos de densidad: Ver la distribución de una variable

par(mfrow=c(1, 2)) 
plot(density(cars$speed), main="Velocidad", ylab="Frecuencia")  
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Distancia de frenado", ylab="Frecuencia")  
polygon(density(cars$dist), col="red")
par(mfrow=c(1, 1)) 




