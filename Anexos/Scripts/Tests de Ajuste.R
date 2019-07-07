# Cargo los datos

datos=read.table("grupo11.txt")
Muestra1=datos[,1];Muestra2=datos[,2]


# Tests de Ajuste #

# Estimacion de las densidades #

# Graficas #

par(mfrow=c(2,2))
hist(Muestra1)
plot(density(Muestra1))
hist(Muestra2)
plot(density(Muestra2))


plot(density(Muestra1))
hist(Muestra1,add=TRUE,prob=TRUE)

plot(density(Muestra2))
hist(Muestra2,add=TRUE,prob=TRUE)


################################################################################
################# Test de Lilliefors de ajuste exponencial #####################

# La hipotesis Ho corresponde a que los datos de la muestra se ajustan a una 
# exponencial de parametro estimado por MVS y H1 que no 

# Muestra1
# Estimo por maxima verosimilitud el parametro p1 como 1/Xn, Xn = promedio
# de los datos de la muestra1

Xn=mean(Muestra1);Xn
p1=1/Xn;p

# Hago el test de Kolmogorov-Smirnov usando el parametro p1
# para averiguar el estadistico Dn

ks.test(Muestra1,"pexp",p1)
Dn=0.0585
# pv para 0.1 de error
pv=0.96/sqrt(100);pv
Dn<pv
# pv para 0.2 de error
pv=0.86/sqrt(100);pv
Dn<pv

# Se concluye que no se rechaza la hipotesis nula Ho para la muestra1 #

# Muestra2
# Estimo por maxima verosimilitud el parametro p2 como 1/Xn, Xn = promedio
# de los datos de la muestra2

Xn=mean(Muestra2);Xn
p2=1/Xn;p2

# Hago el test de Kolmogorov-Smirnov usando el parametro p2
# para averiguar el estadistico Dn

ks.test(Muestra2,"pexp",p2)
Dn=0.0395
# pv para 0.1 de error
pv=0.96/sqrt(100);pv
Dn<pv
# pv para 0.2 de error
pv=0.86/sqrt(100);pv
Dn<pv

# Se concluye que no se rechaza la hipotesis nula Ho para la muestra2 #

# En conclusion ninguna de las dos muestras rechazan la hipotesis que sus datos 
# tengan distribucion exponencial
################################################################################

#####################################
###### Test Kolmogorov-Smirnov ######
#####################################

M1_1=matrix(nrow=50,ncol=1)
for (i in 1:50) {
	M1_1[i]=Muestra1[i]
	}
M1_2=matrix(nrow=50,ncol=1)	
for (i in 1:50) {
	M1_2[i]=Muestra1[i+50]
	}
ks.test(M1_2,"pexp",1/mean(M1_1))
ks.test(M1_1,"pexp",1/mean(M1_2))

# No se rechaza la hipotesis nula Ho para la muestra1 #

M2_1=matrix(nrow=50,ncol=1)
for (i in 1:50) {
	M2_1[i]=Muestra2[i]
	}
M2_2=matrix(nrow=50,ncol=1)	
for (i in 1:50) {
	M2_2[i]=Muestra2[i+50]
	}
ks.test(M2_2,"pexp",1/mean(M2_1))
ks.test(M2_1,"pexp",1/mean(M2_2))

# No se rechaza la hipotesis nula Ho para la muestra2 #
 
