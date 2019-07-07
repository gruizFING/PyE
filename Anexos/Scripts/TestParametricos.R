######################################################################
###########################Test Parametricos##########################

#Cargo los Datos

datos=read.table("grupo11.txt")
Muestra1=datos[,1];Muestra2=datos[,2]


########################################################################
##################Comparo las dos muestras por TCL####################

# Como los datos de la muestra1 y la muestra2 son independientes 
# La hipotesis Ho corresponde a que las medias de las dos muestras
# son iguales y H1 que no son

# La region critica es R={|E|>=Za/2}  

Xn=mean(Muestra1);Yn=mean(Muestra2)
Sn1=0;Sn2=0
for (i in 1:100) {
	Sn1 = Sn1 + (Muestra1[i] - Xn)^2;
	Sn2 = Sn2 + (Muestra2[i] - Yn)^2
	}
Sn1=Sn1/99;Sn2=Sn2/99

E = (Xn - Yn) / sqrt((Sn1 + Sn2) / 100);E
Za2 = 1.96
abs(E) >= Za2
Sn1;Sn2
# El E cae en la region entonces se rechaza Ho

# La hipotesis Ho corresponde a que las medias de las dos muestras
# son iguales y H1 que la media de la muestra1 es menor a la de la
# muestra 2

# La region critica es R={E<=-Za} con E=media 

Za=1.65
E <= -Za

# Concluimos que la media de la muestra1 es menor a la de la
# muestra2



# Intervalos de confianza al 95% para la media de las muestras #

min = Xn - (Sn1*Za2 / sqrt(100));min
max = Xn + (Sn1*Za2 / sqrt(100));max
IC1 <- c(min,max)
IC1

min = Yn - (Sn2*Za2 / sqrt(100));min
max = Yn + (Sn2*Za2 / sqrt(100));max
IC2 <- c(min,max)
IC2
####################################################################


