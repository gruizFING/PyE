# Cargo los datos

datos=read.table("grupo11.txt")
Muestra1=datos[,1];Muestra2=datos[,2]

# Test de aleatoriedad para las Muestras


# Test de rachas de ascensos y descensos para la Muestra1
# La hipotesis H0 corresponde a que la muestra es iid y H1 que no lo es

Tabla1=matrix(nrow=100,ncol=3)
Tabla1[,1]=Muestra1
colnames(Tabla1)=c("Datos1","Subidas","Marca")
colnames(Tabla1)

# Calculo las secuencias de 0's y 1's(1 siendo una subida y 0 una bajada)
# y los introduzco en la tabla
for (i in 1:99) {
	if (Muestra1[i] <= Muestra1[i+1])
		Tabla1[i,2]=1  
	else 
		Tabla1[i,2]=0 
}

# Marco con 1 cuando es el fin de racha y 0 en otro caso
for (i in 1:98) {
	if (Tabla1[i,2] != Tabla1[i+1,2])
		Tabla1[i,3]=1  
	else 
		Tabla1[i,3]=0 
}
Tabla1[99,3]=1;Tabla1[100,3]=0
edit(Tabla1)
# Sumo los elementos de la tercer columna para obtener el total de rachas 
Rachas1=sum(Tabla1[,3]);Rachas1

# Como la muestra es de tamaño mayor a 25, entonces se usa la "aproximacion normal"

m = (2*100 - 1) / 3;m
Rachas1 < m

# El numero de rachas es menor a m, entonces el p-valor p1=P(Z<=zl), Z con N(0,1)

zl = (Rachas1 + 0.5 - m) / sqrt((16*100 -29) / 90);zl

# Calculo el p-valor usando la distribucion normal
p1=pnorm(zl);p1

# Tomando el error de tipoI errI=0.10
errI=0.1
p1>errI
# El p-valor es mayor que el errI, entonces no se rechaza la hipotesis H0
# para la Muestra1


# Test de rachas de ascensos y descensos para la Muestra2

Tabla2=matrix(nrow=100,ncol=3)
Tabla2[,1]=Muestra2
colnames(Tabla2)=c("Datos2","Subidas","Marca")
colnames(Tabla2)

# Calculo las secuencias de 0's y 1's(1 siendo una subida y 0 una bajada)
# y los introduzco en la tabla
for (i in 1:99) {
	if (Muestra2[i] <= Muestra2[i+1])
		Tabla2[i,2]=1  
	else 
		Tabla2[i,2]=0 
}

# Marco con 1 cuando es el fin de racha y 0 en otro caso
for (i in 1:98) {
	if (Tabla2[i,2] != Tabla2[i+1,2])
		Tabla2[i,3]=1  
	else 
		Tabla2[i,3]=0 
}
Tabla2[99,3]=1;Tabla2[100,3]=0

# Sumo los elementos de la tercer columna para obtener el total de rachas 
Rachas2=sum(Tabla2[,3]);Rachas2

# Como la muestra es de tamaño mayor a 25, entonces se usa la "aproximacion normal"

m = (2*100 - 1) / 3;m
Rachas2 < m

# El numero de rachas es mayor a m, entonces el p-valor p2=P(Z>=zr), Z con N(0,1)

zr = (Rachas2 - 0.5 - m) / sqrt((16*100 -29) / 90);zr

# Calculo el p-valor usando la distribucion normal
p2= 1 - pnorm(zr);p2

# Tomando el error de tipoI errI=0.10
errI=0.1
p2>errI
# El p-valor es mayor que el errI, entonces no se rechaza la hipotesis H0
# para la Muestra2

# En Conclusion las dos muestras no rechazan que sean independientes
# identicamente distribuidas segun el test de rachas de ascensos y descensos


################################################################################

#Test de correlacion de rangos de Spearman para la muestra1
#La hipotesis H0 corresponde a que la muestra es iid y H1 que no lo es

Tabla1=matrix(nrow=100,ncol=3)
Tabla1[,1]=Muestra1
Tabla1[,2]=sort(Muestra1)
colnames(Tabla1)=c("Datos","Datos Ordenados","Cuadrados")
colnames(Tabla1)
edit(Tabla1)

#Calculo el cuadrado del rango menos el indice
for (i in 1:100) {
	  for (j in 1:100) {
              if (Tabla1[i,1] == Tabla1[j,2])
                        Tabla1[i,3]=(j-i)^2
        }
  }

#Calculo Rs
x=sum(Tabla1[,3]);x
Rs=1-((6*x)/(100*(100^2-1)));Rs

# Como la cantidad de datos en la muestra es mayor que 30 y Rs menor a 0 entonces 
# el p-valor es Pv=P(Z<=Zl), Z con N(0,1) y Zl=Rs*Raiz(n-1)
Zl=Rs*(sqrt(100-1));Zl
Pv=pnorm(Zl);Pv
a=0.1
a<Pv
#Entonces como el p-valor es mayor a 0.1 No Rechazo H0 para la Muestra1


#Test de correlacion de rangos de Spearman para la muestra2
Tabla2=matrix(nrow=100,ncol=3)
Tabla2[,1]=Muestra2
Tabla2[,2]=sort(Muestra2)
colnames(Tabla2)=c("Datos","Datos Ordenados","Cuadrados")
colnames(Tabla2)
edit(Tabla2)

#Calculo el cuadrado del rango menos el indice
for (i in 1:100) {
	  for (j in 1:100) {
              if (Tabla2[i,1] == Tabla2[j,2])
                        Tabla2[i,3]=(j-i)^2
        }
  }

#Calculo Rs
x=sum(Tabla2[,3]);x 
Rs=1-((6*x)/(100*(100^2-1)));Rs

# Como la cantidad de datos en la muestra es mayor que 30 y Rs mayor a 0 entonces
# el p-valor es Pv=P(Z>=Zr), Z con N(0,1) y Zr=Rs*Raiz(n-1)
Zr=Rs*(sqrt(100-1));Zr
Pv=1-pnorm(Zr);Pv
a=0.1
a<Pv
#Entonces como P-valor es mayor a 0.1 entonces No rechazo Ho
#Concluimos que las dos muestras son iid ya que el test de Spearman 
#y el de rachas dieron que no se rechazaba la hipotesis nula
####################################################################
