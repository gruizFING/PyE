# Cargo los datos

datos=read.table("grupo11.txt")
Muestra1=datos[,1];Muestra2=datos[,2]

# Test de KS para ver si tienen misma distribucion #
# Ho es que las muestras tienen igual distribucion y H1 que no #

ks.test(Muestra1,Muestra2)

# El p-valor da 0.01581 por lo tanto se rechaza Ho # 

# Comparaciones de funciones de distribucion empiricas graficamente #

par(mfrow=c(1,2))
plot(ecdf(Muestra1), do.points=FALSE, verticals=TRUE, xlim=range(Muestra1,Muestra2))
plot(ecdf(Muestra2), do.points=FALSE, verticals=TRUE)

# Observando las graficas se nota que no son identicas #
# La primera tiene saltos mas grandes en los primeros datos #