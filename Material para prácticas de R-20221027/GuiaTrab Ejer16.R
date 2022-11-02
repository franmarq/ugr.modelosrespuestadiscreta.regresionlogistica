## Ejercicio 16 Regresion logistica multiple

getwd()
vasoc<-read.table("vasoconst.txt",header=TRUE,sep=" ")
vasoc<-data.frame(vasoc)
str(vasoc)
names(vasoc)
table(vasoc$Constriccion)


#ajuste del modelo logistico

Ajuste.vasoc<-glm(Constriccion~Volumen+Tasa,
                    data=vasoc,family = binomial)
summary(Ajuste.vasoc)

# para realizar la seleccion stepwise necesita del ajuste del modelo nulo o modelo que solo tiene el parametro independiente

Ajuste.0<-glm(Constriccion~1,
                  data=vasoc,family = binomial)
summary(Ajuste.0)

# la seleccion parte del modelo que tiene solo el parametro
# independiente y se pretende utilizar el procedimiento stepwise en ambas
# direcciones para encontrar los mejores predictores

Ajuste.step<-step(Ajuste.0,scope=list(lower=Constriccion~1,
                                      upper=Constriccion~Volumen+Tasa), direction="both")


### probabilidad normal estandar
pnorm(0.05, mean = 0, sd= 1)
qnorm(0.05, mean = 0, sd= 1)
