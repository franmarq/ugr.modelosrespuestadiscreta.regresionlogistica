covid<-read.table("covid.txt",header=TRUE,sep=" ")
covid<-data.frame(covid)
str(covid)
names(covid)
#table(covid$Constriccion)

#AJUSTE DE REG LOGISTICA
Ajuste.covid<-glm(cbind(pcrs,pcrn)~ncontact,
                    data=covid,family = binomial)
summary(Ajuste.covid)

#probabiliaddes
fitted.values(Ajuste.covid)

#intervalo
confint.default(Ajuste.covid,level=0.99)

#al aumentar en 4 unidades cambia
#p/(1-p)

#csalacular chi cuadrado
pchisq(12.228,9)

#Indique el extremo inferior del intervalo de confianza al 95% para la probabilidad de PCR positiva de una persona con 2 contactos según el modelo
predict(Ajuste.covid,type= "response",se.fit = T)



#cambio a datos no agrupados
Datos.Orig.Explicativa<-c(rep(covid[,1],covid[,2]),
                          rep(covid[,1],covid[,3]))
Datos.Orig.Respuesta<-c(rep(seq(0,0,length=nrow(covid)),
                            covid[,2]),
                        rep(seq(1,1,length=nrow(covid)),
                            covid[,3]))
Datos.Orig<-data.frame(Datos.Orig.Explicativa,Datos.Orig.Respuesta)
names(Datos.Orig)<-c("Explicativa","Respuesta")
table(Datos.Orig$Respuesta)

#AJUSTE DE REG LOGISTICA datos no agrupados
Ajuste.covid2<-glm(Respuesta~Explicativa,
                   Datos.Orig,family = binomial)

summary(Ajuste.covid2)



#tabla de clasificaciones correctas (aplica para datos no agrupados, no es el caso, no se puede asignar el 0y 1)
Categoria.Pred2<-ifelse(fitted.values(Ajuste.covid2)>=0.39,1,0)
table(Datos.Orig$Respuesta,Categoria.Pred2)
#especificidad
Categoria.Pred2<-ifelse(fitted.values(Ajuste.covid2)>=0.59,1,0)
table(Datos.Orig$Respuesta,Categoria.Pred2)





////////////////////////////////////////////////////////////////////



#Ejercicio 2
infarto<-read.csv("infarto.csv",header=TRUE)
Ajuste.all<-glm(Infarto~Edad+Colesterol+Peso+Horas
                ,family=binomial,data=infarto)
summary(Ajuste.all)

#test de bondad Hosmer
hosmerlem.test(infarto,fitted.values(Ajuste.all),g=10,group=F)

#tasa de clasificaciones correctas (aplica para datos no agrupados, si es el caso)

#proporcion exitos en la muestra
table(infarto$Infarto)
#0.7818

Categoria.Pred<-ifelse(fitted.values(Ajuste.all)>=0.78,1,0)
table(infarto$Infarto,Categoria.Pred)

#comparacion de modelos

#MODELO GENERAL
Ajuste.all<-glm(Infarto~Edad+Colesterol+Peso+Horas
                ,family=binomial,data=infarto)
#MODELO PARTICULAR
Ajuste.all.0<-glm(Infarto~Edad+Horas
                ,family=binomial,data=infarto)
#COMPARACION
anova(Ajuste.all.0,Ajuste.all)

#csalacular chi cuadrado
pchisq(2.0623,2)
1-0.6434034


#modelo con interaccion
Ajuste.all.int<-glm(Infarto~Edad*Colesterol
                ,family=binomial,data=infarto)

#proporcion exitos en la muestra
table(infarto$Infarto)
#0.7818
Categoria.Pred.int<-ifelse(fitted.values(Ajuste.all.int)>=0.78,1,0)
table(infarto$Infarto,Categoria.Pred.int)


#inclusion stepwise
Ajuste.0<-glm(Infarto~1
              ,family=binomial,data=infarto)
summary(Ajuste.0)

Ajuste.step<-step(Ajuste.0,scope=list(lower=Infarto~1,
                                      upper=Infarto~Edad+Colesterol+Peso+Horas),
                  direction="both")

