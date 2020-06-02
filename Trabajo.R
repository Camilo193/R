library(readxl)
#Leer datos de la entidad financiera 1
EntidadFinanciera1 <- read_excel("D:/DatosScore(MSV).xlsx",
                                 sheet = "EntidadFinanciera_1")
View(EntidadFinanciera1)

#Leer datos de la entidad financiera 2
EntidadFinanciera2 <- read_excel("D:/DatosScore(MSV).xlsx",
                                 sheet = "EntidadFinanciera_2")
View(EntidadFinanciera2)


#Sólo dejo los datos de los que tienen score
IndexCS <- which(EntidadFinanciera1$Score>0)
IndexSS <- which(is.na(EntidadFinanciera1$Score))
EntidadFinancieraCS = EntidadFinanciera1[IndexCS, ]
EntidadFinancieraSS = EntidadFinanciera1[IndexSS, ]
View(EntidadFinancieraSS)
View(EntidadFinancieraCS)



#Separo las variables cuantitativas
Xc=cbind(EntidadFinancieraCS$Edad, EntidadFinancieraCS$Hijos, EntidadFinancieraCS$Perscarg, EntidadFinancieraCS$Estrato, EntidadFinancieraCS$Ingresos, EntidadFinancieraCS$Egresos, 
         EntidadFinancieraCS$`Total activos`, EntidadFinancieraCS$`Total pasivos`, EntidadFinancieraCS$Salario, EntidadFinancieraCS$`Gasto familiar`, EntidadFinancieraCS$`Gasto arriendo`
        , EntidadFinancieraCS$`Otros gastos`, EntidadFinancieraCS$MoraCome, EntidadFinancieraCS$Score)
colnames(EntidadFinancieraCS)


#Correlación
library(PerformanceAnalytics)
#Indices de variables cuantitativas
IndexCS<-EntidadFinancieraCS[,c(2,5,6,7,10,11,14,15,16,21,22,23,24,26,33,39,40)]
chart.Correlation(IndexCS,histogram=TRUE,pch=19)
View(cor(IndexCS))#Veo las correlaciones más fuertes


#Hallar medidas de tendencia central
Index<-EntidadFinanciera1[,c(14,10,6,21,2)]
Index2<-EntidadFinanciera2[,c(14,10,6,21,2)]
View(Index)
View(Index2)
#Promedio
promedioEntidad1 = apply(Index, 2, mean)
promedioEntidad2 = apply(Index2, 2, mean)
View(promedioEntidad1)
View(promedioEntidad2)
#Mediana
medianaEntidad1 = apply(Index, 2, median)
medianaEntidad2 = apply(Index2, 2, median)
View(medianaEntidad1)
View(medianaEntidad2)
#Moda
#Función para hallar la moda.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modaEntidad1 = apply(Index, 2, getmode)
modaEntidad2 = apply(Index2, 2, getmode)
View(modaEntidad1)
View(modaEntidad2)
#Varianza
variance <- function (x)   sum((x-mean(x))^2)/(length(x)-1)
varianzaEntidad1 = apply(Index, 2, variance)
varianzaEntidad2 = apply(Index2, 2, variance)
View(varianzaEntidad1)
View(varianzaEntidad2)
#Libreria para Curtosis y Asimetria
library(moments)
#Curtosis
curtosisEntidad1 = apply(Index, 2, kurtosis)
curtosisEntidad2 = apply(Index2, 2, kurtosis)
View(curtosisEntidad1)
View(curtosisEntidad2)
#Asimetria
asimetriaEntidad1 = apply(Index, 2, skewness)
asimetriaEntidad2 = apply(Index2, 2, skewness)
View(asimetriaEntidad1)
View(asimetriaEntidad2)
#Caja de bigotes
boxplot(Index$`Total activos`, Index2$`Total activos`, horizontal=TRUE)
boxplot(Index$Ingresos, Index2$Ingresos, horizontal=TRUE)
View(boxplot(Index$Perscargo, Index2$Perscargo, horizontal=TRUE))
boxplot(Index$`Gasto familiar`, Index2$`Gasto familiar`, horizontal=TRUE)
boxplot(Index$Edad, Index2$Edad, horizontal=TRUE)
View(Index)
View(Index2)

#Datos Raros (Outliers)
ls=mean(Index$Ingresos)+2*sd(Index$Ingresos)
li=mean(Index$Ingresos)-2*sd(Index$Ingresos)
ingresosls=(Index$Ingresos)>ls
ingresosli=(Index$Ingresos)<li
View(ingresosls)
View(ingresosli)


#De Cualitativo a cuantitativo
EntidadFinancieraC1 <- EntidadFinancieraCS
EntidadFinancieraC1$`Estado civil` <- factor(EntidadFinancieraC1$`Estado civil`)
EntidadFinancieraC1$`Estado civil` <- as.numeric(EntidadFinancieraC1$`Estado civil`)
EntidadFinancieraC1$`Nivel de estudios` <- factor(EntidadFinancieraC1$`Nivel de estudios`)
EntidadFinancieraC1$`Nivel de estudios` <- as.numeric(EntidadFinancieraC1$`Nivel de estudios`)
EntidadFinancieraC1$`Tipo de contrato` <- factor(EntidadFinancieraC1$`Tipo de contrato`)
EntidadFinancieraC1$`Tipo de contrato` <- as.numeric(EntidadFinancieraC1$`Tipo de contrato`)
EntidadFinancieraC1$`Tipo de vivienda` <- factor(EntidadFinancieraC1$`Tipo de vivienda`)
EntidadFinancieraC1$`Tipo de vivienda` <- as.numeric(EntidadFinancieraC1$`Tipo de vivienda`)
EntidadFinancieraC1$`Tipo de actividad` <- factor(EntidadFinancieraC1$`Tipo de actividad`)
EntidadFinancieraC1$`Tipo de actividad` <- as.numeric(EntidadFinancieraC1$`Tipo de actividad`)
EntidadFinancieraC1$Garantia <- factor(EntidadFinancieraC1$Garantia)
EntidadFinancieraC1$Garantia <- as.numeric(EntidadFinancieraC1$Garantia)
EntidadFinancieraC1$RespPat <- factor(EntidadFinancieraC1$RespPat)
EntidadFinancieraC1$RespPat <- as.numeric(EntidadFinancieraC1$RespPat)

Index<-EntidadFinancieraC1[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,21,22,23,24,26,27,28,33,39,40)]
View(Index)
chart.Correlation(Index,histogram=TRUE,pch=19)
View(cor(Index))#Veo las correlaciones más fuertes
View(cov(Index))


#Modelo de regresión lineal multiple
#vARIABLES A TENER EN CUENTA
vn1<-Index$`Tipo de vivienda`
vn2<-Index$`Total activos`
vn3<-Index$`Estado civil`
vn4<-Index$Ingresos
vn5<-Index$Perscargo
vn6<-Index$`Gasto familiar`
vn7<-Index$Edad
vn8<-Index$`Otros gastos`
vn9<-Index$Garantia
vn10<-Index$`Tipo de contrato`
vn11<-Index$Score
#Las guardamos en un DF
datosCorrelacion = data.frame(vn1, vn2, vn3, vn4, vn5, vn6, vn7, vn8, vn9, vn10, vn11)
View(datosCorrelacion)
#Hacemos la regresión
Regresion = lm(vn11~vn1 + vn2 + vn3 + vn4 + vn5 + vn6 + vn7 + vn8 + vn9 + vn10, data=datosCorrelacion)
summary(Regresion)
#Cualitativas a cuantitavas en las variables sin score
EntidadFinancieraSS$`Estado civil` <- factor(EntidadFinancieraSS$`Estado civil`)
EntidadFinancieraSS$`Estado civil` <- as.numeric(EntidadFinancieraSS$`Estado civil`)
EntidadFinancieraSS$`Nivel de estudios` <- factor(EntidadFinancieraSS$`Nivel de estudios`)
EntidadFinancieraSS$`Nivel de estudios` <- as.numeric(EntidadFinancieraSS$`Nivel de estudios`)
EntidadFinancieraSS$`Tipo de contrato` <- factor(EntidadFinancieraSS$`Tipo de contrato`)
EntidadFinancieraSS$`Tipo de contrato` <- as.numeric(EntidadFinancieraSS$`Tipo de contrato`)
EntidadFinancieraSS$`Tipo de vivienda` <- factor(EntidadFinancieraSS$`Tipo de vivienda`)
EntidadFinancieraSS$`Tipo de vivienda` <- as.numeric(EntidadFinancieraSS$`Tipo de vivienda`)
EntidadFinancieraSS$`Tipo de actividad` <- factor(EntidadFinancieraSS$`Tipo de actividad`)
EntidadFinancieraSS$`Tipo de actividad` <- as.numeric(EntidadFinancieraSS$`Tipo de actividad`)
EntidadFinancieraSS$Garantia <- factor(EntidadFinancieraSS$Garantia)
EntidadFinancieraSS$Garantia <- as.numeric(EntidadFinancieraSS$Garantia)
EntidadFinancieraSS$RespPat <- factor(EntidadFinancieraSS$RespPat)
EntidadFinancieraSS$RespPat <- as.numeric(EntidadFinancieraSS$RespPat)
IndexSC<-EntidadFinancieraSS[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,21,22,23,24,26,27,28,33,39,40)]
#Variables sin score
vn1<-IndexSC$`Tipo de vivienda`
vn2<-IndexSC$`Total activos`
vn3<-IndexSC$`Estado civil`
vn4<-IndexSC$Ingresos
vn5<-IndexSC$Perscargo
vn6<-IndexSC$`Gasto familiar`
vn7<-IndexSC$Edad
vn8<-IndexSC$`Otros gastos`
vn9<-IndexSC$Garantia
vn10<-IndexSC$`Tipo de contrato`
vn11<-IndexSC$Score
#Guardamos las variables en un DF
datosSinCorrelacion = data.frame(vn1, vn2, vn3, vn4, vn5, vn6, vn7, vn8, vn9, vn10, vn11)
View(datosSinCorrelacion)
pronosticoScore<- predict(Regresion,datosSinCorrelacion)
View(pronosticoScore)
EntidadFinanciera1<-EntidadFinanciera1[order(EntidadFinanciera1$Score),]
View(EntidadFinanciera1)
#Le pasamos los valores pronosticados a los datos iniciales
EntidadFinanciera1$Score[38:131] <- pronosticoScore
#Organizamos los datos como estaban originalmente
EntidadFinanciera1<-EntidadFinanciera1[order(EntidadFinanciera1$k),]
View(EntidadFinanciera1)


#Pronostico Score entidad financiera 2
EntidadFinanciera2F <- EntidadFinanciera2
View(EntidadFinanciera2F)

EntidadFinanciera2F$`Estado civil` <- factor(EntidadFinanciera2F$`Estado civil`)
EntidadFinanciera2F$`Estado civil` <- as.numeric(EntidadFinanciera2F$`Estado civil`)
EntidadFinanciera2F$`Nivel de estudios` <- factor(EntidadFinanciera2F$`Nivel de estudios`)
EntidadFinanciera2F$`Nivel de estudios` <- as.numeric(EntidadFinanciera2F$`Nivel de estudios`)
EntidadFinanciera2F$`Tipo de contrato` <- factor(EntidadFinanciera2F$`Tipo de contrato`)
EntidadFinanciera2F$`Tipo de contrato` <- as.numeric(EntidadFinanciera2F$`Tipo de contrato`)
EntidadFinanciera2F$`Tipo de vivienda` <- factor(EntidadFinanciera2F$`Tipo de vivienda`)
EntidadFinanciera2F$`Tipo de vivienda` <- as.numeric(EntidadFinanciera2F$`Tipo de vivienda`)
EntidadFinanciera2F$`Tipo de actividad` <- factor(EntidadFinanciera2F$`Tipo de actividad`)
EntidadFinanciera2F$`Tipo de actividad` <- as.numeric(EntidadFinanciera2F$`Tipo de actividad`)
EntidadFinanciera2F$Garantia <- factor(EntidadFinanciera2F$Garantia)
EntidadFinanciera2F$Garantia <- as.numeric(EntidadFinanciera2F$Garantia)
EntidadFinanciera2F$RespPat <- factor(EntidadFinanciera2F$RespPat)
EntidadFinanciera2F$RespPat <- as.numeric(EntidadFinanciera2F$RespPat)
#Agrego nueva columna para el score
Score=vector(mode='numeric', length=length(EntidadFinanciera2F$k))
EntidadFinanciera2F = data.frame(EntidadFinanciera2F, Score)
View(EntidadFinanciera2F)

Index2<-EntidadFinanciera2F[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,21,22,23,24,26,27,28,33,38)]

vn1<-Index2$Tipo.de.vivienda
vn2<-Index2$Total.activos
vn3<-Index2$Estado.civil
vn4<-Index2$Ingresos
vn5<-Index2$Perscargo
vn6<-Index2$Gasto.familiar
vn7<-Index2$Edad
vn8<-Index2$Otros.gastos
vn9<-Index2$Garantia
vn10<-Index2$Tipo.de.contrato
vn11<-Index2$Score
#Las guardamos en un DF
datosEntidad2 = data.frame(vn1, vn2, vn3, vn4, vn5, vn6,vn7, vn8, vn9, vn10, vn11)
View(datosEntidad2)
#Pronosticamos
pronosticoScore2 <- predict(Regresion,datosEntidad2)
View(pronosticoScore2)

#Creamos una nueva columna para el score en la tabla original
Score=vector(mode='numeric', length=length(EntidadFinanciera2$k))
EntidadFinanciera2 = data.frame(EntidadFinanciera2, Score)
View(EntidadFinanciera2)
#Le pasamos el pronostico a la tabla original
EntidadFinanciera2$Score[1:70] <- pronosticoScore2
View(EntidadFinanciera2)

rattle(EntidadFinanciera2)
data()
EntidadFinanciera2Score <- EntidadFinanciera2
View(EntidadFinanciera2Score)


