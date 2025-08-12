#invocar las librerias necesarias
library(hemp)
library(psych)
library(haven)
library(tidyverse)
library(labelled)
library(qgraph)
library(EGAnet)
library(sna)
install.packages("lavaan")
library(lavaan)
install.packages("semPlot")
library(semPlot)

#base de datos cruda
politica<-read.csv2("C:/Users/santi/Desktop/Postgrado - Segundo Semestre/Medición en Ciencias Sociales/Proyecto final/J6_data.csv", header= TRUE, sep=";")

#seleccion de items 
politicarn <- politica |> 
  dplyr::select(p69_292_A:p69_292_O) |> 
  dplyr::rename(pol94=p69_292_A,
                pol95=p69_292_B,
                pol96=p69_292_C,
                pol97=p69_292_D,
                pol98=p69_292_E,
                pol99=p69_292_F,
                pol100=p69_292_G,
                pol101=p69_292_H,
                pol102=p69_292_I,
                pol103=p69_292_J,
                pol104=p69_292_K,
                pol105=p69_292_L,
                pol106=p69_292_M,
                pol107=p69_292_N,
                pol108=p69_292_O)

#agregación de etiquetas a ítems
labelled::var_label(politicarn$pol94)<-"No estoy seguro de creer en la mayoría de los políticos"
labelled::var_label(politicarn$pol95)<-"Por lo general soy cuidadoso acerca de confiar en los políticos"
labelled::var_label(politicarn$pol96)<-"En general, los políticos son abiertos sobre sus decisiones"
labelled::var_label(politicarn$pol97)<-"En general, el gobierno suele hacer lo correcto"
labelled::var_label(politicarn$pol98)<-"La información proporcionada por el gobierno es generalmente poco confiable"
labelled::var_label(politicarn$pol99)<-"Lo mejor es ser cuidadoso acerca de confiar en el gobierno"
labelled::var_label(politicarn$pol100)<-"La mayoría de los políticos son honestos y veraces"
labelled::var_label(politicarn$pol101)<-"La gente en el gobierno con frecuencia muestra mal juicio"
labelled::var_label(politicarn$pol102)<-"Los políticos frecuentemente son incompetentes e ineficaces"
labelled::var_label(politicarn$pol103)<-"Los políticos no respetan a gente como yo"
labelled::var_label(politicarn$pol104)<-"Los políticos a menudo ponen al país por encima de sus intereses personales"
labelled::var_label(politicarn$pol105)<-"Los políticos suelen ignorar a mi comunidad"
labelled::var_label(politicarn$pol106)<-"El gobierno actúa injustamente con gente como yo"
labelled::var_label(politicarn$pol107)<-"El gobierno entiende las necesidades de mi comunidad"
labelled::var_label(politicarn$pol108)<-"El gobierno frecuentemente tiene buenas intenciones"


#conversión de las variables a factores
polfact<- labelled::to_factor(politicarn)
head(politicarn,3)

#descripción completa de variables 
psych::describe(politicarn)

#descripción simple de variables 
summary(polfact)

#porcentajes de las respuestas de los ítems
count(polfact,pol94) |>  mutate(ppol94=n/sum(n)*100)
count(polfact,pol95) |>  mutate(ppol95=n/sum(n)*100)
count(polfact,pol96) |>  mutate(ppol96=n/sum(n)*100)
count(polfact,pol97) |>  mutate(ppol97=n/sum(n)*100)
count(polfact,pol98) |>  mutate(ppol98=n/sum(n)*100)
count(polfact,pol99) |>  mutate(ppol99=n/sum(n)*100)
count(polfact,pol100) |>  mutate(ppol100=n/sum(n)*100)
count(polfact,pol101) |>  mutate(ppol101=n/sum(n)*100)
count(polfact,pol102) |>  mutate(ppol102=n/sum(n)*100)
count(polfact,pol103) |>  mutate(ppol103=n/sum(n)*100)
count(polfact,pol104) |>  mutate(ppol104=n/sum(n)*100)
count(polfact,pol105) |>  mutate(ppol105=n/sum(n)*100)
count(polfact,pol106) |>  mutate(ppol106=n/sum(n)*100)
count(polfact,pol107) |>  mutate(ppol107=n/sum(n)*100)
count(polfact,pol108) |>  mutate(ppol108=n/sum(n)*100)

#detección de valores atipicos
bollen_plot(politicarn, crit.value = .06)
#conclusión: el registro 824 será eliminado por ser atípico a partir de lo
#sugerido por el bollen plot

# Linealidad
psych::pairs.panels(politicarn,
                    method = "spearman",
                    density = TRUE,
                    ellipses = FALSE,
                    show.points = FALSE)
# Con 15 ítems resulta imposible leer la magnitud de las correlaciones
# Conclusión: (1) En general parece sustentarse la existencia de una relación 
# lineal entre parejas de los indicadores lo que se supone que se trasmitiría 
# a la relación entre los ítems y los factores. (2) En general las 
# distribuciones de los ítems muestran asimetría negativa.

# Normalidad univariada
# A sabiendas que con cinco opciones de respuesta no tiene mucho sentido la
# verificación de la normalidad univariada de los datos llevaremos a cabo la
# verificación de los valores de asimetria y curtosis reproduciendo los 
# resultados descriptivos obtenidos previamente.
psych::describe(politicarn)
# Conclusiones> (1) Todos los valores del coeficiente de asimetría están por
# debajo de 2 y los curtósis por debajo de 7 que son los criterios de
# de referencia sugeridos por Curran et al (1996).

# Normalidad multivariada
psych::mardia(politicarn, na.rm = TRUE, plot = TRUE)
# Conclusiones: (1) El valor de la curtósis es de 71.27 muy superior al valor 
# limite de 5 para de una distribución normal multivariada. (2) la concavidad del
# gráfico Q-Q1 con relación a la recta de normalidad indica asimetría negativa
# ratificando lo que ya habíamos detectado con los gráficos de barras de los
# indicadores.

# Verificaré si la presencia del valor identificado como atipicos
# pueden ejercer una influencia sobre las correlaciones policóricas necesarias 
# para el FA. Calcularé las matrices de correlaciones con y sin este valor y su diferencia.
poly_pol <- psych::polychoric(politicarn)
round(poly_pol$rho, 2)
poly_pol2 <- psych::polychoric(politicarn[-c(75,1138,616,805,824),])
round(poly_pol2$rho, 2)
poly_dif <- poly_pol$rho - poly_pol2$rho
round(poly_dif, 2)

# Conclusión: Las diferencias son muy pequeñas por lo que el impacto del
# valor atipico sobre las correlaciones también es muy reducido.

# Tambien se puede emplear la funcion "outlier" para la deteccionn de valores
# atipicos multivariante que calcula la distancia de Mahalanobis desde cada dato
# multivariado al centroide de los datos y la compara contra el valor esperado
# de una Ji-cuadrado. El resultado se presenta en un gráfico Q-Q y los valores
# identificados como atípicos se guardan en el vector d2.
d2 <- psych::outlier(politicarn, cex=.8)
d22 <- data.frame(politicarn, d2)

# Conclusiones: Se identificaron cinco datos atípicos: 75,1138,616,805,824.

# Es apropiado realizar el análisis factorial?

# Evaluación de la matriz de correlacion policórica ≥ .30
qgraph::qgraph(poly_pol$rho, cut = .30, details = TRUE,
               posCol = "darkgreen", negCol = "red", 
               labels = names(politicarn))

# Prueba de esfericidad de Bartlett
psych::cortest.bartlett(poly_pol$rho, 1190)
# Conclusión: Se rechaza la hipótesis nula de que la matriz de correlaciones 
# es la matriz de identidad

# Prueba KMO
KMO(poly_pol$rho)
# Conclusión: El indice KMO global es de magnitud 0.92 indicativo de adecuación
# muestral y de manera similar podemos concluir para cada ítem individual pues
# todos los indices KMO son mayores o iguales a 0.80.

## Número de factores a retener

# Gráfico de sedimentación
psych::scree(politicarn,
             pc = TRUE,
             fa = FALSE,
             hline = -1,
             main = "Gráfico de sedimentación")

# Análisis paralelo
paral1 <-psych::fa.parallel(politicarn, fm ="minres", fa = "both", 
                            nfactors = 1, 
                            main = "Análisis paralelo:Gráfico de Sedimentación",
                            sim=FALSE, n.iter = 20, ylabel = "Autovalores",
                            cor = "poly")
paral1$values
#conclusion: El análisis paralelo sugiere 4 factores y 2 componentes

# Minimum average partial (MAP)
psych::VSS(politicarn, rotate = "promax", fm = "minres", plot = TRUE,
           n.obs = 1190, cor = "poly")

#conclusiones:
#VSS complexity 1 achieves a maximimum of 0.85  with  1  factors
#VSS complexity 2 achieves a maximimum of 0.77  with  2  factors
#The Velicer MAP achieves a minimum of 0.02  with  2  factors 
#BIC achieves a minimum of  -110.74  with  7  factors
#Sample Size adjusted BIC achieves a minimum of  -44.04  with  7  factors

# Gráfico de redes
EGAnet::EGA(poly_pol$rho, n = 1190, plot.EGA = TRUE, 
            algorithm.args = list(steps = 10))
#A partir de las agrupaciones que se forman en el gráfico de redes con las correlaciones
#policoricas se podría también sugerir la existencia de 2 factores


# EFA de 4 factores
cuatro_factores <- psych::fa(poly_pol$rho, nfactors = 4,	n.obs = 1190,
                           fm = "minres",	rotate = "promax",residuals = TRUE,
                           SMC = TRUE
)		

print(cuatro_factores, sort = TRUE, cut = 0.3, digits = 2)			

#EFA de 3 factores
tres_factores <- psych::fa(poly_pol$rho, nfactors = 3,	n.obs = 1190,
                             fm = "minres",	rotate = "promax",residuals = TRUE,
                             SMC = TRUE
)		
print(tres_factores, sort = TRUE, cut = 0.3, digits = 2)	

#EFA de 2 factores
dos_factores <- psych::fa(poly_pol$rho, nfactors = 2,	n.obs = 1190,
                           fm = "minres",	rotate = "promax",residuals = TRUE,
                           SMC = TRUE
)		
print(dos_factores, sort = TRUE, cut = 0.3, digits = 2)



#análisis factorial confirmatorio con los dos factores resultantes

#creación del modelo
act_mod <- '
          pos =~ pol108+pol97+pol100+pol107+pol96
          neg =~ pol102+pol103+pol105+pol101+pol95+pol106+pol99+pol94'

#calculo del modelo
AFC <- cfa(act_mod, orthogonal=FALSE,data=politicarn, estimator="ULS")

#creación del diagrama de ruta
semPaths(AFC, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.70,edge.color = "black", label.prop=2, sizeLat = 10,"std",layout= "tree", exoVar=FALSE)

#resumen del modelo
summary(AFC,fit.measures=TRUE,standardized=TRUE)

#indices de ajuste del modelo
fitmeasures(AFC,fit.measures = c("chisq","df","rmsea","tli","cfi","nfi","gfi"))

format(pchisq(551.347,89,lower.tail = FALSE),scientific=FALSE)
