# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP08")

# Cargar paquetes
library(tidyverse)
library(ggpubr)
library(ez)
library(nlme)
library(emmeans)

# Cargar datos
datos <- read.csv2("EP08 Datos.csv", stringsAsFactors = TRUE)

################################################################################
# En este momento, los investigadores buscan determinar si existen diferencias
# en el tiempo que tardan los usuarios en formular consultas para problemas con
# diferente nivel de dificultad en el área de matemáticas.
################################################################################

# Puesto que en este caso se requiere inferir acerca de las medias de más de dos
# muestras correlacionadas, se requiere usar un procedimiento ANOVA con las
# siguientes hipótesis:
# H0: El tiempo requerido para formular la consulta asociada a un problema de
#     información en el área de matemáticas es el mismo para todos los niveles
#     de dificultad de los problemas (bajo, medio, alto).
# HA: El tiempo requerido para formular la consulta asociada a un problema de
#     información en el área de matemáticas es diferente para al menos uno de 
#     los niveles de dificultad de los problemas (bajo, medio, alto).

# Seleccionar datos de interés
datos <- datos %>% filter(area == "Matemáticas")
datos <- droplevels(datos)
datos[["id"]] <- factor(datos[["id"]])
datos <- datos %>% select(id, dificultad, tiempo)

# Verificación de condiciones
# Puesto que la variable dependiente corresponde al tiempo, éste se mide en una
# escala continua de intervalos iguales.

# El enunciado indica que las observaciones al interior de cada grupo son
# independientes entre sí, pues provienen de individuos diferentes asignados
# a este grupo en forma aleatoria.

# Comprobción de normalidad
g <- ggqqplot(datos,
              x = "tiempo",
              y = "dificultad",
              color = "dificultad")

g <- g + facet_wrap(~ dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# El gráfico generado muestra que la distribución de los datos de cada una de
# las muestras puede considerarse cercana a la normal pues, si bien no forman
# una recta, todos se encuentran dentro de la región aceptable del gráfico Q-Q.
# Sin embargo, en el caso del nivel de dificultad alto, se observan valores muy
# al límite de la región aceptable.

# En cuanto a la condición de homocedasticidad, se posterga su discusión hasta
# ver el resultado de la prueba de Mauchly efectuada por ezAnova.

# Procedimiento ANOVA
# Puesto que algunos datos están muy cercanos al límite aceptable para la
# condición de normalidad, fijamos un nivel de significación más exigente.
alfa <- 0.025

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
omnibus <- ezANOVA(
  data = datos,
  dv = tiempo,
  within = dificultad,
  wid = id,
  return_aov = TRUE)

print(omnibus)
print(summary(omnibus[["aov"]]))

# La prueba de esfericidad de Mauchly entrega un valor p de 0,2184, bastante
# mayor que el nivel de significación. Así, se falla al rechazar la hipótesis
# nula de esta prueba, por lo que tenemos 97,5% de confianza en que se cumple la
# condición de esfericidad.

# El procedimiento ANOVA, por otra parte, entrega un valor p muy cercano a 0,
# menor que el nivel de significación considerado. En consecuencia, rechazamos
# la hipótesis nula en favor de la hipótesis alternativa. Así, la conclusión del
# procedimiento ómnibus, con una confianza de 97,5%, es que, para al menos uno
# de los tres niveles de dificultad (bajo, medio, alto), los usuarios requieren
# de una cantidad de tiempo diferente para formular las consultas asociadas a un
# problema de información en el área de matemáticas.

# Puesto que el procedimiento ómnibus encuentra diferencias estadísticamente
# significativas, es necesario realizar un procedimiento post-hoc. Puesto que no
# requerimos hacer contrastes adicionales, usaremos la prueba HSD de Tukey, más
# poderosa que los factores de corrección de Bonferroni y Holm.
mixto <- lme(tiempo ~ dificultad , data = datos , random = ~1 | id)
medias <- emmeans (mixto , "dificultad")
post.hoc <- pairs (medias , adjust = "tukey")
print(post.hoc)

# El resultado del procedimiento arroja valores p muy inferiores al nivel de
# significación para todos los parespor lo que podemos concluir con 97,55% de
# confianza que los usuarios requieren, en el área de matemáticas, de una
# cantidad de tiempo diferente para formular la consulta asociada a un
# problema de información según el nivel de dificultad de este último.

# Graficar el tamaño del efecto
efecto <- ezPlot(data = datos, dv = tiempo, wid = id, within = dificultad,
                 y_lab = " Tiempo requerido para formular consulta [s]",
                 x = dificultad)

print(efecto)

# El gráfico del tamaño del efecto nos permite comprobar que, en el área de
# matemáticas, el tiempo requerido por los usuarios para formular la consulta
# asociada a un problema de información aumenta cuando también lo hace el nivel
# de dificultad del problema.
