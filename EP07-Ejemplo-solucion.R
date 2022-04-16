
################################################################################
# Una de las primeras preguntas a responder por el último estudio nacional de
# obesidad infantil fue si existían diferencias en la prevalencia de la obesidad
# entre niños y niñas o si, por el contrario, el porcentaje de obesos no varía
# entre sexos. Se les solicita responder esta pregunta, contando con las
# primeras observaciones obtenidas en el estudio sobre una muestra de 14
# menores:

#       Obesidad
# Sexo	 Sí	No
# Niña 	  1	 4
# Niño 	  7	 2
################################################################################

cat("\nPregunta 1\n\n")

# En este caso se tienen celdas de la tabla con menos de 5 observaciones, por lo
# que hay que usar una prueba para muestras pequeñas.

# Como los datos son independientes (no pareados), corresponde usar la prueba
# exacta de Fisher, con las siguientes hipótesis:
# H0: La obesidad infantil es independiente del sexo.
# HA: La obesidad infantil depende del sexo.

# Ahora podemos aplicar la prueba exacta de Fisher.
#Cargamos primero los datos
Niña <- c(1, 4)
Niño <- c(7, 2)
tabla.1 <- rbind(Niño, Niña)
colnames(tabla.1) <- c("Sí", "No")
print(tabla.1)

# Establecemos un nivel de significación.
alfa.1 <- 0.05

# Efectuamos la prueba exacta de Fisher puede hacerse más fácilmente.

prueba.1 <- fisher.test(tabla.1, conf.level = 1 - alfa.1)
cat("\nResultado de la prueba exacta de Fisher:\n")
print(prueba.1)


# El valor p obtenido, 0,09091, es mayor que el nivel de significación de
# 0,05, por lo que fallamos al rechazamos la hipótesis nula. Así, podemos
# concluir, con 95% de confianza, que la proporción que la obesidad infantil no
# depende del sexo de los niños.

cat("=======================================================================\n")



################################################################################
# En un artículo de García y colaboradores (2010) se describe un estudio en que
# se compararon diferentes versiones de algoritmos evolutivos para resolver
# variadas instancias de problemas de clasificación tomadas desde el repositorio
# UCI Machine Learning. Suponga que la siguiente tabla muestra los resultados de
# la clasificación hecha por dos versiones de un algoritmo genético evaluado en
# el estudio para el problema Breast Cancer. ¿Consigue uno de los algoritmos
# mejor desempeño?
#
#    AG v1	     AG v2
# Incorrecta	  Correcta
#   Correcta	  Correcta
# Incorrecta	  Correcta
#   Correcta	  Correcta
# Incorrecta	Incorrecta
# Incorrecta	  Correcta
#   Correcta	  Correcta
#   Correcta	Incorrecta
#   Correcta	Incorrecta
# Incorrecta	  Correcta
# Incorrecta	  Correcta
# Incorrecta	  Correcta
################################################################################

cat("\nPregunta 2\n\n")

# Primero construimos el data frame.
ag_v1 <- c("Incorrecta",	"Correcta",	"Incorrecta",	"Correcta",	"Incorrecta",
           "Incorrecta",	"Correcta",	"Correcta",	"Correcta",	"Incorrecta",
           "Incorrecta",	"Incorrecta")

ag_v2 <-	c("Correcta", "Correcta", "Correcta", "Correcta", "Incorrecta",
           "Correcta", "Correcta", "Incorrecta", "Incorrecta", "Correcta",
           "Correcta", "Correcta")

tabla.2 <- table(data.frame(ag_v1, ag_v2))
print(tabla.2)

# Como tenemos celdas con menos de 5 observaciones, se debe usar una prueba
# para muestras pequeñas. Además, sabemos que en este caso las observaciones
# están pareadas, por lo que corresponde usar la prueba de McNemar con las
# H0: Los algoritmos tienen igual desempeño.
# HA: Los algoritmos tienen distinto desempeño.

# Establecemos un nivel de significación.
alfa.2 <- 0.05

# En R, también existe una función para la prueba de McNemar.
cat("\nResultado de la prueba de McNemar:\n")
prueba.2 <- mcnemar.test(tabla.2)
print(prueba.2)

# El valor p obtenido, 0,2888, es mayor que el nivel de significación de
# 0,05, por lo que fallamos al rechazamos la hipótesis nula. Así, podemos
# concluir, con 95% de confianza, que no hay diferencia en el desempeño de los
# algoritmos.

cat("=======================================================================\n")



################################################################################
# Una investigación monitoreó a más de 50 mil mujeres adultas durante 10 años
# (Lucas et al., 2011. Coffee, Caffeine, and Risk of Depression Among Women.
# Archives of Internal Medicine, 171(17), 1571–1578) con la intención de
# identificar factores de riesgo de desarrollar un cuadro de depresión. Entre
# otras cosas, se registró el consumo de cafeína, cuyos datos se resumen en la
# siguiente tabla. ¿Existe alguna asociación entre la cantidad de café que se
# bebe y la depresión?
#
#             <= 1 taza   2-6 tazas   1 taza   2-3 tazas   >= 4 tazas
# Depresión   semanal     semanales   al día   al día      al día        Total
#        Sí         670         373      905         564           95    2.512
#        No      11.545       6.244   16.329      11.726        2.288   45.844
#     Total      12.215       6.617   17.234      12.290        2.383   48.356
################################################################################

cat("\nPregunta 3 (versión 1)\n\n")

# En este caso tenemos una tabla de contingencia de dos vías y se busca
# determinar si existe relación entre dos variables, por lo que una buena opción
# es usar una prueba chi-cuadrado de independencia. Las hipótesis a contrastar
# son:
# H0: La incidencia de un cuadro depresivo en mujeres adultas no está asociada
#     a la cantidad de café que estas consumen.
# HA: La cantidad de café consumido por una mujer adulta influye en su
#     posibilidad de desarrollar un cuadro depresivo.

# Comencemos por ingresar los datos.
con.depresion <- c(670, 373, 905, 564, 95)
sin.depresion <- c(11545, 6244, 16329, 11726, 2288)
tabla.3.1.obs <- rbind(con.depresion, sin.depresion)
cat("Frecuencias observadas:\n")
print(tabla.3.1.obs)

# Verifiquemos ahora las condiciones.
# Puesto que en cada categoría son mujeres diferentes, y la muestra representa
# menos del 10% de la población de mujeres que beben café, podemos asumir que
# las observaciones son independientes entre sí.

# Ahora debemos comprobar cuántas observaciones se esperan en cada grupo.
margen.fila <- apply(tabla.3.1.obs, 1, sum)
margen.columna <- apply(tabla.3.1.obs, 2, sum)
n.3.1 <- sum(tabla.3.1.obs)
tabla.3.1.esp <- margen.fila %*% t(margen.columna) / n.3.1
cat("\nFrecuencias esperadas:\n")
print(tabla.3.1.esp)

# Puesto que en cada caso se esperan más de 5 observaciones, podemos proceder
# sin problemas con la prueba seleccionada. Consideremos un nivel de
# significación de 0,05.
prueba.3.1 <- chisq.test(x = tabla.3.1.obs)
cat("\nResultado de la prueba chi-cuadrado de independencia:\n")
print(prueba.3.1)

# El valor p obtenido es significativamente menor que el nivel de significación
# fijado para la prueba, por lo que rechazamos la hipótesis nula en favor de la
# hipótesis alternativa. Concluimos entonces, con 95% de confianza, que la
# cantidad de café ingerida por una mujer influye en el desarrollo de un cuadro
# depresivo.

cat("=======================================================================\n")



################################################################################
# Un memorista, que está trabajando con el grupo de investigación de
# Aplicaciones para la Web de nuestro Departamento, ha diseñado dos algoritmos
# de búsqueda que intentan considerar el estado de ánimo del usuario en los
# parámetros de la búsqueda. Por supuesto, necesita evaluar estas propuestas y
# para eso ha realizado un experimento computacional que mide las veces que el
# usuario necesita hacer solo una búsqueda para encontrar información relevante.
# La siguiente tabla muestra los resultados de estos experimentos, que también
# considera el algoritmo que actualmente utiliza uno de los motores de búsqueda
# más usados. ¿Existe alguna diferencia entre el rendimiento de los algoritmos
# probados?
#
#                     Actual   Nuevo 1   Nuevo 2    Total
#      Una búsqueda    3.511     1.749     1.798    7.058
# 2 o más búsquedas    1.489       751       702    2.942
#             Total    5.000     2.500     2.500   10.000
################################################################################

cat("\nPregunta 3 (versión 2)\n\n")

# Dado que tenemos una tabla de contingencia de dos vías, una alternativa
# apropiada es la prueba chi-cuadrado. En este caso contamos con varias muestras
# (una para el algoritmo actual, otra para el algoritmo nuevo 1 y una última
# para el algoritmo nuevo 2) para las cuales se midió un único factor, que
# podríamos llamar "eficiencia del algoritmo", con dos niveles:"una búsqueda" y
# "más de una búsqueda". Así, Como queremos ver si los algoritmos tienen
# desempeños similares, se trara de una prueba chi-cuadrado de homogeneidad, con
# las siguientes hipótesis a contrastar:
# H0: El desempeño de todos los algoritmos es el mismo.
# HA: Al menos un algoritmo presenta un desempeño disitnto al de los algoritmos
#     restantes.

# Comencemos por ingresar los datos.
una.búsqueda <- c(3511, 1749, 1818)
más.de.una.búsqueda <- c(1489, 751, 682)
tabla.3.2.obs <- rbind(una.búsqueda, más.de.una.búsqueda)
cat("Frecuencias observadas:\n")
print(tabla.3.2.obs)

# Verifiquemos ahora las condiciones.
# Puesto que en cada categoría son mujeres diferentes, y la muestra representa
# menos del 10% de la población de mujeres que beben café, podemos asumir que
# las observaciones son independientes entre sí.

# Ahora debemos comprobar cuántas observaciones se esperan en cada grupo.
margen.fila <- apply(tabla.3.2.obs, 1, sum)
margen.columna <- apply(tabla.3.2.obs, 2, sum)
n.3.2 <- sum(tabla.3.2.obs)
tabla.3.2.esp <- margen.fila %*% t(margen.columna) / n.3.2
cat("\nFrecuencias esperadas:\n")
print(tabla.3.2.esp)

# Puesto que en cada caso se esperan más de 5 observaciones, podemos proceder
# sin problemas con la prueba seleccionada. Consideremos un nivel de
# significación de 0,05.
prueba.3.2 <- chisq.test(x = tabla.3.2.obs)
cat("\nResultado de la prueba chi-cuadrado de homogeneidad:\n")
print(prueba.3.2)

# El valor p obtenido es menor que el nivel de significación fijado para la
# prueba, por lo que rechazamos la hipótesis nula en favor de la hipótesis
# alternativa. Concluimos entonces, con 95% de confianza, que a lo menos uno
# de los algoritmos de búsqueda presenta un desempeño diferente al de los demás.

cat("=======================================================================\n")



################################################################################
# Estudios sobre las creencias de los estadounidenses acerca del origen y
# desarrollo de los seres humanos se llevan haciendo desde hace décadas. En la
# última encuesta, realizada en 2010, se presentaron las siguientes opciones:
# a) Human beings have developed over millions of years from less advanced forms
#    of life, but God guided this process.
# b) Human beings have developed over millions of years from less advanced forms
#    of life, but God had no part in this process.
# c) God created human beings pretty much in their present form at one time
#    within the last 10,000 years or so.
# 1.019 personas fueron consultadas sobre cuál de las opciones anteriores
# representaba mejor su punto de vista. 387 personas se inclinaron por la opción
# 1, 171 por la opción 2, 400 por la opción 3 y 61 no supieron o no quisieron
# responder. En el año 2007, esta misma encuesta registró las siguientes
# proporciones: 38% opción 1, 14% opción 2, 43% opción 3. ¿Han cambiado las
# creencias de los estadounidenses acerca del origen y desarrollo de los seres
# humanos desde 2007?
################################################################################

cat("\nPregunta 3 (versión 3)\n\n")

# Este problema también puede enfrentarse con un procedimiento chi-cuadrado,
# que usualmente se conoce como prueba de bondad de ajuste (goodness-of-fit en
# inglés), donde se tabulan las frecuencias observadas de una variable en
# categorías y luego se comparan con las frecuencias esperadas en cada una de
# dichas categorías de acuerdo a una distribución conocida de referencia. Las
# hipótesis a contrastar son:
# H0: Los estadounidenses han mantenido sus creencias, entre 2007 y 2010, con
#     respecto al origen y desarrollo de los seres humanos.
# HA: Los estadounidenses han modificado sus creencias, entre 2007 y 2010, con
#     respecto al origen y desarrollo de los seres humanos.

# En este caso, tenemos frecuencias observadas el 2010 y conocemos la
# distribución que tenía la "población" en 2007, lo que usaremos como
# referencia para la comparación.

frecuencia.2010 <- c(387, 171, 400)
proporcion.2007 <- c(38, 14, 43) / 100

# En base a estos datos, podemos calcular las frecuencias de 2007 si el tamaño
# de la muestra hubiera sido el mismo que en 2010.
n.3.3 <- sum(frecuencia.2010) + 61
frecuencia.2007 <- proporcion.2007 * n.3.3

# Así, la tabla de datos para este problema es:
tabla.3.3.obs <- rbind(frecuencia.2010, frecuencia.2007)
cat("Frecuencias observadas:\n")
print(tabla.3.3.obs)

# Ahora verificamos las condiciones.
# Puesto que se encuestó a menos del 10% de la población de Estados Unidos,
# podemos afirmar que las observaciones son independientes entre sí.

# Ahora debemos calcular la cantidad de observaciones esperadas por cada grupo.
esperadas <- proporcion.2007 * n.3.3
cat("\nFrecuencias esperadas para 2010:\n")

# Puesto que en cada caso se esperan más de 5 observaciones, es pertinente
# proceder con la prueba. Consideremos un nivel de significación de 0,05.
prueba.3.3 <- chisq.test(x = tabla.3.3.obs)
cat("\nResultado de la prueba chi-cuadrado de bondad de ajuste:\n")
print(prueba.3.3)

# El valor p obtenido es mayor que el nivel de significación fijado para la
# prueba, por lo que fallamos al rechazar la hipótesis nula. Concluimos pues,
# con 95% de confianza, que no ha habido un cambio en las creencias de los
# estadounidenses acera del origen y desrrollo de los seres humanos.

cat("=======================================================================\n")



################################################################################
# La Facultad de Ingeniería desea saber si existe diferencia significativa en el
# desempeño de los estudiantes en asignaturas críticas de primer semestre. Para
# ello, le ha entregado un archivo de datos que, para 3 asignaturas, indica si
# una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la
# Facultad? Indicación: obtenga la muestra a partir del archivo EP07 Datos.csv,
# usando la semilla 600. Considere un nivel de significación α=0,05.
################################################################################

cat("\nPregunta 4\n\n")

# En esta pregunta tenemos una variable independiente (el estudiante) que tiene
# 3 observaciones pareadas de una variable de respuesta dicotómica (si aprueba o
# reprueba cada una de las asignaturas). Una herramienta que conocemos para
# este escenario es la prueba Q de Cochran, con las siguientes hipótesis:
# H0: La tasa de aprobación es la misma para Cálculo, Álgebra y Física.
# HA: Al menos una de las asignaturas (Cálculo, Álgebra y Física) tiene una tasa
#     de aprobación distinta.

# Cargamos los datos.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP07")
datos <- read.csv2(file = "EP07 Datos.csv", stringsAsFactors = TRUE)

# Obtenemos la muestra.
library(dplyr)
set.seed(600)
muestra <- sample_n(datos, size = 50, replace = FALSE)

# Como siempre, comenzamos por verificar las condiciones.
# La variable de respuesta es dicotómica con niveles A (aprueba) y R (reprueba) y
# la variable independiente, correspondiente a la asignatura, es categórica con
# los niveles Cálculo, Álgebra y Física.
# Puesto que la muestra de estudiantes es seleccionada al azar y que el tamaño de
# la muestra (50) corresponde a menos del 10% de los estudiantes que
# históricamente han cursado la asignatura, podemos asumir que las observaciones
# son independientes entre sí.
# Por último, nuestra muestra tiene 50 observaciones y 3 niveles en la variable
# independiente, por lo que se cumple que 50 * 3 = 150 >= 24.
# En consecuencia, podemos usar la prueba Q de Cochran para este problema.


# Llevamos los datos a formato largo, según requiere la prueba.
library(tidyverse)

muestra <- muestra %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                    names_to = "Curso", values_to = "Situacion")

muestra[["Curso"]] <- factor(muestra[["Curso"]])

# Hacemos ahora la prueba Q de Cochran. Recordemos que, como esta prueba
# simplemente comprueba la igualdad de todas las proporciones, se trata de una
# prueba ómnibus.

library(RVAideMemoire)
alfa <- 0.05
prueba.4 <- cochran.qtest(Situacion ~ Curso | Id, data = muestra, alpha = alfa)
cat("Resultado de la prueba Q de Cochran\n")
print(prueba.4)

# El valor p obtenido es menor que el nivel de significación fijado para la
# prueba, por lo que rechazamos la hipótesis nula en favor de la hipótesis
# alternativa. Concluimos pues, con 95% de confianza, que a lo menos una de las
# asignaturas (Cálculo, Álgebra o Física) tiene una tasa de aprobación
# distinta a las demás.

# Como la prueba ómnibus encontró diferencias estadísticamente significativas,
# debemos ahora llevar a cabo un procedimiento post-hoc para determinar
# cuáles son esas diferencias. Puesto que tiene un mayor poder estadístico,
# usaremos la corrección de Holm.
library(rcompanion)

post.hoc <- pairwiseMcnemar(Situacion ~ Curso | Id, data = muestra,
                            method = "holm")

cat("\nResultado del procedimiento post-hoc usando la corrección de Holm")
print(post.hoc)

# El resultado obtenido nos muestra que existe una diferencia significativa
# entre Álgebra y Cálculo, aunque no se evidencia una diferencia importante
# entre ninguna de estas dos asignaturas y Física.
