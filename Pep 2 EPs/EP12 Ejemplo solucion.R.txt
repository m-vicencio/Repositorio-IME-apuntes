library(ggpubr)
library(WRS2)

# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP11")

################################################################################
# 1. En el trabajo de título de un estudiante del DIINF se reportan los
# siguientes tiempos de ejecución (en milisegundos) medidos para dos versiones
# de un algoritmo genético para resolver instancias del problema del vendedor
# viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más
# rápido que el otro?
#
# Instancia.A	Tiempo.A	Instancia.B	Tiempo.B
#          80	  337977	        124	  335566
#         194	  303634	          5	   52696
#         158	   33349	        110	 3192222
#         118	  243679	         57	  393213
#         179	 3453176	         87	  162808
#          60	  398653          	2	 8765321
#         133	  876432	         67	   76857
#         109	 3073534        	114	  231254
#         113	  112326           86	  854213
#         181	   55026          106	  543215
################################################################################

cat("PREGUNTA 1\n")

# Cargar datos.
Tiempo.A <- c(337977, 303634, 33349, 243679, 3453176, 398653, 876432, 3073534,
              112326, 55026)

Tiempo.B <- c(335566, 52696, 3192222, 393213, 162808, 8765321, 76857, 231254,
              854213, 543215)

Algoritmo <- factor(c(rep("A", length(Tiempo.A)), rep("B", length(Tiempo.B))))
Tiempo <- c(Tiempo.A, Tiempo.B)
datos.1 <- data.frame(Algoritmo, Tiempo)

# Construimos el histograma de los datos.
g1 <- gghistogram(datos.1, x = "Tiempo", xlab = "Algoritmo", color = "Algoritmo",
                  fill = "Algoritmo", bins = 5)

g1 <- g1 + facet_grid(~ Algoritmo)
print(g1)

# Podemos ver que, claramente, el comportamiento no es el de una distribución
# normal. En general, los tiempos suelen mostrar una distribución exponencial.
# En la lectura sobre transformaciones de datos, aprendimos que una opción
# es usar el logaritmo de los valores de tiempo en ms.
Log.tiempo <- log(datos.1[["Tiempo"]])
datos.1 <- cbind(datos.1, Log.tiempo)

# Veamos ahora el histograma de los datos transformados.
g2 <- gghistogram(datos.1, x = "Log.tiempo", xlab = "Algoritmo",
                  color = "Algoritmo", fill = "Algoritmo", bins = 5)

g2 <- g2 + facet_grid(~ Algoritmo)
print(g2)

# Ahora el comportamiento de los datos parece estar más cercano a una distribución
# normal. Verifiquemos con una prueba de normalidad.
normalidad <- by(data = datos.1[["Log.tiempo"]], INDICES = datos.1[["Algoritmo"]],
                 FUN = shapiro.test)

cat("\nPruebas de normalidad para log(Tiempo) de cada algoritmo\n")
print(normalidad)

# Vemos que con esta transformación obtenemos datos con distribuciones
# aproximadamente normales. Podemos entonces continuar con una prueba
# paramétrica. Como en este caso queremos inferir acerca de la media de dos
# muestras independientes, es adecuado usar una prueba t de Student.

prueba.1 <- t.test(Log.tiempo ~ Algoritmo, data = datos.1)
cat("Prueba t de Student para log(Tiempo) de cada algoritmo:\n")
print(prueba.1)

# El resultado de la prueba muestra que no hay sufieciente evidencia para
# considerar que existe una diferencia significativa entre las medias
# geométricas de los los tiempos de ejecución requeridos por los algoritmos.
# Sí debemos recordar que es necesario aplicar la transformación inversa para
# reportar el resultado en la escala original de la variable de interés,
# en este caso, el tiempo. Pero debe recordarse que la diferencia de logs de
# tiempos, corresponde a la razón de medias geométricas de los tiempos.

conf.int <- round(exp(prueba.1[["conf.int"]]), 2)
cat("Intervalo con 95% confianza transformado de vuelta: ")
cat("[", conf.int[1], ", ", conf.int[2], "]", "\n", sep = "")

# Como el 1 está incluido dentro del intervalo de confianza,
# no podemos descartar que las medias geométricas del tiempo
# requerido por cada algoritmo son iguales.



################################################################################
# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.
################################################################################

cat("\n\n\nPREGUNTA 2\n")

# Cargar datos
datos <- read.csv2("EP11 Datos.csv", stringsAsFactors = TRUE)

# En este caso, consideraremos la pregunta: ¿Es igual el ingreso per cápita
# entre hombres y mujeres?
cat("¿Es igual el ingreso per cápita entre hombres y mujeres?\n\n")

# Filtrar datos y obtener muestra.
set.seed(348)
datos.2 <- sample_n(datos, 350)
datos.2 <- datos.2 %>% select(ytotcorh, numper, sexo)
datos.2 <- datos.2 %>% mutate(ypercap = ytotcorh/numper, .keep = "unused")

# Veamos ahora el histograma de los datos.
g3 <- gghistogram(datos.2, x = "ypercap", xlab = "sexo", color = "sexo",
                  fill = "sexo", bins = 30)

g3 <- g3 + facet_grid(~ sexo)
print(g3)

# Podemos ver claramente que los datos no se asemejan en absoluto a una
# distribución normal, con  una fuerte desviación hacia la izquierda. Una buena
# alternativa robusta para la prueba t de Student, en este caso, puede ser la
# prueba de Yuen con bootstrapping usando como estimador la mediana.
B <- 5000
alfa <- 0.05

prueba.2 <- pb2gen(ypercap ~ sexo, data = datos.2, est = "median",
                   nboot = B)

print(prueba.2)

# Puesto que p = 0,072 > 0,05, se falla en rechazar H0. Concluimos entonces, con
# 95% confianza, que hombres y mujeres tienen, igual ingreso per cápita.



################################################################################
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.
################################################################################

cat("\n\n\nPREGUNTA 3\n")

# En este caso, consideraremos la pregunta: ¿Es igual el ingreso per cápita
# en las regiones de Atacama, Coquimbo y del Maule?
cat("¿Es igual el ingreso per cápita en las regiones de Atacama, ")
cat("Coquimbo y del Maule?\n\n")

# Filtrar datos y obtener muestra.
regiones <- c("Región de Coquimbo", "Región de Atacama", "Región del Maule")
datos.3 <- droplevels(datos %>% filter(region %in% regiones))

datos.3[["region"]] <- recode_factor(datos.3[["region"]],
                                     "Región de Atacama" = "Atacama",
                                     "Región de Coquimbo" = "Coquimbo",
                                     "Región del Maule" = "Maule")

set.seed(572)
datos.3 <- sample_n(datos.3, 500)
datos.3 <- datos.3 %>% select(ytotcorh, numper, region)
datos.3 <- datos.3 %>% mutate(ypercap = ytotcorh/numper, .keep = "unused")

# Veamos ahora el histograma de los datos.
g4 <- gghistogram(datos.3, x = "ypercap", xlab = "region", color = "region",
                  fill = "region", bins = 30)

g4 <- g4 + facet_grid(~ region)
print(g4)

# Una vez más, podemos ver que los datos no se asemejan en absoluto a una
# distribución normal, con  una fuerte desviación hacia la izquierda. Una buena
# alternativa robusta para ANOVA de una vía, en este caso, puede ser la
# función t1waybt, que realiza un procedimiento similar usando la media truncada y
# remuestreo.
poda <- 0.2
B <- 5000
alfa <- 0.05
prueba.3 <- t1waybt(ypercap ~ region, data = datos.3, tr = poda, nboot = B)
cat("Resultado de la prueba ómbibus:\n")
print(prueba.3)

# Puesto que p = 0,000 < 0,05, rechazamos H0 en favor de HA. Concluimos entonces,
# con 95% confianza, que las regiones de Atacama, Coquimbo y el Maue tienen
# diferente ingreso per cápita.

# Aplicamos ahora un procedimiento post-hoc.
post.hoc <- mcppb20(ypercap ~ region, data = datos.3, tr = poda, nboot = B)
cat("Resultado del procedimiento post-hoc:\n")
print(post.hoc)

# Atacama - Maule: p = 0,0008
# Atacama - Coquimbo: p = 0,1
# Maule - Coquimbo: p = 0
# En base al procedimiento post-hoc, podemos concluir con 99% de confianza
# que el ingreso per cápita de la región del Maule es diferente al de
# las regiones del Atacama y de Coquimbo.
