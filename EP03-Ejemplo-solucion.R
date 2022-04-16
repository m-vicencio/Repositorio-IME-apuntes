# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP03")

# Importar paquetes.
library(dplyr)
library(ggpubr)



################################################################################
# PARTE I: DISTRIBUCIONES CONTINUAS
################################################################################

################################################################################
# Obtener una distribución de ingresos aproximadamente normal.
################################################################################

# Generar una distribución de ingresos aproximadamente a partir de los datos
# de la encuesta Casen 2017 usando la semilla 666.
población <- read.csv2("EP03 Datos Casen 2017.csv")
# tamaño <- nrow(población)
# ingreso <- as.numeric(población[["ytot"]])
# poda <- 0.2
# q20 <- quantile(ingreso, poda)
# q80 <- quantile(ingreso, 1 - poda)
# ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
# tamaño.podado <- length(ingreso.podado)
# media.ingreso <- mean(ingreso.podado)
# sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
# set.seed(666)
# ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
# 
# # Comprobamos si la distribución se asemeja realmente a la normal. Para eso,
# # guardamos nuestra muestra en una matriz de datos y podemos hacer, por ejemplo,
# # un histograma.
# muestra <- data.frame(ingreso.normal)
# 
# g1 <- gghistogram(data = muestra, x = "ingreso.normal", bins = 31)
# g1 <- g1 + xlab("Ingresos [pesos]")
# g1 <- g1 + ylab("Densidad")
# g1 <- g1 + ggtitle("Ingresos para la muestra")
# print(g1)
# 
# # En efecto, la distribución de la muestra se asemeja bastante a la normal. No
# # se observa asimetría ni valores atípicos.
# 
# 
# 
# ################################################################################
# # Generar la correspondiente distribución Z.
# ################################################################################
# 
# # Generar una distribución Z a partir de la distribución normal. La distribución
# # Z, también llamada normal estándar, no es más que una ditribución normal con
# # media 0 y desviación estándar 1.
# 
# media.muestra <- mean(ingreso.normal)
# sd.muestra <- sd(ingreso.normal)
# normal.estandar <- (ingreso.normal - media.muestra) / sd.muestra
# 
# # Guardamos el resultado en una matriz de datos creamos el histograma.
# z <- data.frame(normal.estandar)
# 
# g2 <- gghistogram(data = z, x = "normal.estandar", bins = 31)
# g2 <- g2 + xlab("Ingresos estandarizados")
# g2 <- g2 + ylab("Densidad")
# g2 <- g2 + ggtitle("Ingresos estandarizados para la muestra")
# print(g2)
# 
# # Podemos ver que la forma de la nueva distribución se asemeja mucho a la de la
# # muestra original, pero ahora la media es 0 y la desviación estándar es
# # unitaria.
# 
# 
# 
# ################################################################################
# # Generar dos distribuciones chi-cuadrado.
# ################################################################################
# 
# muestra.chica <- sample(normal.estandar, 30)
# 
# # Sabemos que los grdos de libertad pueden entenderse como la cantidad de
# # valores que pueden cambiar libremente en un conjunto de datos.
# 
# # También sabemos que la distribución chi-cuadrado está relacionada con la
# # distribución Z: la suma de los cuadrados de k variables aleatorias
# # independientes que siguen una distribución Z sigue una distribución
# # chi-cuadrado con k grados de libertad.
# 
# # Puesto que todas las secuencias de 8 elementos para una muestra de tamaño 5000
# # requiere mucha memoria, tomemos una submuestra más pequeña, de tamaño 25, de
# # nuestra distribución Z.
# sub.muestra <- sample(normal.estandar, 25)
# 
# # Ahora obtengamos las secuencias de 8 elementos de nuestra muestra
# # estandarizada:
# secuencias.8 <- combn(sub.muestra, 8)
# 
# # Ahora generemos una distribución chi-cuadrado con 8 grados de libertad
# # sumando los cuadrados de estas secuencias.
# chisq.8 <- apply(secuencias.8, 2, function(x) sum(x^2))
# 
# # Veamos el histograma de esta distribución.
# chi.cuadrado.8 <- data.frame(chisq.8)
# 
# g3 <- gghistogram(data = chi.cuadrado.8, x = "chisq.8", bins = 31)
# g3 <- g3 + xlab("Sumas cuadradas de 8 valores Z")
# g3 <- g3 + ylab("Densidad")
# g3 <- g3 + ggtitle("Distribución chi-cuadrado con 8 grados de libertad")
# print(g3)
# 
# # Repitamos el proceso para generar una nueva distribución chi-cuadrado con 5
# # grados de libertad.
# secuencias.5 <- combn(sub.muestra, 5)
# chisq.5 <- apply(secuencias.5, 2, function(x) sum(x^2))
# chi.cuadrado.5 <- data.frame(chisq.5)
# 
# # Construyamos el histograma.
# g4 <- gghistogram(data = chi.cuadrado.5, x = "chisq.5", bins = 31)
# g4 <- g4 + xlab("Sumas cuadradas de 5 valores Z")
# g4 <- g4 + ylab("Densidad")
# g4 <- g4 + ggtitle("Distribución chi-cuadrado con 5 grados de libertad")
# print(g4)
# 
# # Pero tenemos un problema. Sumar 6 valores z^2 suele dar un resultado mayor
# # que sumar solo 5 de estos valores, por lo que es necesario estandarizar.
# chisq.8.std <- chisq.8 / 8
# chisq.5.std <- chisq.5 / 5
# 
# 
# 
# ################################################################################
# # Generar una distribución F a partir de dos distribuciones chi-cuadrado.
# ################################################################################
# 
# # Hemos estudiado que la distribución F corresponde a la razón entre dos
# # distribuciones chi-cuadrado.
# 
# # Como las distribuciones chi-cuadrado estandarizadas que obtuvimos en el paso
# # anterior tienen tamaños diferentes, tomemos para cada una grupos
# # independientes escogidos al azar.
# n <- min(length(chisq.8.std), length(chisq.5.std))
# 
# # Generamos una ditribución F con la razón entre ambos grupos. 
# f <- sample(chisq.8.std, n) / sample(chisq.5.std, n)
# 
# # Y ahora creamos el histograma correspondiente.
# distribucion.F <- data.frame(f)
# g5 <- gghistogram(data = distribucion.F, x = "f", bins = 31)
# g5 <- g5 + xlab("Razón de sumas cuadradas independientes")
# g5 <- g5 + ylab("Densidad")
# g5 <- g5 + ggtitle("Distribución F con 8 y 5 grados de libertad")
# print(g5)



################################################################################
# PARTE II: DISTRIBUCIONES DISCRETAS
################################################################################

################################################################################
# Obtener una distribución de Bernoulli.
################################################################################

# Generar una distribución de Bernoulli a partir de los datos
# de la encuesta Casen 2017 con 30 repeticiones, usando la semilla 666.
set.seed(666)
n.repeticiones <- 30

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

bernoulli <- sapply(1:n.repeticiones, ensayo)

# Calculamos la proporción de mujeres.
p <- sum(bernoulli == 1) / length(bernoulli)



################################################################################
# Generar una distribución binomial.
################################################################################

# Estudiamos que la  distribución binomial describe la probabilidad de tener
# exactamente k éxitos en n intentos independientes de Bernoulli con
# probabilidad de éxito p.

# Veamos la distribución binomial asociada a la siguiente pregunta: ¿cuántas
# mujeres habrá en un grupo de 5 encuestados?

# Generamos las combinaciones de 5 encuestados.
mujeres.5 <- combn(bernoulli, 5)

# Ahora contamos la cantidad de éxitos en cada grupo.
exitos.mujeres.5 <- apply(mujeres.5, 2, function(x) sum(x == 1))

# Generamos el gráfico correspondiente.
binomial <- data.frame(exitos.mujeres.5)
g6 <- gghistogram(data = binomial, x = "exitos.mujeres.5", bins = 6)
g6 <- g6 + xlab("Cantidad de mujeres en un grupo de 5 encuestados")
g6 <- g6 + ylab("Densidad")
g6 <- g6 + ggtitle("Distribución binomial")
print(g6)



################################################################################
# Generar una distribución geométrica.
################################################################################

# Sabemos que la distribución geométroca describe la cantidad de intentos que
# debemos hacer hasta obtener un éxito para variables de Bernoulli.

# Veamos la distribución binomial asociada a la pregunta: ¿a cuántas
# personas debemos encuestar hasta encontrar a una mujer?

# Deberíamos ordenar nuestra muestra de encuestados de todas las formas
# posibles (permutaciones), aunque eso es muy costoso en memoria. ¡Para una 
# muestra de 30 elementos tenemos 30! = 2,652529 x 10^32 permutaciones!

# Una alternativa es usar "muchas" muestras de la población, por ejemplo, 6.000.
permutaciones <- sapply(1:6000, function(i) sample(bernoulli))

# Ahora vemos en qué posición aparece la primera mujer en cada permutación.
geom <- apply(permutaciones, 2, function(x) which(x == 1)[1])

# Generamos el gráfico correspondiente.
geometrica <- data.frame(geom)
g7 <- gghistogram(data = geometrica, x = "geom", bins = 8)
g7 <- g7 + xlab("Cantidad de encuestados hasta encontrar una mujer")
g7 <- g7 + ylab("Densidad")
g7 <- g7 + ggtitle("Distribución geométrica")
print(g7)



################################################################################
# Generar una distribución binomial negativa.
################################################################################

# Como hemos estudiado, La distribución binomial negativa describe la
# probabilidad de encontrar el k-ésimo éxito al n-ésimo intento.

# Veamos la distribución binomial asociada a la pregunta: ¿a cuántas
# personas debemos encuestar hasta encontrar a 5 mujeres?

# Este ejercicio también requiere que generemos todas las permutaciones,
# pero podemos usar la "muestra" que obtivimos para la pregunta anterior.

# Ahora vemos en qué posición aparece la quinta mujer en cada permutación.
negativa <- apply(permutaciones, 2, function(x) which(x == 1)[5])

# Generamos el gráfico correspondiente.
binomial.negativa <- data.frame(negativa)
g8 <- gghistogram(data = binomial.negativa, x = "negativa", bins = 18)
g8 <- g8 + xlab("Cantidad de encuestados hasta encontrar 5 mujeres")
g8 <- g8 + ylab("Densidad")
g8 <- g8 + ggtitle("Distribución binomial negativa")
print(g8)
