# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP11")

# Cargar paquetes
library(tidyverse)
library(bootES)
library(ez)

# Cargar datos
datos <- read.csv2("EP11 Datos.csv", stringsAsFactors = TRUE)



################################################################################
# 1. Propongan una pregunta de investigación original, que involucre la
# comparación de las medias de dos grupos independiente. Fijando una semilla
# propia, seleccionen una muestra aleatoria de hogares (250 < n < 500) y
# respondan la pregunta propuesta utilizando una simulación Monte Carlo.
################################################################################

# En este caso, consideraremos la pregunta: ¿Es igual el ingreso per cápita
# entre hombres y mujeres?
cat("1. ¿Es igual el ingreso per cápita entre hombres y mujeres?\n\n")

# Filtrar datos y obtener muestra.
set.seed(348)
datosp1 <- sample_n(datos, 350)
datosp1 <- datosp1 %>% select(ytotcorh, numper, sexo)
datosp1 <- datosp1 %>% mutate(ypercap = ytotcorh/numper, .keep = "unused")

# Formulamos las hipótesis:
# H0: En promedio, hombres y mujeres tienen igual ingreso per cápita
#     (media_h - media_m = 0).
# HA: En promedio, hombres y mujeres tienen distinto ingreso per cápita
#     (media_h - media_m != 0).

# Separar hombres y mujeres
hombres <- datosp1[datosp1[["sexo"]] == "Hombre", "ypercap"]
mujeres <- datosp1[datosp1[["sexo"]] != "Hombre", "ypercap"]

# Calcular diferencia observada.
observado <- mean(hombres) - mean(mujeres)

# Aplicar simulación de Monte Carlo, con nivel de significación 0,05.
# Obtener permutaciones y obtener diferencia de medias.
permutar <- function(i, m1, m2) {
  n1 <- length(m1)
  combinada <- c(m1, m2)
  nt <- length(combinada)
  permutacion <- sample(combinada, nt, replace = FALSE)
  nueva1 <- permutacion[1:n1]
  nueva2 <- permutacion[(n1+1):nt]
  return(mean(nueva1) - mean(nueva2))
}

R <- 5000
distribucion <- unlist(lapply(1:R, permutar, hombres, mujeres))

# Calcular valor p.
numerador <- sum(abs(distribucion) > abs(observado)) + 1
denominador <- R + 1
valor_p_1 <- numerador / denominador

cat("Prueba de hipótesis: p =", valor_p_1, "\n\n\n")

# Puesto que p = 0,0028 < 0,05, se rechaza H0 en favor de HA. Concluimos
# entonces, con 95% confianza, que hombres y mujeres tienen, en promedio,
# diferente ingreso per cápita.



################################################################################
# 2. Propongan una pregunta de investigación original, que involucre la
# comparación de las medias de más de dos grupos independientes (más abajo se
# dan unos ejemplos). Fijando una semilla distinta a la anterior, seleccionen
# una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta
# propuesta utilizando bootstrapping. Solo por ejercicio académico, aplique un
# análisis post-hoc con bootstrapping aunque este no sea necesario.
################################################################################

# En este caso, consideraremos la pregunta: ¿Es igual el ingreso per cápita
# en las regiones de Atacama, Coquimbo y del Maule?
cat("2. ¿Es igual el ingreso per cápita en las regiones de Atacama, ")
cat("Coquimbo y del Maule?\n\n")

# Filtrar datos y obtener muestra.
regiones <- c("Región de Coquimbo", "Región de Atacama", "Región del Maule")
datosp2 <- droplevels(datos %>% filter(region %in% regiones))

datosp2[["region"]] <- recode_factor(datosp2[["region"]],
                                     "Región de Atacama" = "Atacama",
                                     "Región de Coquimbo" = "Coquimbo",
                                     "Región del Maule" = "Maule")

set.seed(572)
datosp2 <- sample_n(datosp2, 500)
datosp2 <- datosp2 %>% select(ytotcorh, numper, region)
datosp2 <- datosp2 %>% mutate(ypercap = ytotcorh/numper, .keep = "unused")

# Formulamos las hipótesis:
# H0: En promedio, el ingreso per cápita es igual en las regiones de Coquimbo,
#     Atacama y el Maule.
# HA: En promedio, el ingreso per cápita es distinto en las regiones de
#     Coquimbo, Atacama y el Maule.

# Comprobar tamaño de las muestras.
cat("Tamaño de las muestras:\n")
print(summary(datosp2[["region"]]))
cat("\n")

# Como mas muestras tienen diferente tamaño, es más adecuado usar la prueba de
# Kruskal-Wallis. Consideraremos un nivel de significación de 0,01.

# Obtener estadístico para la muestra original.
alfa <- 0.01
prueba <- kruskal.test(ypercap ~ region , data = datosp2)
observado <- prueba[["statistic"]]

# Aplicar bootstrapping con nivel de significación 0,01.
B <- 2000

# Función para remuestreo.
remuestrear <- function(i, df) {
  n <- nrow(df)
  remuestreado <- sample(df[["ypercap"]], n, replace = TRUE)
  nuevo <- data.frame(df[["region"]], remuestreado)
  colnames(nuevo) <- colnames(df)
  return(nuevo)
}

# Función para obtener el estadístico de la prueba de Kruskal-Wallis.
obtener_chi_cuad <- function(df) {
  kruskal <- kruskal.test(ypercap ~ region , data = df)
  return(kruskal[["statistic"]])
}

remuestreos <- lapply(1:B, remuestrear, datosp2)
distribucion <- sapply(remuestreos, obtener_chi_cuad)

# Calcular el valor p.
valor_p_2 <- (sum(abs(distribucion) > abs(observado)) + 1) / (B + 1)
cat("Prueba ómnibus: p =", valor_p_2, "\n\n")

# Puesto que p = 0,0004997501 < 0,01, se rechaza H0 en favor de HA. Concluimos
# entonces, con 95% confianza, que el ingreso per cápita promedio no es igual
# para todas las regiones.

# Aplicamos ahora un procedimiento post-hoc, aplicando la prueba de suma de
# rangos de Wilcoxon para cada par de regiones.

# Obtener estadísticos observados.
prueba <- wilcox.test(datosp2[datosp2[["region"]] == "Atacama", "ypercap"],
                         datosp2[datosp2[["region"]] == "Maule", "ypercap"],
                         alternative = "two.sided", conf.level = 1 - alfa)

obs_at_ma <- prueba[["statistic"]]

prueba <- wilcox.test(datosp2[datosp2[["region"]] == "Atacama", "ypercap"],
                         datosp2[datosp2[["region"]] == "Coquimbo", "ypercap"],
                         alternative = "two.sided", conf.level = 1 - alfa)

obs_at_co <- prueba[["statistic"]]

prueba <- wilcox.test(datosp2[datosp2[["region"]] == "Maule", "ypercap"],
                         datosp2[datosp2[["region"]] == "Coquimbo", "ypercap"],
                         alternative = "two.sided", conf.level = 1 - alfa)

obs_ma_co <- prueba[["statistic"]]

# Obtener diferencias de para los remuestreos.

obtener_estadistico <- function(df, region1, region2) {
  comparacion <- wilcox.test(df[df[["region"]] == region1, "ypercap"],
                            df[df[["region"]] == region2, "ypercap"],
                            alternative = "two.sided", conf.level = 1 - alfa)
  
  return(comparacion[["statistic"]])
}

dist_at_ma <- sapply(remuestreos, obtener_estadistico, "Atacama", "Maule")
dist_at_co <- sapply(remuestreos, obtener_estadistico, "Atacama", "Coquimbo")
dist_ma_co <- sapply(remuestreos, obtener_estadistico, "Maule", "Coquimbo")

# Calcular valores p.
den <- B + 1

num <- sum(abs(dist_at_ma) > abs(obs_at_ma)) + 1
p_at_ma <- num / den
cat("Procedimiento post-hoc:\n")
cat(sprintf("  Atacama - Maule: p = %.3f\n", p_at_ma))

num <- sum(abs(dist_at_co) > abs(obs_at_co)) + 1
p_at_co <- num / den
cat(sprintf("  Atacama - Coquimbo: p = %.3f\n", p_at_co))

num <- sum(abs(dist_ma_co) > abs(obs_ma_co)) + 1
p_ma_co <- num / den
cat(sprintf("  Maule - Coquimbo: p = %.3f\n", p_ma_co))

# Atacama - Maule: p = 0,000
# Atacama - Coquimbo: p = 0,001
# Maule - Coquimbo: p = 0,926
# En base al procedimiento post-hoc, podemos concluir con 99% de confianza
# que el ingreso per cápita promedio de la región de Atacama es diferente al de
# las regiones del Maule y de Coquimbo.