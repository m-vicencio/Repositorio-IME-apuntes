library(tidyverse)

################################################################################
# Se pide construir un modelo de regresión lineal múltiple para predecir la
# variable respuesta, de acuerdo con las siguientes instrucciones:
# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
#    del RUN (sin considerar el dígito verificador) del integrante de menor edad
#    del equipo.
# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50
#    hombres (si la semilla es impar).
# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.
# 4. Seleccionar, de las otras variables, una que el equipo considere que podría
#    ser útil para predecir los diámetros de las rodillas (Knees.diameter),
#    justificando bien esta selección.
# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el
#    predictor seleccionado en el paso anterior.
# 6. Usando herramientas para la exploración de modelos del entorno R, buscar
#    entre dos y cinco predictores de entre las variables seleccionadas al azar
#    en el punto 3, para agregar al modelo de regresión lineal simple obtenido
#    en el paso 5.
# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema
#    con las condiciones que deben cumplir.
# 8. Evaluar el poder predictivo del modelo en datos no utilizados para
#    construirlo (o utilizando validación cruzada).
################################################################################

# Fijar carpeta de trabajo.
setwd("D:/dropbox/Inferencia/Ejercicios prácticos 1-2022/EP13")
# Fijar semilla.
set.seed(1111)

# Cargar datos.
datos <- read.csv2("EP13 Datos.csv")

# Obtener muestra.
datos <- datos %>% filter(Gender == 1)
datos[["Gender"]] <- NULL
datos <- sample_n(datos, 50, replace = FALSE)

# Separar variable de respuesta.
respuesta <- datos[["Knees.diameter"]]
datos[["Knees.diameter"]] <- NULL

# Seleccionar 8 variables predictoras al azar y construir matriz de datos para
# RLM.
variables <- colnames(datos)
predictores <- sample(variables, 8, replace = FALSE)
cat("Predictores seleccionados al azar:\n")
print(predictores)
datos.rlm <- datos %>% select(predictores)
datos.rlm <- cbind(respuesta, datos.rlm)

# Los predictores seleccionados al azar para el modelo de regresión lineal
# múltiple son: Knee.Girth, Bicep.Girth, Ankles.diameter, Chest.depth,
# Shoulder.Girth, Navel.Girth, Hip.Girth, Biiliac.diameter.

# Evaluar correlación de las variables restantes con la respuesta.
datos <- datos %>% select(!predictores)
cat("\nMatriz de correlación:\n")
correlacion <- cor(datos, y = respuesta)
cat("\nMatriz de correlación:\n")
print(correlacion)

# El mejor predictor para un modelo de RLS es aquella variable con mayor
# correlación (directa o inversa) con la variable de respuesta. Armar conjunto
# de datos para RLS.
mejor <- which(correlacion == max(abs(correlacion)))
predictor <- rownames(correlacion)[mejor]
datos.rls <- datos %>% select(mejor)
datos.rls <- cbind(respuesta, datos.rls)

cat("\nVariable más correlacionada con el diámetro de las rodillas:",
    predictor, "\n\n\n\n")



################################################################################
# RLS
################################################################################

cat("-----------------------------------------------------------------------\n")
cat("Regresión lineal simple\n")
cat("-----------------------------------------------------------------------\n")

# Ajustar modelo de regresión lineal simple usando validación cruzada de 10
# pliegues.
library(caret)

rls <- train (respuesta ~ Thigh.Girth, data = datos.rls, method = "lm",
              trControl = trainControl(method = "cv", number = 10))

cat("\nModelo de regresión lineal simple\n")
print(summary(rls))

# Evaluar modelo.
# Obtener residuos y estadísticas de influencia de los casos.
eval.rls <- data.frame(predicted.probabilities = fitted(rls[["finalModel"]]))
eval.rls[["standardized.residuals"]] <- rstandard(rls[["finalModel"]])
eval.rls[["studentized.residuals"]] <-rstudent(rls[["finalModel"]])
eval.rls[["cooks.distance"]] <- cooks.distance(rls[["finalModel"]])
eval.rls[["dfbeta"]] <- dfbeta(rls[["finalModel"]])
eval.rls[["dffit"]] <- dffits(rls[["finalModel"]])
eval.rls[["leverage"]] <- hatvalues(rls[["finalModel"]])
eval.rls[["covariance.ratios"]] <- covratio(rls[["finalModel"]])

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rls[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rls[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(datos.rls) / nrow(datos.rls)
sospechosos3 <- which(eval.rls[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rls[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio

sospechosos5 <- which(eval.rls[["covariance.ratios"]] < CVRi.lower |
                        eval.rls[["covariance.ratios"]] > CVRi.upper)

cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rls[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

# Ahora bien, podría ser preocupante que el modelo explica apenas 40,91% de la
# variabilidad en el rendimiento, por lo que su bondad de ajuste es bajo con
# respecto a las observaciones.



################################################################################
# RLM
################################################################################

cat("\n\n\n")
cat("-----------------------------------------------------------------------\n")
cat("Regresión lineal múltiple\n")
cat("-----------------------------------------------------------------------\n")

# Seleccionar mejores predictores para modelo de regresión lineal múltiple
# usando el método de todos los subconjuntos.
library(leaps)

rlm.inicial <- regsubsets(respuesta ~ ., data = datos.rlm, nbest = 1, nvmax = 5,
                          method = "exhaustive")

plot(rlm.inicial)

# De acuerdo a la exploración de todos los subconjuntos, el mejor modelo con
# entre 2 y 5 variables es aquel que usa como predictores el diámetro de los
# tobillos y el grosor de las caderas.

# Ajustar el modelo con los mejores predictores usando validación cruzada de 10
# pliegues.
rlm <- train (respuesta ~ Ankles.diameter + Hip.Girth, data = datos.rlm,
              method = "lm",
              trControl = trainControl(method = "cv", number = 10))

cat("\nModelo de regresión lineal múltiple\n")
print(summary(rlm))

# Evaluar modelo.
# Obtener residuos y estadísticas de influencia de los casos.
eval.rlm <- data.frame(predicted.probabilities = fitted(rlm[["finalModel"]]))
eval.rlm[["standardized.residuals"]] <- rstandard(rlm[["finalModel"]])
eval.rlm[["studentized.residuals"]] <-rstudent(rlm[["finalModel"]])
eval.rlm[["cooks.distance"]] <- cooks.distance(rlm[["finalModel"]])
eval.rlm[["dfbeta"]] <- dfbeta(rlm[["finalModel"]])
eval.rlm[["dffit"]] <- dffits(rlm[["finalModel"]])
eval.rlm[["leverage"]] <- hatvalues(rlm[["finalModel"]])
eval.rlm[["covariance.ratios"]] <- covratio(rlm[["finalModel"]])

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rlm[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rlm[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(datos.rlm) / nrow(datos.rlm)
sospechosos3 <- which(eval.rlm[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rlm[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio

sospechosos5 <- which(eval.rlm[["covariance.ratios"]] < CVRi.lower |
                        eval.rlm[["covariance.ratios"]] > CVRi.upper)

cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rlm[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

# En este caso, el modelo explica 51,19% de la variabilidad en el rendimiento,
# lo que sugiere que se ajusta mejor a los datos que el modelo de RLS.

library(car)
cat("\nIndependencia de los residuos\n")
print(durbinWatsonTest(rlm[["finalModel"]]))

# Puesto que la prueba de Durbin-Watson entrega p = 0,466, podemos concluir que
# los residuos son independientes.