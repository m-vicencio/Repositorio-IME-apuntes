library(tidyverse)
library(pROC)
library(caret)
library(leaps)
library(car)

################################################################################
# Ahora podemos construir un modelo de regresión logística para predecir la
# variable EN, de acuerdo con las siguientes instrucciones:
# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
#    del RUN (sin considerar el dígito verificador) del integrante de mayor edad
#    del equipo.
# 2. Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o
#    120 hombres (si la semilla es impar), asegurando que la mitad tenga estado
#    nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir esta
#    muestra en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”)
#    para utilizar en la construcción de los modelos y 40 personas (20 con EN
#    “sobrepeso”) para poder evaluarlos.
# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma
#    aleatoria en el ejercicio anterior.
# 4. Seleccionar, de las otras variables, una que el equipo considere que podría
#    ser útil para predecir la clase EN, justificando bien esta selección.
# 5. Usando el entorno R y paquetes estándares1, construir un modelo de
#    regresión logística con el predictor seleccionado en el paso anterior y
#    utilizando de la muestra obtenida.
# 6. Usando herramientas estándares1 para la exploración de modelos del entorno
#    R, buscar entre dos y cinco predictores de entre las variables
#    seleccionadas al azar, recordadas en el punto 3, para agregar al modelo
#    obtenido en el paso 5.
# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de
#    ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún
#    problema.
# 8. Usando código estándar1, evaluar el poder predictivo de los modelos con los
#    datos de las 40 personas que no se incluyeron en su construcción en
#    términos de sensibilidad y especificidad.
################################################################################

# Fijar carpeta de trabajo.
setwd("D:/dropbox/Inferencia/Ejercicios prácticos 1-2022/EP13")

# Fijar semilla.
set.seed(1111)

# Cargar datos.
datos <- read.csv2("EP13 Datos.csv")

# Generar columna con estado nutricional.
IMC <- datos[["Weight"]] / ((datos[["Height"]] / 100) ** 2)
EN <- rep("Sobrepeso", length(IMC))
EN[IMC < 25] <- "No sobrepeso"
EN <- factor(EN)
datos <- cbind(EN, datos)

# Filtrar datos para dejar solo hombres.
datos <- datos %>% filter(Gender == 1)
datos[["Gender"]] <- NULL

# Seleccionar variables escogidas en ejercicio anterior. Adicionalmente se
# escoge como predictor para el modelo de RLogS el peso, pues esta variable está
# fuertemente relacionada con el IMC, indicador empleado para determinar el
# sobrepeso.
datos <- datos %>% select(EN, Knee.Girth, Bicep.Girth, Ankles.diameter,
                          Chest.depth, Shoulder.Girth, Navel.Girth,
                          Hip.Girth, Biiliac.diameter, Weight)

# Obtener muestra.
sobrepeso <- datos %>% filter(EN == "Sobrepeso")
sobrepeso <- sample_n(sobrepeso, 60, replace = FALSE)

normal <- datos %>% filter(EN == "No sobrepeso")
normal <- sample_n(normal, 60, replace = FALSE)

# Separar conjuntos de entrenamiento y prueba.
split_sobrepeso <- sample.int(nrow(sobrepeso), 40, replace = FALSE)
split_normal <- sample.int(nrow(normal), 40, replace = FALSE)

entr_sobrepeso <- sobrepeso[split_sobrepeso,]
entr_normal <- normal[split_normal,]
prue_sobrepeso <- sobrepeso[-split_sobrepeso,]
prue_normal <- normal[-split_normal,]

entrenamiento <- rbind(entr_sobrepeso, entr_normal)
prueba <- rbind(prue_sobrepeso, prue_normal)



################################################################################
# RLogS
################################################################################

cat("-----------------------------------------------------------------------\n")
cat("Regresión logística simple\n")
cat("-----------------------------------------------------------------------\n")

# Ajustar modelo de regresión logística simple.
rlogs <- glm(EN ~ Weight, data = entrenamiento,
             family = binomial(link = "logit"))

cat("\nModelo de regresión logística simple\n")
print(summary(rlogs))

# Evaluar calidad predictiva del modelo.
umbral <- 0.5
probabilidades <- predict(rlogs, prueba, type = "response")

predicciones <- sapply(probabilidades,
                       function (p) ifelse (p >= umbral, "Sobrepeso", "No sobrepeso"))

predicciones <- factor(predicciones, levels = levels(prueba[["EN"]]))
cat("Calidad predictiva del modelo\n\n")
print(confusionMatrix(predicciones, prueba[["EN"]]))

# Podemos ver que el tiene una buena capacidad predictiva, con una exactitud de
# 82,5%, sensibilidad de 80,0% y especificidad de 85,0%.

# Obtener residuos y estadísticas de influencia de los casos.
eval.rlogs <- data.frame(predicted.probabilities = fitted(rlogs))
eval.rlogs[["standardized.residuals"]] <- rstandard(rlogs)
eval.rlogs[["studentized.residuals"]] <-rstudent(rlogs)
eval.rlogs[["cooks.distance"]] <- cooks.distance(rlogs)
eval.rlogs[["dfbeta"]] <- dfbeta(rlogs)
eval.rlogs[["dffit"]] <- dffits(rlogs)
eval.rlogs[["leverage"]] <- hatvalues(rlogs)
eval.rlogs[["covariance.ratios"]] <- covratio(rlogs)

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rlogs[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rlogs[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(entrenamiento) / nrow(entrenamiento)
sospechosos3 <- which(eval.rlogs[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rlogs[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio
sospechosos5 <- which(eval.rlogs[["covariance.ratios"]] < CVRi.lower |
                        eval.rlogs[["covariance.ratios"]] > CVRi.upper)
cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rlogs[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

# Mostrar curva ROC.
probs.rlogs <- predict(rlogs, entrenamiento, type = "response")
roc.rlogs <- roc(entrenamiento[["EN"]], probs.rlogs)
plot(roc.rlogs)



################################################################################
# RLogM
################################################################################

cat("\n\n\n")
cat("-----------------------------------------------------------------------\n")
cat("Regresión logística multivariada\n")
cat("-----------------------------------------------------------------------\n")

# Seleccionar mejores predictores para modelo de regresión lineal múltiple
# usando el método de todos los subconjuntos.
rlogm.inicial <- regsubsets(EN ~ ., data = entrenamiento, nbest = 1, nvmax = 5,
                            method = "exhaustive")

plot(rlogm.inicial)

# De acuerdo a la exploración de todos los subconjuntos, el mejor modelo con
# entre 2 y 5 variables es aquel que usa como predictores el grosor de los
# bíceps, la profundidad del pecho, el grosor a la altura del ombligo, el grosor
# de las caderas y el diámetro biilíaco.

# Ajustar el modelo con los mejores predictores.
rlogm <- glm(EN ~ Bicep.Girth + Chest.depth + Navel.Girth + Hip.Girth + Biiliac.diameter,
               data = entrenamiento, family = binomial(link = "logit"))

cat("\nModelo de regresión logística multivariada\n")
print(summary(rlogm))

# Evaluamos el factor de inflación de la varianza.
vifs <- vif(rlogm)
cat("\nVIF: ")
print(vifs)

# Si bien ninguna de las variables presenta una inflación de la varianza
# superior a 10, que el VIF promedio sea mayor que 1 podría ser señal de
# preocupación.

# El modelo obtenido tiene un AIC más alto que el modelo de RLogS. En
# consecuencia, el modelo más simple parece ajustarse mejor a los datos.

# Evaluar calidad predictiva del modelo.
probabilidades <- predict(rlogm, prueba, type = "response")

predicciones <- sapply(probabilidades,
                       function (p) ifelse (p >= umbral, "Sobrepeso", "No sobrepeso"))

predicciones <- factor(predicciones, levels = levels(prueba[["EN"]]))
cat("Calidad predictiva del modelo\n\n")
print(confusionMatrix(predicciones, prueba[["EN"]]))

# Podemos ver que el tiene una capacidad predictiva inferior al modelo de
# regresión logística simple, con una exactitud de 75,0%, sensibilidad de 70,0%
# y especificidad de 80,0%.

# Obtener residuos y estadísticas de influencia de los casos.
eval.rlogm <- data.frame(predicted.probabilities = fitted(rlogm))
eval.rlogm[["standardized.residuals"]] <- rstandard(rlogm)
eval.rlogm[["studentized.residuals"]] <-rstudent(rlogm)
eval.rlogm[["cooks.distance"]] <- cooks.distance(rlogm)
eval.rlogm[["dfbeta"]] <- dfbeta(rlogm)
eval.rlogm[["dffit"]] <- dffits(rlogm)
eval.rlogm[["leverage"]] <- hatvalues(rlogm)
eval.rlogm[["covariance.ratios"]] <- covratio(rlogm)

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rlogm[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rlogm[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(entrenamiento) / nrow(entrenamiento)
sospechosos3 <- which(eval.rlogm[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rlogm[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio
sospechosos5 <- which(eval.rlogm[["covariance.ratios"]] < CVRi.lower |
                        eval.rlogm[["covariance.ratios"]] > CVRi.upper)
cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rlogm[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

# Mostrar curva ROC.
probs.rlogm <- predict(rlogm, entrenamiento, type = "response")
roc.rlogm <- roc(entrenamiento[["EN"]], probs.rlogm)
plot(roc.rlogm)

cat("\nIndependencia de los residuos\n")
print(durbinWatsonTest(rlogm))

# Puesto que la prueba de Durbin-Watson entrega p = 0, podemos concluir que
# los residuos NO son independientes entre sí, por lo que el modelo no debería
# ser utilizado.