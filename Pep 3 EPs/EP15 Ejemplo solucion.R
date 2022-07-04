library(tidyverse)
library(caret)
library(pROC)
library(leaps)
library(car)

################################################################################
# 1. Definir la semilla a utilizar, que corresponde a los primeros cinco
#    dígitos del RUN del integrante de mayor edad del equipo.
# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga
#    estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. 
# 3. Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva
#    para seleccionar entre dos y ocho predictores que ayuden a estimar la
#    variable Peso (Weight), obviamente sin considerar las nuevas variables IMC
#    ni EN, y luego utilizar las funciones del paquete caret para construir un
#    modelo de regresión lineal múltiple con los predictores escogidos y
#    evaluarlo usando bootstrapping.
# 4. Haciendo un poco de investigación sobre el paquete caret, en particular
#    cómo hacer Recursive Feature Elimination (RFE), construir un modelo de
#    regresión lineal múltiple para predecir la variable IMC que incluya entre
#    10 y 20 predictores, seleccionando el conjunto de variables que maximice R2
#    y que use cinco repeticiones de validación cruzada de cinco pliegues para
#    evitar el sobreajuste (obviamente no se debe considerar las variables Peso,
#    Estatura ni estado nutricional –Weight, Height, EN respectivamente). 
# 5. Usando RFE, construir un modelo de regresión logística múltiple para la
#    variable EN que incluya el conjunto, de entre dos y seis, predictores que
#    entregue la mejor curva ROC y que utilice validación cruzada dejando uno
#    fuera para evitar el sobreajuste (obviamente no se debe considerar las
#    variables Peso, Estatura –Weight y Height respectivamente– ni IMC).
# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos.
################################################################################

# Fijar carpeta de trabajo.
setwd("D:/dropbox/Inferencia/Ejercicios prácticos 1-2022/EP13")

# Fijar semilla.
set.seed(1111)

# Cargar datos.
datos <- read.csv2("EP13 Datos.csv")

# Generar nuevas columnas.
datos[["IMC"]] <- datos[["Weight"]] / ((datos[["Height"]] / 100) ** 2)
datos[["EN"]] <- rep("Sobrepeso", length(datos[["IMC"]]))
datos[["EN"]][datos[["IMC"]] < 25] <- "No sobrepeso"
datos[["EN"]] <- factor(datos[["EN"]])

# Seleccionar muestra.
sobrepeso <- datos %>% filter(EN == "Sobrepeso")
sobrepeso <- sample_n(sobrepeso, 50, replace = FALSE)
normal <- datos %>% filter(EN != "Sobrepeso")
normal <- sample_n(normal, 50, replace = FALSE)
muestra <- rbind(sobrepeso, normal)
rm(datos, normal, sobrepeso)



################################################################################
# Regresión lineal múltiple para la variable peso.
################################################################################

# Descartar columnas inútiles
datos.peso <- muestra %>% select(-c(IMC, EN))

# Seleccionar predictores usando el método de todos los subconjuntos.
preliminar <- regsubsets(Weight ~ ., data = datos.peso, nbest = 1, nvmax = 8,
                         method = "exhaustive")

plot(preliminar)

# El modelo con menor BIC con entre 2 y 8 predictores incluye únicamente el
# diámetro del pecho y el diámetro de las caderas.
datos.peso <- datos.peso %>% select(Weight, Chest.Girth, Hip.Girth)

# Ajustar modelo usando bootstrapping con 2999 remuestreos.
modelo.peso <- train(Weight ~ ., data = datos.peso, method = "lm",
                     trControl = trainControl(method = "boot", number = 2999))

print(summary(modelo.peso))

# El modelo obtenido presenta un R^2 ajustado de 0,9203. Esto significa que el
# modelo obtenido explica el 92,03% de la variabilidad de los datos.

# Veamos la calidad predictiva del modelo.
predicciones.peso <- predict(modelo.peso, datos.peso)
error.peso <- datos.peso[["Weight"]] - predicciones.peso
rmse.peso <- sqrt(mean(error.peso ** 2))
cat("RMSE:", rmse.peso, "\n\n")

# La raíz del error cuadrático medio para el modelo es de 4,028. Esto indica que
# los valores predichos se asemejan bastante a los valores observados, por lo
# que el modelo tiene una buena capacidad predictiva.

# Obtener residuos y estadísticas de influencia de los casos.
eval.rlm.peso <- data.frame(predicted.probabilities =
                              fitted(modelo.peso[["finalModel"]]))

eval.rlm.peso[["std.residuals"]] <- rstandard(modelo.peso[["finalModel"]])
eval.rlm.peso[["studentized.residuals"]] <-rstudent(modelo.peso[["finalModel"]])
eval.rlm.peso[["cooks.distance"]] <- cooks.distance(modelo.peso[["finalModel"]])
eval.rlm.peso[["dfbeta"]] <- dfbeta(modelo.peso[["finalModel"]])
eval.rlm.peso[["dffit"]] <- dffits(modelo.peso[["finalModel"]])
eval.rlm.peso[["leverage"]] <- hatvalues(modelo.peso[["finalModel"]])
eval.rlm.peso[["covariance.ratios"]] <- covratio(modelo.peso[["finalModel"]])

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rlm.peso[["std.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rlm.peso[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(datos.peso) / nrow(datos.peso)
sospechosos3 <- which(eval.rlm.peso[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rlm.peso[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio

sospechosos5 <- which(eval.rlm.peso[["covariance.ratios"]] < CVRi.lower |
                        eval.rlm.peso[["covariance.ratios"]] > CVRi.upper)

cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rlm.peso[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

cat("\nIndependencia de los residuos\n")
print(durbinWatsonTest(modelo.peso[["finalModel"]]))

# Puesto que la prueba de Durbin-Watson entrega p = 0,542, podemos concluir que
# los residuos son independientes.

# En consecuencia, podemos concluir que el modelo obtenido es confiable.



################################################################################
# Regresión lineal múltiple para la variable IMC.
################################################################################

# Descartar columnas inútiles
datos.imc <- muestra %>% select(-c(Weight, Height, EN))

# Separar variable de respuesta de los predictores.
IMC <- datos.imc[["IMC"]]
datos.imc[["IMC"]] <- NULL

# Ajustamos el modelo usando R^2 para seleccionar predictores y cinco
# repeticiones de validación cruzada de cinco pliegues.

# Caret implementa la regresión escalonada hacia atrás (bajo el nombre de
# Recursive Feature Elimination) mediante la función rfe().
# Se pueden definir alternativas de control que guíen la búsqueda, incluyendo
# funciones wrapper para el tipo de modelo. El paquete caret proporciona la
# función wrapper lmFuncs para modelos de regresión lineal.

control <- rfeControl(functions = lmFuncs, method="repeatedcv",
                      number=5, repeats=5, verbose = FALSE)

modelo.imc <- rfe(datos.imc, IMC, rfeControl = control, sizes = 10:20,
                  metric = "Rsquared")

print(modelo.imc)
cat("Variables seleccionadas:\n")
print(modelo.imc[["optVariables"]])

# Podemos ver que el modelo considera 14 variables: sexo, grosor del antebrazo,
# diámetro de las rodillas, diámetro de los tobillos, diámetro de las muñecas, 
# diámetro de los codos, diámetro biacromial, grosor mínimo de las muñecas, 
# grosor de los muslos, diámetro bitrocantéreo, grosor de los bíceps, 
# profundidad del pecho grosor de la cintura y grosor mínimo de los tobillos.

# Es posible ver gráficamente cómo varía el valor de R^2 en cada iteración.
print(ggplot(modelo.imc))

# El gráfico muestra que R^2 se maximiza para 14 variables, con un valor de
# 87,15% (con una raíz del error cuadrático medio de 1,422).

# En consecuencia, podemos concluir que el modelo obtenido se ajusta bien a los
# datos.



################################################################################
# Regresión logística múltiple para la variable estado nutricional.
################################################################################

# Descartar columnas inútiles
datos.en <- muestra %>% select(-c(IMC, Weight, Height))

# Separar variable de respuesta de los predictores.
EN <- datos.en[["EN"]]
datos.en[["EN"]] <- NULL


# Ajustamos el modelo usando la curva ROC para seleccionar predictores y
# validación cruzada dejando uno fuera.

# Caret implementa la regresión escalonada hacia atrás (bajo el nombre de
# Recursive Feature Elimination) mediante la función rfe().
# Se pueden definir alternativas de control que guíen la búsqueda, incluyendo
# funciones wrapper para el tipo de modelo. El paquete caret proporciona la
# función wrapper lrFuncs para modelos de regresión logística.
lrFuncs$summary <- twoClassSummary

control.seleccion <- rfeControl(functions = lrFuncs, method = "LOOCV",
                                number = 1, verbose = FALSE)

control.entrenamiento <- trainControl(method = "none", classProbs = TRUE,
                                      summaryFunction = twoClassSummary)

modelo.en <- rfe(datos.en, EN, metric = "ROC", rfeControl = control.seleccion,
                 trControl = control.entrenamiento, sizes = 2:6)

print(modelo.en)

# Podemos ver que el modelo considera 4 variables: diámetro biiliaco, grosor del
# antebrazo, diámetro de los codos y grosor de las caderas.

# Podemos ver gráficamente cómo varía el área bajo la curva ROC en cada
# iteración.
print(ggplot(modelo.en))

# El gráfico muestra que el área bajo la curva ROC se maximiza para 4 variables,
# con un valor de 94,12% (86,0% de sensibilidad y 80,0% de especificidad).

# Evaluar calidad predictiva del modelo.
predicciones <- predict(modelo.en, datos.en)[["pred"]]
cat("Calidad predictiva del modelo\n\n")
print(confusionMatrix(predicciones, EN))

# Podemos ver que el tiene una exactitud de 92,0%, por lo que se ajusta bien a
# los datos.
