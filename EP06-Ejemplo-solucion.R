
library(Hmisc)

# Ingresamos los datos.
datos <- data.frame(
  Especialidad = factor(c("Pediatría", "Obstetricia", "Dermatología",
                          "Psiquiatría", "M. Interna", "Oncología",
                          "Neurología", "Anestesiología", "Radiología")),
  Mujeres = c(54, 71, 35, 30, 45, 44, 56, 21, 17),
  Hombres = c(52, 66, 41, 42, 65, 62, 88, 40, 35)
)



################################################################################
# Estudios previos habían determinado que la proporción de autoras en la
# especialidad de psiquiatría era de 27%. ¿Respaldan estos datos tal estimación?
################################################################################

cat("\nPregunta 1\n")

# La pregunta corresponde a una inferencia de una proporción con una muestra, la
# que podemos enfrentar con el método de Wilson para una proporción.

# Comencemos por formular las hipótesis.
# H0: La proporción de autoras en el área de psiquiatría es 27% (p = 0,27).
# HA: La proporción de autoras en el área de psiquiatría no es 27% (p != 0,27).

# Definimos los valores conocidos.
valor.nulo <- 0.27
mujeres <- datos[["Mujeres"]][4]
hombres <- datos[["Hombres"]][4]
n <- mujeres + hombres

# Ahora verifiquemos las condiciones.
# No nos queda más que confiar en que los responsables del estudio fueron
# cuidadosos a este respecto y las observaciones son independientes entre sí.

# La condición de éxito fracaso establece que se espera observar al menos 10
# observaciones correspondientes a éxito (mujeres, en este caso) y al menos 10,
# correspondientes a fracasos (hombres).
exitos.esperados <- n * valor.nulo
fracasos.esperados <- n * (1 - valor.nulo)

cat("\nSe esperan", exitos.esperados, "mujeres y", fracasos.esperados,
    "hombres\n")

# Podemos ver que las cantidades esperadas de éxitos y fracasos superan con
# creces el límite inferior de 10.

# Una vez verificadas las condiciones, procedemos con la prueba. No tenemos
# motivos para ser más exigentes, por lo que fijaremos un nivel de significación
# (alfa) de 0,05.
prueba.1 <- prop.test(x = mujeres, n = n, p = valor.nulo,
                      alternative = "two.sided", conf.level = 0.95,
                      correct = FALSE)

print(prueba.1)

# El valor p obtenido, 0,00506, es mucho menor que el nivel de significación de
# 0,05, por lo que rechazamos la hipótesis nula en favor de la hipótesis
# alternativa. Así, podemos concluir, con 95% de confianza, que la proporción
# de autoras que publican artículos en el área de psiquiatría difiera de 27%.

cat("=======================================================================\n")



################################################################################
# Según estos datos, ¿es igual la proporción de autoras en las áreas de
# psiquiatría y neurología?
################################################################################

cat("\nPregunta 2\n")

# Esta nueva pregunta corresponde a una inferencia acerca de la diferencia entre
# dos proporciones. Una vez más podemos usar el método de Wilson, pero ahora
# para la diferencia entre dos proporciones.

# Comencemos por formular las hipótesis.
# H0: La proporción de autoras es la misma en las áreas de psiquiatría y
#     neurología (p1 - p2 = 0).
# HA: La proporción de autoras es distinta en las áreas de psiquiatría y
#     neurología (p1 - p2 != 0).

# Definimos los valores conocidos.
mujeres.psiquiatria <- datos[["Mujeres"]][4]
hombres.psiquiatria <- datos[["Hombres"]][4]
mujeres.neurologia <- datos[["Mujeres"]][7]
hombres.neurologia <- datos[["Hombres"]][7]
n.psiquiatria <- mujeres.psiquiatria + hombres.psiquiatria
n.neurologia <- mujeres.neurologia + hombres.neurologia

# Ahora verifiquemos las condiciones.
# No tenemos mayor información acerca de cómo se obtuvieron las muestras, pero
# podemos suponer que los autores del estudio inicial fueron rigurosos y que se
# cumple la condición de independencia.

# La condición de éxito fracaso establece que se espera observar al menos 10
# observaciones correspondientes a éxito (mujeres, en este caso) y al menos 10,
# correspondientes a fracasos (hombres) en cada una de las muestras, lo que
# podemos verificar fácilmente en los datos: en el caso de psiquiatría,
# tenemos 30 y 42 observaciones, y en neurología, 56 y 88.

# Una vez verificadas las condiciones, procedemos con la prueba. No tenemos
# motivos para ser más exigentes, por lo que fijaremos un nivel de significación
# (alfa) de 0,05.

prueba.2 <- prop.test(x = c(mujeres.psiquiatria, mujeres.neurologia),
                      n = c(n.psiquiatria, n.neurologia),
                      alternative = "two.sided", conf.level = 0.95,
                      correct = FALSE)

print(prueba.2)

# El valor p obtenido, 0,6942, es mucho mayor que el nivel de significación de
# 0,05, por lo que fallamos al rechazar la hipótesis nula. Concluimos entonces,
# con 95% de confianza, que la proporción de autoras que publican artículos en
# las áreas de psiquiatría y neurología es la misma.

cat("=======================================================================\n")



################################################################################
# Suponiendo que la diferencia en la proporción de autoras la especialidad de
# radiología y la de oncología es de 0,17. ¿A cuántos autores deberíamos
# monitorear para obtener un intervalo de confianza del 97,5% y poder
# estadístico de 85%, si se intenta mantener aproximadamente la misma proporción
# de gente estudiada en cada caso?
###############################################################################

# Esta pregunta nuevamente aborda la diferencia de proporciones entre dos
# grupos, pero ahora se solicita el tamaño de la muestra.

# Las hipótesis asociadas a esta pregunta son, (considerando que no nos dan
# información acerca de la dirección de la hipótesis alternativa):
# H0: Hay una diferencia de 17% en la proporción de autoras en las áreas de
#     radiología y oncología (|p1 - p2| = 0,17).
# HA: No hay una diferencia de 17% en la proporción de autoras en las áreas de
#     radiología y oncología (|p1 - p2| != 0,17).

# Veamos las proporciones observadas:
n.observado.radiologia <- datos[["Mujeres"]][9] + datos[["Hombres"]][9]
p.observada.mujeres.radiologia <- datos[["Mujeres"]][9] / n.observado.radiologia

n.observado.oncologia <- datos[["Mujeres"]][6] + datos[["Hombres"]][6]
p.observada.mujeres.oncologia <- datos[["Mujeres"]][6] / n.observado.oncologia

cat("\n")
cat("Casos observados en radiología:", n.observado.radiologia, "\n")

cat("Proporción observada de mujeres en radiología:",
    p.observada.mujeres.radiologia, "\n")

cat("\n")
cat("Casos observados en oncología:", n.observado.oncologia, "\n")

cat("Proporción observada de mujeres en oncología:",
    p.observada.mujeres.oncologia, "\n")

# Ahora intentemos definir proporciones "esperadas" que se acerquen 
# a las observadas, pero con la diferencia establecida en la hipótesis.
p.esperada.mujeres.radiologia <- .28
p.esperada.mujeres.oncologia <- .45
diferencia <- p.esperada.mujeres.radiologia - p.esperada.mujeres.oncologia

# Ahora calculamos el tamaño de las muestras, que evidentemente tienen tamaños
# diferentes.
alfa <- 0.025
poder <- 0.85
fraccion <- n.observado.radiologia / (n.observado.radiologia + n.observado.oncologia)

resultado <- bsamsize(p1 = p.esperada.mujeres.radiologia,
                      p2 = p.esperada.mujeres.oncologia,
                      fraction = fraccion,
                      alpha = alfa, power = poder)

cat("\nResultado de la función bsamsize():\n\n")
print(resultado)

# Calculamos ahora los tamaños de las muestras (que, obviamente, deben ser
# números enteros positivos).
n.radiologia <- ceiling(resultado[1])
n.oncologia <- ceiling(resultado[2])

cat("\nSe deben considerar", n.radiologia, "autores en radiología y",
    n.oncologia, "en oncología")
