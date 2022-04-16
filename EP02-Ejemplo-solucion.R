# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios prácticos 1-2022/EP02")

# Fijar una semilla para asegurar que los resultados sean reproducibles.
set.seed(87)

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Cargar datos.
datos <- read.csv2("EP02 Datos Casen 2017.csv", stringsAsFactors = TRUE)

# Obtener una muestra de 150 observaciones.
n_muestra <- 150
muestra <- sample_n(datos, n_muestra)

# Si reescalamos la variable ytot de $1 a $1.000, tendremos números más claros.
datos[["ytot"]] <- datos[["ytot"]] / 1000
muestra[["ytot"]] <- muestra[["ytot"]] / 1000



################################################################################
# ANÁLISIS DE FRECUENCIAS
# ¿Se encuestó más o menos la misma cantidad de gente en cada provincia de
# la RM?
################################################################################

cat("¿Se encuestó más o menos la misma cantidad de gente en cada provincia")
cat("de la RM?\n")

# Comencemos con la muestra.
# Contar cuántas observaciones hay por cada provincia.
freq_m <- as.data.frame(table(muestra[["provincia"]]))
colnames(freq_m) <- c("Provincia", "Frecuencia")
cat("\nEncuestados por provincia para la muestra\n")
print(freq_m)

# Construir gráfico de barras.
g1_1_m <- ggbarplot(freq_m, x = "Provincia", y = "Frecuencia",
                   label = TRUE, lab.pos = "out", lab.col = "black",
                   fill = "Provincia", palette = "jco",
                   title = "Encuestados en la RM por provincia",
                   subtitle = "Muestra")

g1_1_m <- g1_1_m + rotate_x_text(angle = 45)
print(g1_1_m)

# Otra opción puede ser un gráfico de torta.
g1_2_m <- ggpie(freq_m, x = "Frecuencia", label = "Frecuencia",
                lab.pos = "out", lab.col = "black", fill = "Provincia",
                color = "black", palette = "jco",
                title = "Encuestados en la RM por provincia")

print(g1_2_m)

# Los gráficos obtenidos muestran que la cantidad de encuestados no es la misma
# en cada provincia. De hecho, la cantidad de encuestados en la provincia de
# Santiago parece ser significativamente mayor. Si miramos las frecuencias,
# Santiago tuvo la mator cantidad de encuestados (116), seguida de Talagante
# ¡con apenas 12!

# La frecuencia es una medida útil para responder esta pregunta. Sin embargo,
# suele estudiarse la proporción, que corresponde a un estimador de la
# probabilidad.
proporciones_m <- freq_m[["Frecuencia"]] / n_muestra
i <- freq_m[["Provincia"]] == "Santiago"

prop_m <- data.frame(Provincia = c("Stgo.", "Otra"),
                           Proporcion = c(proporciones_m[i],
                                          sum(proporciones_m[!i]))
                           )

cat("\nProporción de encuestados en la provincia de Stgo. para la muestra\n")
print(prop_m)

# Las proporciones evidencian que más de 75% de los encuestados de la muestra
# son de la provincia de Santiago.

# Ahora veamos qué pasa con la población.
# Contar cuántas observaciones hay por cada provincia.
freq_p <- as.data.frame(table(datos[["provincia"]]))
colnames(freq_p) <- c("Provincia", "Frecuencia")
cat("\nEncuestados por provincia para la población\n")
print(freq_p)

# Construir gráfico de barras.
g1_1_p <- ggbarplot(freq_p, x = "Provincia", y = "Frecuencia",
                    label = TRUE, lab.pos = "out", lab.col = "black",
                    fill = "Provincia", palette = "jco",
                    title = "Encuestados en la RM por provincia",
                    subtitle = "Población")

g1_1_p <- g1_1_p + rotate_x_text(angle = 45)
print(g1_1_p)

# Calcular las proporciones.
proporciones_p <- freq_p[["Frecuencia"]] / nrow(datos)
i <- freq_p[["Provincia"]] == "Santiago"

prop_p <- data.frame(Provincia = c("Stgo.", "Otra"),
                     Proporcion = c(proporciones_p[i],
                                    sum(proporciones_p[!i]))
)

cat("\nProporción de encuestados en la provincia de Stgo. para la población\n")
print(prop_p)

# Crear figura con los gráficos de la muestra y de la población.
g1 <- ggarrange(g1_1_m, g1_1_p, ncol = 2, nrow = 1)
print(g1)

# Podemos ver que la muestra refleja de manera bastante cercana lo que ocurre en
# la población. Se mantiene que más de 75% de los encuestados son de la
# provincia de Santiago, aunque la proporción es ligeramente menor.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# ANÁLISIS DE UNA VARIABLE NUMÉRICA
# ¿Cómo podemos describir el ingreso de los chilenos de la RM?
################################################################################

cat("\n¿Cómo podemos describir el ingreso de los chilenos de la RM?\n")

# Como el ingreso se acerca bastante a ser una variable continua, podemos usar
# varios tipos diferentes de gráficos para explorarla.

# Comencemos con la muestra.

# Una buena alternativa puede ser un histograma.
g2_1_m <- gghistogram(muestra, x = "ytot", bins = 15, add = "mean",
                   rug = TRUE, color = "#6D9EC1", fill = "#BFD5E3",
                   title = "Ingreso en la RM", subtitle = "Muestra",
                   xlab = "Ingreso total (miles de pesos)", ylab = "Frecuencia")

g2_1_m <- ggpar(g2_1_m, x_lim = c(0, 5050))
print(g2_1_m)

# Otra opción más detallada puede ser un diagrama de puntos, aunque para ello
# tendremos que usar funciones más básicas del paquete ggplot2.
g2_2_m <- ggplot(muestra, aes(x = ytot))

g2_2_m <- g2_2_m + geom_dotplot(binwidth = 100, color = "#6D9EC1",
                                fill = "#BFD5E3")

g2_2_m <- g2_2_m + xlim(0, 5050)
g2_2_m <- g2_2_m + ggtitle("Ingreso en la RM", subtitle = "Muestra")
g2_2_m <- g2_2_m + xlab("Ingreso total (miles de pesos)")
g2_2_m <- g2_2_m + ylab("Densidad detallada")
g2_2_m <- g2_2_m + theme_pubr()
print(g2_2_m)

# Otra muy buena opción es usar un diagrama de densidad.
g2_3_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "#6D9EC1", fill = "#BFD5E3",
                    title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    xlab = "Ingreso total (miles de pesos)", ylab = "Densidad")

g2_3_m <- ggpar(g2_3_m, xlim = c(0, 5050))
print(g2_3_m)

# Aunque también podemos ocultar los detalles con un gráfico de cajas, al cual
# agregaremos un punto con la media. Necesitamos crear (y ocultar) un único
# grupo.
muestra[["grupo"]] <- 1

g2_4_m <- ggboxplot(muestra, x = "grupo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3", title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)")

g2_4_m <- g2_4_m + scale_y_continuous(limits = c(0, 5050))
g2_4_m <- ggpar(g2_4_m, ylab = FALSE, orientation = "horizontal")
g2_4_m <- g2_4_m + rremove("y.text")
g2_4_m <- g2_4_m + rremove("y.ticks")
print(g2_4_m)

# O podemos dar más detalle mostrando los puntos que generan la caja.
g2_5_m <- ggboxplot(muestra, x = "grupo", y = "ytot", add = "jitter",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3", title = "Ingreso en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)")

g2_5_m <- g2_5_m + scale_y_continuous(limits = c(0, 5050))
g2_5_m <- ggpar(g2_5_m, ylab = FALSE, orientation = "horizontal")
g2_5_m <- g2_5_m + rremove("y.text")
g2_5_m <- g2_5_m + rremove("y.ticks")
print(g2_5_m)

# Los gráficos nos muestran que el ingreso presenta una distribución asimétrica
# con valores atípicos muy altos (outliers). Esto hace que ledidas como la
# media se distorcionen, por lo que es mejor usar otras medidas de tendencia
# central más robustas, tales como la mediana, el rango intercuartil o la
# media truncada (en inglés, trimmed mean).

cat("\nMedidas de tendencia central para la muestra\n")
cat("Media:", mean(muestra[["ytot"]]), "\n")
cat("Mediana:", median(muestra[["ytot"]]), "\n")
cat("Rango intercuartil (IQR):", IQR(muestra[["ytot"]]), "\n")
cat("Media truncada (60% de los datos):", mean(muestra[["ytot"]], trim = 0.2))

# Podemos ver que la media muestral del ingreso es $589.357,4, aunque el 50% de
# las observaciones se encuentra por debajo de $ 371.666,5. Esto nos muestra que
# La mayoría de la población tiene ingresos muy bajos y que los valores atípicos
# ejercen una enorme influencia en la media.

# Ahora veamos qué pasa con la población.
# Construir gráfico de densidad.
g2_3_p <- ggdensity(datos, x = "ytot", add = "mean", rug = TRUE,
                    color = "#6D9EC1", fill = "#BFD5E3",
                    title = "Ingreso en la RM",
                    subtitle = "Población",
                    xlab = "Ingreso total (miles de pesos)", ylab = "Densidad")

g2_3_p <- ggpar(g2_3_p, xlim = c(0, 5050))
print(g2_3_p)

# Crear figura con los gráficos de la muestra y de la población.
g2 <- ggarrange(g2_3_m, g2_3_p, ncol = 2, nrow = 1)
print(g2)

cat("\n\nMedidas de tendencia central para la población\n")
cat("Media:", mean(datos[["ytot"]]), "\n")
cat("Mediana:", median(datos[["ytot"]]), "\n")
cat("Rango intercuartil (IQR):", IQR(datos[["ytot"]]), "\n")
cat("Media truncada (60% de los datos):", mean(datos[["ytot"]], trim = 0.2))
cat("\n\n")
cat("=========================================================================")
cat("\n")

# Las medidas robustas son más parecidas entre sí, para la muestra y la
# población que la media.

# Podemos ver que la media poblacional del ingreso es $614.722,4 (¡más alta que
# la de la muestra!), aunque el 50% de las observaciones se encuentra por debajo
# de $ 350.000 (¡más bajo que la muestra!). Esto nos evidencia que la mayoría de
# la población tiene ingresos muy bajos (aún más bajos de lo que anticipábamos a
# partir de la muestra) y que hay unos pocos privilegiados cuyos ingresos son
# desmedidamente altos.



################################################################################
# ANÁLISIS DE UNA VARIABLE NUMÉRICA EN GRUPOS DISTINTOS
# ¿Tienen hombres y mujeres ingresos similares?
################################################################################

cat("\n¿Tienen hombres y mujeres ingresos similares?\n")

# En este caso podemos usar los mismos gráficos que en el ejemplo anterior, pero
# mostrando cada grupo por separado a fin de poder comparar la "masa" o la "forma"
# de los datos. Para ello, usamos la variable categórica (o factor, en jerga
# estadística) para asignar los colores.

# Una vez más, comencemos con la muestra.
# Construyamos primero un gráfico de densidad.
g3_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "sexo", fill = "sexo",
                    title = "Ingreso en la RM para la muestra",
                    subtitle = "(distribuciones separadas por sexo)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g3_1_m <- ggpar(g3_1_m, xlim = c(0, 5050))
print(g3_1_m)

# También podemos hacer un gráfico de cajas.
g3_2_m <- ggboxplot(muestra, x = "sexo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "sexo", title = "Ingreso en la RM para la muestra",
                    subtitle = "(cajas y medias por sexo)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Sexo")

g3_2_m <- g3_2_m + scale_y_continuous(limits = c(0, 5050))
g3_2_m <- ggpar(g3_2_m, orientation = "horizontal")
print(g3_2_m)

# Los gráficos sugieren que las mujeres tienen ingresos algo más bajos que los
# hombres.

# Nos sirven en este caso los mismos estadísticos que en la pregunta anterior,
# pero ahora para cada grupo por separado. La asimertía en la distribución de
# los datos (gráfico de densidad), que también podemos ver como la presencia de
# valores atípicos (el gráfico de cajas), sugiere que usemos medidas de
# tendencia central más robustas que la media.
cat("\nMedidas de tendencia central de la muestra para hombres y mujeres\n")
cat("Media mujeres:", mean(muestra[muestra[["sexo"]] == "Mujer", "ytot"]), "\n")

cat("Media hombres:", mean(muestra[muestra[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("Mediana mujeres:", median(muestra[muestra[["sexo"]] == "Mujer", "ytot"]),
    "\n")

cat("Mediana hombres:", median(muestra[muestra[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("IQR mujeres:", IQR(muestra[muestra[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("IQR hombres:", IQR(muestra[muestra[["sexo"]] == "Hombre", "ytot"]), "\n")

# Al comparar el ingreso medio de hombres y mujeres, vemos que los primeros
# son bastante más altos, diferencia que se acrecenta si miramos las medianas.

# Veamos ahora qué ocurre con la población.
# Comencemos por el gráfico de cajas.
g3_2_p <- ggboxplot(datos, x = "sexo", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "sexo",
                    title = "Ingreso en la RM para la población",
                    subtitle = "(cajas y medias por sexo)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Sexo")

g3_2_p <- g3_2_p + scale_y_continuous(limits = c(0, 5050))
g3_2_p <- ggpar(g3_2_p, orientation = "horizontal")
print(g3_2_p)

# Obtenemos ahora las medidas de tendencia central.
cat("\nMedidas de tendencia central de la población para hombres y mujeres\n")
cat("Media mujeres:", mean(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("Media hombres:", mean(datos[datos[["sexo"]] == "Hombre", "ytot"]), "\n")
cat("Mediana mujeres:", median(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")

cat("Mediana hombres:", median(datos[datos[["sexo"]] == "Hombre", "ytot"]),
    "\n")

cat("IQR mujeres:", IQR(datos[datos[["sexo"]] == "Mujer", "ytot"]), "\n")
cat("IQR hombres:", IQR(datos[datos[["sexo"]] == "Hombre", "ytot"]), "\n")

# Generamos la figura con ambos gráficos.
g3 <- ggarrange(g3_2_m, g3_2_p, ncol = 2, nrow = 1)
print(g3)
cat("\n")
cat("=========================================================================")
cat("\n")

# Tristemente, una vez más vemos que, en la población, la diferencia entre los
# ingresos de hombres y mujeres es aún más acentuada que lo que vimos en la
# muestra.



################################################################################
# ANÁLISIS DE UNA VARIABLE NUMÉRICA EN GRUPOS DISTINTOS
# ¿Tienen las personas que viven en áreas rurales ingresos similares a quienes
# viven en áreas urbanas?
################################################################################

cat("\n¿Tienen las personas que viven en áreas rurales ingresos similares a ")
cat("quienes viven en áreas urbanas?\n")

# El análisis es el mismo que en la pregunta anterior.

# Una vez más, comencemos con la muestra.
# Construyamos primero un gráfico de densidad.
g4_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "zona", fill = "zona",
                    title = "Ingreso en la RM (muestra)",
                    subtitle = "(distribuciones separadas por zona)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g4_1_m <- ggpar(g4_1_m, xlim = c(0, 5050))
print(g4_1_m)

# Veamos ahora el gráfico de cajas.
g4_2_m <- ggboxplot(muestra, x = "zona", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (muestra)",
                    subtitle = "(cajas y medias por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_2_m <- g4_2_m + scale_y_continuous(limits = c(0, 5050))
g4_2_m <- ggpar(g4_2_m, orientation = "horizontal")
print(g4_2_m)

# Obtenemos ahora las medidas de tendencia central.
cat("\nMedidas de tendencia central de la muestra, zonas urbana y rural\n")
cat("Media urbana:", mean(muestra[muestra[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Media rural:", mean(muestra[muestra[["zona"]] == "Rural", "ytot"]), "\n")

cat("Mediana urbana:", median(muestra[muestra[["zona"]] == "Urbano", "ytot"]),
    "\n")

cat("Mediana rural:", median(muestra[muestra[["zona"]] == "Rural", "ytot"]),
    "\n")

cat("IQR urbana:", IQR(muestra[muestra[["zona"]] == "Urbano", "ytot"]), "\n")
cat("IQR rural:", IQR(muestra[muestra[["zona"]] == "Rural", "ytot"]), "\n")

# Resulta interesante ver que, si bien la media es más alta para zonas urbanas,
# la mediana es más elevada en las zonas rurales. Pero ¿qué tan confiables
# serán estos resultados?

# Agreguemos más detalles al gráfico de cajas.
g4_3_m <- ggboxplot(muestra, x = "zona", y = "ytot", add = "jitter",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (muestra)",
                    subtitle = "(cajas y puntos por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_3_m <- g4_3_m + scale_y_continuous(limits = c(0, 5050))
g4_3_m <- ggpar(g4_3_m, orientation = "horizontal")
print(g4_3_m)

# Nuestra muestra tiene muy pocos casos correspondientes a áreas rurales, por lo
# que no podemos asegurar que el comportamiento que vemos en los gráficos sea
# representativo de la población.

# Aunque rara vez tendremos tanta suerte, ¡esta vez conocemos la población!
# Creamos el gráfico de cajas.
g4_2_p <- ggboxplot(datos, x = "zona", y = "ytot", add = "mean",
                    add.params = list(color = "#FC4E07", fill = "#FC4E07"),
                    fill = "zona", title = "Ingreso en la RM (población)",
                    subtitle = "(cajas y medias por zona)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Zona")

g4_2_p <- g4_2_p + scale_y_continuous(limits = c(0, 5050))
g4_2_p <- ggpar(g4_2_p, orientation = "horizontal")
print(g4_2_p)

# Y ahora calculamoslas medidas de tendencia central.
cat("\nMedidas de tendencia central de la población, zonas urbana y rural\n")
cat("Media urbana:", mean(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Media rural:", mean(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")
cat("Mediana urbana:", median(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("Mediana rural:", median(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")
cat("IQR urbana:", IQR(datos[datos[["zona"]] == "Urbano", "ytot"]), "\n")
cat("IQR rural:", IQR(datos[datos[["zona"]] == "Rural", "ytot"]), "\n")

# Podemos observar una enorme diferencia etre los ingresos de zonas rurales y
# urbanas, y que el valor central para las zonas rurales no alcanza siquiera los
# $300.000.

# Generamos la figura con ambos gráficos.
g4 <- ggarrange(g4_2_m, g4_2_p, ncol = 2, nrow = 1)
print(g4)
cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# ANÁLISIS DE UNA VARIABLE NUMÉRICA EN GRUPOS DISTINTOS
# ¿Son similares los ingresos registrados en las diferentes provincias de la RM?
################################################################################

cat("\n¿Son similares los ingresos registrados en las diferentes provincias ")
cat("de la RM?\n")

# El análisis para responder esta pregunta es similar al de las preguntas
# precedentes, aunque ahora tenemos más de dos grupos. En consecuencia, nos
# centraremos únicamente en aquellos aspectos que aporten algo nuevo.

# Construyamos primero un gráfico de densidad para la muestra.
g5_1_m <- ggdensity(muestra, x = "ytot", add = "mean", rug = TRUE,
                    color = "provincia", fill = "provincia",
                    title = "Ingreso en la RM (muestra)",
                    subtitle = "(distribuciones separadas por provincia)",
                    xlab = "Ingreso total (miles de pesos)",
                    ylab = "Frecuencia")

g5_1_m <- ggpar(g5_1_m, xlim = c(0, 5050))
print(g5_1_m)

# Veamos ahora el gráfico de cajas.
g5_2_m <- ggboxplot(muestra, x = "provincia", y = "ytot", color = "provincia",
  title = "Muestra del ingreso en la Región Metropolitana",
  subtitle = "(cajas y puntos por provincia)",
  ylab = "Ingreso total (miles de pesos)", xlab = "Provincia")

g5_2_m <- g5_2_m + scale_y_continuous(limits = c(0, 5050))
g5_2_m <- ggpar(g5_2_m, legend = "right", legend.title = "Provincias de\nla RM")
print(g5_2_m)

# Pero una vez más debemos verificar la validez de la muestra.
g5_3_m <- ggboxplot(muestra, x = "provincia", y = "ytot", color = "provincia",
                    add = "jitter",
                    title = "Muestra del ingreso en la Región Metropolitana",
                    subtitle = "(cajas y puntos por provincia)",
                    ylab = "Ingreso total (miles de pesos)", xlab = "Provincia")

g5_3_m <- g5_3_m + scale_y_continuous(limits = c(0, 5050))
g5_3_m <- ggpar(g5_3_m, legend = "right", legend.title = "Provincias de\nla RM")
print(g5_3_m)

# En este caso puede ser arriesgado inferir con la muestra obtenida, pues
# hay grupos con muy pocas observaciones.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# ANÁLISIS DE DOS VARIABLES NUMÉRICAS
# ¿Tiene relación el ingreso con la riqueza del municipio donde se habita?
################################################################################

cat("\n¿Tiene relación el ingreso con la riqueza del municipio donde se ")
cat("habita?\n")

# En este caso, queremos ver si una variable numérica influye sobre otra
# variable (también numérica):
# - Si una variable crece, ¿la otra también?
# - O bien, si una bariable crece, ¿disminuye la otra?
# - O tal vez no hay relación, es decir, las variables son independientes.

# Una herramienta adecuada para abordar este tipo de preguntas es el gráfico de
# dispersión.

# Como siempre, comencemos por estudiar la muestra.
# Crear gráfico de dispersión.
g6_1_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_1_m <- g6_1_m + scale_y_continuous(limits = c(0, 5050))
print(g6_1_m)

# Parecería que hay una relación positiva, aunque leve.

# Agregamos una línea de tendencia para ver la relación de forma más clara.
g6_2_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_m <- g6_2_m + scale_y_continuous(limits = c(0, 5050))
print(g6_2_m)

# Ahora sí es claro que si una variable crece, la otra también lo hace. Aunque
# la relación no parece muy fuerte.

# Veamos ahora qué pasa con la población.
g6_2_p <- ggscatter(datos, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Población",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_p <- g6_2_p + scale_y_continuous(limits = c(0, 5050))
print(g6_2_p)

# En efecto, hay una relación leve.

cat("\n")
cat("=========================================================================")
cat("\n")



################################################################################
# ANÁLISIS DE DOS VARIABLES NUMÉRICAS EN GRUPOS DISTINTOS
# ¿Van los ingresos de los chilenos incrementándose con la experiencia y de
# forma similar entre hombres y mujeres?
################################################################################

cat("\n¿Van los ingresos de los chilenos incrementándose con la experiencia y ")
cat("de forma similar entre hombres y mujeres?\n")

# Procedemos en forma similar a la pregunta anterior, pero ahora separamos los
# diferentes grupos por colores.

# Construimos primero el gráfico de dispersión para la muestra.
g7_1_m <- ggscatter(muestra, x = "edad", y = "ytot", add = "reg.line",
                    add.params = list(color = "sexo"), color = "sexo",
                    fill = "sexo", title = "Relación ingreso - edad en la RM",
                    subtitle = "(Muestra, separada por sexo)",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Edad (años)")

g7_1_m <- g7_1_m + scale_y_continuous(limits = c(0, 5050))
print(g7_1_m)

# Parecería que, en el caso de las mujeres, no hay relación entre ambas
# variables. En el caso de los hombres, en cambio, el ingreso parece aumentar
# levemente con la edad.

# Veamos ahora qué pasa con la población.
g7_1_p <- ggscatter(datos, x = "edad", y = "ytot", add = "reg.line",
                    add.params = list(color = "sexo"), color = "sexo",
                    fill = "sexo", title = "Relación ingreso - edad en la RM",
                    subtitle = "(Población, separada por sexo)",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Edad (años)")

g7_1_p <- g7_1_p + scale_y_continuous(limits = c(0, 5050))
print(g7_1_p)

# Podemos ver que, a medida que aumentan los años de experiencia, los hombres
# ven sus ingresos incrementados, mientras que las mujeres no.
