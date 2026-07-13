# extrafont: para poner la fuente de LaTeX en las gráficas
# caret: para el train-test y las matrices de confusión con sus métricas
# pROC: para las curvas ROC
paquetes <- c("ggplot2", "e1071", "extrafont", "caret", "pROC")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(ggplot2)
library(e1071)
library(extrafont)
library(caret)
library(pROC)

# Fijamos semilla para reproducibilidad
set.seed(28)

# funciones personalizadas para obtener métricas rendimientos de los modelos ----
evaluar_modelo <- function(modelo_svm,
                           datos_test,
                           variable_real,
                           clase_positiva) {
  predicciones <- predict(modelo_svm, datos_test)

  cm <- confusionMatrix(
    data = predicciones,
    reference = variable_real,
    positive = clase_positiva,
    mode = "everything"
  ) # así saca todas las métricas

  pred_roc <- predict(modelo_svm, datos_test, decision.values = TRUE)
  valores_decision <- as.numeric(attr(pred_roc, "decision.values"))

  curva <- roc(
    response = variable_real, predictor = valores_decision,
    levels = c("B", "M"), quiet = TRUE
  ) # quiet quita los avisos
  valor_auc <- as.numeric(auc(curva))

  tabla <- data.frame(
    Metrica = c(
      "Precisión",
      "Recall (Sensibilidad)",
      "Especificidad",
      "Área Bajo la Curva (AUC)"
    ), # son las variables que queremos
    Valor = round(c(
      cm$byClass["Pos Pred Value"],
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      valor_auc
    ), 4)
  )

  # Quitar los nombres de las filas que pone R por defecto
  rownames(tabla) <- NULL

  cat("\n MATRIZ DE CONFUSIÓN: \n")
  print(cm$table)
  cat("\n MÉTRICAS DE RENDIMIENTO: \n")

  tabla
}


evaluar_modelo_multiclase <- function(modelo_svm, datos_test, variable_real) {
  predicciones <- predict(modelo_svm, datos_test)

  cm <- confusionMatrix(
    data = predicciones,
    reference = variable_real,
    # así saca todas las métricas por clase
    mode = "everything"
  )

  # En el caso multiclase, byClass es una matriz así que calculamos las medias
  # de las métricas evaluadas sobre cada pareja de variables
  macro_prec <- mean(cm$byClass[, "Pos Pred Value"], na.rm = TRUE)
  macro_sens <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
  macro_spec <- mean(cm$byClass[, "Specificity"], na.rm = TRUE)

  tabla <- data.frame(
    Metrica = c(
      "Exactitud (Accuracy)",
      "Precisión",
      "Recall (Sensibilidad)",
      "Especificidad"
    ),

    # sprintf sirve para forzar que siempre dibuje 4 decimales, rellenando con ceros si hace falta
    Valor = sprintf("%.4f", c(
      cm$overall["Accuracy"],
      macro_prec,
      macro_sens,
      macro_spec
    ))
  )

  # Quitar los nombres de las filas que pone R por defecto
  rownames(tabla) <- NULL

  cat("\n MATRIZ DE CONFUSIÓN: \n")
  print(cm$table)
  cat("\n MÉTRICAS DE RENDIMIENTO: \n")

  tabla
}


evaluar_modelo_regresion <- function(modelo_svr, datos_test, variable_real) {
  predicciones <- predict(modelo_svr, datos_test)

  real <- as.numeric(variable_real)
  pred <- as.numeric(predicciones) # ponemos ambos vectores como numéricos

  rmse_test <- sqrt(mean((real - pred)^2))
  mae_test <- mean(abs(real - pred))

  ss_res <- sum((real - pred)^2)
  ss_tot <- sum((real - mean(real))^2)
  r2_test <- 1 - (ss_res / ss_tot)

  tabla <- data.frame(
    Metrica = c("RMSE", "R^2", "MAE"),
    Valor = round(c(rmse_test, r2_test, mae_test), 4)
  )

  cat("\nMÉTRICAS DE RENDIMIENTO:\n")
  tabla
}


# BREAST CANCER (binario)----

# no tienen títulos las variables
breast <- read.csv("data/wdbc.data", header = FALSE)

# quitamos la primera columna porque es el identificador de cada paciente
breast <- breast[, -1]

# Mean (m) es la media, SE el error estándar y w_(w) la media de los 3 valores más grandes
# es así porque cada individuo es una muestra de tejido donde hay varias células
colnames(breast) <- c(
  "Diagnosis",
  "m_radius",
  "m_texture",
  "m_perimeter",
  "m_area",
  "m_smoothness",
  "m_compactness",
  "m_concavity",
  "m_concave_points",
  "m_symmetry",
  "m_fractal_dimension",
  "SE_radius",
  "SE_texture",
  "SE_perimeter",
  "SE_area",
  "SE_smoothness",
  "SE_compactness",
  "SE_concavity",
  "SE_concave_points",
  "SE_symmetry",
  "SE_fractal_dimension",
  "w_radius",
  "w_texture",
  "w_perimeter",
  "w_area",
  "w_smoothness",
  "w_compactness",
  "w_concavity",
  "w_concave_points",
  "w_symmetry",
  "w_fractal_dimension"
)

breast$Diagnosis <- as.factor(breast$Diagnosis) # para que pueda clasificar bien


head(breast)

# La función createDataPartition mantiene las proporciones de Diagnosis
set.seed(28)
indices_breast <- createDataPartition(breast$Diagnosis, p = 0.70, list = FALSE)

breast_train <- breast[indices_breast, ]
breast_test <- breast[-indices_breast, ]

breast_train$Diagnosis <- as.factor(breast_train$Diagnosis)
breast_test$Diagnosis <- as.factor(breast_test$Diagnosis)

prop.table(table(breast$Diagnosis)) # Proporción original
prop.table(table(breast_train$Diagnosis)) # Proporción en train
prop.table(table(breast_test$Diagnosis)) # Proporción en test


## breast lineal----
set.seed(28)
param_breast_lineal <- tune(svm,
  Diagnosis ~ .,
  data = breast_train,
  kernel = "linear",
  scale = TRUE,
  ranges = list(cost = 10^c(-2:2))
)

summary(param_breast_lineal) # tomamos cost = 1

resultados_bl <- param_breast_lineal$performances
top_5_bl <- head(resultados_bl[order(resultados_bl$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL LINEAL): \n")
print(top_5_bl, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
breast_lineal <- svm(
  formula = Diagnosis ~ .,
  data = breast_train,
  kernel = "linear",
  cost = 1, scale = TRUE
)
summary(breast_lineal)

pct_sv_breast_lin <- (breast_lineal$tot.nSV / nrow(breast_train)) * 100
cat(
  "El SVM Lineal usa el",
  round(pct_sv_breast_lin, 2),
  "% de los pacientes del entrenamiento como vectores de soporte.\n"
)


## breast polinómico----
set.seed(28)
param_breast_polinomico <- tune(svm,
  Diagnosis ~ .,
  data = breast_train,
  kernel = "polynomial",
  scale = TRUE,
  coef0 = 1,
  ranges = list(
    cost = 10^c(-2:2),
    degree = c(2:4)
  )
)

summary(param_breast_polinomico) # tomamos c = 1, grado = 2 en vez de grado 4 ya
# que solo aumenta en 0.005 tanto error como la dispersion y se vuelve más simple

resultados_bp <- param_breast_polinomico$performances
top_5_bp <- head(resultados_bp[order(resultados_bp$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL POLINÓMICO): \n")
print(top_5_bp, row.names = TRUE) # esta tabla está incluida en el anexo del TFG


set.seed(28)
breast_polinomico <- svm(
  formula = Diagnosis ~ .,
  data = breast_train,
  kernel = "polynomial",
  cost = 1,
  degree = 2,
  coef0 = 1,
  scale = TRUE
)
summary(breast_polinomico)

pct_sv_breast_pol <- (breast_polinomico$tot.nSV / nrow(breast_train)) * 100
cat(
  "El SVM polinómico usa el",
  round(pct_sv_breast_pol, 2),
  "% de los pacientes del entrenamiento como vectores de soporte.\n"
)


## breast radial----
set.seed(28)
param_breast_radial <- tune(svm,
  Diagnosis ~ .,
  data = breast_train,
  kernel = "radial",
  scale = TRUE,
  ranges = list(
    cost = 10^c(-2:2),
    gamma = 10^c(-4:1)
  )
)

summary(param_breast_radial) # tomamos c = 10, gamma = 0.01

resultados_br <- param_breast_radial$performances
top_5_br <- head(resultados_br[order(resultados_br$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL RADIAL): \n")
print(top_5_br, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
breast_radial <- svm(
  formula = Diagnosis ~ .,
  data = breast_train,
  kernel = "radial",
  cost = 10,
  gamma = 0.01,
  scale = TRUE
)
summary(breast_radial)

pct_sv_breast_rad <- (breast_radial$tot.nSV / nrow(breast_train)) * 100
cat(
  "El SVM radial usa el",
  round(pct_sv_breast_rad, 2),
  "% de los pacientes del entrenamiento como vectores de soporte.\n"
)


## evaluar modelos----
evaluar_modelo(breast_lineal, breast_test, breast_test$Diagnosis, "M")
evaluar_modelo(breast_polinomico, breast_test, breast_test$Diagnosis, "M")
evaluar_modelo(breast_radial, breast_test, breast_test$Diagnosis, "M")


## curvas ROC----

pred_bl_test <- predict(breast_lineal, breast_test, decision.values = TRUE)
scores_bl <- attr(pred_bl_test, "decision.values")

pred_bp_test <- predict(breast_polinomico, breast_test, decision.values = TRUE)
scores_bp <- attr(pred_bp_test, "decision.values")

pred_br_test <- predict(breast_radial, breast_test, decision.values = TRUE)
scores_br <- attr(pred_br_test, "decision.values")

roc_bl <- roc(
  response = breast_test$Diagnosis,
  predictor = as.numeric(scores_bl),
  levels = c("B", "M")
)
roc_bp <- roc(
  response = breast_test$Diagnosis,
  predictor = as.numeric(scores_bp),
  levels = c("B", "M")
)
roc_br <- roc(
  response = breast_test$Diagnosis,
  predictor = as.numeric(scores_br),
  levels = c("B", "M")
)

auc_bl <- round(auc(roc_bl), 4)
auc_bp <- round(auc(roc_bp), 4)
auc_br <- round(auc(roc_br), 4)

pdf("figures/roc_breast.pdf", width = 6, height = 6) # para que guarde la imagen

par(family = "serif") # para que ponga una fuente parecida a la de latex


plot(roc_bl,
  col = "royalblue",
  lwd = 2,
  xlab = "1 - Especificidad",
  ylab = "Sensibilidad",
  legacy.axes = TRUE, # para que dibuje en el sentido correcto la curva
  cex.lab = 1.4,
  cex.axis = 1.2
) # agranda la letra
plot(roc_bp,
  col = "red",
  lwd = 2,
  add = TRUE
)
plot(roc_br,
  col = "green4",
  lwd = 2,
  add = TRUE
)

legend("bottomright",
  legend = c(
    paste("Kernel Lineal (AUC =", auc_bl, ")"),
    paste("Kernel Polinómico (AUC =", auc_bp, ")"),
    paste("Kernel Radial (AUC =", auc_br, ")")
  ),
  col = c("royalblue", "red", "green4"),
  lwd = 2,
  bty = "n", # )
  cex = 1.2, # tamaño del texto
  y.intersp = 1
)

dev.off()


# VEHICLE SILHOUETTES (multiclase)----

# el dataset venía en 9 archivos .dat distintos
archivos_dat <- list.files(
  path = "data",
  pattern = "\\.dat$",
  full.names = TRUE
)

# así leemos los 9 documentos, los cuales no tienen una primera fila con el título de las variables
lista_datos <- lapply(archivos_dat, read.table, header = FALSE)

# ahora los unimos todos en un solo dataframe
vehicles <- do.call(rbind, lista_datos)

# y le damos los nombres a cada variable
colnames(vehicles) <- c(
  "compactness",
  "circularity",
  "distance_circularity",
  "radius_ratio",
  "pr_axis_aspect_ratio",
  "max_length_aspect_ratio",
  "scatter_ratio",
  "elongateness",
  "pr_axis_rectangularity",
  "max_length_rectangularity",
  "scaled_variance_major_axis",
  "scaled_variance_minor_axis",
  "scaled_radius_gyration",
  "skewness_major_axis",
  "skewness_minor_axis",
  "kurtosis_minor_axis",
  "kurtosis_major_axis",
  "hollows_ratio",
  "Vehicle"
)

vehicles$Vehicle <- as.factor(vehicles$Vehicle) # variable objetivo

head(vehicles)

set.seed(28)
indices_vehicles <- createDataPartition(vehicles$Vehicle,
  p = 0.70,
  list = FALSE
)

vehicles_train <- vehicles[indices_vehicles, ]
vehicles_test <- vehicles[-indices_vehicles, ]

vehicles_train$Vehicle <- as.factor(vehicles_train$Vehicle)
vehicles_test$Vehicle <- as.factor(vehicles_test$Vehicle)

# Proporción original
prop.table(table(vehicles$Vehicle))
# Proporción en train (debería ser idéntica)
prop.table(table(vehicles_train$Vehicle))
# Proporción en test (debería ser idéntica)
prop.table(table(vehicles_test$Vehicle))


## vehicles lineal----
set.seed(28)
param_vehicles_lineal <- tune(svm,
  Vehicle ~ .,
  data = vehicles_train,
  kernel = "linear",
  scale = TRUE,
  ranges = list(cost = 10^c(-2:2))
)

summary(param_vehicles_lineal) # tomamos cost = 10

resultados_vl <- param_vehicles_lineal$performances
top_5_vl <- head(resultados_vl[order(resultados_vl$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL LINEAL): \n")
print(top_5_vl, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
vehicles_lineal <- svm(
  formula = Vehicle ~ .,
  data = vehicles_train,
  kernel = "linear",
  cost = 10,
  scale = TRUE
)
summary(vehicles_lineal)

pct_sv_vehicles_lin <- (vehicles_lineal$tot.nSV / nrow(vehicles_train)) * 100
cat(
  "El SVM Lineal usa el",
  round(pct_sv_vehicles_lin, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)


## vehicles polinomico----
set.seed(28)
param_vehicles_polinomico <- tune(svm,
  Vehicle ~ .,
  data = vehicles_train,
  kernel = "polynomial",
  scale = TRUE,
  coef0 = 1,
  ranges = list(
    cost = 10^c(-2:2),
    degree = c(2:4)
  )
)

summary(param_vehicles_polinomico) # tomamos cost = 10, degree = 2

resultados_vp <- param_vehicles_polinomico$performances
top_5_vp <- head(resultados_vp[order(resultados_vp$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL POLINÓMICO): \n")
print(top_5_vp, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
vehicles_polinomico <- svm(
  formula = Vehicle ~ .,
  data = vehicles_train,
  kernel = "polynomial",
  cost = 10,
  degree = 2,
  coef0 = 1,
  scale = TRUE
)
summary(vehicles_polinomico)

pct_sv_vehicles_pol <- (vehicles_polinomico$tot.nSV / nrow(vehicles_train)) * 100
cat(
  "El SVM Polinómico usa el",
  round(pct_sv_vehicles_pol, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)


## vehicles radial----
set.seed(28)
param_vehicles_radial <- tune(svm,
  Vehicle ~ .,
  data = vehicles_train,
  kernel = "radial",
  scale = TRUE,
  ranges = list(
    cost = 10^c(-2:2),
    gamma = 10^c(-4:1)
  )
)

summary(param_vehicles_radial) # tomamos cost = 100, gamma = 10^-2

resultados_vr <- param_vehicles_radial$performances
top_5_vr <- head(resultados_vr[order(resultados_vr$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL RADIAL): \n")
print(top_5_vr, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
vehicles_radial <- svm(
  formula = Vehicle ~ .,
  data = vehicles_train,
  kernel = "radial",
  cost = 100,
  gamma = 10^-2,
  scale = TRUE
)
summary(vehicles_radial)

pct_sv_vehicles_rad <- (vehicles_radial$tot.nSV / nrow(vehicles_train)) * 100
cat(
  "El SVM radial usa el",
  round(pct_sv_vehicles_rad, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)


## evaluar modelos----
evaluar_modelo_multiclase(vehicles_lineal, vehicles_test, vehicles_test$Vehicle)
evaluar_modelo_multiclase(vehicles_polinomico, vehicles_test, vehicles_test$Vehicle)
evaluar_modelo_multiclase(vehicles_radial, vehicles_test, vehicles_test$Vehicle)


# ABALONE (regresión)----

abalone <- read.csv("data/abalone.data", header = FALSE)

colnames(abalone) <- c(
  "Sex", "Length", "Diameter", "Height",
  "Whole_weight", "Shucked_weight",
  "Viscera_weight", "Shell_weight", "Rings"
)

abalone$Sex <- as.factor(abalone$Sex) # esta variable es categórica

set.seed(28)
indices_abalone <- createDataPartition(abalone$Rings, p = 0.70, list = FALSE)

abalone_train <- abalone[indices_abalone, ]
abalone_test <- abalone[-indices_abalone, ]

abalone_train$Sex <- as.factor(abalone_train$Sex)
abalone_test$Sex <- as.factor(abalone_test$Sex)

table(abalone$Rings)
prop.table(table(abalone$Rings))
prop.table(table(abalone_train$Rings))
prop.table(table(abalone_test$Rings))


## Histograma de la variable Rings----
grafica_hist_abalone <- ggplot(abalone, aes(x = Rings)) +
  # binwidth = 1 hace que cada barra represente exactamente 1 anillo
  geom_histogram(
    binwidth = 1,
    fill = rgb(0.2, 0.5, 0.8, alpha = 0.7),
    color = "white"
  ) +
  labs(
    x = "Número de Anillos",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray95"),
    # quitamos la cuadrícula vertical para que quede más limpio
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # para que la imagen sea transparente
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

print(grafica_hist_abalone)
ggsave("figures/histograma_abalone.png",
  plot = grafica_hist_abalone,
  width = 6, height = 4.5, units = "in", dpi = 300, bg = "transparent"
)


## abalone lineal----
set.seed(28)
param_abalone_lineal <- tune(svm,
  Rings ~ .,
  data = abalone_train,
  kernel = "linear",
  scale = TRUE,
  ranges = list(
    cost = 10^c(-1:2),
    epsilon = c(0.01, 0.05, 0.1, 0.5, 1)
  )
)

summary(param_abalone_lineal) # aunque gana la fila 16, la fila 15 no sube tanto
# el error (solo 0.003) ni la dispersión, además es más simple por tener menor coste
# NOS QUEDAMOS CON C = 10, epsilon = 0.5 (fila 15)

resultados_al <- param_abalone_lineal$performances
top_5_al <- head(resultados_al[order(resultados_al$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL LINEAL): \n")
print(top_5_al, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
abalone_lineal <- svm(
  formula = Rings ~ .,
  data = abalone_train,
  kernel = "linear",
  cost = 10,
  epsilon = 0.5,
  scale = TRUE
)
summary(abalone_lineal)

pct_sv_abalone_lin <- (abalone_lineal$tot.nSV / nrow(abalone_train)) * 100
cat(
  "La SVR Lineal usa el",
  round(pct_sv_abalone_lin, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)

predicciones_al <- predict(abalone_lineal, abalone_test)

datos_grafica_al <- data.frame(
  Real = abalone_test$Rings,
  Predicho = predicciones_al
)

grafica_al <- ggplot(datos_grafica_al, aes(x = Real, y = Predicho)) +
  geom_point(
    color = rgb(0.2, 0.5, 0.8),
    alpha = 0.4,
    shape = 16,
    size = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    x = "Anillos Reales",
    y = "Anillos Predichos"
  ) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 30)) +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 16), # tipografía de LaTeX
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),

    # para que se vea transparente el fondo
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

print(grafica_al)

ggsave("figures/abalone_lineal.png",
  plot = grafica_al,
  width = 6, height = 4.5, units = "in", dpi = 300,
  bg = "transparent"
) # esto último permite que se vea transparente


## abalone polinómico----

set.seed(28)
param_abalone_polinomico <- tune(svm,
  Rings ~ .,
  data = abalone_train,
  kernel = "polynomial",
  scale = TRUE,
  ranges = list(
    cost = 10^c(-1:2),
    degree = c(2:4),
    epsilon = c(0.01, 0.05, 0.1, 0.5, 1)
  )
)
# ha soltado 24 veces el warning de max number of iterations (tarda 30 min en compilar)

summary(param_abalone_polinomico) # C = 10, degree = 2, epsilon = 0.5

resultados_ap <- param_abalone_polinomico$performances
top_5_ap <- head(resultados_ap[order(resultados_ap$error), -2], 5)
# ese -2 evita mostrar la columna de coef0 que siempre la tomamos con el valor 1
cat("\n TOP 5 MODELOS (KERNEL POLINÓMICO): \n")
print(top_5_ap, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
abalone_polinomico <- svm(
  formula = Rings ~ .,
  data = abalone_train,
  kernel = "polynomial",
  cost = 10,
  coef0 = 1,
  degree = 2,
  epsilon = 0.5,
  scale = TRUE
)
summary(abalone_polinomico)

pct_sv_abalone_pol <- (abalone_polinomico$tot.nSV / nrow(abalone_train)) * 100
cat(
  "La SVR Polinómica usa el",
  round(pct_sv_abalone_pol, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)

predicciones_ap <- predict(abalone_polinomico, abalone_test)

datos_grafica_ap <- data.frame(
  Real = abalone_test$Rings,
  Predicho = predicciones_ap
)

grafica_ap <- ggplot(datos_grafica_ap, aes(x = Real, y = Predicho)) +
  geom_point(
    color = rgb(0.2, 0.5, 0.8),
    alpha = 0.4,
    shape = 16,
    size = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    x = "Anillos Reales",
    y = "Anillos Predichos"
  ) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 30)) +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 16), # tipografía de LaTeX
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),

    # para que se vea transparente el fondo
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

print(grafica_ap)

ggsave("figures/abalone_polinomico.png",
  plot = grafica_ap,
  width = 6, height = 4.5, units = "in", dpi = 300,
  bg = "transparent"
) # esto último permite que se vea transparente


## abalone radial----

set.seed(28)
param_abalone_radial <- tune(svm,
  Rings ~ .,
  data = abalone_train,
  kernel = "radial",
  scale = TRUE,
  ranges = list(
    cost = 10^c(-1:2),
    gamma = 10^c(-3:0),
    epsilon = c(0.01, 0.05, 0.1, 0.5, 1)
  )
)
# ha tardado unos 20 minutos y sin warnings

summary(param_abalone_radial) # C=10, gamma =0.1, epsilon = 0.5

resultados_ar <- param_abalone_radial$performances
top_5_ar <- head(resultados_ar[order(resultados_ar$error), ], 5)
cat("\n TOP 5 MODELOS (KERNEL RADIAL): \n")
print(top_5_ar, row.names = TRUE) # esta tabla está incluida en el anexo del TFG

set.seed(28)
abalone_radial <- svm(
  formula = Rings ~ .,
  data = abalone_train,
  kernel = "radial",
  cost = 10,
  gamma = 0.1,
  epsilon = 0.5,
  scale = TRUE
)
summary(abalone_radial)


pct_sv_abalone_rad <- (abalone_radial$tot.nSV / nrow(abalone_train)) * 100
cat(
  "La SVR Radial usa el",
  round(pct_sv_abalone_rad, 2),
  "% de los individuos del entrenamiento como vectores de soporte.\n"
)

predicciones_ar <- predict(abalone_radial, abalone_test)

datos_grafica_ar <- data.frame(
  Real = abalone_test$Rings,
  Predicho = predicciones_ar
)

grafica_ar <- ggplot(datos_grafica_ar, aes(x = Real, y = Predicho)) +
  geom_point(
    color = rgb(0.2, 0.5, 0.8),
    alpha = 0.4,
    shape = 16,
    size = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    x = "Anillos Reales",
    y = "Anillos Predichos"
  ) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 30)) +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 16), # tipografía de LaTeX
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),

    # para que se vea transparente el fondo
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

print(grafica_ar)

ggsave("figures/abalone_radial.png",
  plot = grafica_ar,
  width = 6, height = 4.5, units = "in", dpi = 300,
  bg = "transparent"
) # esto último permite que se vea transparente


## evaluar modelos----

evaluar_modelo_regresion(abalone_lineal, abalone_test, abalone_test$Rings)
evaluar_modelo_regresion(abalone_polinomico, abalone_test, abalone_test$Rings)
evaluar_modelo_regresion(abalone_radial, abalone_test, abalone_test$Rings)
