# extrafont: para poner la fuente de LaTeX en las gráficas
paquetes <- c("ggplot2", "e1071", "extrafont")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(ggplot2)
library(e1071)
library(extrafont)

set.seed(28) # para que siempre salga igual cada vez que compilamos

# colors() # descomentar para consultar los nombres de color disponibles en R

############################################################################### -
# HIPERPLANOS DISCRIMINANTES----
############################################################################### -

# Generar datos simulados de dos grupos G1 y G2

x1 <- rnorm(30, mean = 2, sd = 1)
y1 <- rnorm(30, mean = 2, sd = 1)
x2 <- rnorm(30, mean = 6, sd = 1)
y2 <- rnorm(30, mean = 6, sd = 1)
# están centrados en torno al (2,2) y el (6,6) con desviación típica de 1

datos1 <- data.frame(
  X1 = c(x1, x2), # son las coordenadas de los puntos
  X2 = c(y1, y2),
  Clase = factor(rep(c("G1", "G2"), each = 30))
)

# Definimos un hiperplano que los separe perfectamente,
# por ejemplo la ecuación x1 + x2 - 8 = 0, w=(1,1)^T y b=-8
# entonces x2 = -x1 + 8 (pendiente -1, ordenada 8)

grafica_hiperplano <- ggplot(datos1, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 3) + # puntos más grandes y claros
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) + # colores manualmente
  geom_abline(intercept = 8, slope = -1, color = "black", linewidth = 1) + # dibujar la recta (hiperplano)
  theme_bw() + # tener fondo blanco
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  ) # para que la cuadrícula no sea tan fina

ggsave("figures/hiperplano_discriminante.png", plot = grafica_hiperplano, width = 6, height = 4.5, units = "in")

############################################################################### -
# CLASIFICADOR DE MÁXIMO MARGEN----
############################################################################### -

set.seed(28)

x1 <- rnorm(30, 2, 1)
y1 <- rnorm(30, 2, 1)
x2 <- rnorm(30, 6, 1)
y2 <- rnorm(30, 6, 1)

datos2 <- data.frame(
  X1 = c(x1, x2),
  X2 = c(y1, y2),
  Clase = factor(rep(c("G1", "G2"), each = 30))
)

modelo <- svm(Clase ~ X1 + X2, data = datos2, kernel = "linear", cost = 1000, scale = FALSE)
# ajustamos el modelo para obtener el máximo margen real

w <- t(modelo$coefs) %*% modelo$SV
b <- -modelo$rho
# obtenemos los parametros del hiperplano óptimo

pendiente <- -w[1] / w[2]
intercept <- -b / w[2]
margen_sup <- (-b + 1) / w[2]
margen_inf <- (-b - 1) / w[2]
# calculamos las rectas del hiperplano y los margenes tangentes

sv <- datos2[modelo$index, ]
# seleccionamos los vectores de soporte reales

norm_w2 <- sum(w^2)
sv$x_end <- sv$X1 - w[1] * (w[1] * sv$X1 + w[2] * sv$X2 + b) / norm_w2
sv$y_end <- sv$X2 - w[2] * (w[1] * sv$X1 + w[2] * sv$X2 + b) / norm_w2
# calculamos la proyeccion perpendicular de los vectores de soporte
# sobre el hiperplano para las flechas

x_vals <- seq(0, 8.5, length.out = 200)

datos2_sombreado <- data.frame(
  X1 = x_vals,
  Y_inf = pendiente * x_vals + margen_inf,
  Y_mid = pendiente * x_vals + intercept,
  Y_sup = pendiente * x_vals + margen_sup
)

grafica_max_margen <- ggplot() +
  geom_point(data = datos2, aes(x = X1, y = X2, color = Clase), size = 2.5) +
  geom_ribbon(
    data = datos2_sombreado, aes(x = X1, ymin = Y_inf, ymax = Y_mid),
    fill = "royalblue", alpha = 0.12
  ) +
  geom_ribbon(
    data = datos2_sombreado, aes(x = X1, ymin = Y_mid, ymax = Y_sup),
    fill = "red", alpha = 0.12
  ) +
  # sombreado suave entre el margen y el hiperplano

  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_segment(
    data = sv, aes(x = X1, y = X2, xend = x_end, yend = y_end),
    arrow = arrow(length = unit(0.35, "cm")), color = "grey20", inherit.aes = FALSE
  ) +

  # dibujamos las flechas de distancia al hiperplano (el margen m)
  geom_abline(intercept = intercept, slope = pendiente, color = "black", linewidth = 1.2) +
  geom_abline(intercept = margen_sup, slope = pendiente, color = "grey20", linetype = "dashed", linewidth = 0.7) +
  geom_abline(intercept = margen_inf, slope = pendiente, color = "grey20", linetype = "dashed", linewidth = 0.7) +

  # dibujamos el hiperplano solido y los margenes tangentes punteados
  coord_fixed(xlim = c(0, 8.5), ylim = c(-1, 8), expand = FALSE) + # para que salga bien
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/max_margen.png", plot = grafica_max_margen, width = 6, height = 4.5, units = "in")


############################################################################### -
# CASO NO SEPARABLE----
############################################################################### -

# generamos datos que se solapan intencionadamente
set.seed(28)
x1 <- rnorm(30, 3, 1.2)
y1 <- rnorm(30, 3, 1.2)
x2 <- rnorm(30, 5, 1.2)
y2 <- rnorm(30, 5, 1.2)

datos3 <- data.frame(
  X1 = c(x1, x2),
  X2 = c(y1, y2),
  Clase = factor(rep(c("G1", "G2"), each = 30))
)

# coste bajo (1) para permitir que los puntos violen el margen
modelo <- svm(Clase ~ X1 + X2, data = datos3, kernel = "linear", cost = 1, scale = FALSE)

w <- t(modelo$coefs) %*% modelo$SV
b <- -modelo$rho

# los vectores de soporte ahora incluyen a los que invaden el margen
sv <- datos3[modelo$index, ]
norm_w2 <- sum(w^2)
sv$x_end <- sv$X1 - w[1] * (w[1] * sv$X1 + w[2] * sv$X2 + b) / norm_w2
sv$y_end <- sv$X2 - w[2] * (w[1] * sv$X1 + w[2] * sv$X2 + b) / norm_w2

datos3_sombreado <- data.frame(
  X1 = x_vals,
  Y_inf = pendiente * x_vals + margen_inf,
  Y_mid = pendiente * x_vals + intercept,
  Y_sup = pendiente * x_vals + margen_sup
)

grafica_no_separable <- ggplot() +
  geom_point(data = datos3, aes(x = X1, y = X2, color = Clase), size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  # lineas estructurales
  coord_fixed(xlim = c(0, 8), ylim = c(0, 8), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/no_separable.png", plot = grafica_no_separable, width = 6, height = 4.5, units = "in")


############################################################################### -
# ALTA SENSIBILIDAD----
############################################################################### -

set.seed(28)
x1 <- rnorm(15, 2, 0.5)
y1 <- rnorm(15, 2, 0.5)
x2 <- rnorm(15, 5, 0.5)
y2 <- rnorm(15, 5, 0.5)

datos4 <- data.frame(
  X1 = c(x1, x2),
  X2 = c(y1, y2),
  Clase = factor(rep(c("G1", "G2"), each = 15))
)

modelo_base <- svm(Clase ~ X1 + X2, data = datos4, kernel = "linear", cost = 1000, scale = FALSE)
w_base <- t(modelo_base$coefs) %*% modelo_base$SV
b_base <- -modelo_base$rho
pend_base <- -w_base[1] / w_base[2]
int_base <- -b_base / w_base[2]

# añadimos un punto azul muy cerca del grupo rojo
datos_atipico <- rbind(datos4, data.frame(X1 = 3.5, X2 = 2.5, Clase = "G2"))

# nuevo modelo
modelo_sensible <- svm(Clase ~ X1 + X2, data = datos_atipico, kernel = "linear", cost = 1000, scale = FALSE)
w_sens <- t(modelo_sensible$coefs) %*% modelo_sensible$SV
b_sens <- -modelo_sensible$rho
pend_sens <- -w_sens[1] / w_sens[2]
int_sens <- -b_sens / w_sens[2]

grafica_izq <- ggplot(datos4, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_abline(intercept = int_base, slope = pend_base, color = "black", linewidth = 1.2) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

grafica_der <- ggplot(datos_atipico, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_abline(intercept = int_sens, slope = pend_sens, color = "black", linewidth = 1.2) + # Nueva recta
  geom_abline(intercept = int_base, slope = pend_base, color = "grey20", linetype = "dashed", linewidth = 0.8) + # Recta antigua
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/sensibilidad_izq.png", plot = grafica_izq, width = 4, height = 4)
ggsave("figures/sensibilidad_der.png", plot = grafica_der, width = 4, height = 4)


############################################################################### -
# CLASIFICADOR DE VECTORES DE SOPORTE----
############################################################################### -

set.seed(28)
x1 <- rnorm(30, 3, 1.2)
y1 <- rnorm(30, 3, 1.2)
x2 <- rnorm(30, 5, 1.2)
y2 <- rnorm(30, 5, 1.2)

datos5 <- data.frame(
  X1 = c(x1, x2),
  X2 = c(y1, y2),
  Clase = factor(rep(c("G1", "G2"), each = 30))
)

# modelo con margen ancho (mucha tolerancia a errores, bajo coste)
modelo_ancho <- svm(Clase ~ X1 + X2, data = datos5, kernel = "linear", cost = 0.05, scale = FALSE)
w_a <- t(modelo_ancho$coefs) %*% modelo_ancho$SV
b_a <- -modelo_ancho$rho
pend_a <- -w_a[1] / w_a[2]
int_a <- -b_a / w_a[2]
int_a_sup <- (-b_a + 1) / w_a[2]
int_a_inf <- (-b_a - 1) / w_a[2]

# modelo con margen estrecho (poca tolerancia, alto coste)
modelo_estrecho <- svm(Clase ~ X1 + X2, data = datos5, kernel = "linear", cost = 5, scale = FALSE)
w_e <- t(modelo_estrecho$coefs) %*% modelo_estrecho$SV
b_e <- -modelo_estrecho$rho
pend_e <- -w_e[1] / w_e[2]
int_e <- -b_e / w_e[2]
int_e_sup <- (-b_e + 1) / w_e[2]
int_e_inf <- (-b_e - 1) / w_e[2]

grafica_ancho <- ggplot(datos5, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_abline(intercept = int_a, slope = pend_a, color = "black", linewidth = 1.2) +
  geom_abline(intercept = int_a_sup, slope = pend_a, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_abline(intercept = int_a_inf, slope = pend_a, color = "black", linetype = "dashed", linewidth = 0.8) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

grafica_estrecho <- ggplot(datos5, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_abline(intercept = int_e, slope = pend_e, color = "black", linewidth = 1.2) +
  geom_abline(intercept = int_e_sup, slope = pend_e, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_abline(intercept = int_e_inf, slope = pend_e, color = "black", linetype = "dashed", linewidth = 0.8) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/margen_ancho.png", plot = grafica_ancho, width = 4, height = 4)
ggsave("figures/margen_estrecho.png", plot = grafica_estrecho, width = 4, height = 4)


############################################################################### -
# FRONTERA NO LINEAL----
############################################################################### -

set.seed(28)

n_rojos <- 45
n_azules <- 30

x1 <- runif(n_rojos, 0.5, 7) # distribución aleatoria uniforme
y1 <- 0.4 * (x1 - 4)^2 + 1 + rnorm(n_rojos, 0, 0.4)

x2 <- rnorm(n_azules, 4, 0.6)
y2 <- rnorm(n_azules, 3.5, 0.6)
# no hace falta modificar lo de arriba, con esos datos las fronteras salen muy bien


datos6 <- data.frame(
  X1 = c(x1, x2),
  X2 = c(y1, y2),
  Clase = factor(c(rep("G1", n_rojos), rep("G2", n_azules)))
)

grafica_no_lineal <- ggplot(datos6, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

print(grafica_no_lineal)


## kernel lineal----
modelo_lin <- svm(Clase ~ X1 + X2, data = datos6, kernel = "linear", cost = 1, scale = FALSE)
w_l <- t(modelo_lin$coefs) %*% modelo_lin$SV
b_l <- -modelo_lin$rho
pend_l <- -w_l[1] / w_l[2]
int_l <- -b_l / w_l[2]
int_l_sup <- (-b_l + 1) / w_l[2]
int_l_inf <- (-b_l - 1) / w_l[2]

grafica_lin <- ggplot(datos6, aes(x = X1, y = X2, color = Clase)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  geom_abline(intercept = int_l, slope = pend_l, color = "black", linewidth = 1.2) +
  geom_abline(intercept = int_l_sup, slope = pend_l, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_abline(intercept = int_l_inf, slope = pend_l, color = "black", linetype = "dashed", linewidth = 0.8) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )


ggsave("figures/klineal.png", plot = grafica_lin, width = 4, height = 4)


## kernel polinomico----

modelo_polinomico <- svm(Clase ~ .,
  data = datos6, kernel = "polynomial", degree = 2,
  cost = 10, coef0 = 1, decision.values = TRUE
)
# para el radial quitamos degree y ponemos gamma

resolucion <- 0.05
grid <- expand.grid(
  X1 = seq(0, 7, by = resolucion),
  X2 = seq(0, 7, by = resolucion)
)
# para que se vea la cuadrícula de fondo

# obtener los valores de decisión para las curvas de nivel
pred <- predict(modelo_polinomico, grid, decision.values = TRUE)
grid$z <- attr(pred, "decision.values")[, 1]

# creamos un dataframe solo con los puntos que son vectores de soporte
sv_polinomico <- datos6[modelo_polinomico$index, ]

grafica_polinomico <- ggplot() +
  geom_point(data = datos6, aes(x = X1, y = X2, color = Clase), size = 2.5) +
  # resaltamos los vectores de soporte
  geom_point(
    data = sv_polinomico, aes(x = X1, y = X2),
    color = "black", size = 3, shape = 1, stroke = 1
  ) +

  # frontera de decisión (curva de nivel 0)
  geom_contour(
    data = grid, aes(x = X1, y = X2, z = z),
    breaks = 0, color = "black", linewidth = 1
  ) +

  # líneas de margen (niveles 1 y -1)
  geom_contour(
    data = grid, aes(x = X1, y = X2, z = z),
    breaks = c(-1, 1), color = "black", linetype = "dashed", linewidth = 0.5
  ) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/kpolinomico.png", plot = grafica_polinomico, width = 4, height = 4)


## kernel gaussiano----

modelo_gaussiano <- svm(Clase ~ .,
  data = datos6, kernel = "radial", gamma = 0.3,
  cost = 10, coef0 = 1, decision.values = TRUE
)
# para el polinomico quitamos gamma y ponemos degree

resolucion <- 0.05
grid <- expand.grid(
  X1 = seq(0, 7, by = resolucion),
  X2 = seq(0, 7, by = resolucion)
)
# para que se vea la cuadrícula de fondo


# obtener los valores de decisión para las curvas de nivel
pred <- predict(modelo_gaussiano, grid, decision.values = TRUE)
grid$z <- attr(pred, "decision.values")[, 1]

# creamos un dataframe solo con los puntos que son vectores de soporte
sv_gaussiano <- datos6[modelo_gaussiano$index, ]

grafica_gaussiano <- ggplot() +
  geom_point(data = datos6, aes(x = X1, y = X2, color = Clase), size = 2.5) +

  # resaltamos los vectores de soporte
  geom_point(
    data = sv_gaussiano, aes(x = X1, y = X2),
    color = "black", size = 3, shape = 1, stroke = 1
  ) +

  # frontera de decisión (curva de nivel 0)
  geom_contour(
    data = grid, aes(x = X1, y = X2, z = z),
    breaks = 0, color = "black", linewidth = 1
  ) +

  # líneas de margen (niveles 1 y -1)
  geom_contour(
    data = grid, aes(x = X1, y = X2, z = z),
    breaks = c(-1, 1), color = "black", linetype = "dashed", linewidth = 0.5
  ) +
  scale_color_manual(values = c("G1" = "red", "G2" = "royalblue")) +
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), expand = FALSE) +
  theme_bw() +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/kgaussiano.png", plot = grafica_gaussiano, width = 4, height = 4)


############################################################################### -
# HINGE-LOSS----
############################################################################### -

hinge_loss <- function(z) {
  pmax(0, 1 - z)
}
error_cuadratico <- function(z) {
  (1 - z)^2
}
logistica <- function(z) {
  log(1 + exp(-z))
}

grafica_hingeloss <- ggplot(data.frame(z = c(-2.5, 3)), aes(z)) +
  # Ejes cartesianos
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  stat_function(fun = hinge_loss, aes(color = "Hinge Loss"), linewidth = 1.2) +
  stat_function(fun = error_cuadratico, aes(color = "Error Cuadrático"), linewidth = 1) +
  stat_function(fun = logistica, aes(color = "Regresión Logística"), linewidth = 1) +

  # para que se vea bien
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(0, 2)) +
  scale_color_manual(
    values = c(
      "Hinge Loss" = "red",
      "Regresión Logística" = "royalblue",
      "Error Cuadrático" = "green4"
    ),
    breaks = c("Hinge Loss", "Regresión Logística", "Error Cuadrático")
  ) +
  labs(
    x = expression(y %.% g(x)),
    y = "Penalización"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.3),
    text = element_text(family = "serif", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("figures/hingeloss.png", plot = grafica_hingeloss, width = 6, height = 4)


############################################################################### -
# TUBO SVR----
############################################################################### -

set.seed(28)

x_reg <- seq(0, 10, length.out = 40)
y_reg <- 0.6 * x_reg + 2 + rnorm(40, sd = 1.2) # ecuación lineal 0.6x + 2

datos7 <- data.frame(X = x_reg, Y = y_reg)

epsilon_val <- 1 # tubo de radio 1
modelo_svr <- svm(Y ~ X,
  data = datos7, kernel = "linear",
  cost = 10, epsilon = epsilon_val, scale = FALSE
) # incluimos epsilon

# f(x) = w^T*x + b
w_svr <- sum(modelo_svr$coefs * datos7$X[modelo_svr$index])
b_svr <- -modelo_svr$rho

# intercepts del tubo (f(x) + epsilon, f(x) - epsilon)
int_principal <- b_svr
int_sup <- b_svr + epsilon_val
int_inf <- b_svr - epsilon_val
pendiente <- w_svr

grafica_svr <- ggplot(datos7, aes(x = X, y = Y)) +
  geom_point(color = "gray30", size = 2) +

  # límites del tubo
  geom_abline(intercept = int_sup, slope = pendiente, color = "royalblue", linetype = "dashed", linewidth = 0.8) +
  geom_abline(intercept = int_inf, slope = pendiente, color = "royalblue", linetype = "dashed", linewidth = 0.8) +

  # recta de regresión
  geom_abline(intercept = int_principal, slope = pendiente, color = "royalblue", linewidth = 1.2) +

  # pintar vectores de soporte
  geom_point(
    data = datos7[modelo_svr$index, ], aes(x = X, y = Y),
    color = "red", size = 2, shape = 1, stroke = 1.2
  ) +
  coord_fixed(xlim = c(0, 12), ylim = c(0, 10)) + # fijar los límites del plot
  theme_bw() +
  labs(
    x = "Variable Predictora (X)",
    y = "Variable Respuesta (T)"
  ) +
  theme(
    text = element_text(family = "serif", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("figures/tubo_svr.png", plot = grafica_svr, width = 8, height = 4)
