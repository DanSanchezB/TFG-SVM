# Máquinas de Vectores de Soporte (SVM)

Este repositorio contiene el código fuente y la memoria final de mi Trabajo de Fin de Grado centrado en las Máquinas de Vectores de Soporte. El proyecto profundiza en el funcionamiento matemático de este algoritmo y evalúa su rendimiento en diferentes problemas de aprendizaje automático supervisado.

## Resumen del proyecto

El trabajo está estructurado en dos grandes bloques:

Por un lado, el marco teórico explora los fundamentos analíticos de las SVM. El texto abarca desde la geometría de los hiperplanos discriminantes y los clasificadores de margen máximo hasta el uso de funciones kernel para proyectar datos en espacios de alta dimensionalidad. También se detalla la adaptación del algoritmo para problemas de clasificación multiclase y la Regresión de Vectores de Soporte o SVR.

Por otro lado, la fase práctica implementa estos conceptos teóricos para evaluar el comportamiento de los kernels lineal, polinómico y radial sobre conjuntos de datos reales.

## Estructura del repositorio

```
data/      Conjuntos de datos empleados en la fase práctica
figures/   Gráficas generadas por los scripts e incluidas en la memoria
src/       Código fuente en R
thesis/    Memoria final del trabajo
```

- `src/main.R`: script principal de la fase práctica. Incluye la carga de datos, el preprocesamiento, el entrenamiento y la evaluación de los modelos sobre tres escenarios distintos: diagnóstico de tumores mamarios para clasificación binaria, identificación de siluetas de vehículos para clasificación multiclase y estimación de la edad biológica en moluscos para regresión.

- `src/plots.R`: código empleado para generar las gráficas de hiperplanos, márgenes, funciones kernel y fronteras de decisión que ilustran las explicaciones teóricas de la memoria.

- `thesis/TFG_DanielSánchez.pdf`: documento completo con el desarrollo teórico, la metodología aplicada y las conclusiones finales del análisis.

## Datos

Los tres conjuntos de datos provienen del [UCI Machine Learning Repository](https://archive.ics.uci.edu/):

| Fichero | Conjunto de datos | Problema |
|---|---|---|
| `data/wdbc.data` | Breast Cancer Wisconsin (Diagnostic) | Clasificación binaria |
| `data/xaa.dat` … `data/xai.dat` | Statlog (Vehicle Silhouettes) | Clasificación multiclase |
| `data/abalone.data` | Abalone | Regresión |

El conjunto de siluetas de vehículos se distribuye originalmente repartido en nueve ficheros `.dat`; `main.R` los lee y los combina en un único dataframe.

## Tecnologías y requisitos

El proyecto está desarrollado íntegramente en el lenguaje de programación R y depende de los siguientes paquetes:

- `e1071`: implementación base de los modelos SVM y SVR utilizados a lo largo del trabajo.
- `ggplot2`: generación de todas las gráficas.
- `caret`: particiones train-test y matrices de confusión con sus métricas.
- `pROC`: curvas ROC.
- `extrafont`: tipografía de LaTeX en las gráficas.

Ambos scripts instalan automáticamente los paquetes que falten al ejecutarse.

## Ejecución

Las rutas de los scripts son relativas a la raíz del proyecto, por lo que deben ejecutarse desde ella (y no desde `src/`):

```bash
Rscript src/main.R    # fase práctica: entrenamiento y evaluación de los modelos
Rscript src/plots.R   # gráficas teóricas de la memoria
```

Desde RStudio, basta con abrir el proyecto en la carpeta raíz antes de hacer *Source*.
