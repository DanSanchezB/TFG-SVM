# Máquinas de Vectores de Soporte (SVM)

Este repositorio contiene el código fuente y la memoria final de mi Trabajo de Fin de Grado centrado en las Máquinas de Vectores de Soporte. El proyecto profundiza en el funcionamiento matemático de este algoritmo y evalúa su rendimiento en diferentes problemas de aprendizaje automático supervisado.

## Resumen del proyecto

El trabajo está estructurado en dos grandes bloques:

Por un lado, el marco teórico explora los fundamentos analíticos de las SVM. El texto abarca desde la geometría de los hiperplanos discriminantes y los clasificadores de margen máximo hasta el uso de funciones kernel para proyectar datos en espacios de alta dimensionalidad. También se detalla la adaptación del algoritmo para problemas de clasificación multiclase y la Regresión de Vectores de Soporte o SVR.

Por otro lado, la fase práctica implementa estos conceptos teóricos para evaluar el comportamiento de los kernels lineal, polinómico y radial sobre conjuntos de datos reales.

## Archivos del repositorio

- codigo_practico.R: Script principal que incluye la carga de datos, el preprocesamiento, el entrenamiento y la evaluación de los modelos. Los algoritmos se han probado en tres escenarios distintos: diagnóstico de tumores mamarios para clasificación binaria, identificación de siluetas de vehículos para clasificación multiclase y estimación de la edad biológica en moluscos para regresión.

- imagenes.R: Código empleado para generar las gráficas de hiperplanos, márgenes y fronteras de decisión que ilustran las explicaciones de la memoria.

- TFG_DanielSánchez.pdf: Documento completo con el desarrollo teórico, la metodología aplicada y las conclusiones finales del análisis.

## Tecnologías y requisitos

El proyecto está desarrollado íntegramente en el lenguaje de programación R. 

Para poder ejecutar los scripts correctamente es necesario tener instalada la librería e1071, ya que proporciona la implementación base de los modelos SVM y SVR utilizados a lo largo del trabajo.
