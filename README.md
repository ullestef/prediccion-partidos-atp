# Predicción de Partidos ATP con Machine Learning

Modelo predictivo para el resultado de partidos de tenis ATP, con foco en resolver
un problema clásico de proyectos predictivos: la fuga de información (data leakage).

## Problema

El dataset original de partidos ATP trae "impreso" el resultado en su propia
estructura (columnas Winner/Loser), lo que genera fuga de información y modelos
con métricas infladas e inútiles en la práctica. El objetivo de este proyecto fue
rediseñar el dataset para que solo use información disponible **antes** de que el
partido ocurra, y así obtener un modelo predictivo real.

## Dataset

- 34 años de historia del circuito ATP masculino (1990–2024)
- +112.000 partidos
- Fuente: estadísticas históricas del circuito ATP

## Metodología

1. **Rediseño de la estructura de datos**: se transformaron las columnas
   Winner/Loser en posiciones neutras (`player_A` / `player_B`) para eliminar
   la fuga de información.
2. **Feature engineering**: reconstrucción de variables usando solo datos previos
   al partido — rankings, promedios móviles de los últimos 10 encuentros, presión
   en el servicio, head-to-head histórico.
3. **Modelado**: comparación de tres algoritmos de clasificación — Regresión
   Logística, SVM y XGBoost.
4. **Selección de variables**: Random Forest para el ranking de variables más
   relevantes.
5. **Validación**: validación cruzada, curvas ROC y calibración de probabilidades
   (Brier Score).

## Resultados

Sobre 19.304 partidos de test:

- **XGBoost** lideró con **66% de accuracy** y **AUC 0.72**
- La diferencia de ranking y la gestión de puntos de quiebre recientes fueron los
  predictores más fuertes
- El techo predictivo (~68-70%) confirma la estocasticidad intrínseca del deporte:
  ni el mejor modelo elimina la incertidumbre

## Estructura del repositorio

| Archivo | Contenido |
|---|---|
| `scripts/eliminacion_na.R` | Limpieza de valores faltantes |
| `scripts/transformacion_variables.R` | Feature engineering (rankings, promedios móviles, head-to-head) |
| `scripts/svm.R` | Modelo de Support Vector Machine |
| `scripts/regresion_logistica.R` | Modelo de Regresión Logística |
| `presentacion/Prediccion_Partidos_ATP.pptx` | Presentación completa del proyecto: metodología de feature engineering, modelo XGBoost, comparación de métricas y conclusiones |

> **Nota**: el código de las etapas de preprocesamiento, SVM y Regresión Logística
> está disponible en este repositorio. La etapa de feature engineering avanzado y
> el modelo XGBoost (el de mejor desempeño) se documentan en la presentación
> adjunta.

## Herramientas

R · Regresión Logística · SVM · XGBoost · Random Forest · Validación Cruzada

## Contexto académico

Proyecto grupal desarrollado para la materia Ciencia de Datos para la Toma de
Decisiones, Facultad de Ingeniería, Universidad de Buenos Aires (UBA).
