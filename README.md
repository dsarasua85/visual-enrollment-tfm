# Visual Enrollment Recommender – TFM

Este repositorio contiene el código y los resultados agregados del Trabajo Fin de
Máster (TFM):

**Ajuste de parámetros de un sistema visual de recomendación de matrícula**  
Universitat Oberta de Catalunya (UOC) – Máster en Ciencia de Datos  
Autor: Diego Sarasúa
Tutor: Julià Minguillón (Grupo de investigación LAIKA)

---

## Descripción del proyecto

Este trabajo estudia y mejora un sistema visual de recomendación de matrícula que
apoya a los estudiantes en la elección de asignaturas en cada semestre del Grado
en Ingeniería Informática (GII) de la UOC.

El sistema representa las asignaturas en un mapa bidimensional en el que las
asignaturas cercanas son buenas candidatas para cursarse conjuntamente. Las distancias
entre asignaturas se calculan combinando métricas basadas en datos reales:

- **Dpop** – Popularidad y afinidad por co-matrícula  
- **Ddif** – Dificultad académica conjunta  
- **Dso** – Solapamiento temporal de actividades  
- **Dsem** – Compatibilidad histórica por semestre  

Este TFM amplía y evalúa desarrollos previos realizados por Rivas (2025) y Thorp (2025)
dentro del grupo de investigación LAIKA.

---

## Metodología

El proyecto implementa un pipeline completo y reproducible:

1. ETL y normalización de datos académicos  
2. Construcción de matrices de distancia (Dpop, Ddif, Dso)  
3. Combinación de métricas mediante pesos configurables  
4. Proyección 2D de asignaturas mediante Sammon mapping  
5. Simulación retrospectiva de procesos reales de matrícula  
6. Evaluación frente a un recomendador aleatorio como línea base  

Se consideran dos escenarios a la hora de realizar simulaciones:
- Simulaciones con notas conocidas (semestres pasados: 20241, 20242)  
- Simulaciones sin notas conocidas (semestre presente: 20251)

---

## Estructura del repositorio

analysis/ Scripts en R para generación de métricas y simulaciones
inst/data_files/ Datos de entrada (no incluidos)
inst/reports/ Resultados agregados de los experimentos
inst/figures/ Figuras generadas a partir de los análisis


---

## Disponibilidad de los datos

Este repositorio incluye todos los datos necesarios para reproducir los análisis
y evaluar el sistema de recomendación desarrollado en el TFM.

En particular, en inst/data_files/ se publican las matrices de popularidad (Dpop),
dificultad (Ddif), solapamiento (Dso), los pares de asignaturas y el fichero
Dabs_INFORMATICA.csv, que constituyen las entradas del sistema de recomendación.

En inst/reports/ se incluyen los resultados completos de las simulaciones, resúmenes
y rankings generados por el pipeline.

Si se dispone de los ficheros base utilizados para generar estas matrices, se puede
reproducir completamente el pipeline ejecutando los scripts del repositorio tras
copiarlos en: inst/data_files/

De este modo, el repositorio proporciona un entorno reproducible y verificable para
el análisis y la evaluación del sistema.

---

## Resultados incluidos

El repositorio incluye:
- Matrices Dpop, Ddif y Dso  
- Resúmenes agregados de las simulaciones por configuración  
- Métricas comparativas frente a un recomendador aleatorio  
- Figuras utilizadas en la memoria y en la presentación del TFM  

Estos ficheros permiten analizar y reproducir los resultados reportados en el trabajo.

---

## Uso

Este código se publica con fines académicos y de investigación.  
Si se utiliza o se extiende este trabajo, debe citarse el TFM correspondiente y la
línea de investigación del grupo LAIKA.

## Licencia

Este proyecto se publica bajo la licencia  
**Creative Commons Reconocimiento-NoComercial-CompartirIgual 3.0 España (CC BY-NC-SA 3.0 ES)**,  
en coherencia con la licencia del Trabajo Fin de Máster.

---

## Contacto

Diego Sarasúa
Julià Minguillón
Universitat Oberta de Catalunya (UOC)
