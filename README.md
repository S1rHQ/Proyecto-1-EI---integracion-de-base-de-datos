# Integración de datos ICFES — Panel longitudinal Saber 11 → Saber Pro (2014–2025)

Proyecto 1 · Estadística Industrial · Universidad del Magdalena · 2026-II

## Qué hace este proyecto

Construye una base consolidada a nivel de estudiante que vincula los
resultados de **Saber 11** con los de **Saber Pro** para todos los pares
observables entre 2014 y 2025, con lectura defensiva, homologación entre
periodos, reglas de consistencia temporal, validación automática (incluida
la matriz de cobertura de cohortes) y exportación de la base final más su
diccionario. Además genera un analisis de sesgo de selección.

## Cómo ejecutar

1. Instalar R **4.6.1**.
2. Instalar paquetes (una sola vez):
   ```r
   install.packages(c("data.table", "arrow", "yaml", "readxl", "stringr",
                       "stringi", "purrr", "glue", "lubridate"))
   ```
3. Colocar los microdatos originales (no incluidos en este repositorio, ver
   abajo) en:
   - `data/raw/Saber11/` — un archivo por periodo
   - `data/raw/SaberPro/` — un archivo por año
   - `data/raw/Cruces/` — tabla(s) de equivalencia de llaves entre las dos pruebas
4. Colocar el diccionario de variables (`docs/diccionario_variables.xlsx`,
   ampliar a medida que se revisen cambien o agreguen variables).
5. Desde la raíz del proyecto - archivo "proyecto_icfes.Rproj"-:
   ```
   source("run_all.R", echo = TRUE)
   ```
6. La base final queda en `data/output/base_saber11_saberpro.csv`, el
   diccionario de salida en `data/output/diccionario_base_final.csv`, y el
   reporte de validación en `data/output/validacion/`.

**Separador de campo de todos los archivos planos de entrada:** `;`
**Separador de salida de la base final:** `;` (declarado explícitamente,
punto como separador decimal, UTF-8, nombres de variable en minúscula sin
tildes ni espacios).

## Incorporar un año nuevo (ej. Saber Pro 2026)

Editar **solo** `config.yml` → sección `periodos`, agregar el año/periodo,
colocar el archivo correspondiente en `data/raw/`, y volver a ejecutar
`Rscript run_all.R`. Los checkpoints de periodos ya procesados
(`data/interim/*.parquet`) se reutilizan automáticamente; solo se procesa lo
nuevo. No hay que tocar ningún script en `R/`.

## Entorno declarado

- R 4.6.1 (ver `sessionInfo()` al final de una ejecución completa; recomendado
  guardar la salida en `docs/sessionInfo.txt` antes de la entrega final).
- Equipo de ejecución de referencia: 16 GB RAM. El flujo procesa por periodo
  y libera memoria (`gc()`) entre ciclos; evitar cargar todos los años en
  memoria simultáneamente para archivos grandes.
- Semilla fija: definida en `config.yml` (`semilla_aleatoria`).

## Estructura de carpetas

```
config.yml                     Parámetros del proyecto (años, rutas, reglas)
run_all.R                      Script maestro
R/
  00_setup.R                   Paquetes, semilla, log
  01_io_lectura.R              Lectura defensiva (encoding, separador, columnas)
  02_diccionario.R              Carga y consulta del diccionario de variables
  03_limpieza_saber11.R        Limpieza específica Saber 11 (incl. caso 2014-1)
  04_limpieza_saberpro.R        Limpieza específica Saber Pro
  05_homologacion.R             Homologación de categorías y geografía DANE
  06_cruce.R                    Cruce vía tabla puente + reglas temporales
  07_validacion.R                Matriz de cobertura de cohortes y métricas
  08_exportacion.R               Exportación de base final y diccionario
data/
  raw/                          Microdatos originales (NO se versiona, ver .gitignore)
  interim/                      Checkpoints Parquet por periodo
  output/                       Base final, diccionario de salida, validación
logs/                           Registro de cada ejecución
docs/
  diccionario_variables.xlsx    Diccionario de variables
config/
  homologacion_categorias.csv   Tabla editable de equivalencias de categorías
  homologacion_geografia_dane.csv  Tabla editable de códigos DANE
```

## Análisis adicionales (fuera de run_all.R)

La carpeta `analisis/` guarda scripts de análisis estadístico que se corren
DESPUÉS de `run_all.R` (reutilizan lo que ya dejó en disco, no reprocesan
los microdatos). Se ejecutan aparte a propósito, para no mezclar la
construcción de la base con el análisis:

```r
source("analisis/02_sesgo_seleccion.R")
```

- **`02_sesgo_seleccion.R`** —:
  compara el perfil (estrato, género, tipo de colegio) de los estudiantes
  vinculados a Saber Pro contra los que no llegaron a presentarlo, con prueba
  chi-cuadrado de independencia y proporciones con intervalo de confianza.
  Resultados en `data/output/analisis/`.

## Decisiones metodológicas relevantes

- **Periodo 2014-1 (Saber 11):** el archivo trae las 5 competencias
  recalificadas bajo el nombre `recaf_punt_*`. Se renombran directamente al
  nombre estándar equivalente (`punt_c_naturales`, `punt_sociales_ciudadanas`,
  `punt_lectura_critica`, `punt_matematicas`, `punt_ingles`) antes de armar
  la base, así que el valor cae en la misma columna que usan todos los demás
  periodos — no quedan como columnas separadas.
- **Presentaciones múltiples:** si un estudiante presentó una prueba varias
  veces, se toma la **primera** presentación de Saber 11 y la **última** de
  Saber Pro (`config.yml` → `cruce` → `regla_presentaciones_multiples`).
- **Llave de cruce:** tabla puente en `data/raw/Cruces/`, columnas
  configurables en `config.yml` → `cruce`.
- **Rezago:** calculado en meses y años (`rezago_meses`, `rezago_anios`) a
  partir del mes aproximado de cada periodo (ver `periodo_a_meses()` en
  `R/06_cruce.R`). Los registros con rezago fuera del rango configurado se
  marcan (`rezago_implausible = TRUE`) pero no se eliminan automáticamente
- **Orden y columnas de la base final:** definidos explícitamente en
  `config.yml` → `orden_columnas_finales` / `columnas_a_eliminar`.
