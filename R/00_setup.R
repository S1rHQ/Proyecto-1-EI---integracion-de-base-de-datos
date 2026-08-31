# =============================================================================
# 00_setup.R — Carga de paquetes, opciones globales y semilla
# =============================================================================
# Probado para R 4.6.1. Paquetes requeridos (instalar una sola vez):
#   install.packages(c("data.table","arrow","yaml","readxl","stringr",
#                       "stringi","purrr","glue","lubridate"))
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(yaml)
  library(readxl)
  library(stringr)
  library(stringi)
  library(purrr)
  library(glue)
  library(lubridate)
})

# data.table usa multithreading por defecto; con 16 GB de RAM y archivos
# grandes, limitar hilos evita picos de memoria simultáneos por columna.
data.table::setDTthreads(percent = 75)

cargar_config <- function(path = "config.yml") {
  if (!file.exists(path)) {
    stop(glue::glue("No se encontró config.yml en: {normalizePath(path, mustWork = FALSE)}"))
  }
  yaml::read_yaml(path)
}

crear_carpetas_proyecto <- function(config) {
  rutas <- unlist(config$rutas[c("raw_saber11", "raw_saberpro", "raw_cruces",
                                  "interim", "output", "logs")])
  for (r in rutas) {
    if (!dir.exists(r)) dir.create(r, recursive = TRUE)
  }
  invisible(TRUE)
}

fijar_semilla <- function(config) {
  set.seed(config$semilla_aleatoria %||% 20260824)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# Lista base de 25 variables, actualizada (2026-08-24) con los nombres
# CONFIRMADOS por el diccionario terminado (esquema moderno de 5
# competencias en vez de las asignaturas individuales viejas). Se usa como
# respaldo cuando el diccionario aún no tiene ninguna variable marcada como
# disponible para un periodo -- así no se procesa el archivo con una lista
# vacía o incorrecta de columnas esperadas.
VARIABLES_BASE_25 <- c(
  "estu_genero", "fami_estratovivienda", "periodo", "estu_consecutivo",
  "estu_areareside", "estu_etnia", "punt_c_naturales", "punt_sociales_ciudadanas",
  "punt_lectura_critica", "punt_matematicas", "punt_ingles", "punt_global",
  "mod_competen_ciudada_punt", "mod_comuni_escrita_punt", "mod_ingles_punt",
  "mod_razona_cuantitat_punt", "mod_lectura_critica_punt",
  "cole_area_ubicacion", "cole_depto_ubicacion", "cole_mcpio_ubicacion",
  "estu_inst_departamento", "estu_inst_municipio", "cole_naturaleza",
  "cole_codigo_icfes", "inst_cod_institucion", "cole_jornada"
)

# -----------------------------------------------------------------------------
# Log estructurado. Cada llamada agrega una línea a logs/ejecucion_<fecha>.log
# con timestamp, nivel y mensaje. No se sobreescribe entre corridas del mismo día.
# -----------------------------------------------------------------------------
.log_path <- NULL

iniciar_log <- function(config) {
  dir.create(config$rutas$logs, showWarnings = FALSE, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d")
  .log_path <<- file.path(config$rutas$logs, glue::glue("ejecucion_{ts}.log"))
  registrar_log("INFO", "===== Nueva ejecución iniciada =====")
  invisible(.log_path)
}

registrar_log <- function(nivel = c("INFO", "ADVERTENCIA", "ERROR"), mensaje) {
  nivel <- match.arg(nivel)
  linea <- glue::glue("[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] [{nivel}] {mensaje}")
  message(linea)
  if (!is.null(.log_path)) {
    cat(linea, "\n", file = .log_path, append = TRUE)
  }
  invisible(linea)
}
