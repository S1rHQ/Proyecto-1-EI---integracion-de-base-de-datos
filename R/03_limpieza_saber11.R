# =============================================================================
# 03_limpieza_saber11.R — Limpieza y estandarización de un periodo Saber 11
# =============================================================================

#' Normaliza texto: mayúsculas, sin tildes, sin espacios redundantes.
#' Se usa sobre variables categóricas (genero, naturaleza, área, etc.), NUNCA
#' sobre variables numéricas o identificadores.
normalizar_texto <- function(x) {
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # quita tildes/diacríticos
  x <- toupper(x)
  x[x %in% c("", "NA", "N/A", "NULL", "SIN DATO", "*", "-")] <- NA_character_
  x
}

#' Convierte a numérico de forma segura, dejando NA (con advertencia en log,
#' no con error) cuando el valor no es convertible.
a_numerico_seguro <- function(x, nombre_columna = "") {
  x_num <- suppressWarnings(as.numeric(x))
  n_perdidos <- sum(is.na(x_num) & !is.na(x) & x != "")
  if (n_perdidos > 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "{nombre_columna}: {n_perdidos} valor(es) no convertibles a numérico -> NA."))
  }
  x_num
}

# Variables numéricas de puntaje. Actualizado (2026-08-24) con los nombres
# CONFIRMADOS por el diccionario terminado: el esquema moderno de 5
# competencias (c_naturales, sociales_ciudadanas, lectura_critica,
# matematicas, ingles) es el nombre ESTÁNDAR en TODOS los periodos de
# Saber 11 -- ya no se usan los nombres viejos por asignatura individual.
VARIABLES_NUMERICAS_RESPALDO <- c(
  "punt_c_naturales", "punt_sociales_ciudadanas", "punt_lectura_critica",
  "punt_matematicas", "punt_ingles", "punt_global", "cole_codigo_icfes"
)
# NOTA (2026-08-24): fami_estratovivienda se retiró de la lista numérica.
# El diccionario dice "1 a 6, Sin estrato" -- viene como texto (ej. "Estrato
# 1", "Sin Estrato"), y forzarlo a as.numeric() volvía NA la enorme mayoría
# de los registros. Se conserva como texto y se deriva una versión numérica
# aparte (ver extraer_estrato_numerico más abajo).
#
# NOTA: estu_consecutivo también se retiró (es alfanumérico: "SB11..." o
# "EK...").

# Equipo (2026-08-25): las 5 variables recalificadas del periodo 2014-1
# (columnas recaf_punt_*) YA NO quedan como columnas propias -- se renombran
# hacia el nombre estándar equivalente ANTES de la selección defensiva, para
# que el valor caiga directo en la misma columna que usan todos los demás
# periodos. Es un renombrado directo (mismo concepto, "recalificado"), no
# una aproximación entre asignaturas distintas.
RECAF_A_ESTANDAR <- list(
  recaf_punt_c_naturales         = "punt_c_naturales",
  recaf_punt_sociales_ciudadanas = "punt_sociales_ciudadanas",
  recaf_punt_lectura_critica     = "punt_lectura_critica",
  recaf_punt_matematicas         = "punt_matematicas",
  recaf_punt_ingles              = "punt_ingles"
)

#' Extrae el número de estrato (1-6) de un texto tipo "Estrato 3", dejando NA
#' para "Sin estrato" u otros valores no numéricos. Conserva la columna de
#' texto original intacta -- esta es una variable DERIVADA adicional.
extraer_estrato_numerico <- function(x) {
  digitos <- stringr::str_extract(x, "[1-6]")
  suppressWarnings(as.numeric(digitos))
}

#' Limpia un único archivo/periodo de Saber 11.
#'
#' @param path Ruta al archivo plano del periodo.
#' @param periodo Código de periodo, ej. "20141".
#' @param diccionario Diccionario cargado con cargar_diccionario().
#' @param config Configuración cargada con cargar_config().
limpiar_saber11 <- function(path, periodo, diccionario, config) {

  vars_esperadas <- variables_esperadas_periodo(diccionario, "saber11", periodo)
  if (length(vars_esperadas) == 0) {
    # Sin información del diccionario para este periodo: se usa la lista base
    # de 25 variables como mejor esfuerzo.
    vars_esperadas <- VARIABLES_BASE_25
  }

  # Las columnas recaf_* (solo existen en el archivo de 2014-1) se renombran
  # a su equivalente estándar ANTES de la selección defensiva -- así el
  # valor cae directo en punt_c_naturales/etc., la misma columna que usan
  # todos los demás periodos, sin crear columnas nuevas.
  dt <- leer_archivo_defensivo(
    path = path,
    columnas_esperadas = vars_esperadas,
    separador = config$lectura$separador,
    encodings = config$lectura$encodings_a_probar,
    na_strings = config$lectura$na_strings,
    renombrar_antes = RECAF_A_ESTANDAR
  )

  if (nrow(dt) == 0) return(dt)

  # --- Tipificación ----------------------------------------------------
  cols_numericas <- intersect(names(dt), VARIABLES_NUMERICAS_RESPALDO)
  for (col in cols_numericas) {
    dt[, (col) := a_numerico_seguro(get(col), nombre_columna = col)]
  }

  cols_texto <- setdiff(names(dt), c(cols_numericas, "archivo_origen"))
  for (col in cols_texto) {
    dt[, (col) := normalizar_texto(get(col))]
  }

  if ("fami_estratovivienda" %in% names(dt)) {
    dt[, fami_estrato_num := extraer_estrato_numerico(fami_estratovivienda)]
  }

  # --- Metadatos de trazabilidad ----------------------------------------
  # IMPORTANTE: se usa ..periodo (no periodo) porque dt YA tiene una columna
  # llamada "periodo" (viene del archivo/diccionario). Sin el prefijo ..,
  # data.table busca "periodo" primero como columna de dt y NO como el
  # parámetro de esta función -- ese fue el bug que producía valores de
  # rezago sin sentido (miles en vez de semestres): el periodo real
  # ("20141", etc.) nunca se estaba guardando.
  dt[, periodo := ..periodo]
  dt[, prueba := "saber11"]

  dt[]
}
