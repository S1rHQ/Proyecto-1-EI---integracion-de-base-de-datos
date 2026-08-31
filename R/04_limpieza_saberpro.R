# =============================================================================
# 04_limpieza_saberpro.R — Limpieza y estandarización de un periodo Saber Pro
# =============================================================================

VARIABLES_NUMERICAS_SABERPRO <- c(
  "punt_global", "mod_competen_ciudada_punt", "mod_comuni_escrita_punt",
  "mod_ingles_punt", "mod_razona_cuantitat_punt", "mod_lectura_critica_punt",
  "inst_cod_institucion"
)
# Ver notas en 03_limpieza_saber11.R: estu_consecutivo (alfanumérico) y
# fami_estratovivienda (texto tipo "Estrato 3") se tratan como texto, no
# como número.

#' Limpia un único archivo/periodo de Saber Pro.
limpiar_saberpro <- function(path, periodo, diccionario, config) {

  vars_esperadas <- variables_esperadas_periodo(diccionario, "saberpro", periodo)
  if (length(vars_esperadas) == 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "Sin variables confirmadas en el diccionario para saberpro {periodo}; ",
      "se procesa igual usando la lista base de 25 variables como respaldo."))
    vars_esperadas <- VARIABLES_BASE_25
  }

  dt <- leer_archivo_defensivo(
    path = path,
    columnas_esperadas = vars_esperadas,
    separador = config$lectura$separador,
    encodings = config$lectura$encodings_a_probar,
    na_strings = config$lectura$na_strings
  )

  if (nrow(dt) == 0) return(dt)

  cols_numericas <- intersect(names(dt), VARIABLES_NUMERICAS_SABERPRO)
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

  dt[, periodo := ..periodo]  # ..periodo: ver nota en 03_limpieza_saber11.R
  dt[, prueba := "saberpro"]

  dt[]
}
