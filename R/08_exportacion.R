# =============================================================================
# 08_exportacion.R — Exportación de la base final y su diccionario
# =============================================================================

#' Normaliza los nombres de columnas al estándar exigido.
normalizar_nombres_columnas <- function(dt) {
  nuevos <- names(dt) |>
    tolower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^a-z0-9_]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("_$")
  data.table::setnames(dt, names(dt), nuevos)
  dt
}

#' Reestructura la base final: unifica archivo_origen_s11/archivo_origen_spro
#' en una sola columna archivo_origen, elimina las columnas marcadas en
#' config.yml -> columnas_a_eliminar, y reordena según config.yml ->
#' orden_columnas_finales. Cualquier columna presente en la base que no esté
#' en el orden deseado queda al final (nunca se descarta en silencio).
estructurar_columnas_finales <- function(dt, orden_deseado, columnas_a_eliminar = character(0)) {

  if (all(c("archivo_origen_s11", "archivo_origen_spro") %in% names(dt))) {
    dt[, archivo_origen := paste(archivo_origen_s11, archivo_origen_spro, sep = " | ")]
    dt[, c("archivo_origen_s11", "archivo_origen_spro") := NULL]
  }

  presentes_a_eliminar <- intersect(columnas_a_eliminar, names(dt))
  if (length(presentes_a_eliminar) > 0) {
    dt[, (presentes_a_eliminar) := NULL]
    registrar_log("INFO", glue::glue(
      "Columnas eliminadas de la base final por pedido del equipo: ",
      "{paste(presentes_a_eliminar, collapse = ', ')}"))
  }

  orden_presente <- intersect(orden_deseado, names(dt))
  faltantes      <- setdiff(orden_deseado, names(dt))
  sobrantes      <- setdiff(names(dt), orden_deseado)

  if (length(faltantes) > 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "orden_columnas_finales pide columnas que no existen en la base (revisar ",
      "nombres en config.yml): {paste(faltantes, collapse = ', ')}"))
  }
  if (length(sobrantes) > 0) {
    registrar_log("INFO", glue::glue(
      "Columnas presentes en la base pero no incluidas en orden_columnas_finales ",
      "-- quedan al final del archivo, no se pierden: {paste(sobrantes, collapse = ', ')}"))
  }

  data.table::setcolorder(dt, c(orden_presente, sobrantes))
  dt[]
}

#' Exporta la base final consolidada a .csv (UTF-8, separador ";" declarado,
#' punto decimal).
exportar_base_final <- function(base_cruzada, path_salida) {
  base_cruzada <- normalizar_nombres_columnas(base_cruzada)
  dir.create(dirname(path_salida), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(
    base_cruzada, path_salida,
    sep = ";", dec = ".", bom = FALSE, encoding = "UTF-8"
  )
  registrar_log("INFO", glue::glue(
    "Base final exportada: {path_salida} ({nrow(base_cruzada)} registros, ",
    "{ncol(base_cruzada)} variables)."))
  invisible(path_salida)
}

#' Construye el diccionario de la base final (entregable 4): una fila por
#' variable con nombre, fuente, tipo, categorías, % de faltantes y
#' observaciones/transformaciones aplicadas.
exportar_diccionario_salida <- function(base_cruzada, diccionario_variables,
                                          path_salida) {
  n <- nrow(base_cruzada)
  filas <- lapply(names(base_cruzada), function(col) {
    meta <- metadatos_variable(diccionario_variables, col)
    pct_faltantes <- round(100 * sum(is.na(base_cruzada[[col]])) / n, 2)
    data.table::data.table(
      variable = col,
      tipo_r = class(base_cruzada[[col]])[1],
      tipo_diccionario = if (!is.null(meta)) meta$Tipo %||% NA_character_ else NA_character_,
      descripcion = if (!is.null(meta)) meta$Descripción_oficial %||% NA_character_ else NA_character_,
      categorias_valores = if (!is.null(meta)) meta$`Categorías / Valores` %||% NA_character_ else NA_character_,
      pct_faltantes = pct_faltantes,
      observaciones = if (!is.null(meta)) meta$`Observaciones / Cambios` %||% NA_character_ else NA_character_
    )
  })
  dicc_salida <- data.table::rbindlist(filas, fill = TRUE)
  dir.create(dirname(path_salida), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(dicc_salida, path_salida, sep = ";", encoding = "UTF-8")
  registrar_log("INFO", glue::glue("Diccionario de la base final exportado: {path_salida}"))
  invisible(path_salida)
}
