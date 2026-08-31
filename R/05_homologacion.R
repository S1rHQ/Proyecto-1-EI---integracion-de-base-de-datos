# =============================================================================
# 05_homologacion.R — Homologación de categorías y geografía entre periodos
# =============================================================================

#' Carga (o crea una plantilla de) la tabla de homologación de categorías.
#' Formato esperado: variable, valor_origen, valor_homologado, periodo_origen (opcional)
cargar_homologacion_categorias <- function(path) {
  if (!file.exists(path)) {
    registrar_log("ADVERTENCIA", glue::glue(
      "No existe {path}; se crea plantilla vacía. Complétala a medida que ",
      "se detecten categorías que cambian de nombre entre periodos."))
    plantilla <- data.table::data.table(
      variable = character(), valor_origen = character(),
      valor_homologado = character(), periodo_origen = character()
    )
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(plantilla, path, sep = ";")
    return(plantilla)
  }
  data.table::fread(path, sep = ";", encoding = "UTF-8", colClasses = "character")
}

#' Aplica la tabla de homologación de categorías sobre un data.table ya
#' limpio (después de 03/04). Solo homologa las variables presentes en la
#' tabla; el resto queda intacto.
homologar_categorias <- function(dt, tabla_homologacion) {
  if (nrow(tabla_homologacion) == 0) return(dt)
  for (var in unique(tabla_homologacion$variable)) {
    if (!var %in% names(dt)) next
    mapa <- tabla_homologacion[variable == var]
    dt[get(var) %in% mapa$valor_origen,
       (var) := mapa$valor_homologado[match(get(var), mapa$valor_origen)]]
  }
  dt[]
}

#' Carga (o crea plantilla de) la tabla de homologación geográfica DANE.
#' Formato esperado: nombre_departamento_crudo, codigo_dane_depto,
#' nombre_municipio_crudo, codigo_dane_mpio
cargar_homologacion_geografia <- function(path) {
  if (!file.exists(path)) {
    registrar_log("ADVERTENCIA", glue::glue(
      "No existe {path}; se crea plantilla vacía. Completar con la codificación ",
      "oficial DANE (departamento/municipio) antes de la Fase 5 de validación."))
    plantilla <- data.table::data.table(
      nombre_departamento_crudo = character(), codigo_dane_depto = character(),
      nombre_municipio_crudo = character(), codigo_dane_mpio = character()
    )
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(plantilla, path, sep = ";")
    return(plantilla)
  }
  data.table::fread(path, sep = ";", encoding = "UTF-8", colClasses = "character")
}

#' Añade códigos DANE a las columnas de departamento/municipio de colegio e
#' institución, cuando la tabla de homologación esté disponible. Si una
#' combinación no está en la tabla, queda NA y se cuenta en el log (no se
#' detiene la ejecución: es información para la sección de calidad de datos).
homologar_geografia_dane <- function(dt, tabla_dane,
                                      col_departamento, col_municipio,
                                      prefijo_salida) {
  if (nrow(tabla_dane) == 0 || !col_departamento %in% names(dt)) return(dt)

  dt[tabla_dane, on = setNames(c("nombre_departamento_crudo", "nombre_municipio_crudo"),
                                c(col_departamento, col_municipio)),
     `:=`(
       codigo_dane_depto = i.codigo_dane_depto,
       codigo_dane_mpio  = i.codigo_dane_mpio
     )]

  data.table::setnames(dt,
    old = c("codigo_dane_depto", "codigo_dane_mpio"),
    new = paste0(prefijo_salida, c("_cod_dane_depto", "_cod_dane_mpio")))

  n_sin_match <- sum(is.na(dt[[paste0(prefijo_salida, "_cod_dane_depto")]]) &
                        !is.na(dt[[col_departamento]]))
  if (n_sin_match > 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "{prefijo_salida}: {n_sin_match} registro(s) sin código DANE homologado."))
  }
  dt[]
}
