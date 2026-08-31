# =============================================================================
# 02_diccionario.R — Carga y consulta del diccionario de variables
# =============================================================================

#' Carga la hoja "Diccionario variables" y la deja en formato largo:
#' una fila por (variable, prueba, periodo, disponible).
#'
#' Esto evita tener que acordarse de nombres de columna como "Saber11_20141"
#' en el resto del código: basta con preguntar variables_esperadas_periodo().
cargar_diccionario <- function(path_xlsx) {

  if (!file.exists(path_xlsx)) {
    stop(glue::glue("No se encontró el diccionario en: {path_xlsx}"))
  }

  raw <- readxl::read_excel(path_xlsx, sheet = "Diccionario variables")
  data.table::setDT(raw)

  # La primera columna del Excel es una columna auxiliar sin nombre fijo
  # (a veces trae texto como "recaf_punt_c_naturales" o "saber 11" / "saber pro"
  # -- observado en el archivo actual). Se conserva como metadato, no se usa
  # para el cruce de disponibilidad.
  nombre_col_variable <- names(raw)[2]  # "Variable" está en la 2da columna
  data.table::setnames(raw, nombre_col_variable, "variable")
  raw[, variable := tolower(trimws(variable))]
  raw <- raw[!is.na(variable) & variable != ""]

  # Columnas de disponibilidad por periodo: siguen el patrón
  # Saber11_XXXXX / SaberPro_XXXX. Todo lo demás (Tipo, Descripción_oficial,
  # Categorías / Valores, Observaciones, Fuente) es metadato de la variable.
  cols_periodo <- grep("^(Saber11_|SaberPro_)", names(raw), value = TRUE)
  cols_meta    <- setdiff(names(raw), c(names(raw)[1], "variable", cols_periodo))

  largo <- data.table::melt(
    raw,
    id.vars = c("variable", cols_meta),
    measure.vars = cols_periodo,
    variable.name = "columna_periodo",
    value.name = "disponible"
  )

  largo[, prueba := ifelse(grepl("^Saber11_", columna_periodo), "saber11", "saberpro")]
  largo[, periodo := stringr::str_remove(columna_periodo, "^(Saber11_|SaberPro_)")]
  largo[, disponible := as.logical(disponible)]
  largo[is.na(disponible), disponible := FALSE]  # celda vacía = no confirmado -> se trata como no disponible

  largo[, columna_periodo := NULL]
  largo[]
}

#' Devuelve el vector de nombres de variable que el diccionario marca como
#' disponibles para una prueba y periodo dados. Si el diccionario aún no
#' tiene información para ese periodo (todo NA/FALSE), devuelve character(0)
#' y se registra una advertencia -- no es un error fatal, el equipo debe
#' completar el diccionario o el módulo tratará el periodo como "sin variables
#' conocidas" y las completará todas como NA.
variables_esperadas_periodo <- function(diccionario, prueba_, periodo_) {
  sub <- diccionario[prueba == prueba_ & periodo == periodo_ & disponible == TRUE]
  vars <- unique(sub$variable)
  if (length(vars) == 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "El diccionario no registra variables disponibles para {prueba_} {periodo_}. ",
      "Revisar docs/diccionario_variables.xlsx (documento aún incompleto)."))
  }
  vars
}

#' Metadatos de una variable (tipo, descripción, categorías, observaciones)
#' para armar el diccionario de la base final (entregable 4).
metadatos_variable <- function(diccionario, variable_) {
  cols_meta <- setdiff(names(diccionario), c("variable", "prueba", "periodo", "disponible"))
  sub <- diccionario[variable == tolower(variable_)]
  if (nrow(sub) == 0) return(NULL)
  unique(sub[, ..cols_meta])[1]
}
