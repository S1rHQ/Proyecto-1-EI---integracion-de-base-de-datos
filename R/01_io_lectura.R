# =============================================================================
# 01_io_lectura.R — Lectura defensiva de archivos planos ICFES
# =============================================================================


#' Detecta la codificación probable de un archivo probando una lista fija.
#' No se apoya en herramientas externas (no instalables aquí) — usa un
#' criterio simple: si la lectura de las primeras líneas produce caracteres
#' de reemplazo (\ufffd), se descarta esa codificación.
detectar_encoding <- function(path, encodings = c("UTF-8", "Latin1", "WINDOWS-1252")) {
  for (enc in encodings) {
    ok <- tryCatch({
      primeras_lineas <- readLines(path, n = 5, encoding = enc, warn = FALSE)
      !any(grepl("\ufffd", primeras_lineas, useBytes = TRUE))
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(enc)
  }
  # Si ninguna calza limpiamente, se usa la primera como mejor esfuerzo y se
  # deja constancia en el log — no se detiene la ejecución por esto.
  registrar_log("ADVERTENCIA", glue::glue(
    "No se pudo confirmar encoding para {basename(path)}; se usa {encodings[1]} por defecto."))
  encodings[1]
}

#' Encuentra los archivos de un periodo dado dentro de un directorio, según
#' el patrón definido en config.yml. Devuelve character(0) si no hay match
#' (no es un error: puede que ese periodo aún no esté publicado).
buscar_archivos_periodo <- function(directorio, periodo, patron, extensiones) {
  patron_regex <- patron |>
    stringr::str_replace("\\{periodo\\}", periodo) |>
    stringr::str_replace("\\*", ".*") |>
    stringr::str_replace("\\{ext\\}", paste0("(", paste(extensiones, collapse = "|"), ")"))
  archivos <- list.files(directorio, pattern = patron_regex, full.names = TRUE,
                          ignore.case = TRUE)
  archivos
}

#' Lectura defensiva de un único archivo plano.
#'
#' @param path Ruta al archivo.
#' @param columnas_esperadas Vector de nombres de columnas que el diccionario
#'   indica como válidas para este periodo/prueba.
#' @param separador Separador de campo (";" por defecto, según especificación).
#' @param renombrar_antes Lista nombrada opcional de renombres a aplicar
#'   ANTES de la selección defensiva (usada para el caso recaf_ 2014-1).
#' @return data.table con exactamente las columnas de `columnas_esperadas`
#'   (las ausentes en el archivo original quedan como NA), más metadatos.
leer_archivo_defensivo <- function(path, columnas_esperadas, separador = ";",
                                    encodings = c("UTF-8", "Latin1", "WINDOWS-1252"),
                                    na_strings = c("", "NA"),
                                    renombrar_antes = NULL) {

  enc <- detectar_encoding(path, encodings)

  dt <- tryCatch({
    data.table::fread(
      file = path,
      sep = separador,
      encoding = if (enc == "UTF-8") "UTF-8" else "unknown",
      na.strings = na_strings,
      colClasses = "character",   # se tipifica después, en el módulo de limpieza
      showProgress = FALSE
    )
  }, error = function(e) {
    registrar_log("ERROR", glue::glue("Fallo al leer {basename(path)}: {conditionMessage(e)}"))
    NULL
  })

  if (is.null(dt) || nrow(dt) == 0) {
    registrar_log("ADVERTENCIA", glue::glue("{basename(path)} vacío o ilegible; se omite."))
    return(data.table::data.table())
  }

  # Normalizar nombres de columnas del archivo (minúsculas, sin espacios)
  # para que el cruce contra el diccionario sea robusto a diferencias de
  # mayúsculas/minúsculas entre periodos.
  data.table::setnames(dt, tolower(trimws(names(dt))))

  # Renombres previos (caso recaf_ 2014-1 u otros casos futuros del mismo
  # tipo). OJO: si la columna destino YA EXISTE en el archivo (ej. 2014-1
  # trae "punt_ingles" original Y "recaf_punt_ingles" recalculada al mismo
  # tiempo), un simple setnames() deja DOS columnas con el mismo nombre en
  # la tabla -- y la selección posterior por nombre se queda con la primera
  # que encuentra (la vieja), descartando la recalificada sin avisar. Por
  # eso, cuando hay choque, se SOBRESCRIBE el destino con el valor de la
  # columna origen (se prioriza siempre el dato recalificado) y se elimina
  # la columna origen, en vez de renombrar a ciegas.
  if (!is.null(renombrar_antes) && length(renombrar_antes) > 0) {
    disponibles <- intersect(names(renombrar_antes), names(dt))
    for (col_origen in disponibles) {
      col_destino <- renombrar_antes[[col_origen]]
      if (col_destino %in% names(dt)) {
        dt[, (col_destino) := get(col_origen)]
        dt[, (col_origen) := NULL]
        registrar_log("INFO", glue::glue(
          "{basename(path)}: {col_destino} YA existía en el archivo junto con ",
          "{col_origen} -- se prioriza el valor recalificado ({col_origen}) y ",
          "se sobrescribe {col_destino}."))
      } else {
        data.table::setnames(dt, old = col_origen, new = col_destino)
      }
    }
    if (length(disponibles) > 0) {
      registrar_log("INFO", glue::glue(
        "{basename(path)}: aplicado renombrado/priorización de {length(disponibles)} ",
        "columna(s) ({paste(disponibles, collapse = ', ')})."))
    }
  }

  # --- Selección defensiva -------------------------------------------------
  columnas_esperadas <- tolower(columnas_esperadas)
  presentes <- intersect(columnas_esperadas, names(dt))
  ausentes  <- setdiff(columnas_esperadas, names(dt))

  if (length(ausentes) > 0) {
    registrar_log("INFO", glue::glue(
      "{basename(path)}: {length(ausentes)} variable(s) del diccionario no está(n) ",
      "en este archivo y se completan con NA -> {paste(ausentes, collapse = ', ')}"))
  }

  # IMPORTANTE: se fija n_filas ANTES de seleccionar columnas. Si en algún
  # momento `presentes` queda vacío (ninguna columna esperada existe en el
  # archivo), `dt[, ..presentes]` colapsa a una tabla de 0 columnas, y en
  # data.table eso se reporta con nrow() = 0 -- una asignación posterior con
  # `:=` sobre esa tabla vacía crea entonces UNA sola fila "fantasma" en vez
  # de conservar los registros reales. Construir dt_sel de forma explícita
  # con n_filas evita este colapso silencioso (se detectó en la corrida real
  # del 2026-08-24: varios periodos quedaron con 1 registro en vez de miles).
  n_filas <- nrow(dt)
  dt_sel <- data.table::data.table(matrix(NA_character_, nrow = n_filas,
                                           ncol = length(columnas_esperadas)))
  data.table::setnames(dt_sel, columnas_esperadas)
  for (col in presentes) data.table::set(dt_sel, j = col, value = dt[[col]])

  dt_sel[, archivo_origen := basename(path)]
  dt_sel[]
}
