# =============================================================================
# 07_validacion.R — Fase 5: validación de la base construida
# =============================================================================

#' Relaciona cada columna de la base final con su variable equivalente en el
#' diccionario y la prueba de la que proviene. Se usa SOLO para poder
#' distinguir "no preguntado en ese periodo" de "no respondido" en
#' resumen_faltantes(); las variables derivadas dentro de este mismo
#' pipeline (rezago, fami_estrato_num, archivo_origen, etc.) no
#' tienen equivalente 1:1 en el diccionario y quedan fuera a propósito --
#' para esas, resumen_faltantes() reporta el % de faltantes sin distinción,
#' dejándolo explícito en la columna `distincion_disponible`.
MAPEO_VARIABLE_DICCIONARIO <- list(
  estu_genero_s11           = list(prueba = "saber11",  variable = "estu_genero"),
  estu_genero_spro          = list(prueba = "saberpro", variable = "estu_genero"),
  estu_etnia_s11            = list(prueba = "saber11",  variable = "estu_etnia"),
  estu_etnia_spro           = list(prueba = "saberpro", variable = "estu_etnia"),
  estu_inst_departamento    = list(prueba = "saberpro", variable = "estu_inst_departamento"),
  estu_inst_municipio       = list(prueba = "saberpro", variable = "estu_inst_municipio"),
  cole_depto_ubicacion      = list(prueba = "saber11",  variable = "cole_depto_ubicacion"),
  cole_mcpio_ubicacion      = list(prueba = "saber11",  variable = "cole_mcpio_ubicacion"),
  cole_area_ubicacion       = list(prueba = "saber11",  variable = "cole_area_ubicacion"),
  cole_naturaleza           = list(prueba = "saber11",  variable = "cole_naturaleza"),
  cole_jornada              = list(prueba = "saber11",  variable = "cole_jornada"),
  cole_codigo_icfes         = list(prueba = "saber11",  variable = "cole_codigo_icfes"),
  inst_cod_institucion      = list(prueba = "saberpro", variable = "inst_cod_institucion"),
  punt_c_naturales          = list(prueba = "saber11",  variable = "punt_c_naturales"),
  punt_sociales_ciudadanas  = list(prueba = "saber11",  variable = "punt_sociales_ciudadanas"),
  punt_lectura_critica      = list(prueba = "saber11",  variable = "punt_lectura_critica"),
  punt_matematicas          = list(prueba = "saber11",  variable = "punt_matematicas"),
  punt_ingles               = list(prueba = "saber11",  variable = "punt_ingles"),
  punt_global_s11           = list(prueba = "saber11",  variable = "punt_global"),
  punt_global_spro          = list(prueba = "saberpro", variable = "punt_global"),
  mod_competen_ciudada_punt = list(prueba = "saberpro", variable = "mod_competen_ciudada_punt"),
  mod_comuni_escrita_punt   = list(prueba = "saberpro", variable = "mod_comuni_escrita_punt"),
  mod_ingles_punt           = list(prueba = "saberpro", variable = "mod_ingles_punt"),
  mod_razona_cuantitat_punt = list(prueba = "saberpro", variable = "mod_razona_cuantitat_punt"),
  mod_lectura_critica_punt  = list(prueba = "saberpro", variable = "mod_lectura_critica_punt")
)

#' Matriz de cobertura de cohortes: conteo de estudiantes emparejados por
#' cada combinación de periodo Saber11 x año SaberPro. Es el entregable de
#' validación central del proyecto.
matriz_cobertura_cohortes <- function(base_cruzada) {
  data.table::dcast(
    base_cruzada,
    periodo_s11 ~ periodo_spro,
    fun.aggregate = length,
    value.var = "periodo_s11"
  )
}

#' Tasa de vinculación desagregada por cohorte de Saber 11, clasificada por
#' régimen (los "tres regímenes" del enunciado):
#'   - "censura_por_la_derecha_probable": el periodo de Saber 11 es tan
#'     reciente que, sumándole el rezago mínimo plausible (config.yml ->
#'     cruce$rezago_meses_min), todavía no alcanzaría a aparecer en los
#'     datos de Saber Pro que se tienen disponibles -- una tasa baja ahí es
#'     ESPERADA, no un problema de calidad de datos.
#'   - "regimen_normal": el periodo ya tuvo tiempo suficiente para aparecer
#'     en Saber Pro dado el rezago mínimo configurado.
tasa_vinculacion_por_cohorte <- function(saber11_consolidado, base_cruzada, config) {
  total_por_periodo <- saber11_consolidado[, .(total_saber11 = .N), by = periodo]
  emparejados_por_periodo <- base_cruzada[, .(emparejados = .N), by = .(periodo = periodo_s11)]

  tasas <- merge(total_por_periodo, emparejados_por_periodo, by = "periodo", all.x = TRUE)
  tasas[is.na(emparejados), emparejados := 0]
  tasas[, tasa_vinculacion := round(emparejados / total_saber11, 4)]

  # Régimen esperado según el rezago mínimo configurado y el último periodo
  # de Saber Pro realmente disponible en los datos.
  ultimo_periodo_spro_valor <- max(periodo_a_valor(unique(base_cruzada$periodo_spro), "saberpro"),
                                    na.rm = TRUE)
  tasas[, periodo_valor := periodo_a_valor(periodo, "saber11")]
  tasas[, meses_hasta_ultimo_spro := (ultimo_periodo_spro_valor * 12 + 6) -
          periodo_a_meses(periodo, "saber11")]
  tasas[, regimen := ifelse(
    meses_hasta_ultimo_spro < config$cruce$rezago_meses_min,
    "censura_por_la_derecha_probable", "regimen_normal")]
  tasas[, c("periodo_valor", "meses_hasta_ultimo_spro") := NULL]

  data.table::setorder(tasas, periodo)
  tasas[]
}

#' % de faltantes por variable, distinguiendo -donde hay un mapeo conocido
#' al diccionario (ver MAPEO_VARIABLE_DICCIONARIO)- "no preguntado en ese
#' periodo" (el diccionario marca la variable como no disponible para ese
#' periodo/prueba) de "no respondido" (el diccionario la marca disponible,
#' pero el valor llegó vacío). Para variables sin mapeo conocido (derivadas
#' del propio pipeline), se reporta el % de faltantes sin esa distinción,
#' con distincion_disponible = FALSE.
resumen_faltantes <- function(base_cruzada, diccionario) {
  n <- nrow(base_cruzada)
  filas <- vector("list", ncol(base_cruzada))
  names(filas) <- names(base_cruzada)

  for (var in names(base_cruzada)) {
    es_na <- is.na(base_cruzada[[var]])
    n_faltantes <- sum(es_na)
    mapa <- MAPEO_VARIABLE_DICCIONARIO[[var]]

    if (!is.null(mapa)) {
      col_periodo <- if (mapa$prueba == "saber11") "periodo_s11" else "periodo_spro"
      periodos_disponibles <- unique(diccionario[
        prueba == mapa$prueba & variable == mapa$variable & disponible == TRUE, periodo])
      periodo_col <- as.character(base_cruzada[[col_periodo]])
      preguntado <- periodo_col %in% periodos_disponibles

      filas[[var]] <- data.table::data.table(
        variable = var,
        n_faltantes = n_faltantes,
        pct_faltantes = round(100 * n_faltantes / n, 2),
        n_no_preguntado = sum(es_na & !preguntado),
        n_no_respondido = sum(es_na & preguntado),
        distincion_disponible = TRUE
      )
    } else {
      filas[[var]] <- data.table::data.table(
        variable = var,
        n_faltantes = n_faltantes,
        pct_faltantes = round(100 * n_faltantes / n, 2),
        n_no_preguntado = NA_integer_,
        n_no_respondido = NA_integer_,
        distincion_disponible = FALSE
      )
    }
  }

  out <- data.table::rbindlist(filas, fill = TRUE)
  data.table::setorder(out, -pct_faltantes)
  out[]
}

#' Verificación de unicidad: debe haber un registro por estudiante en la
#' base final. Devuelve los duplicados residuales -- vacío es el resultado
#' CORRECTO si 06_cruce.R funcionó bien, porque resolver_presentaciones_
#' multiples() ya deja un solo registro por estudiante y prueba ANTES de
#' cruzar. El conteo explícito de "verificados" y "duplicados" (incluso
#' cuando es 0) queda en resumen_general.csv para que no quede ambiguo si
#' el archivo vacío es un resultado o un archivo que no se generó.
verificar_unicidad <- function(base_cruzada, llave) {
  conteo <- base_cruzada[, .N, by = llave]
  conteo[N > 1]
}

#' Distribución del rezago (en meses y en años): estadísticos descriptivos
#' + conteo de registros implausibles, para acompañar la columna
#' rezago_implausible de la base final.
rezago_distribucion <- function(base_cruzada) {
  data.table::data.table(
    unidad = c("meses", "anios"),
    n = c(sum(!is.na(base_cruzada$rezago_meses)), sum(!is.na(base_cruzada$rezago_anios))),
    minimo  = c(min(base_cruzada$rezago_meses, na.rm = TRUE), min(base_cruzada$rezago_anios, na.rm = TRUE)),
    p25     = c(quantile(base_cruzada$rezago_meses, 0.25, na.rm = TRUE), quantile(base_cruzada$rezago_anios, 0.25, na.rm = TRUE)),
    mediana = c(median(base_cruzada$rezago_meses, na.rm = TRUE), median(base_cruzada$rezago_anios, na.rm = TRUE)),
    media   = c(round(mean(base_cruzada$rezago_meses, na.rm = TRUE), 2), round(mean(base_cruzada$rezago_anios, na.rm = TRUE), 2)),
    p75     = c(quantile(base_cruzada$rezago_meses, 0.75, na.rm = TRUE), quantile(base_cruzada$rezago_anios, 0.75, na.rm = TRUE)),
    maximo  = c(max(base_cruzada$rezago_meses, na.rm = TRUE), max(base_cruzada$rezago_anios, na.rm = TRUE)),
    desv_estandar = c(round(sd(base_cruzada$rezago_meses, na.rm = TRUE), 2), round(sd(base_cruzada$rezago_anios, na.rm = TRUE), 2))
  )
}

#' Rangos de puntajes verificados contra los valores teóricamente posibles,
#' SENSIBLE AL PERIODO. Las escalas de calificación del ICFES cambiaron a lo
#' largo de los 12 años cubiertos (ver config.yml -> rangos_puntajes) -- un
#' registro solo se evalúa contra el rango del segmento cuyo periodo lo
#' cubre. Si ningún segmento cubre el periodo de un registro, se cuenta como
#' "sin_segmento_definido" en vez de marcarse falsamente como fuera de rango.
verificar_rangos_puntajes <- function(base_cruzada, rangos_por_variable) {
  resultados <- list()

  for (var in names(rangos_por_variable)) {
    if (!var %in% names(base_cruzada)) next
    segmentos <- rangos_por_variable[[var]]

    n <- nrow(base_cruzada)
    cubierto    <- logical(n)
    fuera_rango <- logical(n)
    valor       <- base_cruzada[[var]]

    for (seg in segmentos) {
      col_periodo <- if (identical(seg$prueba, "saber11")) "periodo_valor_s11" else "periodo_valor_spro"
      if (!col_periodo %in% names(base_cruzada)) next
      periodo_col <- base_cruzada[[col_periodo]]
      en_ventana <- !is.na(periodo_col) &
        periodo_col >= seg$periodo_desde & periodo_col <= seg$periodo_hasta
      cubierto <- cubierto | en_ventana
      fuera_rango <- fuera_rango |
        (en_ventana & !is.na(valor) & (valor < seg$min | valor > seg$max))
    }

    resultados[[var]] <- data.table::data.table(
      variable = var,
      n_evaluados = sum(cubierto & !is.na(valor)),
      n_fuera_de_rango = sum(fuera_rango, na.rm = TRUE),
      n_sin_segmento_definido = sum(!cubierto & !is.na(valor))
    )
  }
  data.table::rbindlist(resultados, fill = TRUE)
}

#' Corre todas las validaciones y escribe el reporte a disco.
generar_reporte_validacion <- function(saber11_consolidado, base_cruzada,
                                        llave, rangos_esperados, diccionario,
                                        config, dir_output) {
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)

  cobertura  <- matriz_cobertura_cohortes(base_cruzada)
  tasas      <- tasa_vinculacion_por_cohorte(saber11_consolidado, base_cruzada, config)
  faltantes  <- resumen_faltantes(base_cruzada, diccionario)
  duplicados <- verificar_unicidad(base_cruzada, llave)
  rangos     <- verificar_rangos_puntajes(base_cruzada, rangos_esperados)
  rezago     <- rezago_distribucion(base_cruzada)

  # Punto 1 y consolidado de los puntos 4 y 5 del checklist: números
  # centrales que responden "¿esta base es confiable?" de un vistazo, sin
  # tener que interpretar un archivo vacío (como duplicados_residuales.csv)
  # ni abrir cinco archivos distintos.
  resumen_general <- data.table::data.table(
    metrica = c(
      "n_registros_base_final",
      "n_variables_base_final",
      "n_pares_en_tabla_puente",
      "n_excluidos_por_orden_temporal_invalido",
      "n_estudiantes_saber11_con_mas_de_una_presentacion",
      "n_estudiantes_saberpro_con_mas_de_una_presentacion",
      "n_duplicados_residuales_en_base_final",
      "n_registros_con_rezago_implausible"
    ),
    valor = c(
      nrow(base_cruzada),
      ncol(base_cruzada),
      attr(base_cruzada, "n_pares_puente") %||% NA_integer_,
      attr(base_cruzada, "n_orden_invalido_excluidos") %||% NA_integer_,
      attr(base_cruzada, "n_s11_presentaciones_multiples") %||% NA_integer_,
      attr(base_cruzada, "n_spro_presentaciones_multiples") %||% NA_integer_,
      nrow(duplicados),
      sum(base_cruzada$rezago_implausible, na.rm = TRUE)
    )
  )

  data.table::fwrite(resumen_general, file.path(dir_output, "resumen_general.csv"), sep = ";")
  data.table::fwrite(cobertura,  file.path(dir_output, "matriz_cobertura_cohortes.csv"), sep = ";")
  data.table::fwrite(tasas,      file.path(dir_output, "tasa_vinculacion_por_cohorte.csv"), sep = ";")
  data.table::fwrite(faltantes,  file.path(dir_output, "resumen_faltantes.csv"), sep = ";")
  data.table::fwrite(duplicados, file.path(dir_output, "duplicados_residuales.csv"), sep = ";")
  data.table::fwrite(rangos,     file.path(dir_output, "verificacion_rangos.csv"), sep = ";")
  data.table::fwrite(rezago,     file.path(dir_output, "rezago_distribucion.csv"), sep = ";")

  registrar_log("INFO", glue::glue(
    "Reporte de validación escrito en {dir_output}. ",
    "Registros finales: {nrow(base_cruzada)}. ",
    "Duplicados residuales: {nrow(duplicados)} (0 es el resultado esperado). ",
    "Rezagos implausibles: {sum(base_cruzada$rezago_implausible, na.rm = TRUE)}."))

  list(resumen_general = resumen_general, cobertura = cobertura, tasas = tasas,
       faltantes = faltantes, duplicados = duplicados, rangos = rangos, rezago = rezago)
}
