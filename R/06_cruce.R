# =============================================================================
# 06_cruce.R — Cruce Saber11 -> Saber Pro vía tabla puente, con reglas de
#              consistencia temporal
# =============================================================================
# IMPORTANTE: los nombres de columnas de la tabla de cruces reales del
# ICFES/la universidad se leen desde config.yml (cruce$llave_*). Si la tabla
# real trae nombres distintos, se ajusta AHÍ, no en este script.

#' Convierte un código de periodo a un valor numérico ordenable ("año.semestre").
#' Soporta:
#'   - Saber 11: "20141" -> 2014.1, "20241" -> 2024.1
#'   - Saber Pro (dato solo con año): "2014" -> 2014.0 (se asume aplicación
#'     de mitad de año si no hay semestre; ajustar si el ICFES publica
#'     periodo con semestre para Saber Pro también)
periodo_a_valor <- function(periodo, prueba) {
  periodo <- as.character(periodo)
  if (prueba == "saber11") {
    anio <- as.numeric(substr(periodo, 1, 4))
    sem  <- as.numeric(substr(periodo, 5, 5))
    anio + sem / 10
  } else {
    as.numeric(periodo)
  }
}

#' Convierte un código de periodo a una aproximación en MESES desde el año 0
#' (útil para calcular rezago en meses/años, no solo para ordenar). Para
#' Saber 11 se usa el mes central de cada semestre (junio para el semestre 1,
#' diciembre para el semestre 2); para Saber Pro, del que solo se conoce el
#' año, se asume mitad de año (junio) como mejor aproximación -- AJUSTAR si
#' se llega a tener el mes/periodo exacto de aplicación de Saber Pro.
periodo_a_meses <- function(periodo, prueba) {
  periodo <- as.character(periodo)
  if (prueba == "saber11") {
    anio <- as.numeric(substr(periodo, 1, 4))
    sem  <- as.numeric(substr(periodo, 5, 5))
    mes_aprox <- ifelse(sem == 1, 6, 12)
    anio * 12 + mes_aprox
  } else {
    anio <- as.numeric(periodo)
    anio * 12 + 6
  }
}

#' Resuelve presentaciones múltiples de la misma prueba para un mismo
#' estudiante, según la regla de config.yml (cruce$regla_presentaciones_multiples).
resolver_presentaciones_multiples <- function(dt, llave, col_periodo_valor, regla) {
  data.table::setorderv(dt, c(llave, col_periodo_valor),
                         order = if (regla == "primera") 1L else -1L)
  dt[, .SD[1], by = llave]
}

#' Construye la base consolidada Saber11 x SaberPro.
#'
#' @param saber11_consolidado data.table con todos los periodos Saber 11 ya
#'   limpios y apilados (rbindlist).
#' @param saberpro_consolidado data.table con todos los periodos Saber Pro ya
#'   limpios y apilados.
#' @param tabla_cruces data.table de la tabla puente /data/raw/Cruces/, ya
#'   leída (sin procesar aún).
#' @param config Configuración global.
construir_cruce <- function(saber11_consolidado, saberpro_consolidado,
                             tabla_cruces, config) {

  cfg <- config$cruce

  # IMPORTANTE: data.table pasa objetos por REFERENCIA, no por copia. Sin
  # este copy(), las llamadas a `:=` y setorderv() de aquí abajo modificarían
  # "in situ" los objetos saber11_consolidado/saberpro_consolidado del
  # entorno que llamó a esta función (los mismos que ves en el panel
  # Environment de RStudio) -- fue justo eso lo que causó que, al inspeccionar
  # esas tablas después de correr el cruce, parecieran tener muchos menos
  # periodos de los reales: quedaban reordenadas por identificador de
  # estudiante en vez de por periodo. Con copy(), esta función trabaja sobre
  # su propia copia y los objetos originales del usuario quedan intactos.
  saber11_consolidado  <- data.table::copy(saber11_consolidado)
  saberpro_consolidado <- data.table::copy(saberpro_consolidado)

  # --- 1. Un registro por estudiante en cada prueba (Fase 4/2.1) ----------
  saber11_consolidado[, periodo_valor := periodo_a_valor(periodo, "saber11")]
  saberpro_consolidado[, periodo_valor := periodo_a_valor(periodo, "saberpro")]

  s11_unico <- resolver_presentaciones_multiples(
    saber11_consolidado, cfg$llave_saber11_en_datos, "periodo_valor",
    cfg$regla_presentaciones_multiples$saber11)
  spro_unico <- resolver_presentaciones_multiples(
    saberpro_consolidado, cfg$llave_saberpro_en_datos, "periodo_valor",
    cfg$regla_presentaciones_multiples$saberpro)

  n_s11_dupli  <- nrow(saber11_consolidado) - nrow(s11_unico)
  n_spro_dupli <- nrow(saberpro_consolidado) - nrow(spro_unico)
  registrar_log("INFO", glue::glue(
    "Presentaciones múltiples resueltas: {n_s11_dupli} en Saber11 (regla: ",
    "{cfg$regla_presentaciones_multiples$saber11}), {n_spro_dupli} en SaberPro ",
    "(regla: {cfg$regla_presentaciones_multiples$saberpro})."))

  # --- 2. Cruce vía tabla puente -------------------------------------------
  llave_s11_puente  <- cfg$llave_saber11_en_tabla_cruce
  llave_spro_puente <- cfg$llave_saberpro_en_tabla_cruce

  faltantes_puente <- setdiff(c(llave_s11_puente, llave_spro_puente), names(tabla_cruces))
  if (length(faltantes_puente) > 0) {
    stop(glue::glue(
      "La tabla de cruces no tiene las columnas esperadas: {paste(faltantes_puente, collapse=', ')}. ",
      "Ajustar config.yml -> cruce$llave_*_en_tabla_cruce según las columnas reales."))
  }

  puente <- unique(tabla_cruces[, c(llave_s11_puente, llave_spro_puente), with = FALSE])

  base <- merge(
    puente, s11_unico,
    by.x = llave_s11_puente, by.y = cfg$llave_saber11_en_datos,
    all.x = FALSE
  )
  base <- merge(
    base, spro_unico,
    by.x = llave_spro_puente, by.y = cfg$llave_saberpro_en_datos,
    all.x = FALSE,
    suffixes = c("_s11", "_spro")
  )

  registrar_log("INFO", glue::glue(
    "Registros emparejados antes de reglas de consistencia temporal: {nrow(base)}"))

  # --- 3. Reglas de consistencia temporal (sección 2.1 del enunciado) -----
  base[, orden_valido := periodo_valor_spro > periodo_valor_s11]

  n_orden_invalido <- sum(!base$orden_valido, na.rm = TRUE)
  if (n_orden_invalido > 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "{n_orden_invalido} registro(s) excluidos por orden temporal inválido ",
      "(Saber Pro no posterior al Saber 11)."))
  }
  base <- base[orden_valido == TRUE]

  # --- 4. Rezago entre pruebas (variable derivada obligatoria) ------------
  # En meses y en años (redondeado a 1 decimal), calculado a partir del mes
  # aproximado de cada periodo (ver periodo_a_meses arriba).
  base[, meses_s11  := periodo_a_meses(periodo_s11, "saber11")]
  base[, meses_spro := periodo_a_meses(periodo_spro, "saberpro")]
  base[, rezago_meses := meses_spro - meses_s11]
  base[, rezago_anios  := round(rezago_meses / 12, 1)]
  base[, c("meses_s11", "meses_spro") := NULL]

  base[, rezago_implausible := rezago_meses < cfg$rezago_meses_min |
                                 rezago_meses > cfg$rezago_meses_max]

  n_implausible <- sum(base$rezago_implausible, na.rm = TRUE)
  registrar_log("INFO", glue::glue(
    "Rezago calculado para {nrow(base)} registros (en meses y años). ",
    "{n_implausible} marcados como 'rezago_implausible' (rango configurado: ",
    "{cfg$rezago_meses_min}-{cfg$rezago_meses_max} meses). ",
    "NO se eliminan automáticamente -- requieren decisión y justificación del equipo ",
    "(ver sección 2.1 del enunciado)."))

  base[, orden_valido := NULL]

  # Métricas de diagnóstico que no forman parte de la base en sí, pero que
  # necesita el reporte de validación (07_validacion.R). Se guardan como
  # atributos para no cambiar el tipo de retorno de esta función (sigue
  # siendo un data.table normal en todo lo demás).
  data.table::setattr(base, "n_pares_puente", nrow(puente))
  data.table::setattr(base, "n_orden_invalido_excluidos", n_orden_invalido)
  data.table::setattr(base, "n_s11_presentaciones_multiples", n_s11_dupli)
  data.table::setattr(base, "n_spro_presentaciones_multiples", n_spro_dupli)

  base[]
}
