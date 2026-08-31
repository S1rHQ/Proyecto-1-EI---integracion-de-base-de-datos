# =============================================================================
# run_all.R — Script maestro. Punto de entrada único del proyecto.
# =============================================================================
# Ejecutar desde la raíz del proyecto (donde está este archivo y config.yml):
#     Rscript run_all.R
# o desde RStudio, con el proyecto (.Rproj) abierto, con Source este archivo.
#
# Para incorporar un año/periodo nuevo: SOLO editar config.yml -> periodos.
# No hay que tocar nada en R/.
# =============================================================================

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

config <- cargar_config("config.yml")
crear_carpetas_proyecto(config)
fijar_semilla(config)
iniciar_log(config)

registrar_log("INFO", glue::glue("Sesión: R {getRversion()}, plataforma {R.version$platform}"))

# -----------------------------------------------------------------------------
# 1. Diccionario de variables
# -----------------------------------------------------------------------------
diccionario <- cargar_diccionario(config$rutas$diccionario_xlsx)

# -----------------------------------------------------------------------------
# 2. Procesamiento por periodo, Saber 11 -- checkpoint en Parquet + gc()
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Fase Saber 11: lectura y limpieza por periodo =====")

for (periodo in config$periodos$saber11) {
  archivo_checkpoint <- file.path(config$rutas$interim, glue::glue("saber11_{periodo}.parquet"))
  if (file.exists(archivo_checkpoint)) {
    registrar_log("INFO", glue::glue("saber11 {periodo}: checkpoint ya existe, se omite."))
    next
  }

  archivos <- buscar_archivos_periodo(
    config$rutas$raw_saber11, periodo,
    config$patrones_archivo$saber11, config$patrones_archivo$extensiones_validas)

  if (length(archivos) == 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "saber11 {periodo}: no se encontró archivo en {config$rutas$raw_saber11}. ",
      "Puede ser un periodo aún no publicado (ver Fase 2 -- verificación de disponibilidad)."))
    next
  }

  dt_periodo <- limpiar_saber11(archivos[1], periodo, diccionario, config)
  if (nrow(dt_periodo) > 0) {
    arrow::write_parquet(dt_periodo, archivo_checkpoint)
    registrar_log("INFO", glue::glue("saber11 {periodo}: {nrow(dt_periodo)} registros -> {archivo_checkpoint}"))
  }
  rm(dt_periodo); gc(verbose = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Procesamiento por periodo, Saber Pro
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Fase Saber Pro: lectura y limpieza por periodo =====")

for (periodo in config$periodos$saberpro) {
  archivo_checkpoint <- file.path(config$rutas$interim, glue::glue("saberpro_{periodo}.parquet"))
  if (file.exists(archivo_checkpoint)) {
    registrar_log("INFO", glue::glue("saberpro {periodo}: checkpoint ya existe, se omite."))
    next
  }

  archivos <- buscar_archivos_periodo(
    config$rutas$raw_saberpro, periodo,
    config$patrones_archivo$saberpro, config$patrones_archivo$extensiones_validas)

  if (length(archivos) == 0) {
    registrar_log("ADVERTENCIA", glue::glue(
      "saberpro {periodo}: no se encontró archivo en {config$rutas$raw_saberpro}."))
    next
  }

  dt_periodo <- limpiar_saberpro(archivos[1], periodo, diccionario, config)
  if (nrow(dt_periodo) > 0) {
    arrow::write_parquet(dt_periodo, archivo_checkpoint)
    registrar_log("INFO", glue::glue("saberpro {periodo}: {nrow(dt_periodo)} registros -> {archivo_checkpoint}"))
  }
  rm(dt_periodo); gc(verbose = FALSE)
}

# -----------------------------------------------------------------------------
# 4. Consolidación (lectura de checkpoints, no re-procesamiento)
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Consolidación de periodos =====")

archivos_s11  <- list.files(config$rutas$interim, pattern = "^saber11_.*\\.parquet$", full.names = TRUE)
archivos_spro <- list.files(config$rutas$interim, pattern = "^saberpro_.*\\.parquet$", full.names = TRUE)

if (length(archivos_s11) == 0 || length(archivos_spro) == 0) {
  registrar_log("ERROR", "No hay checkpoints suficientes para consolidar. Revisar Fases 2-3.")
  stop("Ejecución detenida: faltan datos de entrada.")
}

saber11_consolidado  <- data.table::rbindlist(lapply(archivos_s11, arrow::read_parquet), fill = TRUE)
saberpro_consolidado <- data.table::rbindlist(lapply(archivos_spro, arrow::read_parquet), fill = TRUE)
data.table::setDT(saber11_consolidado); data.table::setDT(saberpro_consolidado)

registrar_log("INFO", glue::glue(
  "Consolidado: {nrow(saber11_consolidado)} registros Saber11, ",
  "{nrow(saberpro_consolidado)} registros SaberPro."))

# -----------------------------------------------------------------------------
# 5. Homologación de categorías y geografía
# -----------------------------------------------------------------------------
tabla_homologacion_cat <- cargar_homologacion_categorias(config$rutas$homologacion_categorias)
saber11_consolidado  <- homologar_categorias(saber11_consolidado, tabla_homologacion_cat)
saberpro_consolidado <- homologar_categorias(saberpro_consolidado, tabla_homologacion_cat)

# La homologación geográfica DANE requiere completar config/homologacion_geografia_dane.csv
# con la codificación oficial -- se deja preparada para activarse cuando esa
# tabla exista con contenido real (mientras tanto no falla, solo advierte).
tabla_dane <- cargar_homologacion_geografia(config$rutas$homologacion_geografia)

# -----------------------------------------------------------------------------
# 6. Cruce Saber11 -> SaberPro
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Fase de cruce =====")

ruta_cruces <- list.files(config$rutas$raw_cruces, full.names = TRUE,
                           pattern = "\\.(csv|txt)$", ignore.case = TRUE)
if (length(ruta_cruces) == 0) {
  registrar_log("ERROR", glue::glue(
    "No se encontró ningún archivo en {config$rutas$raw_cruces}. ",
    "El cruce Saber11-SaberPro requiere la tabla puente."))
  stop("Ejecución detenida: falta la tabla de cruces.")
}

tabla_cruces <- data.table::fread(ruta_cruces[1], sep = config$lectura$separador,
                                   encoding = "UTF-8", colClasses = "character")
data.table::setnames(tabla_cruces, tolower(trimws(names(tabla_cruces))))

# Las llaves de cruce se normalizan igual que en 03/04_limpieza (mayúsculas,
# sin espacios, sin tildes) para que coincidan con las columnas ya limpias
# de saber11_consolidado / saberpro_consolidado. Sin esto, una diferencia de
# mayúsculas o un espacio suelto en el archivo de cruces basta para que el
# merge no encuentre coincidencias (fue la causa del 0 en la primera corrida).
cols_llave_cruce <- intersect(
  c(config$cruce$llave_saber11_en_tabla_cruce, config$cruce$llave_saberpro_en_tabla_cruce),
  names(tabla_cruces)
)
for (col in cols_llave_cruce) {
  tabla_cruces[, (col) := normalizar_texto(get(col))]
}

base_final <- construir_cruce(saber11_consolidado, saberpro_consolidado, tabla_cruces, config)

# -----------------------------------------------------------------------------
# 7. Validación (Fase 5)
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Fase de validación =====")

# Rangos teóricos definidos en config.yml -> rangos_puntajes, segmentados por
# periodo y por prueba (ver ahí la nota sobre comparabilidad de escalas a lo
# largo de los 12 años -- ya NO se aplica un rango único a toda la serie).
reporte <- generar_reporte_validacion(
  saber11_consolidado, base_final,
  llave = config$cruce$llave_saber11_en_tabla_cruce,
  rangos_esperados = config$rangos_puntajes,
  diccionario = diccionario,
  config = config,
  dir_output = file.path(config$rutas$output, "validacion")
)

# -----------------------------------------------------------------------------
# 8. Reestructuración final de columnas (unificar archivo_origen, eliminar
#    columnas irrelevantes, reordenar) -- definido en config.yml
# -----------------------------------------------------------------------------
base_final <- estructurar_columnas_finales(
  base_final,
  orden_deseado = config$orden_columnas_finales,
  columnas_a_eliminar = config$columnas_a_eliminar
)

# -----------------------------------------------------------------------------
# 9. Exportación final
# -----------------------------------------------------------------------------
registrar_log("INFO", "===== Exportación final =====")

exportar_base_final(base_final, file.path(config$rutas$output, "base_saber11_saberpro.csv"))
exportar_diccionario_salida(base_final, diccionario,
                             file.path(config$rutas$output, "diccionario_base_final.csv"))

registrar_log("INFO", "===== Ejecución completa =====")
