# =============================================================================
# analisis/02_sesgo_seleccion.R
# =============================================================================
# Estudio 2 (ver estudios_estadisticos_propuestos.docx): SESGO DE SELECCIÓN.
# Pregunta: de todos los que presentaron Saber 11, ¿quién NO llega a
# presentar Saber Pro, y esa ausencia está asociada al estrato, al género o
# al tipo de colegio? Si la respuesta es sí, cualquier conclusión de los
# otros estudios (que solo miran a quienes SÍ llegaron) debe leerse con esa
# salvedad -- no es un defecto del análisis, es una característica real de
# los datos que hay que reportar.
#
# Requisito: haber corrido run_all.R al menos una vez. Este script NO vuelve
# a leer los microdatos crudos ni los reprocesa -- reutiliza lo que run_all.R
# ya dejó en disco:
#   - data/interim/saber11_*.parquet          (checkpoints por periodo)
#   - data/output/base_saber11_saberpro.csv    (la base final ya cruzada)
#   - data/output/validacion/tasa_vinculacion_por_cohorte.csv (columna regimen)
#
# Cómo correrlo (con la carpeta de trabajo en la raíz del proyecto):
#   source("analisis/02_sesgo_seleccion.R")
#
# Salidas, todas en data/output/analisis/:
#   - sesgo_seleccion_chi_cuadrado.csv   una fila por variable con el
#                                         resultado de la prueba chi-cuadrado
#   - sesgo_seleccion_proporciones.csv    proporción vinculada por categoría,
#                                         con intervalo de confianza del 95%
#   - graficos/vinculacion_por_<variable>.png
#   - sesgo_seleccion_resumen.txt          lectura en texto plano, lista para
#                                         pegar en el informe
#
# Objetos que quedan en el Environment al terminar (para inspeccionar en
# RStudio, además de los archivos de arriba):
#   - saber11_unico            universo de Saber 11 ya filtrado y con la
#                               columna vinculado
#   - chi_final / prop_final   los mismos resultados de los .csv, ya en R
#   - perfil_<variable>        UNA tabla por variable (estu_consecutivo,
#                               <variable>, vinculado) -- datos individuales,
#                               sin agrupar
#   - frecuencia_<variable>    UNA tabla por variable, ya agrupada por
#                               frecuencia (<variable>, no_vinculado,
#                               si_vinculado, total)
# Todo lo demás (variables de trabajo de los bucles, el universo crudo de
# Saber 11 antes de deduplicar, listas intermedias, etc.) se borra
# automáticamente al final del script -- ver el Bloque 6.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(yaml)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

config <- yaml::read_yaml("config.yml")

# Solo se necesitan dos funciones de 06_cruce.R (periodo_a_valor y
# resolver_presentaciones_multiples) -- source() aquí no vuelve a correr el
# cruce, ese archivo únicamente define funciones.
source("R/06_cruce.R")

dir_analisis <- file.path(config$rutas$output, "analisis")
dir_graficos <- file.path(dir_analisis, "graficos")
dir.create(dir_graficos, showWarnings = FALSE, recursive = TRUE)

cat(glue::glue("[{Sys.time()}] Iniciando análisis de sesgo de selección...\n"))

# -----------------------------------------------------------------------------
# 1. Universo completo de Saber 11: UN registro por estudiante, con la MISMA
#    regla de presentaciones múltiples que usa 06_cruce.R -- así se compara
#    exactamente el mismo universo que entra al cruce, ni uno más ni uno menos.
# -----------------------------------------------------------------------------
archivos_s11 <- list.files(config$rutas$interim, pattern = "^saber11_.*\\.parquet$", full.names = TRUE)
if (length(archivos_s11) == 0) {
  stop("No hay checkpoints de Saber 11 en data/interim/ -- corre run_all.R primero.")
}

saber11_todos <- data.table::rbindlist(lapply(archivos_s11, arrow::read_parquet), fill = TRUE)
data.table::setDT(saber11_todos)

saber11_todos[, periodo_valor := periodo_a_valor(periodo, "saber11")]

saber11_unico <- resolver_presentaciones_multiples(
  saber11_todos, "estu_consecutivo", "periodo_valor",
  config$cruce$regla_presentaciones_multiples$saber11)

cat(glue::glue("  Universo de Saber 11 (1 registro por estudiante): {nrow(saber11_unico)}\n"))

# -----------------------------------------------------------------------------
# 2. vinculado = TRUE si el estudiante SÍ aparece en la base final cruzada
#    (data/output/base_saber11_saberpro.csv), FALSE si no.
# -----------------------------------------------------------------------------
ruta_base_final <- file.path(config$rutas$output, "base_saber11_saberpro.csv")
if (!file.exists(ruta_base_final)) {
  stop("No existe ", ruta_base_final, " -- corre run_all.R primero.")
}

ids_vinculados <- unique(
  data.table::fread(ruta_base_final, sep = ";", select = "estu_consecutivo_sb11",
                     encoding = "UTF-8", colClasses = "character")$estu_consecutivo_sb11
)

saber11_unico[, vinculado := estu_consecutivo %in% ids_vinculados]

cat(glue::glue(
  "  Vinculados a Saber Pro: {sum(saber11_unico$vinculado)} de {nrow(saber11_unico)} ",
  "({round(100 * mean(saber11_unico$vinculado), 1)}%)\n"))

# -----------------------------------------------------------------------------
# 3. Excluir las cohortes con censura por la derecha esperada -- ya vienen
#    marcadas en tasa_vinculacion_por_cohorte.csv (columna regimen). No son
#    sesgo: son cohortes tan recientes de Saber 11 que, dado el rezago mínimo
#    configurado, todavía no ha pasado tiempo suficiente para que aparezcan
#    en Saber Pro. Incluirlas inflaría artificialmente la asociación.
# -----------------------------------------------------------------------------
ruta_tasas <- file.path(config$rutas$output, "validacion", "tasa_vinculacion_por_cohorte.csv")
if (!file.exists(ruta_tasas)) {
  stop("No existe ", ruta_tasas, " -- corre run_all.R primero.")
}
tasas <- data.table::fread(ruta_tasas, sep = ";", encoding = "UTF-8", colClasses = "character")

cohortes_censuradas <- tasas[regimen == "censura_por_la_derecha_probable", periodo]
n_antes <- nrow(saber11_unico)
saber11_unico <- saber11_unico[!periodo %in% cohortes_censuradas]
cat(glue::glue(
  "  Excluidos por censura por la derecha esperada: {n_antes - nrow(saber11_unico)} ",
  "registro(s), de {length(cohortes_censuradas)} cohorte(s) reciente(s)\n"))

# -----------------------------------------------------------------------------
# 4. Variables de perfil a comparar. Definidas en config.yml para poder
#    agregar/quitar una sin tocar este script.
# -----------------------------------------------------------------------------
variables_perfil <- config$analisis_sesgo_seleccion$variables_perfil
nivel_confianza  <- config$analisis_sesgo_seleccion$nivel_confianza %||% 0.95

resultados_chi  <- list()
resultados_prop <- list()

for (var in variables_perfil) {

  if (!var %in% names(saber11_unico)) {
    cat(glue::glue("  [AVISO] {var} no está disponible en los datos -- se omite.\n"))
    next
  }

  # Los NA quedan como su propia categoría "SIN DATO" -- nunca se descartan
  # en silencio, siguiendo el mismo principio defensivo del resto del
  # proyecto (ver 01_io_lectura.R).
  categoria <- as.character(saber11_unico[[var]])
  categoria[is.na(categoria)] <- "SIN DATO"

  # --- Tabla de tres columnas (datos individuales, sin agrupar) -------------
  # Exactamente lo que describe el documento de cálculo: una fila por
  # estudiante, con su ID, su categoría, y si se vinculó o no. Se guarda
  # como un objeto propio (perfil_<variable>) para poder abrirla en el
  # panel Environment de RStudio, como cualquier otra tabla del proyecto.
  perfil_dt <- data.table::data.table(
    estu_consecutivo = saber11_unico$estu_consecutivo,
    categoria = categoria,
    vinculado = saber11_unico$vinculado
  )
  data.table::setnames(perfil_dt, "categoria", var)
  assign(as.character(glue::glue("perfil_{var}")), perfil_dt, envir = .GlobalEnv)

  tabla <- table(categoria, saber11_unico$vinculado)

  # --- Tabla de frecuencias (la tabla de dos columnas ya agrupada/contada) --
  # Es la misma tabla que arma table(), pero convertida a un data.table
  # legible (categoria | no_vinculado | si_vinculado | total) en vez de un
  # objeto "table" de R, para que también se pueda abrir como spreadsheet.
  frecuencia_dt <- data.table::as.data.table(as.data.frame(tabla, stringsAsFactors = FALSE))
  data.table::setnames(frecuencia_dt, c("categoria", "vinculado", "n"))
  frecuencia_dt <- data.table::dcast(frecuencia_dt, categoria ~ vinculado, value.var = "n")
  if ("FALSE" %in% names(frecuencia_dt)) data.table::setnames(frecuencia_dt, "FALSE", "no_vinculado")
  if ("TRUE"  %in% names(frecuencia_dt)) data.table::setnames(frecuencia_dt, "TRUE",  "si_vinculado")
  if (!"no_vinculado" %in% names(frecuencia_dt)) frecuencia_dt[, no_vinculado := 0L]
  if (!"si_vinculado"  %in% names(frecuencia_dt)) frecuencia_dt[, si_vinculado  := 0L]
  frecuencia_dt[, total := no_vinculado + si_vinculado]
  data.table::setnames(frecuencia_dt, "categoria", var)
  assign(as.character(glue::glue("frecuencia_{var}")), frecuencia_dt, envir = .GlobalEnv)

  # --- Prueba chi-cuadrado de independencia ---------------------------------
  prueba <- tryCatch(suppressWarnings(chisq.test(tabla)), error = function(e) NULL)

  if (!is.null(prueba)) {
    celda_baja <- any(prueba$expected < 5)
    resultados_chi[[var]] <- data.table::data.table(
      variable = var,
      estadistico_chi2 = unname(prueba$statistic),
      grados_libertad = unname(prueba$parameter),
      p_valor = prueba$p.value,
      conclusion = ifelse(prueba$p.value < 0.05,
        "Asociación estadísticamente significativa (p < 0.05)",
        "Sin evidencia de asociación (p >= 0.05)"),
      advertencia = ifelse(celda_baja,
        "Al menos una categoría con frecuencia esperada < 5; interpretar con cautela",
        "")
    )
  } else {
    cat(glue::glue("  [AVISO] No se pudo calcular chi-cuadrado para {var}.\n"))
  }

  # --- Proporción vinculada por categoría, con IC (nivel_confianza) --------
  for (cat_val in rownames(tabla)) {
    n_total <- sum(tabla[cat_val, ])
    n_vinc  <- tabla[cat_val, "TRUE"]
    if (is.na(n_vinc)) n_vinc <- 0
    pt <- prop.test(n_vinc, n_total, conf.level = nivel_confianza, correct = TRUE)
    resultados_prop[[glue::glue("{var}__{cat_val}")]] <- data.table::data.table(
      variable = var, categoria = cat_val,
      n_total = n_total, n_vinculados = n_vinc,
      proporcion_vinculada = round(n_vinc / n_total, 4),
      ic_inferior = round(pt$conf.int[1], 4),
      ic_superior = round(pt$conf.int[2], 4)
    )
  }

  # --- Gráfico de barras: proporción vinculada por categoría ----------------
  props <- vapply(rownames(tabla), function(cv) {
    tot <- sum(tabla[cv, ]); v <- tabla[cv, "TRUE"]; if (is.na(v)) v <- 0
    v / tot
  }, numeric(1))
  prop_global <- mean(saber11_unico$vinculado)

  ruta_png <- file.path(dir_graficos, glue::glue("vinculacion_por_{var}.png"))
  png(ruta_png, width = 900, height = 600, res = 120)
  barplot(props,
          main = glue::glue("Proporción vinculada a Saber Pro, por {var}"),
          ylab = "Proporción vinculada", ylim = c(0, max(props, prop_global) * 1.15),
          col = "#1C7293", las = 2, cex.names = 0.85)
  abline(h = prop_global, col = "#F9A825", lwd = 2, lty = 2)
  legend("topright", legend = glue::glue("Promedio global ({round(prop_global * 100, 1)}%)"),
         col = "#F9A825", lty = 2, lwd = 2, bty = "n", cex = 0.8)
  dev.off()
}

chi_final  <- data.table::rbindlist(resultados_chi, fill = TRUE)
prop_final <- data.table::rbindlist(resultados_prop, fill = TRUE)

data.table::fwrite(chi_final, file.path(dir_analisis, "sesgo_seleccion_chi_cuadrado.csv"), sep = ";")
data.table::fwrite(prop_final, file.path(dir_analisis, "sesgo_seleccion_proporciones.csv"), sep = ";")

# -----------------------------------------------------------------------------
# 5. Resumen en texto plano, listo para pegar en el informe técnico.
# -----------------------------------------------------------------------------
lineas_chi <- vapply(seq_len(nrow(chi_final)), function(i) {
  glue::glue(
    "  - {chi_final$variable[i]}: chi2 = {round(chi_final$estadistico_chi2[i], 2)}, ",
    "gl = {chi_final$grados_libertad[i]}, p = {format.pval(chi_final$p_valor[i], digits = 3)} ",
    "-> {chi_final$conclusion[i]}",
    "{if (nchar(chi_final$advertencia[i]) > 0) paste0(' [', chi_final$advertencia[i], ']') else ''}"
  )
}, character(1))

resumen <- c(
  "RESUMEN -- Sesgo de selección (Saber 11 -> Saber Pro)",
  glue::glue("Generado: {Sys.time()}"),
  glue::glue("Universo analizado: {nrow(saber11_unico)} estudiantes de Saber 11 ",
             "(excluidas las cohortes con censura por la derecha esperada)"),
  glue::glue("Vinculados a Saber Pro: {sum(saber11_unico$vinculado)} ",
             "({round(100 * mean(saber11_unico$vinculado), 1)}%)"),
  "",
  "Resultado de la prueba chi-cuadrado de independencia, por variable:",
  lineas_chi,
  "",
  "Ver sesgo_seleccion_proporciones.csv para el detalle por categoría (con IC 95%)",
  "y graficos/ para el comparativo visual de cada variable."
)
writeLines(resumen, file.path(dir_analisis, "sesgo_seleccion_resumen.txt"))

nombres_perfil     <- glue::glue("perfil_{variables_perfil}")
nombres_frecuencia <- glue::glue("frecuencia_{variables_perfil}")
cat(glue::glue(
  "  Objetos nuevos en el Environment (uno por variable, ábrelos con clic ",
  "para verlos como tabla):\n",
  "    - {paste(nombres_perfil, collapse = ', ')}  (estu_consecutivo + categoría + vinculado, sin agrupar)\n",
  "    - {paste(nombres_frecuencia, collapse = ', ')}  (ya agrupadas por frecuencia)\n"
))

# -----------------------------------------------------------------------------
# 6. Limpieza del Environment -- se borra todo lo que fue solo de trabajo
#    interno (variables de los bucles, tablas intermedias, listas ya
#    volcadas en chi_final/prop_final, el universo crudo de 8 millones de
#    filas ya resumido en saber11_unico, etc.). Se conservan ÚNICAMENTE:
#      - saber11_unico                          (el universo ya filtrado)
#      - chi_final / prop_final                  (los resultados agregados)
#      - perfil_<variable> / frecuencia_<variable> (una por variable, ver arriba)
#      - config, dir_analisis, dir_graficos       (por si se quiere re-inspeccionar)
#    Las funciones cargadas con source("R/06_cruce.R") NO se borran -- son
#    código, no datos, y se necesitarían de nuevo para volver a correr esto.
# -----------------------------------------------------------------------------
objetos_a_conservar <- c(
  "saber11_unico", "chi_final", "prop_final",
  "config", "dir_analisis", "dir_graficos",
  as.character(nombres_perfil), as.character(nombres_frecuencia)
)
objetos_en_environment <- ls(envir = .GlobalEnv)
objetos_a_borrar <- setdiff(objetos_en_environment, objetos_a_conservar)
# Nunca se borran funciones (periodo_a_valor, resolver_presentaciones_multiples,
# %||%, etc.) -- solo datos de trabajo interno.
objetos_a_borrar <- objetos_a_borrar[
  !vapply(objetos_a_borrar, function(o) is.function(get(o, envir = .GlobalEnv)), logical(1))
]
rm(list = objetos_a_borrar, envir = .GlobalEnv)

cat(glue::glue(
  "  Environment limpiado: se eliminaron {length(objetos_a_borrar)} objeto(s) de trabajo interno ",
  "({paste(objetos_a_borrar, collapse = ', ')}).\n"))

cat(glue::glue("[{Sys.time()}] Análisis completo. Resultados en {dir_analisis}\n"))

# Por último, las variables auxiliares de esta misma limpieza. Algunas ya
# se borraron en el rm() de arriba (si quedaron atrapadas en su propia
# lista de "a borrar"); intersect() evita un error por intentar borrar dos
# veces algo que ya no existe.
sobrantes_limpieza <- intersect(
  c("objetos_a_conservar", "objetos_en_environment", "objetos_a_borrar"),
  ls(envir = .GlobalEnv))
rm(list = sobrantes_limpieza, envir = .GlobalEnv)
rm(sobrantes_limpieza)
