# =============================================================================
# 00_RUN_PIPELINE_02_06.R
# PIPELINE AUTOMÁTICO — SCRIPTS 02 A 06
# =============================================================================
# Ejecuta secuencialmente:
#   02_place_effects.R
#   03_esda.R
#   04_spatial_econometrics.R
#   05_hpcl_hphc_pvi_diagnostics_esda.R
#   06_robustness_sdm_knn6.R
#
# Objetivos:
#   1. Ejecutar los scripts en orden.
#   2. Medir tiempo individual y acumulado.
#   3. Liberar memoria entre etapas.
#   4. Introducir una pausa corta y prudente entre scripts.
#   5. Detener el pipeline si una etapa falla.
#   6. Guardar una bitácora CSV de tiempos y estado.
#
# IMPORTANTE:
#   Cada script se ejecuta en un entorno limpio independiente, evitando que
#   objetos de una etapa contaminen accidentalmente la siguiente.
# =============================================================================

# ---- 0. CONFIGURACIÓN ---------------------------------------------------------
SCRIPT_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/script"

SCRIPTS <- c(
  "02_place_effects.R",
  "03_esda.R",
  "04_spatial_econometrics.R",
  "05_hpcl_hphc_pvi_diagnostics_esda.R",
  "06_robustness_sdm_knn6.R"
)

# Pausa entre scripts, en segundos.
# 15 s es suficientemente corta para no hacer lento el pipeline y permite que
# R/Windows terminen de liberar recursos y completar operaciones de escritura.
PAUSE_SECONDS <- 15L

# Carpeta para la bitácora del pipeline.
LOG_DIR <- file.path(
  "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos",
  "outputs_master",
  "00_pipeline_logs"
)

dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. VALIDACIÓN ------------------------------------------------------------
script_paths <- file.path(SCRIPT_DIR, SCRIPTS)

missing_scripts <- script_paths[!file.exists(script_paths)]

if (length(missing_scripts) > 0L) {
  stop(
    paste0(
      "PIPELINE CANCELADO.\nNo se encontraron los siguientes scripts:\n",
      paste(" -", missing_scripts, collapse = "\n")
    ),
    call. = FALSE
  )
}

cat("\n")
cat("====================================================================\n")
cat(" PIPELINE CHAPTER III — WAGES | SCRIPTS 02–06\n")
cat("====================================================================\n")
cat("Directorio :", SCRIPT_DIR, "\n")
cat("Inicio     :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Pausa      :", PAUSE_SECONDS, "segundos entre scripts\n")
cat("Scripts    :", length(SCRIPTS), "\n")
cat("====================================================================\n\n")

# ---- 2. TABLA DE RESULTADOS ---------------------------------------------------
pipeline_log <- data.frame(
  order = integer(),
  script = character(),
  start_time = character(),
  end_time = character(),
  elapsed_seconds = numeric(),
  elapsed_minutes = numeric(),
  status = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

pipeline_start <- Sys.time()

# ---- 3. EJECUCIÓN SECUENCIAL --------------------------------------------------
for (i in seq_along(SCRIPTS)) {

  script_name <- SCRIPTS[i]
  script_file <- script_paths[i]

  cat("\n")
  cat("####################################################################\n")
  cat(sprintf(" [%02d/%02d] EJECUTANDO: %s\n", i, length(SCRIPTS), script_name))
  cat("####################################################################\n")

  start_time <- Sys.time()

  cat("Inicio :", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

  # Cada script corre en su propio environment.
  # parent = globalenv() permite acceso normal a paquetes y funciones base,
  # pero evita conservar los objetos creados por el script anterior.
  script_env <- new.env(parent = globalenv())

  error_message <- NA_character_

  status <- tryCatch({

    sys.source(
      file = script_file,
      envir = script_env,
      chdir = FALSE,
      keep.source = TRUE
    )

    "OK"

  }, error = function(e) {

    error_message <<- conditionMessage(e)
    "ERROR"

  })

  end_time <- Sys.time()
  elapsed_seconds <- as.numeric(
    difftime(end_time, start_time, units = "secs")
  )
  elapsed_minutes <- elapsed_seconds / 60

  pipeline_log <- rbind(
    pipeline_log,
    data.frame(
      order = i,
      script = script_name,
      start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
      end_time = format(end_time, "%Y-%m-%d %H:%M:%S"),
      elapsed_seconds = round(elapsed_seconds, 2),
      elapsed_minutes = round(elapsed_minutes, 2),
      status = status,
      error_message = ifelse(is.na(error_message), "", error_message),
      stringsAsFactors = FALSE
    )
  )

  cat("\nEstado :", status, "\n")
  cat(
    "Tiempo :",
    sprintf(
      "%02d:%02d:%02d",
      floor(elapsed_seconds / 3600),
      floor((elapsed_seconds %% 3600) / 60),
      floor(elapsed_seconds %% 60)
    ),
    sprintf("(%.2f minutos)", elapsed_minutes),
    "\n"
  )

  # Guardar bitácora después de CADA etapa.
  # Así queda registro incluso si una etapa posterior falla.
  log_file <- file.path(
    LOG_DIR,
    paste0(
      "pipeline_02_06_",
      format(pipeline_start, "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )

  utils::write.csv(
    pipeline_log,
    log_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  # Eliminar explícitamente el entorno completo del script recién ejecutado.
  rm(script_env)

  # Varias pasadas de GC ayudan especialmente después de modelos/objetos
  # espaciales grandes.
  invisible(gc(verbose = FALSE))
  Sys.sleep(2)
  invisible(gc(verbose = FALSE))

  mem_now <- gc()
  cat(
    "GC     : memoria liberada; Vcells usadas =",
    format(mem_now["Vcells", "used"], big.mark = ","),
    "\n"
  )

  # Si hay error, NO continuar con scripts dependientes.
  if (status == "ERROR") {

    cat("\n")
    cat("====================================================================\n")
    cat(" PIPELINE DETENIDO\n")
    cat("====================================================================\n")
    cat("Script :", script_name, "\n")
    cat("Error  :", error_message, "\n")
    cat("Log    :", log_file, "\n")
    cat("====================================================================\n")

    stop(
      paste0(
        "El pipeline se detuvo en ", script_name,
        ". Revise el error mostrado arriba y la bitácora."
      ),
      call. = FALSE
    )
  }

  # Pausa solo si todavía queda otro script por ejecutar.
  if (i < length(SCRIPTS)) {

    cat(
      "\nPausa de recuperación:",
      PAUSE_SECONDS,
      "segundos antes del siguiente script...\n"
    )

    Sys.sleep(PAUSE_SECONDS)

    # GC adicional justo antes de la siguiente etapa.
    invisible(gc(verbose = FALSE))

    cat("Continuando...\n")
  }
}

# ---- 4. RESUMEN FINAL ---------------------------------------------------------
pipeline_end <- Sys.time()

total_seconds <- as.numeric(
  difftime(pipeline_end, pipeline_start, units = "secs")
)

total_minutes <- total_seconds / 60

cat("\n\n")
cat("====================================================================\n")
cat(" PIPELINE COMPLETADO CORRECTAMENTE\n")
cat("====================================================================\n")

print(
  pipeline_log[
    , c(
      "order",
      "script",
      "elapsed_seconds",
      "elapsed_minutes",
      "status"
    )
  ],
  row.names = FALSE
)

cat("\n")
cat("Inicio pipeline :", format(pipeline_start, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Fin pipeline    :", format(pipeline_end, "%Y-%m-%d %H:%M:%S"), "\n")
cat(
  "Tiempo total    :",
  sprintf(
    "%02d:%02d:%02d",
    floor(total_seconds / 3600),
    floor((total_seconds %% 3600) / 60),
    floor(total_seconds %% 60)
  ),
  sprintf("(%.2f minutos)", total_minutes),
  "\n"
)
cat("Bitácora        :", log_file, "\n")
cat("====================================================================\n")

# ---- 5. OBJETO FINAL DISPONIBLE EN LA CONSOLA --------------------------------
PIPELINE_TIMES_02_06 <- pipeline_log
