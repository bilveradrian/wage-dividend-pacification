# =============================================================================
# SCRIPT 01 — BASE INDIVIDUAL PARA MINCER, MEDELLÍN 2004–2018
# =============================================================================
# INPUT:
#   datos/ecv_medellin_2004_2025/consolidado_2004_2025/depurados/
# OUTPUT:
#   datos/outputs_master/01_mincer/
#
# Este script NO modifica las bases estandarizadas. Solo construye el universo
# y la muestra completa para la etapa Mincer a partir de los productos del 00.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readxl)
  library(readr)
})

# ---- 1. RUTAS ----------------------------------------------------------------
DATA_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos"

DEPURADOS_DIR <- file.path(
  DATA_DIR, "ecv_medellin_2004_2025", "consolidado_2004_2025", "depurados"
)

MASTER_OUT_DIR <- file.path(DATA_DIR, "outputs_master")
OUT_DIR <- file.path(MASTER_OUT_DIR, "01_mincer")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(DEPURADOS_DIR)) {
  stop("No existe el directorio de bases estandarizadas: ", DEPURADOS_DIR)
}

YEARS <- 2004:2018

# ---- 2. LOCALIZAR EXCEL ESTANDARIZADOS ---------------------------------------
find_standardized_file <- function(y) {
  exact <- file.path(DEPURADOS_DIR, paste0("ECV_", y, "_subsample_estandarizada.xlsx"))
  if (file.exists(exact)) return(exact)

  cand <- list.files(
    DEPURADOS_DIR,
    pattern = paste0("^ECV_", y, "_.*\\.xlsx$"),
    full.names = TRUE,
    recursive = FALSE
  )
  cand <- cand[!grepl("AUDITORIA|RESUMEN|CONTROL", basename(cand), ignore.case = TRUE)]

  if (!length(cand)) {
    stop("No se encontró base estandarizada para ", y, " en: ", DEPURADOS_DIR)
  }

  # Preferir explícitamente el archivo de submuestra estandarizada.
  ord <- order(
    !grepl("subsample_estandarizada", basename(cand), ignore.case = TRUE),
    basename(cand)
  )
  cand[ord][1]
}

std_files <- setNames(vapply(YEARS, find_standardized_file, character(1)), YEARS)

FILE_MAP <- tibble(
  year = YEARS,
  standardized_file = unname(std_files)
)
readr::write_csv(FILE_MAP, file.path(OUT_DIR, "01_mincer_input_file_map.csv"))

# ---- 3. CONSTRUIR UNIVERSO Y MUESTRA MINCER ----------------------------------
keep_mincer <- c(
  "year","form_id","household_id","hh_uid","commune_id","commune_name",
  "barrio_id","barrio_name","stratum","age","female","edu_years",
  "experience","experience_sq","activity","employed","pension",
  "formal_employee","informal_employee","occ_status","economic_sector",
  "economic_sector_raw","economic_sector_source","wage_emp","wage_self",
  "wage_total","fep_p"
)

mincer_person <- vector("list", length(YEARS))
names(mincer_person) <- as.character(YEARS)

for (y in YEARS) {
  message("[01 MINCER] Leyendo ", y, " ...")

  dm <- readxl::read_excel(std_files[[as.character(y)]], sheet = "standardized") |>
    tibble::as_tibble()

  for (vv in setdiff(keep_mincer, names(dm))) dm[[vv]] <- NA

  dm <- dm |>
    dplyr::mutate(
      year = y,
      wage_total = suppressWarnings(as.numeric(wage_total)),
      ln_wage_total = dplyr::if_else(
        !is.na(wage_total) & wage_total > 0,
        log(wage_total),
        NA_real_
      ),
      mincer_eligible = as.integer(
        !is.na(age) & age >= 18 & age <= 65 &
        !is.na(employed) & employed == 1 &
        !is.na(wage_total) & wage_total > 0
      ),
      mincer_complete = as.integer(
        mincer_eligible == 1 &
        !is.na(edu_years) &
        !is.na(experience) &
        !is.na(experience_sq) &
        !is.na(female) &
        !is.na(formal_employee) &
        !is.na(economic_sector) &
        !is.na(barrio_id)
      )
    ) |>
    dplyr::select(
      dplyr::all_of(keep_mincer),
      ln_wage_total,
      mincer_eligible,
      mincer_complete
    )

  mincer_person[[as.character(y)]] <- dm
}

MINCER_ALL <- dplyr::bind_rows(mincer_person)
MINCER_SAMPLE <- MINCER_ALL |> dplyr::filter(mincer_complete == 1)

# ---- 4. AUDITORÍA -------------------------------------------------------------
MINCER_AUDIT <- MINCER_ALL |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n_universe = dplyr::n(),
    n_eligible = sum(mincer_eligible == 1, na.rm = TRUE),
    n_complete = sum(mincer_complete == 1, na.rm = TRUE),
    pct_complete_over_eligible = dplyr::if_else(
      n_eligible > 0, 100 * n_complete / n_eligible, NA_real_
    ),
    .groups = "drop"
  )

# ---- 5. GUARDAR ---------------------------------------------------------------
MINCER_ALL_CSV <- file.path(OUT_DIR, "ECV_MINCER_2004_2018_UNIVERSO.csv")
MINCER_SAMPLE_CSV <- file.path(OUT_DIR, "ECV_MINCER_2004_2018_MUESTRA_COMPLETA.csv")
MINCER_SAMPLE_RDS <- file.path(OUT_DIR, "ECV_MINCER_2004_2018_MUESTRA_COMPLETA.rds")
MINCER_AUDIT_CSV <- file.path(OUT_DIR, "01_mincer_sample_audit.csv")

readr::write_csv(MINCER_ALL, MINCER_ALL_CSV, na = "")
readr::write_csv(MINCER_SAMPLE, MINCER_SAMPLE_CSV, na = "")
readr::write_csv(MINCER_AUDIT, MINCER_AUDIT_CSV, na = "")
saveRDS(MINCER_SAMPLE, MINCER_SAMPLE_RDS)

cat("\n============================================================\n")
cat("SCRIPT 01 MINCER COMPLETE\n")
cat("Input standardized :", DEPURADOS_DIR, "\n")
cat("Output             :", OUT_DIR, "\n")
cat("N universo         :", nrow(MINCER_ALL), "\n")
cat("N muestra completa :", nrow(MINCER_SAMPLE), "\n")
cat("============================================================\n")
