# =============================================================================
# SCRIPT 02D V1 — PLACE EFFECTS BARRIO–COMUNA CON PARTIAL POOLING
#               Y MASTER ECV BARRIO-AÑO, MEDELLÍN 2004–2018
# Autor: Bilver A. Astorquiza Bustos
# Proyecto: Spatial Wage Penalties and the Stigma of Violence
# Objetivo: Journal of Urban Economics
# Fecha: septiembre de 2026
# =============================================================================
# INPUT:  bases standardized generadas por Script 01 (V16)
# OUTPUT PRINCIPAL: MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.csv/.rds/.xlsx
# OUTPUTS ANALÍTICOS: consolidado_2004_2025/02D_place_effects_barrio_comuna/
#
# DISEÑO:
# - Individuo = unidad de observación de la ecuación salarial.
# - Comuna = dominio de representatividad de la ECV y ancla jerárquica.
# - Barrio = unidad espacial/contextual de interés del paper.
# - Se estiman FE barrio×año directos y efectos Empirical-Bayes (partial pooling).
# - El MASTER resultante NO incorpora aún homicidios externos, SPI, Metro,
#   establecimientos financieros ni shapefile; esos insumos se unirán en Script 03.
# =============================================================================

rm(list = ls()); gc()
options(stringsAsFactors = FALSE, scipen = 999)

# ---- 0. PAQUETES -------------------------------------------------------------
pkgs <- c("dplyr","tidyr","purrr","stringr","readr","readxl","openxlsx",
          "fixest","broom","forcats","tibble","officer","flextable","ggplot2","scales","lme4")
inst <- rownames(installed.packages())
for (p in setdiff(pkgs, inst)) install.packages(p, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 1. RUTAS ----------------------------------------------------------------
ROOT_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos/ecv_medellin_2004_2025"
CONS_DIR <- file.path(ROOT_DIR, "consolidado_2004_2025")
MASTER_OUT_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos/outputs_master"
OUT_DIR  <- file.path(MASTER_OUT_DIR, "02_place_effects")
DIRS <- c(
  data = file.path(OUT_DIR,"01_datos_analiticos"),
  desc = file.path(OUT_DIR,"02_descriptivas"),
  model = file.path(OUT_DIR,"03_modelos"),
  paper = file.path(OUT_DIR,"04_tablas_paper"),
  fe = file.path(OUT_DIR,"05_efectos_fijos"),
  audit = file.path(OUT_DIR,"06_auditoria"),
  figures = file.path(OUT_DIR,"07_graficas_place_effects"),
  bridge = file.path(OUT_DIR,"08_base_para_merge_espacial")
)
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
invisible(lapply(DIRS, dir.create, recursive=TRUE, showWarnings=FALSE))

MASTER_CSV  <- file.path(ROOT_DIR,"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.csv")
MASTER_RDS  <- file.path(ROOT_DIR,"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.rds")
MASTER_XLSX <- file.path(ROOT_DIR,"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.xlsx")

# ---- 2. FUNCIONES -------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))
valid_w <- function(w) !is.na(w) & is.finite(w) & w > 0
wmean <- function(x,w){ ok <- !is.na(x)&is.finite(x)&valid_w(w); if(!any(ok)) return(NA_real_); weighted.mean(x[ok],w[ok]) }
wmedian <- function(x,w){
  ok <- !is.na(x)&is.finite(x)&valid_w(w); if(!any(ok)) return(NA_real_)
  x<-x[ok]; w<-w[ok]; o<-order(x); x<-x[o]; w<-w[o]; x[which(cumsum(w)/sum(w)>=.5)[1]]
}
wshare <- function(x,w,value=1){ ok<-!is.na(x)&valid_w(w); if(!any(ok)) return(NA_real_); sum(w[ok]*(x[ok]==value))/sum(w[ok]) }
q_safe <- function(x,p){ x<-x[is.finite(x)&!is.na(x)]; if(!length(x)) NA_real_ else unname(quantile(x,p,na.rm=TRUE,type=7)) }
mode_chr <- function(x,w=NULL){
  ok<-!is.na(x)&x!=""; x<-as.character(x[ok]); if(!length(x)) return(NA_character_)
  if(is.null(w)){ tt<-table(x); return(names(tt)[which.max(tt)]) }
  w<-w[ok]; z<-tapply(w,x,sum,na.rm=TRUE); names(z)[which.max(z)]
}

# Clasificación educativa derivada de edu_level documentado en Script 01:
# 0 ninguna; 1 preescolar; 2 primaria; 3 secundaria; 4 media;
# 5 técnica/tecnológica; 6 universitaria; 7 posgrado.
edu_group <- function(x) dplyr::case_when(
  x %in% c(0,1) ~ "none_preschool",
  x == 2 ~ "primary", x %in% c(3,4) ~ "secondary",
  x == 5 ~ "technical", x == 6 ~ "university", x == 7 ~ "postgraduate",
  TRUE ~ NA_character_)

# Adecuación de paredes/pisos: Script 01 define precario explícitamente.
# Para evitar imponer una taxonomía nueva, se usa 1-precarious_*.

# ---- 3. LOCALIZAR BASES STANDARDIZED DEL SCRIPT 01 ---------------------------
# Se buscan primero los Excel anuales en CONS_DIR/depurados y luego recursivamente.
all_xlsx <- list.files(CONS_DIR, pattern="\\.xlsx$", recursive=TRUE, full.names=TRUE)
all_xlsx <- all_xlsx[!grepl("AUDITORIA|MAP_|MASTER_|~\\$", basename(all_xlsx), ignore.case=TRUE)]

has_std <- function(f){ tryCatch("standardized" %in% readxl::excel_sheets(f), error=function(e) FALSE) }
std_candidates <- all_xlsx[vapply(all_xlsx, has_std, logical(1))]
if(!length(std_candidates)) stop("SCRIPT 02: no se localizaron Excel con hoja 'standardized' bajo CONS_DIR.")

year_from_file <- function(f){
  m <- stringr::str_extract(basename(f), "20(0[4-9]|1[0-8])")
  suppressWarnings(as.integer(m))
}
file_map <- tibble(file=std_candidates, year=vapply(std_candidates, year_from_file, integer(1))) %>%
  filter(year %in% 2004:2018) %>% arrange(year)
# Si hay duplicados, prioriza rutas que contengan depurados.
file_map <- file_map %>% mutate(priority=ifelse(grepl("depurados",file,ignore.case=TRUE),0,1)) %>%
  arrange(year,priority) %>% distinct(year,.keep_all=TRUE)
if(nrow(file_map)!=15) stop("SCRIPT 02: se esperaban 15 bases standardized (2004–2018) y se encontraron ",nrow(file_map),". Años: ",paste(file_map$year,collapse=", "))
readr::write_csv(file_map,file.path(DIRS["audit"],"mapa_archivos_standardized_2004_2018.csv"))

message("[02] Leyendo standardized 2004–2018...")
# Lectura secuencial para reducir picos de memoria (especialmente 2018).
# Se lee un año, se seleccionan sólo las columnas necesarias y se libera memoria.
VARS_02 <- c(
  "year","source_row","form_id","commune_id","commune_name","stratum",
  "barrio_id","barrio_name","barrio_match_method","area","dwelling_type",
  "person_order","household_id","age","female","civil_status","kinship",
  "edu_level","activity","occ_status","pension","health",
  "economic_sector_raw","economic_sector_source","economic_sector_candidate_10",
  "economic_sector","economic_sector_6_label",
  "walls","floor","electricity","water","sewerage","internet",
  "refrigerator","washer","computer","fep_p","fep_est","violence","edu_years",
  "employed","formal_employee","informal_employee","head_hh","tertiary",
  "female_head_hh","precarious_walls","precarious_floor",
  "housing_domains_observed","housing_domains_deprived",
  "housing_deprivation","housing_deprivation_intensity","hh_uid","age_cohort",
  "experience","experience_sq","wage_emp","wage_self","wage_total",
  "wage_emp_nom","wage_self_nom","wage_total_nom"
)

read_standardized_safe <- function(f, y){
  message(sprintf("[02] Leyendo standardized %s: %s", y, basename(f)))
  gc(verbose=FALSE)

  # Intento principal: readxl. Leer como texto reduce inferencia costosa de tipos.
  d <- tryCatch(
    readxl::read_excel(
      f, sheet="standardized",
      col_types="text",
      .name_repair="minimal",
      guess_max=1000
    ),
    error=function(e){
      message(sprintf("[02] readxl falló en %s: %s", y, conditionMessage(e)))
      NULL
    }
  )

  if(is.null(d)){
    stop(
      "No fue posible leer la hoja standardized del año ", y,
      ". El archivo Excel supera la memoria disponible para readxl. ",
      "La solución recomendada es guardar ese standardized como CSV/RDS desde Script 01 ",
      "y hacer que Script 02 lo lea directamente. Archivo: ", f
    )
  }

  d <- tibble::as_tibble(d)
  keep <- intersect(VARS_02, names(d))
  missing_core <- setdiff(c("barrio_id","age","female","edu_years","experience",
                            "experience_sq","economic_sector","wage_total_nom","fep_p"), keep)
  if(length(missing_core)){
    warning("Año ",y,": faltan columnas centrales: ",paste(missing_core,collapse=", "))
  }

  d <- dplyr::select(d, dplyr::all_of(keep))
  d$year <- as.integer(y)

  # Conversión controlada sólo de variables que deben ser numéricas.
  numeric_vars <- intersect(c(
    "source_row","commune_id","stratum","person_order","household_id","age","female",
    "pension","health","economic_sector_candidate_10","economic_sector",
    "walls","floor","electricity","water","sewerage","internet","refrigerator",
    "washer","computer","fep_p","fep_est","violence","edu_years","employed",
    "formal_employee","informal_employee","head_hh","tertiary","female_head_hh",
    "precarious_walls","precarious_floor","housing_domains_observed",
    "housing_domains_deprived","housing_deprivation",
    "housing_deprivation_intensity","experience","experience_sq",
    "wage_emp","wage_self","wage_total","wage_emp_nom","wage_self_nom",
    "wage_total_nom"
  ), names(d))

  d <- dplyr::mutate(
    d,
    dplyr::across(dplyr::all_of(numeric_vars), ~suppressWarnings(as.numeric(.x))),
    barrio_id=stringr::str_pad(as.character(barrio_id),4,pad="0")
  )
  gc(verbose=FALSE)
  d
}

micro_list <- vector("list", nrow(file_map))
for(i in seq_len(nrow(file_map))){
  micro_list[[i]] <- read_standardized_safe(file_map$file[i], file_map$year[i])
  message(sprintf("[02] %s listo: %s filas.",file_map$year[i],
                  format(nrow(micro_list[[i]]),big.mark=",")))
  gc(verbose=FALSE)
}
micro <- dplyr::bind_rows(micro_list)
rm(micro_list)
gc(verbose=FALSE)

required <- c("year","source_row","form_id","commune_id","commune_name","stratum","barrio_id","barrio_name","area","dwelling_type","person_order","household_id","age","female","civil_status","kinship","edu_level","activity","occ_status","pension","economic_sector","economic_sector_6_label","walls","floor","electricity","water","sewerage","internet","refrigerator","washer","computer","fep_p","fep_est","violence","edu_years","employed","formal_employee","informal_employee","head_hh","tertiary","female_head_hh","precarious_walls","precarious_floor","housing_domains_observed","housing_domains_deprived","housing_deprivation","housing_deprivation_intensity","hh_uid","experience","experience_sq","wage_total_nom")
miss <- setdiff(required,names(micro)); if(length(miss)) stop("SCRIPT 02: faltan columnas del Script 01: ",paste(miss,collapse=", "))

# Tipos básicos
numvars <- intersect(c("year","stratum","area","dwelling_type","age","female","civil_status","edu_level","activity","occ_status","pension","economic_sector","walls","floor","electricity","water","sewerage","internet","refrigerator","washer","computer","fep_p","fep_est","violence","edu_years","employed","formal_employee","informal_employee","head_hh","tertiary","female_head_hh","precarious_walls","precarious_floor","housing_domains_observed","housing_domains_deprived","housing_deprivation","housing_deprivation_intensity","experience","experience_sq","wage_total_nom"),names(micro))
micro <- micro %>% mutate(across(dplyr::all_of(numvars),num), barrio_id=stringr::str_pad(as.character(barrio_id),4,pad="0"), edu_group=edu_group(edu_level))

# ---- 4. IPC Y SALARIOS REALES: PESOS DE DICIEMBRE DE 2018 ------------------
# INPUT IPC local suministrado por el investigador.
# Archivo: IPC_Índice de Precios al Consumidor.xlsx
# Ruta:    .../Chapter III. Wages/datos/
# Hoja:    Datos
# Columnas esperadas: Fecha + Índice de Precios al Consumidor (IPC)
#
# ESPECIFICACIÓN PRINCIPAL:
# Se utiliza el índice de DICIEMBRE de cada año y diciembre de 2018 = 100.
# wage_dec2018 = wage_total_nom * IPC_dic2018 / IPC_dic_t
# Esta elección expresa todos los salarios en pesos de diciembre de 2018.
# El promedio anual se conserva SOLO como auditoría/robustez y no se usa en
# la especificación Mincer principal.

DATA_DIR <- dirname(ROOT_DIR)
IPC_XLSX <- file.path(DATA_DIR, "IPC_Índice de Precios al Consumidor.xlsx")
if(!file.exists(IPC_XLSX)){
  stop("SCRIPT 02: no se encontró el archivo IPC local: ", IPC_XLSX)
}

ipc_raw <- readxl::read_excel(IPC_XLSX, sheet="Datos") %>% as_tibble()
if(ncol(ipc_raw) < 2) stop("SCRIPT 02: la hoja 'Datos' del IPC debe tener al menos dos columnas.")

# Se usan las dos primeras columnas para tolerar el nombre largo/acento del índice.
ipc_monthly <- ipc_raw %>%
  transmute(
    fecha_raw = .data[[names(ipc_raw)[1]]],
    ipc_index = suppressWarnings(as.numeric(.data[[names(ipc_raw)[2]]]))
  ) %>%
  mutate(
    fecha = as.Date(fecha_raw),
    year = as.integer(format(fecha,"%Y")),
    month = as.integer(format(fecha,"%m"))
  ) %>%
  filter(year %in% 2004:2018, is.finite(ipc_index), ipc_index > 0) %>%
  arrange(year,month)

# Auditoría: deben existir 12 observaciones mensuales por año.
ipc_month_coverage <- ipc_monthly %>% count(year,name="n_months")
if(nrow(ipc_month_coverage)!=15 ||
   !all(ipc_month_coverage$year==2004:2018) ||
   any(ipc_month_coverage$n_months!=12)){
  stop("SCRIPT 02: el archivo IPC no contiene 12 meses válidos para cada año 2004–2018.")
}

# Serie principal: diciembre de cada año.
ipc_dec <- ipc_monthly %>%
  filter(month==12) %>%
  transmute(year, ipc_dec=ipc_index) %>%
  arrange(year)

if(nrow(ipc_dec)!=15 || !all(ipc_dec$year==2004:2018))
  stop("SCRIPT 02: falta diciembre para uno o más años 2004–2018.")

IPC_DEC_2018 <- ipc_dec$ipc_dec[ipc_dec$year==2018][1]
if(!is.finite(IPC_DEC_2018) || IPC_DEC_2018<=0)
  stop("SCRIPT 02: IPC de diciembre de 2018 inválido.")

# La base suministrada está expresada con diciembre de 2018 = 100.
# Se valida con tolerancia pequeña para evitar errores de redondeo/importación.
if(abs(IPC_DEC_2018-100) > 0.05)
  stop("SCRIPT 02: diciembre de 2018 no es aproximadamente 100. Valor leído: ", IPC_DEC_2018)

ipc_dec <- ipc_dec %>%
  mutate(
    factor_to_dec2018 = IPC_DEC_2018/ipc_dec,
    base_reference = "Diciembre 2018 = 100",
    source_file = basename(IPC_XLSX)
  )

# Promedio anual: únicamente para auditoría y futura robustez.
ipc_avg <- ipc_monthly %>%
  group_by(year) %>%
  summarise(ipc_avg=mean(ipc_index,na.rm=TRUE), .groups="drop") %>%
  mutate(factor_avg_to_2018 = ipc_avg[year==2018][1]/ipc_avg)

ipc_audit <- ipc_dec %>%
  left_join(ipc_avg,by="year") %>%
  mutate(
    check_dec2018 = ifelse(year==2018,abs(ipc_dec-100)<=0.05,TRUE),
    main_deflator = "IPC diciembre"
  )

readr::write_csv(
  ipc_monthly %>% dplyr::select(fecha, year, month, ipc_index),
  file.path(DIRS["audit"], "IPC_MENSUAL_FUENTE_2004_2018.csv"))
readr::write_csv(ipc_audit,
                 file.path(DIRS["audit"],"IPC_DICIEMBRE_Y_PROMEDIO_AUDITORIA_2004_2018.csv"))

# Integración con microdatos y deflactación principal a diciembre de 2018.
micro <- micro %>%
  left_join(ipc_dec %>% dplyr::select(year,ipc_dec,factor_to_dec2018),by="year") %>%
  mutate(
    wage_2018 = if_else(!is.na(wage_total_nom) & wage_total_nom>0,
                        wage_total_nom*factor_to_dec2018, NA_real_),
    ln_wage_2018 = if_else(!is.na(wage_2018) & wage_2018>0,
                           log(wage_2018),NA_real_),
    barrio_year = paste0(barrio_id,"__",year)
  )

# Auditoría salarial antes de cualquier winsorización.
wage_audit <- micro %>% group_by(year) %>% summarise(
  ipc_dec=first(ipc_dec), factor_to_dec2018=first(factor_to_dec2018),
  n=n(), n_positive=sum(!is.na(wage_2018)&wage_2018>0),
  mean=wmean(wage_2018,fep_p), median=wmedian(wage_2018,fep_p),
  p01=q_safe(wage_2018,.01),p05=q_safe(wage_2018,.05),
  p95=q_safe(wage_2018,.95),p99=q_safe(wage_2018,.99),
  min=ifelse(any(wage_2018>0,na.rm=TRUE),min(wage_2018[wage_2018>0],na.rm=TRUE),NA_real_),
  max=ifelse(any(wage_2018>0,na.rm=TRUE),max(wage_2018,na.rm=TRUE),NA_real_),
  .groups="drop")
readr::write_csv(wage_audit,file.path(DIRS["audit"],"auditoria_salario_pesos_dic2018.csv"))

# Robustez: winsorización anual P1–P99; NO sustituye la variable principal.
bounds <- micro %>% filter(!is.na(wage_2018),wage_2018>0) %>%
  group_by(year) %>%
  summarise(lo=q_safe(wage_2018,.01),hi=q_safe(wage_2018,.99),.groups="drop")
micro <- micro %>% left_join(bounds,by="year") %>% mutate(
  wage_2018_w99=if_else(!is.na(wage_2018),pmin(pmax(wage_2018,lo),hi),NA_real_),
  ln_wage_2018_w99=if_else(!is.na(wage_2018_w99)&wage_2018_w99>0,
                           log(wage_2018_w99),NA_real_)
) %>% dplyr::select(-lo,-hi)

# ---- 5. MUESTRA MINCER --------------------------------------------------------
# Civil status entra como factor. Se conserva categoría original; fixest elige referencia.
mincer <- micro %>% filter(year<=2018, age>=18,age<=65, employed==1, wage_2018>0,
  !is.na(edu_years),!is.na(experience),!is.na(experience_sq),!is.na(female),
  !is.na(formal_employee),!is.na(civil_status),!is.na(economic_sector),
  !is.na(barrio_id),barrio_id!="",valid_w(fep_p)) %>%
  mutate(civil_status_f=factor(civil_status), economic_sector_f=factor(economic_sector))
if(!nrow(mincer)) stop("Muestra Mincer vacía.")

readr::write_csv(mincer,file.path(DIRS["data"],"MINCER_ANALITICA_2004_2018.csv"),na="")
saveRDS(mincer,file.path(DIRS["data"],"MINCER_ANALITICA_2004_2018.rds"))

# ---- 6. DESCRIPTIVAS PAPER ----------------------------------------------------
desc_vars <- c("wage_2018","ln_wage_2018","edu_years","experience","female","formal_employee","age")
desc <- purrr::map_dfr(desc_vars,function(v){ x<-mincer[[v]]; w<-mincer$fep_p; tibble(
  Variable=v,N=sum(!is.na(x)),Mean=wmean(x,w),SD=sqrt(wmean((x-wmean(x,w))^2,w)),
  P25=q_safe(x,.25),Median=wmedian(x,w),P75=q_safe(x,.75),Min=min(x,na.rm=TRUE),Max=max(x,na.rm=TRUE)) })
sector_desc <- mincer %>% group_by(economic_sector,economic_sector_6_label) %>% summarise(N=n(),WeightedN=sum(fep_p,na.rm=TRUE),.groups="drop") %>% mutate(Share=WeightedN/sum(WeightedN))
civil_desc <- mincer %>% group_by(civil_status) %>% summarise(N=n(),WeightedN=sum(fep_p,na.rm=TRUE),.groups="drop") %>% mutate(Share=WeightedN/sum(WeightedN))
readr::write_csv(desc,file.path(DIRS["desc"],"descriptivas_mincer.csv"))


# =============================================================================
# 7. AUDITORÍA DE PESOS Y PESOS RELATIVOS
# =============================================================================
# Los FEP son pesos del diseño de la ECV a su dominio de inferencia. No se
# interpretan aquí como "población del barrio". Para el pooled 2004–2018 se
# normalizan dentro de año, preservando pesos relativos y evitando que cambios
# de escala entre archivos anuales dominen la función objetivo.

fep_audit <- mincer %>%
  group_by(year) %>%
  summarise(
    n=n(),
    fep_min=min(fep_p,na.rm=TRUE),
    fep_p01=q_safe(fep_p,.01),
    fep_median=q_safe(fep_p,.50),
    fep_mean=mean(fep_p,na.rm=TRUE),
    fep_p99=q_safe(fep_p,.99),
    fep_max=max(fep_p,na.rm=TRUE),
    sum_fep=sum(fep_p,na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    median_scale_ratio=fep_median/median(fep_median,na.rm=TRUE),
    scale_flag=median_scale_ratio>100 | median_scale_ratio<.01
  )

readr::write_csv(fep_audit,file.path(DIRS["audit"],"AUDITORIA_FEP_MINCER_POR_YEAR.csv"),na="")
cat("\n--- AUDITORÍA FEP MINCER POR AÑO ---\n")
print(fep_audit)

mincer <- mincer %>%
  mutate(
    commune_id=suppressWarnings(as.integer(commune_id)),
    commune_year=paste0(commune_id,"__",year)
  ) %>%
  group_by(year) %>%
  mutate(
    fep_norm_year=if_else(
      valid_w(fep_p),
      fep_p/mean(fep_p[valid_w(fep_p)],na.rm=TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(commune_id %in% 1:16, valid_w(fep_norm_year))

# Peso relativo adicional dentro de comuna-año para descriptivas territoriales.
micro <- micro %>%
  mutate(commune_id=suppressWarnings(as.integer(commune_id))) %>%
  group_by(year,commune_id) %>%
  mutate(
    fep_rel_commune_year=if_else(
      valid_w(fep_p),
      fep_p/mean(fep_p[valid_w(fep_p)],na.rm=TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

# =============================================================================
# 8. MINCER DIRECTA: FE BARRIO×AÑO
# =============================================================================
# Benchmark directo. NO se interpreta cada FE como estimador directo
# representativo del barrio. Su función es comparar con el estimador jerárquico.

fml_direct <- ln_wage_2018 ~
  edu_years + experience + experience_sq + female + formal_employee +
  i(civil_status_f,ref=4) |
  economic_sector_f + barrio_year

m_direct <- fixest::feols(
  fml_direct,
  data=mincer,
  weights=~fep_norm_year,
  cluster=~barrio_id,
  notes=FALSE
)

m_direct_unweighted <- fixest::feols(
  fml_direct,
  data=mincer,
  cluster=~barrio_id,
  notes=FALSE
)

fml_direct_w99 <- ln_wage_2018_w99 ~
  edu_years + experience + experience_sq + female + formal_employee +
  i(civil_status_f,ref=4) |
  economic_sector_f + barrio_year

m_direct_w99 <- fixest::feols(
  fml_direct_w99,
  data=mincer,
  weights=~fep_norm_year,
  cluster=~barrio_id,
  notes=FALSE
)

cat("\n====================================================================\n")
cat("MINCER DIRECTA — FE BARRIO×AÑO\n")
cat("====================================================================\n")
print(summary(m_direct))
cat("\n--- ROBUSTEZ SIN PESOS ---\n"); print(summary(m_direct_unweighted))
cat("\n--- ROBUSTEZ SALARIO WINSORIZADO P1-P99 ---\n"); print(summary(m_direct_w99))
cat("\n--- COMPARACIÓN ---\n")
print(fixest::etable(
  m_direct,m_direct_unweighted,m_direct_w99,
  headers=c("Direct FE weighted","Unweighted","Winsorized P1-P99"),
  fitstat=~n+r2+wr2
))

saveRDS(
  list(direct=m_direct,unweighted=m_direct_unweighted,w99=m_direct_w99),
  file.path(DIRS["model"],"MODELOS_MINCER_DIRECT_FE.rds")
)

# =============================================================================
# 9. PLACE EFFECT DIRECTO BARRIO×AÑO
# =============================================================================
fe_direct <- fixest::fixef(m_direct)
if(!"barrio_year" %in% names(fe_direct))
  stop("02D: no se pudo extraer FE barrio_year del modelo directo.")

theta_direct <- tibble(
  barrio_year=names(fe_direct$barrio_year),
  theta_direct_raw=as.numeric(fe_direct$barrio_year)
) %>%
  tidyr::separate(
    barrio_year,into=c("barrio_id","year"),
    sep="__",remove=FALSE,convert=FALSE
  ) %>%
  mutate(
    year=as.integer(year),
    barrio_id=stringr::str_pad(as.character(barrio_id),4,pad="0")
  )

cell_support <- mincer %>%
  group_by(year,barrio_id) %>%
  summarise(
    commune_id=as.integer(mode_chr(as.character(commune_id),fep_norm_year)),
    commune_name=mode_chr(commune_name,fep_norm_year),
    barrio_name=mode_chr(barrio_name,fep_norm_year),
    n_mincer=n(),
    sum_w_norm=sum(fep_norm_year,na.rm=TRUE),
    sum_w2_norm=sum(fep_norm_year^2,na.rm=TRUE),
    n_eff_kish=ifelse(sum_w2_norm>0,(sum_w_norm^2)/sum_w2_norm,NA_real_),
    .groups="drop"
  )

theta_direct <- theta_direct %>%
  left_join(cell_support,by=c("year","barrio_id")) %>%
  group_by(year) %>%
  mutate(
    theta_direct=theta_direct_raw-
      weighted.mean(theta_direct_raw,w=pmax(n_eff_kish,1),na.rm=TRUE),
    cswp_direct_pct=100*(exp(theta_direct)-1)
  ) %>%
  ungroup()

# =============================================================================
# 10. MODELO JERÁRQUICO: INDIVIDUO → BARRIO×AÑO → COMUNA×AÑO
# =============================================================================
# Partial pooling:
#   ln(w_i) = X_i beta + FE_year + FE_sector
#             + u_commune×year + v_barrio×year + error_i
#
# v_barrio×year es la desviación intra-comuna. El total territorial EB es
# u_commune×year + v_barrio×year.
#
# IMPORTANTE: barrio se usa como contexto/localización, no como dominio de
# representatividad directa de la ECV.

mincer_h <- mincer %>%
  mutate(
    year_f=factor(year),
    sector_f=factor(economic_sector),
    civil_f=factor(civil_status),
    barrio_year=factor(barrio_year),
    commune_year=factor(commune_year)
  )

fml_hier <- ln_wage_2018 ~
  edu_years + experience + experience_sq + female + formal_employee +
  civil_f + sector_f + year_f +
  (1|commune_year) + (1|barrio_year)

message("[02D] Estimando modelo jerárquico con partial pooling...")
m_hier <- lme4::lmer(
  fml_hier,
  data=mincer_h,
  weights=fep_norm_year,
  REML=TRUE,
  control=lme4::lmerControl(
    optimizer="bobyqa",
    optCtrl=list(maxfun=200000),
    check.conv.singular="ignore"
  )
)

cat("\n====================================================================\n")
cat("MODELO JERÁRQUICO — PARTIAL POOLING BARRIO DENTRO DE COMUNA\n")
cat("====================================================================\n")
print(summary(m_hier))
cat("\nSingular fit: ",lme4::isSingular(m_hier,tol=1e-5),"\n")

saveRDS(m_hier,file.path(DIRS["model"],"MODELO_HIERARCHICAL_PARTIAL_POOLING.rds"))

# =============================================================================
# 11. EXTRAER EFECTOS EMPIRICAL-BAYES Y SU INCERTIDUMBRE
# =============================================================================
extract_re <- function(model,grp,prefix){
  rr <- lme4::ranef(model,condVar=TRUE)[[grp]]
  pv <- attr(rr,"postVar")
  se <- if(!is.null(pv)) sqrt(as.numeric(pv[1,1,])) else rep(NA_real_,nrow(rr))
  tibble(
    key=rownames(rr),
    effect=as.numeric(rr[["(Intercept)"]]),
    posterior_se=se
  ) %>%
    rename(
      !!paste0(prefix,"_raw") := effect,
      !!paste0(prefix,"_posterior_se") := posterior_se
    )
}

re_b <- extract_re(m_hier,"barrio_year","theta_barrio_eb") %>%
  rename(barrio_year=key) %>%
  tidyr::separate(
    barrio_year,into=c("barrio_id","year"),
    sep="__",remove=FALSE,convert=FALSE
  ) %>%
  mutate(
    year=as.integer(year),
    barrio_id=stringr::str_pad(as.character(barrio_id),4,pad="0")
  )

re_c <- extract_re(m_hier,"commune_year","theta_commune_eb") %>%
  rename(commune_year=key) %>%
  tidyr::separate(
    commune_year,into=c("commune_id","year"),
    sep="__",remove=FALSE,convert=FALSE
  ) %>%
  mutate(year=as.integer(year),commune_id=as.integer(commune_id))

vc <- as.data.frame(lme4::VarCorr(m_hier))
tau2_barrio <- vc %>%
  filter(grp=="barrio_year",var1=="(Intercept)",is.na(var2)) %>%
  pull(vcov)
tau2_commune <- vc %>%
  filter(grp=="commune_year",var1=="(Intercept)",is.na(var2)) %>%
  pull(vcov)
sigma2_eps <- sigma(m_hier)^2

if(!length(tau2_barrio)) tau2_barrio <- NA_real_
if(!length(tau2_commune)) tau2_commune <- NA_real_

variance_components <- tibble(
  component=c("commune_year","barrio_year","residual"),
  variance=c(tau2_commune[1],tau2_barrio[1],sigma2_eps)
)
readr::write_csv(
  variance_components,
  file.path(DIRS["model"],"VARIANCE_COMPONENTS_HIERARCHICAL.csv"),
  na=""
)

theta_eb <- re_b %>%
  left_join(cell_support,by=c("year","barrio_id")) %>%
  mutate(commune_year=paste0(commune_id,"__",year)) %>%
  left_join(
    re_c %>% dplyr::select(year,commune_id,theta_commune_eb_raw,theta_commune_eb_posterior_se),
    by=c("year","commune_id")
  ) %>%
  mutate(
    # Efecto territorial total EB = componente comuna-año + desviación barrio-año.
    theta_eb_total_raw=theta_commune_eb_raw+theta_barrio_eb_raw,

    # Desviación barrial intra-comuna.
    theta_within_commune_raw=theta_barrio_eb_raw,

    # Reliability aproximada basada en varianza entre barrios y tamaño efectivo Kish.
    reliability_barrio=ifelse(
      is.finite(tau2_barrio[1]) & tau2_barrio[1]>0 &
        is.finite(n_eff_kish) & n_eff_kish>0,
      tau2_barrio[1]/(tau2_barrio[1]+sigma2_eps/n_eff_kish),
      NA_real_
    ),

    # SE aproximado del efecto total suponiendo independencia de RE de ambos niveles.
    theta_eb_total_se=sqrt(
      theta_barrio_eb_posterior_se^2+
      theta_commune_eb_posterior_se^2
    )
  ) %>%
  group_by(year) %>%
  mutate(
    theta_eb_total=theta_eb_total_raw-
      weighted.mean(theta_eb_total_raw,w=pmax(n_eff_kish,1),na.rm=TRUE),
    cswp_eb_pct=100*(exp(theta_eb_total)-1)
  ) %>%
  ungroup() %>%
  group_by(year,commune_id) %>%
  mutate(
    theta_within_commune=theta_within_commune_raw-
      weighted.mean(theta_within_commune_raw,w=pmax(n_eff_kish,1),na.rm=TRUE),
    cswp_within_commune_pct=100*(exp(theta_within_commune)-1)
  ) %>%
  ungroup()

# Intervalos aproximados del efecto territorial EB en escala log y porcentual.
theta_eb <- theta_eb %>%
  mutate(
    theta_eb_lo95=theta_eb_total-1.96*theta_eb_total_se,
    theta_eb_hi95=theta_eb_total+1.96*theta_eb_total_se,
    cswp_eb_lo95_pct=100*(exp(theta_eb_lo95)-1),
    cswp_eb_hi95_pct=100*(exp(theta_eb_hi95)-1),
    precision_flag=case_when(
      n_eff_kish>=30 & reliability_barrio>=.70 ~ "HIGH",
      n_eff_kish>=10 & reliability_barrio>=.40 ~ "MEDIUM",
      TRUE ~ "LOW"
    )
  )

# =============================================================================
# 12. COMPARAR FE DIRECTO VS PARTIAL POOLING
# =============================================================================
place_effects <- theta_eb %>%
  left_join(
    theta_direct %>%
      dplyr::select(year,barrio_id,theta_direct,cswp_direct_pct),
    by=c("year","barrio_id")
  ) %>%
  mutate(
    shrinkage_log=theta_eb_total-theta_direct,
    shrinkage_abs=abs(shrinkage_log),
    shrinkage_cswp_pp=cswp_eb_pct-cswp_direct_pct
  ) %>%
  arrange(year,commune_id,barrio_id)

readr::write_csv(
  place_effects,
  file.path(DIRS["fe"],"PLACE_EFFECTS_BARRIO_YEAR_DIRECT_EB_2004_2018.csv"),
  na=""
)
saveRDS(
  place_effects,
  file.path(DIRS["fe"],"PLACE_EFFECTS_BARRIO_YEAR_DIRECT_EB_2004_2018.rds")
)

diag_place <- place_effects %>%
  group_by(year) %>%
  summarise(
    n_barrio_year=n(),
    median_n=n_mincer %>% median(na.rm=TRUE),
    median_n_eff=median(n_eff_kish,na.rm=TRUE),
    median_reliability=median(reliability_barrio,na.rm=TRUE),
    p05_cswp_direct=q_safe(cswp_direct_pct,.05),
    p95_cswp_direct=q_safe(cswp_direct_pct,.95),
    p05_cswp_eb=q_safe(cswp_eb_pct,.05),
    p95_cswp_eb=q_safe(cswp_eb_pct,.95),
    max_abs_direct=max(abs(cswp_direct_pct),na.rm=TRUE),
    max_abs_eb=max(abs(cswp_eb_pct),na.rm=TRUE),
    corr_direct_eb=cor(theta_direct,theta_eb_total,use="complete.obs"),
    pct_low_precision=mean(precision_flag=="LOW",na.rm=TRUE),
    .groups="drop"
  )

readr::write_csv(
  diag_place,
  file.path(DIRS["audit"],"DIAGNOSTICO_PLACE_EFFECTS_DIRECT_VS_EB.csv"),
  na=""
)
cat("\n--- DIAGNÓSTICO PLACE EFFECTS DIRECTOS VS EB ---\n")
print(diag_place)

# =============================================================================
# 13. SALARIO OBSERVADO BARRIO-AÑO (SOLO DESCRIPTIVO)
# =============================================================================
observed_wage <- mincer %>%
  group_by(year,barrio_id) %>%
  summarise(
    observed_wage_mean_2018=wmean(wage_2018,fep_norm_year),
    observed_wage_median_2018=wmedian(wage_2018,fep_norm_year),
    observed_ln_wage_mean=wmean(ln_wage_2018,fep_norm_year),
    .groups="drop"
  ) %>%
  group_by(year) %>%
  mutate(
    observed_log_gap=
      observed_ln_wage_mean -
      mean(observed_ln_wage_mean,na.rm=TRUE),
    observed_gap_pct=100*(exp(observed_log_gap)-1)
  ) %>%
  ungroup()
# =============================================================================
# 14. MASTER CONTEXTUAL ECV BARRIO-AÑO
# =============================================================================
# Estos agregados barriales son DESCRIPTIVOS/MODEL-BASED, no estimadores directos
# oficialmente representativos del barrio. Se calculan con pesos relativos del
# diseño dentro de comuna-año. Los N y denominadores se conservan para auditoría.

person_rt <- micro %>%
  filter(
    year<=2018,
    commune_id %in% 1:16,
    !is.na(barrio_id),barrio_id!="",
    valid_w(fep_rel_commune_year)
  ) %>%
  group_by(year,barrio_id) %>%
  summarise(
    barrio_name=mode_chr(barrio_name,fep_rel_commune_year),
    commune_id=as.integer(mode_chr(as.character(commune_id),fep_rel_commune_year)),
    commune_name=mode_chr(commune_name,fep_rel_commune_year),
    n_person=n(),
    n_person_weight_valid=sum(valid_w(fep_rel_commune_year)),
    mean_age=wmean(age,fep_rel_commune_year),
    pct_female=wshare(female,fep_rel_commune_year),
    mean_edu_years=wmean(edu_years,fep_rel_commune_year),
    pct_edu_none_preschool=wshare(edu_group,fep_rel_commune_year,"none_preschool"),
    pct_edu_primary=wshare(edu_group,fep_rel_commune_year,"primary"),
    pct_edu_secondary=wshare(edu_group,fep_rel_commune_year,"secondary"),
    pct_edu_technical=wshare(edu_group,fep_rel_commune_year,"technical"),
    pct_edu_university=wshare(edu_group,fep_rel_commune_year,"university"),
    pct_edu_postgraduate=wshare(edu_group,fep_rel_commune_year,"postgraduate"),
    pct_tertiary=wshare(tertiary,fep_rel_commune_year),
    pct_employed=wshare(employed,fep_rel_commune_year),
    pct_formal=wshare(formal_employee,fep_rel_commune_year),
    pct_informal=wshare(informal_employee,fep_rel_commune_year),
    pct_violence_victim=wshare(violence,fep_rel_commune_year),
    n_valid_violence=sum(!is.na(violence)),
    pct_civil_1=wshare(civil_status,fep_rel_commune_year,1),
    pct_civil_2=wshare(civil_status,fep_rel_commune_year,2),
    pct_civil_3=wshare(civil_status,fep_rel_commune_year,3),
    pct_civil_4=wshare(civil_status,fep_rel_commune_year,4),
    pct_sector_primary=wshare(economic_sector,fep_rel_commune_year,1),
    pct_sector_industry=wshare(economic_sector,fep_rel_commune_year,2),
    pct_sector_commerce=wshare(economic_sector,fep_rel_commune_year,3),
    pct_sector_public_transport=wshare(economic_sector,fep_rel_commune_year,4),
    pct_sector_fin_business=wshare(economic_sector,fep_rel_commune_year,5),
    pct_sector_social_gov_edu=wshare(economic_sector,fep_rel_commune_year,6),
    .groups="drop"
  )

# Hogares: deduplicación por hh_uid y pesos relativos dentro de comuna-año.
hh <- micro %>%
  filter(
    year<=2018,commune_id %in% 1:16,
    !is.na(barrio_id),barrio_id!="",
    !is.na(hh_uid),hh_uid!=""
  ) %>%
  arrange(year,hh_uid,desc(head_hh==1)) %>%
  group_by(year,hh_uid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(w_hh_raw=if_else(valid_w(fep_est),fep_est,fep_p)) %>%
  group_by(year,commune_id) %>%
  mutate(
    w_hh_rel=if_else(
      valid_w(w_hh_raw),
      w_hh_raw/mean(w_hh_raw[valid_w(w_hh_raw)],na.rm=TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(valid_w(w_hh_rel))

hh_rt <- hh %>%
  group_by(year,barrio_id) %>%
  summarise(
    n_households=n(),
    mean_stratum=wmean(stratum,w_hh_rel),
    pct_stratum1=wshare(stratum,w_hh_rel,1),
    pct_stratum2=wshare(stratum,w_hh_rel,2),
    pct_stratum3=wshare(stratum,w_hh_rel,3),
    pct_stratum4=wshare(stratum,w_hh_rel,4),
    pct_stratum5=wshare(stratum,w_hh_rel,5),
    pct_stratum6=wshare(stratum,w_hh_rel,6),
    pct_area1=wshare(area,w_hh_rel,1),
    pct_area2=wshare(area,w_hh_rel,2),
    pct_dwelling_type1=wshare(dwelling_type,w_hh_rel,1),
    pct_dwelling_type2=wshare(dwelling_type,w_hh_rel,2),
    pct_dwelling_type3=wshare(dwelling_type,w_hh_rel,3),
    pct_female_head=wshare(female_head_hh,w_hh_rel),
    pct_adequate_walls=ifelse(
      any(!is.na(precarious_walls)),
      1-wshare(precarious_walls,w_hh_rel),NA_real_
    ),
    pct_adequate_floor=ifelse(
      any(!is.na(precarious_floor)),
      1-wshare(precarious_floor,w_hh_rel),NA_real_
    ),
    pct_electricity=wshare(electricity,w_hh_rel),
    pct_water=wshare(water,w_hh_rel),
    pct_sewerage=wshare(sewerage,w_hh_rel),
    pct_internet=wshare(internet,w_hh_rel),
    pct_refrigerator=wshare(refrigerator,w_hh_rel),
    pct_washer=wshare(washer,w_hh_rel),
    pct_computer=wshare(computer,w_hh_rel),
    pct_housing_deprivation=wshare(housing_deprivation,w_hh_rel),
    housing_deprivation_intensity=wmean(housing_deprivation_intensity,w_hh_rel),
    mean_housing_domains_observed=wmean(housing_domains_observed,w_hh_rel),
    n_valid_electricity=sum(!is.na(electricity)),
    n_valid_water=sum(!is.na(water)),
    n_valid_sewerage=sum(!is.na(sewerage)),
    n_valid_internet=sum(!is.na(internet)),
    n_valid_refrigerator=sum(!is.na(refrigerator)),
    n_valid_washer=sum(!is.na(washer)),
    n_valid_computer=sum(!is.na(computer)),
    .groups="drop"
  )

master <- person_rt %>%
  full_join(hh_rt,by=c("year","barrio_id")) %>%
  left_join(place_effects,by=c("year","barrio_id","commune_id","commune_name","barrio_name")) %>%
  left_join(observed_wage,by=c("year","barrio_id")) %>%
  arrange(year,commune_id,barrio_id)

# Si nombres difieren por pequeñas inconsistencias, rescatar place effects por llave.
missing_pe <- sum(is.na(master$theta_eb_total))
if(missing_pe>0){
  master <- master %>%
    dplyr::select(-dplyr::any_of(c(
      "n_mincer","sum_w_norm","sum_w2_norm","n_eff_kish",
      "theta_barrio_eb_raw","theta_barrio_eb_posterior_se",
      "theta_commune_eb_raw","theta_commune_eb_posterior_se",
      "theta_eb_total_raw","theta_within_commune_raw","reliability_barrio",
      "theta_eb_total_se","theta_eb_total","cswp_eb_pct",
      "theta_within_commune","cswp_within_commune_pct",
      "theta_eb_lo95","theta_eb_hi95","cswp_eb_lo95_pct","cswp_eb_hi95_pct",
      "precision_flag","theta_direct","cswp_direct_pct",
      "shrinkage_log","shrinkage_abs","shrinkage_cswp_pp"
    ))) %>%
    left_join(
      place_effects %>%
        dplyr::select(-commune_name,-barrio_name),
      by=c("year","barrio_id","commune_id")
    )
}

# =============================================================================
# 15. BASE PUENTE PARA EL SIGUIENTE SCRIPT ESPACIAL
# =============================================================================
# Esta es la base mínima recomendada para unir después:
# homicidios, violencia histórica, SPI, establecimientos financieros,
# Metro/Metrocable, densidad poblacional, shapefile y W.

bridge <- master %>%
  dplyr::select(
    year,commune_id,commune_name,barrio_id,barrio_name,
    n_mincer,n_eff_kish,reliability_barrio,precision_flag,
    theta_direct,cswp_direct_pct,
    theta_eb_total,theta_eb_total_se,
    theta_eb_lo95,theta_eb_hi95,
    cswp_eb_pct,cswp_eb_lo95_pct,cswp_eb_hi95_pct,
    theta_within_commune,cswp_within_commune_pct,
    observed_wage_mean_2018,observed_wage_median_2018,
    observed_gap_pct,
    mean_edu_years,pct_tertiary,pct_employed,pct_formal,pct_informal,
    pct_violence_victim,
    mean_stratum,pct_housing_deprivation,housing_deprivation_intensity,
    pct_internet,pct_computer,
    n_person,n_households
  ) %>%
  arrange(year,commune_id,barrio_id)

readr::write_csv(
  bridge,
  file.path(DIRS["bridge"],"BASE_PUENTE_PLACE_EFFECTS_PARA_SCRIPT03.csv"),
  na=""
)
saveRDS(
  bridge,
  file.path(DIRS["bridge"],"BASE_PUENTE_PLACE_EFFECTS_PARA_SCRIPT03.rds")
)

# Diccionario mínimo de variables críticas.
dictionary <- tibble(
  variable=c(
    "theta_direct","cswp_direct_pct",
    "theta_eb_total","cswp_eb_pct",
    "theta_eb_total_se","reliability_barrio","n_eff_kish","precision_flag",
    "theta_within_commune","cswp_within_commune_pct",
    "pct_violence_victim"
  ),
  interpretation=c(
    "FE barrio-año directo de Mincer, centrado por año; benchmark sin partial pooling",
    "Transformación porcentual del FE directo",
    "Place effect EB total: componente comuna-año + desviación barrio-año, centrado por año",
    "Conditional Neighborhood Wage Premium/Penalty EB (%)",
    "Error posterior aproximado del place effect EB total",
    "Confiabilidad aproximada del componente barrial, 0-1",
    "Tamaño efectivo Kish de la celda Mincer barrio-año",
    "Clasificación HIGH/MEDIUM/LOW según soporte muestral y reliability",
    "Desviación salarial condicional del barrio respecto de su propia comuna",
    "Transformación porcentual de la heterogeneidad salarial intra-comuna",
    "Victimización ECV descriptiva; NO sustituye homicidios externos ni el SPI"
  ),
  role_next_stage=c(
    "robustness","robustness",
    "main_outcome_candidate","main_outcome_candidate",
    "precision","precision","precision","precision",
    "within_commune_outcome","within_commune_outcome",
    "context_control_or_validation"
  )
)

readr::write_csv(
  dictionary,
  file.path(DIRS["bridge"],"DICCIONARIO_BASE_PUENTE_SCRIPT03.csv"),
  na=""
)

# =============================================================================
# 16. AUDITORÍAS DE COBERTURA Y PRECISIÓN
# =============================================================================
availability_vars <- c(
  "theta_direct","theta_eb_total","cswp_eb_pct","theta_within_commune",
  "mean_edu_years","pct_tertiary","pct_formal","pct_informal",
  "pct_violence_victim","pct_housing_deprivation"
)

availability <- tidyr::expand_grid(
  year=2004:2018,variable=availability_vars
) %>%
  rowwise() %>%
  mutate(
    n_barrio=sum(master$year==year & !is.na(master[[variable]])),
    n_barrio_total=sum(master$year==year),
    pct_valid=ifelse(n_barrio_total>0,n_barrio/n_barrio_total,NA_real_),
    status=case_when(
      n_barrio==0 ~ "NO DISPONIBLE",
      pct_valid<.80 ~ "DISPONIBILIDAD PARCIAL",
      TRUE ~ "DISPONIBLE"
    )
  ) %>%
  ungroup()

precision_audit <- place_effects %>%
  count(year,precision_flag,name="n_barrio_year") %>%
  group_by(year) %>%
  mutate(share=n_barrio_year/sum(n_barrio_year)) %>%
  ungroup()

sample_thresholds <- place_effects %>%
  group_by(year) %>%
  summarise(
    n_total=n(),
    n_ge5=sum(n_mincer>=5,na.rm=TRUE),
    n_ge10=sum(n_mincer>=10,na.rm=TRUE),
    n_ge20=sum(n_mincer>=20,na.rm=TRUE),
    n_ge30=sum(n_mincer>=30,na.rm=TRUE),
    n_eff_ge10=sum(n_eff_kish>=10,na.rm=TRUE),
    n_eff_ge20=sum(n_eff_kish>=20,na.rm=TRUE),
    .groups="drop"
  )

readr::write_csv(availability,file.path(DIRS["audit"],"DISPONIBILIDAD_VARIABLE_BARRIO_YEAR.csv"),na="")
readr::write_csv(precision_audit,file.path(DIRS["audit"],"AUDITORIA_PRECISION_PLACE_EFFECTS.csv"),na="")
readr::write_csv(sample_thresholds,file.path(DIRS["audit"],"AUDITORIA_UMBRALES_MUESTRA.csv"),na="")

# =============================================================================
# 17. GRÁFICAS DE DIAGNÓSTICO
# =============================================================================
theme_paper <- ggplot2::theme_minimal(base_size=11) +
  ggplot2::theme(
    panel.grid.minor=ggplot2::element_blank(),
    legend.position="bottom",
    plot.title=ggplot2::element_text(face="bold")
  )

# A. Directo vs EB.
p1 <- ggplot(place_effects,aes(x=cswp_direct_pct,y=cswp_eb_pct)) +
  geom_hline(yintercept=0,linetype=2,linewidth=.3) +
  geom_vline(xintercept=0,linetype=2,linewidth=.3) +
  geom_point(aes(alpha=pmin(n_eff_kish/30,1)),size=1.3) +
  geom_abline(slope=1,intercept=0,linetype=3) +
  labs(
    title="Direct versus Empirical-Bayes neighborhood wage effects",
    subtitle="Low-support barrio-years are partially pooled toward their commune-year",
    x="Direct CSWP (%)",y="Empirical-Bayes CSWP (%)",alpha="Effective N"
  ) + theme_paper
print(p1)
ggsave(file.path(DIRS["figures"],"FIG01_DIRECT_VS_EB_CSWP.png"),p1,width=7.5,height=6,dpi=320)

# B. Shrinkage vs tamaño efectivo.
p2 <- ggplot(place_effects,aes(x=n_eff_kish,y=shrinkage_abs)) +
  geom_point(alpha=.35,size=1.2) +
  geom_smooth(method="loess",se=TRUE,linewidth=.7) +
  scale_x_log10() +
  labs(
    title="Shrinkage decreases with effective barrio-year sample size",
    x="Kish effective sample size (log scale)",
    y="Absolute change in log place effect"
  ) + theme_paper
print(p2)
ggsave(file.path(DIRS["figures"],"FIG02_SHRINKAGE_VS_EFFECTIVE_N.png"),p2,width=7.5,height=5.5,dpi=320)

# C. Distribución anual del outcome recomendado.
p3 <- ggplot(place_effects,aes(x=factor(year),y=cswp_eb_pct)) +
  geom_hline(yintercept=0,linetype=2,linewidth=.3) +
  geom_boxplot(outlier.alpha=.20) +
  labs(
    title="Conditional neighborhood wage premiums/penalties after partial pooling",
    x=NULL,y="Empirical-Bayes CSWP (%)"
  ) + theme_paper +
  theme(axis.text.x=element_text(angle=45,hjust=1))
print(p3)
ggsave(file.path(DIRS["figures"],"FIG03_CSWP_EB_BY_YEAR.png"),p3,width=9,height=5.5,dpi=320)

# D. Heterogeneidad intra-comuna.
p4 <- ggplot(place_effects,aes(x=factor(year),y=cswp_within_commune_pct)) +
  geom_hline(yintercept=0,linetype=2,linewidth=.3) +
  geom_boxplot(outlier.alpha=.20) +
  labs(
    title="Within-commune neighborhood wage heterogeneity",
    subtitle="Barrio deviation from its commune-year conditional wage component",
    x=NULL,y="Within-commune CSWP (%)"
  ) + theme_paper +
  theme(axis.text.x=element_text(angle=45,hjust=1))
print(p4)
ggsave(file.path(DIRS["figures"],"FIG04_WITHIN_COMMUNE_CSWP_BY_YEAR.png"),p4,width=9,height=5.5,dpi=320)

# E. Reliability.
p5 <- ggplot(place_effects,aes(x=n_eff_kish,y=reliability_barrio)) +
  geom_point(alpha=.35,size=1.2) +
  geom_smooth(method="loess",se=FALSE,linewidth=.7) +
  scale_x_log10() +
  labs(
    title="Reliability of barrio-year place effects",
    x="Kish effective sample size (log scale)",
    y="Approximate reliability"
  ) + theme_paper
print(p5)
ggsave(file.path(DIRS["figures"],"FIG05_RELIABILITY_VS_EFFECTIVE_N.png"),p5,width=7.5,height=5.5,dpi=320)

# =============================================================================
# 18. EXPORTAR MASTER
# =============================================================================
readr::write_csv(master,MASTER_CSV,na="")
saveRDS(master,MASTER_RDS)

openxlsx::write.xlsx(
  list(
    MASTER_BARRIO_YEAR=master,
    BASE_PUENTE_SCRIPT03=bridge,
    PLACE_EFFECTS=place_effects,
    DIAGNOSTICO_PLACE_EFFECTS=diag_place,
    PRECISION=precision_audit,
    UMBRALES_MUESTRA=sample_thresholds,
    DISPONIBILIDAD=availability,
    FEP_AUDIT=fep_audit,
    VAR_COMPONENTS=variance_components,
    DICCIONARIO=dictionary,
    DESCRIPTIVAS_MINCER=desc,
    AUDITORIA_SALARIO=wage_audit
  ),
  MASTER_XLSX,
  overwrite=TRUE
)

# Copias analíticas.
readr::write_csv(master,file.path(DIRS["data"],"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.csv"),na="")
saveRDS(master,file.path(DIRS["data"],"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.rds"))

# =============================================================================
# 19. QA FINAL
# =============================================================================
if(anyDuplicated(master[c("year","barrio_id")]))
  stop("QA FINAL 02D: MASTER tiene duplicados barrio-año.")

if(any(!place_effects$year %in% 2004:2018))
  stop("QA FINAL 02D: place effects fuera de 2004–2018.")

if(any(!is.finite(place_effects$theta_eb_total)))
  stop("QA FINAL 02D: theta_eb_total contiene valores no finitos.")

if(any(place_effects$reliability_barrio<0 | place_effects$reliability_barrio>1,na.rm=TRUE))
  stop("QA FINAL 02D: reliability fuera de [0,1].")

if(!file.exists(MASTER_CSV)) stop("QA FINAL 02D: no se creó MASTER_CSV.")
if(!file.exists(MASTER_RDS)) stop("QA FINAL 02D: no se creó MASTER_RDS.")
if(!file.exists(MASTER_XLSX)) stop("QA FINAL 02D: no se creó MASTER_XLSX.")

BRIDGE_CSV <- file.path(DIRS["bridge"],"BASE_PUENTE_PLACE_EFFECTS_PARA_SCRIPT03.csv")
if(!file.exists(BRIDGE_CSV)) stop("QA FINAL 02D: no se creó BASE_PUENTE para Script 03.")

cat("\n====================================================================\n")
cat("SCRIPT 02D V1 FINALIZADO CORRECTAMENTE\n")
cat("====================================================================\n")
cat("MASTER: ",MASTER_CSV,"\n")
cat("BASE PUENTE SCRIPT 03: ",BRIDGE_CSV,"\n")
cat("N barrio-año MASTER: ",nrow(master),"\n")
cat("N place effects EB: ",nrow(place_effects),"\n")
cat("Mediana reliability: ",median(place_effects$reliability_barrio,na.rm=TRUE),"\n")
cat("Place effects LOW precision: ",sum(place_effects$precision_flag=="LOW",na.rm=TRUE),"\n")
cat("Modelo jerárquico singular: ",lme4::isSingular(m_hier,tol=1e-5),"\n")
cat("====================================================================\n")
