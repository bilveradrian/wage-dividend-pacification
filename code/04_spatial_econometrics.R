# =============================================================================
# SCRIPT 04 V5 — ROBUST SPATIAL ECONOMETRICS OF NEIGHBORHOOD WAGE EFFECTS
# Medellín, 2006–2018
# =============================================================================
# PURPOSE
#   Rebuild the econometric strategy from first principles:
#   1) define staged specifications before spatial estimation;
#   2) distinguish historical from contemporary violence;
#   3) verify the algebraic SPI re-parameterization;
#   4) express neighborhood event/density rates per 10,000 inhabitants;
#   5) define the complete balanced estimation sample FIRST;
#   6) rebuild Queen, Rook, KNN4, KNN6 and KNN8 W matrices ONLY for those barrios;
#   7) estimate TWFE + SAR/SEM/SLX/SDM/SDEM on an identical sample;
#   8) report spatial diagnostics, restrictions and residual dependence;
#   9) calculate SAR/SDM direct, indirect and total impacts using the full
#      joint covariance matrix of rho, beta and theta;
#  10) produce publication-style Word tables (estimate + [95% CI] + stars);
#  11) keep IV as an explicit identification module, but DO NOT manufacture
#      an IV from variables whose exclusion restriction is not defensible.
#
# MAIN OUTCOME
#   theta_eb_total = empirical-Bayes conditional barrio-year wage effect
#
# MAIN ECONOMETRIC SPECIFICATION (CORE)
#   theta_bt = beta_H*z_historical_violence_bt
#            + beta_C*z_current_homicide_bt
#            + gamma1*pct_informal_bt
#            + gamma2*pct_tertiary_bt
#            + gamma3*pct_housing_deprivation_bt
#            + barrio FE + year FE + e_bt
#
# SPATIAL GENERALIZATION
#   SDM: y = rho*W*y + X*beta + W*X*theta + barrio FE + year FE + e
#
# IMPORTANT
#   SPI_dynamic = z_historical_violence - z_current_homicide.
#   Therefore NEVER estimate SPI + z_historical + z_current simultaneously.
#   The script explicitly verifies that:
#       [z_historical + z_current] and [SPI + z_current]
#   are algebraically equivalent parameterizations on the same sample.
#
# REFERENCES
#   LeSage & Pace (2009), Introduction to Spatial Econometrics.
#   Elhorst (2010), Spatial Economic Analysis 5(1): 9–28.
#   Elhorst (2014), Spatial Econometrics: From Cross-Sectional Data to Spatial Panels.
#   Millo & Piras (2012), Journal of Statistical Software 47(1): 1–38.
# =============================================================================

rm(list=ls()); gc()
options(stringsAsFactors=FALSE, scipen=999, width=180)
set.seed(20260919)

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
REQ <- c(
  "data.table","dplyr","tidyr","sf","spdep","splm","plm","fixest",
  "lmtest","sandwich","Matrix","MASS","broom","ggplot2","openxlsx",
  "officer","flextable","stringr","car","rlang","tibble"
)
missing_pkg <- REQ[!vapply(REQ, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing_pkg)){
  stop("Install required packages first: install.packages(c(",
       paste0('"',missing_pkg,'"',collapse=","), "))")
}
invisible(lapply(REQ, library, character.only=TRUE))

# -----------------------------------------------------------------------------
# 1. PATHS
# -----------------------------------------------------------------------------
DATA_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos/ecv_medellin_2004_2025/consolidado_2004_2025"
WAGES_ROOT <- normalizePath(file.path(DATA_DIR,"..",".."), winslash="/", mustWork=TRUE)

find_first <- function(candidates, pattern=NULL, roots=c(WAGES_ROOT)){
  for(z in candidates){
    if(file.exists(z)) return(normalizePath(z,winslash="/",mustWork=TRUE))
  }
  if(!is.null(pattern)){
    ff <- unique(unlist(lapply(roots,function(r){
      if(dir.exists(r)) list.files(r,pattern=pattern,recursive=TRUE,
                                   full.names=TRUE,ignore.case=TRUE)
      else character()
    })))
    if(length(ff)) return(normalizePath(ff[1],winslash="/",mustWork=TRUE))
  }
  stop("File not found. Pattern: ",pattern)
}

MASTER_FILE <- find_first(
  c(file.path(WAGES_ROOT,"outputs_master","03_spatial_master_esda",
              "01_MASTER_DATA","MASTER_SPATIAL_BARRIO_YEAR_2004_2018.csv")),
  "^MASTER_SPATIAL_BARRIO_YEAR_2004_2018.*\\.csv$"
)
SHP_FILE <- find_first(
  c(file.path(WAGES_ROOT,"Mapas","BarrioVereda_2014.shp")),
  "^BarrioVereda_2014\\.shp$"
)

OUT_DIR <- file.path(WAGES_ROOT,"outputs_master","04_spatial_econometrics")
DIR <- list(
  qa=file.path(OUT_DIR,"01_QA"),
  samples=file.path(OUT_DIR,"02_SAMPLES"),
  twfe=file.path(OUT_DIR,"03_TWFE"),
  weights=file.path(OUT_DIR,"04_WEIGHTS"),
  models=file.path(OUT_DIR,"05_SPATIAL_MODELS"),
  impacts=file.path(OUT_DIR,"06_IMPACTS"),
  robust=file.path(OUT_DIR,"07_ROBUSTNESS"),
  iv=file.path(OUT_DIR,"08_IV_READINESS"),
  paper=file.path(OUT_DIR,"09_PAPER_TABLES")
)
invisible(lapply(DIR,dir.create,recursive=TRUE,showWarnings=FALSE))

cat("\n",strrep("=",100),"\n",sep="")
cat("SCRIPT 04 V5 — ROBUST SPATIAL ECONOMETRICS\n")
cat("MASTER :",MASTER_FILE,"\n")
cat("SHAPE  :",SHP_FILE,"\n")
cat("OUTPUT :",OUT_DIR,"\n")
cat(strrep("=",100),"\n\n",sep="")

# -----------------------------------------------------------------------------
# 2. LOAD MASTER + CONSTRUCT INTERPRETABLE RATES
# -----------------------------------------------------------------------------
d <- data.table::fread(MASTER_FILE) |> as.data.frame()
d$year <- as.integer(d$year)
d$commune_id <- as.integer(d$commune_id)
d$barrio_id <- sprintf("%04d",as.integer(d$barrio_id))

needed_core <- c(
  "theta_eb_total","theta_eb_total_se","reliability_barrio",
  "SPI_dynamic","z_historical_violence","z_current_homicide",
  "pct_informal","pct_tertiary","pct_housing_deprivation"
)
miss_core <- setdiff(needed_core,names(d))
if(length(miss_core)) stop("Missing required core variables: ",paste(miss_core,collapse=", "))

# Rates per 10,000 inhabitants: interpretation only; z-scores remain unchanged.
if("homicide_rate_100k" %in% names(d))
  d$homicide_rate_10k <- d$homicide_rate_100k/10
if("robbery_person_rate_100k" %in% names(d))
  d$robbery_person_rate_10k <- d$robbery_person_rate_100k/10
if(all(c("arrests","population") %in% names(d)))
  d$arrests_rate_10k <- ifelse(d$population>0,10000*d$arrests/d$population,NA_real_)
if("financial_per_10k" %in% names(d))
  d$financial_density_10k <- d$financial_per_10k
if(!"log_population" %in% names(d) && "population" %in% names(d))
  d$log_population <- ifelse(d$population>0,log(d$population),NA_real_)

# V10: arrests are NEVER used in levels. The preferred models use only
# contemporaneous arrests per 10,000 inhabitants. Lagged arrests are deliberately
# excluded from the wage specification.

# Objective legacy/stigma proxies. No variable is selected because of its p-value.
# The primary nonlinear proxy identifies places whose historical violence remains
# worse than their current violence: a positive historical-current reputation gap.
d$stigma_legacy_gap <- pmax(d$SPI_dynamic,0)
d$stigma_recovery_gap <- pmin(d$SPI_dynamic,0)
# A deliberately coarse robustness measure: historically high but currently low.
d <- d |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    stigma_highpast_lowcurrent=as.integer(
      z_historical_violence >= stats::quantile(z_historical_violence,.75,na.rm=TRUE) &
      z_current_homicide <= stats::quantile(z_current_homicide,.50,na.rm=TRUE)
    )
  ) |>
  dplyr::ungroup()

# V10 objective "excess historical violence" measure.
# Within each year, remove the component of historical violence linearly associated
# with current homicide. The positive residual identifies neighborhoods whose
# historical violence is unusually high relative to their current violence.
# This is defined ex ante and is NOT selected according to its p-value.
d <- d |>
  dplyr::group_by(year) |>
  dplyr::group_modify(~{
    z <- .x
    ok <- is.finite(z$z_historical_violence) & is.finite(z$z_current_homicide)
    z$legacy_residual <- NA_real_
    if(sum(ok) >= 10 && stats::sd(z$z_current_homicide[ok]) > 0){
      rr <- stats::lm(z_historical_violence ~ z_current_homicide, data=z[ok,,drop=FALSE])
      z$legacy_residual[ok] <- stats::residuals(rr)
    }
    z
  }) |>
  dplyr::ungroup()
d$legacy_residual_positive <- pmax(d$legacy_residual,0)

# Persistent-violence measures: EXACTLY the same annual definitions as Script 03.
# These are recomputed here as an audit; if Script 03 exported them, equality is
# checked below. HPHC is the direct persistence indicator. PVI is continuous.
old_hphc <- if("stigma_highpast_highcurrent" %in% names(d)) d$stigma_highpast_highcurrent else NULL
old_pvi  <- if("persistent_violence_intensity" %in% names(d)) d$persistent_violence_intensity else NULL
d <- d |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    q75_hist = stats::quantile(z_historical_violence,.75,na.rm=TRUE,names=FALSE),
    q75_curr = stats::quantile(z_current_homicide,.75,na.rm=TRUE,names=FALSE),
    stigma_highpast_highcurrent=as.integer(
      z_historical_violence >= q75_hist & z_current_homicide >= q75_curr),
    persistent_violence_intensity=pmax(pmin(z_historical_violence,z_current_homicide),0),
    violence_regime_q75=dplyr::case_when(
      !is.finite(z_historical_violence) | !is.finite(z_current_homicide) ~ NA_character_,
      z_historical_violence >= q75_hist & z_current_homicide >= q75_curr ~ "High past / high current",
      z_historical_violence >= q75_hist & z_current_homicide < q75_curr ~ "High past / not-high current",
      z_historical_violence < q75_hist & z_current_homicide >= q75_curr ~ "Not-high past / high current",
      TRUE ~ "Not-high past / not-high current"),
    regime_HPLC_q75=as.integer(violence_regime_q75=="High past / not-high current"),
    regime_LPHC_q75=as.integer(violence_regime_q75=="Not-high past / high current"),
    regime_HPHC_q75=as.integer(violence_regime_q75=="High past / high current")
  ) |> dplyr::ungroup() |> dplyr::select(-q75_hist,-q75_curr)
if(!is.null(old_hphc) && !isTRUE(all.equal(old_hphc,d$stigma_highpast_highcurrent,check.attributes=FALSE)))
  stop("Script 03/04 inconsistency in stigma_highpast_highcurrent.")
if(!is.null(old_pvi) && !isTRUE(all.equal(old_pvi,d$persistent_violence_intensity,check.attributes=FALSE)))
  stop("Script 03/04 inconsistency in persistent_violence_intensity.")

# FINAL PAPER SCALE: financial establishments per 100,000 inhabitants.
# This is a pure rescaling of the existing density measure; it preserves the
# underlying information while improving interpretation in the paper.
if(!"financial_density_10k" %in% names(d))
  stop("financial_density_10k is required to construct financial_density_100k.")
d$financial_density_100k <- 10 * d$financial_density_10k

MAIN_YEARS <- 2006:2018
Y_MAIN <- "theta_eb_total"

# -----------------------------------------------------------------------------
# 3. PRE-SPECIFIED MODEL BLOCKS — V10 WAGE-SETTING CONTEXT
# -----------------------------------------------------------------------------
# V10.12 preferred specification: no economic-sector shares.
# Informality is retained instead of formality, at the user's request.
# The same contextual controls are used across the historical, legacy-gap,
# high-past/low-current and residualized-legacy models.
WAGE_CONTEXT <- c(
  "pct_informal","pct_tertiary","pct_housing_deprivation",
  "financial_density_100k","distance_nearest_metrocable_km","arrests_rate_10k",
  "pct_female_head","mean_age","mean_stratum"
)
missing_context <- setdiff(WAGE_CONTEXT,names(d))
if(length(missing_context)) stop("V10 missing requested wage-context variables: ",
                                 paste(missing_context,collapse=", "))

SPEC <- list(
  S0_SPI = c("SPI_dynamic"),
  S1_HISTORY_CURRENT = c("z_historical_violence","z_current_homicide"),
  S1B_SPI_CURRENT = c("SPI_dynamic","z_current_homicide"),
  # Historical-violence benchmark under the full wage-setting context.
  S5_EXTENDED_CONTEXT = c("z_historical_violence","z_current_homicide",WAGE_CONTEXT),
  # Positive historical-current standardized violence gap.
  S6_STIGMA_LEGACY = c("stigma_legacy_gap","z_current_homicide",WAGE_CONTEXT),
  # Coarse nonlinear robustness.
  S6B_STIGMA_HIGHPAST_LOWCURRENT = c("stigma_highpast_lowcurrent",
                                     "z_current_homicide",WAGE_CONTEXT),
  # Direct persistence hypothesis: high historical AND high current violence.
  # Current homicide is not added separately because it defines the treatment.
  S6C_PERSISTENT_HIGH_VIOLENCE = c("stigma_highpast_highcurrent",WAGE_CONTEXT),
  # Continuous joint persistence intensity; again no separate current-homicide control.
  S6D_PERSISTENCE_INTENSITY = c("persistent_violence_intensity",WAGE_CONTEXT),
  # Q75 regime comparison; omitted category = not-high past / not-high current.
  S6E_VIOLENCE_REGIMES = c("regime_HPLC_q75","regime_LPHC_q75","regime_HPHC_q75",WAGE_CONTEXT),
  # Preferred V10 objective excess-legacy specification.
  S7_RESIDUALIZED_LEGACY = c("legacy_residual_positive",
                              "z_current_homicide",WAGE_CONTEXT)
)

# Spatial benchmark follows the transparent historical + current decomposition
# with the SAME wage-setting controls used in Table 3A.
X_MAIN <- SPEC$S5_EXTENDED_CONTEXT

EXPECTED_SIGNS <- tibble::tribble(
  ~variable,~construct,~expected_sign,~role,
  "SPI_dynamic","Historical violence relative to current violence","-","Alternative parameterization",
  "z_historical_violence","Lagged cumulative historical homicide exposure","-","Historical exposure benchmark",
  "z_current_homicide","Contemporary homicide intensity","-","Current violence control",
  "pct_informal","Neighborhood labor informality","-","Labor-market composition",
  "pct_tertiary","Share with tertiary education","+","Human-capital composition",
  "pct_housing_deprivation","Housing deprivation","-","Neighborhood deprivation",
  "financial_density_10k","Financial establishments per 10,000 inhabitants","Ambiguous","Local economic structure",
  "distance_nearest_metrocable_km","Distance to nearest open Metrocable station","Ambiguous","Accessibility",
  "arrests_rate_10k","Arrests per 10,000 inhabitants","Ambiguous","Contemporaneous conflict/policing intensity",
  "pct_female_head","Share of female-headed households","Ambiguous","Household demographic structure",
  "mean_age","Mean age","Ambiguous","Demographic composition",
  "mean_stratum","Mean socioeconomic stratum","+","Socioeconomic residential composition",
  "stigma_legacy_gap","Positive historical-minus-current standardized violence gap","Ambiguous","Objective violence-legacy gap",
  "stigma_highpast_lowcurrent","Top-quartile historical and below-median current violence","Ambiguous","Recovery/nonlinear legacy contrast",
  "stigma_highpast_highcurrent","Top-quartile historical and top-quartile current violence","-","Primary persistent-violence indicator",
  "persistent_violence_intensity","max[min(z historical,z current),0]","-","Continuous persistent-violence intensity",
  "regime_HPLC_q75","High historical / not-high current (Q75)","Ambiguous","Violence-regime contrast",
  "regime_LPHC_q75","Not-high historical / high current (Q75)","-","Emergent-current-violence contrast",
  "regime_HPHC_q75","High historical / high current (Q75)","-","Persistent-violence regime contrast",
  "legacy_residual_positive","Positive historical-violence residual conditional on current homicide","Ambiguous","Preferred objective excess-legacy proxy"
)
write.csv(EXPECTED_SIGNS,file.path(DIR$qa,"QA00_expected_signs_and_roles.csv"),row.names=FALSE)

cat("PRE-SPECIFIED MODEL BLOCKS\n")
for(nm in names(SPEC)) cat(sprintf("  %-22s : %s\n",nm,paste(SPEC[[nm]],collapse=", ")))
cat("\n")

# -----------------------------------------------------------------------------
# 4. VARIABLE COVERAGE + PANEL VARIATION
# -----------------------------------------------------------------------------
all_spec_vars <- unique(c(Y_MAIN,unlist(SPEC)))
coverage <- d |>
  filter(year %in% MAIN_YEARS,commune_id %in% 1:16) |>
  group_by(year) |>
  summarise(n_rows=n(),across(all_of(all_spec_vars),~sum(is.finite(.x))),.groups="drop")
write.csv(coverage,file.path(DIR$qa,"QA01_variable_coverage_by_year.csv"),row.names=FALSE)
cat("\nQA01 — VARIABLE COVERAGE\n"); print(coverage,n=Inf,width=Inf)

decomp_var <- function(dat,v){
  z <- dat |> filter(is.finite(.data[[v]]))
  overall <- sd(z[[v]],na.rm=TRUE)
  means <- z |> group_by(barrio_id) |> summarise(m=mean(.data[[v]],na.rm=TRUE),.groups="drop")
  between <- sd(means$m,na.rm=TRUE)
  wd <- z |> group_by(barrio_id) |> mutate(dev=.data[[v]]-mean(.data[[v]],na.rm=TRUE)) |>
    ungroup() |> pull(dev)
  within <- sd(wd,na.rm=TRUE)
  tibble(
    variable=v,N=nrow(z),mean=mean(z[[v]],na.rm=TRUE),
    sd_overall=overall,sd_between=between,sd_within=within,
    within_variance_share=ifelse((within^2+between^2)>0,within^2/(within^2+between^2),NA_real_),
    min=min(z[[v]],na.rm=TRUE),max=max(z[[v]],na.rm=TRUE)
  )
}
d_window <- d |> filter(year %in% MAIN_YEARS,commune_id %in% 1:16)
VARIATION <- bind_rows(lapply(all_spec_vars,decomp_var,dat=d_window))
write.csv(VARIATION,file.path(DIR$qa,"QA02_panel_variation.csv"),row.names=FALSE)
cat("\nQA02 — PANEL VARIATION\n"); print(VARIATION,n=Inf,width=Inf)

# -----------------------------------------------------------------------------
# 5. HISTORICAL VS CURRENT VIOLENCE: COLLINEARITY + SPI IDENTITY
# -----------------------------------------------------------------------------
viol <- d_window |>
  filter(if_all(all_of(c("SPI_dynamic","z_historical_violence","z_current_homicide")),is.finite))

viol_within <- viol |>
  group_by(barrio_id) |>
  mutate(
    wh=z_historical_violence-mean(z_historical_violence),
    wc=z_current_homicide-mean(z_current_homicide),
    ws=SPI_dynamic-mean(SPI_dynamic)
  ) |> ungroup()

CORR_VIOLENCE <- tibble(
  metric=c(
    "Pooled corr(Historical, Current)",
    "Pooled corr(Historical, SPI)",
    "Pooled corr(Current, SPI)",
    "Within-barrio corr(Historical, Current)",
    "Within-barrio corr(Historical, SPI)",
    "Within-barrio corr(Current, SPI)",
    "Max abs identity error: SPI - (Historical-Current)"
  ),
  value=c(
    cor(viol$z_historical_violence,viol$z_current_homicide,use="complete.obs"),
    cor(viol$z_historical_violence,viol$SPI_dynamic,use="complete.obs"),
    cor(viol$z_current_homicide,viol$SPI_dynamic,use="complete.obs"),
    cor(viol_within$wh,viol_within$wc,use="complete.obs"),
    cor(viol_within$wh,viol_within$ws,use="complete.obs"),
    cor(viol_within$wc,viol_within$ws,use="complete.obs"),
    max(abs(viol$SPI_dynamic-(viol$z_historical_violence-viol$z_current_homicide)),na.rm=TRUE)
  )
)
write.csv(CORR_VIOLENCE,file.path(DIR$qa,"QA03_historical_current_SPI_identity.csv"),row.names=FALSE)
cat("\nQA03 — HISTORICAL/CURRENT/SPI DIAGNOSTICS\n"); print(CORR_VIOLENCE,n=Inf)

# VIF and condition number for the preferred CORE regressors, pooled and within transformed.
core_cc <- d_window |> filter(if_all(all_of(c(Y_MAIN,X_MAIN)),is.finite))
vif_fit <- lm(as.formula(paste(Y_MAIN,"~",paste(X_MAIN,collapse="+"))),data=core_cc)
vif_raw <- tryCatch(car::vif(vif_fit),error=function(e) rep(NA_real_,length(X_MAIN)))
if(is.matrix(vif_raw)) vif_raw <- vif_raw[,1]
VIF_CORE <- tibble(variable=names(vif_raw),VIF=as.numeric(vif_raw))

within_X <- core_cc |>
  group_by(barrio_id) |>
  mutate(across(all_of(X_MAIN),~.x-mean(.x),.names="WTH_{.col}")) |>
  ungroup()
Xw <- as.matrix(within_X[,paste0("WTH_",X_MAIN),drop=FALSE])
Xw <- Xw[complete.cases(Xw),,drop=FALSE]
sv <- svd(scale(Xw,center=TRUE,scale=FALSE),nu=0,nv=0)$d
condition_number_within <- max(sv)/min(sv[sv>sqrt(.Machine$double.eps)])

write.csv(VIF_CORE,file.path(DIR$qa,"QA04_VIF_CORE.csv"),row.names=FALSE)
write.csv(data.frame(condition_number_within=condition_number_within),
          file.path(DIR$qa,"QA05_condition_number_within.csv"),row.names=FALSE)
cat("\nQA04 — CORE VIF\n"); print(VIF_CORE)
cat("\nQA05 — WITHIN CONDITION NUMBER:",condition_number_within,"\n")

# -----------------------------------------------------------------------------
# 6. DEFINE ESTIMATION SAMPLES BEFORE BUILDING W
# -----------------------------------------------------------------------------
make_balanced_sample <- function(vars,label){
  z <- d_window |> filter(if_all(all_of(c(Y_MAIN,vars)),is.finite))
  supp <- z |> count(barrio_id,name="T_obs")
  ids <- sort(supp$barrio_id[supp$T_obs==length(MAIN_YEARS)])
  out <- z |> filter(barrio_id %in% ids) |> arrange(barrio_id,year)
  stopifnot(nrow(out)==length(ids)*length(MAIN_YEARS))
  write.csv(data.frame(spec=label,N_barrio=length(ids),T=length(MAIN_YEARS),
                       NT=nrow(out),variables=paste(vars,collapse=" + ")),
            file.path(DIR$samples,paste0("SAMPLE_",label,".csv")),row.names=FALSE)
  list(data=out,ids=ids)
}

SAMPLES <- lapply(names(SPEC),function(nm) make_balanced_sample(SPEC[[nm]],nm))
names(SAMPLES) <- names(SPEC)

SAMPLE_SUMMARY <- bind_rows(lapply(names(SAMPLES),function(nm){
  tibble(spec=nm,N_barrio=length(SAMPLES[[nm]]$ids),
         T=length(MAIN_YEARS),NT=nrow(SAMPLES[[nm]]$data),
         K=length(SPEC[[nm]]))
}))
write.csv(SAMPLE_SUMMARY,file.path(DIR$samples,"SAMPLE_SUMMARY.csv"),row.names=FALSE)
cat("\nESTIMATION SAMPLE SUMMARY\n"); print(SAMPLE_SUMMARY,n=Inf)

# MAIN spatial sample = the preferred full wage-context benchmark.
# V10 no longer defines S2_CORE; using S5_EXTENDED_CONTEXT also guarantees that
# the spatial models and Table 3A are estimated on the same 186-barrios × 13-years
# complete balanced sample and with the requested wage-setting covariates.
panel <- SAMPLES$S5_EXTENDED_CONTEXT$data
ids_main <- SAMPLES$S5_EXTENDED_CONTEXT$ids
N <- length(ids_main); TT <- length(MAIN_YEARS); NT <- nrow(panel)

panel <- panel |>
  mutate(
    invvar_weight=ifelse(is.finite(theta_eb_total_se) & theta_eb_total_se>0,
                         1/pmax(theta_eb_total_se^2,1e-8),NA_real_),
    reliability_weight=ifelse(is.finite(reliability_barrio),
                              pmax(reliability_barrio,0.01),NA_real_)
  )

cat(sprintf("\nMAIN SPATIAL SAMPLE: %d barrios x %d years = %d barrio-years\n",N,TT,NT))

# -----------------------------------------------------------------------------
# 7. REBUILD GEOGRAPHY AND ALL W MATRICES ON MAIN COMPLETE SAMPLE ONLY
# -----------------------------------------------------------------------------
g <- sf::st_read(SHP_FILE,quiet=TRUE)
codevar <- if("CODIGO" %in% names(g)) "CODIGO" else
  names(g)[grep("barr|codigo|cod",names(g),ignore.case=TRUE)][1]
if(is.na(codevar) || !nzchar(codevar)) stop("Could not identify barrio code in shapefile.")

digits <- gsub("[^0-9]","",as.character(g[[codevar]]))
g$barrio_id <- ifelse(nchar(digits)>=4,
                      substr(digits,nchar(digits)-3,nchar(digits)),
                      sprintf("%04d",as.integer(digits)))
if("SUBTIPO_BA" %in% names(g)) g <- g[g$SUBTIPO_BA==1,]
g <- g[g$barrio_id %in% ids_main,]
g <- g[!duplicated(g$barrio_id),]
g <- g[match(ids_main,g$barrio_id),]
if(anyNA(g$barrio_id) || nrow(g)!=N)
  stop("Shapefile/main-sample mismatch. Expected ",N," barrios; found ",nrow(g))
stopifnot(identical(as.character(g$barrio_id),as.character(ids_main)))

# Contiguity matrices
nb_q <- spdep::poly2nb(g,queen=TRUE,row.names=g$barrio_id)
nb_r <- spdep::poly2nb(g,queen=FALSE,row.names=g$barrio_id)

# KNN matrices using projected point-on-surface coordinates
g_proj <- sf::st_transform(g,3116)
pts <- sf::st_coordinates(sf::st_point_on_surface(g_proj))
mk_knn <- function(k) spdep::knn2nb(spdep::knearneigh(pts,k=k),row.names=g$barrio_id)
nb_k4 <- mk_knn(4); nb_k6 <- mk_knn(6); nb_k8 <- mk_knn(8)

make_lw <- function(nb) spdep::nb2listw(nb,style="W",zero.policy=TRUE)
LW <- list(
  Queen=make_lw(nb_q),
  Rook=make_lw(nb_r),
  KNN4=make_lw(nb_k4),
  KNN6=make_lw(nb_k6),
  KNN8=make_lw(nb_k8)
)
NB <- list(Queen=nb_q,Rook=nb_r,KNN4=nb_k4,KNN6=nb_k6,KNN8=nb_k8)

W_DIAG <- bind_rows(lapply(names(NB),function(nm){
  nb <- NB[[nm]]
  tibble(
    W=nm,N=N,mean_neighbors=mean(spdep::card(nb)),
    median_neighbors=median(spdep::card(nb)),
    min_neighbors=min(spdep::card(nb)),max_neighbors=max(spdep::card(nb)),
    islands=sum(spdep::card(nb)==0),
    components=spdep::n.comp.nb(nb)$nc,
    symmetric=spdep::is.symmetric.nb(nb)
  )
}))
write.csv(W_DIAG,file.path(DIR$weights,"W_diagnostics_main_complete_sample.csv"),row.names=FALSE)
cat("\nSPATIAL WEIGHTS — BUILT ONLY ON MAIN COMPLETE SAMPLE\n"); print(W_DIAG,n=Inf)

saveRDS(list(geometry=g,NB=NB,LW=LW,ids=ids_main),
        file.path(DIR$weights,"WEIGHTS_MAIN_COMPLETE_SAMPLE.rds"))

# -----------------------------------------------------------------------------
# 8. STAGED TWFE MODELS + SPI RE-PARAMETERIZATION CHECK
# -----------------------------------------------------------------------------
fit_twfe <- function(dat,vars){
  pd <- plm::pdata.frame(dat,index=c("barrio_id","year"))
  fm <- as.formula(paste(Y_MAIN,"~",paste(vars,collapse="+")))
  m <- plm::plm(fm,data=pd,model="within",effect="twoways")
  vc <- plm::vcovSCC(m,type="HC1",maxlag=2)
  ct <- lmtest::coeftest(m,vcov.=vc)
  list(model=m,table=broom::tidy(ct),vcov=vc)
}

TWFE <- list()
for(nm in names(SPEC)){
  TWFE[[nm]] <- fit_twfe(SAMPLES[[nm]]$data,SPEC[[nm]])
  cat("\n",strrep("-",90),"\n",nm," — TWFE + Driscoll-Kraay\n",sep="")
  print(TWFE[[nm]]$table,n=Inf)
}

TWFE_ALL <- bind_rows(lapply(names(TWFE),function(nm)
  TWFE[[nm]]$table |> mutate(spec=nm,.before=1)))
write.csv(TWFE_ALL,file.path(DIR$twfe,"TWFE_staged_models_DK.csv"),row.names=FALSE)

# Exact equivalence check on SAME sample.
eq_dat <- SAMPLES$S1_HISTORY_CURRENT$data
m_hc <- plm::plm(theta_eb_total ~ z_historical_violence + z_current_homicide,
                 data=plm::pdata.frame(eq_dat,index=c("barrio_id","year")),
                 model="within",effect="twoways")
m_sc <- plm::plm(theta_eb_total ~ SPI_dynamic + z_current_homicide,
                 data=plm::pdata.frame(eq_dat,index=c("barrio_id","year")),
                 model="within",effect="twoways")
EQUIV <- tibble(
  diagnostic=c("Max abs fitted difference","Max abs residual difference",
               "RSS history+current","RSS SPI+current"),
  value=c(
    max(abs(fitted(m_hc)-fitted(m_sc)),na.rm=TRUE),
    max(abs(residuals(m_hc)-residuals(m_sc)),na.rm=TRUE),
    sum(residuals(m_hc)^2),sum(residuals(m_sc)^2)
  )
)
write.csv(EQUIV,file.path(DIR$qa,"QA06_SPI_reparameterization_equivalence.csv"),row.names=FALSE)
cat("\nSPI RE-PARAMETERIZATION EQUIVALENCE CHECK\n"); print(EQUIV,n=Inf)


# -----------------------------------------------------------------------------
# 9. MAIN TWFE SPATIAL DIAGNOSTICS — EACH W
# -----------------------------------------------------------------------------
pdat <- plm::pdata.frame(panel,index=c("barrio_id","year"))
f_main <- as.formula(paste(Y_MAIN,"~",paste(X_MAIN,collapse="+")))
m_twfe_main <- plm::plm(f_main,data=pdat,model="within",effect="twoways")
vc_dk_main <- plm::vcovSCC(m_twfe_main,type="HC1",maxlag=2)
MAIN_TWFE <- broom::tidy(lmtest::coeftest(m_twfe_main,vcov.=vc_dk_main))
write.csv(MAIN_TWFE,file.path(DIR$twfe,"MAIN_CORE_TWFE_DK.csv"),row.names=FALSE)

resid_twfe <- tibble(barrio_id=panel$barrio_id,year=panel$year,
                     resid=as.numeric(residuals(m_twfe_main)))

moran_by_W <- bind_rows(lapply(names(LW),function(wn){
  lw <- LW[[wn]]
  bind_rows(lapply(MAIN_YEARS,function(tt){
    rr <- resid_twfe |> filter(year==tt) |> arrange(match(barrio_id,ids_main))
    mt <- tryCatch(spdep::moran.mc(rr$resid,lw,nsim=999,zero.policy=TRUE),
                   error=function(e) NULL)
    if(is.null(mt)) tibble(W=wn,year=tt,Moran_I=NA_real_,p_perm=NA_real_)
    else tibble(W=wn,year=tt,Moran_I=as.numeric(mt$statistic),p_perm=mt$p.value)
  }))
}))
write.csv(moran_by_W,file.path(DIR$twfe,"MAIN_TWFE_residual_Moran_all_W.csv"),row.names=FALSE)

LM_ALL <- bind_rows(lapply(names(LW),function(wn){
  bind_rows(lapply(c("lml","lme","rlml","rlme"),function(z){
    o <- tryCatch(splm::slmtest(m_twfe_main,listw=LW[[wn]],test=z),
                  error=function(e) e)
    if(inherits(o,"error"))
      tibble(W=wn,test=z,statistic=NA_real_,p_value=NA_real_,note=conditionMessage(o))
    else
      tibble(W=wn,test=z,statistic=as.numeric(o$statistic),
             p_value=o$p.value,note="")
  }))
}))
write.csv(LM_ALL,file.path(DIR$twfe,"MAIN_TWFE_spatial_LM_all_W.csv"),row.names=FALSE)
cat("\nMAIN TWFE — DK COEFFICIENTS\n"); print(MAIN_TWFE,n=Inf)
cat("\nMAIN TWFE — SPATIAL LM/ROBUST LM\n"); print(LM_ALL,n=Inf)

# -----------------------------------------------------------------------------
# 10. SPATIAL MODEL ENGINE — V6 FAIL-SAFE
# -----------------------------------------------------------------------------
# Key change relative to V5:
# - each model is estimated independently;
# - one failed model NEVER discards the remaining models for that W;
# - spml extraction follows the documented object structure:
#     coefficients = beta slopes
#     arcoef       = spatial lag on y (rho)
#     errcomp      = spatial/error variance components
#     vcov         = covariance of beta slopes
#     vcov.arcoef  = variance of rho
#     vcov.errcomp = covariance of error components
# - every attempt is recorded in MODEL_RUN_STATUS.
# CRAN splm documentation: spml supports model="within", effect="twoways",
# lag=TRUE/FALSE and spatial.error in c("b","kkp","none").

add_WX <- function(dat,Wmat,vars,ids,years){
  out <- dat
  for(v in vars){
    zz <- lapply(years,function(tt){
      z <- out[out$year==tt,,drop=FALSE]
      z <- z[match(ids,z$barrio_id),,drop=FALSE]
      if(anyNA(z$barrio_id) || !identical(as.character(z$barrio_id),as.character(ids)))
        stop("Ordering mismatch while constructing W*",v," in year ",tt)
      data.frame(barrio_id=ids,year=tt,
                 value=as.numeric(Wmat %*% z[[v]]))
    })
    lagtab <- dplyr::bind_rows(zz)
    names(lagtab)[3] <- paste0("W_",v)
    out <- dplyr::left_join(out,lagtab,by=c("barrio_id","year"))
  }
  dplyr::arrange(out,barrio_id,year)
}

MODEL_RUN_STATUS <- tibble::tibble()
record_status <- function(W,model,status,message="",attempt=""){
  MODEL_RUN_STATUS <<- dplyr::bind_rows(
    MODEL_RUN_STATUS,
    tibble::tibble(W=W,model=model,status=status,attempt=attempt,message=message)
  )
}

fit_spml_retry <- function(fm,pd,lw,lag,error,Wname,Mname){
  attempts <- list(
    list(hess=FALSE,initval="estimate"),
    list(hess=FALSE,initval="zeros"),
    list(hess=TRUE, initval="estimate")
  )
  errs <- character()
  for(a in seq_along(attempts)){
    aa <- attempts[[a]]
    fit <- tryCatch(
      splm::spml(
        fm,data=pd,index=c("barrio_id","year"),listw=lw,
        model="within",effect="twoways",
        lag=lag,spatial.error=error,
        hess=aa$hess,initval=aa$initval
      ),
      error=function(e)e
    )
    if(!inherits(fit,"error")){
      record_status(Wname,Mname,"OK","",
                    paste0("hess=",aa$hess,"; initval=",aa$initval))
      return(fit)
    }
    errs <- c(errs,paste0("attempt ",a,": ",conditionMessage(fit)))
  }
  record_status(Wname,Mname,"FAILED",paste(errs,collapse=" | "),"all retries")
  NULL
}

fit_plm_safe <- function(fm,pd,Wname,Mname){
  fit <- tryCatch(
    plm::plm(fm,data=pd,model="within",effect="twoways"),
    error=function(e)e
  )
  if(inherits(fit,"error")){
    record_status(Wname,Mname,"FAILED",conditionMessage(fit),"plm within twoways")
    return(NULL)
  }
  record_status(Wname,Mname,"OK","","plm within twoways")
  fit
}

extract_spml <- function(m,model_name,Wname){
  if(is.null(m)) return(tibble::tibble())

  if(inherits(m,"plm")){
    VV <- tryCatch(plm::vcovSCC(m,type="HC1",maxlag=2),
                   error=function(e) stats::vcov(m))
    ct <- lmtest::coeftest(m,vcov.=VV)
    return(tibble::tibble(
      W=Wname,model=model_name,param_type="slope",
      term=rownames(ct),estimate=as.numeric(ct[,1]),
      std.error=as.numeric(ct[,2]),statistic=as.numeric(ct[,3]),
      p.value=as.numeric(ct[,4])
    ))
  }

  # Slopes
  b <- tryCatch(stats::coef(m),error=function(e)m$coefficients)
  if(is.null(b)) b <- numeric()
  Vb <- tryCatch(stats::vcov(m),error=function(e)m$vcov)

  out <- tibble::tibble()
  if(length(b)){
    if(is.null(Vb) || nrow(as.matrix(Vb)) != length(b)){
      se <- rep(NA_real_,length(b))
    } else {
      se <- sqrt(pmax(diag(as.matrix(Vb)),0))
    }
    z <- b/se
    out <- tibble::tibble(
      W=Wname,model=model_name,param_type="slope",
      term=names(b),estimate=as.numeric(b),std.error=as.numeric(se),
      statistic=as.numeric(z),
      p.value=2*stats::pnorm(abs(z),lower.tail=FALSE)
    )
  }

  # Spatial lag parameter rho: documented separately in arcoef.
  if(model_name %in% c("SAR","SDM")){
    rho <- suppressWarnings(as.numeric(m$arcoef)[1])
    vr <- suppressWarnings(as.numeric(m$vcov.arcoef)[1])
    if(is.finite(rho)){
      ser <- if(is.finite(vr) && vr>=0) sqrt(vr) else NA_real_
      zr <- rho/ser
      out <- dplyr::bind_rows(
        tibble::tibble(
          W=Wname,model=model_name,param_type="spatial_lag_y",
          term="rho_Wy",estimate=rho,std.error=ser,statistic=zr,
          p.value=ifelse(is.finite(zr),2*stats::pnorm(abs(zr),lower.tail=FALSE),NA_real_)
        ),out
      )
    }
  }

  # Error components: preserve raw package names rather than guessing.
  if(model_name %in% c("SEM","SDEM") && !is.null(m$errcomp)){
    ee <- unlist(m$errcomp)
    if(length(ee)){
      Ve <- tryCatch(as.matrix(m$vcov.errcomp),error=function(e) NULL)
      see <- rep(NA_real_,length(ee))
      if(!is.null(Ve) && nrow(Ve)>=length(ee))
        see <- sqrt(pmax(diag(Ve)[seq_along(ee)],0))
      ze <- ee/see
      nm <- names(ee)
      if(is.null(nm)) nm <- paste0("error_component_",seq_along(ee))
      eout <- tibble::tibble(
        W=Wname,model=model_name,param_type="error_component",
        term=paste0("err_",nm),estimate=as.numeric(ee),
        std.error=see,statistic=ze,
        p.value=ifelse(is.finite(ze),2*stats::pnorm(abs(ze),lower.tail=FALSE),NA_real_)
      )
      out <- dplyr::bind_rows(out,eout)
    }
  }
  out
}

get_ll <- function(m){
  if(is.null(m) || inherits(m,"plm")) return(NA_real_)
  x <- tryCatch(as.numeric(stats::logLik(m))[1],error=function(e) NA_real_)
  if(!is.finite(x) && !is.null(m$logLik)) x <- suppressWarnings(as.numeric(m$logLik)[1])
  x
}

residual_moran_summary <- function(m,Wname,Mname,dat,lw){
  if(is.null(m)) return(tibble::tibble(
    W=Wname,model=Mname,mean_Moran=NA_real_,median_Moran=NA_real_,
    significant_years=NA_integer_))
  rr <- tryCatch(as.numeric(stats::residuals(m)),error=function(e)
    tryCatch(as.numeric(m$residuals),error=function(e2) numeric()))
  if(length(rr)!=nrow(dat)) return(tibble::tibble(
    W=Wname,model=Mname,mean_Moran=NA_real_,median_Moran=NA_real_,
    significant_years=NA_integer_))
  rdf <- data.frame(barrio_id=dat$barrio_id,year=dat$year,resid=rr)
  ans <- lapply(MAIN_YEARS,function(tt){
    z <- rdf[rdf$year==tt,,drop=FALSE]
    z <- z[match(ids_main,z$barrio_id),,drop=FALSE]
    o <- tryCatch(spdep::moran.mc(z$resid,lw,nsim=999,zero.policy=TRUE),
                  error=function(e) NULL)
    if(is.null(o)) c(I=NA_real_,p=NA_real_)
    else c(I=as.numeric(o$statistic),p=o$p.value)
  })
  mm <- as.data.frame(do.call(rbind,ans))
  tibble::tibble(
    W=Wname,model=Mname,
    mean_Moran=mean(mm$I,na.rm=TRUE),
    median_Moran=median(mm$I,na.rm=TRUE),
    significant_years=sum(mm$p<.05,na.rm=TRUE)
  )
}

# -----------------------------------------------------------------------------
# 11. ESTIMATE EACH MODEL INDEPENDENTLY FOR EACH W
# -----------------------------------------------------------------------------
BATTERY <- list()
COEFS_ALL <- tibble::tibble(
  W=character(),model=character(),param_type=character(),term=character(),
  estimate=double(),std.error=double(),statistic=double(),p.value=double()
)
MODEL_EVIDENCE <- tibble::tibble(
  W=character(),model=character(),logLik=double(),AIC=double(),BIC=double(),
  K=integer(),N=integer(),T=integer(),NT=integer(),
  mean_Moran=double(),median_Moran=double(),significant_years=integer()
)
WALD_WX <- tibble::tibble(
  W=character(),restriction=character(),statistic=double(),df=integer(),p_value=double()
)

for(wn in names(LW)){
  cat("\n",strrep("=",100),"\nSPATIAL BATTERY — ",wn,"\n",strrep("=",100),"\n",sep="")
  lw <- LW[[wn]]
  Wmat <- spdep::listw2mat(lw)

  datw <- tryCatch(add_WX(panel,Wmat,X_MAIN,ids_main,MAIN_YEARS),
                   error=function(e)e)
  if(inherits(datw,"error")){
    record_status(wn,"ALL","FAILED",conditionMessage(datw),"WX construction")
    cat("WX CONSTRUCTION FAILED: ",conditionMessage(datw),"\n",sep="")
    next
  }

  pd <- plm::pdata.frame(datw,index=c("barrio_id","year"),drop.index=FALSE)
  f_core <- stats::as.formula(paste(Y_MAIN,"~",paste(X_MAIN,collapse="+")))
  f_durb <- stats::as.formula(
    paste(Y_MAIN,"~",paste(c(X_MAIN,paste0("W_",X_MAIN)),collapse="+"))
  )

  mods <- list()
  mods$SAR  <- fit_spml_retry(f_core,pd,lw,TRUE, "none",wn,"SAR")
  mods$SEM  <- fit_spml_retry(f_core,pd,lw,FALSE,"b",   wn,"SEM")
  mods$SLX  <- fit_plm_safe(f_durb,pd,wn,"SLX")
  mods$SDM  <- fit_spml_retry(f_durb,pd,lw,TRUE, "none",wn,"SDM")
  mods$SDEM <- fit_spml_retry(f_durb,pd,lw,FALSE,"b",   wn,"SDEM")

  BATTERY[[wn]] <- list(data=datw,pdata=pd,Wmat=Wmat,models=mods)

  for(mn in names(mods)){
    m <- mods[[mn]]
    if(is.null(m)){
      cat("\n",mn," | ",wn," : FAILED (see MODEL_RUN_STATUS)\n",sep="")
      next
    }

    tab <- extract_spml(m,mn,wn)
    COEFS_ALL <- dplyr::bind_rows(COEFS_ALL,tab)
    cat("\n",mn," | ",wn,"\n",sep="")
    print(tab,n=Inf,width=Inf)

    ll <- get_ll(m)
    k <- if(inherits(m,"plm")) length(stats::coef(m)) else
      length(stats::coef(m)) +
      ifelse(mn %in% c("SAR","SDM") && length(m$arcoef),1,0) +
      ifelse(mn %in% c("SEM","SDEM") && length(m$errcomp),length(unlist(m$errcomp)),0)

    ev <- tibble::tibble(
      W=wn,model=mn,logLik=ll,
      AIC=ifelse(is.finite(ll),-2*ll+2*k,NA_real_),
      BIC=ifelse(is.finite(ll),-2*ll+log(NT)*k,NA_real_),
      K=as.integer(k),N=N,T=TT,NT=NT
    )
    ev <- dplyr::left_join(
      ev,residual_moran_summary(m,wn,mn,datw,lw),by=c("W","model")
    )
    MODEL_EVIDENCE <- dplyr::bind_rows(MODEL_EVIDENCE,ev)
  }

  # SDM -> SAR Wald restriction using beta covariance only (valid for WX block).
  if(!is.null(mods$SDM)){
    b <- stats::coef(mods$SDM)
    V <- tryCatch(stats::vcov(mods$SDM),error=function(e) mods$SDM$vcov)
    ix <- grep("^W_",names(b))
    if(length(ix) && !is.null(V)){
      bb <- b[ix]
      VV <- as.matrix(V)[ix,ix,drop=FALSE]
      iV <- tryCatch(solve(VV),error=function(e) MASS::ginv(VV))
      st <- as.numeric(t(bb)%*%iV%*%bb)
      WALD_WX <- dplyr::bind_rows(
        WALD_WX,
        tibble::tibble(
          W=wn,restriction="SDM -> SAR: all WX = 0",
          statistic=st,df=as.integer(length(bb)),
          p_value=stats::pchisq(st,length(bb),lower.tail=FALSE)
        )
      )
    }
  }
}

write.csv(MODEL_RUN_STATUS,file.path(DIR$models,"MODEL_RUN_STATUS.csv"),row.names=FALSE)
write.csv(COEFS_ALL,file.path(DIR$models,"ALL_SPATIAL_COEFFICIENTS.csv"),row.names=FALSE)
write.csv(MODEL_EVIDENCE,file.path(DIR$models,"MODEL_EVIDENCE_ALL_W.csv"),row.names=FALSE)
write.csv(WALD_WX,file.path(DIR$models,"SDM_TO_SAR_WALD_ALL_W.csv"),row.names=FALSE)

cat("\nMODEL RUN STATUS\n"); print(MODEL_RUN_STATUS,n=Inf,width=Inf)
cat("\nMODEL EVIDENCE — ALL W\n"); print(MODEL_EVIDENCE,n=Inf,width=Inf)
cat("\nSDM -> SAR WALD — ALL W\n"); print(WALD_WX,n=Inf,width=Inf)

if(nrow(COEFS_ALL)==0){
  stop(
    paste0(
      "\nNo spatial model was estimated successfully. ",
      "This V6 has written the exact errors to:\n",
      file.path(DIR$models,"MODEL_RUN_STATUS.csv"),
      "\nThe script stops HERE intentionally so that empty Word tables can never be produced."
    )
  )
}

# -----------------------------------------------------------------------------
# 11B. V10.12 — KNN6: SEPARATE PUBLICATION TABLE FOR EACH VIOLENCE/LEGACY MEASURE
# -----------------------------------------------------------------------------
# Each specification is estimated with the SAME contextual controls and the SAME
# 186-barrios x 13-years balanced sample. Economic-sector shares remain excluded.
#
# IMPORTANT:
# - A separate coefficient table is produced for each violence/legacy definition.
# - Each table has SAR, SEM, SLX, SDM and SDEM as columns, like the original Table 7.
# - N, T, NT, K, residual Moran diagnostics and information criteria are appended.
# - Wald tests of all WX=0 are reported for SDM->SAR and SDEM->SEM.
# - AIC/BIC are NOT used to compare SAR/SDM mechanically with SEM/SDEM because the
#   likelihood conventions returned by the current splm paths are not on a common
#   scale. They are most useful within comparable likelihood families.
# - Model choice is based on a joint reading of theory, WX restriction tests,
#   residual spatial dependence, and comparable information criteria—not stars.

SPATIAL_SPECS <- list(
  S4_SPI_CONTEXT = c("SPI_dynamic",WAGE_CONTEXT),
  S5_HISTORY_CURRENT = SPEC$S5_EXTENDED_CONTEXT,
  S6_OBJECTIVE_LEGACY_GAP = SPEC$S6_STIGMA_LEGACY,
  S6B_HIGHPAST_LOWCURRENT = SPEC$S6B_STIGMA_HIGHPAST_LOWCURRENT,
  S6C_PERSISTENT_HIGH_VIOLENCE = SPEC$S6C_PERSISTENT_HIGH_VIOLENCE,
  S6D_PERSISTENCE_INTENSITY = SPEC$S6D_PERSISTENCE_INTENSITY,
  S6E_VIOLENCE_REGIMES = SPEC$S6E_VIOLENCE_REGIMES,
  S7_RESIDUALIZED_LEGACY = SPEC$S7_RESIDUALIZED_LEGACY
)

SPATIAL_SPEC_TITLES <- c(
  S4_SPI_CONTEXT="SPI dynamic",
  S5_HISTORY_CURRENT="Historical violence + current homicide",
  S6_OBJECTIVE_LEGACY_GAP="Objective violence legacy gap",
  S6B_HIGHPAST_LOWCURRENT="High-past / low-current violence",
  S6C_PERSISTENT_HIGH_VIOLENCE="High-past / high-current persistent violence",
  S6D_PERSISTENCE_INTENSITY="Persistent violence intensity",
  S6E_VIOLENCE_REGIMES="Historical-current violence regimes",
  S7_RESIDUALIZED_LEGACY="Positive residualized violence legacy"
)

SPATIAL_ALL_SPECS <- list()
SPATIAL_ALL_RESULTS <- tibble::tibble()
SPATIAL_SPEC_EVIDENCE <- tibble::tibble()
SPATIAL_SPEC_WALD <- tibble::tibble()

wald_WX_v105 <- function(m,spec,model){
  if(is.null(m)) return(tibble::tibble())
  bb <- tryCatch(stats::coef(m),error=function(e) numeric())
  VV <- tryCatch(as.matrix(stats::vcov(m)),error=function(e)
    tryCatch(as.matrix(m$vcov),error=function(e2) NULL))
  ix <- grep("^W_",names(bb))
  if(!length(ix) || is.null(VV)) return(tibble::tibble())
  VV <- VV[ix,ix,drop=FALSE]
  b <- bb[ix]
  iV <- tryCatch(solve(VV),error=function(e) MASS::ginv(VV))
  stat <- as.numeric(t(b)%*%iV%*%b)
  tibble::tibble(
    spec=spec,model=model,
    restriction=ifelse(model=="SDM","SDM -> SAR: all WX = 0",
                       "SDEM -> SEM: all WX = 0"),
    statistic=stat,df=as.integer(length(b)),
    p_value=stats::pchisq(stat,length(b),lower.tail=FALSE)
  )
}

for(sp in names(SPATIAL_SPECS)){
  cat("\n",strrep("=",100),"\n",
      "KNN6 — ALL MODELS — ",sp,"\n",strrep("=",100),"\n",sep="")

  vars <- SPATIAL_SPECS[[sp]]
  ps <- make_balanced_sample(vars,paste0("SPATIAL_",sp))
  dd <- ps$data |> dplyr::arrange(barrio_id,year)

  if(!identical(as.character(ps$ids),as.character(ids_main))){
    stop("Spatial specification ",sp,
         " does not use the identical main 186-barrio sample.")
  }

  Nsp <- length(ps$ids)
  Tsp <- length(MAIN_YEARS)
  NTsp <- nrow(dd)

  Wmat6 <- spdep::listw2mat(LW$KNN6)
  dat6 <- add_WX(dd,Wmat6,vars,ids_main,MAIN_YEARS)
  pd6 <- plm::pdata.frame(dat6,index=c("barrio_id","year"),drop.index=FALSE)

  f_core6 <- stats::as.formula(paste(Y_MAIN,"~",paste(vars,collapse="+")))
  f_durb6 <- stats::as.formula(
    paste(Y_MAIN,"~",paste(c(vars,paste0("W_",vars)),collapse="+"))
  )

  tag <- paste0("KNN6_",sp)
  mods <- list(
    SAR  = fit_spml_retry(f_core6,pd6,LW$KNN6,TRUE, "none",tag,"SAR"),
    SEM  = fit_spml_retry(f_core6,pd6,LW$KNN6,FALSE,"b",   tag,"SEM"),
    SLX  = fit_plm_safe(f_durb6,pd6,tag,"SLX"),
    SDM  = fit_spml_retry(f_durb6,pd6,LW$KNN6,TRUE, "none",tag,"SDM"),
    SDEM = fit_spml_retry(f_durb6,pd6,LW$KNN6,FALSE,"b",   tag,"SDEM")
  )

  SPATIAL_ALL_SPECS[[sp]] <- list(
    data=dat6,pdata=pd6,models=mods,N=Nsp,T=Tsp,NT=NTsp,vars=vars
  )

  for(mn in names(mods)){
    m <- mods[[mn]]
    if(is.null(m)){
      cat("\n",sp," — ",mn," : NO ESTIMABLE RESULT\n",sep="")
      next
    }

    tt <- extract_spml(m,mn,tag)
    if(nrow(tt)){
      tt$spec <- sp
      SPATIAL_ALL_RESULTS <- dplyr::bind_rows(SPATIAL_ALL_RESULTS,tt)
      cat("\n",sp," — ",mn,"\n",sep="")
      print(tt,n=Inf,width=Inf)
    }

    ll <- get_ll(m)
    kk <- if(inherits(m,"plm")) length(stats::coef(m)) else
      length(stats::coef(m)) +
      ifelse(mn %in% c("SAR","SDM") && length(m$arcoef),1,0) +
      ifelse(mn %in% c("SEM","SDEM") && length(m$errcomp),
             length(unlist(m$errcomp)),0)

    mor <- residual_moran_summary(m,tag,mn,dat6,LW$KNN6)

    SPATIAL_SPEC_EVIDENCE <- dplyr::bind_rows(
      SPATIAL_SPEC_EVIDENCE,
      tibble::tibble(
        spec=sp,model=mn,logLik=ll,
        AIC=ifelse(is.finite(ll),-2*ll+2*kk,NA_real_),
        BIC=ifelse(is.finite(ll),-2*ll+log(NTsp)*kk,NA_real_),
        K=as.integer(kk),N=as.integer(Nsp),T=as.integer(Tsp),NT=as.integer(NTsp),
        mean_Moran=mor$mean_Moran,
        median_Moran=mor$median_Moran,
        significant_years=mor$significant_years
      )
    )
  }

  if(!is.null(mods$SDM))
    SPATIAL_SPEC_WALD <- dplyr::bind_rows(
      SPATIAL_SPEC_WALD,wald_WX_v105(mods$SDM,sp,"SDM"))
  if(!is.null(mods$SDEM))
    SPATIAL_SPEC_WALD <- dplyr::bind_rows(
      SPATIAL_SPEC_WALD,wald_WX_v105(mods$SDEM,sp,"SDEM"))
}

write.csv(SPATIAL_ALL_RESULTS,
          file.path(DIR$models,"KNN6_ALL_MODELS_ALL_LEGACY_SPECS.csv"),
          row.names=FALSE)
write.csv(SPATIAL_SPEC_EVIDENCE,
          file.path(DIR$models,"KNN6_MODEL_EVIDENCE_BY_LEGACY_SPEC.csv"),
          row.names=FALSE)
write.csv(SPATIAL_SPEC_WALD,
          file.path(DIR$models,"KNN6_WX_WALD_BY_LEGACY_SPEC.csv"),
          row.names=FALSE)

cat("\nKNN6 — MODEL EVIDENCE BY VIOLENCE/LEGACY SPECIFICATION\n")
print(SPATIAL_SPEC_EVIDENCE,n=Inf,width=Inf)
cat("\nKNN6 — WX RESTRICTION TESTS BY VIOLENCE/LEGACY SPECIFICATION\n")
print(SPATIAL_SPEC_WALD,n=Inf,width=Inf)

# -----------------------------------------------------------------------------
# 12. IMPACTS — V8 ROBUST EXTRACTION + DELTA METHOD
# -----------------------------------------------------------------------------
# V7 failed here because splm objects may expose the spatial lag parameter in
# different slots/names depending on the estimator path. V8 therefore:
#   1) extracts rho from arcoef when available;
#   2) otherwise searches coefficients for rho/lambda;
#   3) extracts beta/theta by exact variable names;
#   4) computes exact point impacts;
#   5) computes delta-method SEs using a block covariance matrix.
# This avoids Monte-Carlo failure caused by ambiguous covariance indexing.

finite1 <- function(x){
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if(length(x)) x[1] else NA_real_
}

get_spatial_parts <- function(m, model_name, vars){
  rawb <- tryCatch(stats::coef(m),error=function(e) NULL)
  if(is.null(rawb) || !length(rawb)) rawb <- tryCatch(m$coefficients,error=function(e) NULL)
  if(is.null(rawb) || !length(rawb)) stop("No coefficient vector found in spatial model.")
  rawb <- unlist(rawb)
  if(is.null(names(rawb)) || any(names(rawb)=="")){
    stop("Spatial coefficient vector is not fully named; impacts cannot be matched safely to regressors.")
  }

  rho <- finite1(m$arcoef)
  rho_name <- NA_character_
  if(!is.finite(rho)){
    cand <- intersect(names(rawb),c("rho","lambda","spatial","arcoef"))
    if(length(cand)){
      rho_name <- cand[1]
      rho <- finite1(rawb[rho_name])
    } else if(length(rawb) && !(names(rawb)[1] %in% c(vars,paste0("W_",vars)))){
      rho_name <- names(rawb)[1]
      rho <- finite1(rawb[1])
    }
  }
  if(!is.finite(rho)) stop("Could not extract spatial lag parameter rho.")

  slopes <- rawb
  if(!is.na(rho_name)) slopes <- slopes[names(slopes)!=rho_name]

  # Prefer the model vcov method; some splm objects keep only partial blocks in slots.
  Vraw <- tryCatch(as.matrix(stats::vcov(m)),error=function(e) NULL)
  if(is.null(Vraw) || !length(Vraw) || all(!is.finite(Vraw))){
    Vraw <- tryCatch(as.matrix(m$vcov),error=function(e) NULL)
  }

  # Fallback: recover at least marginal slope variances from the published
  # coefficient table. This is used only if a full named covariance matrix
  # is unavailable; covariance terms are then explicitly treated as zero.
  V_source <- "full_vcov"
  if(is.null(Vraw) || !length(Vraw)){
    smct <- tryCatch(as.data.frame(summary(m)$CoefTable),error=function(e) NULL)
    if(!is.null(smct) && nrow(smct)){
      rn <- rownames(smct)
      secol <- grep("Std|standard|Std\\. Error",names(smct),ignore.case=TRUE,value=TRUE)[1]
      if(!is.na(secol) && length(secol)){
        ses <- suppressWarnings(as.numeric(smct[[secol]]))
        names(ses) <- rn
        keep <- intersect(names(slopes),names(ses))
        Vraw <- matrix(0,length(slopes),length(slopes),
                       dimnames=list(names(slopes),names(slopes)))
        Vraw[cbind(match(keep,names(slopes)),match(keep,names(slopes)))] <- ses[keep]^2
        V_source <- "summary_SE_diagonal_fallback"
      }
    }
  }

  vrho <- finite1(m$vcov.arcoef)
  if(!is.finite(vrho) && !is.null(Vraw) && !is.na(rho_name) &&
     !is.null(rownames(Vraw)) && rho_name %in% rownames(Vraw)){
    vrho <- Vraw[rho_name,rho_name]
  }
  if(!is.finite(vrho) || vrho < 0) vrho <- 0

  list(rho=rho,vrho=vrho,b=slopes,V=Vraw,rho_name=rho_name,V_source=V_source)
}

impact_one_v8 <- function(m,model_name,Wmat,vars,spec_name=NULL){
  if(is.null(m) || !(model_name %in% c("SAR","SDM"))) return(tibble::tibble())
  pp <- get_spatial_parts(m,model_name,vars)
  rho <- pp$rho; b <- pp$b; Vraw <- pp$V
  n <- nrow(Wmat); I <- diag(n)

  calc <- function(rho,beta,theta=0){
    A <- solve(I-rho*Wmat)
    S <- A %*% (beta*I + theta*Wmat)
    direct <- mean(diag(S))
    total <- mean(rowSums(S))
    c(Direct=direct,Indirect=total-direct,Total=total)
  }

  out <- lapply(vars,function(v){
    if(!(v %in% names(b))) return(NULL)
    wv <- paste0("W_",v)
    if(model_name=="SDM" && !(wv %in% names(b))) return(NULL)

    beta <- unname(b[v])
    theta <- if(model_name=="SDM") unname(b[wv]) else 0
    point <- calc(rho,beta,theta)

    # Covariance of (rho,beta,theta), block diagonal when rho is stored separately.
    parnames <- if(model_name=="SDM") c(v,wv) else v
    Vbt <- matrix(0,length(parnames),length(parnames),
                  dimnames=list(parnames,parnames))
    cov_ok <- FALSE
    if(!is.null(Vraw)){
      rn <- rownames(Vraw)
      if(!is.null(rn) && all(parnames %in% rn)){
        Vbt <- Vraw[parnames,parnames,drop=FALSE]
        cov_ok <- all(is.finite(diag(Vbt))) && all(diag(Vbt)>0)
      } else if(nrow(Vraw)==length(b)){
        names_b <- names(b)
        rownames(Vraw) <- colnames(Vraw) <- names_b
        Vbt <- Vraw[parnames,parnames,drop=FALSE]
        cov_ok <- all(is.finite(diag(Vbt))) && all(diag(Vbt)>0)
      }
    }
    # Publication-safe fallback.
    # CRITICAL V10.12 FIX: for Tables 7A-7E use the coefficient table from the
    # SAME specification. The old code incorrectly searched COEFS_ALL, which
    # contains the historical benchmark and therefore had no SE for variables
    # such as stigma_legacy_gap or legacy_residual_positive.
    if(!cov_ok){
      ses <- NULL
      if(!is.null(spec_name) && exists("SPATIAL_ALL_RESULTS",inherits=TRUE)){
        ses <- SPATIAL_ALL_RESULTS |>
          dplyr::filter(.data$spec==spec_name,
                        .data$model==model_name,
                        .data$term %in% parnames) |>
          dplyr::select(.data$term,.data$std.error)
        pp$V_source <- paste0("SPATIAL_ALL_RESULTS_same_spec_diagonal_fallback:",spec_name)
      }
      if(is.null(ses) || nrow(ses)!=length(parnames) ||
         any(!is.finite(ses$std.error)) || any(ses$std.error<=0)){
        ses <- COEFS_ALL |>
          dplyr::filter(.data$W=="KNN6",.data$model==model_name,
                        .data$term %in% parnames) |>
          dplyr::select(.data$term,.data$std.error)
        pp$V_source <- "COEFS_ALL_benchmark_diagonal_fallback"
      }
      if(nrow(ses)==length(parnames) && all(is.finite(ses$std.error)) &&
         all(ses$std.error>0)){
        ss <- stats::setNames(ses$std.error,ses$term)[parnames]
        Vbt <- diag(as.numeric(ss)^2,nrow=length(parnames))
        dimnames(Vbt) <- list(parnames,parnames)
        cov_ok <- TRUE
      }
    }
    if(!cov_ok){
      stop("No valid covariance/SE information for impact term(s): ",
           paste(parnames,collapse=", "),
           if(!is.null(spec_name)) paste0(" in ",spec_name) else "")
    }

    Vpar <- matrix(0,1+length(parnames),1+length(parnames))
    Vpar[1,1] <- pp$vrho
    Vpar[-1,-1] <- Vbt
    p0 <- c(rho,beta,if(model_name=="SDM") theta else NULL)

    # Numerical gradient of each impact wrt rho, beta, theta.
    grad_one <- function(effect_index){
      g <- numeric(length(p0))
      for(j in seq_along(p0)){
        h <- max(1e-6,abs(p0[j])*1e-5)
        pplus <- pminus <- p0
        pplus[j] <- pplus[j]+h
        pminus[j] <- pminus[j]-h
        f1 <- calc(pplus[1],pplus[2],
                   if(model_name=="SDM") pplus[3] else 0)[effect_index]
        f0 <- calc(pminus[1],pminus[2],
                   if(model_name=="SDM") pminus[3] else 0)[effect_index]
        g[j] <- (f1-f0)/(2*h)
      }
      g
    }

    dplyr::bind_rows(lapply(seq_along(point),function(k){
      g <- grad_one(k)
      vv <- as.numeric(t(g)%*%Vpar%*%g)
      se <- if(is.finite(vv) && vv>0) sqrt(vv) else NA_real_
      z <- if(is.finite(se) && se>0) point[k]/se else NA_real_
      tibble::tibble(
        term=v,effect=names(point)[k],estimate=unname(point[k]),
        std.error=se,z=z,
        p.value=ifelse(is.finite(z),2*stats::pnorm(abs(z),lower.tail=FALSE),NA_real_),
        lo95=point[k]-1.96*se,hi95=point[k]+1.96*se,
        rho=rho,beta=beta,theta_Wx=theta,
        inference_note=paste0("Delta method; covariance source=",pp$V_source,
          "; rho-beta cross-covariance set to zero when splm stores covariance blocks separately")
      )
    }))
  })
  dplyr::bind_rows(out)
}

IMPACTS_ALL <- tibble::tibble()
IMPACT_STATUS <- tibble::tibble()

run_impacts <- function(Wtag,mn,m,Wmat){
  if(is.null(m)) return(NULL)
  W_CURRENT_FOR_IMPACT <<- Wtag
  ans <- tryCatch(
    impact_one_v8(m,mn,Wmat,X_MAIN),
    error=function(e)e
  )
  if(inherits(ans,"error")){
    IMPACT_STATUS <<- dplyr::bind_rows(
      IMPACT_STATUS,
      tibble::tibble(W=Wtag,model=mn,status="FAILED",
                     message=conditionMessage(ans))
    )
    return(NULL)
  }
  IMPACT_STATUS <<- dplyr::bind_rows(
    IMPACT_STATUS,
    tibble::tibble(W=Wtag,model=mn,status="OK",
                   message=paste0(nrow(ans)," impact rows"))
  )
  if(nrow(ans)) dplyr::mutate(ans,W=Wtag,model=mn,.before=1) else NULL
}

for(wn in names(BATTERY)){
  for(mn in c("SAR","SDM")){
    zz <- run_impacts(wn,mn,BATTERY[[wn]]$models[[mn]],BATTERY[[wn]]$Wmat)
    if(!is.null(zz)) IMPACTS_ALL <- dplyr::bind_rows(IMPACTS_ALL,zz)
  }
}
# Queen/Rook giant-component impacts are robustness-only.  Some runs do not
# construct CONTIG_BATTERY (e.g. when contiguity models are skipped/fail), so
# never let its absence stop the main KNN impact analysis.
if(exists("CONTIG_BATTERY", inherits=TRUE) &&
   is.list(CONTIG_BATTERY) && length(CONTIG_BATTERY)>0){
  for(wn in names(CONTIG_BATTERY)){
    tag <- paste0(wn,"_GC")
    for(mn in c("SAR","SDM")){
      zz <- run_impacts(tag,mn,CONTIG_BATTERY[[wn]]$models[[mn]],
                        CONTIG_BATTERY[[wn]]$Wmat)
      if(!is.null(zz)) IMPACTS_ALL <- dplyr::bind_rows(IMPACTS_ALL,zz)
    }
  }
} else {
  cat("\nIMPACTS NOTE: CONTIG_BATTERY was not created. ",
      "Queen/Rook giant-component impacts are skipped; ",
      "main KNN4/KNN6/KNN8 impacts continue normally.\n",sep="")
}

write.csv(IMPACTS_ALL,file.path(DIR$impacts,"SAR_SDM_IMPACTS_ALL_W.csv"),row.names=FALSE)
write.csv(IMPACT_STATUS,file.path(DIR$impacts,"IMPACT_STATUS.csv"),row.names=FALSE)
cat("\nIMPACT STATUS\n"); print(IMPACT_STATUS,n=Inf,width=Inf)
cat("\nSAR/SDM IMPACTS — ALL W\n"); print(IMPACTS_ALL,n=Inf,width=Inf)

# -----------------------------------------------------------------------------
# 12B. IV DECISION — V10
# -----------------------------------------------------------------------------
# No arrests-based IV is estimated. L1 arrests_rate_10k is excluded from the
# preferred wage specification and from the IV strategy after the previous
# relevance audit failed. Current arrests_rate_10k remains only as a contextual
# conflict/policing rate.
IV_RESULTS <- tibble::tibble()
IV_FIRST_STAGE <- tibble::tibble()
IV_STATUS <- tibble::tibble(
  model="Arrests-based IV",
  status="NOT USED IN V10",
  message="Lagged arrests per 10,000 are excluded. Current arrests per 10,000 enter only as a contextual control; no causal IV interpretation is made.",
  N=NA_integer_,T=NA_integer_
)

V10_RICH_TWFE <- TWFE_ALL |>
  dplyr::filter(spec %in% c("S5_EXTENDED_CONTEXT","S6_STIGMA_LEGACY",
                            "S6B_STIGMA_HIGHPAST_LOWCURRENT",
                            "S6C_PERSISTENT_HIGH_VIOLENCE",
                            "S6D_PERSISTENCE_INTENSITY",
                            "S6E_VIOLENCE_REGIMES",
                            "S7_RESIDUALIZED_LEGACY")) |>
  dplyr::left_join(
    tibble::tibble(spec=names(SAMPLES),
                   N=vapply(SAMPLES,function(x)nrow(x$data),numeric(1)),
                   barrios=vapply(SAMPLES,function(x)dplyr::n_distinct(x$data$barrio_id),numeric(1))),
    by="spec")
write.csv(V10_RICH_TWFE,file.path(DIR$twfe,"V10_EXTENDED_AND_STIGMA_TWFE.csv"),row.names=FALSE)
write.csv(IV_STATUS,file.path(DIR$iv,"IV_STATUS_V10.csv"),row.names=FALSE)
cat("\nV10 EXTENDED + OBJECTIVE LEGACY TWFE\n"); print(V10_RICH_TWFE,n=Inf,width=Inf)
cat("\nIV DECISION\n"); print(IV_STATUS,n=Inf,width=Inf)

# -----------------------------------------------------------------------------
# 13. ROBUSTNESS OF GENERATED OUTCOME
# -----------------------------------------------------------------------------
f_fix <- stats::as.formula(paste(Y_MAIN,"~",paste(X_MAIN,collapse="+"),"| barrio_id + year"))
GEN_OUT <- tibble::tibble()

if(any(is.finite(panel$invvar_weight))){
  mi <- fixest::feols(f_fix,data=panel,weights=~invvar_weight,vcov=~barrio_id)
  GEN_OUT <- dplyr::bind_rows(GEN_OUT,broom::tidy(mi) |>
                                dplyr::mutate(spec="Inverse-variance weighted TWFE"))
}
if(any(is.finite(panel$reliability_weight))){
  mr <- fixest::feols(f_fix,data=panel,weights=~reliability_weight,vcov=~barrio_id)
  GEN_OUT <- dplyr::bind_rows(GEN_OUT,broom::tidy(mr) |>
                                dplyr::mutate(spec="Reliability-weighted TWFE"))
}
write.csv(GEN_OUT,file.path(DIR$robust,"generated_outcome_precision_weighting.csv"),row.names=FALSE)

COMMUNE_YEAR <- fixest::feols(
  stats::as.formula(paste(Y_MAIN,"~",paste(X_MAIN,collapse="+"),
                          "| barrio_id + commune_id^year")),
  data=panel,vcov=~barrio_id
)
COMMUNE_YEAR_TAB <- broom::tidy(COMMUNE_YEAR)
write.csv(COMMUNE_YEAR_TAB,file.path(DIR$robust,"commune_by_year_FE_robustness.csv"),row.names=FALSE)

WITHIN_COMMUNE <- tibble::tibble()
if("theta_within_commune" %in% names(panel) &&
   all(is.finite(panel$theta_within_commune))){
  pd_wc <- plm::pdata.frame(panel,index=c("barrio_id","year"))
  fm_wc <- stats::as.formula(paste("theta_within_commune ~",paste(X_MAIN,collapse="+")))
  mm_wc <- plm::plm(fm_wc,data=pd_wc,model="within",effect="twoways")
  ct_wc <- lmtest::coeftest(mm_wc,vcov.=plm::vcovSCC(mm_wc,type="HC1",maxlag=2))
  WITHIN_COMMUNE <- broom::tidy(ct_wc)
  write.csv(WITHIN_COMMUNE,file.path(DIR$robust,"within_commune_outcome_TWFE.csv"),row.names=FALSE)
}

# -----------------------------------------------------------------------------
# 14. IV READINESS — DO NOT INVENT AN INSTRUMENT
# -----------------------------------------------------------------------------
iv_candidates <- c(
  "financial_density_10k","distance_nearest_metrocable_km",
  "arrests_rate_10k","L1_arrests_rate_10k","pct_violence_victim"
)
IV_AUDIT <- tibble::tibble(
  candidate=iv_candidates,
  available=c(
    "financial_density_10k" %in% names(d),
    "distance_nearest_metrocable_km" %in% names(d),
    "arrests_rate_10k" %in% names(d),
    "arrests_rate_10k" %in% names(d),
    "pct_violence_victim" %in% names(d)
  ),
  status=c("Not accepted as IV","Not accepted as IV",
           "Contextual conflict/policing rate; not accepted as causal IV",
           "Rejected as IV: empirically irrelevant in first stage","Not accepted as IV"),
  reason=c(
    "Plausible direct local economic/formalization channel to wages",
    "Plausible direct labor-market accessibility channel to wages",
    "Captures may measure conflict/policing but can directly affect local labor markets",
    "Lag improves temporal ordering but V8.1 first stage is essentially zero; retained only as contextual control",
    "Contemporaneous victimization is part of the violence mechanism"
  )
)
write.csv(IV_AUDIT,file.path(DIR$iv,"IV_READINESS_AUDIT.csv"),row.names=FALSE)

# -----------------------------------------------------------------------------
# 15. PUBLICATION TABLES — NAMESPACE-SAFE
# -----------------------------------------------------------------------------
stars <- function(p){
  ifelse(is.na(p),"",ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.10,"*",""))))
}
fmt_est_ci <- function(est,se,p,d=3){
  lo <- est-1.96*se; hi <- est+1.96*se
  paste0(formatC(est,format="f",digits=d),stars(p),
         "\n[",formatC(lo,format="f",digits=d),", ",
         formatC(hi,format="f",digits=d),"]")
}
pretty_name <- c(
  SPI_dynamic="Stigma Persistence Index",
  z_historical_violence="Historical violence (z)",
  z_current_homicide="Current homicide (z)",
  pct_informal="Informality share",
  pct_female_head="Female-headed households share",
  mean_age="Mean age",
  mean_stratum="Mean socioeconomic stratum",
  legacy_residual_positive="Positive residualized violence legacy",
  pct_tertiary="Tertiary education share",
  pct_housing_deprivation="Housing deprivation share",
  distance_nearest_metrocable_km="Distance to Metrocable (km)",
  financial_density_10k="Financial establishments per 10,000",
  financial_density_100k="Financial establishments per 100,000",
  log_population="Log population",
  arrests_rate_10k="Arrests per 10,000 inhabitants",
  stigma_legacy_gap="Objective violence legacy gap",
  stigma_highpast_lowcurrent="High-past / low-current violence indicator",
  stigma_highpast_highcurrent="High-past / high-current persistent violence",
  persistent_violence_intensity="Persistent violence intensity (PVI)",
  regime_HPLC_q75="High past / not-high current regime",
  regime_LPHC_q75="Not-high past / high current regime",
  regime_HPHC_q75="High past / high current regime",
  rho_Wy="Spatial lag parameter (rho)"
)


# V10.12 publication-style KNN6 tables: one table per violence/legacy definition.
fmt_num <- function(x,d=3){
  ifelse(is.finite(x),formatC(x,format="f",digits=d),"")
}
fmt_int <- function(x){
  ifelse(is.finite(x),formatC(x,format="d"),"")
}

make_spatial_spec_table <- function(sp){
  vars <- SPATIAL_SPECS[[sp]]
  keep_terms <- c(vars,paste0("W_",vars),"rho_Wy","lambda","rho")

  coef_tab <- SPATIAL_ALL_RESULTS |>
    dplyr::filter(spec==sp,model %in% c("SAR","SEM","SLX","SDM","SDEM"),
                  term %in% keep_terms) |>
    dplyr::mutate(
      variable=dplyr::case_when(
        term=="rho_Wy" ~ "Spatial lag parameter (rho)",
        term=="lambda" & model %in% c("SAR","SDM") ~ "Spatial lag parameter (rho)",
        term=="rho" & model %in% c("SEM","SDEM") ~ "Spatial error parameter",
        startsWith(term,"W_") ~ paste0(
          "W × ",
          dplyr::recode(sub("^W_","",term),!!!as.list(pretty_name),
                        .default=sub("^W_","",term))
        ),
        TRUE ~ dplyr::recode(term,!!!as.list(pretty_name),.default=term)
      ),
      cell=fmt_est_ci(estimate,std.error,p.value)
    ) |>
    dplyr::select(variable,model,cell) |>
    dplyr::distinct(variable,model,.keep_all=TRUE) |>
    tidyr::pivot_wider(names_from=model,values_from=cell)

  # Preserve substantive order: local X, WX, then spatial parameter.
  ord <- c(
    dplyr::recode(vars,!!!as.list(pretty_name),.default=vars),
    paste0("W × ",dplyr::recode(vars,!!!as.list(pretty_name),.default=vars)),
    "Spatial lag parameter (rho)","Spatial error parameter"
  )
  coef_tab <- coef_tab |>
    dplyr::mutate(.ord=match(variable,ord)) |>
    dplyr::arrange(.ord) |>
    dplyr::select(-.ord)

  ev <- SPATIAL_SPEC_EVIDENCE |>
    dplyr::filter(spec==sp) |>
    dplyr::select(model,logLik,AIC,BIC,K,N,T,NT,mean_Moran,median_Moran,
                  significant_years)

  get_ev_row <- function(label,col,digits=3){
    z <- ev |>
      dplyr::transmute(model,cell=if(col %in% c("K","N","T","NT"))
        fmt_int(.data[[col]]) else fmt_num(.data[[col]],digits))
    z |>
      tidyr::pivot_wider(names_from=model,values_from=cell) |>
      dplyr::mutate(variable=label,.before=1)
  }

  stat_rows <- dplyr::bind_rows(
    get_ev_row("N (barrios)","N",0),
    get_ev_row("T (years)","T",0),
    get_ev_row("NT observations","NT",0),
    get_ev_row("Estimated parameters (K)","K",0),
    get_ev_row("Log-likelihood","logLik",3),
    get_ev_row("AIC","AIC",3),
    get_ev_row("BIC","BIC",3),
    get_ev_row("Mean residual Moran's I","mean_Moran",4),
    get_ev_row("Median residual Moran's I","median_Moran",4),
    get_ev_row("Years residual Moran p<0.05","significant_years",0)
  )

  # Wald rows are only meaningful in the unrestricted Durbin columns.
  ww <- SPATIAL_SPEC_WALD |> dplyr::filter(spec==sp)
  wstat <- tibble::tibble(
    variable=c("Wald: all WX = 0","Wald p-value: all WX = 0"),
    SAR=c("",""),SEM=c("",""),SLX=c("",""),SDM=c("",""),SDEM=c("","")
  )
  if(any(ww$model=="SDM")){
    q <- ww[ww$model=="SDM",]
    wstat$SDM <- c(fmt_num(q$statistic[1],3),fmt_num(q$p_value[1],4))
  }
  if(any(ww$model=="SDEM")){
    q <- ww[ww$model=="SDEM",]
    wstat$SDEM <- c(fmt_num(q$statistic[1],3),fmt_num(q$p_value[1],4))
  }

  out <- dplyr::bind_rows(coef_tab,stat_rows,wstat)
  # Guarantee the same five model columns even if a model failed.
  for(mm in c("SAR","SEM","SLX","SDM","SDEM"))
    if(!mm %in% names(out)) out[[mm]] <- ""
  out |> dplyr::select(variable,SAR,SEM,SLX,SDM,SDEM)
}

TABLE7_SPI       <- make_spatial_spec_table("S4_SPI_CONTEXT")
TABLE7_HISTORY   <- make_spatial_spec_table("S5_HISTORY_CURRENT")
TABLE7_GAP       <- make_spatial_spec_table("S6_OBJECTIVE_LEGACY_GAP")
TABLE7_HILO      <- make_spatial_spec_table("S6B_HIGHPAST_LOWCURRENT")
TABLE7_HIHI      <- make_spatial_spec_table("S6C_PERSISTENT_HIGH_VIOLENCE")
TABLE7_PVI       <- make_spatial_spec_table("S6D_PERSISTENCE_INTENSITY")
TABLE7_REGIMES   <- make_spatial_spec_table("S6E_VIOLENCE_REGIMES")
TABLE7_RESIDUAL  <- make_spatial_spec_table("S7_RESIDUALIZED_LEGACY")

write.csv(TABLE7_SPI,file.path(DIR$paper,"TABLE7A_SPI_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_HISTORY,file.path(DIR$paper,"TABLE7B_HISTORY_CURRENT_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_GAP,file.path(DIR$paper,"TABLE7C_OBJECTIVE_GAP_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_HILO,file.path(DIR$paper,"TABLE7D_HIGHPAST_LOWCURRENT_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_HIHI,file.path(DIR$paper,"TABLE7E_PERSISTENT_HIGH_VIOLENCE_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_PVI,file.path(DIR$paper,"TABLE7F_PERSISTENCE_INTENSITY_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_REGIMES,file.path(DIR$paper,"TABLE7G_VIOLENCE_REGIMES_KNN6.csv"),row.names=FALSE)
write.csv(TABLE7_RESIDUAL,file.path(DIR$paper,"TABLE7E_RESIDUALIZED_LEGACY_KNN6.csv"),row.names=FALSE)

cat("\nTABLE 7A — SPI DYNAMIC — KNN6\n"); print(TABLE7_SPI,n=Inf,width=Inf)
cat("\nTABLE 7B — HISTORICAL + CURRENT — KNN6\n"); print(TABLE7_HISTORY,n=Inf,width=Inf)
cat("\nTABLE 7C — OBJECTIVE LEGACY GAP — KNN6\n"); print(TABLE7_GAP,n=Inf,width=Inf)
cat("\nTABLE 7D — HIGH-PAST / LOW-CURRENT — KNN6\n"); print(TABLE7_HILO,n=Inf,width=Inf)
cat("\nTABLE 7E — HIGH-PAST / HIGH-CURRENT PERSISTENT VIOLENCE — KNN6\n"); print(TABLE7_HIHI,n=Inf,width=Inf)
cat("\nTABLE 7F — PERSISTENCE INTENSITY — KNN6\n"); print(TABLE7_PVI,n=Inf,width=Inf)
cat("\nTABLE 7G — VIOLENCE REGIMES — KNN6\n"); print(TABLE7_REGIMES,n=Inf,width=Inf)
cat("\nTABLE 7H — RESIDUALIZED LEGACY — KNN6\n"); print(TABLE7_RESIDUAL,n=Inf,width=Inf)

# -----------------------------------------------------------------------------
# 15B. V10.12 — SDM-KNN6 IMPACTS FOR EACH VIOLENCE / LEGACY SPECIFICATION
# -----------------------------------------------------------------------------
# IMPORTANT: these are impacts from the SDM corresponding to each Table 7A-7E,
# not impacts recycled from the historical benchmark BATTERY.
# The same exact spatial multiplier and delta-method routine defined in Section 12
# is used, but with each specification's own SDM, regressors and KNN6 matrix.

IMPACTS_KNN6_SPECS <- tibble::tibble()
IMPACTS_KNN6_SPECS_STATUS <- tibble::tibble()

for(sp in names(SPATIAL_ALL_SPECS)){
  obj <- SPATIAL_ALL_SPECS[[sp]]
  m <- obj$models$SDM
  vars <- obj$vars

  if(is.null(m)){
    IMPACTS_KNN6_SPECS_STATUS <- dplyr::bind_rows(
      IMPACTS_KNN6_SPECS_STATUS,
      tibble::tibble(spec=sp,model="SDM",status="FAILED",
                     message="SDM model is NULL")
    )
    next
  }

  ans <- tryCatch(
    impact_one_v8(m,"SDM",Wmat6,vars,spec_name=sp),
    error=function(e)e
  )

  if(inherits(ans,"error")){
    IMPACTS_KNN6_SPECS_STATUS <- dplyr::bind_rows(
      IMPACTS_KNN6_SPECS_STATUS,
      tibble::tibble(spec=sp,model="SDM",status="FAILED",
                     message=conditionMessage(ans))
    )
  } else {
    IMPACTS_KNN6_SPECS_STATUS <- dplyr::bind_rows(
      IMPACTS_KNN6_SPECS_STATUS,
      tibble::tibble(spec=sp,model="SDM",status="OK",
                     message=paste0(nrow(ans)," impact rows"))
    )
    if(nrow(ans)){
      ans$spec <- rep(as.character(sp),nrow(ans))
      ans$W <- rep("KNN6",nrow(ans))
      ans$model <- rep("SDM",nrow(ans))
      IMPACTS_KNN6_SPECS <- dplyr::bind_rows(IMPACTS_KNN6_SPECS,ans)
    } else {
      IMPACTS_KNN6_SPECS_STATUS <- dplyr::bind_rows(
        IMPACTS_KNN6_SPECS_STATUS,
        tibble::tibble(spec=as.character(sp),model="SDM",status="EMPTY",
          message=paste0("No impact rows. coef names: ",
                         paste(names(tryCatch(stats::coef(m),error=function(e) numeric())),collapse=", ")))
      )
    }
  }
}

write.csv(IMPACTS_KNN6_SPECS,
          file.path(DIR$models,"KNN6_SDM_IMPACTS_ALL_LEGACY_SPECS.csv"),
          row.names=FALSE)
write.csv(IMPACTS_KNN6_SPECS_STATUS,
          file.path(DIR$models,"KNN6_SDM_IMPACTS_ALL_LEGACY_STATUS.csv"),
          row.names=FALSE)

cat("\n",strrep("=",100),"\n",sep="")
cat("V10.12 — KNN6 SDM IMPACTS BY VIOLENCE / LEGACY SPECIFICATION\n")
cat(strrep("=",100),"\n",sep="")
print(IMPACTS_KNN6_SPECS_STATUS,n=Inf,width=Inf)
print(IMPACTS_KNN6_SPECS,n=Inf,width=Inf)

make_impact_spec_table <- function(sp){
  if(!is.data.frame(IMPACTS_KNN6_SPECS) ||
     !"spec" %in% names(IMPACTS_KNN6_SPECS) ||
     nrow(IMPACTS_KNN6_SPECS)==0){
    print(IMPACTS_KNN6_SPECS_STATUS,n=Inf,width=Inf)
    stop("KNN6-SDM impacts are empty.")
  }

  raw <- IMPACTS_KNN6_SPECS |>
    dplyr::filter(.data$spec == as.character(sp))

  if(nrow(raw)==0) stop("No impact rows found for specification: ",sp)
  if(any(!is.finite(raw$std.error)) || any(!is.finite(raw$p.value)) ||
     any(!is.finite(raw$lo95)) || any(!is.finite(raw$hi95))){
    bad <- raw |>
      dplyr::filter(!is.finite(.data$std.error) | !is.finite(.data$p.value) |
                    !is.finite(.data$lo95) | !is.finite(.data$hi95))
    print(bad,n=Inf,width=Inf)
    stop("Publication table blocked: missing impact inference in ",sp)
  }

  z <- raw |>
    dplyr::mutate(
      variable=dplyr::recode(.data$term,!!!as.list(pretty_name),.default=.data$term),
      cell=paste0(
        formatC(.data$estimate,format="f",digits=3),stars(.data$p.value),
        "\n(SE ",formatC(.data$std.error,format="f",digits=3),")",
        "\n[",formatC(.data$lo95,format="f",digits=3),", ",
        formatC(.data$hi95,format="f",digits=3),"]"
      )
    ) |>
    dplyr::select(.data$variable,.data$effect,.data$cell) |>
    tidyr::pivot_wider(names_from="effect",values_from="cell")

  for(cc in c("Direct","Indirect","Total"))
    if(!cc %in% names(z)) z[[cc]] <- ""
  z |> dplyr::select(.data$variable,dplyr::all_of(c("Direct","Indirect","Total")))
}

TABLE7A_IMPACTS <- make_impact_spec_table("S4_SPI_CONTEXT")
TABLE7B_IMPACTS <- make_impact_spec_table("S5_HISTORY_CURRENT")
TABLE7C_IMPACTS <- make_impact_spec_table("S6_OBJECTIVE_LEGACY_GAP")
TABLE7D_IMPACTS <- make_impact_spec_table("S6B_HIGHPAST_LOWCURRENT")
TABLE7E_IMPACTS <- make_impact_spec_table("S6C_PERSISTENT_HIGH_VIOLENCE")
TABLE7F_IMPACTS <- make_impact_spec_table("S6D_PERSISTENCE_INTENSITY")
TABLE7G_IMPACTS <- make_impact_spec_table("S6E_VIOLENCE_REGIMES")
TABLE7H_IMPACTS <- make_impact_spec_table("S7_RESIDUALIZED_LEGACY")

write.csv(TABLE7A_IMPACTS,file.path(DIR$paper,"TABLE7A1_SPI_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7B_IMPACTS,file.path(DIR$paper,"TABLE7B1_HISTORY_CURRENT_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7C_IMPACTS,file.path(DIR$paper,"TABLE7C1_OBJECTIVE_GAP_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7D_IMPACTS,file.path(DIR$paper,"TABLE7D1_HIGHPAST_LOWCURRENT_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7E_IMPACTS,file.path(DIR$paper,"TABLE7E1_PERSISTENT_HIGH_VIOLENCE_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7F_IMPACTS,file.path(DIR$paper,"TABLE7F1_PERSISTENCE_INTENSITY_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7G_IMPACTS,file.path(DIR$paper,"TABLE7G1_VIOLENCE_REGIMES_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(TABLE7H_IMPACTS,file.path(DIR$paper,"TABLE7H1_RESIDUALIZED_LEGACY_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)

cat("\nTABLE 7A.1 — SPI — SDM KNN6 IMPACTS\n"); print(TABLE7A_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7B.1 — HISTORICAL + CURRENT — SDM KNN6 IMPACTS\n"); print(TABLE7B_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7C.1 — OBJECTIVE LEGACY GAP — SDM KNN6 IMPACTS\n"); print(TABLE7C_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7C.1 RAW INFERENCE — estimate, SE, z, p, CI and covariance source\n")
print(IMPACTS_KNN6_SPECS |>
        dplyr::filter(.data$spec=="S6_OBJECTIVE_LEGACY_GAP"),
      n=Inf,width=Inf)
cat("\nTABLE 7D.1 — HIGH-PAST / LOW-CURRENT — SDM KNN6 IMPACTS\n"); print(TABLE7D_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7E.1 — PERSISTENT HIGH VIOLENCE — SDM KNN6 IMPACTS\n"); print(TABLE7E_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7F.1 — PERSISTENCE INTENSITY — SDM KNN6 IMPACTS\n"); print(TABLE7F_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7G.1 — VIOLENCE REGIMES — SDM KNN6 IMPACTS\n"); print(TABLE7G_IMPACTS,n=Inf,width=Inf)
cat("\nTABLE 7H.1 — RESIDUALIZED LEGACY — SDM KNN6 IMPACTS\n"); print(TABLE7H_IMPACTS,n=Inf,width=Inf)

twfe_paper <- TWFE_ALL |>
  dplyr::filter(term %in% unique(unlist(SPEC))) |>
  dplyr::mutate(
    variable=dplyr::recode(term,!!!as.list(pretty_name),.default=term),
    cell=fmt_est_ci(estimate,std.error,p.value)
  ) |>
  dplyr::select(variable,spec,cell) |>
  tidyr::pivot_wider(names_from=spec,values_from=cell)

spatial_knn6 <- COEFS_ALL |>
  dplyr::filter(W=="KNN6",model %in% c("SAR","SEM","SLX","SDM","SDEM"),
                term %in% c(X_MAIN,paste0("W_",X_MAIN),"rho_Wy")) |>
  dplyr::mutate(
    variable=dplyr::case_when(
      startsWith(term,"W_") ~ paste0(
        "W × ",
        dplyr::recode(sub("^W_","",term),!!!as.list(pretty_name),
                      .default=sub("^W_","",term))
      ),
      TRUE ~ dplyr::recode(term,!!!as.list(pretty_name),.default=term)
    ),
    cell=fmt_est_ci(estimate,std.error,p.value)
  ) |>
  dplyr::select(variable,model,cell) |>
  tidyr::pivot_wider(names_from=model,values_from=cell)

spatial_knn_sensitivity <- COEFS_ALL |>
  dplyr::filter(W %in% c("KNN4","KNN6","KNN8"),
                model %in% c("SAR","SDM"),
                term %in% c("z_historical_violence","z_current_homicide","rho_Wy")) |>
  dplyr::mutate(
    variable=dplyr::recode(term,!!!as.list(pretty_name),.default=term),
    specification=paste(W,model,sep=" — "),
    cell=fmt_est_ci(estimate,std.error,p.value)
  ) |>
  dplyr::select(variable,specification,cell) |>
  tidyr::pivot_wider(names_from=specification,values_from=cell)

impact_main <- tibble::tibble()
impact_sensitivity <- tibble::tibble()
if(nrow(IMPACTS_ALL)){
  impact_main <- IMPACTS_ALL |>
    dplyr::filter(W=="KNN6",model=="SDM",
                  term %in% c("z_historical_violence","z_current_homicide")) |>
    dplyr::mutate(
      variable=dplyr::recode(term,!!!as.list(pretty_name),.default=term),
      cell=paste0(formatC(estimate,format="f",digits=3),stars(p.value),
                  "\n[",formatC(lo95,format="f",digits=3),", ",
                  formatC(hi95,format="f",digits=3),"]")
    ) |>
    dplyr::select(variable,effect,cell) |>
    tidyr::pivot_wider(names_from=effect,values_from=cell)

  impact_sensitivity <- IMPACTS_ALL |>
    dplyr::filter(W %in% c("KNN4","KNN6","KNN8"),
                  model=="SDM",
                  term %in% c("z_historical_violence","z_current_homicide"),
                  effect=="Total") |>
    dplyr::mutate(
      variable=dplyr::recode(term,!!!as.list(pretty_name),.default=term),
      cell=paste0(formatC(estimate,format="f",digits=3),stars(p.value),
                  "\n[",formatC(lo95,format="f",digits=3),", ",
                  formatC(hi95,format="f",digits=3),"]")
    ) |>
    dplyr::select(variable,W,cell) |>
    tidyr::pivot_wider(names_from=W,values_from=cell)
}


rich_paper <- tibble::tibble()
if(nrow(V10_RICH_TWFE)){
  rich_paper <- V10_RICH_TWFE |>
    dplyr::mutate(variable=dplyr::recode(term,!!!as.list(pretty_name),.default=term),
                  cell=fmt_est_ci(estimate,std.error,p.value)) |>
    dplyr::select(variable,spec,cell) |>
    tidyr::pivot_wider(names_from=spec,values_from=cell)
}


# -----------------------------------------------------------------------------
# 15C. PUBLICATION FIGURES — PERSISTENCE HYPOTHESIS
# -----------------------------------------------------------------------------
# Figure 11 compares the raw focal coefficient across the five KNN6 spatial
# specifications. Figure 12 reports SDM direct/indirect/total impacts. These
# figures use exactly the same model objects as Tables 7D-7H.
FOCAL_TERMS <- c(
  S6B_HIGHPAST_LOWCURRENT="stigma_highpast_lowcurrent",
  S6C_PERSISTENT_HIGH_VIOLENCE="stigma_highpast_highcurrent",
  S6D_PERSISTENCE_INTENSITY="persistent_violence_intensity"
)
coef_plot_dat <- dplyr::bind_rows(lapply(names(FOCAL_TERMS),function(sp){
  term0 <- unname(FOCAL_TERMS[[sp]])
  SPATIAL_ALL_RESULTS %>% dplyr::filter(.data$spec==sp,.data$term==term0,
    .data$model %in% c("SAR","SEM","SLX","SDM","SDEM")) %>%
    dplyr::mutate(conf.low=.data$estimate-1.96*.data$std.error,
                  conf.high=.data$estimate+1.96*.data$std.error,
                  measure=dplyr::recode(sp,
      S6B_HIGHPAST_LOWCURRENT="High past / low current",
      S6C_PERSISTENT_HIGH_VIOLENCE="High past / high current",
      S6D_PERSISTENCE_INTENSITY="Persistence intensity"))
}))
if(nrow(coef_plot_dat)){
  FIG11 <- ggplot2::ggplot(coef_plot_dat,ggplot2::aes(x=model,y=estimate,ymin=conf.low,ymax=conf.high,shape=measure,group=measure))+
    ggplot2::geom_hline(yintercept=0,linetype=2,linewidth=.4)+
    ggplot2::geom_pointrange(position=ggplot2::position_dodge(width=.55))+
    ggplot2::labs(title="Figure 11. Violence transition and persistence across KNN6 spatial models",
      subtitle="Focal coefficients with 95% confidence intervals",x=NULL,y="Coefficient",shape=NULL)+
    ggplot2::theme_minimal(base_size=10)+ggplot2::theme(legend.position="bottom")
  ggplot2::ggsave(file.path(DIR$paper,"FIGURE_11_PERSISTENCE_COEFFICIENTS_KNN6.png"),FIG11,width=9.5,height=5.8,dpi=600,bg="white")
}
impact_plot_dat <- IMPACTS_KNN6_SPECS %>%
  dplyr::filter(.data$spec %in% names(FOCAL_TERMS)) %>%
  dplyr::filter(.data$term == dplyr::recode(.data$spec,!!!as.list(FOCAL_TERMS))) %>%
  dplyr::mutate(measure=dplyr::recode(.data$spec,
    S6B_HIGHPAST_LOWCURRENT="High past / low current",
    S6C_PERSISTENT_HIGH_VIOLENCE="High past / high current",
    S6D_PERSISTENCE_INTENSITY="Persistence intensity"),
    effect=factor(.data$effect,levels=c("Direct","Indirect","Total")))
if(nrow(impact_plot_dat)){
  FIG12 <- ggplot2::ggplot(impact_plot_dat,ggplot2::aes(x=effect,y=estimate,ymin=lo95,ymax=hi95,shape=measure,group=measure))+
    ggplot2::geom_hline(yintercept=0,linetype=2,linewidth=.4)+
    ggplot2::geom_pointrange(position=ggplot2::position_dodge(width=.55))+
    ggplot2::labs(title="Figure 12. SDM-KNN6 impacts of violence transition and persistence",
      subtitle="Direct, indirect and total impacts with 95% confidence intervals",x=NULL,y="Impact",shape=NULL)+
    ggplot2::theme_minimal(base_size=10)+ggplot2::theme(legend.position="bottom")
  ggplot2::ggsave(file.path(DIR$paper,"FIGURE_12_PERSISTENCE_SDM_IMPACTS_KNN6.png"),FIG12,width=9.5,height=5.8,dpi=600,bg="white")
}

# -----------------------------------------------------------------------------
# V10.12 SAFE OPTIONAL ROBUSTNESS OBJECTS
# -----------------------------------------------------------------------------
# Queen/Rook giant-component outputs are optional robustness results. If the
# contiguity block was not estimable/constructed in the current run, create
# publication-safe empty objects so the Word export and final RDS save do not fail.
if(!exists("CONTIG_COMPONENT_DIAG",inherits=TRUE)){
  CONTIG_COMPONENT_DIAG <- tibble::tibble()
  cat("\nWORD NOTE: CONTIG_COMPONENT_DIAG not available; Table 8B will report no estimable results.\n")
}
if(!exists("CONTIG_EVIDENCE",inherits=TRUE)){
  CONTIG_EVIDENCE <- tibble::tibble()
  cat("WORD NOTE: CONTIG_EVIDENCE not available; Table 8C will report no estimable results.\n")
}
if(!exists("CONTIG_BATTERY",inherits=TRUE)){
  CONTIG_BATTERY <- list()
}
if(!exists("CONTIG_COEFS",inherits=TRUE)){
  CONTIG_COEFS <- tibble::tibble()
  cat("EXPORT NOTE: CONTIG_COEFS not available; Excel sheet will be empty.\n")
}
if(!exists("CONTIG_WALD",inherits=TRUE)){
  CONTIG_WALD <- tibble::tibble()
  cat("EXPORT NOTE: CONTIG_WALD not available; Excel sheet will be empty.\n")
}
# -----------------------------------------------------------------------------
# 16. FINAL WORD — CORE RESULTS + COMPLETE KNN6 STIGMA BATTERY
# -----------------------------------------------------------------------------
# FINAL CLOSED VERSION:
#   * SPI is NOT reported in the Word document.
#   * The paper core contains the three preferred SDM-KNN6 specifications.
#   * The same Word also reports the complete KNN6 spatial-model battery
#     (SAR, SEM, SLX, SDM, SDEM) for every retained violence/stigma measure,
#     followed immediately by its corresponding SDM-KNN6 impacts table.
#   * Historical + current violence is retained only as the benchmark.
#   * Stigma/trajectory measures retained: Objective Gap, HighPast/LowCurrent,
#     HighPast/HighCurrent, PVI, Violence Regimes, Residualized Legacy.
#   * Arrests are retained. Industry, commerce and pct_violence_victim are out.
#   * Financial density is reported per 100,000 inhabitants.

WORD_USABLE_WIDTH <- 9.6

make_ft <- function(df){
  ft <- flextable::flextable(as.data.frame(df)) |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size=8,part="all") |>
    flextable::font(fontname="Times New Roman",part="all") |>
    flextable::align(align="center",part="all") |>
    flextable::align(j=1,align="left",part="all") |>
    flextable::valign(valign="center",part="all") |>
    flextable::autofit()
  ft <- flextable::fit_to_width(ft,max_width=WORD_USABLE_WIDTH)
  flextable::set_table_properties(ft,layout="fixed",width=1)
}
add_note <- function(doc,text)
  officer::body_add_par(doc,paste0("Notes: ",text),style="Normal")
add_table <- function(doc,title,df,note=NULL){
  doc <- officer::body_add_par(doc,title,style="heading 2")
  if(nrow(as.data.frame(df))){
    doc <- flextable::body_add_flextable(doc,make_ft(df))
  } else {
    doc <- officer::body_add_par(doc,"No estimable results for this table.",style="Normal")
  }
  if(!is.null(note)) doc <- add_note(doc,note)
  doc
}

# ---- Core SDM table: only the three specifications selected for the paper ----
core_sdm_one <- function(tab, label){
  z <- as.data.frame(tab,stringsAsFactors=FALSE)
  if(!all(c("variable","SDM") %in% names(z)))
    stop("Core SDM table cannot be built for ",label,". Missing variable/SDM columns.")
  out <- z[,c("variable","SDM"),drop=FALSE]
  names(out)[2] <- label
  out
}

CORE_SDM <- Reduce(
  function(x,y) dplyr::full_join(x,y,by="variable"),
  list(
    core_sdm_one(TABLE7_HILO,"High past / low current"),
    core_sdm_one(TABLE7_HIHI,"High past / high current"),
    core_sdm_one(TABLE7_PVI,"Persistent violence intensity")
  )
)
CORE_SDM[is.na(CORE_SDM)] <- ""

core_order <- c(
  "High-past / low-current violence indicator",
  "High-past / high-current persistent violence",
  "Persistent violence intensity (PVI)",
  "Current homicide (z)",
  "Informality share","Tertiary education share","Housing deprivation share",
  "Financial establishments per 100,000","Distance to Metrocable (km)",
  "Arrests per 10,000 inhabitants","Female-headed households share",
  "Mean age","Mean socioeconomic stratum",
  "W × High-past / low-current violence indicator",
  "W × High-past / high-current persistent violence",
  "W × Persistent violence intensity (PVI)",
  "W × Current homicide (z)",
  "W × Informality share","W × Tertiary education share","W × Housing deprivation share",
  "W × Financial establishments per 100,000","W × Distance to Metrocable (km)",
  "W × Arrests per 10,000 inhabitants","W × Female-headed households share",
  "W × Mean age","W × Mean socioeconomic stratum",
  "Spatial lag parameter (rho)","Spatial error parameter",
  "N (barrios)","T (years)","NT observations",
  "Estimated parameters (K)","Log-likelihood","AIC","BIC",
  "Mean residual Moran's I","Median residual Moran's I","Years residual Moran p<0.05",
  "Wald: all WX = 0","Wald p-value: all WX = 0"
)
CORE_SDM <- CORE_SDM |>
  dplyr::mutate(.ord=match(.data$variable,core_order)) |>
  dplyr::arrange(is.na(.data$.ord),.data$.ord) |>
  dplyr::select(-.data$.ord)

impact_tag <- function(tab,label){
  z <- as.data.frame(tab,stringsAsFactors=FALSE)
  hit <- names(z) %in% c("Direct","Indirect","Total")
  names(z)[hit] <- paste0(label," — ",names(z)[hit])
  z
}
CORE_IMPACTS <- Reduce(
  function(x,y) dplyr::full_join(x,y,by="variable"),
  list(
    impact_tag(TABLE7D_IMPACTS,"High past / low current"),
    impact_tag(TABLE7E_IMPACTS,"High past / high current"),
    impact_tag(TABLE7F_IMPACTS,"Persistent violence intensity")
  )
)
CORE_IMPACTS[is.na(CORE_IMPACTS)] <- ""
CORE_IMPACTS <- CORE_IMPACTS |>
  dplyr::mutate(.ord=match(.data$variable,core_order)) |>
  dplyr::arrange(is.na(.data$.ord),.data$.ord) |>
  dplyr::select(-.data$.ord)

focal_vars <- c(
  "High-past / low-current violence indicator",
  "High-past / high-current persistent violence",
  "Persistent violence intensity (PVI)"
)
CORE_FOCAL_IMPACTS <- CORE_IMPACTS |>
  dplyr::filter(.data$variable %in% focal_vars)

write.csv(CORE_SDM,file.path(DIR$paper,"CORE_TABLE_1_SDM_KNN6.csv"),row.names=FALSE)
write.csv(CORE_IMPACTS,file.path(DIR$paper,"CORE_TABLE_2_SDM_KNN6_IMPACTS.csv"),row.names=FALSE)
write.csv(CORE_FOCAL_IMPACTS,file.path(DIR$paper,"CORE_TABLE_2A_FOCAL_VIOLENCE_IMPACTS.csv"),row.names=FALSE)

# ---- Final Word ----
doc <- officer::read_docx()
landscape_section <- officer::prop_section(
  page_size=officer::page_size(orient="landscape"),
  page_margins=officer::page_mar(top=0.55,bottom=0.55,left=0.55,right=0.55,
                                 header=0.3,footer=0.3)
)
doc <- officer::body_set_default_section(doc,landscape_section)
doc <- officer::body_add_par(doc,"Spatial Econometric Results — Medellín, 2006–2018",
                             style="heading 1")
doc <- officer::body_add_par(
  doc,paste0("Balanced estimation sample: ",N," barrios × ",TT,
             " years = ",NT," barrio-year observations.")
)

# Part I — heart of the paper
doc <- officer::body_add_par(doc,"Part I. Core paper results",style="heading 1")
doc <- add_table(
  doc,"Table 1. Main SDM-KNN6 estimates — violence trajectories and persistence",
  CORE_SDM,
  paste0(
    "The three columns use the same wage-setting controls and KNN6 spatial weights. ",
    "Arrests are retained; industry, commerce and violence-victimization shares are excluded. ",
    "Financial establishments are expressed per 100,000 inhabitants. ",
    "Current homicide enters separately only in the high-past/low-current specification because current violence is part of the HPHC and PVI definitions. ",
    "SDM slope coefficients are not marginal effects; substantive interpretation should rely on Table 2 impacts. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )
)
doc <- add_table(
  doc,"Table 2. SDM-KNN6 impacts — direct, indirect and total effects",
  CORE_IMPACTS,
  paste0(
    "Direct, indirect and total effects use the exact SDM spatial multiplier and incorporate endogenous spatial feedback through rho and WX. ",
    "Confidence intervals use the delta-method routine implemented in the script."
  )
)
doc <- add_table(
  doc,"Table 2A. Focal violence impacts — compact main-text version",
  CORE_FOCAL_IMPACTS,
  "Compact presentation of the three focal violence measures only."
)

# Part II — complete KNN6 battery. SPI deliberately omitted.
doc <- officer::body_add_par(doc,"Part II. Complete KNN6 spatial-model results by violence/stigma measure",style="heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "Each coefficient table reports SAR, SEM, SLX, SDM and SDEM using KNN6. ",
    "Each is followed by the direct, indirect and total impacts from its corresponding SDM-KNN6 model. ",
    "SPI is intentionally omitted from the final reporting set."
  ),style="Normal"
)

# Benchmark (not treated as a stigma measure)
doc <- add_table(doc,"Table 3. Benchmark: historical violence + current homicide — KNN6 spatial models",
                 TABLE7_HISTORY,
                 "Benchmark specification. Columns report SAR, SEM, SLX, SDM and SDEM under the same KNN6 matrix.")
doc <- add_table(doc,"Table 3.1. Benchmark SDM-KNN6 impacts — historical violence + current homicide",
                 TABLE7B_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 benchmark.")

# Stigma / violence-trajectory measures
doc <- add_table(doc,"Table 4. Objective Violence Legacy Gap (OVLG) — KNN6 spatial models",
                 TABLE7_GAP,
                 "All five KNN6 spatial specifications: SAR, SEM, SLX, SDM and SDEM.")
doc <- add_table(doc,"Table 4.1. SDM-KNN6 impacts — Objective Violence Legacy Gap",
                 TABLE7C_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

doc <- add_table(doc,"Table 5. High-past / low-current violence — KNN6 spatial models",
                 TABLE7_HILO,
                 "Recovery/nonlinear legacy contrast. All five KNN6 spatial specifications are reported.")
doc <- add_table(doc,"Table 5.1. SDM-KNN6 impacts — high-past / low-current violence",
                 TABLE7D_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

doc <- add_table(doc,"Table 6. High-past / high-current persistent violence — KNN6 spatial models",
                 TABLE7_HIHI,
                 "Primary binary persistent-violence specification. All five KNN6 spatial specifications are reported.")
doc <- add_table(doc,"Table 6.1. SDM-KNN6 impacts — persistent high violence",
                 TABLE7E_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

doc <- add_table(doc,"Table 7. Persistent Violence Intensity (PVI) — KNN6 spatial models",
                 TABLE7_PVI,
                 "Continuous persistence measure PVI=max[min(z historical violence, z current homicide),0].")
doc <- add_table(doc,"Table 7.1. SDM-KNN6 impacts — Persistent Violence Intensity",
                 TABLE7F_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

doc <- add_table(doc,"Table 8. Historical-current violence regimes — KNN6 spatial models",
                 TABLE7_REGIMES,
                 "Four-state violence-trajectory framework represented by the three non-reference regime indicators. All five KNN6 spatial specifications are reported.")
doc <- add_table(doc,"Table 8.1. SDM-KNN6 impacts — historical-current violence regimes",
                 TABLE7G_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

doc <- add_table(doc,"Table 9. Positive residualized violence legacy (PRVL) — KNN6 spatial models",
                 TABLE7_RESIDUAL,
                 "Residualized legacy robustness measure. All five KNN6 spatial specifications are reported.")
doc <- add_table(doc,"Table 9.1. SDM-KNN6 impacts — Positive Residualized Violence Legacy",
                 TABLE7H_IMPACTS,
                 "Direct, indirect and total impacts from the corresponding SDM-KNN6 model.")

DOCX_PATH <- file.path(DIR$paper,"Spatial_Econometrics_FINAL_KNN6_Stigma_Models_and_Impacts.docx")
print(doc,target=DOCX_PATH)

# -----------------------------------------------------------------------------
# 17. EXCEL + RDS
# -----------------------------------------------------------------------------
wb <- openxlsx::createWorkbook()
SHEETS <- list(
  expected_signs=EXPECTED_SIGNS,
  sample_summary=SAMPLE_SUMMARY,
  coverage=coverage,
  panel_variation=VARIATION,
  violence_SPI_diag=CORR_VIOLENCE,
  VIF_core=VIF_CORE,
  SPI_equivalence=EQUIV,
  W_diagnostics=W_DIAG,
  contig_component_diag=CONTIG_COMPONENT_DIAG,
  contig_evidence=CONTIG_EVIDENCE,
  contig_coefficients=CONTIG_COEFS,
  contig_wald=CONTIG_WALD,
  TWFE_staged=TWFE_ALL,
  TWFE_main=MAIN_TWFE,
  LM_all_W=LM_ALL,
  Moran_TWFE_all_W=moran_by_W,
  run_status=MODEL_RUN_STATUS,
  model_evidence=MODEL_EVIDENCE,
  SDM_to_SAR_Wald=WALD_WX,
  spatial_coefficients=COEFS_ALL,
  impacts_all_W=IMPACTS_ALL,
  impact_status=IMPACT_STATUS,
  KNN6_SDM_impacts_specs=IMPACTS_KNN6_SPECS,
  KNN6_SDM_impacts_status=IMPACTS_KNN6_SPECS_STATUS,
  Table7A1_SPI_impacts=TABLE7A_IMPACTS,
  Table7B1_HistCurrent_impacts=TABLE7B_IMPACTS,
  Table7C1_ObjectiveGap_impacts=TABLE7C_IMPACTS,
  Table7D1_HiLo_impacts=TABLE7D_IMPACTS,
  Table7E1_PersistentHigh_impacts=TABLE7E_IMPACTS,
  Table7F1_PVI_impacts=TABLE7F_IMPACTS,
  Table7G1_Regime_impacts=TABLE7G_IMPACTS,
  Table7H1_Residual_impacts=TABLE7H_IMPACTS,
  V10_rich_TWFE=V10_RICH_TWFE,
  IV_first_stage=IV_FIRST_STAGE,
  IV_spatial_results=IV_RESULTS,
  IV_spatial_status=IV_STATUS,
  precision_weighting=GEN_OUT,
  commune_year_FE=COMMUNE_YEAR_TAB,
  IV_readiness=IV_AUDIT
)
if(nrow(WITHIN_COMMUNE)) SHEETS$within_commune_outcome <- WITHIN_COMMUNE

for(nm in names(SHEETS)){
  openxlsx::addWorksheet(wb,nm)
  openxlsx::writeData(wb,nm,as.data.frame(SHEETS[[nm]]))
  openxlsx::freezePane(wb,nm,firstRow=TRUE)
  openxlsx::setColWidths(wb,nm,cols=seq_len(ncol(as.data.frame(SHEETS[[nm]]))),width="auto")
}
XLSX_PATH <- file.path(OUT_DIR,"04_CONTROL_Spatial_Econometrics_V10_12.xlsx")
openxlsx::saveWorkbook(wb,XLSX_PATH,overwrite=TRUE)

saveRDS(list(
  specifications=SPEC,main_sample=panel,main_ids=ids_main,
  weights=LW,neighborhoods=NB,TWFE=TWFE,main_TWFE=m_twfe_main,
  spatial_battery=BATTERY,
  contiguity_battery=if(exists("CONTIG_BATTERY",inherits=TRUE)) CONTIG_BATTERY else NULL,
  contiguity_component_diag=if(exists("CONTIG_COMPONENT_DIAG",inherits=TRUE)) CONTIG_COMPONENT_DIAG else tibble::tibble(),run_status=MODEL_RUN_STATUS,
  coefficients=COEFS_ALL,evidence=MODEL_EVIDENCE,
  wald_WX=WALD_WX,impacts=IMPACTS_ALL,impact_status=IMPACT_STATUS,
  KNN6_SDM_impacts_specs=IMPACTS_KNN6_SPECS,
  KNN6_SDM_impacts_status=IMPACTS_KNN6_SPECS_STATUS,
  Table7C1_ObjectiveGap_impacts=TABLE7C_IMPACTS,
  V10_rich_TWFE=V10_RICH_TWFE,
  iv_first_stage=IV_FIRST_STAGE,iv_spatial_results=IV_RESULTS,
  iv_spatial_status=IV_STATUS,iv_audit=IV_AUDIT
),file.path(DIR$models,"MODELS_04_V10_12.rds"))

# -----------------------------------------------------------------------------
# 18. FINAL CONSOLE REPORT
# -----------------------------------------------------------------------------
cat("\n",strrep("=",100),"\n",sep="")
cat("SCRIPT 04 V10.12 COMPLETED\n")
cat(strrep("=",100),"\n",sep="")
cat("Main S5_EXTENDED_CONTEXT sample: N=",N,", T=",TT,", NT=",NT,"\n",sep="")
cat("Word : ",DOCX_PATH,"\n",sep="")
cat("Excel: ",XLSX_PATH,"\n",sep="")
cat("\nMODEL RUN STATUS\n"); print(MODEL_RUN_STATUS,n=Inf,width=Inf)
cat("\nMODEL EVIDENCE\n"); print(MODEL_EVIDENCE,n=Inf,width=Inf)
cat("\nCONTIGUITY GIANT-COMPONENT EVIDENCE\n"); print(CONTIG_EVIDENCE,n=Inf,width=Inf)
cat("\nSDM -> SAR WALD\n"); print(WALD_WX,n=Inf,width=Inf)
cat("\nSPATIAL COEFFICIENTS\n"); print(COEFS_ALL,n=Inf,width=Inf)
cat("\nIMPACT STATUS\n"); print(IMPACT_STATUS,n=Inf,width=Inf)
cat("\nIMPACTS\n"); print(IMPACTS_ALL,n=Inf,width=Inf)
cat("\nKNN6 SDM IMPACTS — ALL VIOLENCE / LEGACY SPECIFICATIONS\n"); print(IMPACTS_KNN6_SPECS,n=Inf,width=Inf)
cat("\nTABLE 7C.1 — OBJECTIVE LEGACY GAP IMPACTS\n"); print(TABLE7C_IMPACTS,n=Inf,width=Inf)
cat("\nV10 RICH TWFE\n"); print(V10_RICH_TWFE,n=Inf,width=Inf)
cat("\nIV FIRST STAGE\n"); print(IV_FIRST_STAGE,n=Inf,width=Inf)
cat("\nEXPLORATORY SPATIAL IV\n"); print(IV_RESULTS,n=Inf,width=Inf)
cat("\nIMPORTANT: V6 does NOT choose the preferred model automatically.\n")
cat("Model selection requires joint review of LM/robust LM, residual Moran,\n")
cat("SDM->SAR restrictions, ML information criteria, parameter stability,\n")
cat("and Queen/Rook/KNN sensitivity.\n")
cat(strrep("=",100),"\n")
sessionInfo()
