# ==============================================================================
# 03_MASTER_SPATIAL_ESDA_BARRIOS_2004_2018_V3_PAPER.R
# Spatial Wage Penalties and the Stigma of Violence - Medellin, 2006-2018
# PURPOSE
#   (1) Build the master barrio-year spatial database for the 16 urban communes.
#   (2) Merge place effects (02D), crime, finance, population and Metro/Metrocable.
#   (3) Construct transparent historical-violence / stigma candidates.
#   (4) Audit geography and candidate spatial weights.
#   (5) Run publication-grade ESDA: maps, Moran I, LISA and persistence.
#   (6) Use common cross-year cartographic scales and visible black barrio borders.
#   (7) Identify Queen/Rook islands explicitly and report Moran significance over time.
# IMPORTANT
#   - Spatial univt = URBAN BARRIO only. Veredas/corregimientos are excluded.
#   - barrio_id is ALWAYS character with 4 digits (e.g. 101 -> "0101").
#   - 7007 is a valid vereda code, not a barrio code; it is excluded by design.
#   - This script does NOT estimate the final causal spatial panel model.
# ============================================================================== 
rm(list = ls())
gc()

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260918)

# ---- Analytical window used throughout Script 03 ------------------------------
STUDY_START <- 2006L
STUDY_END   <- 2018L
STUDY_YEARS <- STUDY_START:STUDY_END


# ---- 0. Packages --------------------------------------------------------------
req <- c("sf","dplyr","tidyr","stringr","readxl","readr","purrr","tibble",
         "ggplot2","scales","spdep","openxlsx","viridisLite","units","patchwork")
new <- req[!vapply(req, requireNamespace, logical(1), quietly=TRUE)]
if(length(new)) install.packages(new, dependencies=TRUE)
invisible(lapply(req, library, character.only=TRUE))

# ---- 1. Paths -----------------------------------------------------------------
DATA_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos"
ECV_DIR  <- file.path(DATA_DIR,"ecv_medellin_2004_2025")
# All Script 03 products are stored inside the journal replication output tree.
OUT_DIR  <- file.path(DATA_DIR,"outputs_master","03_spatial_master_esda")
DIR <- c(
  data=file.path(OUT_DIR,"01_MASTER_DATA"),
  qa=file.path(OUT_DIR,"02_QA"),
  weights=file.path(OUT_DIR,"03_SPATIAL_WEIGHTS"),
  esda=file.path(OUT_DIR,"04_ESDA_TABLES"),
  paper=file.path(OUT_DIR,"05_PAPER_MAIN"),
  appendix=file.path(OUT_DIR,"06_APPENDIX")
)

dir.create(OUT_DIR,recursive=TRUE,showWarnings=FALSE)

# Remove only obsolete Script-03 output folders. These directories are not used
# anywhere in the current replication pipeline.
obsolete_dirs <- file.path(
  OUT_DIR,
  c("05_maps_JUE","06_figures_JUE","07_PAPER_MAIN","08_APPENDIX")
)
for(p in obsolete_dirs){
  if(dir.exists(p)) unlink(p,recursive=TRUE,force=TRUE)
}
invisible(lapply(DIR,dir.create,recursive=TRUE,showWarnings=FALSE))

# Remove stale publication images before estimation. Therefore an old PNG can
# never be mistaken for a figure from the current run.
old_png <- c(
  list.files(DIR["paper"],pattern="\\.png$",full.names=TRUE),
  list.files(DIR["appendix"],pattern="\\.png$",full.names=TRUE)
)
if(length(old_png)) unlink(old_png,force=TRUE)

find_first <- function(paths, label){
  z <- paths[file.exists(paths)]
  if(!length(z)) stop("No se encontro: ",label,"\nBuscado en:\n",paste(paths,collapse="\n"))
  normalizePath(z[1],winslash="/",mustWork=TRUE)
}

PLACE_FILE <- find_first(c(
  file.path(DATA_DIR,"outputs_master","02_place_effects","01_datos_analiticos","MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.csv")
  ),"MASTER_PLACE_EFFECTS_BARRIO_YEAR_2004_2018.csv")

CRIME_FILE <- find_first(c(file.path(DATA_DIR,"crime","delitos_2003_2019.xlsx"),file.path(DATA_DIR,"crime","delitos_2003_2019(1).xlsx")),"delitos 2003-2019")
FIN_FILE   <- find_first(c(file.path(DATA_DIR,"financiero","entidades_financieras_por_barrio_2003_2026.xlsx"),file.path(DATA_DIR,"financiero","entidades_financieras_por_barrio_2003_2026(2).xlsx")),"entidades financieras")
METRO_FILE <- find_first(c(file.path(DATA_DIR,"metro","metro_metrocable_medellin.xlsx"),file.path(DATA_DIR,"metro","metro_metrocable_medellin(1).xlsx")),"metro/metrocable")
POP_FILE   <- find_first(c(
  file.path(DATA_DIR,"poblacion","base_de_datos_poblacion_barrios_medellin_1993_2025.xlsx"),
  file.path(DATA_DIR,"poblacion","poblacion_barrios_medellin_2005_2020.xlsx"),
  file.path(DATA_DIR,"poblacion","poblacion_barrios_medellin_2005_2020(2).xlsx")
),"poblacion por barrio")
SHP_FILE   <- find_first(c(file.path(DATA_DIR,"Mapas","BarrioVereda_2014.shp"),file.path(DATA_DIR,"mapas","BarrioVereda_2014.shp")),"BarrioVereda_2014.shp")

cat("\nINPUTS\n",PLACE_FILE,"\n",CRIME_FILE,"\n",FIN_FILE,"\n",METRO_FILE,"\n",POP_FILE,"\n",SHP_FILE,"\n")

# ---- 2. Helpers ---------------------------------------------------------------
id4 <- function(x){
  x <- trimws(as.character(x)); x[x %in% c("","NA","NaN")] <- NA_character_
  ok <- grepl("^[0-9]+$",x)
  x[ok] <- stringr::str_pad(as.character(as.integer(x[ok])),4,"left","0")
  x
}
wmean <- function(x,w){
  ok <- is.finite(x)&is.finite(w)&w>0
  if(!any(ok)) return(NA_real_)
  weighted.mean(x[ok],w[ok])
}
zsafe <- function(x){
  s <- sd(x,na.rm=TRUE); m <- mean(x,na.rm=TRUE)
  if(!is.finite(s)||s==0) return(rep(NA_real_,length(x)))
  (x-m)/s
}

save_figure <- function(plot,path,width,height,dpi=600){
  if(file.exists(path)) unlink(path,force=TRUE)
  ggplot2::ggsave(
    filename=path,plot=plot,width=width,height=height,dpi=dpi,bg="white"
  )
  if(!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size<=0){
    stop("Figure was not created correctly: ",path)
  }
  info <- file.info(path)
  cat(sprintf(
    "FIGURE SAVED | %s | %.1f KB | %s\n",
    basename(path),info$size/1024,
    format(info$mtime,"%Y-%m-%d %H:%M:%S")
  ))
  invisible(path)
}

# ---- 3. Geography: official 2014 urban barrios -------------------------------
geo_all <- sf::st_read(SHP_FILE,quiet=TRUE)
geo <- geo_all %>%
  dplyr::mutate(barrio_id=id4(CODIGO), barrio_name_shape=NOMBRE) %>%
  dplyr::filter(SUBTIPO_BA==1, grepl("^[0-9]{4}$",barrio_id)) %>%
  dplyr::select(barrio_id,barrio_name_shape,dplyr::everything())
if(anyDuplicated(geo$barrio_id)) stop("Duplicated barrio_id in urban shape.")

# Projected CRS is preserved for distances; WGS84 only for station distances.
geo_ll <- st_transform(geo,4326)
geo_pt_ll <- st_point_on_surface(geo_ll)
geo_pt_xy <- st_coordinates(geo_pt_ll)
geo_cent <- tibble::tibble(barrio_id=geo$barrio_id, lon=geo_pt_xy[,1], lat=geo_pt_xy[,2])

# ---- 4. Place effects from Script 02D ----------------------------------------
place <- readr::read_csv(PLACE_FILE,show_col_types=FALSE) %>%
  dplyr::mutate(year=as.integer(year), barrio_id=id4(barrio_id), commune_id=as.integer(commune_id)) %>%
  dplyr::filter(year %in% STUDY_YEARS, commune_id %in% 1:16)

# Strict urban universe: IDs that exist as urban barrio polygons.
place_nonurban <- place %>% dplyr::filter(!barrio_id %in% geo$barrio_id)
readr::write_csv(place_nonurban,file.path(DIR["qa"],"QA_PLACE_EFFECTS_EXCLUDED_NONURBAN.csv"))
place <- place %>% dplyr::filter(barrio_id %in% geo$barrio_id)

# Crosswalk audit.
ids_place <- place %>% dplyr::distinct(barrio_id,barrio_name,commune_id,commune_name)
qa_cross <- dplyr::full_join(
  geo %>% st_drop_geometry() %>% dplyr::select(barrio_id,barrio_name_shape),
  ids_place,
  by="barrio_id"
) %>% dplyr::mutate(status=dplyr::case_when(
  !is.na(barrio_name_shape)&!is.na(barrio_name)~"MATCH",
  !is.na(barrio_name_shape)~"ONLY_SHAPE",
  TRUE~"ONLY_PLACE_EFFECTS"))
readr::write_csv(qa_cross,file.path(DIR["qa"],"QA_CROSSWALK_SHAPE_PLACE_EFFECTS.csv"))

# ---- 5. Crime -----------------------------------------------------------------
crime0 <- readxl::read_excel(CRIME_FILE,sheet=1) %>%
  dplyr::transmute(indicator=as.character(Indicadores), year=as.integer(Año),
            barrio_id=id4(`Barrio código`), barrio_name_crime=as.character(`Barrio nombre`),
            count=as.numeric(Cantidad)) %>%
  dplyr::filter(year %in% 2003:2019)

crime <- crime0 %>%
  dplyr::mutate(indicator_key=dplyr::case_when(
    stringr::str_to_lower(indicator)=="homicidio"~"homicide",
    stringr::str_detect(stringr::str_to_lower(indicator),"hurto a persona")~"robbery_person",
    stringr::str_detect(stringr::str_to_lower(indicator),"captura")~"arrests",
    TRUE~NA_character_)) %>%
  dplyr::filter(!is.na(indicator_key)) %>%
  dplyr::group_by(year,barrio_id,indicator_key) %>%
  dplyr::summarise(
    value=sum(count,na.rm=TRUE),
    .groups="drop"
  ) %>%
  # Original ESDA rule: within a barrio-year represented in the recognized crime
  # source, an absent recognized indicator is treated as zero.
  tidyr::pivot_wider(
    names_from=indicator_key,
    values_from=value,
    values_fill=0
  )

# Crime-source coverage audit for the original source-conditional coding rule.
# A barrio-year represented by at least one recognized crime indicator is source-covered.
# Missing recognized indicators inside such a covered barrio-year are coded 0 above.
# A barrio-year absent from all recognized crime records is NOT added to `crime`;
# after the master left join it therefore remains NA.
crime_presence <- crime0 %>%
  dplyr::mutate(indicator_key=dplyr::case_when(
    stringr::str_to_lower(indicator)=="homicidio"~"homicide",
    stringr::str_detect(stringr::str_to_lower(indicator),"hurto a persona")~"robbery_person",
    stringr::str_detect(stringr::str_to_lower(indicator),"captura")~"arrests",
    TRUE~NA_character_
  )) %>%
  dplyr::filter(!is.na(indicator_key)) %>%
  dplyr::distinct(year,barrio_id) %>%
  dplyr::mutate(crime_source_present=1L)

crime_coverage_audit <- tidyr::expand_grid(
  year=2003:2019,
  barrio_id=geo$barrio_id
) %>%
  dplyr::left_join(crime_presence,by=c("year","barrio_id")) %>%
  dplyr::left_join(crime,by=c("year","barrio_id")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_urban_barrio=dplyr::n(),
    n_source_covered=sum(crime_source_present==1L,na.rm=TRUE),
    n_source_absent=sum(is.na(crime_source_present)),
    n_homicide_available=sum(!is.na(homicide)),
    n_homicide_zero=sum(homicide==0,na.rm=TRUE),
    n_arrests_available=sum(!is.na(arrests)),
    n_arrests_zero=sum(arrests==0,na.rm=TRUE),
    n_robbery_available=sum(!is.na(robbery_person)),
    n_robbery_zero=sum(robbery_person==0,na.rm=TRUE),
    homicide_total=sum(homicide,na.rm=TRUE),
    arrests_total=sum(arrests,na.rm=TRUE),
    .groups="drop"
  )

# ---- 6. Population ------------------------------------------------------------
# Preferred source: audited 1993-2025 barrio population file. This provides the
# 2003 denominator required for historical homicide exposure and preserves the
# observed 2005-2018 population series used in the paper.
pop_sheets <- readxl::excel_sheets(POP_FILE)
pop_sheet <- if("poblacion_barrio_1993_2025" %in% pop_sheets) {
  "poblacion_barrio_1993_2025"
} else if("poblacion_barrio" %in% pop_sheets) {
  "poblacion_barrio"
} else {
  pop_sheets[1]
}
pop_raw <- readxl::read_excel(POP_FILE,sheet=pop_sheet)
nm_pop <- names(pop_raw)
pick_col <- function(candidates,label){
  hit <- candidates[candidates %in% nm_pop]
  if(!length(hit)) stop("Population file: missing column for ",label,". Available: ",paste(nm_pop,collapse=", "))
  hit[1]
}
pop_year_col <- pick_col(c("year","año","ano"),"year")
pop_id_col   <- pick_col(c("barrio_id","codigo_barrio"),"barrio_id")
pop_n_col    <- pick_col(c("population","poblacion_total"),"population")
pop_m_col    <- intersect(c("pop_male","hombres_total"),nm_pop)
pop_f_col    <- intersect(c("pop_female","mujeres_total"),nm_pop)
pop <- pop_raw %>%
  dplyr::transmute(
    year=as.integer(.data[[pop_year_col]]),
    barrio_id=id4(.data[[pop_id_col]]),
    population=as.numeric(.data[[pop_n_col]]),
    pop_male=if(length(pop_m_col)) as.numeric(.data[[pop_m_col[1]]]) else NA_real_,
    pop_female=if(length(pop_f_col)) as.numeric(.data[[pop_f_col[1]]]) else NA_real_
  ) %>%
  dplyr::filter(year %in% 1993:2025) %>%
  dplyr::group_by(year,barrio_id) %>%
  dplyr::summarise(dplyr::across(c(population,pop_male,pop_female),~if(all(is.na(.x))) NA_real_ else sum(.x,na.rm=TRUE)),.groups="drop")

# ---- 7. Financial establishments ---------------------------------------------
fin <- readxl::read_excel(FIN_FILE,sheet="financieras_barrio") %>%
  dplyr::transmute(year=as.integer(año),barrio_id=id4(id_barrio),financial_establishments=as.numeric(cantidad)) %>%
  dplyr::group_by(year,barrio_id) %>% dplyr::summarise(financial_establishments=sum(financial_establishments,na.rm=TRUE),.groups="drop")

# ---- 8. Metro / Metrocable ----------------------------------------------------
# Workbook contains descriptive rows; machine-readable header is row 4.
metro_raw <- readxl::read_excel(
  METRO_FILE,
  sheet = "Estaciones_Metro",
  col_names = FALSE
)

metro <- metro_raw[-c(1:4), ]
names(metro) <- as.character(unlist(metro_raw[4, ]))

metro <- metro %>%
  dplyr::transmute(
    station_id   = as.character(station_id),
    system       = as.character(system),
    line         = as.character(line),
    station_name = as.character(station_name),
    year_open    = suppressWarnings(as.integer(year_open)),
    station_lat  = suppressWarnings(as.numeric(latitude)),
    station_lon  = suppressWarnings(as.numeric(longitude))
  ) %>%
  dplyr::filter(
    !is.na(station_id),
    !is.na(year_open),
    is.finite(station_lat),
    is.finite(station_lon)
  )

# Convert stations to sf
metro_sf <- sf::st_as_sf(
  metro,
  coords = c("station_lon", "station_lat"),
  crs = 4326,
  remove = FALSE
)

# -------------------------------------------------------------------------------
# Distances barrio -> station
# IMPORTANT:
# Build indices explicitly instead of using as.table(dmat), which can generate
# factor labels and NA coercions in i/j.
# -------------------------------------------------------------------------------

dmat <- units::drop_units(
  sf::st_distance(geo_pt_ll, metro_sf)
) / 1000

stopifnot(
  nrow(dmat) == nrow(geo),
  ncol(dmat) == nrow(metro)
)

metro_dist_long <- tidyr::expand_grid(
  i = seq_len(nrow(dmat)),
  j = seq_len(ncol(dmat))
) %>%
  dplyr::mutate(
    distance_km = dmat[cbind(i, j)],
    barrio_id   = geo$barrio_id[i],
    station_id  = metro$station_id[j],
    year_open   = metro$year_open[j],
    system      = metro$system[j],
    line        = metro$line[j]
  ) %>%
  dplyr::select(
    barrio_id,
    station_id,
    year_open,
    system,
    line,
    distance_km
  )

# QA before constructing panel
stopifnot(
  !anyNA(metro_dist_long$barrio_id),
  !anyNA(metro_dist_long$station_id),
  !anyNA(metro_dist_long$year_open),
  !anyNA(metro_dist_long$distance_km)
)

# -------------------------------------------------------------------------------
# Annual accessibility panel
# A station contributes only from its opening year onward.
# -------------------------------------------------------------------------------

metro_panel <- tidyr::crossing(
  year = STUDY_YEARS,
  barrio_id = geo$barrio_id
) %>%
  dplyr::left_join(
    metro_dist_long,
    by = "barrio_id",
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    open = !is.na(year_open) & year_open <= year
  ) %>%
  dplyr::group_by(year, barrio_id) %>%
  dplyr::summarise(
    
    distance_nearest_open_station_km =
      if (any(open, na.rm = TRUE)) {
        min(distance_km[open], na.rm = TRUE)
      } else {
        NA_real_
      },
    
    n_open_stations_1km =
      sum(open & distance_km <= 1, na.rm = TRUE),
    
    n_open_stations_2km =
      sum(open & distance_km <= 2, na.rm = TRUE),
    
    n_open_stations_3km =
      sum(open & distance_km <= 3, na.rm = TRUE),
    
    distance_nearest_metrocable_km =
      if (any(open & system == "Metrocable", na.rm = TRUE)) {
        min(
          distance_km[open & system == "Metrocable"],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    
    n_open_metrocable_2km =
      sum(
        open & system == "Metrocable" & distance_km <= 2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    access_station_1km =
      as.integer(n_open_stations_1km > 0),
    
    access_station_2km =
      as.integer(n_open_stations_2km > 0),
    
    access_metrocable_2km =
      as.integer(n_open_metrocable_2km > 0)
  )

# -------------------------------------------------------------------------------
# Final QA
# -------------------------------------------------------------------------------

cat("\nMetro accessibility panel:\n")
cat("Rows:", format(nrow(metro_panel), big.mark = ","), "\n")
cat("Barrios:", dplyr::n_distinct(metro_panel$barrio_id), "\n")
cat("Years:", dplyr::n_distinct(metro_panel$year), "\n")
cat(
  "Missing barrio-year duplicates:",
  anyDuplicated(metro_panel[c("year", "barrio_id")]),
  "\n"
)

stopifnot(
  nrow(metro_panel) ==
    length(STUDY_YEARS) * dplyr::n_distinct(geo$barrio_id),
  
  anyDuplicated(
    metro_panel[c("year", "barrio_id")]
  ) == 0
)

# ---- 9. Build barrio-year master ---------------------------------------------
# Panel is defined by observed place effects. Geography remains separately complete.
master <- place %>%
  dplyr::left_join(pop,by=c("year","barrio_id")) %>%
  dplyr::left_join(crime,by=c("year","barrio_id")) %>%
  dplyr::left_join(fin,by=c("year","barrio_id")) %>%
  dplyr::left_join(metro_panel,by=c("year","barrio_id")) %>%
  dplyr::left_join(geo_cent,by="barrio_id")

# Missing crime records remain NA. In particular, absence of a homicide or
# arrest record is NOT interpreted as an observed zero. Rates are therefore
# calculated only when both the event count and a positive population denominator
# are observed.
master <- master %>%
  dplyr::mutate(
    homicide_rate_100k=ifelse(!is.na(homicide)&population>0,1e5*homicide/population,NA_real_),
    arrests_rate_100k=ifelse(!is.na(arrests)&population>0,1e5*arrests/population,NA_real_),
    robbery_person_rate_100k=ifelse(!is.na(robbery_person)&population>0,1e5*robbery_person/population,NA_real_),
    financial_per_10k=ifelse(population>0,1e4*financial_establishments/population,NA_real_),
    log_population=ifelse(population>0,log(population),NA_real_)
  )

# ---- 10. Historical violence / stigma candidates -----------------------------
# PRINCIPAL candidate: information available at t only.
# Historical exposure = mean lagged homicide rate from 2003 to t-1.
# This avoids using future violence to explain earlier wages.
hom_hist <- crime %>% dplyr::select(year,barrio_id,homicide) %>%
  dplyr::left_join(pop,by=c("year","barrio_id")) %>%
  dplyr::mutate(homicide_rate_100k=ifelse(population>0,1e5*homicide/population,NA_real_)) %>%
  dplyr::arrange(barrio_id,year) %>% dplyr::group_by(barrio_id) %>%
  dplyr::mutate(historical_homicide_rate=purrr::map_dbl(seq_along(year),function(i){
    idx <- which(year < year[i] & year>=2003)
    if(!length(idx) || all(is.na(homicide_rate_100k[idx]))) NA_real_ else mean(homicide_rate_100k[idx],na.rm=TRUE)
  })) %>% dplyr::ungroup() %>% dplyr::select(year,barrio_id,historical_homicide_rate)
master <- master %>% dplyr::left_join(hom_hist,by=c("year","barrio_id")) %>%
  dplyr::group_by(year) %>% dplyr::mutate(
    z_historical_violence=zsafe(historical_homicide_rate),
    z_current_homicide=zsafe(homicide_rate_100k),
    # Main objective-stigma measure: positive historical-current violence gap.
    OVLG=pmax(z_historical_violence-z_current_homicide,0),
    # Retained only as a backward-compatible diagnostic alias; not substantive.
    SPI_dynamic=z_historical_violence-z_current_homicide
  ) %>% dplyr::ungroup()

# Robustness measure: Positive Residualized Violence Legacy (PRVL).
# Within each year, residualize standardized historical violence on standardized
# contemporaneous homicide and retain only the positive residual component.
master <- master %>%
  dplyr::group_by(year) %>%
  dplyr::group_modify(~{
    d <- .x
    ok <- is.finite(d$z_historical_violence) & is.finite(d$z_current_homicide)
    d$PRVL <- NA_real_
    if(sum(ok) >= 3 && stats::sd(d$z_current_homicide[ok]) > 0){
      fit <- stats::lm(z_historical_violence ~ z_current_homicide, data=d[ok,,drop=FALSE])
      r <- stats::residuals(fit)
      d$PRVL[ok] <- pmax(r,0)
    }
    d
  }) %>%
  dplyr::ungroup()

# NEW: persistent-violence measures. Thresholds are computed WITHIN YEAR so the
# classification is relative to Medellin's contemporaneous violence distribution.
# HPHC captures the paper's persistence hypothesis directly: a barrio is in the
# top quartile of BOTH lagged historical exposure and current homicide.
# PVI is a continuous joint-intensity measure and is positive only when both
# standardized historical and current violence are above the annual mean.
master <- master %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    q75_hist = stats::quantile(z_historical_violence,.75,na.rm=TRUE,names=FALSE),
    q75_curr = stats::quantile(z_current_homicide,.75,na.rm=TRUE,names=FALSE),
    q50_curr = stats::quantile(z_current_homicide,.50,na.rm=TRUE,names=FALSE),
    stigma_highpast_highcurrent = dplyr::if_else(
      is.finite(z_historical_violence) & is.finite(z_current_homicide),
      as.integer(z_historical_violence >= q75_hist & z_current_homicide >= q75_curr),
      NA_integer_
    ),
    stigma_highpast_lowcurrent = dplyr::if_else(
      is.finite(z_historical_violence) & is.finite(z_current_homicide),
      as.integer(z_historical_violence >= q75_hist & z_current_homicide <= q50_curr),
      NA_integer_
    ),
    persistent_violence_intensity = dplyr::if_else(
      is.finite(z_historical_violence) & is.finite(z_current_homicide),
      pmax(pmin(z_historical_violence,z_current_homicide),0),
      NA_real_
    ),
    violence_regime_q75 = dplyr::case_when(
      !is.finite(z_historical_violence) | !is.finite(z_current_homicide) ~ NA_character_,
      z_historical_violence >= q75_hist & z_current_homicide >= q75_curr ~ "High past / high current",
      z_historical_violence >= q75_hist & z_current_homicide <  q75_curr ~ "High past / not-high current",
      z_historical_violence <  q75_hist & z_current_homicide >= q75_curr ~ "Not-high past / high current",
      TRUE ~ "Not-high past / not-high current"
    ),
    regime_HPLC_q75 = as.integer(violence_regime_q75 == "High past / not-high current"),
    regime_LPHC_q75 = as.integer(violence_regime_q75 == "Not-high past / high current"),
    regime_HPHC_q75 = as.integer(violence_regime_q75 == "High past / high current")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-q75_hist,-q75_curr,-q50_curr)

# Required-measure audit: fail here rather than later during figure construction.
required_bridge_vars <- c("OVLG","PRVL","stigma_highpast_highcurrent",
                          "persistent_violence_intensity","violence_regime_q75",
                          "cswp_eb_pct","theta_eb_total")
missing_bridge_vars <- setdiff(required_bridge_vars,names(master))
if(length(missing_bridge_vars)) stop("Missing required bridge variables: ",paste(missing_bridge_vars,collapse=", "))
if(!any(is.finite(master$OVLG))) stop("OVLG was created but has no finite observations in the analytical panel.")
if(!any(is.finite(master$PRVL))) stop("PRVL was created but has no finite observations in the analytical panel.")

# Legacy 2003-2015 stock: robustness/descriptive legacy measure ONLY.
# Do not interpret as predetermined for outcomes before 2015.
legacy <- crime %>% dplyr::filter(year>=2003,year<=2015) %>% dplyr::select(year,barrio_id,homicide) %>%
  dplyr::left_join(pop,by=c("year","barrio_id")) %>%
  dplyr::mutate(rate=ifelse(population>0,1e5*homicide/population,NA_real_)) %>%
  dplyr::group_by(barrio_id) %>% dplyr::summarise(historical_violence_2003_2015=mean(rate,na.rm=TRUE),.groups="drop")
master <- master %>% dplyr::left_join(legacy,by="barrio_id") %>%
  dplyr::group_by(year) %>% dplyr::mutate(SPI_legacy_2003_2015=zsafe(historical_violence_2003_2015)-z_current_homicide) %>% dplyr::ungroup()

# ---- 11. Coverage audit -------------------------------------------------------
coverage <- master %>% dplyr::group_by(year) %>% dplyr::summarise(
  n_barrio=dplyr::n_distinct(barrio_id), n_cswp=sum(!is.na(cswp_eb_pct)), n_pop=sum(!is.na(population)),
  n_homicide=sum(!is.na(homicide)), n_homicide_rate=sum(!is.na(homicide_rate_100k)),
  n_arrests=sum(!is.na(arrests)), n_arrests_rate=sum(!is.na(arrests_rate_100k)),
  n_finance=sum(!is.na(financial_establishments)), n_metro=sum(!is.na(distance_nearest_open_station_km)),
  n_ovlg=sum(!is.na(OVLG)), n_prvl=sum(!is.na(PRVL)),
  n_hphc=sum(!is.na(stigma_highpast_highcurrent)),
  n_hphc_one=sum(stigma_highpast_highcurrent==1,na.rm=TRUE),
  n_pvi=sum(!is.na(persistent_violence_intensity)), .groups="drop")
readr::write_csv(coverage,file.path(DIR["qa"],"QA_COVERAGE_BY_YEAR.csv"))
readr::write_csv(crime_coverage_audit,file.path(DIR["qa"],"QA_CRIME_SOURCE_COVERAGE_BY_YEAR.csv"))

# ---- 12. Spatial weights QA ---------------------------------------------------
# Weight universe: all official urban barrio polygons. Queen/Rook preserve topology.
nb_q <- spdep::poly2nb(geo,queen=TRUE,snap=1)
nb_r <- spdep::poly2nb(geo,queen=FALSE,snap=1)
coords_m <- st_coordinates(st_point_on_surface(geo))
nb_k4 <- spdep::knn2nb(spdep::knearneigh(coords_m,k=4))
nb_k6 <- spdep::knn2nb(spdep::knearneigh(coords_m,k=6))
nb_k8 <- spdep::knn2nb(spdep::knearneigh(coords_m,k=8))

nb_diag <- function(nb,name){
  nc <- spdep::n.comp.nb(nb)
  tibble::tibble(W=name,n=length(nb),mean_neighbors=mean(card(nb)),median_neighbors=median(card(nb)),
         min_neighbors=min(card(nb)),max_neighbors=max(card(nb)),islands=sum(card(nb)==0),components=nc$nc)
}
wqa <- dplyr::bind_rows(nb_diag(nb_q,"Queen"),nb_diag(nb_r,"Rook"),nb_diag(nb_k4,"KNN4"),nb_diag(nb_k6,"KNN6"),nb_diag(nb_k8,"KNN8"))
readr::write_csv(wqa,file.path(DIR["weights"],"QA_SPATIAL_WEIGHTS.csv"))

# Explicitly identify topology islands instead of silently relying on zero.policy.
island_tbl <- dplyr::bind_rows(
  tibble::tibble(W=rep("Queen",sum(spdep::card(nb_q)==0)), barrio_id=geo$barrio_id[spdep::card(nb_q)==0]),
  tibble::tibble(W=rep("Rook", sum(spdep::card(nb_r)==0)), barrio_id=geo$barrio_id[spdep::card(nb_r)==0])
) %>%
  dplyr::left_join(geo %>% st_drop_geometry() %>% dplyr::select(barrio_id,barrio_name_shape),by="barrio_id")
readr::write_csv(island_tbl,file.path(DIR["weights"],"QA_SPATIAL_ISLANDS_QUEEN_ROOK.csv"))
if(nrow(island_tbl)>0){
  cat("\nSpatial topology islands detected:\n")
  print(island_tbl)
}

# Save neighbor lists reproducibly.
saveRDS(list(queen=nb_q,rook=nb_r,knn4=nb_k4,knn6=nb_k6,knn8=nb_k8,barrio_order=geo$barrio_id),file.path(DIR["weights"],"SPATIAL_NEIGHBORS_URBAN_BARRIOS.rds"))

# ---- 13. Moran I by year and W ------------------------------------------------
# Subsetting is year-specific; zero.policy permits temporary islands after missing-Y removal.
get_nb <- function(name) switch(name,Queen=nb_q,Rook=nb_r,KNN4=nb_k4,KNN6=nb_k6,KNN8=nb_k8)
run_moran <- function(yvar,wname,yr,nsim=999){
  d <- master %>% dplyr::filter(year==yr) %>% dplyr::select(barrio_id,y=dplyr::all_of(yvar)) %>% dplyr::filter(is.finite(y))
  idx <- match(d$barrio_id,geo$barrio_id); ok <- !is.na(idx); d<-d[ok,]; idx<-idx[ok]
  if(nrow(d)<20) return(tibble::tibble(variable=yvar,W=wname,year=yr,n=nrow(d),I=NA,p_perm=NA))
  nb_sub <- subset(get_nb(wname), seq_along(geo$barrio_id) %in% idx)
  # subset() retains original relative order; align y to sorted original indices.
  ord <- order(idx); y <- d$y[ord]
  lw <- nb2listw(nb_sub,style="W",zero.policy=TRUE)
  mt <- moran.test(y,lw,zero.policy=TRUE,randomisation=TRUE)
  mc <- moran.mc(y,lw,nsim=nsim,zero.policy=TRUE)
  tibble::tibble(variable=yvar,W=wname,year=yr,n=length(y),I=unname(mt$estimate[1]),
         expected_I=unname(mt$estimate[2]),p_asym=mt$p.value,p_perm=mc$p.value)
}
vars_esda <- c("theta_eb_total","theta_within_commune","cswp_eb_pct","cswp_within_commune_pct",
               "historical_homicide_rate","homicide_rate_100k","OVLG","PRVL","arrests_rate_100k")
moran_tbl <- tidyr::crossing(variable=vars_esda,W=c("Queen","Rook","KNN4","KNN6","KNN8"),year=STUDY_YEARS) %>%
  purrr::pmap_dfr(~run_moran(..1,..2,..3,999))
readr::write_csv(moran_tbl,file.path(DIR["esda"],"MORAN_GLOBAL_BY_YEAR_AND_W.csv"))

# ---- 14. LISA using Queen as transparent topological baseline -----------------
run_lisa <- function(yvar,yr){
  d <- master %>% dplyr::filter(year==yr) %>% dplyr::select(barrio_id,y=dplyr::all_of(yvar)) %>% dplyr::filter(is.finite(y))
  idx <- match(d$barrio_id,geo$barrio_id); d<-d[!is.na(idx),]; idx<-idx[!is.na(idx)]
  nb_sub <- subset(nb_q,seq_along(geo$barrio_id)%in%idx); ord<-order(idx); d<-d[ord,]
  lw<-nb2listw(nb_sub,style="W",zero.policy=TRUE)
  z<-as.numeric(scale(d$y)); wz<-lag.listw(lw,z,zero.policy=TRUE)
  li<-localmoran_perm(d$y,lw,nsim=999,zero.policy=TRUE,iseed=20260918)
  pcol <- grep("Pr\\(",colnames(li),value=TRUE)[1]
  p <- if(length(pcol)) li[,pcol] else li[,5]
  p_fdr <- p.adjust(p,method="BH")
  q_raw <- dplyr::case_when(z>=0&wz>=0~"HH",z<0&wz<0~"LL",z>=0&wz<0~"HL",TRUE~"LH")
  q <- q_raw
  q[p>=0.05|is.na(p)] <- "Not significant"
  q_fdr <- q_raw
  q_fdr[p_fdr>=0.05|is.na(p_fdr)] <- "Not significant"
  tibble::tibble(year=yr,barrio_id=d$barrio_id,variable=yvar,value=d$y,z=z,wz=wz,
         local_I=li[,1],p_perm=p,p_fdr_bh=p_fdr,LISA=q,LISA_FDR=q_fdr)
}
lisa_total <- purrr::map_dfr(STUDY_YEARS,~run_lisa("theta_eb_total",.x))
lisa_within <- purrr::map_dfr(STUDY_YEARS,~run_lisa("theta_within_commune",.x))
lisa <- dplyr::bind_rows(lisa_total,lisa_within)
readr::write_csv(lisa,file.path(DIR["esda"],"LISA_QUEEN_999_PERMUTATIONS.csv"))

persistence <- lisa %>% dplyr::filter(p_perm<.05,LISA %in% c("HH","LL")) %>%
  dplyr::count(variable,barrio_id,LISA,name="n_years") %>% dplyr::arrange(variable,LISA,dplyr::desc(n_years))
readr::write_csv(persistence,file.path(DIR["esda"],"LISA_HH_LL_PERSISTENCE.csv"))

# ---- 15. Publication cartography V3 -------------------------------------------
# PRINCIPLES
#   * restrained, print-friendly palette;
#   * light geographic graticule in the background;
#   * barrio boundaries visible but secondary; commune boundaries emphasized;
#   * common scales across years;
#   * figures/tables explicitly classified as PAPER or APPENDIX.

# Geographic graticule is drawn in WGS84 while geometries remain in their native CRS.
MAP_DATUM <- sf::st_crs(4326)
COL_NEG <- "#6F8FBF"   # muted blue
COL_ZERO <- "#FAFAF8"  # near-white
COL_POS <- "#C97A73"   # muted red
COL_NA <- "#ECEBE7"
COL_BARRIO <- "#777777"
COL_COMMUNE <- "#222222"
COL_GRID <- "#D9D9D9"

# Commune outlines inferred from stable barrio -> commune mapping.
geo_commune <- geo %>%
  dplyr::left_join(ids_place %>% dplyr::distinct(barrio_id,commune_id),by="barrio_id") %>%
  dplyr::filter(!is.na(commune_id)) %>%
  dplyr::group_by(commune_id) %>% dplyr::summarise(.groups="drop")

theme_map_paper <- function(){
  theme_minimal(base_size=10.5)+
    theme(
      panel.grid.major=element_line(color=COL_GRID,linewidth=.28),
      panel.grid.minor=element_blank(),
      axis.title=element_blank(),
      axis.text=element_text(size=7.5,color="grey45"),
      axis.ticks=element_blank(),
      plot.title=element_text(face="bold",size=14,hjust=0),
      plot.subtitle=element_text(size=9.5,color="grey30"),
      plot.caption=element_text(size=7.5,color="grey40",hjust=0),
      legend.position="right",
      legend.title=element_text(size=8.5),
      legend.text=element_text(size=8.5),
      plot.margin=margin(8,10,8,8),
      panel.background=element_rect(fill="white",color=NA),
      plot.background=element_rect(fill="white",color=NA)
    )
}

common_sym_limits <- function(var, probs=c(.025,.975)){
  x <- master[[var]]; x <- x[is.finite(x)]
  if(!length(x)) return(c(-1,1))
  q <- stats::quantile(x,probs=probs,na.rm=TRUE,names=FALSE,type=7)
  M <- max(abs(q))
  if(!is.finite(M)||M==0) M <- max(abs(x),na.rm=TRUE)
  c(-M,M)
}
LIM_EB <- common_sym_limits("cswp_eb_pct")
LIM_WITHIN <- common_sym_limits("cswp_within_commune_pct")
scale_audit <- tibble::tibble(
  variable=c("cswp_eb_pct","cswp_within_commune_pct"),
  lower=c(LIM_EB[1],LIM_WITHIN[1]), upper=c(LIM_EB[2],LIM_WITHIN[2]),
  rule="Symmetric around zero; pooled P2.5/P97.5 absolute maximum; data unchanged; display squished"
)
readr::write_csv(scale_audit,file.path(DIR["qa"],"QA_COMMON_MAP_SCALES.csv"))

make_continuous_map <- function(var,yr,title=NULL,subtitle=NULL,limits,legend_title="Percent"){
  d <- master %>% dplyr::filter(year==yr) %>% dplyr::select(barrio_id,val=dplyr::all_of(var))
  g <- geo %>% dplyr::left_join(d,by="barrio_id")
  ggplot(g)+
    geom_sf(aes(fill=val),color=COL_BARRIO,linewidth=.13)+
    geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
    scale_fill_gradient2(low=COL_NEG,mid=COL_ZERO,high=COL_POS,midpoint=0,
                         limits=limits,oob=scales::squish,na.value=COL_NA,name=legend_title)+
    coord_sf(datum=MAP_DATUM,expand=FALSE)+
    labs(title=title,subtitle=subtitle,
         caption="Urban barrios of Medellín. Thin grey lines delimit barrios; darker lines delimit communes; grey fill = no estimate.")+
    theme_map_paper()
}

# ---- 15A. PAPER FIGURE 1: conditional neighborhood wage effects --------------
# Three benchmark years summarize beginning, middle and end of the study period.
p1a <- make_continuous_map("cswp_eb_pct",STUDY_START,paste0("Panel A: ",STUDY_START),NULL,LIM_EB)
p1b <- make_continuous_map("cswp_eb_pct",2012,"Panel B: 2012",NULL,LIM_EB)
p1c <- make_continuous_map("cswp_eb_pct",2018,"Panel C: 2018",NULL,LIM_EB)
FIG1 <- (p1a|p1b|p1c)+patchwork::plot_annotation(
  title="Figure 1. Conditional neighborhood wage premiums and penalties",
  subtitle="Empirical-Bayes barrio-year place effects; common 2006-2018 scale"
)
save_figure(FIG1,file.path(DIR["paper"],"FIGURE_01_CSWP_BENCHMARK_YEARS.png"),width=15.5,height=6.1)

# ---- 15B. PAPER FIGURE 2: within-commune heterogeneity ------------------------
p2a <- make_continuous_map("cswp_within_commune_pct",STUDY_START,paste0("Panel A: ",STUDY_START),NULL,LIM_WITHIN)
p2b <- make_continuous_map("cswp_within_commune_pct",2012,"Panel B: 2012",NULL,LIM_WITHIN)
p2c <- make_continuous_map("cswp_within_commune_pct",2018,"Panel C: 2018",NULL,LIM_WITHIN)
FIG2 <- (p2a|p2b|p2c)+patchwork::plot_annotation(
  title="Figure 2. Within-commune neighborhood wage heterogeneity",
  subtitle="Barrio deviation from its commune-year component; common 2006-2018 scale"
)
save_figure(FIG2,file.path(DIR["paper"],"FIGURE_02_WITHIN_COMMUNE_BENCHMARK_YEARS.png"),width=15.5,height=6.1)

# Export all benchmark-year standalone maps to APPENDIX, not main paper.
for(yr in c(2006,2008,2012,2015,2018)){
  pa <- make_continuous_map("cswp_eb_pct",yr,paste0("Conditional neighborhood wage premium/penalty, ",yr),
                            "Empirical-Bayes place effect; common 2006-2018 scale",LIM_EB)
  pb <- make_continuous_map("cswp_within_commune_pct",yr,paste0("Within-commune wage heterogeneity, ",yr),
                            "Barrio deviation from commune-year component; common 2006-2018 scale",LIM_WITHIN)
  save_figure(pa,file.path(DIR["appendix"],paste0("FIGURE_A_CSWP_",yr,".png")),width=7.2,height=8.2)
  save_figure(pb,file.path(DIR["appendix"],paste0("FIGURE_A_WITHIN_",yr,".png")),width=7.2,height=8.2)
}

# ---- 15C. LISA: FDR-BH is main inferential map -------------------------------
lisa_cols <- c(HH="#B85C62",LL="#5E7FA8",HL="#D7A29A",LH="#9FB5CF","Not significant"="#E8E8E5")
make_lisa_map <- function(yr,use_fdr=TRUE,title_prefix=""){
  lisa_var <- if(use_fdr) "LISA_FDR" else "LISA"
  dl <- lisa_total %>% dplyr::filter(year==yr) %>% dplyr::select(barrio_id,LISA=dplyr::all_of(lisa_var))
  g <- geo %>% dplyr::left_join(dl,by="barrio_id")
  sigtxt <- if(use_fdr) "FDR-BH q < 0.05" else "permutation p < 0.05"
  ggplot(g)+
    geom_sf(aes(fill=LISA),color=COL_BARRIO,linewidth=.13)+
    geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
    scale_fill_manual(values=lisa_cols,na.value=COL_NA,drop=FALSE)+
    coord_sf(datum=MAP_DATUM,expand=FALSE)+
    labs(title=paste0(title_prefix,yr),subtitle=paste0("Local Moran's I; Queen; 999 permutations; ",sigtxt),fill=NULL,
         caption="HH/LL = high-high/low-low spatial association. Thin grey lines delimit barrios; darker lines delimit communes.")+
    theme_map_paper()
}

# PAPER FIGURE 3: selected LISA years with multiplicity control.
p3a <- make_lisa_map(2006,TRUE,"Panel A: ")
p3b <- make_lisa_map(2012,TRUE,"Panel B: ")
p3c <- make_lisa_map(2018,TRUE,"Panel C: ")
FIG3 <- (p3a|p3b|p3c)+patchwork::plot_annotation(
  title="Figure 3. Local spatial clusters of conditional neighborhood wage effects",
  subtitle="FDR-adjusted Local Moran classification under Queen contiguity"
)
save_figure(FIG3,file.path(DIR["paper"],"FIGURE_03_LISA_FDR_SELECTED_YEARS.png"),width=15.5,height=6.1)

# Raw-p and FDR maps for all benchmark years -> appendix.
for(yr in c(2006,2008,2012,2015,2018)){
  save_figure(make_lisa_map(yr,FALSE),file.path(DIR["appendix"],paste0("FIGURE_A_LISA_RAW_",yr,".png")),width=7.2,height=8.2)
  save_figure(make_lisa_map(yr,TRUE),file.path(DIR["appendix"],paste0("FIGURE_A_LISA_FDR_",yr,".png")),width=7.2,height=8.2)
}

# ---- 15D. PAPER FIGURE 4: Moran I over time ----------------------------------
pdat <- moran_tbl %>%
  dplyr::filter(variable %in% c("theta_eb_total","theta_within_commune"),W %in% c("Queen","KNN6")) %>%
  dplyr::mutate(component=dplyr::recode(variable,theta_eb_total="Total barrio-year effect",theta_within_commune="Within-commune component"),
         significant=dplyr::if_else(is.finite(p_perm)&p_perm<.05,"p < 0.05","p >= 0.05"))
FIG4 <- ggplot(pdat,aes(year,I,linetype=W,group=interaction(component,W)))+
  geom_hline(yintercept=0,linewidth=.35,color="grey45")+geom_line(linewidth=.72,color="grey25")+
  geom_point(aes(shape=significant),size=2.0,stroke=.7,color="grey15")+
  facet_wrap(~component,ncol=1,scales="free_y")+
  scale_shape_manual(values=c("p < 0.05"=16,"p >= 0.05"=1))+
  scale_x_continuous(breaks=seq(STUDY_START,STUDY_END,2))+
  labs(title="Figure 4. Spatial dependence in conditional neighborhood wage effects",
       subtitle="Global Moran's I, 2006-2018; Queen baseline and KNN6 robustness",x=NULL,y="Moran's I",
       linetype="Spatial weights",shape="Permutation test")+
  theme_minimal(base_size=10.5)+theme(panel.grid.minor=element_blank(),legend.position="bottom",plot.title=element_text(face="bold",size=14))
save_figure(FIG4,file.path(DIR["paper"],"FIGURE_04_MORAN_TIME_TOTAL_WITHIN.png"),width=8.4,height=6.5)

# ---- 15E. PAPER FIGURE 5: persistence, FDR as conservative benchmark ----------
persistence_fdr <- lisa %>% dplyr::filter(p_fdr_bh<.05,LISA_FDR %in% c("HH","LL")) %>%
  dplyr::count(variable,barrio_id,LISA_FDR,name="n_years") %>% dplyr::arrange(variable,LISA_FDR,dplyr::desc(n_years))
readr::write_csv(persistence_fdr,file.path(DIR["esda"],"LISA_HH_LL_PERSISTENCE_FDR_BH.csv"))
pers_wide_fdr <- persistence_fdr %>% dplyr::filter(variable=="theta_eb_total") %>%
  dplyr::select(barrio_id,LISA=LISA_FDR,n_years) %>% tidyr::pivot_wider(names_from=LISA,values_from=n_years,values_fill=0) %>%
  dplyr::mutate(dominant=dplyr::case_when(HH>LL~"Persistent high-high",LL>HH~"Persistent low-low",HH==0&LL==0~"No persistent HH/LL",TRUE~"Mixed"),years=pmax(HH,LL))
gpers <- geo %>% dplyr::left_join(pers_wide_fdr,by="barrio_id")
FIG5 <- ggplot(gpers)+
  geom_sf(aes(fill=dominant,alpha=years),color=COL_BARRIO,linewidth=.13)+
  geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  scale_fill_manual(values=c("Persistent high-high"="#B85C62","Persistent low-low"="#5E7FA8","Mixed"="#9B8FB2","No persistent HH/LL"="#E8E8E5"),na.value=COL_NA)+
  scale_alpha_continuous(range=c(.30,.90),breaks=c(1,3,5,10,15),limits=c(1,15),oob=scales::squish)+
  coord_sf(datum=MAP_DATUM,expand=FALSE)+
  labs(title="Figure 5. Persistence of local wage clusters, 2006-2018",
       subtitle="Dominant FDR-significant HH/LL classification and number of years under Queen contiguity",fill=NULL,alpha="Years",
       caption="Color identifies dominant cluster type; intensity records persistence. FDR-BH q < 0.05.")+
  theme_map_paper()
save_figure(FIG5,file.path(DIR["paper"],"FIGURE_05_LISA_PERSISTENCE_FDR_2006_2018.png"),width=7.4,height=8.2)


# ---- 15F. PAPER FIGURE 6: OVLG and conditional wage effects -------------------
# OVLG is the principal objective-stigma regressor:
#   OVLG_bt = max[z(HV_bt) - z(CH_bt), 0].
# This figure is descriptive; causal interpretation is reserved for the spatial panel model.

sym_lim_longrun <- function(x){
  x <- x[is.finite(x)]
  if(!length(x)) return(c(-1,1))
  q <- stats::quantile(x,c(.025,.975),na.rm=TRUE,names=FALSE)
  M <- max(abs(q)); if(!is.finite(M)||M==0) M <- max(abs(x),na.rm=TRUE)
  c(-M,M)
}
seq_lim <- function(x){
  x <- x[is.finite(x)]
  if(!length(x)) return(c(0,1))
  hi <- as.numeric(stats::quantile(x,.975,na.rm=TRUE)); if(!is.finite(hi)||hi<=0) hi <- max(x,na.rm=TRUE)
  c(0,hi)
}

stigma_wage_longrun <- master %>%
  dplyr::filter(year %in% STUDY_YEARS) %>%
  dplyr::group_by(barrio_id) %>%
  dplyr::summarise(
    mean_historical_homicide_rate=if(all(is.na(historical_homicide_rate))) NA_real_ else mean(historical_homicide_rate,na.rm=TRUE),
    mean_current_homicide_rate=if(all(is.na(homicide_rate_100k))) NA_real_ else mean(homicide_rate_100k,na.rm=TRUE),
    mean_OVLG=if(all(is.na(OVLG))) NA_real_ else mean(OVLG,na.rm=TRUE),
    mean_PRVL=if(all(is.na(PRVL))) NA_real_ else mean(PRVL,na.rm=TRUE),
    mean_theta_eb=if(all(is.na(theta_eb_total))) NA_real_ else mean(theta_eb_total,na.rm=TRUE),
    mean_cswp_eb_pct=if(all(is.na(cswp_eb_pct))) NA_real_ else mean(cswp_eb_pct,na.rm=TRUE),
    years_OVLG_wage=sum(is.finite(OVLG)&is.finite(theta_eb_total)),
    years_PRVL_wage=sum(is.finite(PRVL)&is.finite(theta_eb_total)),
    .groups="drop"
  ) %>%
  dplyr::mutate(
    OVLG_wage_type=dplyr::case_when(
      !is.finite(mean_OVLG)|!is.finite(mean_theta_eb)~NA_character_,
      mean_OVLG>0 & mean_theta_eb<0~"Positive OVLG / wage penalty",
      mean_OVLG>0 & mean_theta_eb>=0~"Positive OVLG / wage premium",
      mean_OVLG<=0 & mean_theta_eb<0~"Zero OVLG / wage penalty",
      TRUE~"Zero OVLG / wage premium"),
    PRVL_wage_type=dplyr::case_when(
      !is.finite(mean_PRVL)|!is.finite(mean_theta_eb)~NA_character_,
      mean_PRVL>0 & mean_theta_eb<0~"Positive PRVL / wage penalty",
      mean_PRVL>0 & mean_theta_eb>=0~"Positive PRVL / wage premium",
      mean_PRVL<=0 & mean_theta_eb<0~"Zero PRVL / wage penalty",
      TRUE~"Zero PRVL / wage premium")
  )
readr::write_csv(stigma_wage_longrun,file.path(DIR["esda"],"OVLG_PRVL_WAGE_LONGRUN_SPATIAL_BRIDGE.csv"))

# Descriptive association audit only; these correlations do not account for spatial dependence.
STIGMA_WAGE_CORR <- tibble::tibble(
  measure=c("OVLG","OVLG","PRVL","PRVL"),
  statistic=c("Pearson correlation","Spearman correlation","Pearson correlation","Spearman correlation"),
  value=c(
    stats::cor(stigma_wage_longrun$mean_OVLG,stigma_wage_longrun$mean_theta_eb,use="complete.obs",method="pearson"),
    stats::cor(stigma_wage_longrun$mean_OVLG,stigma_wage_longrun$mean_theta_eb,use="complete.obs",method="spearman"),
    stats::cor(stigma_wage_longrun$mean_PRVL,stigma_wage_longrun$mean_theta_eb,use="complete.obs",method="pearson"),
    stats::cor(stigma_wage_longrun$mean_PRVL,stigma_wage_longrun$mean_theta_eb,use="complete.obs",method="spearman")
  )
)
readr::write_csv(STIGMA_WAGE_CORR,file.path(DIR["appendix"],"TABLE_A06_OVLG_PRVL_WAGE_DESCRIPTIVE_ASSOCIATION.csv"))

g_bridge <- geo %>% dplyr::left_join(stigma_wage_longrun,by="barrio_id")
LIM_OVLG <- seq_lim(stigma_wage_longrun$mean_OVLG)
LIM_PRVL <- seq_lim(stigma_wage_longrun$mean_PRVL)
LIM_WAGE <- sym_lim_longrun(stigma_wage_longrun$mean_cswp_eb_pct)

ovlg_cols <- c("Positive OVLG / wage penalty"="#8C4F62","Positive OVLG / wage premium"="#D59A86",
               "Zero OVLG / wage penalty"="#718EAF","Zero OVLG / wage premium"="#A9BBAA")
prvl_cols <- c("Positive PRVL / wage penalty"="#8C4F62","Positive PRVL / wage premium"="#D59A86",
               "Zero PRVL / wage penalty"="#718EAF","Zero PRVL / wage premium"="#A9BBAA")

p6a <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_OVLG),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_OVLG,oob=scales::squish,na.value=COL_NA,name="Mean OVLG")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel A: Objective Violence Legacy Gap",subtitle="Mean OVLG, 2006-2018")+theme_map_paper()
p6b <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_cswp_eb_pct),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient2(low="#6F8FB8",mid="#F5F4F0",high="#C56F68",midpoint=0,limits=LIM_WAGE,oob=scales::squish,na.value=COL_NA,name="Percent")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel B: Conditional wage effect",subtitle="Mean EB wage premium/penalty, 2006-2018")+theme_map_paper()
p6c <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=OVLG_wage_type),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_manual(values=ovlg_cols,na.value=COL_NA,drop=FALSE,name=NULL)+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel C: Spatial overlap",subtitle="OVLG and conditional wage sign typology",
                caption="Descriptive overlap only; causal effects are estimated in the spatial panel model.")+theme_map_paper()+ggplot2::theme(legend.position="bottom")
# PRVL panels are also included in Figure 6 so the paper can inspect both
# substantive violence-legacy measures against the same wage outcome.
p6d <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_PRVL),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_PRVL,oob=scales::squish,na.value=COL_NA,name="Mean PRVL")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel B: Positive Residualized Violence Legacy",subtitle="Mean PRVL, 2006-2018")+theme_map_paper()

p6e <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=PRVL_wage_type),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_manual(values=prvl_cols,na.value=COL_NA,drop=FALSE,name=NULL)+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel E: PRVL-wage spatial overlap",subtitle="PRVL and conditional wage sign typology")+
  theme_map_paper()+ggplot2::theme(legend.position="bottom")

# Relabel the common wage and OVLG-overlap panels for the combined figure.
p6b <- p6b + ggplot2::labs(title="Panel C: Conditional wage effect",subtitle="Mean EB wage premium/penalty, 2006-2018")
p6c <- p6c + ggplot2::labs(title="Panel D: OVLG-wage spatial overlap",subtitle="OVLG and conditional wage sign typology")

FIG6 <- (p6a | p6b | p6c) +
  patchwork::plot_annotation(
    title="Figure 6. Violence legacy measures and conditional neighborhood wage effects",
    subtitle="Long-run spatial patterns and descriptive overlap, Medellin 2006-2018",
    caption="OVLG = max[z(historical violence) - z(current homicide), 0]. PRVL = positive residual from annual historical-violence-on-current-homicide regressions. Overlap panels are descriptive, not causal."
  )
save_figure(FIG6,file.path(DIR["paper"],"FIGURE_06_OVLG_PRVL_WAGE_SPATIAL_BRIDGE.png"),width=16.9,height=6.4)
# Backward-compatible filename used by earlier manifests/workflows.
save_figure(FIG6,file.path(DIR["paper"],"FIGURE_06_OVLG_WAGE_SPATIAL_BRIDGE.png"),width=16.2,height=6.4)

# ---- 15G. PAPER FIGURE 7: PRVL robustness and conditional wage effects --------
p7a <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_PRVL),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_PRVL,oob=scales::squish,na.value=COL_NA,name="Mean PRVL")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel A: Positive Residualized Violence Legacy",subtitle="Mean PRVL, 2006-2018")+theme_map_paper()
p7b <- p6b + ggplot2::labs(title="Panel B: Conditional wage effect",subtitle="Mean EB wage premium/penalty, 2006-2018")
p7c <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=PRVL_wage_type),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_manual(values=prvl_cols,na.value=COL_NA,drop=FALSE,name=NULL)+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel C: Spatial overlap",subtitle="PRVL and conditional wage sign typology",
                caption="Robustness measure; descriptive overlap only.")+theme_map_paper()+ggplot2::theme(legend.position="bottom")
FIG7 <- (p7a|p7b|p7c)+patchwork::plot_annotation(
  title="Figure 7. Positive Residualized Violence Legacy and conditional neighborhood wage effects",
  subtitle="Long-run spatial overlap, Medellin 2006-2018; robustness measure")
save_figure(FIG7,file.path(DIR["paper"],"FIGURE_07_PRVL_WAGE_SPATIAL_BRIDGE.png"),width=16.9,height=6.4)

# ---- 15H. PAPER FIGURE 8: components underlying OVLG --------------------------
components_longrun <- stigma_wage_longrun
LIM_HIST <- seq_lim(components_longrun$mean_historical_homicide_rate)
LIM_CURR <- seq_lim(components_longrun$mean_current_homicide_rate)
p8a <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_historical_homicide_rate),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_HIST,oob=scales::squish,na.value=COL_NA,name="Per 100,000")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+ggplot2::labs(title="Panel A: Historical violence",subtitle="Mean lagged homicide rate per 100,000")+theme_map_paper()
p8b <- ggplot2::ggplot(g_bridge)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_current_homicide_rate),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_CURR,oob=scales::squish,na.value=COL_NA,name="Per 100,000")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+ggplot2::labs(title="Panel B: Contemporary homicide",subtitle="Mean current homicide rate per 100,000")+theme_map_paper()
p8c <- p6b + ggplot2::labs(title="Panel C: Conditional wage effect",subtitle="Mean EB wage premium/penalty, 2006-2018")
FIG8 <- (p8a|p8b|p8c)+patchwork::plot_annotation(
  title="Figure 8. Historical violence, contemporary homicide, and conditional neighborhood wage effects",
  subtitle="Components underlying the objective violence legacy measure, Medellin 2006-2018")
save_figure(FIG8,file.path(DIR["paper"],"FIGURE_08_HISTORICAL_CURRENT_WAGE_PANEL.png"),width=16.2,height=6.4)

# ---- 15I. PAPER TABLES ---------------------------------------------------------



# Table 1: spatial-weight diagnostics. Compact enough for main text.
TABLE1 <- wqa %>% dplyr::arrange(match(W,c("Queen","Rook","KNN4","KNN6","KNN8")))
readr::write_csv(TABLE1,file.path(DIR["paper"],"TABLE_01_SPATIAL_WEIGHTS_DIAGNOSTICS.csv"))

# Table 2: annual Moran I for core outcomes under Queen; KNN6 shown as robustness columns.
TABLE2 <- moran_tbl %>%
  dplyr::filter(variable %in% c("theta_eb_total","theta_within_commune"),W %in% c("Queen","KNN6")) %>%
  dplyr::transmute(year,component=dplyr::recode(variable,theta_eb_total="Total effect",theta_within_commune="Within-commune"),W,
            Moran_I=I,p_permutation=p_perm,n) %>%
  tidyr::pivot_wider(names_from=W,values_from=c(Moran_I,p_permutation,n),names_glue="{W}_{.value}") %>% dplyr::arrange(component,year)
readr::write_csv(TABLE2,file.path(DIR["paper"],"TABLE_02_GLOBAL_MORAN_CORE_RESULTS.csv"))


# Table 3: compact ESDA summary for paper discussion.
# Statistical significance is based on permutation tests, not on whether the
# arithmetic mean of annual Moran's I differs from zero. The long-run column
# applies Moran's I directly to each barrio's 2006-2018 mean spatial distribution.
core_esda_vars <- c("theta_eb_total","theta_within_commune","historical_homicide_rate",
                    "homicide_rate_100k","OVLG","PRVL")
var_labels <- c(
  theta_eb_total="Conditional wage effect (total)",
  theta_within_commune="Conditional wage effect (within commune)",
  historical_homicide_rate="Historical homicide rate",
  homicide_rate_100k="Contemporary homicide rate",
  OVLG="Objective Violence Legacy Gap (OVLG)",
  PRVL="Positive Residualized Violence Legacy (PRVL)"
)

run_longrun_moran <- function(yvar,wname,nsim=999){
  d <- master %>% dplyr::group_by(barrio_id) %>%
    dplyr::summarise(y=if(all(is.na(.data[[yvar]]))) NA_real_ else mean(.data[[yvar]],na.rm=TRUE),.groups="drop") %>%
    dplyr::filter(is.finite(y))
  idx <- match(d$barrio_id,geo$barrio_id); keep <- !is.na(idx); d <- d[keep,,drop=FALSE]; idx <- idx[keep]
  nb0 <- switch(wname,Queen=nb_q,KNN6=nb_k6,stop("Unsupported W in long-run Moran."))
  nb_sub <- spdep::subset.nb(nb0,seq_along(geo$barrio_id)%in%idx)
  ord <- order(idx); d <- d[ord,,drop=FALSE]
  lw <- spdep::nb2listw(nb_sub,style="W",zero.policy=TRUE)
  mt <- spdep::moran.mc(d$y,lw,nsim=nsim,zero.policy=TRUE,alternative="greater")
  tibble::tibble(variable=yvar,W=wname,n=nrow(d),long_run_I=unname(mt$statistic),long_run_p=mt$p.value)
}

annual_summary <- moran_tbl %>%
  dplyr::filter(variable %in% core_esda_vars,W %in% c("Queen","KNN6")) %>%
  dplyr::group_by(variable,W) %>%
  dplyr::summarise(
    mean_annual_I=mean(I,na.rm=TRUE), sd_annual_I=stats::sd(I,na.rm=TRUE),
    min_annual_I=min(I,na.rm=TRUE), max_annual_I=max(I,na.rm=TRUE),
    significant_years=sum(p_perm<0.05,na.rm=TRUE), years_tested=sum(is.finite(p_perm)),
    share_significant=significant_years/years_tested,.groups="drop")
longrun_summary <- tidyr::crossing(variable=core_esda_vars,W=c("Queen","KNN6")) %>%
  purrr::pmap_dfr(~run_longrun_moran(..1,..2,999))
TABLE3 <- annual_summary %>% dplyr::left_join(longrun_summary,by=c("variable","W")) %>%
  dplyr::mutate(indicator=unname(var_labels[variable]),
                long_run_significant=dplyr::if_else(long_run_p<0.05,"Yes","No")) %>%
  dplyr::select(indicator,variable,W,mean_annual_I,sd_annual_I,min_annual_I,max_annual_I,
                significant_years,years_tested,share_significant,long_run_I,long_run_p,long_run_significant) %>%
  dplyr::arrange(match(variable,core_esda_vars),match(W,c("Queen","KNN6")))
readr::write_csv(TABLE3,file.path(DIR["paper"],"TABLE_03_ESDA_SUMMARY_AVERAGE_AND_SIGNIFICANCE.csv"))

# Appendix tables: complete W sensitivity and local-cluster persistence.
readr::write_csv(moran_tbl,file.path(DIR["appendix"],"TABLE_A01_MORAN_ALL_VARIABLES_ALL_W.csv"))
readr::write_csv(persistence,file.path(DIR["appendix"],"TABLE_A02_LISA_PERSISTENCE_RAW.csv"))
readr::write_csv(persistence_fdr,file.path(DIR["appendix"],"TABLE_A03_LISA_PERSISTENCE_FDR.csv"))
readr::write_csv(coverage,file.path(DIR["appendix"],"TABLE_A04_DATA_COVERAGE_BY_YEAR.csv"))
readr::write_csv(island_tbl,file.path(DIR["appendix"],"TABLE_A05_SPATIAL_ISLANDS.csv"))

# Reproducible publication manifest: prevents later ambiguity about what belongs where.
manifest <- tibble::tribble(
  ~number,~type,~destination,~file,~purpose,
  "Figure 1","Figure","PAPER","FIGURE_01_CSWP_BENCHMARK_YEARS.png","Spatial distribution of EB conditional wage premiums/penalties",
  "Figure 2","Figure","PAPER","FIGURE_02_WITHIN_COMMUNE_BENCHMARK_YEARS.png","Heterogeneity remaining within communes",
  "Figure 3","Figure","PAPER","FIGURE_03_LISA_FDR_SELECTED_YEARS.png","Local HH/LL clusters after multiplicity correction",
  "Figure 4","Figure","PAPER","FIGURE_04_MORAN_TIME_TOTAL_WITHIN.png","Evolution and robustness of global spatial dependence",
  "Figure 5","Figure","PAPER","FIGURE_05_LISA_PERSISTENCE_FDR_2006_2018.png","Persistence of significant local clusters",
  "Figure 6","Figure","PAPER","FIGURE_06_OVLG_PRVL_WAGE_SPATIAL_BRIDGE.png","Combined OVLG, PRVL and conditional wage spatial bridge, 2006-2018",
  "Figure 7","Figure","PAPER","FIGURE_07_PRVL_WAGE_SPATIAL_BRIDGE.png","PRVL robustness spatial bridge to conditional wage effects",
  "Figure 8","Figure","PAPER","FIGURE_08_HISTORICAL_CURRENT_WAGE_PANEL.png","Historical violence, contemporary homicide and conditional wage effects",
  "Table 1","Table","PAPER","TABLE_01_SPATIAL_WEIGHTS_DIAGNOSTICS.csv","Transparency and diagnostics for candidate W matrices",
  "Table 2","Table","PAPER","TABLE_02_GLOBAL_MORAN_CORE_RESULTS.csv","Core global Moran results: Queen + KNN6 robustness",
  "Table 3","Table","PAPER","TABLE_03_ESDA_SUMMARY_AVERAGE_AND_SIGNIFICANCE.csv","Average annual Moran I, significant-year frequency and long-run mean-map permutation test",
  "Figures A1+","Figures","APPENDIX","FIGURE_A_*.png","Full benchmark-year maps and raw/FDR LISA robustness",
  "Tables A1-A6","Tables","APPENDIX","TABLE_A*.csv","Complete W sensitivity, persistence, coverage, islands and descriptive stigma-wage association"
)
readr::write_csv(manifest,file.path(OUT_DIR,"PUBLICATION_OUTPUT_MANIFEST.csv"))
# ---- 15I. PAPER FIGURE 9: persistent high violence and wage effects ------------
persistence_longrun <- master %>%
  dplyr::group_by(barrio_id) %>%
  dplyr::summarise(
    share_HPHC=mean(stigma_highpast_highcurrent,na.rm=TRUE),
    mean_PVI=mean(persistent_violence_intensity,na.rm=TRUE),
    mean_cswp=mean(cswp_eb_pct,na.rm=TRUE),
    years_HPHC=sum(stigma_highpast_highcurrent==1,na.rm=TRUE),
    .groups="drop"
  ) %>%
  dplyr::mutate(
    HPHC_wage_type=dplyr::case_when(
      !is.finite(share_HPHC)|!is.finite(mean_cswp)~NA_character_,
      share_HPHC>0 & mean_cswp<0~"Persistent violence / wage penalty",
      share_HPHC>0 & mean_cswp>=0~"Persistent violence / wage premium",
      share_HPHC==0 & mean_cswp<0~"No HPHC years / wage penalty",
      TRUE~"No HPHC years / wage premium")
  )
readr::write_csv(persistence_longrun,file.path(DIR["esda"],"PERSISTENT_VIOLENCE_WAGE_LONGRUN.csv"))

g_persist <- geo %>% dplyr::left_join(persistence_longrun,by="barrio_id")
LIM_PVI <- seq_lim(persistence_longrun$mean_PVI)
LIM_WAGE_PERSIST <- sym_lim_longrun(persistence_longrun$mean_cswp)
hphc_cols <- c("Persistent violence / wage penalty"="#8C4F62",
               "Persistent violence / wage premium"="#D59A86",
               "No HPHC years / wage penalty"="#718EAF",
               "No HPHC years / wage premium"="#A9BBAA")

p9a <- ggplot2::ggplot(g_persist)+
  ggplot2::geom_sf(ggplot2::aes(fill=share_HPHC),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=c(0,1),oob=scales::squish,na.value=COL_NA,name="Share HPHC")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel A: Persistent high violence",subtitle="Share of years in annual top quartile for both past and current violence")+theme_map_paper()
p9b <- ggplot2::ggplot(g_persist)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_PVI),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient(low="#F5F4F0",high="#B85C5C",limits=LIM_PVI,oob=scales::squish,na.value=COL_NA,name="Mean PVI")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel B: Persistence intensity",subtitle="Mean max[min(z historical, z current), 0]")+theme_map_paper()
p9c <- ggplot2::ggplot(g_persist)+
  ggplot2::geom_sf(ggplot2::aes(fill=mean_cswp),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_gradient2(low="#6F8FB8",mid="#F5F4F0",high="#C56F68",midpoint=0,limits=LIM_WAGE_PERSIST,oob=scales::squish,na.value=COL_NA,name="Percent")+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel C: Conditional wage effect",subtitle="Mean EB wage premium/penalty, 2006-2018")+theme_map_paper()
p9d <- ggplot2::ggplot(g_persist)+
  ggplot2::geom_sf(ggplot2::aes(fill=HPHC_wage_type),color=COL_BARRIO,linewidth=.13)+
  ggplot2::geom_sf(data=geo_commune,fill=NA,color=COL_COMMUNE,linewidth=.48,inherit.aes=FALSE)+
  ggplot2::scale_fill_manual(values=hphc_cols,na.value=COL_NA,drop=FALSE,name=NULL)+
  ggplot2::coord_sf(datum=MAP_DATUM,expand=FALSE)+
  ggplot2::labs(title="Panel D: Persistent violence and wage sign",subtitle="Descriptive long-run overlap")+theme_map_paper()+
  ggplot2::theme(legend.position="bottom",legend.justification="center",legend.box.just="center")
FIG9 <- (p9a|p9b|p9c|p9d)+patchwork::plot_annotation(
  title="Figure 9. Persistent violence and conditional neighborhood wage effects",
  subtitle="Medellin, 2006-2018",
  caption="HPHC=1 when historical violence and current homicide are both at or above their annual 75th percentiles. PVI=max[min(z historical,z current),0]. Overlap is descriptive, not causal.")
save_figure(FIG9,file.path(DIR["paper"],"FIGURE_09_PERSISTENT_VIOLENCE_WAGE_BRIDGE.png"),width=19,height=6.4)

# ---- 15J. PAPER FIGURE 10: annual violence-regime composition -----------------
regime_year <- master %>%
  dplyr::filter(!is.na(violence_regime_q75)) %>%
  dplyr::count(year,violence_regime_q75,name="n") %>%
  dplyr::group_by(year) %>% dplyr::mutate(share=100*n/sum(n)) %>% dplyr::ungroup()
readr::write_csv(regime_year,file.path(DIR["esda"],"VIOLENCE_REGIME_Q75_BY_YEAR.csv"))
FIG10 <- ggplot2::ggplot(regime_year,ggplot2::aes(x=year,y=share,group=violence_regime_q75,linetype=violence_regime_q75))+
  ggplot2::geom_line(linewidth=.8)+ggplot2::geom_point(size=1.7)+
  ggplot2::scale_x_continuous(breaks=STUDY_YEARS)+
  ggplot2::labs(title="Figure 10. Evolution of historical-current violence regimes",
                subtitle="Annual Q75 classification, Medellin 2006-2018",
                x=NULL,y="Share of barrios (%)",linetype=NULL,
                caption="High/high identifies persistent high violence; high/not-high identifies historical exposure without current top-quartile violence.")+
  ggplot2::theme_minimal(base_size=10)+ggplot2::theme(legend.position="bottom",axis.text.x=ggplot2::element_text(angle=45,hjust=1))
save_figure(FIG10,file.path(DIR["paper"],"FIGURE_10_VIOLENCE_REGIMES_OVER_TIME.png"),width=10.5,height=6.2)

# ---- 16. Save master spatial database -----------------------------------------
# CSV/RDS panel + GeoPackage (geometry repeated by year, useful for mapping/modeling).
# Canonical analytical outputs (2006-2018).
readr::write_csv(master,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2006_2018.csv"),na="")
saveRDS(master,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2006_2018.rds"))
master_sf <- geo %>% dplyr::select(barrio_id,barrio_name_shape,geometry) %>% dplyr::inner_join(master,by="barrio_id",relationship="many-to-many")
sf::st_write(master_sf,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2006_2018.gpkg"),layer="barrio_year",delete_dsn=TRUE,quiet=TRUE)

# Compatibility exports: Script 04 currently searches the legacy 2004_2018 name.
# These files contain exactly the same 2006-2018 analytical panel.
readr::write_csv(master,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2004_2018.csv"),na="")
saveRDS(master,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2004_2018.rds"))
sf::st_write(master_sf,file.path(DIR["data"],"MASTER_SPATIAL_BARRIO_YEAR_2004_2018.gpkg"),layer="barrio_year",delete_dsn=TRUE,quiet=TRUE)
st_write(geo,file.path(DIR["data"],"URBAN_BARRIOS_2014.gpkg"),layer="urban_barrios",delete_dsn=TRUE,quiet=TRUE)

# Excel workbook: compact audit/results, not geometry.
wb <- openxlsx::createWorkbook()
for(nm in c("coverage","crosswalk","weights","islands","map_scales","moran","persistence","persistence_fdr","paper_table1","paper_table2","paper_table3","stigma_wage","stigma_wage_corr","crime_coverage_audit","ovlg_prvl_longrun","persistent_violence","violence_regimes","manifest")) openxlsx::addWorksheet(wb,nm)
openxlsx::writeData(wb,"coverage",coverage)
openxlsx::writeData(wb,"crosswalk",qa_cross)
openxlsx::writeData(wb,"weights",wqa)
openxlsx::writeData(wb,"islands",island_tbl)
openxlsx::writeData(wb,"map_scales",scale_audit)
openxlsx::writeData(wb,"moran",moran_tbl)
openxlsx::writeData(wb,"persistence",persistence)
openxlsx::writeData(wb,"persistence_fdr",persistence_fdr)
openxlsx::writeData(wb,"paper_table1",TABLE1)
openxlsx::writeData(wb,"paper_table2",TABLE2)
openxlsx::writeData(wb,"paper_table3",TABLE3)
openxlsx::writeData(wb,"stigma_wage",stigma_wage_longrun)
openxlsx::writeData(wb,"stigma_wage_corr",STIGMA_WAGE_CORR)
openxlsx::writeData(wb,"crime_coverage_audit",crime_coverage_audit)
openxlsx::writeData(wb,"ovlg_prvl_longrun",stigma_wage_longrun)
openxlsx::writeData(wb,"persistent_violence",persistence_longrun)
openxlsx::writeData(wb,"violence_regimes",regime_year)
openxlsx::writeData(wb,"manifest",manifest)
openxlsx::saveWorkbook(wb,file.path(OUT_DIR,"03_CONTROL_SPATIAL_ESDA.xlsx"),overwrite=TRUE)

# ---- 17. Final QA --------------------------------------------------------------
stopifnot(all(nchar(master$barrio_id)==4),all(master$commune_id %in% 1:16),!any(master$barrio_id=="7007"))
cat("\n============================================================\n")
cat("SCRIPT 03 COMPLETE\n")
cat("Rows master:",nrow(master),"\n")
cat("Unique urban barrios:",dplyr::n_distinct(master$barrio_id),"\n")
cat("Years:",min(master$year),"-",max(master$year),"\n")
cat("Output:",OUT_DIR,"\n")
cat("Crime source-conditional coding audit saved: QA_CRIME_SOURCE_COVERAGE_BY_YEAR.csv\n")
cat("Homicide and arrest variables are expressed per 100,000 inhabitants when source counts are observed; missing source records remain NA and are never zero-imputed.\n")
cat("IMPORTANT: rate scaling changes coefficient units, not statistical information or significance by itself.\n")
cat("Analytical window:",STUDY_START,"-",STUDY_END,"\n")
cat("Legacy measures: OVLG and PRVL. Persistent-violence measures: HPHC and PVI.\n")
figure_files <- c(
  list.files(DIR["paper"],pattern="\\.png$",full.names=TRUE),
  list.files(DIR["appendix"],pattern="\\.png$",full.names=TRUE)
)
figure_audit <- if(length(figure_files)){
  fi <- file.info(figure_files)
  tibble::tibble(
    file=basename(figure_files),
    folder=basename(dirname(figure_files)),
    size_kb=round(fi$size/1024,1),
    modified=format(fi$mtime,"%Y-%m-%d %H:%M:%S")
  ) %>% dplyr::arrange(folder,file)
} else {
  tibble::tibble(file=character(),folder=character(),size_kb=numeric(),modified=character())
}
readr::write_csv(figure_audit,file.path(DIR["qa"],"QA_FIGURE_FILES_CURRENT_RUN.csv"))
cat("\n--- FIGURE FILE AUDIT: CURRENT RUN ---\n")
print(figure_audit,n=Inf,width=Inf)
main_figures_expected <- sprintf("FIGURE_%02d_", 1:8)

main_figures_found <- vapply(
  main_figures_expected,
  function(prefix) any(startsWith(figure_audit$file, prefix)),
  logical(1)
)

if (!all(main_figures_found)) {
  missing_figures <- main_figures_expected[!main_figures_found]
  
  stop(
    "The current run did not generate all eight main-paper figures. Missing: ",
    paste(missing_figures, collapse = ", ")
  )
}

cat(
  "Main-paper figure QA: PASS —",
  sum(main_figures_found),
  "of 8 figures generated.\n"
)

cat("\n--- ESDA SUMMARY: AVERAGE MORAN I AND SIGNIFICANCE ---\n")
print(TABLE3,n=Inf,width=Inf)
cat("\n--- OVLG / PRVL DESCRIPTIVE ASSOCIATION WITH WAGE EFFECTS ---\n")
print(STIGMA_WAGE_CORR,n=Inf,width=Inf)
cat("ESDA summary table saved: TABLE_03_ESDA_SUMMARY_AVERAGE_AND_SIGNIFICANCE.csv\n")
cat("Use Figures 6-8 and Table 3 as the descriptive bridge to the spatial panel econometric analysis.\n")
cat("============================================================\n")
