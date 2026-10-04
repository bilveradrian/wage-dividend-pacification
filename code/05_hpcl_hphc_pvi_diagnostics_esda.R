# =============================================================================
# SCRIPT 05 FINAL — DIAGNOSTICS / DESCRIPTIVES / ESDA
# NEW VIOLENCE-TRAJECTORY MEASURES ONLY: HPLC, HPHC, PVI
# Medellín, 2006–2018 | KNN6
# =============================================================================
rm(list=ls()); gc()
options(stringsAsFactors=FALSE, scipen=999, width=200)
set.seed(20260921)

REQ <- c("dplyr","tidyr","tibble","sf","spdep","ggplot2","openxlsx")
miss <- REQ[!vapply(REQ,requireNamespace,logical(1),quietly=TRUE)]
if(length(miss)) stop("Install required packages first: ",paste(miss,collapse=", "))
invisible(lapply(REQ,library,character.only=TRUE))

# 1. PATHS --------------------------------------------------------------------
DATA_DIR <- "D:/2. Formación Posgrado/1. Doctorado en Ciencias Economicas/2. Thesis/Doctorado/Chapter III. Wages/datos"
WAGES_ROOT <- DATA_DIR

# Producto exacto generado por el Script 04 sin modificar.
RDS_ENV <- Sys.getenv("SPATIAL_RDS", unset = "")
RDS_CANDIDATES <- unique(c(
  RDS_ENV,
  file.path(WAGES_ROOT, "outputs_master", "04_spatial_econometrics",
            "05_SPATIAL_MODELS", "MODELS_04_V10_12.rds")
))
RDS_CANDIDATES <- RDS_CANDIDATES[nzchar(RDS_CANDIDATES)]
RDS_FILE <- RDS_CANDIDATES[file.exists(RDS_CANDIDATES)][1]
if (is.na(RDS_FILE) || !length(RDS_FILE)) {
  stop("No se encontró el RDS del Script 04. Ruta esperada: ",
       file.path(WAGES_ROOT, "outputs_master", "04_spatial_econometrics",
                 "05_SPATIAL_MODELS", "MODELS_04_V10_12.rds"))
}

SHP_FILE <- file.path(WAGES_ROOT, "Mapas", "BarrioVereda_2014.shp")
if (!file.exists(SHP_FILE)) stop("Shapefile not found: ", SHP_FILE)

OUT_DIR <- file.path(WAGES_ROOT, "outputs_master", "05_hpcl_hphc_pvi_diagnostics_esda")
DIR <- list(
  tables = file.path(OUT_DIR, "01_TABLES"),
  maps = file.path(OUT_DIR, "02_MAPS"),
  figures = file.path(OUT_DIR, "03_FIGURES"),
  data = file.path(OUT_DIR, "04_DATA")
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
invisible(lapply(DIR, dir.create, recursive = TRUE, showWarnings = FALSE))

OBJ <- readRDS(RDS_FILE)
if(is.null(OBJ$main_sample)) stop("RDS does not contain OBJ$main_sample")
d <- as.data.frame(OBJ$main_sample)
d$year <- as.integer(d$year)
d$barrio_id <- sprintf("%04d",as.integer(d$barrio_id))
Y <- "theta_eb_total"

if(!"financial_density_100k" %in% names(d)) {
  if("financial_density_10k" %in% names(d)) d$financial_density_100k <- 10*d$financial_density_10k
  else if(all(c("financial_establishments","population") %in% names(d))) d$financial_density_100k <- ifelse(d$population>0,100000*d$financial_establishments/d$population,NA_real_)
  else stop("Cannot construct financial_density_100k.")
}
FINAL_CONTEXT <- c("pct_informal","pct_tertiary","pct_housing_deprivation","financial_density_100k","distance_nearest_metrocable_km","arrests_rate_10k","pct_female_head","mean_age","mean_stratum")
NEW_VIOLENCE <- c("stigma_highpast_lowcurrent","stigma_highpast_highcurrent","persistent_violence_intensity")
needed <- unique(c(Y,FINAL_CONTEXT,NEW_VIOLENCE,"z_historical_violence","z_current_homicide"))
missing <- setdiff(needed,names(d)); if(length(missing)) stop("Missing variables: ",paste(missing,collapse=", "))

# Common sample across the THREE new violence measures: comparability is deliberate.
sample_c <- d |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(c(Y,FINAL_CONTEXT,NEW_VIOLENCE,"z_historical_violence","z_current_homicide")),is.finite)) |>
  dplyr::arrange(.data$barrio_id,.data$year)
years <- sort(unique(sample_c$year)); ids <- sort(unique(sample_c$barrio_id))
balanced <- all(table(sample_c$barrio_id)==length(years)) && all(table(sample_c$year)==length(ids))
if(!balanced) stop("Common final sample is not balanced.")

# 3. EXACT CONSTRUCTION AUDIT -------------------------------------------------
# Thresholds are YEAR-SPECIFIC, matching the final Script 04 definitions.
rebuild <- sample_c |>
  dplyr::group_by(.data$year) |>
  dplyr::mutate(
    q75_hist=stats::quantile(.data$z_historical_violence,.75,na.rm=TRUE,type=7),
    q50_curr=stats::quantile(.data$z_current_homicide,.50,na.rm=TRUE,type=7),
    q75_curr=stats::quantile(.data$z_current_homicide,.75,na.rm=TRUE,type=7),
    HPLC_rebuilt=as.numeric(.data$z_historical_violence>=.data$q75_hist & .data$z_current_homicide<=.data$q50_curr),
    HPHC_rebuilt=as.numeric(.data$z_historical_violence>=.data$q75_hist & .data$z_current_homicide>=.data$q75_curr),
    PVI_rebuilt=pmax(pmin(.data$z_historical_violence,.data$z_current_homicide),0)
  ) |> dplyr::ungroup()

IDENTITY <- tibble::tibble(
  measure=c("HPLC","HPHC","PVI"),
  saved_variable=NEW_VIOLENCE,
  max_abs_difference=c(
    max(abs(rebuild$stigma_highpast_lowcurrent-rebuild$HPLC_rebuilt),na.rm=TRUE),
    max(abs(rebuild$stigma_highpast_highcurrent-rebuild$HPHC_rebuilt),na.rm=TRUE),
    max(abs(rebuild$persistent_violence_intensity-rebuild$PVI_rebuilt),na.rm=TRUE)
  ),
  correlation=c(
    cor(rebuild$stigma_highpast_lowcurrent,rebuild$HPLC_rebuilt,use="complete.obs"),
    cor(rebuild$stigma_highpast_highcurrent,rebuild$HPHC_rebuilt,use="complete.obs"),
    cor(rebuild$persistent_violence_intensity,rebuild$PVI_rebuilt,use="complete.obs")
  )
)
print(IDENTITY,n=Inf,width=Inf)
if(any(IDENTITY$max_abs_difference>1e-10)) warning("At least one saved violence measure does not exactly match the rebuilt definition. Inspect thresholds before estimation.")

sample_c <- rebuild |>
  dplyr::mutate(HPLC=.data$stigma_highpast_lowcurrent,HPHC=.data$stigma_highpast_highcurrent,PVI=.data$persistent_violence_intensity)

# 4. SAMPLE / DESCRIPTIVES / IDENTIFICATION ----------------------------------
SAMPLE_AUDIT <- tibble::tibble(outcome=Y,N_barrios=length(ids),T_years=length(years),NT=nrow(sample_c),first_year=min(years),last_year=max(years),balanced_panel=balanced)

desc_one <- function(x) tibble::tibble(N=sum(is.finite(x)),mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE),min=min(x,na.rm=TRUE),p25=quantile(x,.25,na.rm=TRUE),median=median(x,na.rm=TRUE),p75=quantile(x,.75,na.rm=TRUE),max=max(x,na.rm=TRUE))
DESCRIPTIVES <- dplyr::bind_rows(lapply(c(Y,"HPLC","HPHC","PVI","z_historical_violence","z_current_homicide"),function(v) dplyr::mutate(desc_one(sample_c[[v]]),variable=v,.before=1)))

# FE identification: binary switchers + continuous within variation.
IDENTIFICATION <- sample_c |>
  dplyr::group_by(.data$barrio_id) |>
  dplyr::summarise(
    HPLC_mean=mean(.data$HPLC), HPLC_switches=sum(abs(diff(.data$HPLC))>0),
    HPHC_mean=mean(.data$HPHC), HPHC_switches=sum(abs(diff(.data$HPHC))>0),
    PVI_mean=mean(.data$PVI), PVI_within_sd=sd(.data$PVI), .groups="drop"
  )
IDENTIFICATION_SUMMARY <- tibble::tibble(
  measure=c("HPLC","HPHC","PVI"),
  barrios_with_within_variation=c(sum(IDENTIFICATION$HPLC_switches>0),sum(IDENTIFICATION$HPHC_switches>0),sum(IDENTIFICATION$PVI_within_sd>0,na.rm=TRUE)),
  share_barrios_with_within_variation=c(mean(IDENTIFICATION$HPLC_switches>0),mean(IDENTIFICATION$HPHC_switches>0),mean(IDENTIFICATION$PVI_within_sd>0,na.rm=TRUE))
)

YEAR_PREVALENCE <- sample_c |> dplyr::group_by(.data$year) |> dplyr::summarise(HPLC_share=mean(.data$HPLC),HPHC_share=mean(.data$HPHC),PVI_mean=mean(.data$PVI),PVI_positive_share=mean(.data$PVI>0),.groups="drop")
CORRELATIONS <- as.data.frame(cor(sample_c[,c(Y,"HPLC","HPHC","PVI","z_historical_violence","z_current_homicide")],use="pairwise.complete.obs")) |> tibble::rownames_to_column("variable")

# 5. KNN6 ESDA ----------------------------------------------------------------
geo <- sf::st_read(SHP_FILE,quiet=TRUE)
# Robust barrio-id detection.
id_candidates <- c("barrio_id","COD_BARRIO","cod_barrio","CODIGO","codigo")
id_geo <- id_candidates[id_candidates %in% names(geo)][1]
if(is.na(id_geo)) stop("Could not identify barrio id in shapefile. Available: ",paste(names(geo),collapse=", "))
geo$barrio_id <- sprintf("%04d",as.integer(geo[[id_geo]]))
geo <- geo[!duplicated(geo$barrio_id) & geo$barrio_id %in% ids,]
geo <- geo[match(ids,geo$barrio_id),]
if(nrow(geo)!=length(ids) || any(is.na(geo$barrio_id))) stop("Geometry does not match final common sample.")
g_proj <- if(sf::st_is_longlat(geo)) sf::st_transform(geo,3116) else geo
xy <- sf::st_coordinates(sf::st_point_on_surface(g_proj))
nb6 <- spdep::knn2nb(spdep::knearneigh(xy,k=6),row.names=ids)
lw6 <- spdep::nb2listw(nb6,style="W",zero.policy=TRUE)

moran_one <- function(x,var,yr){
  mt <- spdep::moran.test(x,lw6,zero.policy=TRUE,randomisation=TRUE)
  tibble::tibble(year=yr,variable=var,Moran_I=unname(mt$estimate[["Moran I statistic"]]),p_value=mt$p.value)
}
MORAN_YEAR <- dplyr::bind_rows(lapply(years,function(tt){
  z <- sample_c |> dplyr::filter(.data$year==tt) |> dplyr::arrange(match(.data$barrio_id,ids))
  dplyr::bind_rows(moran_one(z$HPLC,"HPLC",tt),moran_one(z$HPHC,"HPHC",tt),moran_one(z$PVI,"PVI",tt),moran_one(z[[Y]],"Wage effect",tt))
}))

# 6. FIGURES ------------------------------------------------------------------
theme_set(ggplot2::theme_minimal(base_size=11))
save_plot <- function(p,name,w=8,h=5) ggplot2::ggsave(file.path(DIR$figures,name),p,width=w,height=h,dpi=320)
p1 <- ggplot2::ggplot(YEAR_PREVALENCE,ggplot2::aes(.data$year,.data$HPLC_share))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(title="High-past / low-current violence (HPLC)",x=NULL,y="Share of barrios")
p2 <- ggplot2::ggplot(YEAR_PREVALENCE,ggplot2::aes(.data$year,.data$HPHC_share))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(title="High-past / high-current violence (HPHC)",x=NULL,y="Share of barrios")
p3 <- ggplot2::ggplot(YEAR_PREVALENCE,ggplot2::aes(.data$year,.data$PVI_mean))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(title="Persistent Violence Intensity (PVI)",x=NULL,y="Mean PVI")
save_plot(p1,"FIG01_HPLC_prevalence_by_year.png"); save_plot(p2,"FIG02_HPHC_prevalence_by_year.png"); save_plot(p3,"FIG03_PVI_mean_by_year.png")

# 7. EXPORT -------------------------------------------------------------------
write.csv(SAMPLE_AUDIT,file.path(DIR$tables,"sample_audit.csv"),row.names=FALSE)
write.csv(IDENTITY,file.path(DIR$tables,"construction_identity_checks.csv"),row.names=FALSE)
write.csv(DESCRIPTIVES,file.path(DIR$tables,"descriptives.csv"),row.names=FALSE)
write.csv(IDENTIFICATION_SUMMARY,file.path(DIR$tables,"within_identification_summary.csv"),row.names=FALSE)
write.csv(YEAR_PREVALENCE,file.path(DIR$tables,"year_prevalence.csv"),row.names=FALSE)
write.csv(CORRELATIONS,file.path(DIR$tables,"correlations.csv"),row.names=FALSE)
write.csv(MORAN_YEAR,file.path(DIR$tables,"moran_knn6_by_year.csv"),row.names=FALSE)

wb <- openxlsx::createWorkbook()
for(nm in c("Sample","Construction","Descriptives","Identification","Year prevalence","Correlations","Moran KNN6")) openxlsx::addWorksheet(wb,nm)
openxlsx::writeData(wb,"Sample",SAMPLE_AUDIT); openxlsx::writeData(wb,"Construction",IDENTITY); openxlsx::writeData(wb,"Descriptives",DESCRIPTIVES); openxlsx::writeData(wb,"Identification",IDENTIFICATION_SUMMARY); openxlsx::writeData(wb,"Year prevalence",YEAR_PREVALENCE); openxlsx::writeData(wb,"Correlations",CORRELATIONS); openxlsx::writeData(wb,"Moran KNN6",MORAN_YEAR)
openxlsx::saveWorkbook(wb,file.path(OUT_DIR,"05_HPLC_HPHC_PVI_Diagnostics_ESDA.xlsx"),overwrite=TRUE)
cat("\nSCRIPT 05 FINAL COMPLETE — HPLC / HPHC / PVI ONLY\nOutput: ",OUT_DIR,"\n",sep="")
