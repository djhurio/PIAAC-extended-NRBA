# 1. COMPARISON OF ESTIMATES BEFORE AND AFTER WEIGHTING ADJUSTMENTS
# APPENDIX A. EXTENDED NONRESPONSE BIAS ANALYSIS FILE LAYOUT

# Libs
library(data.table)
library(pxweb)
library(sf)
library(ggplot2)
library(openxlsx2)
library(purrr)

# Options
theme_set(new = theme_bw())

# Reset
rm(list = ls())
gc()

# Options
# import_px <- TRUE
import_px <- FALSE


# Function to categorise or recode to N2 format
categorise <- function(x, n = 100L) {
  cut(x = x, breaks = n, labels = FALSE, right = FALSE) - 1L
}

# Pretty range
prettyRange <- function(x, fmt = "%.5f") {
  paste0("[", paste(unique(sprintf(fmt = fmt, range(x))), collapse = ";"), "]")
}

# Fancy range for codebook
fancy.range <- function(x) {
  return(paste(range(x[!is.na(x)]), collapse = " to "))
}

# Generate codebook
make_codebook <- function(variable, nrba_variable, nrba_label, data = dat_nrba) {
  variable <- as.character(variable)[1]
  nrba_variable <- as.character(nrba_variable)[1]
  nrba_label <- as.character(nrba_label)[1]
  data <- as.data.table(data)
  
  if (!variable %in% names(data)) stop("Variable not in data")
  if (!nrba_variable %in% names(data)) stop("NRBA variable not in data")
  
  tab <- data[, .(
    `NRBA variable` = nrba_variable,
    `NRBA variable label` = nrba_label,
    `Value label` = prettyRange(get(variable))
  ), keyby = .(Values = get(nrba_variable))] |>
    setcolorder(c("NRBA variable", "NRBA variable label"))
  
  # tab_labels <- data[, .(
  #   labels = prettyRange(get(variable))
  # ), keyby = .(values = get(nrba_variable))]
  # 
  # tab <- data.table(
  #   `NRBA variable` = nrba_variable,
  #   `NRBA variable label` = nrba_label,
  #   `Values` = fancy.range(data[[nrba_variable]]),
  #   `Value label` = tab_labels[, paste(values, labels, sep = "=", collapse = "\n")]
  # )
  
  return(tab)
}


# Data

# Apkaimes
# https://data.gov.lv/dati/lv/dataset/apkaimes
shp_apk <- read_sf(dsn = "apkaimes")
# shp_apk
# shp_apk[c("Code", "geometry")]
# shp_apk[c("Code", "geometry")] |> plot()
# st_crs(shp_apk)

# ATVK on 2022-07-01
# https://data.gov.lv/dati/dataset/robezas
# https://data.gov.lv/dati/dataset/robezas/resource/d09c36c8-bcb2-4429-b88f-3a08d79c6aab
shp_atvk_2022 <- read_sf(dsn = "Territorial_units_LV_1.2m_(2022.07.01.)")
# shp_atvk_2022
# shp_atvk_2022[c("L1_code", "geometry")] |> plot()
# shp_atvk_2022[c("L0_code", "geometry")] |> plot()
# st_crs(shp_atvk_2022)


# Final SDIF
dat_sdif <- fread(file = "../PIAAC-data-2023/result/CY2_Final_SDIF_LVA.csvy",
                  yaml = TRUE)
# dat_sdif[, .(CNTRYID, CASEID, PERSID, AGE_R, GENDER_R)]
# dat_sdif[, .(CNTRYID, CASEID, PERSID, AGE_R, GENDER_R)] |> str()


# Sample file for fieldwork
dat_sample <- fread(
  file = "../PIAAC-sample-2022/data/sample_piaac_fw.csv",
  colClasses = list(numeric = "CASEID", character = "atvk")
)
# dat_sample[, .(CASEID, lon, lat, atvk)]
# dat_sample[, .(CASEID, lon, lat)] |> str()


# BQ
dat_bq <- readRDS(file = "../PIAAC-data-2023/data/dat_bq.rds")
# dat_bq[, .N, keyby = .(c2_q07)]
# dat_bq[, .(persid, c2_q07)]


# Spatial join
# dat_sample[is.na(lks92x) | is.na(lks92y)]

dat_sf <- st_as_sf(x = dat_sample[, .(lks92x, lks92y)],
                   coords = c("lks92x", "lks92y"))
# dat_sf
# class(dat_sf)

st_crs(shp_apk) == st_crs(shp_atvk_2022)

st_crs(dat_sf)
st_crs(dat_sf) <- st_crs(shp_apk)
st_crs(dat_sf)
dat_sf$geometry[is.na(dat_sf$geometry)]


# ATVK 2022
dat_atvk_2022 <- st_join(x = dat_sf, y = shp_atvk_2022,
                         join = st_nearest_feature)
dat_sample[, atvk_2022           := dat_atvk_2022$L1_code]
dat_sample[, adm_terit_kods_2022 := dat_atvk_2022$L0_code]
rm(dat_atvk_2022)

dat_sample[grep("Ķekava|Mārupe", atvk_nosauk), .N,
           keyby = .(atvk, atvk_nosauk, atvk_2022)]

dat_sample[is.na(atvk_2022), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat_sample[is.na(atvk_2022), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]
dat_sample[is.na(adm_terit_kods_2022), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat_sample[is.na(adm_terit_kods_2022), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]

# dat[, .N, keyby = .(adm_terit_kods, adm_terit_kods_2021)] |> View()
dat_sample[, .N, keyby = .(adm_terit_kods)]
dat_sample[, .N, keyby = .(adm_terit_kods_2022)]


# Add apkaimes
atvk_with_apk <- c("0001000", "0054010") # Rīga un Valmiera
atvk_with_apk

dat_apk <- st_join(x = dat_sf, y = shp_apk)

names(dat_sample)
dat_sample[, apk_kods   := dat_apk$Code]
dat_sample[, apk_nosauk := dat_apk$Name]

dat_sample[, .N, keyby = .(atvk %chin% atvk_with_apk,
                           !is.na(apk_kods),
                           !is.na(apk_nosauk))]
rm(dat_apk)


# Merge
dat_nrba <- merge(
  x = dat_sdif[, .(CNTRYID, CASEID, PERSID, WEIGHTFLG, GENDER_R, AGE_R, REGION,
                   PERSVAR1)],
  y = dat_sample[, .(CASEID, lon, lat, atvk_2022, apk_kods)],
  by = "CASEID",
  all.x = TRUE,
  sort = FALSE
) |> merge(
  y = dat_bq[, .(
    PERSID = persid,
    c2_q07, # Labour status
    b2_q01lv # Education level
  )],
  by = "PERSID",
  all.x = TRUE,
  sort = FALSE
) |> setcolorder(c("CNTRYID", "CASEID", "PERSID"))

dat_nrba
dat_nrba[, .N, keyby = .(PERSID = !is.na(PERSID), WEIGHTFLG)]

if (dat_nrba[is.na(lon) | is.na(lat), .N]) stop("Missing lon or lat")
if (dat_nrba[is.na(atvk_2022), .N]) stop("Missing ATVK")

ggplot(data = dat_nrba,
       mapping = aes(x = lon, y = lat, colour = substr(atvk_2022, 1, 6))) +
  geom_point(size = 1) +
  theme(legend.position = "none")

dat_nrba[, NRBAVAR1 := categorise(lon)]
dat_nrba[, NRBAVAR2 := categorise(lat)]

dat_nrba[, .(prettyRange(lon)), keyby = .(NRBAVAR1)]
dat_nrba[, .(prettyRange(lat)), keyby = .(NRBAVAR2)]

ggplot(data = dat_nrba,
       mapping = aes(x = NRBAVAR1, y = NRBAVAR2,
                     colour = substr(atvk_2022, 1, 6))) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 5), minor_breaks = 0:99) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), minor_breaks = 0:99) +
  theme(legend.position = "none")

dat_nrba[, cor(lon, NRBAVAR1)]
dat_nrba[, cor(lat, NRBAVAR2)]

codebook_NRBAVAR1 <- make_codebook(
  variable = "lon",
  nrba_variable = "NRBAVAR1",
  nrba_label = "Longitude of a dwelling"
)

codebook_NRBAVAR2 <- make_codebook(
  variable = "lat",
  nrba_variable = "NRBAVAR2",
  nrba_label = "Latitude of a dwelling"
)


# Share of population with ISCED education level
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__IZG__IZ__IZI/IZT041/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/IZG/IZ/IZI/IZT041"

if (import_px) {
  api_meta <- pxweb_get(url = api_url)
  saveRDS(object = api_meta, file = "data/IZT041-meta.rds")
} else {
  api_meta <- readRDS(file = "data/IZT041-meta.rds")
}

api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
grep("LVDPA", api_meta$variables[[2]]$values, value = T, invert = T)
grep("^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
     api_meta$variables[[2]]$values, value = T)

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

if (import_px) {
  dat_IZT041 <- pxweb_get(
    url = api_url,
    query = pxweb_query(x = list(
      EDUCATION_LEVEL = "*",
      AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                  x = api_meta$variables[[2]]$values,
                  value = T),
      ContentsCode = "IZT041",
      TIME = "2022"))
  )
  saveRDS(object = dat_IZT041, file = "data/IZT041-data.rds")
} else {
  dat_IZT041 <- readRDS(file = "data/IZT041-data.rds")
}

dat_IZT041$columns
col_names <- purrr::map_chr(
  .x = dat_IZT041$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_IZT041 <- purrr::map(
  .x = dat_IZT041$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_IZT041)
setnames(dat_IZT041, col_names)
dat_IZT041

dat_IZT041[, .N, keyby = .(value)]
# Non numeric values
dat_IZT041[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_IZT041[dat_IZT041[grep("\\(.*\\)", value), .(area)], on = "area"]
dat_IZT041[!grep("^[0-9]+$", value), value := NA]
dat_IZT041[, value := as.integer(value)]

dat_IZT041[, .N, keyby = .(education_level)]

dat_IZT041[, prop := value / value[education_level == "TOTAL"],
           keyby = .(area, time)]
dat_IZT041[, sum(prop, na.rm = TRUE), keyby = .(area, time)][, .N, keyby = .(V1)]

dat_IZT041 <- dcast.data.table(data = dat_IZT041[education_level != "TOTAL"],
                               formula = area ~ education_level,
                               value.var = "prop")

nrba_var_list <- paste0(
  "NRBAVAR", seq(from = 3, by = 1, length.out = ncol(dat_IZT041) - 1)
)

setnames(dat_IZT041, c("area", nrba_var_list))

dat_IZT041_ter <- dat_IZT041[grep("LV[0-9]{7}", area)]
setnames(dat_IZT041_ter, c("atvk_2022", paste0(nrba_var_list, "_ter")))

dat_IZT041_apk <- dat_IZT041[grep("LV[A-Z]{3}[0-9]{2}", area)]
setnames(dat_IZT041_apk, c("apk_kods", paste0(nrba_var_list, "_apk")))

dat_nrba[, .(atvk_2022, apk_kods)]

dat_nrba <- merge(dat_nrba, dat_IZT041_ter, by = "atvk_2022", all.x = T, sort = F)
dat_nrba <- merge(dat_nrba, dat_IZT041_apk, by = "apk_kods",  all.x = T, sort = F)
rm(dat_IZT041, dat_IZT041_apk, dat_IZT041_ter)
setcolorder(dat_nrba, c("CNTRYID", "CASEID", "PERSID"))

dat_nrba[, NRBAVAR3_val := fifelse(is.na(NRBAVAR3_apk), NRBAVAR3_ter, NRBAVAR3_apk)]
dat_nrba[, NRBAVAR4_val := fifelse(is.na(NRBAVAR4_apk), NRBAVAR4_ter, NRBAVAR4_apk)]
dat_nrba[, NRBAVAR5_val := fifelse(is.na(NRBAVAR5_apk), NRBAVAR5_ter, NRBAVAR5_apk)]
dat_nrba[, NRBAVAR6_val := fifelse(is.na(NRBAVAR6_apk), NRBAVAR6_ter, NRBAVAR6_apk)]
dat_nrba[, NRBAVAR7_val := fifelse(is.na(NRBAVAR7_apk), NRBAVAR7_ter, NRBAVAR7_apk)]

dat_nrba[, c(paste0(nrba_var_list, "_ter"), paste0(nrba_var_list, "_apk")) := NULL]

dat_nrba[, as.list(summary(NRBAVAR3_val))]
dat_nrba[, as.list(summary(NRBAVAR4_val))]
dat_nrba[, as.list(summary(NRBAVAR5_val))]
dat_nrba[, as.list(summary(NRBAVAR6_val))]
dat_nrba[, as.list(summary(NRBAVAR7_val))]

dat_nrba[, NRBAVAR3 := categorise(NRBAVAR3_val)]
dat_nrba[, NRBAVAR4 := categorise(NRBAVAR4_val)]
dat_nrba[, NRBAVAR5 := categorise(NRBAVAR5_val)]
dat_nrba[, NRBAVAR6 := categorise(NRBAVAR6_val)]
dat_nrba[, NRBAVAR7 := categorise(NRBAVAR7_val)]

dat_nrba[, .N, keyby = .(NRBAVAR3)][, P := round(prop.table(N), 3)][]
dat_nrba[, .N, keyby = .(NRBAVAR4)][, P := round(prop.table(N), 3)][]
dat_nrba[, .N, keyby = .(NRBAVAR5)][, P := round(prop.table(N), 3)][]
dat_nrba[, .N, keyby = .(NRBAVAR6)][, P := round(prop.table(N), 3)][]
dat_nrba[, .N, keyby = .(NRBAVAR7)][, P := round(prop.table(N), 3)][]

dat_nrba[, cor(NRBAVAR3_val, NRBAVAR3)]
dat_nrba[, cor(NRBAVAR4_val, NRBAVAR4)]
dat_nrba[, cor(NRBAVAR5_val, NRBAVAR5)]
dat_nrba[, cor(NRBAVAR6_val, NRBAVAR6)]
dat_nrba[, cor(NRBAVAR7_val, NRBAVAR7)]

codebook_NRBAVAR3 <- make_codebook(
  variable = "NRBAVAR3_val",
  nrba_variable = "NRBAVAR3",
  nrba_label = "Share of population with ISCED 0-1"
)

codebook_NRBAVAR4 <- make_codebook(
  variable = "NRBAVAR4_val",
  nrba_variable = "NRBAVAR4",
  nrba_label = "Share of population with ISCED 2"
)

codebook_NRBAVAR5 <- make_codebook(
  variable = "NRBAVAR5_val",
  nrba_variable = "NRBAVAR5",
  nrba_label = "Share of population with ISCED 3"
)

codebook_NRBAVAR6 <- make_codebook(
  variable = "NRBAVAR6_val",
  nrba_variable = "NRBAVAR6",
  nrba_label = "Share of population with ISCED 4"
)

codebook_NRBAVAR7 <- make_codebook(
  variable = "NRBAVAR7_val",
  nrba_variable = "NRBAVAR7",
  nrba_label = "Share of population with ISCED 5-8"
)


# 2. COMPARISON OF WEIGHTED ESTIMATES TO EXTERNAL TOTALS
# Ekonomiskās aktivitātes statuss anketā: C2_Q07
# Izglītības līmenis: B2_Q01LV

dat_nrba[, .N, keyby = .(c2_q07)]
dat_nrba[!c2_q07 %in% 1:10, c2_q07 := NA]
dat_nrba[, .N, keyby = .(c2_q07)]

dat_nrba[, .N, keyby = .(b2_q01lv)]
dat_nrba[!b2_q01lv %in% 0:16, b2_q01lv := NA]
dat_nrba[, .N, keyby = .(b2_q01lv)]

dat_nrba[, .N, keyby = .(
  PERSID = !is.na(PERSID),
  GENDER_R = !is.na(GENDER_R),
  AGE_R = !is.na(AGE_R),
  WEIGHTFLG,
  c2_q07 = !is.na(c2_q07),
  b2_q01lv = !is.na(b2_q01lv))]

dat_nrba[WEIGHTFLG == 0 & (!is.na(c2_q07) | !is.na(b2_q01lv)),
         .(PERSID, GENDER_R, AGE_R)][order(PERSID)]

dat_nrba[WEIGHTFLG == 0 & (!is.na(c2_q07) | !is.na(b2_q01lv)), WEIGHTFLG := 1]

dat_nrba[, .N, keyby = .(
  PERSID = !is.na(PERSID),
  GENDER_R = !is.na(GENDER_R),
  AGE_R = !is.na(AGE_R),
  WEIGHTFLG,
  c2_q07 = !is.na(c2_q07),
  b2_q01lv = !is.na(b2_q01lv))]

# set.seed(215526)
# dat_imp1 <- VIM::hotdeck(
#   data = dat_nrba[!is.na(PERSID), .(PERSID, GENDER_R, AGE_R, c2_q07)],
#   variable = "c2_q07",
#   domain_var = c("GENDER_R", "AGE_R")
# )
# 
# set.seed(215526)
# dat_imp2 <- VIM::hotdeck(
#   data = dat_nrba[!is.na(PERSID), .(PERSID, GENDER_R, AGE_R, c2_q07)],
#   variable = "c2_q07",
#   domain_var = c("GENDER_R", "AGE_R")
# )
# 
# all.equal(dat_imp1, dat_imp2)

set.seed(215526)
dat_imp <- VIM::hotdeck(
  data = dat_nrba[WEIGHTFLG == 1,
                  .(PERSID, REGION, GENDER_R, AGE_R, c2_q07, b2_q01lv)],
  variable = c("c2_q07", "b2_q01lv"),
  domain_var = c("REGION", "GENDER_R", "AGE_R")
)

dat_imp
dat_imp[, .N, keyby = .(c2_q07_imp, b2_q01lv_imp)]

dat_nrba[, c2_q07 := NULL]
dat_nrba[, b2_q01lv := NULL]

dat_nrba <- merge(
  x = dat_nrba,
  y = dat_imp[, .(PERSID, c2_q07, c2_q07_imp, b2_q01lv, b2_q01lv_imp)],
  by = "PERSID",
  all.x = TRUE,
  sort = FALSE
) |> setcolorder(c("CNTRYID", "CASEID", "PERSID"))

dat_nrba

dat_nrba[, .N, keyby = .(WEIGHTFLG, c2_q07)]
dat_nrba[, .N, keyby = .(WEIGHTFLG, c2_q07_imp)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(c2_q07_imp)][, P := prop.table(N)][]

dat_nrba[, .N, keyby = .(WEIGHTFLG, b2_q01lv)]
dat_nrba[, .N, keyby = .(WEIGHTFLG, b2_q01lv_imp)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(b2_q01lv_imp)][, P := prop.table(N)][]


# Economic activity
dat_nrba[c2_q07 %in% 1:2,  ecact := 1L]
dat_nrba[c2_q07 %in% 3,    ecact := 2L]
dat_nrba[c2_q07 %in% 4:10, ecact := 3L]
dat_nrba[, ecact := factor(
  x = ecact,
  levels = 1:3,
  labels = c("Employed", "Unemployed", "Inactive population")
)]

dat_nrba[, .N, keyby = .(WEIGHTFLG, c2_q07, ecact)]
dat_nrba[, .N, keyby = .(WEIGHTFLG, ecact)]


# Gender
dat_nrba[, gender := factor(GENDER_R, 1:2, c("Male", "Female"))]


# Age group
dat_nrba[, summary(AGE_R)]

if ("agegrp" %in% names(dat_nrba)) dat_nrba[, agegrp := NULL]
dat_nrba[, agegrp := ifelse(
  test = as.integer(ecact) != 2L,
  yes = as.integer((AGE_R - 6) %/% 10),
  no = 6L
)]
dat_nrba[, .N, keyby = .(agegrp)]

dat_nrba[, agegrp := factor(
  x = agegrp,
  levels = 1:6,
  labels = c(paste(
    seq(from = 16, by = 10, length.out = 5),
    seq(to   = 65, by = 10, length.out = 5),
    sep = "-"
  ), "16-65")
)]

dat_nrba[
  WEIGHTFLG == 1L,
  .(min = min(AGE_R), max = max(AGE_R), n = length(unique(AGE_R))),
  keyby = .(ecact, agegrp)
]

count.unique <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  return(length(unique(x)))
}

setorder(dat_nrba, ecact)
dat_nrba[WEIGHTFLG == 1L, NRBAVAR8 := .GRP, by = .(ecact)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(NRBAVAR8, ecact)]

setorder(dat_nrba, ecact, gender)
dat_nrba[WEIGHTFLG == 1L, NRBAVAR9 := .GRP, by = .(ecact, gender)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(NRBAVAR9, ecact, gender)]

setorder(dat_nrba, ecact, agegrp)
dat_nrba[WEIGHTFLG == 1L, NRBAVAR10 := .GRP, by = .(ecact, agegrp)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(NRBAVAR10, ecact, agegrp)]

setorder(dat_nrba, ecact, gender, agegrp)
dat_nrba[WEIGHTFLG == 1L, NRBAVAR11 := .GRP, by = .(ecact, gender, agegrp)]
dat_nrba[WEIGHTFLG == 1L, .N, keyby = .(NRBAVAR11, ecact, gender, agegrp)]

# https://stat.gov.lv/en/search?Search=%22standart%22&DataSource=%22data%22

label_ecact <- "Labour status (3 categories: employed, unemployed, inactive population)"
label_gender <- "Gender (2 categories)"
label_agegrp <- glue::glue(
  "Age groups (6 categories: {paste(levels(dat_nrba$agegrp), collapse = ', ')})"
)

codebook_NRBAVAR8 <- data.table(
  `NRBA variable` = "NRBAVAR8",
  `NRBA variable label` = label_ecact,
  # `Values` = dat_nrba[, fancy.range(NRBAVAR8)],
  `Value label` = "See the file External_Estimates_Codebook_LVA.xlsx"
)

codebook_NRBAVAR9 <- data.table(
  `NRBA variable` = "NRBAVAR9",
  `NRBA variable label` = paste(label_ecact, "X", label_gender),
  `Value label` = "See the file External_Estimates_Codebook_LVA.xlsx"
)

codebook_NRBAVAR10 <- data.table(
  `NRBA variable` = "NRBAVAR10",
  `NRBA variable label` = paste(label_ecact, "X", label_agegrp),
  `Value label` = "See the file External_Estimates_Codebook_LVA.xlsx"
)

codebook_NRBAVAR11 <- data.table(
  `NRBA variable` = "NRBAVAR11",
  `NRBA variable label` = paste(label_ecact, "X", label_gender, "X", label_agegrp),
  `Value label` = "See the file External_Estimates_Codebook_LVA.xlsx"
)

codebook_ALT_RAKEDIM <- data.table(
  `NRBA variable` = "ALT_RAKEDIM",
  `NRBA variable label` = "See the file Alternative_Totals_Codebook_LVA.xlsx"
)

codebook_NRBAVAR8_ext <- dat_nrba[
  !is.na(NRBAVAR8),
  .(NRBAVAR8, `Labour status` = ecact)
] |> setorder(NRBAVAR8) |> unique()

codebook_NRBAVAR9_ext <- dat_nrba[
  !is.na(NRBAVAR9),
  .(NRBAVAR9, `Labour status` = ecact, `Gender` = gender)
] |> setorder(NRBAVAR9) |> unique()

codebook_NRBAVAR10_ext <- dat_nrba[
  !is.na(NRBAVAR10),
  .(NRBAVAR10, `Labour status` = ecact, `Age Group` = agegrp)
] |> setorder(NRBAVAR10) |> unique()

codebook_NRBAVAR11_ext <- dat_nrba[
  !is.na(NRBAVAR11),
  .(NRBAVAR11, `Labour status` = ecact, `Gender` = gender, `Age Group` = agegrp)
] |> setorder(NRBAVAR11) |> unique()

codebook_NRBAVAR8_ext
codebook_NRBAVAR9_ext
codebook_NRBAVAR10_ext
codebook_NRBAVAR11_ext


# 4. COMPARISON OF ESTIMATES FROM ALTERNATIVE WEIGHTING ADJUSTMENTS

# Education
dat_nrba[, .N, keyby = .(WEIGHTFLG, b2_q01lv)]
dat_nrba[, NRBAVAR12 := b2_q01lv]
dat_nrba[, .N, keyby = .(WEIGHTFLG, b2_q01lv, NRBAVAR12)]

codebook_NRBAVAR12 <- data.table(
  `NRBA variable` = "NRBAVAR12",
  `NRBA variable label` = "Highest education level (B2_Q01LV)",
  `Values` = dat_nrba[WEIGHTFLG == 1L, sort(unique(NRBAVAR12))],
  `Value label` = c(
    "Has not attended school or graduated from primary school",
    "Primary school education",
    "Basic vocational education",
    "Primary education",
    "General secondary education",
    "Vocational education after basic education (less than 2 years)",
    "Vocational education after primary education (2 years or more)",
    "General secondary education after vocational training",
    "Vocational secondary education (with the right to study in an institution of higher education)",
    "Vocational education after general secondary education; it is both vocational education (up to 1 year) and vocational education (1.5-2 years)",
    "Level 1 professional higher education diploma (college)",
    "Professional Bachelor's degree",
    "Bachelor's degree",
    "Higher education during the USSR",
    "Professional Master's degree",
    "Master's degree",
    "Doctor; habilitated doctor; science candidate (during USSR)"
  )
)


# Region
dat_nrba[, .N, keyby = .(REGION)]
dat_nrba[, NRBAVAR13 := REGION]
dat_nrba[, .N, keyby = .(REGION, NRBAVAR13)]

codebook_NRBAVAR13 <- data.table(
  `NRBA variable` = "NRBAVAR13",
  `NRBA variable label` = "Region (NUTS-3)",
  `Values` = dat_nrba[WEIGHTFLG == 1, sort(unique(NRBAVAR13))],
  `Value label` = c(
    "Rīga",
    "Pierīga",
    "Vidzeme",
    "Kurzeme",
    "Zemgale",
    "Latgale"
  )
)



# 5. ANALYSIS OF VARIABLES COLLECTED DURING DATA COLLECTION

dat_nrba[!is.na(PERSID), .N, keyby = .(GENDER_R)]
dat_nrba[!is.na(PERSID), .N, keyby = .(AGE_R)]
dat_nrba[!is.na(PERSID), .N, keyby = .(PERSVAR1)]

dat_nrba[, NRBAVAR14 := GENDER_R]
dat_nrba[, NRBAVAR15 := AGE_R]
dat_nrba[, NRBAVAR16 := PERSVAR1]

codebook_NRBAVAR14 <- data.table(
  `NRBA variable` = "NRBAVAR14",
  `NRBA variable label` = "Gender",
  `Values` = dat_nrba[!is.na(PERSID), sort(unique(NRBAVAR14))],
  `Value label` = c(
    "Male",
    "Female"
  )
)

codebook_NRBAVAR15 <- data.table(
  `NRBA variable` = "NRBAVAR15",
  `NRBA variable label` = "Age",
  `Values` = dat_nrba[!is.na(PERSID), sort(unique(NRBAVAR15))],
  `Value label` = dat_nrba[!is.na(PERSID), paste(sort(unique(NRBAVAR15)), "years")]
)

codebook_NRBAVAR16 <- data.table(
  `NRBA variable` = "NRBAVAR16",
  `NRBA variable label` = "Highest education level collected from Screener",
  `Values` = dat_nrba[!is.na(PERSID), sort(unique(NRBAVAR16))],
  `Value label` = c(
    "Primary education or lower",
    "Secondary education",
    "Higher education"
  )
)




# ALT_RAKEDIM
dat_nrba[, ALT_RAKEDIM := NA_integer_]


# Save
names(dat_nrba)
setorderv(dat_nrba, names(dat_nrba)[1:3])
write_xlsx(
  x = dat_nrba[, .SD, .SDcols = patterns(
    "^(CNTRYID|CASEID|PERSID|NRBAVAR[0-9]+|ALT_RAKEDIM)$"
  )],
  file = "results/Extended_NRBA_LVA.xlsx",
  sheetName = "Sheet1",
  colWidths = 15,
  na.strings = "",
  overwrite = TRUE
)

codebook_NRBAVAR <- rbindlist(
  mget(
    c(gtools::mixedsort(
      ls(pattern = "^codebook_NRBAVAR[0-9]+$")
    ), "codebook_ALT_RAKEDIM")
  ),
  fill = TRUE
)

wb_workbook() |>
  wb_add_worksheet(sheet = "Sheet1") |>
  wb_add_data(
    x = codebook_NRBAVAR,
    with_filter = TRUE,
    na.strings = ""
  ) |>
  wb_add_font(
    dims = wb_dims(1, seq_along(codebook_NRBAVAR)),
    bold = TRUE
  ) |>
  wb_add_font(
    dims = wb_dims(x = codebook_NRBAVAR, rows = codebook_NRBAVAR[, .I]),
    size = 10
  ) |>
  wb_add_border(
    dims = wb_dims(x = codebook_NRBAVAR),
    inner_hgrid = "thin",
    inner_hcolor = wb_color("black"),
    inner_vgrid = "thin",
    inner_vcolor = wb_color("black")
  ) |>
  wb_freeze_pane(first_row = TRUE) |>
  wb_set_col_widths(
    cols = seq_along(codebook_NRBAVAR),
    widths = c(15, 140, 15, 40)
  ) |>
  wb_set_row_heights(
    rows = 1 + 1:nrow(codebook_NRBAVAR),
    heights = 13
  ) |>
  wb_save(file = "results/Extended_NRBA_Codebook_LVA.xlsx")

write_xlsx(
  x = list(
    NRBAVAR8  = codebook_NRBAVAR8_ext,
    NRBAVAR9  = codebook_NRBAVAR9_ext,
    NRBAVAR10 = codebook_NRBAVAR10_ext,
    NRBAVAR11 = codebook_NRBAVAR11_ext
  ),
  file = "results/External_Estimates_Codebook_LVA.xlsx",
  colWidths = 20,
  na.strings = "",
  overwrite = TRUE
)
