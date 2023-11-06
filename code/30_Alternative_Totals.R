# Alternative Totals
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__IZG__IZ__IZI/IZT010

# Libs
library(data.table)
library(pxweb)
library(openxlsx2)
library(purrr)

# Reset
rm(list = ls())
gc()

# Options
# import_px <- TRUE
import_px <- FALSE


# Population aged 15 and over by highest educational level attained, sex and
# age groups at the beginning of year 2019 - 2022

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/IZG/IZ/IZI/IZT010"

if (import_px) {
  api_meta <- pxweb_get(url = api_url)
  saveRDS(object = api_meta, file = "data/IZT010-meta.rds")
} else {
  api_meta <- readRDS("data/IZT010-meta.rds")
}

api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

api_meta$variables[[5]]$code
api_meta$variables[[5]]$values

map_chr(.x = api_meta$variables, \(x) x$code)

if (import_px) {
  dat_edu <- pxweb_get_data(
    url = api_url,
    query = pxweb_query(x = list(
      EDUCATION_LEVEL = paste0("ED", 0:8),
      AgeGroup = paste0(
        "Y", seq(from = 15, by = 5, length.out = 10),
        "-", seq(to   = 64, by = 5, length.out = 10)
      ),
      SEX = c("M", "F"),
      ContentsCode = "IZT010",
      TIME = "2022"
    )),
    column.name.type = "code",
    variable.value.type = "code"
  )
  saveRDS(object = dat_edu, file = "data/IZT010-data.rds")
} else {
  dat_edu <- readRDS(file = "data/IZT010-data.rds")
}

setDT(dat_edu) |>
  setnames(old = length(dat_edu), new = c("value")) |>
  setnames(tolower)

dat_edu

dat_edu[, edu_ISCED := car::recode(
  var = as.integer(substr(education_level, 3, 3)),
  recodes = "0:1='0-1'; 2=2; 3=3; 4=4; 5=5; 6=6; 7:8='7-8'"
)]
dat_edu[, .N, keyby = .(education_level, edu_ISCED)]

dat_edu[, gender := factor(
  x = sex,
  levels = c("M", "F"),
  labels = c("Male", "Female")
)]

dat_ALT_RAKEDIM <- dat_edu[
  ,
  .(ALT_RAKEDIM = .GRP, value = sum(value)),
  keyby = .(gender, edu_ISCED)
] |> setcolorder(neworder = "ALT_RAKEDIM")

dat_ALT_RAKEDIM


# CSP totals 2023, 16-65, private dwellings
dat_totals <- openxlsx2::read_xlsx(
  file = "../PIAAC-data-2023/data/CY2_Prelim_MS_Benchmark_WIF_Codebook_LVA_CSP_2023-06-03.xlsx",
  skip_empty_rows = TRUE,
  skip_empty_cols = TRUE
) |> setDT()

dat_totals[, gender := factor(
  x = Gender,
  levels = c("Males", "Females"),
  labels = c("Male", "Female")
)]

dat_totals <- dat_totals[, .(pop = sum(Sum)), keyby = .(gender)]

dat_ALT_RAKEDIM[, sum(value), keyby = .(gender)]
dat_totals

dat_ALT_RAKEDIM <- merge(
  x = dat_ALT_RAKEDIM,
  y = dat_totals
)

dat_ALT_RAKEDIM[, TOTAL := round(value * pop / sum(value)), by = .(gender)]

tmp <- dat_ALT_RAKEDIM[
  ,
  map(.SD, sum),
  .SDcols = c("value", "TOTAL"),
  keyby = .(gender)
] |> merge(y = dat_totals)
if (tmp[, !isTRUE(all.equal(TOTAL, pop))]) stop("TOTAL != pop")

dat_ALT_RAKEDIM[, .(ALT_RAKEDIM, TOTAL)]

# Save
write_xlsx(
  x = list(
    ALT_RAKEDIM = dat_ALT_RAKEDIM[, .(ALT_RAKEDIM, TOTAL)]
  ),
  file = "results/Alternative_Totals_LVA.xlsx",
  colWidths = 20,
  na.strings = "",
  overwrite = TRUE
)
