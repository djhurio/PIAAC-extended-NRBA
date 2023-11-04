# External Estimates
# LFS

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

# Age groups
agegrp <- paste0("Y", c("15-64", paste(
  seq(from = 15, by = 10, length.out = 5),
  seq(to   = 64, by = 10, length.out = 5),
  sep = "-"
)))
agegrp

# https://stat.gov.lv/en/search?Search=%22standart%22&DataSource=%22data%22

# Employed and employment rate by age group, sex
# https://data.stat.gov.lv:443/sq/19273
api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/EMP/NB/NBLA/NBL020c"

if (import_px) {
  api_meta <- pxweb_get(url = api_url)
  saveRDS(object = api_meta, file = "data/NBL020c-meta.rds")
} else {
  api_meta <- readRDS("data/NBL020c-meta.rds")
}

api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
all(agegrp %in% api_meta$variables[[2]]$values)

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
api_meta$variables[[3]]$valueTexts
grep("^NBL020c1?$", api_meta$variables[[3]]$values, value = TRUE)

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

map_chr(.x = api_meta$variables, \(x) x$code)

if (import_px) {
  dat_empl <- pxweb_get_data(
    url = api_url,
    query = pxweb_query(x = list(
      SEX = "*",
      AgeGroup = agegrp,
      ContentsCode = grep(
        pattern = "^NBL020c1?$", x = api_meta$variables[[3]]$values, value = TRUE
      ),
      TIME = "2023Q1")),
    column.name.type = "code",
    variable.value.type = "code"
  )
  saveRDS(object = dat_empl, file = "data/NBL020c-data.rds")
} else {
  dat_empl <- readRDS(file = "data/NBL020c-data.rds")
}

setDT(dat_empl) |>
  setnames(old = 4:5, new = c("extest", "extse")) |>
  setnames(tolower)

dat_empl


# Unemployed persons and unemployment rate by age group and sex
# https://data.stat.gov.lv:443/sq/19274
api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/EMP/NBBA/NBB1/NBB160c"

if (import_px) {
  api_meta <- pxweb_get(url = api_url)
  saveRDS(object = api_meta, file = "data/NBB160c-meta.rds")
} else {
  api_meta <- readRDS(file = "data/NBB160c-meta.rds")
}

api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
# first(agegrp, -1) %in% api_meta$variables[[2]]$values

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
api_meta$variables[[3]]$valueTexts
grep(pattern = "^NBB1601?c$", x = api_meta$variables[[3]]$values, value = TRUE)

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

map_chr(.x = api_meta$variables, \(x) x$code)

if (import_px) {
  dat_unem <- pxweb_get_data(
    url = api_url,
    query = pxweb_query(x = list(
      SEX = "*",
      # AgeGroup = first(agegrp, -1),
      AgeGroup = "Y15-64",
      ContentsCode = grep(
        pattern = "^NBB1601?c$", x = api_meta$variables[[3]]$values, value = TRUE
      ),
      TIME = "2023Q1")),
    column.name.type = "code",
    variable.value.type = "code"
  )
  saveRDS(object = dat_unem, file = "data/NBB160c-data.rds")
} else {
  dat_unem <- readRDS("data/NBB160c-data.rds")
}

setDT(dat_unem) |>
  setnames(old = 4:5, new = c("extest", "extse")) |>
  setnames(tolower)

dat_unem


# Active population, inactive population and activity rate by age group, sex
# https://data.stat.gov.lv:443/sq/19275
api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/EMP/NBB/NBA/NBA050c"

if (import_px) {
  api_meta <- pxweb_get(url = api_url)
  saveRDS(object = api_meta, file = "data/NBA050c-meta.rds")
} else {
  api_meta <- readRDS("data/NBA050c-meta.rds")
}

api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
all(agegrp %in% api_meta$variables[[2]]$values)

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
api_meta$variables[[3]]$valueTexts
grep(pattern = "^NBA050c[12]$", x = api_meta$variables[[3]]$values, value = TRUE)

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

map_chr(.x = api_meta$variables, \(x) x$code)

if (import_px) {
  dat_inac <- pxweb_get_data(
    url = api_url,
    query = pxweb_query(x = list(
      SEX = "*",
      AgeGroup = agegrp,
      ContentsCode = grep(
        pattern = "^NBA050c[12]$", x = api_meta$variables[[3]]$values, value = TRUE
      ),
      TIME = "2023Q1")),
    column.name.type = "code",
    variable.value.type = "code"
  )
  saveRDS(object = dat_inac, file = "data/NBA050c-data.rds")
} else {
  dat_inac <- readRDS("data/NBA050c-data.rds")
}

setDT(dat_inac) |>
  setnames(old = 4:5, new = c("extest", "extse")) |>
  setnames(tolower)

dat_inac


# combine
dat_ext <- rbindlist(list(dat_empl, dat_unem, dat_inac), idcol = "ecact")

dat_ext[, ecact := factor(
  x = ecact,
  levels = 1:3,
  labels = c("Employed", "Unemployed", "Inactive population")
)]

dat_ext[, sex := factor(
  x = sex,
  levels = c("T", "M", "F"),
  labels = c("T", "M", "F")
)]

dat_ext[, agegroup := factor(
  x = agegroup,
  levels = agegrp,
  labels = agegrp
)]


setorder(dat_ext)

dat_ext[, extest := extest * 1e3]
dat_ext[, extse := extse * 1e3]

dat_NRBAVAR8 <- dat_ext[
  sex == "T" & agegroup == "Y15-64",
  .(NRBAVAR8 = .I, extest, extse)
]
dat_NRBAVAR8

dat_NRBAVAR9 <- dat_ext[
  sex != "T" & agegroup == "Y15-64",
  .(NRBAVAR9 = .I, extest, extse)
]
dat_NRBAVAR9

dat_NRBAVAR10 <- dat_ext[
  sex == "T" & (agegroup != "Y15-64" | ecact == "Unemployed"),
  .(NRBAVAR10 = .I, extest, extse)
]
dat_NRBAVAR10

dat_NRBAVAR11 <- dat_ext[
  sex != "T" & (agegroup != "Y15-64" | ecact == "Unemployed"),
  .(NRBAVAR11 = .I, extest, extse)
]
dat_NRBAVAR11

setnames(dat_NRBAVAR8,  toupper)
setnames(dat_NRBAVAR9,  toupper)
setnames(dat_NRBAVAR10, toupper)
setnames(dat_NRBAVAR11, toupper)

# Save
write_xlsx(
  x = list(
    NRBAVAR8  = dat_NRBAVAR8,
    NRBAVAR9  = dat_NRBAVAR9,
    NRBAVAR10 = dat_NRBAVAR10,
    NRBAVAR11 = dat_NRBAVAR11
  ),
  file = "results/External_Estimates_LVA.xlsx",
  colWidths = 20,
  na.strings = "",
  overwrite = TRUE
)
