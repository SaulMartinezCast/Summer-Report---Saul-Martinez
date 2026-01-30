# Survey data -------------------------------------------------------------
# Load libraries
library(haven)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(sf)
library(mapSpain)
library(fixest)
library(modelsummary)
library(tidyr)
library(ggplot2)
library(stringi)
library(data.table)   # ✅ needed for fast rolling join (population at birth)

# Clean environment
rm(list = ls())

# Load datasets -----------------------------------------------------------
ESGE_2013 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2013.csv") %>%
  mutate(survey_year = 2013)

ESGE_2015 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2015.csv") %>%
  mutate(survey_year = 2015)

ESGE_2017 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2017.sav") %>%
  mutate(survey_year = 2017)

ESGE_2023 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2023.sav") %>%
  mutate(survey_year = 2023)

ESGE_2024_w1 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2024.sav") %>%
  mutate(survey_year = 2024, wave_2024 = 1)

ESGE_2024_w2 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2024_2.sav") %>%
  mutate(survey_year = 2024, wave_2024 = 2)

rain_summary <- readr::read_csv(
  "C:/Users/Saúl/Desktop/Semana Santa project/Grid precipitation/province_holy_week_summary.csv",
  locale = readr::locale(encoding = "UTF-8")
)

# ✅ Population files (province x year)
pop_1900_1991_path <- "C:/Users/Saúl/Documents/holy_week_data/poblacion 1900-1991.xlsx"
pop_1996_2022_path <- "C:/Users/Saúl/Documents/holy_week_data/poblacion 1996-2022.xlsx"

# Robust normalizer for province names 
normalize_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%  # remove accents
    stringr::str_replace_all("[^a-z0-9]+", "")      # drop spaces & punctuation
}

# Official provinces from mapSpain 
provinces <- esp_get_prov(moveCAN = TRUE) %>%
  mutate(
    ine.prov.name = case_when(
      ine.prov.name == "Coruña, A"      ~ "A Coruña",
      ine.prov.name == "Rioja, La"      ~ "La Rioja",
      ine.prov.name == "Balears, Illes" ~ "Illes Balears",
      ine.prov.name == "Palmas, Las"    ~ "Las Palmas",
      TRUE ~ ine.prov.name
    )
  )

canon_map <- provinces %>%
  st_drop_geometry() %>%
  distinct(ine_name = ine.prov.name) %>%
  mutate(key = normalize_name(ine_name))

alias_extra <- tibble::tibble(
  alias = c(
    "Alava","Araba","Araba/Alava","Vizcaya","Guipuzcoa",
    "Baleares","Balears Illes",
    "Valencia","Valencia/Valencia",
    "Sta. Cruz de Tenerife","Santa Cruz Tenerife",
    "Alicante","Alacant","Alicante/Alacant",
    "Castellon","Castello","Castellon/Castello","Castellón/Castelló"
  ),
  ine_name = c(
    rep("Araba/Álava", 3), "Bizkaia","Gipuzkoa",
    "Illes Balears","Illes Balears",
    "Valencia/València","Valencia/València",
    "Santa Cruz de Tenerife","Santa Cruz de Tenerife",
    rep("Alicante/Alacant", 3),
    rep("Castellón/Castelló", 4)
  )
) %>%
  mutate(key = normalize_name(alias)) %>%
  select(key, ine_name)

name_map <- bind_rows(
  canon_map %>% select(key, ine_name),
  alias_extra
) %>%
  distinct(key, .keep_all = TRUE)

prov_code_map <- provinces %>%
  st_drop_geometry() %>%
  transmute(
    prov_nac            = as.integer(cpro),
    provincia_official  = ine.prov.name,
    provincia_norm      = normalize_name(ine.prov.name)
  )

# Rain summary: harmonize province names
rain_summary <- rain_summary %>%
  mutate(key = normalize_name(provincia)) %>%
  left_join(name_map, by = "key") %>%
  mutate(
    provincia_official = coalesce(ine_name, provincia),
    provincia_norm     = normalize_name(provincia_official)
  ) %>%
  select(
    provincia = provincia_official,
    provincia_norm,
    year,
    avg_precip,
    dry_days_1,
    dry_days_5,
    dry_days_10
  )

unmatched <- rain_summary %>% filter(is.na(provincia_norm)) %>% distinct(provincia)
if (nrow(unmatched) > 0) {
  message("Unmatched provinces in rain_summary:")
  print(unmatched)
}


# ✅ POPULATION LOADING (FIXED for your INE excel structure)


clean_pop_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(x))))
}

load_pop_1900_1991 <- function(path) {
  raw <- readxl::read_excel(path, sheet = 1, skip = 6, col_names = TRUE)
  names(raw)[1] <- "provincia"
  
  raw %>%
    mutate(provincia = trimws(as.character(provincia))) %>%
    filter(!is.na(provincia), provincia != "") %>%
    pivot_longer(
      cols = -provincia,
      names_to = "year",
      values_to = "population"
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(str_extract(as.character(year), "\\d{4}"))),
      population = clean_pop_num(population),
      provincia_name = str_trim(str_remove(provincia, "^\\d{1,2}\\s+")),
      provincia_norm = normalize_name(provincia_name)
    ) %>%
    filter(!is.na(year)) %>%
    left_join(prov_code_map %>% select(prov_nac, provincia_norm), by = "provincia_norm") %>%
    filter(!is.na(prov_nac), !is.na(population)) %>%
    transmute(
      prov_nac = as.integer(prov_nac),
      year = as.integer(year),
      population = as.numeric(population)
    )
}

load_pop_1996_2022 <- function(path) {
  raw <- readxl::read_excel(path, sheet = 1, skip = 7, col_names = TRUE)
  names(raw)[1] <- "provincia"
  
  raw %>%
    mutate(provincia = trimws(as.character(provincia))) %>%
    filter(!is.na(provincia), provincia != "") %>%
    pivot_longer(
      cols = -provincia,
      names_to = "year",
      values_to = "population"
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(str_extract(as.character(year), "\\d{4}"))),
      population = clean_pop_num(population),
      provincia_name = str_trim(str_remove(provincia, "^\\d{1,2}\\s+")),
      provincia_norm = normalize_name(provincia_name)
    ) %>%
    filter(!is.na(year)) %>%
    left_join(prov_code_map %>% select(prov_nac, provincia_norm), by = "provincia_norm") %>%
    filter(!is.na(prov_nac), !is.na(population)) %>%
    transmute(
      prov_nac = as.integer(prov_nac),
      year = as.integer(year),
      population = as.numeric(population)
    )
}


# Build province-year population panel (for rolling join later)

pop_panel <- bind_rows(
  load_pop_1900_1991(pop_1900_1991_path),
  load_pop_1996_2022(pop_1996_2022_path)
) %>%
  distinct(prov_nac, year, .keep_all = TRUE)

# ✅ Quick sanity checks (run once)
pop_panel %>%
  summarise(
    n = n(),
    n_prov = n_distinct(prov_nac),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

harmonize <- function(df, year, survey_year = year, wave_2024 = NA_integer_) {
  
  df <- df %>% mutate(year = year)
  
  if (year == 2013) {
    
    df <- df %>%
      mutate(
        BIRTH = as.numeric(P2802),
        AGE = survey_year - BIRTH,
        prov_nac = as.numeric(P30A),
        SIZE_TOWN = as.numeric(P16A),
        BORN_SPAIN = if_else(P30 == 1, 1, 0),
        CATHOLIC = case_when(
          P61 == 1 ~ 1,
          P61 %in% c(8, 9) ~ NA_integer_,
          TRUE ~ 0
        ),
        RELIGIOUS_PRACTICE = case_when(
          CATHOLIC == 0              ~ 0,
          is.na(CATHOLIC)            ~ NA_real_,
          P61C == 1                  ~ 1,
          P61C == 2                  ~ 2,
          P61C == 3                  ~ 3,
          P61C == 4                  ~ 4,
          P61C == 5                  ~ 5,
          TRUE                       ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = as.numeric(P32H),
        CATHOLIC_SCHOOL = if_else(P32I == 1, 1, 0),
        CONSERVATIVE_VOTE = case_when(
          P62A %in% c(0, 97, 99, 98) ~ NA_real_,
          P62A %in% c(2, 5, 7, 12) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          P62A %in% c(0, 97, 99, 98) ~ NA_real_,
          P62A %in% c(2) ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P62 %in% c(6, 8, 2, 9) ~ NA_real_,
          P62 %in% c(1) ~ 1,
          TRUE ~ 0
        ),
        FEMALE = case_when(
          P27 == 2 ~ 1,
          P27 == 1 ~ 0,
          TRUE     ~ NA_real_
        ),
        INCOME = case_when(
          P66 >= 1 & P66 <= 11 ~ as.integer(P66),
          TRUE ~ NA_integer_
        ),
        HH_INCOME = case_when(
          P65 >= 1 & P65 <= 11 ~ as.integer(P65),
          TRUE ~ NA_integer_
        ),
        SCHOOL = if_else(P32 == 3, 1, 0),
        EDUCATION = case_when(
          P32 %in% c(1,2) ~ 1,
          P32A01 == 1 ~ 1,
          P32A01 == 2 ~ 2,
          P32A01 %in% c(3,4) ~ 3,
          P32A01 %in% c(5,6) ~ 4,
          P32A01 %in% c(7,8,9) ~ 5,
          P32A01 %in% c(10,11,12) ~ 6,
          P32A01 == 13 ~ 7,
          P32A01 %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_BORN_SPAIN = case_when(P15C == 1 ~ 1, P15C == 2 ~ 0, TRUE ~ NA_integer_),
        MOTHER_BORN_SPAIN = case_when(P14C == 1 ~ 1, P14C == 2 ~ 0, TRUE ~ NA_integer_),
        FATHER_SCHOOL = case_when(P15M == 3 ~ 1, P15M %in% c(0, 1, 2) ~ 0, TRUE ~ NA_integer_),
        FATHER_EDUCATION = case_when(
          P15M != 3 ~ 1,
          P15N02 == 1 ~ 1,
          P15N02 == 2 ~ 2,
          P15N02 %in% c(3,4) ~ 3,
          P15N02 %in% c(5,6) ~ 4,
          P15N02 %in% c(7,8,9) ~ 5,
          P15N02 %in% c(10,11,12) ~ 6,
          P15N02 == 13 ~ 7,
          P15N02 %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(P14M == 3 ~ 1, P14M %in% c(0, 1, 2) ~ 0, TRUE ~ NA_integer_),
        MOTHER_EDUCATION = case_when(
          P14M != 3 ~ 1,
          P14N02 == 1 ~ 1,
          P14N02 == 2 ~ 2,
          P14N02 %in% c(3,4) ~ 3,
          P14N02 %in% c(5,6) ~ 4,
          P14N02 %in% c(7,8,9) ~ 5,
          P14N02 %in% c(10,11,12) ~ 6,
          P14N02 == 13 ~ 7,
          P14N02 %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_EMPLOYMENT = case_when(P15O %in% c(98, 99) ~ NA_integer_, P15O == 1 ~ 1, TRUE ~ 0),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P15Q),
        MOTHER_EMPLOYMENT = case_when(P14O %in% c(98, 99) ~ NA_integer_, P14O == 1 ~ 1, TRUE ~ 0),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P14Q),
        FATHER_CATHOLIC = if_else(P15S == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P14S == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(P15T %in% 1:5 ~ as.numeric(P15T), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(P14T %in% 1:5 ~ as.numeric(P14T), TRUE ~ NA_real_),
        SAME_LOC_BIRTH = if_else(PROV == P30A, 1, 0),
        MOTHER_IDEOLOGY_LR = as.numeric(P14V01),
        FATHER_IDEOLOGY_LR = as.numeric(P15V01),
        COUPLE_IDEOLOGY_LR = if_else(P7801 %in% 1:10, P7801, NA_real_),
        IDEOLOGY_LR = if_else(P60 %in% 1:10, P60, NA_real_),
        SUBJECTIVE_CLASS = if_else(P18 %in% 0:10, P18, NA_real_),
        TRUST_PEOPLE = if_else(P3 %in% 0:10, P3, NA_real_),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = if_else(P19 %in% 0:10, P19, NA_real_),
        PUBLIC_SECTOR_EMP = if_else(P39C %in% c(1, 2), 1, 0),
        LIFE_SATISFACTION = if_else(P1 %in% 0:10, P1, NA_real_),
        CIVIL_RELATION = if_else(P29 %in% 0:10, P29, NA_real_),
        COUPLE_BORN_SPAIN = if_else(P73 == 1, 1, 0),
        HAS_A_COUPLE = if_else(P67 %in% c(1, 2), 1, 0),
        LEFT_RIGHT = if_else(P59 %in% 0:10, P59, NA_real_),
        FATHER_LEFT_RIGHT = if_else(P15U %in% 0:10, P15U, NA_real_),
        MOTHER_LEFT_RIGHT = if_else(P14U %in% 0:10, P14U, NA_real_),
        COUPLE_CATHOLIC = if_else(P79 == 1, 1, 0),
        COUPLE_LEFT_RIGHT = if_else(P77 %in% 0:10, P77, NA_real_)
      )
    
    return(df)
    
  } else if (year == 2015) {
    
    df <- df %>%
      mutate(
        BIRTH = as.numeric(P5402),
        AGE = survey_year - BIRTH,
        prov_nac = as.numeric(P56A),
        SIZE_TOWN = as.numeric(P44A),
        BORN_SPAIN = if_else(P56 == 1, 1, 0),
        CATHOLIC = case_when(
          P79 == 1 ~ 1,
          P79 %in% c(8, 9) ~ NA_integer_,
          TRUE ~ 0
        ),
        RELIGIOUS_PRACTICE = case_when(
          CATHOLIC == 0              ~ 0,
          is.na(CATHOLIC)            ~ NA_real_,
          P79B %in% 1:5 ~ as.numeric(P79B),
          TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(97, 95, 94, 98, 99, 0) ~ NA_real_,
          RECUERDO %in% c(1, 9, 11) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          RECUERDO %in% c(97, 95, 94, 98, 99, 0) ~ NA_real_,
          RECUERDO %in% c(1) ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P80 %in% c(9, 8, 6, 2) ~ NA_real_,
          P80 == 1 ~ 1,
          TRUE ~ 0
        ),
        FEMALE = case_when(
          P53 == 2 ~ 1,
          P53 == 1 ~ 0,
          TRUE     ~ NA_real_
        ),
        INCOME = case_when(P84 >= 1 & P84 <= 11 ~ as.integer(P84), TRUE ~ NA_integer_),
        HH_INCOME = case_when(P83 >= 1 & P83 <= 11 ~ as.integer(P83), TRUE ~ NA_integer_),
        SCHOOL = if_else(P58 == 3, 1, 0),
        EDUCATION = case_when(
          P58 %in% c(1,2) ~ 1,
          P58A == 1 ~ 1,
          P58A == 2 ~ 2,
          P58A %in% c(3,4) ~ 3,
          P58A %in% c(5,6) ~ 4,
          P58A %in% c(7,8,9) ~ 5,
          P58A %in% c(10,11,12) ~ 6,
          P58A == 13 ~ 7,
          P58A %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_BORN_SPAIN = case_when(P43C == 1 ~ 1, P43C == 2 ~ 0, TRUE ~ NA_integer_),
        MOTHER_BORN_SPAIN = case_when(P42C == 1 ~ 1, P42C == 2 ~ 0, TRUE ~ NA_integer_),
        FATHER_SCHOOL = case_when(P43J == 3 ~ 1, P43J %in% c(0,1,2) ~ 0, TRUE ~ NA_integer_),
        FATHER_EDUCATION = case_when(
          P43J != 3 ~ 1,
          P43K == 1 ~ 1,
          P43K == 2 ~ 2,
          P43K %in% c(3,4) ~ 3,
          P43K %in% c(5,6) ~ 4,
          P43K %in% c(7,8,9) ~ 5,
          P43K %in% c(10,11,12) ~ 6,
          P43K == 13 ~ 7,
          P43K %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(P42J == 3 ~ 1, P42J %in% c(0,1,2) ~ 0, TRUE ~ NA_integer_),
        MOTHER_EDUCATION = case_when(
          P42J != 3 ~ 1,
          P42K == 1 ~ 1,
          P42K == 2 ~ 2,
          P42K %in% c(3,4) ~ 3,
          P42K %in% c(5,6) ~ 4,
          P42K %in% c(7,8,9) ~ 5,
          P42K %in% c(10,11,12) ~ 6,
          P42K == 13 ~ 7,
          P42K %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_EMPLOYMENT = case_when(P43L %in% c(98, 99) ~ NA_integer_, P43L == 1 ~ 1, TRUE ~ 0),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P43N),
        MOTHER_EMPLOYMENT = case_when(P42L %in% c(98, 99) ~ NA_integer_, P42L == 1 ~ 1, TRUE ~ 0),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P42N),
        FATHER_CATHOLIC = if_else(P43P == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P42P == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(P43R %in% 1:5 ~ as.numeric(P43R), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(P42R %in% 1:5 ~ as.numeric(P42R), TRUE ~ NA_real_),
        SAME_LOC_BIRTH = if_else(PROV == P56A, 1, 0),
        MOTHER_IDEOLOGY_LR = as.numeric(P43T),
        FATHER_IDEOLOGY_LR = as.numeric(P42T),
        COUPLE_IDEOLOGY_LR = if_else(P95 %in% 1:10, P95, NA_real_),
        IDEOLOGY_LR = if_else(P78 %in% 1:10, P78, NA_real_),
        SUBJECTIVE_CLASS = if_else(P45 %in% 0:10, P45, NA_real_),
        TRUST_PEOPLE = if_else(P3 %in% 0:10, P3, NA_real_),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = if_else(P46 %in% 0:10, P46, NA_real_),
        PUBLIC_SECTOR_EMP = if_else(P63C %in% c(1, 2), 1, 0),
        LIFE_SATISFACTION = if_else(P1 %in% 0:10, P1, NA_real_),
        CIVIL_RELATION = if_else(P55 %in% 0:10, P55, NA_real_),
        COUPLE_BORN_SPAIN = if_else(P73 == 1, 1, 0),
        HAS_A_COUPLE = if_else(P86 %in% c(1, 2), 1, 0),
        LEFT_RIGHT = if_else(P77 %in% 0:10, P77, NA_real_),
        FATHER_LEFT_RIGHT = if_else(P43S %in% 0:10, P43S, NA_real_),
        MOTHER_LEFT_RIGHT = if_else(P42S %in% 0:10, P42S, NA_real_),
        COUPLE_CATHOLIC = if_else(P96 == 1, 1, 0),
        COUPLE_LEFT_RIGHT = if_else(P94 %in% 0:10, P94, NA_real_)
      )
    
    return(df)
    
  } else if (year == 2017) {
    
    df <- df %>%
      mutate(
        BIRTH = as.numeric(FNACIMANYO2),
        AGE = survey_year - as.numeric(FNACIMANYO2),
        prov_nac = as.numeric(P38A),
        SIZE_TOWN = as.numeric(P28A),
        BORN_SPAIN = if_else(P38 == 1, 1, 0),
        CATHOLIC = case_when(
          P63 == 1 ~ 1,
          P63 %in% c(8, 9) ~ NA_integer_,
          TRUE ~ 0
        ),
        RELIGIOUS_PRACTICE = case_when(
          CATHOLIC == 0              ~ 0,
          is.na(CATHOLIC)            ~ NA_real_,
          P63B %in% 1:5 ~ as.numeric(P63B),
          TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(93, 94, 97, 98, 99, 0) ~ NA_real_,
          RECUERDO %in% c(1, 8, 10) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          RECUERDO %in% c(93, 94, 97, 98, 99, 0) ~ NA_real_,
          RECUERDO == 1 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P64 %in% c(9, 2, 6, 7) ~ NA_real_,
          P64 == 1 ~ 1,
          TRUE ~ 0
        ),
        FEMALE = case_when(
          P0 == 2 ~ 1,
          P0 == 1 ~ 0,
          TRUE    ~ NA_real_
        ),
        INCOME = case_when(P68 >= 1 & P68 <= 11 ~ as.integer(P68), TRUE ~ NA_integer_),
        HH_INCOME = case_when(P67 >= 1 & P67 <= 11 ~ as.integer(P67), TRUE ~ NA_integer_),
        SCHOOL = if_else(P40 == 3, 1, 0),
        EDUCATION = case_when(
          P40 %in% c(1,2) ~ 1,
          P401 == 1 ~ 1,
          P401 == 2 ~ 2,
          P401 %in% c(3,4) ~ 3,
          P401 %in% c(5,6) ~ 4,
          P401 %in% c(7,8,9) ~ 5,
          P401 %in% c(10,11,12) ~ 6,
          P401 == 13 ~ 7,
          P401 %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_BORN_SPAIN = case_when(P27C == 1 ~ 1, P27C == 2 ~ 0, TRUE ~ NA_integer_),
        MOTHER_BORN_SPAIN = case_when(P26C == 1 ~ 1, P26C == 2 ~ 0, TRUE ~ NA_integer_),
        FATHER_SCHOOL = case_when(P27J == 3 ~ 1, P27J %in% c(0,1,2) ~ 0, TRUE ~ NA_integer_),
        FATHER_EDUCATION = case_when(
          P27J != 3 ~ 1,
          P27K == 1 ~ 1,
          P27K == 2 ~ 2,
          P27K %in% c(3,4) ~ 3,
          P27K %in% c(5,6) ~ 4,
          P27K %in% c(7,8,9) ~ 5,
          P27K %in% c(10,11,12) ~ 6,
          P27K == 13 ~ 7,
          P27K %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(P26J == 3 ~ 1, P26J %in% c(0,1,2) ~ 0, TRUE ~ NA_integer_),
        MOTHER_EDUCATION = case_when(
          P26J != 3 ~ 1,
          P26K == 1 ~ 1,
          P26K == 2 ~ 2,
          P26K %in% c(3,4) ~ 3,
          P26K %in% c(5,6) ~ 4,
          P26K %in% c(7,8,9) ~ 5,
          P26K %in% c(10,11,12) ~ 6,
          P26K == 13 ~ 7,
          P26K %in% c(14,15) ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_EMPLOYMENT = case_when(P27L %in% c(98, 99) ~ NA_integer_, P27L == 1 ~ 1, TRUE ~ 0L),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P27N),
        MOTHER_EMPLOYMENT = case_when(P26L %in% c(98, 99) ~ NA_integer_, P26L == 1 ~ 1, TRUE ~ 0L),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P26N),
        FATHER_CATHOLIC = if_else(P27P == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P26P == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(P27R %in% 1:5 ~ as.numeric(P27R), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(P26R %in% 1:5 ~ as.numeric(P26R), TRUE ~ NA_real_),
        SAME_LOC_BIRTH = if_else(PROV == P38A, 1, 0),
        MOTHER_IDEOLOGY_LR = as.numeric(P26T),
        FATHER_IDEOLOGY_LR = as.numeric(P27T),
        COUPLE_IDEOLOGY_LR = if_else(P80_1 %in% 1:10, P80_1, NA_real_),
        IDEOLOGY_LR = if_else(P62_1 %in% 1:10, P62_1, NA_real_),
        SUBJECTIVE_CLASS = if_else(P29 %in% 0:10, P29, NA_real_),
        TRUST_PEOPLE = if_else(P3 %in% 0:10, P3, NA_real_),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = if_else(P30 %in% 0:10, P30, NA_real_),
        PUBLIC_SECTOR_EMP = if_else(P82D %in% c(1, 2), 1, 0),
        LIFE_SATISFACTION = if_else(P1 %in% 0:10, P1, NA_real_),
        CIVIL_RELATION = if_else(P37 %in% 0:10, P37, NA_real_),
        COUPLE_BORN_SPAIN = if_else(P76 == 1, 1, 0),
        HAS_A_COUPLE = if_else(P70 %in% c(1, 2), 1, 0),
        LEFT_RIGHT = if_else(P61 %in% 0:10, P61, NA_real_),
        FATHER_LEFT_RIGHT = if_else(P27S %in% 0:10, P27S, NA_real_),
        MOTHER_LEFT_RIGHT = if_else(P26S %in% 0:10, P26S, NA_real_),
        COUPLE_CATHOLIC = if_else(P81 == 1, 1, 0),
        COUPLE_LEFT_RIGHT = if_else(P79 %in% 0:10, P79, NA_real_)
      )
    
    return(df)
    
  } else if (year == 2023) {
    
    df <- df %>%
      mutate(
        BIRTH = as.numeric(BIRTH),
        AGE = survey_year - BIRTH,
        prov_nac = as.numeric(PROV_NAC),
        SIZE_TOWN = NA_real_,
        BORN_SPAIN = if_else(LUGAR_NAC == 1, 1, 0),
        CATHOLIC = case_when(
          NAT_RELIG == 1 ~ 1,
          NAT_RELIG %in% c(97, 99) ~ NA_integer_,
          TRUE ~ 0
        ),
        RELIGIOUS_PRACTICE = case_when(
          CATHOLIC == 0              ~ 0,
          is.na(CATHOLIC)            ~ NA_real_,
          ATTEND == 1 ~ 5,
          ATTEND == 2 ~ 4,
          ATTEND %in% c(3,4) ~ 3,
          ATTEND %in% c(5,6) ~ 2,
          ATTEND %in% c(7,8) ~ 1,
          TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = TIPO_COLEGIO,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(16, 17, 99, 98, 0) ~ NA_real_,
          RECUERDO %in% c(2, 6, 9, 10, 12) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          RECUERDO %in% c(16, 17, 99, 98, 0) ~ NA_real_,
          RECUERDO == 2 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = case_when(
          RECUERDO %in% c(16, 17, 98, 99, 0) ~ NA_real_,
          RECUERDO == 6 ~ 1,
          TRUE ~ 0
        ),
        PARTICIPATION = case_when(
          VOTE_LE %in% c(4, 5, 9) ~ NA_real_,
          VOTE_LE == 1 ~ 1,
          TRUE ~ 0
        ),
        FEMALE = case_when(
          SEX == 2 ~ 1,
          SEX == 1 ~ 0,
          TRUE     ~ NA_real_
        ),
        INCOME = NA_real_,
        HH_INCOME = NA_real_,
        SCHOOL = NA_real_,
        EDUCATION = case_when(
          NAT_DEGR %in% c(1, 2)       ~ 1,
          NAT_DEGR == 3               ~ 2,
          NAT_DEGR %in% 4:6           ~ 3,
          NAT_DEGR %in% 7:10          ~ 4,
          NAT_DEGR %in% 11:15         ~ 5,
          NAT_DEGR == 16              ~ 6,
          NAT_DEGR == 17              ~ 7,
          NAT_DEGR == 18              ~ 8,
          TRUE                        ~ NA_real_
        ),
        FATHER_BORN_SPAIN = case_when(F_BORN == 1 ~ 1, F_BORN == 2 ~ 0, TRUE ~ NA_integer_),
        MOTHER_BORN_SPAIN = case_when(M_BORN == 1 ~ 1, M_BORN == 2 ~ 0, TRUE ~ NA_integer_),
        FATHER_SCHOOL = if_else(FATH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
        FATHER_EDUCATION = case_when(
          FATH_NAT_DEGR %in% c(1, 4) ~ 1,
          FATH_NAT_DEGR == 2 ~ 2,
          FATH_NAT_DEGR %in% c(5, 6) ~ 3,
          FATH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
          FATH_NAT_DEGR == 10 ~ 5,
          FATH_NAT_DEGR == 11 ~ 6,
          FATH_NAT_DEGR == 12 ~ 7,
          FATH_NAT_DEGR == 13 ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = if_else(MOTH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
        MOTHER_EDUCATION = case_when(
          MOTH_NAT_DEGR %in% c(1, 4) ~ 1,
          MOTH_NAT_DEGR == 2 ~ 2,
          MOTH_NAT_DEGR %in% c(5, 6) ~ 3,
          MOTH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
          MOTH_NAT_DEGR == 10 ~ 5,
          MOTH_NAT_DEGR == 11 ~ 6,
          MOTH_NAT_DEGR == 12 ~ 7,
          MOTH_NAT_DEGR == 13 ~ 8,
          TRUE ~ NA_real_
        ),
        FATHER_EMPLOYMENT = if_else(FATH_WORK %in% c(1, 2), 1, 0),
        FATHER_EMPLOYMENT_TYPE = as.numeric(FATH_WORK),
        MOTHER_EMPLOYMENT = if_else(MOTH_WORK %in% c(1, 2), 1, 0),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(MOTH_WORK),
        FATHER_CATHOLIC = NA_real_,
        MOTHER_CATHOLIC = NA_real_,
        FATHER_RELIGIOUS_PRACTICE = NA_real_,
        MOTHER_RELIGIOUS_PRACTICE = NA_real_,
        SAME_LOC_BIRTH = if_else(as.numeric(PROV) == as.numeric(PROV_NAC), 1, 0),
        MOTHER_IDEOLOGY_LR = NA_real_,
        FATHER_IDEOLOGY_LR = NA_real_,
        COUPLE_IDEOLOGY_LR = NA_real_,
        IDEOLOGY_LR = if_else(IDEOL_CATEG_01 %in% 1:10, IDEOL_CATEG_01, NA_real_),
        SUBJECTIVE_CLASS = NA_real_,
        TRUST_PEOPLE = case_when(
          V10 == 8 ~ NA_real_,
          V10 %in% 1:5 ~ V10 * 2,
          TRUE ~ NA_real_
        ),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = NA_real_,
        PUBLIC_SECTOR_EMP = if_else(TYPORG1 %in% c(1, 2), 1, 0),
        LIFE_SATISFACTION = if_else(C_SATISFVIDA %in% 0:10, C_SATISFVIDA, NA_real_),
        CIVIL_RELATION = if_else(MARITAL %in% 0:10, MARITAL, NA_real_),
        COUPLE_BORN_SPAIN = NA_real_,
        HAS_A_COUPLE = if_else(PARTLIV %in% c(1, 2), 1, 0),
        LEFT_RIGHT = if_else(LEFT_RIGHT %in% 0:10, LEFT_RIGHT, NA_real_),
        FATHER_LEFT_RIGHT = NA_real_,
        MOTHER_LEFT_RIGHT = NA_real_,
        COUPLE_CATHOLIC = NA_real_,
        COUPLE_LEFT_RIGHT = NA_real_
      )
    
    return(df)
    
  } else if (year == 2024) {
    
    
    if (is.na(wave_2024)) {
      stop("For year == 2024 you must pass wave_2024 = 1 or 2 (or have it as a column and pass it in).")
    }
    
    if (wave_2024 == 1) {
      
      
      # 2024 WAVE 1 (your ESGE_2024.sav)
      
      df <- df %>%
        mutate(
          BIRTH = as.numeric(BIRTH),
          AGE = survey_year - BIRTH,
          prov_nac = as.numeric(PROV),
          SIZE_TOWN = NA_real_,
          
          BORN_SPAIN = if_else(LUGAR_NAC == 1, 1, 0),
          
          CATHOLIC = case_when(
            NAT_RELIG == 1 ~ 1,
            NAT_RELIG %in% c(97, 99) ~ NA_integer_,
            TRUE ~ 0
          ),
          
          RELIGIOUS_PRACTICE = case_when(
            CATHOLIC == 0              ~ 0,
            is.na(CATHOLIC)            ~ NA_real_,
            ATTEND == 1 ~ 5,
            ATTEND == 2 ~ 4,
            ATTEND %in% c(3,4) ~ 3,
            ATTEND %in% c(5,6) ~ 2,
            ATTEND %in% c(7,8) ~ 1,
            TRUE ~ NA_real_
          ),
          
          PRIMARY_SCHOOL_TYPE = TIPO_COLEGIO,
          CATHOLIC_SCHOOL     = TIPO_COLEGIO_2,
          
          CONSERVATIVE_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO %in% c(1, 3, 7, 9, 12) ~ 1,
            TRUE ~ 0
          ),
          PP_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO == 1 ~ 1,
            TRUE ~ 0
          ),
          FAR_RIGHT_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO == 3 ~ 1,
            TRUE ~ 0
          ),
          PARTICIPATION = case_when(
            VOTE_LE %in% c(4, 5, 9) ~ NA_real_,
            VOTE_LE == 1 ~ 1,
            TRUE ~ 0
          ),
          
          FEMALE = case_when(
            SEXO == 2 ~ 1,
            SEXO == 1 ~ 0,
            TRUE ~ NA_real_
          ),
          
          INCOME = NA_real_,
          HH_INCOME = NA_real_,
          SCHOOL = NA_real_,
          
          EDUCATION = case_when(
            NAT_DEGR %in% c(1, 2)       ~ 1,
            NAT_DEGR == 3               ~ 2,
            NAT_DEGR %in% 4:6           ~ 3,
            NAT_DEGR %in% 7:10          ~ 4,
            NAT_DEGR %in% 11:15         ~ 5,
            NAT_DEGR == 16              ~ 6,
            NAT_DEGR == 17              ~ 7,
            NAT_DEGR == 18              ~ 8,
            TRUE                        ~ NA_real_
          ),
          
          FATHER_BORN_SPAIN = case_when(F_BORN == 1 ~ 1, F_BORN == 2 ~ 0, TRUE ~ NA_integer_),
          MOTHER_BORN_SPAIN = case_when(M_BORN == 1 ~ 1, M_BORN == 2 ~ 0, TRUE ~ NA_integer_),
          
          FATHER_SCHOOL = if_else(FATH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
          FATHER_EDUCATION = case_when(
            FATH_NAT_DEGR %in% c(1, 4) ~ 1,
            FATH_NAT_DEGR == 2 ~ 2,
            FATH_NAT_DEGR %in% c(5, 6) ~ 3,
            FATH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
            FATH_NAT_DEGR == 10 ~ 5,
            FATH_NAT_DEGR == 11 ~ 6,
            FATH_NAT_DEGR == 12 ~ 7,
            FATH_NAT_DEGR == 13 ~ 8,
            TRUE ~ NA_real_
          ),
          MOTHER_SCHOOL = if_else(MOTH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
          MOTHER_EDUCATION = case_when(
            MOTH_NAT_DEGR %in% c(1, 4) ~ 1,
            MOTH_NAT_DEGR == 2 ~ 2,
            MOTH_NAT_DEGR %in% c(5, 6) ~ 3,
            MOTH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
            MOTH_NAT_DEGR == 10 ~ 5,
            MOTH_NAT_DEGR == 11 ~ 6,
            MOTH_NAT_DEGR == 12 ~ 7,
            MOTH_NAT_DEGR == 13 ~ 8,
            TRUE ~ NA_real_
          ),
          
          FATHER_EMPLOYMENT = if_else(FATH_WORK %in% c(1, 2), 1, 0),
          FATHER_EMPLOYMENT_TYPE = as.numeric(FATH_WORK),
          MOTHER_EMPLOYMENT = if_else(MOTH_WORK %in% c(1, 2), 1, 0),
          MOTHER_EMPLOYMENT_TYPE = as.numeric(MOTH_WORK),
          
          FATHER_CATHOLIC = NA_real_,
          MOTHER_CATHOLIC = NA_real_,
          FATHER_RELIGIOUS_PRACTICE = NA_real_,
          MOTHER_RELIGIOUS_PRACTICE = NA_real_,
          
          SAME_LOC_BIRTH = if_else(as.numeric(PROV) == as.numeric(PROV_NAC), 1, 0),
          
          MOTHER_IDEOLOGY_LR = NA_real_,
          FATHER_IDEOLOGY_LR = NA_real_,
          COUPLE_IDEOLOGY_LR = NA_real_,
          IDEOLOGY_LR = NA_real_,
          SUBJECTIVE_CLASS = NA_real_,
          
          TRUST_PEOPLE =  NA_real_,
          
          INST_CONFIDENCE = NA_real_,
          MERITOCRACY_BELIEF = if_else(MERIT %in% 0:10, MERIT, NA_real_),
          
          PUBLIC_SECTOR_EMP = if_else(TYPORG1 %in% c(1, 2), 1, 0),
          
          LIFE_SATISFACTION = if_else(C_SATISFVIDA_24 %in% 0:10, C_SATISFVIDA_24, NA_real_),
          CIVIL_RELATION = if_else(MARITAL %in% 0:10, MARITAL, NA_real_),
          
          COUPLE_BORN_SPAIN = NA_real_,
          HAS_A_COUPLE = if_else(PARTLIV %in% c(1, 2), 1, 0),
          LEFT_RIGHT = if_else(LEFT_RIGHT %in% 0:10, LEFT_RIGHT, NA_real_),
          FATHER_LEFT_RIGHT = NA_real_,
          MOTHER_LEFT_RIGHT = NA_real_,
          COUPLE_CATHOLIC = NA_real_,
          COUPLE_LEFT_RIGHT = NA_real_
        )
      
      return(df)
      
    } else if (wave_2024 == 2) {
      
      
      # 2024 WAVE 2 (your ESGE_2024_2.sav)
      # (Matches your current 2024 code: SEXO, NACIONALIDAD, C_SATISFVIDA_24, etc.)
      
      df <- df %>%
        mutate(
          BIRTH = as.numeric(BIRTH),
          AGE = survey_year - BIRTH,
          prov_nac = as.numeric(PROV_NAC),
          SIZE_TOWN = NA_real_,
          
          BORN_SPAIN = if_else(NACIONALIDAD == 1, 1, 0),
          
          CATHOLIC = case_when(
            NAT_RELIG == 1 ~ 1,
            NAT_RELIG %in% c(97, 99) ~ NA_integer_,
            TRUE ~ 0
          ),
          
          RELIGIOUS_PRACTICE = case_when(
            CATHOLIC == 0              ~ 0,
            is.na(CATHOLIC)            ~ NA_real_,
            ATTEND == 1 ~ 5,
            ATTEND == 2 ~ 4,
            ATTEND %in% c(3,4) ~ 3,
            ATTEND %in% c(5,6) ~ 2,
            ATTEND %in% c(7,8) ~ 1,
            TRUE ~ NA_real_
          ),
          
          PRIMARY_SCHOOL_TYPE = TIPO_COLEGIO,
          CATHOLIC_SCHOOL     = TIPO_COLEGIO_2,
          
          CONSERVATIVE_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO %in% c(1, 3, 7, 9, 12) ~ 1,
            TRUE ~ 0
          ),
          PP_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO == 1 ~ 1,
            TRUE ~ 0
          ),
          FAR_RIGHT_VOTE = case_when(
            RECUERDO %in% c(95, 94, 90, 98, 99, 0, 97) ~ NA_real_,
            RECUERDO == 3 ~ 1,
            TRUE ~ 0
          ),
          PARTICIPATION = case_when(
            VOTE_LE %in% c(4, 5, 9) ~ NA_real_,
            VOTE_LE == 1 ~ 1,
            TRUE ~ 0
          ),
          
          FEMALE = case_when(
            SEXO == 2 ~ 1,
            SEXO == 1 ~ 0,
            TRUE ~ NA_real_
          ),
          
          INCOME = NA_real_,
          HH_INCOME = NA_real_,
          SCHOOL = NA_real_,
          
          EDUCATION = case_when(
            NAT_DEGR %in% c(1, 2)       ~ 1,
            NAT_DEGR == 3               ~ 2,
            NAT_DEGR %in% 4:6           ~ 3,
            NAT_DEGR %in% 7:10          ~ 4,
            NAT_DEGR %in% 11:15         ~ 5,
            NAT_DEGR == 16              ~ 6,
            NAT_DEGR == 17              ~ 7,
            NAT_DEGR == 18              ~ 8,
            TRUE                        ~ NA_real_
          ),
          
          FATHER_BORN_SPAIN = case_when(F_BORN == 1 ~ 1, F_BORN == 2 ~ 0, TRUE ~ NA_integer_),
          MOTHER_BORN_SPAIN = case_when(M_BORN == 1 ~ 1, M_BORN == 2 ~ 0, TRUE ~ NA_integer_),
          
          FATHER_SCHOOL = if_else(FATH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
          FATHER_EDUCATION = case_when(
            FATH_NAT_DEGR %in% c(1, 4) ~ 1,
            FATH_NAT_DEGR == 2 ~ 2,
            FATH_NAT_DEGR %in% c(5, 6) ~ 3,
            FATH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
            FATH_NAT_DEGR == 10 ~ 5,
            FATH_NAT_DEGR == 11 ~ 6,
            FATH_NAT_DEGR == 12 ~ 7,
            FATH_NAT_DEGR == 13 ~ 8,
            TRUE ~ NA_real_
          ),
          MOTHER_SCHOOL = if_else(MOTH_NAT_DEGR == 1, 0, 1, missing = NA_integer_),
          MOTHER_EDUCATION = case_when(
            MOTH_NAT_DEGR %in% c(1, 4) ~ 1,
            MOTH_NAT_DEGR == 2 ~ 2,
            MOTH_NAT_DEGR %in% c(5, 6) ~ 3,
            MOTH_NAT_DEGR %in% c(7, 8, 9) ~ 4,
            MOTH_NAT_DEGR == 10 ~ 5,
            MOTH_NAT_DEGR == 11 ~ 6,
            MOTH_NAT_DEGR == 12 ~ 7,
            MOTH_NAT_DEGR == 13 ~ 8,
            TRUE ~ NA_real_
          ),
          
          FATHER_EMPLOYMENT = if_else(FATH_WORK %in% c(1, 2), 1, 0),
          FATHER_EMPLOYMENT_TYPE = as.numeric(FATH_WORK),
          MOTHER_EMPLOYMENT = if_else(MOTH_WORK %in% c(1, 2), 1, 0),
          MOTHER_EMPLOYMENT_TYPE = as.numeric(MOTH_WORK),
          
          FATHER_CATHOLIC = NA_real_,
          MOTHER_CATHOLIC = NA_real_,
          FATHER_RELIGIOUS_PRACTICE = NA_real_,
          MOTHER_RELIGIOUS_PRACTICE = NA_real_,
          
          SAME_LOC_BIRTH = if_else(as.numeric(PROV) == as.numeric(PROV_NAC), 1, 0),
          
          MOTHER_IDEOLOGY_LR = NA_real_,
          FATHER_IDEOLOGY_LR = NA_real_,
          COUPLE_IDEOLOGY_LR = NA_real_,
          IDEOLOGY_LR = NA_real_,
          SUBJECTIVE_CLASS = NA_real_,
          
          TRUST_PEOPLE =  NA_real_,
          
          INST_CONFIDENCE = NA_real_,
          MERITOCRACY_BELIEF = if_else(MERIT %in% 0:10, MERIT, NA_real_),
          
          PUBLIC_SECTOR_EMP = if_else(TYPORG %in% c(1, 2), 1, 0),
          
          LIFE_SATISFACTION = if_else(C_SATISFVIDA %in% 0:10, C_SATISFVIDA, NA_real_),
          CIVIL_RELATION = if_else(MARITAL %in% 0:10, MARITAL, NA_real_),
          
          COUPLE_BORN_SPAIN = NA_real_,
          HAS_A_COUPLE = if_else(PARTLIV %in% c(1, 2), 1, 0),
          LEFT_RIGHT = if_else(LEFT_RIGHT %in% 0:10, LEFT_RIGHT, NA_real_),
          FATHER_LEFT_RIGHT = NA_real_,
          MOTHER_LEFT_RIGHT = NA_real_,
          COUPLE_CATHOLIC = NA_real_,
          COUPLE_LEFT_RIGHT = NA_real_
        )
      
      return(df)
      
    } else {
      stop("wave_2024 must be 1 or 2 for year == 2024.")
    }
  }
  
  stop("Year not supported in harmonize().")
}

# Harmonize surveys 
ESGE_2013_h <- harmonize(ESGE_2013, 2013)
ESGE_2015_h <- harmonize(ESGE_2015, 2015)
ESGE_2017_h <- harmonize(ESGE_2017, 2017)
ESGE_2023_h <- harmonize(ESGE_2023, 2023)
ESGE_2024_w1_h <- harmonize(ESGE_2024_w1, 2024, wave_2024 = 1)
ESGE_2024_w2_h <- harmonize(ESGE_2024_w2, 2024, wave_2024 = 2)

# Add PROV and MUN columns (ensure numeric) 
ESGE_2013_h    <- ESGE_2013_h    %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))
ESGE_2015_h    <- ESGE_2015_h    %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))
ESGE_2017_h    <- ESGE_2017_h    %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))
ESGE_2023_h    <- ESGE_2023_h    %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))
ESGE_2024_w1_h <- ESGE_2024_w1_h %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))
ESGE_2024_w2_h <- ESGE_2024_w2_h %>% mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

# Stack the two 2024 waves 
ESGE_2024_h <- bind_rows(
  ESGE_2024_w1_h %>% mutate(wave_2024 = 1),
  ESGE_2024_w2_h %>% mutate(wave_2024 = 2)
)

# Combine all surveys (keep a harmonized set of vars)
harmonized_vars <- c(
  "survey_year","year","BIRTH","AGE","prov_nac","SIZE_TOWN","BORN_SPAIN","CATHOLIC",
  "RELIGIOUS_PRACTICE","PRIMARY_SCHOOL_TYPE","CATHOLIC_SCHOOL","CONSERVATIVE_VOTE",
  "FAR_RIGHT_VOTE","PARTICIPATION","FEMALE","INCOME","SCHOOL","EDUCATION",
  "FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN","FATHER_SCHOOL","FATHER_EDUCATION",
  "MOTHER_SCHOOL","MOTHER_EDUCATION","FATHER_EMPLOYMENT","FATHER_EMPLOYMENT_TYPE",
  "MOTHER_EMPLOYMENT","MOTHER_EMPLOYMENT_TYPE","FATHER_CATHOLIC","MOTHER_CATHOLIC",
  "FATHER_RELIGIOUS_PRACTICE","MOTHER_RELIGIOUS_PRACTICE","SAME_LOC_BIRTH",
  "MOTHER_IDEOLOGY_LR","FATHER_IDEOLOGY_LR","COUPLE_IDEOLOGY_LR","IDEOLOGY_LR",
  "SUBJECTIVE_CLASS","TRUST_PEOPLE","INST_CONFIDENCE","MERITOCRACY_BELIEF",
  "PUBLIC_SECTOR_EMP","LIFE_SATISFACTION","CIVIL_RELATION","COUPLE_BORN_SPAIN",
  "HAS_A_COUPLE","LEFT_RIGHT","FATHER_LEFT_RIGHT","MOTHER_LEFT_RIGHT",
  "COUPLE_CATHOLIC","COUPLE_LEFT_RIGHT","PROV","MUN","PP_VOTE"
)

survey <- dplyr::bind_rows(
  dplyr::select(ESGE_2013_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2015_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2017_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2023_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2024_h, dplyr::any_of(harmonized_vars))
) %>%
  dplyr::mutate(respondent_id = dplyr::row_number())

# Clean & prepare; attach province + population-at-birth control 
survey_clean <- survey %>%
  mutate(
    childhood_start = BIRTH + 5,
    childhood_end   = BIRTH + 18
  ) %>%
  left_join(prov_code_map, by = "prov_nac") %>%
  filter(!is.na(provincia_norm), !is.na(BIRTH), BIRTH > 1900)

library(data.table)

# pop_panel must have: prov_nac, year, population
pop_dt <- as.data.table(pop_panel)
setnames(pop_dt, c("year", "population"), c("pop_year", "pop_total"))

# key pop table
setkey(pop_dt, prov_nac, pop_year)

# work on a copy of survey_clean (do NOT destroy it)
survey_dt <- as.data.table(copy(survey_clean))

# HARD check: must have BIRTH + prov_nac
stopifnot("BIRTH" %in% names(survey_dt), "prov_nac" %in% names(survey_dt))

# helper birth year for matching (do NOT overwrite BIRTH)
survey_dt[, BIRTH_int := as.integer(BIRTH)]

# build lookup (unique prov x birth) and KEEP keys as normal columns
lookup <- unique(survey_dt[, .(prov_nac, BIRTH_int)])
setnames(lookup, "BIRTH_int", "pop_year")     # pop_year == birth year (for rolling)
setkey(lookup, prov_nac, pop_year)

# rolling join that preserves lookup keys as prov_nac + pop_year (birth year)
# result has: prov_nac, pop_year (birth), pop_total, pop_year.1 (matched year) etc.
# So we avoid collisions by renaming BEFORE join:
pop2 <- copy(pop_dt)
setnames(pop2, c("pop_year", "pop_total"), c("ref_year", "ref_pop"))
setkey(pop2, prov_nac, ref_year)

# Now join: for each (prov_nac, pop_year=birth), take last ref_year <= pop_year
lookup[, birth_year := pop_year]              # keep explicit birth_year
lookup[, pop_year := NULL]                    # drop to avoid confusion

setkey(lookup, prov_nac, birth_year)
# roll join uses ref_year <= birth_year by joining on a common "year" name:
lookup[, join_year := birth_year]
setkey(lookup, prov_nac, join_year)

# create a version of pop2 with join_year name
pop3 <- pop2[, .(prov_nac, join_year = ref_year, ref_year, ref_pop)]
setkey(pop3, prov_nac, join_year)

tmp <- pop3[lookup, roll = TRUE]

# tmp keeps lookup columns explicitly (birth_year is there), and ref_year/ref_pop are matched
lookup_out <- tmp[, .(
  prov_nac              = prov_nac,
  BIRTH_int             = birth_year,
  pop_birth_ref_year    = ref_year,
  pop_birth_last_census = ref_pop
)]

# merge back to survey_dt (simple join, no rolling)
setkey(lookup_out, prov_nac, BIRTH_int)
setkey(survey_dt,  prov_nac, BIRTH_int)

survey_dt <- lookup_out[survey_dt]

# clean helper
survey_dt[, BIRTH_int := NULL]

# back to tibble
survey_clean <- as_tibble(survey_dt)

# HARD guarantee: BIRTH still exists
stopifnot("BIRTH" %in% names(survey_clean))


# Rainfall summary lookup
get_summary <- function(prov_norm, start_year, end_year) {
  yrs <- seq(start_year, end_year)
  df <- rain_summary %>%
    filter(provincia_norm == prov_norm, year %in% yrs)
  
  if (nrow(df) == 0) return(c(NA_real_, NA_integer_))
  
  # strict coverage check
  if (nrow(df) < length(yrs) || any(is.na(df$dry_days_10))) {
    return(c(mean(df$avg_precip, na.rm = TRUE), NA_integer_))
  }
  
  c(mean(df$avg_precip, na.rm = TRUE), sum(df$dry_days_10))
}

# Compute dry-day metrics 
results <- survey_clean %>%
  rowwise() %>%
  mutate(
    dry_days_last_5        = get_summary(provincia_norm, survey_year - 6, survey_year - 1)[2],
    childhood_total_dry_days = get_summary(provincia_norm, childhood_start, childhood_end)[2],
    dry_days_0_4           = get_summary(provincia_norm, BIRTH, BIRTH + 4)[2],
    dry_days_5_9          = get_summary(provincia_norm, BIRTH + 5,  BIRTH + 9)[2],
    dry_days_10_14         = get_summary(provincia_norm, BIRTH + 10, BIRTH + 14)[2],
    dry_days_15_18         = get_summary(provincia_norm, BIRTH + 15, BIRTH + 18)[2],
    dry_days_8_18         = get_summary(provincia_norm, BIRTH + 8, BIRTH + 18)[2]
  ) %>%
  ungroup() %>%
  select(
    respondent_id,
    dry_days_last_5,
    childhood_total_dry_days,
    dry_days_0_4,
    dry_days_5_9,
    dry_days_10_14, 
    dry_days_15_18,
    dry_days_8_18  
  )

# Coverage diagnostics 
check_cov <- survey_clean %>%
  rowwise() %>%
  mutate(
    yrs = list(seq(childhood_start, childhood_end)),
    have_years = nrow(dplyr::filter(
      rain_summary,
      provincia_norm == .env$provincia_norm,   # <-- refer to current row's province
      year %in% yrs[[1]]
    )),
    expected_years = length(yrs[[1]]),         # <-- length of the sequence, not the list
    coverage_ratio = have_years / expected_years
  ) %>%
  ungroup() %>%
  select(respondent_id, provincia_norm, childhood_start, childhood_end,
         expected_years, have_years, coverage_ratio) %>%
  left_join(select(results, respondent_id, childhood_total_dry_days), by = "respondent_id")

# Example summaries
check_cov %>%
  summarise(
    n = n(),
    median_coverage = median(coverage_ratio, na.rm = TRUE),
    share_cov_lt80  = mean(coverage_ratio < 0.8, na.rm = TRUE),
    cor_low         = cor(coverage_ratio, childhood_total_dry_days, use = "complete.obs")
  )

# Flag suspicious records (incomplete coverage + low totals)
suspects <- check_cov %>%
  filter(coverage_ratio < 0.9) %>%
  arrange(coverage_ratio, childhood_total_dry_days)

# Unmatched provinces (should be empty with the new mapping)
missing_provinces <- setdiff(survey_clean$provincia_norm, rain_summary$provincia_norm)
print(missing_provinces)

# Final merge 
survey_final <- left_join(survey_clean, results, by = "respondent_id") %>%
  mutate(age = survey_year - BIRTH) %>%
  filter(BIRTH >= 1934
         , BIRTH <= 2004)

survey_final %>%
  summarise(
    n_total = n(),
    n_missing_childhood_total_dry_days = sum(is.na(childhood_total_dry_days))
  )



# Export result
write_csv(survey_final, "survey_with_childhood_weather_harmonized.csv")


# Checks about balance and graphs --------------------------------------------------------------
library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(tidyr)
library(ggplot2)

# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, EDUCATION, CONSERVATIVE_VOTE, FAR_RIGHT_VOTE, INCOME, PP_VOTE)

summary(model_data$childhood_total_dry_days)  # check within-group variation

# Check how much variation is left in dry days after BIRTH + prov_nac FE
treatment_fe <- feols(childhood_total_dry_days ~ 1 | BIRTH + prov_nac, data = model_data)
summary(treatment_fe)
sd(resid(treatment_fe))


# Variable overview table -------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(kableExtra)

# 0) Load data + analysis sample (same as balance sample)
survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

balance_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    treat_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(treat_std, 4)
  )

# 1) Variables to include ------------------------------------------------
# Outcomes you use (edit if needed)
outcome_vars <- c(
  "CATHOLIC",
  "RELIGIOUS_PRACTICE",
  "COUPLE_CATHOLIC",
  "PARTICIPATION",
  "CONSERVATIVE_VOTE",
  "LEFT_RIGHT"
)

# Treatment variable to report
treat_vars <- c("childhood_total_dry_days")  # or "treat_std" if you prefer standardized

# Your balance_vars (already filtered to existing)
balance_vars <- c(
  "FEMALE","age","EDUCATION","INCOME",
  "FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT","MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL","MOTHER_SCHOOL",
  "FATHER_CATHOLIC","MOTHER_CATHOLIC",
  "survey_year","pop_birth_last_census","SAME_LOC_BIRTH"
)
balance_vars <- balance_vars[balance_vars %in% names(balance_data)]

# Combine and keep only those that exist in data
vars_all <- unique(c(outcome_vars, treat_vars, balance_vars))
vars_all <- vars_all[vars_all %in% names(balance_data)]

# 2) Pretty labels --------------------------------------------------------
pretty_labels <- c(
  CATHOLIC = "Catholic",
  RELIGIOUS_PRACTICE = "Church attendance",
  COUPLE_CATHOLIC = "Catholic partner",
  PARTICIPATION = "Participation",
  CONSERVATIVE_VOTE = "Conservative Vote",
  LEFT_RIGHT = "Ideological positioning",
  childhood_total_dry_days = "Dry Days",
  
  FEMALE = "Female",
  age = "Age",
  EDUCATION = "Education",
  INCOME = "Income",
  FATHER_BORN_SPAIN = "Father Born Spain",
  MOTHER_BORN_SPAIN = "Mother Born Spain",
  FATHER_EMPLOYMENT = "Father Employment",
  MOTHER_EMPLOYMENT = "Mother Employment",
  FATHER_SCHOOL = "Father school",
  MOTHER_SCHOOL = "Mother school",
  FATHER_CATHOLIC = "Father Catholic",
  MOTHER_CATHOLIC = "Mother Catholic",
  survey_year = "Survey year",
  pop_birth_last_census = "Province population at birth",
  SAME_LOC_BIRTH = "Dummy living province of birth"
)

# 3) Descriptions (edit freely) ------------------------------------------
desc_map <- c(
  CATHOLIC = "Respondent identifies as Catholic (dummy).",
  RELIGIOUS_PRACTICE = "Religious practice frequency (higher = more frequent).",
  COUPLE_CATHOLIC = "Respondent identifies couple as Catholic (dummy).",
  PARTICIPATION = "Voted in the last election (dummy).",
  CONSERVATIVE_VOTE = "Voted for a conservative party in the last election (dummy).",
  LEFT_RIGHT = "Self-placement on left-right scale (0 = left, 10 = right).",
  childhood_total_dry_days = "Total number of dry days during childhood (5–18) in province of birth.",
  
  FEMALE = "Respondent gender (dummy).",
  age = "Age at survey (years).",
  EDUCATION = "Education category/level.",
  INCOME = "Income category.",
  FATHER_BORN_SPAIN = "Father born in Spain (dummy).",
  MOTHER_BORN_SPAIN = "Mother born in Spain (dummy).",
  FATHER_EMPLOYMENT = "Father employed during respondent’s youth (dummy).",
  MOTHER_EMPLOYMENT = "Mother employed during respondent’s youth (dummy).",
  FATHER_SCHOOL = "Father attended school (dummy/category).",
  MOTHER_SCHOOL = "Mother attended school (dummy/category).",
  FATHER_CATHOLIC = "Father identifies as Catholic (dummy).",
  MOTHER_CATHOLIC = "Mother identifies as Catholic (dummy).",
  survey_year = "Survey wave year.",
  pop_birth_last_census = "Province population at birth (census-based).",
  SAME_LOC_BIRTH = "Lives in same province as birth (dummy)."
)

# 4) Build summary stats --------------------------------------------------
summ_one <- function(x) {
  tibble(
    N = sum(!is.na(x)),
    Min = suppressWarnings(min(x, na.rm = TRUE)),
    Mean = mean(x, na.rm = TRUE),
    Max = suppressWarnings(max(x, na.rm = TRUE))
  )
}

tab1 <- lapply(vars_all, function(v) {
  out <- summ_one(balance_data[[v]])
  out$variable <- v
  out
}) %>%
  bind_rows() %>%
  mutate(
    Variable = recode(variable, !!!pretty_labels),
    Description = unname(desc_map[variable]),
    Description = ifelse(is.na(Description), "", Description)
  ) %>%
  select(Variable, N, Min, Mean, Max, Description) %>%
  mutate(
    across(c(Min, Mean, Max), ~ round(.x, 2))
  )

# 5) LaTeX output ---------------------------------------------------------
latex_tab <- kbl(
  tab1,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE,
  caption = "Summary statistics (analysis sample).",
  align = "lrrrrp{7.5cm}"
) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 9) %>%
  column_spec(1, width = "3.5cm") %>%
  column_spec(6, width = "8cm") %>%
  row_spec(0, bold = TRUE)

# Wrap with resizebox (works even with p{...})
cat(paste0("\\resizebox{\\textwidth}{!}{%\n", latex_tab, "\n}\n"))


# ROBUSTNESS: Balance of observables across treatment quartiles --------------------


library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)


# 0) Load + build analysis sample
survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

model_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    # Standardize treatment (pooled sample)
    treat_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(treat_std, 4)
  )

# Choose observables (edit to match what you want in the paper)
balance_vars <- c(
  "FEMALE",
  "age",
  "EDUCATION",
  "INCOME",
  "FATHER_BORN_SPAIN",
  "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT",
  "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL",
  "MOTHER_SCHOOL",
  "FATHER_CATHOLIC",
  "MOTHER_CATHOLIC", 
  "survey_year", 
  "pop_birth_last_census",
  "SAME_LOC_BIRTH"
)

# Keep only existing vars (safe)
balance_vars <- balance_vars[balance_vars %in% names(model_data)]


# 1) Helper: standardized diff (Q1 vs Q4)
std_diff_q1_q4 <- function(x, q) {
  x1 <- x[q == 1]
  x4 <- x[q == 4]
  m1 <- mean(x1, na.rm = TRUE)
  m4 <- mean(x4, na.rm = TRUE)
  s  <- sqrt((var(x1, na.rm = TRUE) + var(x4, na.rm = TRUE)) / 2)
  if (is.na(s) || s == 0) return(NA_real_)
  abs((m4 - m1) / s)  # absolute standardized difference
}


# 2) Compute RAW standardized differences

raw_smd <- sapply(balance_vars, function(v) {
  std_diff_q1_q4(model_data[[v]], model_data$treat_q)
})


# 3) Residualize each covariate on FE, then compute SMD

fe_smd <- sapply(balance_vars, function(v) {
  
  # residualize v with same FE used in main regressions
  # (cluster not needed for residuals)
  m <- feols(as.formula(paste0(v, " ~ 1 | BIRTH + prov_nac")), data = model_data)
  
  v_resid <- residuals(m)
  
  std_diff_q1_q4(v_resid, model_data$treat_q)
})

# 4) Build plotting dataframe
balance_plot <- tibble(
  variable = balance_vars,
  Raw      = raw_smd,
  `After FE (BIRTH + prov)` = fe_smd
) %>%
  pivot_longer(-variable, names_to = "spec", values_to = "smd") %>%
  mutate(variable = factor(variable, levels = rev(balance_vars)))

library(forcats)

# --- Optional prettier labels (include EVERYTHING you plot) ---
pretty_labels <- c(
  FEMALE = "Female",
  age = "Age",
  EDUCATION = "Education",
  INCOME = "Income",
  FATHER_BORN_SPAIN = "Father born in Spain",
  MOTHER_BORN_SPAIN = "Mother born in Spain",
  FATHER_EMPLOYMENT = "Father employed",
  MOTHER_EMPLOYMENT = "Mother employed",
  FATHER_SCHOOL = "Father attended school",
  MOTHER_SCHOOL = "Mother attended school",
  FATHER_CATHOLIC = "Father Catholic",
  MOTHER_CATHOLIC = "Mother Catholic",
  survey_year = "Survey year",
  pop_birth_last_census = "Province population",
  SAME_LOC_BIRTH = "Same province at birth"
)

# --- Apply labels AND enforce ordering correctly ---
balance_plot <- balance_plot %>%
  mutate(
    # recode variable names
    variable = recode(as.character(variable), !!!pretty_labels),
    # enforce y-axis order using the (recoded) labels
    variable = factor(variable, levels = rev(pretty_labels[balance_vars])),
    # ensure spec order for legend/colors
    spec = factor(spec, levels = c("Raw", "After FE (Birth year + province)"))
  )

# --- Plot (LaTeX-ready) ---
p_balance <- ggplot(balance_plot, aes(x = smd, y = variable, color = spec, shape = spec)) +
  geom_point(size = 2.8) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  scale_color_manual(values = c(
    "Raw" = "orange",
    "After FE (Birth year + province)" = "blue"
  )) +
  labs(
    title = "Balance of observables across treatment quartiles",
    subtitle = "Absolute standardized difference between Q1 and Q4 (Raw vs FE)",
    x = "Absolute standardized difference |(Q4 − Q1)/SD|",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_balance)

# ROBUSTNESS TABLE (LaTeX-ready):

library(dplyr)
library(tidyr)
library(fixest)
library(kableExtra)


# 0) Load + build analysis sample

survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

balance_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    treat_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(treat_std, 4)
  )

balance_vars <- c(
  "FEMALE",
  "age",
  "EDUCATION",
  "INCOME",
  "FATHER_BORN_SPAIN",
  "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT",
  "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL",
  "MOTHER_SCHOOL",
  "FATHER_CATHOLIC",
  "MOTHER_CATHOLIC",
  "survey_year",
  "pop_birth_last_census",
  "SAME_LOC_BIRTH"
)

balance_vars <- balance_vars[balance_vars %in% names(balance_data)]

pretty_labels <- c(
  FEMALE = "Female",
  age = "Age",
  EDUCATION = "Education",
  INCOME = "Income",
  FATHER_BORN_SPAIN = "Father born in Spain",
  MOTHER_BORN_SPAIN = "Mother born in Spain",
  FATHER_EMPLOYMENT = "Father employed",
  MOTHER_EMPLOYMENT = "Mother employed",
  FATHER_SCHOOL = "Father school",
  MOTHER_SCHOOL = "Mother school",
  FATHER_CATHOLIC = "Father Catholic",
  MOTHER_CATHOLIC = "Mother Catholic",
  survey_year = "Survey year",
  pop_birth_last_census = "Province population at birth", 
  SAME_LOC_BIRTH = "Dummy living province of birth" 
)


# 1) Helpers

mean_by_q <- function(x, q, k) mean(x[q == k], na.rm = TRUE)

# standardized diff vs Q1 using SD(Q1)
std_diff_vs_q1 <- function(x, q, k) {
  x1 <- x[q == 1]
  xk <- x[q == k]
  m1 <- mean(x1, na.rm = TRUE)
  mk <- mean(xk, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE)
  if (is.na(s1) || s1 == 0) return(NA_real_)
  (mk - m1) / s1
}


# 2) Raw means (Q1–Q4)

means_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Q1 = mean_by_q(balance_data[[v]], balance_data$treat_q, 1),
    Q2 = mean_by_q(balance_data[[v]], balance_data$treat_q, 2),
    Q3 = mean_by_q(balance_data[[v]], balance_data$treat_q, 3),
    Q4 = mean_by_q(balance_data[[v]], balance_data$treat_q, 4)
  )
}) |> bind_rows()


# 3) Raw standardized diffs vs Q1

smd_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Raw_Q2vsQ1 = std_diff_vs_q1(balance_data[[v]], balance_data$treat_q, 2),
    Raw_Q3vsQ1 = std_diff_vs_q1(balance_data[[v]], balance_data$treat_q, 3),
    Raw_Q4vsQ1 = std_diff_vs_q1(balance_data[[v]], balance_data$treat_q, 4)
  )
}) |> bind_rows()


# 4) FE-residualized standardized diffs vs Q1

smd_fe <- lapply(balance_vars, function(v) {
  
  fe_mod <- feols(
    as.formula(paste0(v, " ~ 1 | BIRTH + prov_nac")),
    data = balance_data
  )
  x_res <- residuals(fe_mod)
  
  tibble(
    variable = v,
    FE_Q2vsQ1 = std_diff_vs_q1(x_res, balance_data$treat_q, 2),
    FE_Q3vsQ1 = std_diff_vs_q1(x_res, balance_data$treat_q, 3),
    FE_Q4vsQ1 = std_diff_vs_q1(x_res, balance_data$treat_q, 4)
  )
}) |> bind_rows()


# 5) Merge + labels + add Ns

n_by_q <- balance_data %>% count(treat_q) %>% arrange(treat_q) %>% pull(n)

tab <- means_raw %>%
  left_join(smd_raw, by = "variable") %>%
  left_join(smd_fe,  by = "variable") %>%
  mutate(variable = recode(variable, !!!pretty_labels)) %>%
  relocate(variable)

# Optional: round nicely (keep numeric as numeric)
tab_out <- tab %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


# 6) LaTeX table with clean grouping

# Build the dynamic label first
means_lab <- sprintf(
  "Means by quartile (n = %d, %d, %d, %d)",
  n_by_q[1], n_by_q[2], n_by_q[3], n_by_q[4]
)

# Now build the header vector (names must be literal strings)
header_vec <- c(
  " " = 1,
  setNames(4, means_lab),
  "Std. diff vs Q1 (Raw)" = 3,
  "Std. diff vs Q1 (After FE)" = 3
)

# Use it
kbl(
  tab_out,
  format = "latex",
  booktabs = TRUE,
  caption = "Balance of observables across quartiles of childhood dry days. Columns Q1--Q4 report raw means by quartile. Columns Raw and After FE report standardized differences relative to Q1, using SD(Q1). After FE residualizes each observable on birth-year and province fixed effects.",
  align = "lrrrrrrr"
) %>%
  add_header_above(header_vec) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



# Covariate Balance table -------------------------------------------------
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#  Create quartiles of treatment 
dat <- survey_final %>%
  filter(BORN_SPAIN == 1, !is.na(childhood_total_dry_days)) %>%
  mutate(dry_q = ntile(childhood_total_dry_days, 4))

# Covariates 
covars <- c("FEMALE",
            "FATHER_BORN_SPAIN", "CATHOLIC", "FATHER_EMPLOYMENT",
            "MOTHER_BORN_SPAIN", "CONSERVATIVE_VOTE", "MOTHER_EMPLOYMENT",
            "survey_year", "EDUCATION", "INCOME", "TRUST_PEOPLE", "pop_birth_last_census", "SAME_LOC_BIRTH" )

# Function for SMD (Q1 vs Q4)
smd_fun <- function(x, g) {
  m1 <- mean(x[g == 1], na.rm = TRUE)
  m4 <- mean(x[g == 4], na.rm = TRUE) 
  s  <- sqrt((var(x[g == 1], na.rm = TRUE) + var(x[g == 4], na.rm = TRUE)) / 2)
  if (is.na(s) || s == 0) return(NA_real_)
  (m1 - m4) / s
}

# Compute means by quartile 
means_by_q <- dat %>%
  group_by(dry_q) %>%
  summarise(across(all_of(covars), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(-dry_q, names_to = "variable", values_to = "mean") %>%
  pivot_wider(names_from = dry_q, values_from = mean, names_prefix = "Q")

# Compute SMDs
smds <- sapply(covars, function(v) smd_fun(dat[[v]], dat$dry_q))
smd_table <- tibble(variable = covars, `SMD (Q1 vs Q4)` = smds)

# Merge means and SMDs -
final_balance <- means_by_q %>%
  left_join(smd_table, by = "variable")


# Create summary of missing values and frequencies
summary_table <- survey %>% 
  summarise(across(everything(), list(
    missing = ~sum(is.na(.)),
    missing_pct = ~mean(is.na(.)) * 100,
    unique_values = ~n_distinct(.)
  ))) %>% 
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_sep = "_")

# View the summary table
summary_table

# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(  # use namespace explicitly to avoid conflicts
    PARTICIPATION,  # or PARTICIPATION depending on your model
    , CATHOLIC, childhood_total_dry_days, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, PP_VOTE, pop_birth_last_census, SAME_LOC_BIRTH
  )


library(dplyr)
library(ggplot2)

# Prepare province-level summary (excluding CEUTA)
prov_summary <- model_data %>%
  mutate(prov_nac = sprintf("%02d", as.integer(prov_nac))) %>%
  filter(prov_nac != "51") %>%
  group_by(prov_nac) %>%
  summarise(
    catholic_share     = mean(CATHOLIC, na.rm = TRUE),
    conservative_share = mean(CONSERVATIVE_VOTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(prov_code_map %>%
              mutate(prov_nac = sprintf("%02d", as.integer(prov_nac))),
            by = "prov_nac")

ggplot(prov_summary, aes(x = catholic_share, y = conservative_share)) +
  geom_point(size = 2.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", size = 1, alpha = 0.3) +
  geom_text(aes(label = prov_nac),
            vjust = -0.8, size = 3.5, check_overlap = TRUE, fontface = "bold") +
  labs(
    x = "Share Identifying as Catholic",
    y = "Share Voting Conservative last election"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )



# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION, SIZE_TOWN, pop_birth_last_census)

# Summary statistics (before standardization)
summary_stats <- model_data %>%
  summarize(
    Min    = min(childhood_total_dry_days, na.rm = TRUE),
    Q1     = quantile(childhood_total_dry_days, 0.25, na.rm = TRUE),
    Median = median(childhood_total_dry_days, na.rm = TRUE),
    Mean   = mean(childhood_total_dry_days, na.rm = TRUE),
    Q3     = quantile(childhood_total_dry_days, 0.75, na.rm = TRUE),
    Max    = max(childhood_total_dry_days, na.rm = TRUE),
    SD     = sd(childhood_total_dry_days, na.rm = TRUE)
  )


# Create tidy summary table (Variables in rows, Stats in columns)
summary_stats_vars <- model_data %>%
  summarize(across(c(childhood_total_dry_days, CATHOLIC, FEMALE, FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EMPLOYMENT, FATHER_CATHOLIC, 
                     MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, RELIGIOUS_PRACTICE, 
                     COUPLE_CATHOLIC, PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT, SAME_LOC_BIRTH, pop_birth_last_census, EDUCATION, INCOME, TRUST_PEOPLE ),
                   list(
                     N      = ~sum(!is.na(.x)),
                     Min    = ~min(.x, na.rm = TRUE),
                     Median = ~median(.x, na.rm = TRUE),
                     Mean   = ~mean(.x, na.rm = TRUE),
                     Max    = ~max(.x, na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(N|Min|Median|Mean|Max)$",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = Value
  ) %>%
  arrange(Variable)


summary_stats_vars

# Convert to LaTeX table
library(kableExtra)
kable(summary_stats, format = "latex", digits = 2,
      caption = "Summary Statistics for used variables") %>%
  kable_styling(latex_options = "hold_position")


ggplot(model_data, aes(x = childhood_total_dry_days)) +
  geom_density(fill = "orange", alpha = 0.4, color = "#2C3E50", size = 1) +
  labs(
    x = "Total Dry Days During Childhood",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )

ggplot(model_data, aes(x = childhood_total_dry_days)) +
  geom_histogram(aes(y = after_stat(density)), bins =  31, fill = "darkorange", color = "white", alpha = 0.7) +
  geom_density(color = "#2C3E50", size = 1) +
  labs(
    x = "Total Dry Days During Childhood",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )



# Covariate Balance LaTeX Table

library(dplyr)
library(tidyr)
library(kableExtra)
library(tibble)

# --- Quartiles of treatment ---
dat <- survey_final %>%
  filter(BORN_SPAIN == 1, !is.na(childhood_total_dry_days)) %>%
  mutate(dry_q = ntile(childhood_total_dry_days, 4))

covars <- c("FEMALE",
            "FATHER_BORN_SPAIN", "CATHOLIC", "FATHER_EMPLOYMENT",
            "MOTHER_BORN_SPAIN", "CONSERVATIVE_VOTE", "MOTHER_EMPLOYMENT",
            "survey_year", "EDUCATION", "INCOME")

# --- SMD function (Q1 vs Q4) ---
smd_fun <- function(x, g) {
  m1 <- mean(x[g == 1], na.rm = TRUE)
  m4 <- mean(x[g == 4], na.rm = TRUE)
  s  <- sqrt((var(x[g == 1], na.rm = TRUE) + var(x[g == 4], na.rm = TRUE)) / 2)
  if (is.na(s) || s == 0) return(NA_real_)
  (m1 - m4) / s
}

# --- Means by quartile ---
means_by_q <- dat %>%
  group_by(dry_q) %>%
  summarise(across(all_of(covars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-dry_q, names_to = "variable", values_to = "mean") %>%
  pivot_wider(names_from = dry_q, values_from = mean, names_prefix = "Q")

# --- SMDs (Q1 vs Q4) ---
smd_table <- tibble(
  variable = covars,
  `SMD (Q1 vs Q4)` = sapply(covars, function(v) smd_fun(dat[[v]], dat$dry_q))
)

# --- Merge & order ---
final_balance <- means_by_q %>%
  left_join(smd_table, by = "variable") %>%
  mutate(variable = factor(variable, levels = covars)) %>%
  arrange(variable)

# --- Pretty labels (optional) ---
pretty_labels <- c(
  FEMALE = "Female",
  FATHER_BORN_SPAIN = "Father born in Spain",
  CATHOLIC = "Catholic",
  FATHER_EMPLOYMENT = "Father employed",
  MOTHER_BORN_SPAIN = "Mother born in Spain",
  CONSERVATIVE_VOTE = "Conservative vote",
  MOTHER_EMPLOYMENT = "Mother employed",
  survey_year = "Survey year",
  EDUCATION = "Education (cat/years)",
  INCOME = "Income (cat)"
)

final_balance <- final_balance %>%
  mutate(variable = recode(as.character(variable), !!!pretty_labels))

# --- Add N per quartile to column headers (FIXED) ---
n_by_q <- dat %>% count(dry_q) %>% arrange(dry_q) %>% pull(n)
q_cols <- c("Q1","Q2","Q3","Q4")

final_balance_out <- final_balance %>%
  select(variable, all_of(q_cols), `SMD (Q1 vs Q4)`) %>%
  rename_with(
    .fn   = ~ paste0(.x, " (n=", n_by_q[match(.x, q_cols)], ")"),
    .cols = all_of(q_cols)
  )

# --- LaTeX table ---
kbl(
  final_balance_out,
  format = "latex", booktabs = TRUE, digits = 3,
  caption = "Covariate means by quartile of childhood dry days and standardized mean difference (Q1 vs Q4)."
) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Means by treatment quartile" = 4, " " = 1))

library(dplyr); library(tidyr); library(fixest); library(kableExtra); library(tibble)



# Main regressions -------------------------------------------

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(tidyr)
library(ggplot2)


# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

model_data %>% count(BIRTH, name = "n") %>% arrange(BIRTH)


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, INCOME, EDUCATION, RELIGIOUS_PRACTICE, PARTICIPATION, SIZE_TOWN, dry_days_5_9, dry_days_10_14, dry_days_15_18, PP_VOTE, pop_birth_last_census)

model_data %>% count(BIRTH, name = "n") %>% arrange(BIRTH)

model_data %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>% arrange(desc(n_na), variable) %>% print(n = Inf, width = Inf)

model_data <- model_data %>%
  mutate(
    log_pop_birth = log(pop_birth_last_census)
  )

# Standardize Treatment Variable


model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) / sd(childhood_total_dry_days, na.rm = TRUE)
  )


# Linear Probability Models: CATHOLIC 


# 1. With FE, No controls
lpm_fe_nocontrols_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)| BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN +  survey_year +
     MOTHER_BORN_SPAIN +  FATHER_EMPLOYMENT +
    MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Linear Probability Models: RELIGIOUS PRACTICE 


# 1. With FE, No controls
lpm_fe_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT + log_pop_birth  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Linear Probability Models: COUPLE_CATHOLIC


# 1. With FE, No controls
lpm_fe_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN  + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT + log_pop_birth  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Model summary: Linear (Standardized), single Controls indicator


modelsummary(
  list(
    "Catholic"                        = lpm_fe_nocontrols_cat,
    "Catholic + Controls"             = lpm_fe_controls_cat,
    "Religious practice"              = lpm_fe_nocontrols_rel,
    "Religious practice + Controls"   = lpm_fe_controls_rel,
    "Couple catholic"              = lpm_fe_nocontrols_cou,
    "Couple catholic + Controls"   = lpm_fe_controls_cou
  ),
  title = "LPM Catholic identification and OLS Religious attendance",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",   # survey_year always included
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Catholic` = "No",
    `Catholic + Controls` = "Yes",
    `Religious practice` = "No",
    `Religious practice + Controls` = "Yes",
    `Couple Catholic` = "No",
    `Couple Catholic + Controls` = "Yes"
  )
)


# Linear Probability Models: PARTICIPATION

# 1. With FE, No controls
lpm_fe_nocontrols_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + survey_year  + I(childhood_total_dry_days_std^2)    | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN +  MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Linear Probability Models: CONSERVATIVE_VOTE

# 1. With FE, No controls
lpm_fe_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)   | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN + 
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN +  MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Linear Probability Models: LEFT_RIGHT


# 1. With FE, No controls
lpm_fe_nocontrols_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2) | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) +
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT + survey_year  + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)




# Model summary: Linear (Standardized), single Controls indicator


modelsummary(
  list(
    "Participation"                   = lpm_fe_nocontrols_par,
    "Participation + Controls"        = lpm_fe_controls_par,
    "Conservative"                    = lpm_fe_nocontrols_con,
    "Conservative + Controls"         = lpm_fe_controls_con,
    "Left or Right"                       = lpm_fe_nocontrols_far,
    "Left or Right"            = lpm_fe_controls_far
  ),
  title = "LPM: Participation in last election, probability voting conservative and ideological positioning",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Participation` = "No",
    `Participation + Controls` = "Yes",
    `Conservative` = "No",
    `Conservative + Controls` = "Yes",
    `Left or Right` = "No",
    `Left or Right + Controls` = "Yes"
  )
)

# Falsification analysis - Income, Education and Trust people ---------------------------


# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION, INCOME, EDUCATION, pop_birth_last_census)


# Standardize Treatment Variable

model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) / sd(childhood_total_dry_days, na.rm = TRUE)
  )

model_data <- model_data %>%
  mutate(
    log_pop_birth = log(pop_birth_last_census)
  )


# Linear Probability Models: EDUCATION


# 1. With FE, No controls
lpm_fe_nocontrols_cat <- feols(
  EDUCATION ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2) | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cat <- feols(
  EDUCATION ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN +  survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Linear Probability Models: INCOME


# 1. With FE, No controls
lpm_fe_nocontrols_rel <- feols(
  INCOME ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_rel <- feols(
  INCOME ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN  + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)



# Linear Probability Models: TRUST_PEOPLE


# 1. With FE, No controls
lpm_fe_nocontrols_cou_trust <- feols(
  TRUST_PEOPLE ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cou_trust <- feols(
  TRUST_PEOPLE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN  + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)



# Model summary: Linear (Standardized), single Controls indicator


modelsummary(
  list(
    "Education"                        = lpm_fe_nocontrols_cat,
    "Education"             = lpm_fe_controls_cat,
    "Household Income"              = lpm_fe_nocontrols_rel,
    "Household Income"   = lpm_fe_controls_rel,
    "Trust People"                 = lpm_fe_nocontrols_cou_trust,
    "Trust People"      = lpm_fe_controls_cou_trust
  ),
  title = "OLS education level, OLS Household income and OLS ideology",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",   # survey_year always included
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Catholic` = "No",
    `Catholic + Controls` = "Yes",
    `Religious practice` = "No",
    `Religious practice + Controls` = "Yes",
    `Couple Catholic` = "No",
    `Couple Catholic + Controls` = "Yes"
  )
)





# Spline section ----------------------------------------------------------



library(fixest)
library(splines)

# Knots for the spline (based on treatment distribution)
knots_treat <- quantile(
  model_data$childhood_total_dry_days_std,
  probs = c(0.33, 0.66),
  na.rm = TRUE
)

# --- Religious outcomes ---

lpm_fe_spline_cat <- feols(
  CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_rel <- feols(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_cou <- feols(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# --- Political outcomes ---

lpm_fe_spline_par <- feols(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_con <- feols(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_lr <- feols(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)


# Cubic spline ADRF with CIs (graphs)


library(splines)
library(dplyr)
library(ggplot2)
library(sandwich)   # install.packages("sandwich") if needed

# 0) Spline knots and boundary knots (match your estimation)
knots_treat <- quantile(
  model_data$childhood_total_dry_days_std,
  probs = c(0.33, 0.66),
  na.rm = TRUE
)

boundary_knots <- range(model_data$childhood_total_dry_days_std, na.rm = TRUE)

# 1) Refit spline models for plotting (FE as dummies)

lm_spline_cat <- lm(
  CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

lm_spline_rel <- lm(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

lm_spline_cou <- lm(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

lm_spline_par <- lm(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

lm_spline_con <- lm(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

lm_spline_lr <- lm(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac),
  data = model_data
)

# 2) Grid for treatment (standardized dry days)
t_min <- quantile(model_data$childhood_total_dry_days_std, 0.01, na.rm = TRUE)
t_max <- quantile(model_data$childhood_total_dry_days_std, 0.99, na.rm = TRUE)
grid_t <- seq(t_min, t_max, length.out = 100)
t0     <- 0  # baseline: 0 SD

# 3) Helper: build spline-only ADRF with cluster CIs (effect vs T = 0)

make_spline_curve_lm <- function(mod, outcome_name) {
  beta_full <- coef(mod)
  
  # pick only spline coefficients: ns(...)
  idx_spline <- grepl("^ns\\(", names(beta_full))
  beta_s     <- beta_full[idx_spline]
  
  # cluster-robust vcov by province, then keep spline block
  V_full <- sandwich::vcovCL(mod, cluster = ~ prov_nac, type = "HC1")
  V_s    <- V_full[idx_spline, idx_spline, drop = FALSE]
  
  # spline basis at grid and at baseline T0
  X_grid <- splines::ns(
    grid_t,
    knots = knots_treat,
    Boundary.knots = boundary_knots
  )
  X0 <- splines::ns(
    t0,
    knots = knots_treat,
    Boundary.knots = boundary_knots
  )
  
  # repeat X0 for each row of grid to form differences ns(T) - ns(0)
  X0_mat <- matrix(rep(X0, each = nrow(X_grid)),
                   nrow = nrow(X_grid))
  
  diffX <- X_grid - X0_mat
  
  # effect and CIs: diffX * beta_s, var = diffX V_s diffX'
  fit <- as.numeric(diffX %*% beta_s)
  
  XV   <- diffX %*% V_s
  var_ <- rowSums(XV * diffX)
  se   <- sqrt(pmax(var_, 0))
  
  tibble(
    Outcome = outcome_name,
    childhood_total_dry_days_std = grid_t,
    fit     = fit,
    conf_lo = fit - 1.96 * se,
    conf_hi = fit + 1.96 * se
  )
}

# 4) Curves for religious outcomes
religious_spline_curves <- dplyr::bind_rows(
  make_spline_curve_lm(lm_spline_cat, "Catholic"),
  make_spline_curve_lm(lm_spline_rel, "Religious practice"),
  make_spline_curve_lm(lm_spline_cou, "Couple Catholic")
)

# 5) Curves for political outcomes
political_spline_curves <- dplyr::bind_rows(
  make_spline_curve_lm(lm_spline_par, "Participation"),
  make_spline_curve_lm(lm_spline_con, "Conservative vote"),
  make_spline_curve_lm(lm_spline_lr,  "Left-right")
)

# 6) Plot: religious outcomes
ggplot(religious_spline_curves,
       aes(x = childhood_total_dry_days_std, y = fit)) +
  geom_ribbon(aes(ymin = conf_lo, ymax = conf_hi), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Cubic-spline effect of childhood dry days on religious outcomes\n(effect vs 0 SD; FE + controls)",
    x = "Childhood total dry days (standardized)",
    y = "Effect on outcome (vs 0 SD dry days)"
  ) +
  theme_minimal()

# 7) Plot: political outcomes
ggplot(political_spline_curves,
       aes(x = childhood_total_dry_days_std, y = fit)) +
  geom_ribbon(aes(ymin = conf_lo, ymax = conf_hi), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Cubic-spline effect of childhood dry days on political outcomes\n(effect vs 0 SD; FE + controls)",
    x = "Childhood total dry days (standardized)",
    y = "Effect on outcome (vs 0 SD dry days)"
  ) +
  theme_minimal()


# kernel regression  ------------------------------------------------------

library(fixest)
library(dplyr)
library(np)
library(ggplot2)
library(scales)

# 0) USER INPUTS (change here only)

y_var <- "LEFT_RIGHT"                     # outcome
d_var <- "childhood_total_dry_days_std" # treatment

controls_rhs <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL",
  "log_pop_birth",
  "survey_year"
)

fe_rhs <- c("BIRTH", "prov_nac")

n_bins <- 50    # 🔴 number of bins (change here)

# 1) Build ONE consistent estimation sample


vars_needed <- unique(c(y_var, d_var, controls_rhs, fe_rhs))

df <- model_data %>%
  dplyr::select(dplyr::all_of(vars_needed)) %>%
  dplyr::filter(stats::complete.cases(.))


# 2) Residualize outcome Y


f_y <- as.formula(
  paste0(
    y_var, " ~ ",
    paste(controls_rhs, collapse = " + "),
    " | ",
    paste(fe_rhs, collapse = " + ")
  )
)

m_y <- feols(f_y, data = df)
df$y_res <- resid(m_y)


# 3) Residualize treatment D


f_d <- as.formula(
  paste0(
    d_var, " ~ ",
    paste(controls_rhs, collapse = " + "),
    " | ",
    paste(fe_rhs, collapse = " + ")
  )
)

m_d <- feols(f_d, data = df)
df$d_res <- resid(m_d)


# 4) Local-linear kernel regression E[y_res | d_res]


bw <- npregbw(
  xdat = df$d_res,
  ydat = df$y_res,
  regtype = "ll"
)

kr <- npreg(
  bws   = bw,
  exdat = df$d_res,
  eydat = df$y_res
)

grid <- data.frame(
  d_res = seq(
    min(df$d_res, na.rm = TRUE),
    max(df$d_res, na.rm = TRUE),
    length.out = 300
  )
)

grid$yhat <- as.numeric(predict(kr, exdat = grid$d_res))


# ------------------------------------------------------------
# 5) Binned means (40 quantile bins — cosmetic change)
# ------------------------------------------------------------

n_bins <- 50   # 🔴 reduced from 50 to 40

df_bins <- df %>%
  mutate(bin = ntile(d_res, n_bins)) %>%
  group_by(bin) %>%
  summarise(
    d_bin = mean(d_res),
    y_bin = mean(y_res),
    se    = sd(y_res) / sqrt(n()),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 6) Plot (cosmetic improvements only)
# ------------------------------------------------------------

ggplot() +
  # binned means
  geom_point(
    data = df_bins,
    aes(d_bin, y_bin),
    size = 2.1,
    color = "black"
  ) +
  # confidence intervals (lighter)
  geom_errorbar(
    data = df_bins,
    aes(
      x = d_bin,
      ymin = y_bin - 1.96 * se,
      ymax = y_bin + 1.96 * se
    ),
    width = 0,
    alpha = 0.35     # 🔴 reduced opacity
  ) +
  # kernel regression line (thicker)
  geom_line(
    data = grid,
    aes(d_res, yhat),
    linewidth = 1.6, # 🔴 thicker line
    color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Residualized childhood dry days (standardized)",
    y = "Ideological positioning",
    title = "Nonparametric relationship between residualized treatment and outcome",
    subtitle = paste0(
      "Points show ", n_bins,
      " binned means with 95% CIs; solid line is local-linear kernel fit"
    )
  ) +
  theme_minimal(base_size = 12)


# Tertile and quartile plots ----------------------------------------------


library(dplyr)

# Raw variable on which you defined tertiles/quartiles
x_var <- "childhood_total_dry_days"


model_data <- model_data %>%
  mutate(
    q_group = ntile(.data[[x_var]], 4),  # quartiles (≈ same n in each)
    t_group = ntile(.data[[x_var]], 3)   # tertiles  (≈ same n in each)
  )

# -------- Quartiles table (equal-sized groups) --------
quartile_table <- model_data %>%
  group_by(q_group) %>%
  summarise(
    n_obs    = n(),
    min_dry  = min(.data[[x_var]], na.rm = TRUE),
    max_dry  = max(.data[[x_var]], na.rm = TRUE),
    mean_dry = mean(.data[[x_var]], na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(quartile = paste0("Q", q_group)) %>%
  select(quartile, n_obs, min_dry, max_dry, mean_dry)

quartile_table





# -------- Tertiles table (equal-sized groups) --------
tertile_table <- model_data %>%
  group_by(t_group) %>%
  summarise(
    n_obs    = n(),
    min_dry  = min(.data[[x_var]], na.rm = TRUE),
    max_dry  = max(.data[[x_var]], na.rm = TRUE),
    mean_dry = mean(.data[[x_var]], na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(tertile = paste0("T", t_group)) %>%
  select(tertile, n_obs, min_dry, max_dry, mean_dry)

tertile_table


library(dplyr)
library(ggplot2)

# Raw variable
x_var <- "childhood_total_dry_days"


# 1) Define groups with ntile (same as regressions)


model_data <- model_data %>%
  mutate(
    q_group = ntile(.data[[x_var]], 4),  # quartiles
    t_group = ntile(.data[[x_var]], 3)   # tertiles
  )

# Ranges for each quartile / tertile (min & max x in that ntile)
quartile_ranges <- model_data %>%
  group_by(q_group) %>%
  summarise(
    xmin = min(.data[[x_var]], na.rm = TRUE),
    xmax = max(.data[[x_var]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(xmin) %>%
  mutate(Quartile = factor(paste0("Q", row_number()),
                           levels = paste0("Q", 1:4)))

tertile_ranges <- model_data %>%
  group_by(t_group) %>%
  summarise(
    xmin = min(.data[[x_var]], na.rm = TRUE),
    xmax = max(.data[[x_var]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(xmin) %>%
  mutate(Tertile = factor(paste0("T", row_number()),
                          levels = paste0("T", 1:3)))

xmin_all <- min(model_data[[x_var]], na.rm = TRUE)
xmax_all <- max(model_data[[x_var]], na.rm = TRUE)


# 2) Quartiles plot (bands from ntile groups)

p_quartiles <- ggplot(model_data, aes(x = .data[[x_var]])) +
  geom_rect(data = quartile_ranges,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf, fill = Quartile),
            inherit.aes = FALSE, alpha = 0.15) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 fill = "grey80", color = "white", alpha = 0.9) +
  geom_density(color = "black", size = 0.8) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  coord_cartesian(xlim = c(xmin_all, xmax_all)) +
  labs(
    x = "Total Dry Days During Childhood",
    y = "Density",
    title = "Distribution of Childhood Dry Days (Quartiles, ntile groups)"
  ) +
  theme_minimal()

p_quartiles


# 3) Tertiles plot (bands from ntile groups)

p_tertiles <- ggplot(model_data, aes(x = .data[[x_var]])) +
  geom_rect(data = tertile_ranges,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf, fill = Tertile),
            inherit.aes = FALSE, alpha = 0.15) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 fill = "grey80", color = "white", alpha = 0.9) +
  geom_density(color = "black", size = 0.8) +
  scale_fill_brewer(palette = "Set3", name = NULL) +
  coord_cartesian(xlim = c(xmin_all, xmax_all)) +
  labs(
    x = "Total Dry Days During Childhood",
    y = "Density",
    title = "Distribution of Childhood Dry Days (Tertiles, ntile groups)"
  ) +
  theme_minimal()

p_tertiles


# Non-parametric identification: quartile dummies of treatment -----------------------------------------------------------------------

library(ggplot2)
library(scales)


# ADRF plot styling helpers

theme_adrf <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.25),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

plot_adrf <- function(df, nbins, xlabels, title,
                      ylab = "Estimated effect (vs lowest bin)",
                      line_color = "#1f77b4") {
  
  ggplot(df, aes(x = treat_level, y = estimate, group = Outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.55) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.10, color = "grey55", linewidth = 0.55) +
    geom_line(color = line_color, linewidth = 0.95) +
    geom_point(color = line_color, size = 2.2) +
    facet_wrap(~ Outcome, scales = "free_y") +
    scale_x_continuous(breaks = 1:nbins, labels = xlabels) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(title = title, x = NULL, y = ylab) +
    theme_adrf()
}


model_data <- model_data %>%
  mutate(
    treat_q = factor(ntile(childhood_total_dry_days_std, 4), levels = 1:4),
    treat_t = factor(ntile(childhood_total_dry_days_std, 3), levels = 1:3)
  )

# 1) Quartiles of standardized treatment (overall distribution)
model_data <- model_data %>%
  mutate(
    treat_q = ntile(childhood_total_dry_days_std, 4)  # 1 = lowest exposure, 4 = highest
  )

table(model_data$treat_q, useNA = "ifany")  # quick sanity check


# Religious outcomes: CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC

# CATHOLIC
lpm_fe_q_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth  | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_q_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# COUPLE CATHOLIC
lpm_fe_q_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)


# Joint tests (religious outcomes)
# H0: all quartile dummies = 0  (Q2 = Q3 = Q4 = 0)


wald_cat_ctrl <- wald(lpm_fe_q_controls_cat, keep = "treat_q::")
wald_rel_ctrl <- wald(lpm_fe_q_controls_rel, keep = "treat_q::")
wald_cou_ctrl <- wald(lpm_fe_q_controls_cou, keep = "treat_q::")

wald_cat_ctrl
wald_rel_ctrl
wald_cou_ctrl


modelsummary(
  list(
    "Catholic "                        = lpm_fe_q_nocontrols_cat,
    "Catholic (Q bins) + Controls"             = lpm_fe_q_controls_cat,
    "Religious practice (Q bins)"              = lpm_fe_q_nocontrols_rel,
    "Religious practice (Q bins) + Controls"   = lpm_fe_q_controls_rel,
    "Couple catholic (Q bins)"                 = lpm_fe_q_nocontrols_cou,
    "Couple catholic (Q bins) + Controls"      = lpm_fe_q_controls_cou
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Catholic (Q bins)`                      = "No",
    `Catholic (Q bins) + Controls`           = "Yes",
    `Religious practice (Q bins)`            = "No",
    `Religious practice (Q bins) + Controls` = "Yes",
    `Couple catholic (Q bins)`               = "No",
    `Couple catholic (Q bins) + Controls`    = "Yes"
  )
)


# Political outcomes: PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT

# PARTICIPATION
lpm_fe_q_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# CONSERVATIVE VOTE
lpm_fe_q_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# LEFT-RIGHT SCALE
lpm_fe_q_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)


# Joint tests (political outcomes)
# H0: all quartile dummies = 0


wald_par_ctrl <- wald(lpm_fe_q_controls_par, keep = "treat_q::")
wald_con_ctrl <- wald(lpm_fe_q_controls_con, keep = "treat_q::")
wald_lr_ctrl  <- wald(lpm_fe_q_controls_lr,  keep = "treat_q::")

wald_par_ctrl
wald_con_ctrl
wald_lr_ctrl

modelsummary(
  list(
    "Participation (Q bins)"                  = lpm_fe_q_nocontrols_par,
    "Participation (Q bins) + Controls"       = lpm_fe_q_controls_par,
    "Conservative (Q bins)"                   = lpm_fe_q_nocontrols_con,
    "Conservative (Q bins) + Controls"        = lpm_fe_q_controls_con,
    "Left-right (Q bins)"                     = lpm_fe_q_nocontrols_lr,
    "Left-right (Q bins) + Controls"          = lpm_fe_q_controls_lr
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Participation (Q bins)`             = "No",
    `Participation (Q bins) + Controls`  = "Yes",
    `Conservative (Q bins)`              = "No",
    `Conservative (Q bins) + Controls`   = "Yes",
    `Left-right (Q bins)`                = "No",
    `Left-right (Q bins) + Controls`     = "Yes"
  )
)



library(broom)
library(dplyr)
library(ggplot2)
library(purrr)


# Religious outcomes: extract estimates

religious_models_q <- list(
  Catholic           = lpm_fe_q_controls_cat,
  ReligiousPractice  = lpm_fe_q_controls_rel,
  CoupleCatholic     = lpm_fe_q_controls_cou
)

coef_religious_q <- map_dfr(
  religious_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )


# Political outcomes: extract estimates

political_models_q <- list(
  Participation = lpm_fe_q_controls_par,
  Conservative  = lpm_fe_q_controls_con,
  LeftRight     = lpm_fe_q_controls_lr
)

coef_political_q <- map_dfr(
  political_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )


# Plot: Religious Outcomes (Quartile Dummies)

ggplot(coef_religious_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Religious Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()


# Plot: Political Outcomes (Quartile Dummies)

ggplot(coef_political_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Political Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()

# Prepare ADRF-style data for religious outcomes
adrf_data_religious <- coef_religious_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(coef_religious_q$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )

# Plot ADRF approximation
p_adrf_q_rel <- plot_adrf(
  df      = adrf_data_religious,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  title   = "Religious Outcomes — Quartiles",
  line_color = "#1f77b4"
)

p_adrf_q_rel


adrf_data_political <- coef_political_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(coef_political_q$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )

# Plot
p_adrf_q_pol <- plot_adrf(
  df      = adrf_data_political,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  title   = "Political Outcomes — Quartiles",
  line_color = "#1f77b4"
)

p_adrf_q_pol






# 2) OTHER outcomes regressions 

# INCOME
lpm_fe_q_nocontrols_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_q_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_q_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)


# 3) Joint tests (OTHER outcomes): H0: Q2 = Q3 = Q4 = 0 

wald_inc_q_ctrl   <- wald(lpm_fe_q_controls_inc,   keep = "treat_q::")
wald_edu_q_ctrl   <- wald(lpm_fe_q_controls_edu,   keep = "treat_q::")
wald_trust_q_ctrl <- wald(lpm_fe_q_controls_trust, keep = "treat_q::")

wald_inc_q_ctrl
wald_edu_q_ctrl
wald_trust_q_ctrl


# 4) Table (OTHER outcomes) 

modelsummary(
  list(
    "Income (Q bins)"                  = lpm_fe_q_nocontrols_inc,
    "Income (Q bins) + Controls"       = lpm_fe_q_controls_inc,
    "Education (Q bins)"               = lpm_fe_q_nocontrols_edu,
    "Education (Q bins) + Controls"    = lpm_fe_q_controls_edu,
    "Trust people (Q bins)"            = lpm_fe_q_nocontrols_trust,
    "Trust people (Q bins) + Controls" = lpm_fe_q_controls_trust
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Income (Q bins)`                  = "No",
    `Income (Q bins) + Controls`       = "Yes",
    `Education (Q bins)`               = "No",
    `Education (Q bins) + Controls`    = "Yes",
    `Trust people (Q bins)`            = "No",
    `Trust people (Q bins) + Controls` = "Yes"
  )
)


# 5) Plot: coefficients + 95% CI (OTHER outcomes) 

library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

other_models_q <- list(
  Income      = lpm_fe_q_controls_inc,
  Education   = lpm_fe_q_controls_edu,
  TrustPeople = lpm_fe_q_controls_trust
)

coef_other_q <- map_dfr(
  other_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )

ggplot(coef_other_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Other Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()


# ADRF-style plot (OTHER outcomes) 

adrf_data_other <- coef_other_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome     = unique(coef_other_q$Outcome),
      treat_level = 1,
      estimate    = 0,
      conf.low    = 0,
      conf.high   = 0
    )
  )

p_adrf_q_other <- plot_adrf(
  df      = adrf_data_other,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  title   = "Other Outcomes — Quartiles",
  line_color = "#1f77b4"
)

p_adrf_q_other



# Non-parametric identification: tertile dummies of treatment -----------------------------

# 1) Tertiles of standardized treatment (overall distribution)
model_data <- model_data %>%
  mutate(
    treat_t = ntile(childhood_total_dry_days_std, 3)  # 1 = low, 2 = mid, 3 = high exposure
  )

table(model_data$treat_t, useNA = "ifany")  # sanity check

# Religious outcomes: CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC

# CATHOLIC
lpm_fe_t_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_t_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth| BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# COUPLE CATHOLIC
lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
) 

lpm_fe_t_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year  + log_pop_birth  | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# Joint tests (religious outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_cat_t_ctrl <- wald(lpm_fe_t_controls_cat, keep = "treat_t::")
wald_rel_t_ctrl <- wald(lpm_fe_t_controls_rel, keep = "treat_t::")
wald_cou_t_ctrl <- wald(lpm_fe_t_controls_cou, keep = "treat_t::")

wald_cat_t_ctrl
wald_rel_t_ctrl
wald_cou_t_ctrl

modelsummary(
  list(
    "Catholic (T bins)"                        = lpm_fe_t_nocontrols_cat,
    "Catholic (T bins) + Controls"             = lpm_fe_t_controls_cat,
    "Religious practice (T bins)"              = lpm_fe_t_nocontrols_rel,
    "Religious practice (T bins) + Controls"   = lpm_fe_t_controls_rel,
    "Couple catholic (T bins)"                 = lpm_fe_t_nocontrols_cou,
    "Couple catholic (T bins) + Controls"      = lpm_fe_t_controls_cou
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Catholic (T bins)`                      = "No",
    `Catholic (T bins) + Controls`           = "Yes",
    `Religious practice (T bins)`            = "No",
    `Religious practice (T bins) + Controls` = "Yes",
    `Couple catholic (T bins)`               = "No",
    `Couple catholic (T bins) + Controls`    = "Yes"
  )
)

# Political outcomes: PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT


# PARTICIPATION
lpm_fe_t_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# CONSERVATIVE VOTE
lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# LEFT-RIGHT SCALE
lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year  + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Joint tests (political outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_par_t_ctrl <- wald(lpm_fe_t_controls_par, keep = "treat_t::")
wald_con_t_ctrl <- wald(lpm_fe_t_controls_con, keep = "treat_t::")
wald_lr_t_ctrl <- wald(lpm_fe_t_controls_lr, keep = "treat_t::")


wald_par_t_ctrl
wald_con_t_ctrl
wald_lr_t_ctrl


modelsummary(
  list(
    "Participation (T bins)" = lpm_fe_t_nocontrols_par,
    "Participation (T bins) + Controls" = lpm_fe_t_controls_par,
    "Conservative (T bins)" = lpm_fe_t_nocontrols_con,
    "Conservative (T bins) + Controls" = lpm_fe_t_controls_con,
    "Left-right (T bins)" = lpm_fe_t_nocontrols_lr,
    "Left-right (T bins) + Controls" = lpm_fe_t_controls_lr
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Participation (T bins)` = "No",
    `Participation (T bins) + Controls` = "Yes",
    `Conservative (T bins)` = "No",
    `Conservative (T bins) + Controls` = "Yes",
    `Left-right (T bins)` = "No",
    `Left-right (T bins) + Controls` = "Yes"
  )
)



# Extract coefficients and CIs
religious_models <- list(
  Catholic           = lpm_fe_t_controls_cat,
  ReligiousPractice  = lpm_fe_t_controls_rel,
  CoupleCatholic     = lpm_fe_t_controls_cou
)

coef_df <- purrr::map_dfr(
  religious_models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )

# Plot
ggplot(coef_df, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies)",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()

political_models <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

coef_df <- purrr::map_dfr(
  political_models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )


# Plot
ggplot(coef_df, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies)",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()


# Step 1: Extract coefficients from religious models (with controls)

religious_models_t <- list(
  Catholic           = lpm_fe_t_controls_cat,
  ReligiousPractice  = lpm_fe_t_controls_rel,
  CoupleCatholic     = lpm_fe_t_controls_cou
)

coef_religious_t <- map_dfr(
  religious_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1"),
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(.$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )


# Step 2: Repeat for political models

political_models_t <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

coef_political_t <- map_dfr(
  political_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1"),
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(.$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )


# Step 3: Plot ADRF-style graphs


# Religious Outcomes ADRF Plot
p_adrf_t_rel <- plot_adrf(
  df      = coef_religious_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  title   = "Religious Outcomes — Tertiles",
  line_color = "#1f77b4"
)

p_adrf_t_rel


p_adrf_t_pol <- plot_adrf(
  df      = coef_political_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  title   = "Political Outcomes — Tertiles",
  line_color = "#1f77b4"
)

p_adrf_t_pol


# 1) OTHER outcomes regressions 

# INCOME
lpm_fe_t_nocontrols_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year  + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)


# 2) Joint tests (OTHER outcomes): H0: T2 = T3 = 0 

wald_inc_t_ctrl   <- wald(lpm_fe_t_controls_inc,   keep = "treat_t::")
wald_edu_t_ctrl   <- wald(lpm_fe_t_controls_edu,   keep = "treat_t::")
wald_trust_t_ctrl <- wald(lpm_fe_t_controls_trust, keep = "treat_t::")

wald_inc_t_ctrl
wald_edu_t_ctrl
wald_trust_t_ctrl


# 3) Table (OTHER outcomes) 

modelsummary(
  list(
    "Income (T bins)"                  = lpm_fe_t_nocontrols_inc,
    "Income (T bins) + Controls"       = lpm_fe_t_controls_inc,
    "Education (T bins)"               = lpm_fe_t_nocontrols_edu,
    "Education (T bins) + Controls"    = lpm_fe_t_controls_edu,
    "Trust people (T bins)"            = lpm_fe_t_nocontrols_trust,
    "Trust people (T bins) + Controls" = lpm_fe_t_controls_trust
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Income (T bins)`                  = "No",
    `Income (T bins) + Controls`       = "Yes",
    `Education (T bins)`               = "No",
    `Education (T bins) + Controls`    = "Yes",
    `Trust people (T bins)`            = "No",
    `Trust people (T bins) + Controls` = "Yes"
  )
)


# 4) Plot: coefficients + 95% CI (OTHER outcomes) 

library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

other_models_t <- list(
  Income      = lpm_fe_t_controls_inc,
  Education   = lpm_fe_t_controls_edu,
  TrustPeople = lpm_fe_t_controls_trust
)

coef_other_t <- map_dfr(
  other_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )

ggplot(coef_other_t, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies) on Other Outcomes",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()


# 5) Optional: ADRF-style plot (OTHER outcomes) 

adrf_data_other_t <- coef_other_t %>%
  mutate(
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome     = unique(coef_other_t$Outcome),
      treat_level = 1,
      estimate    = 0,
      conf.low    = 0,
      conf.high   = 0
    )
  )

p_adrf_t_other <- plot_adrf(
  df      = adrf_data_other_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  title   = "Other Outcomes — Tertiles",
  line_color = "#1f77b4"
)

p_adrf_t_other



# Non-parametric identification: fixed dry-day bins of treatment -----------------------------

# 1) Bins of raw childhood_total_dry_days
model_data <- model_data %>%
  mutate(
    treat_t = case_when(
      childhood_total_dry_days <= 100 ~ 1L,                           # Low exposure
      childhood_total_dry_days > 100 & childhood_total_dry_days < 107 ~ 2L,  # Medium
      childhood_total_dry_days >= 107 ~ 3L,                            # High
      TRUE ~ NA_integer_
    )
  )

table(model_data$treat_t, useNA = "ifany")  # sanity check


# Religious outcomes: CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC

# CATHOLIC
lpm_fe_t_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# COUPLE CATHOLIC
lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# Joint tests (religious outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_cat_t_ctrl <- wald(lpm_fe_t_controls_cat, keep = "treat_t::")
wald_rel_t_ctrl <- wald(lpm_fe_t_controls_rel, keep = "treat_t::")
wald_cou_t_ctrl <- wald(lpm_fe_t_controls_cou, keep = "treat_t::")

wald_cat_t_ctrl
wald_rel_t_ctrl
wald_cou_t_ctrl

modelsummary(
  list(
    "Catholic (bins)"                      = lpm_fe_t_nocontrols_cat,
    "Catholic (bins) + Controls"           = lpm_fe_t_controls_cat,
    "Religious practice (bins)"            = lpm_fe_t_nocontrols_rel,
    "Religious practice (bins) + Controls" = lpm_fe_t_controls_rel,
    "Couple catholic (bins)"               = lpm_fe_t_nocontrols_cou,
    "Couple catholic (bins) + Controls"    = lpm_fe_t_controls_cou
  ),
  title = "LPM with dry-day bins of childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Medium exp vs Low",
    "treat_t::3" = "High exposure vs Low"
  ),
  add_rows = tibble(
    term = "Controls",
    `Catholic (bins)`                      = "No",
    `Catholic (bins) + Controls`           = "Yes",
    `Religious practice (bins)`            = "No",
    `Religious practice (bins) + Controls` = "Yes",
    `Couple catholic (bins)`               = "No",
    `Couple catholic (bins) + Controls`    = "Yes"
  )
)


# Political outcomes: PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT


# PARTICIPATION
lpm_fe_t_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

# CONSERVATIVE VOTE
lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

# LEFT-RIGHT SCALE
lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

# Joint tests (political outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_par_t_ctrl <- wald(lpm_fe_t_controls_par, keep = "treat_t::")
wald_con_t_ctrl <- wald(lpm_fe_t_controls_con, keep = "treat_t::")
wald_lr_t_ctrl  <- wald(lpm_fe_t_controls_lr,  keep = "treat_t::")

wald_par_t_ctrl
wald_con_t_ctrl
wald_lr_t_ctrl

modelsummary(
  list(
    "Participation (bins)"             = lpm_fe_t_nocontrols_par,
    "Participation (bins) + Controls"  = lpm_fe_t_controls_par,
    "Conservative (bins)"              = lpm_fe_t_nocontrols_con,
    "Conservative (bins) + Controls"   = lpm_fe_t_controls_con,
    "Left-right (bins)"                = lpm_fe_t_nocontrols_lr,
    "Left-right (bins) + Controls"     = lpm_fe_t_controls_lr
  ),
  title = "LPM with dry-day bins of childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Medium vs Low",
    "treat_t::3" = "High vs Low"
  ),
  add_rows = tibble(
    term = "Controls",
    `Participation (bins)`             = "No",
    `Participation (bins) + Controls`  = "Yes",
    `Conservative (bins)`              = "No",
    `Conservative (bins) + Controls`   = "Yes",
    `Left-right (bins)`                = "No",
    `Left-right (bins) + Controls`     = "Yes"
  )
)



# ADRF-style graphs (only graphs in this section)

library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

# Helper to build ADRF data from a list of models
build_adrf_data <- function(models_list) {
  map_dfr(
    models_list,
    ~ tidy(.x, conf.int = TRUE),
    .id = "Outcome"
  ) %>%
    filter(grepl("^treat_t::", term)) %>%
    mutate(
      treat_level = case_when(
        term == "treat_t::2" ~ 2L,
        term == "treat_t::3" ~ 3L
      )
    ) %>%
    select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
    bind_rows(
      tibble(
        Outcome     = unique(.$Outcome),
        treat_level = 1L,
        estimate    = 0,
        conf.low    = 0,
        conf.high   = 0
      )
    ) %>%
    mutate(
      exposure_level = factor(
        treat_level,
        levels = 1:3,
        labels = c("Low ",
                   "Medium",
                   "High")
      )
    )
}

# Religious ADRF data
religious_models_t <- list(
  Catholic          = lpm_fe_t_controls_cat,
  ReligiousPractice = lpm_fe_t_controls_rel,
  CoupleCatholic    = lpm_fe_t_controls_cou
)

coef_religious_t <- build_adrf_data(religious_models_t)

# Political ADRF data
political_models_t <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

coef_political_t <- build_adrf_data(political_models_t)

# Religious Outcomes ADRF Plot
ggplot(coef_religious_t,
       aes(x = exposure_level, y = estimate,
           ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Approximate Dose Response (Religious Outcomes)",
    x = "Childhood dry-day exposure",
    y = "Estimated Effect (vs Low exposure)"
  ) +
  theme_minimal()

# Political Outcomes ADRF Plot
ggplot(coef_political_t,
       aes(x = exposure_level, y = estimate,
           ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Approximate Dose Response (Political Outcomes)",
    x = "Childhood dry-day exposure",
    y = "Estimated Effect (vs Low exposure)"
  ) +
  theme_minimal()


# Other outcomes: INCOME, EDUCATION, TRUST_PEOPLE

# INCOME
lpm_fe_t_nocontrols_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + 
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# Joint tests (other outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_inc_t_ctrl   <- wald(lpm_fe_t_controls_inc,   keep = "treat_t::")
wald_edu_t_ctrl   <- wald(lpm_fe_t_controls_edu,   keep = "treat_t::")
wald_trust_t_ctrl <- wald(lpm_fe_t_controls_trust, keep = "treat_t::")

wald_inc_t_ctrl
wald_edu_t_ctrl
wald_trust_t_ctrl

modelsummary(
  list(
    "Income (bins)"                  = lpm_fe_t_nocontrols_inc,
    "Income (bins) + Controls"       = lpm_fe_t_controls_inc,
    "Education (bins)"               = lpm_fe_t_nocontrols_edu,
    "Education (bins) + Controls"    = lpm_fe_t_controls_edu,
    "Trust people (bins)"            = lpm_fe_t_nocontrols_trust,
    "Trust people (bins) + Controls" = lpm_fe_t_controls_trust
  ),
  title = "LPM with dry-day bins of childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Medium vs Low",
    "treat_t::3" = "High vs Low"
  ),
  add_rows = tibble(
    term = "Controls",
    `Income (bins)`                  = "No",
    `Income (bins) + Controls`       = "Yes",
    `Education (bins)`               = "No",
    `Education (bins) + Controls`    = "Yes",
    `Trust people (bins)`            = "No",
    `Trust people (bins) + Controls` = "Yes"
  )
)


# ADRF-style graph for other outcomes 

other_models_t <- list(
  Income      = lpm_fe_t_controls_inc,
  Education   = lpm_fe_t_controls_edu,
  TrustPeople = lpm_fe_t_controls_trust
)

coef_other_t <- build_adrf_data(other_models_t)  # uses the helper defined earlier

ggplot(coef_other_t,
       aes(x = exposure_level, y = estimate,
           ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Approximate Dose Response (Other Outcomes)",
    x = "Childhood dry-day exposure",
    y = "Estimated Effect (vs Low exposure)"
  ) +
  theme_minimal()



# Robustness: Conley (spatial) standard errors -----------------------------------------------------------------------


library(fixest)
library(mapSpain)
library(sf)
library(dplyr)
library(modelsummary)

# 1) Province centroids (true coordinates for distances)
#    Use moveCAN = FALSE to keep actual positions of Canary Islands, etc.
provinces_conley <- esp_get_prov(moveCAN = FALSE)

prov_coords_conley <- provinces_conley %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat  = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  transmute(
    prov_nac = as.integer(cpro),
    lat, long
  )

# 2) Merge coordinates into the model_data used for quartile regressions
model_data_conley <- model_data %>%
  left_join(prov_coords_conley, by = "prov_nac")

# quick sanity check
model_data_conley %>%
  summarise(
    n              = n(),
    n_missing_lat  = sum(is.na(lat)),
    n_missing_long = sum(is.na(long))
  )


# Re-estimate main "+ controls" specs on model_data_conley
# (point estimates will be the same as with IID SEs; only SEs change)


#  Religious outcomes (+ controls) 
lpm_fe_q_controls_cat_c <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_rel_c <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_cou_c <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

# Political outcomes (+ controls) 
lpm_fe_q_controls_par_c <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_con_c <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_lr_c <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)


# Conley spatial SEs (e.g. 200km cutoff, spherical distance)


cutoff_km <- 200  # you can also try 100, 300, etc. as robustness

sum_cat_conley <- summary(
  lpm_fe_q_controls_cat_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_rel_conley <- summary(
  lpm_fe_q_controls_rel_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_cou_conley <- summary(
  lpm_fe_q_controls_cou_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_par_conley <- summary(
  lpm_fe_q_controls_par_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_con_conley <- summary(
  lpm_fe_q_controls_con_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_lr_conley <- summary(
  lpm_fe_q_controls_lr_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

# (Optional) check how much SEs change for one coefficient
se(lpm_fe_q_controls_cat)          # clustered by prov_nac (your baseline)
se(sum_cat_conley)                 # Conley SEs


# LaTeX tables with Conley SEs

modelsummary(
  list(
    "Catholic (Q bins) + Controls, Conley SE"           = sum_cat_conley,
    "Religious practice (Q bins) + Controls, Conley SE" = sum_rel_conley,
    "Couple catholic (Q bins) + Controls, Conley SE"    = sum_cou_conley
  ),
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days ",
    "(religious outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Catholic (Q bins) + Controls, Conley SE`           = "Yes",
    `Religious practice (Q bins) + Controls, Conley SE` = "Yes",
    `Couple catholic (Q bins) + Controls, Conley SE`    = "Yes"
  )
)


modelsummary(
  list(
    "Participation (Q bins) + Controls, Conley SE" = sum_par_conley,
    "Conservative (Q bins) + Controls, Conley SE"  = sum_con_conley,
    "Left-right (Q bins) + Controls, Conley SE"    = sum_lr_conley
  ),
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days ",
    "(political outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Participation (Q bins) + Controls, Conley SE` = "Yes",
    `Conservative (Q bins) + Controls, Conley SE`  = "Yes",
    `Left-right (Q bins) + Controls, Conley SE`    = "Yes"
  )
)

model_data %>%
  group_by(treat_q) %>%
  summarise(
    n_total      = n(),
    n_missing_Y  = sum(is.na(CONSERVATIVE_VOTE)),
    share_missing_Y = mean(is.na(CONSERVATIVE_VOTE))
  )



# Religious outcomes (+ controls, tertiles)

lpm_fe_t_controls_cat_c <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_rel_c <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_cou_c <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

# Political outcomes (+ controls, tertiles)

lpm_fe_t_controls_par_c <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_con_c <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_lr_c <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)


# Conley spatial SEs for tertile models

sum_cat_t_conley <- summary(
  lpm_fe_t_controls_cat_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_rel_t_conley <- summary(
  lpm_fe_t_controls_rel_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_cou_t_conley <- summary(
  lpm_fe_t_controls_cou_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_par_t_conley <- summary(
  lpm_fe_t_controls_par_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_con_t_conley <- summary(
  lpm_fe_t_controls_con_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_lr_t_conley <- summary(
  lpm_fe_t_controls_lr_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

# (optional sanity check)
se(lpm_fe_t_controls_cat)      # clustered by prov_nac (baseline)
se(sum_cat_t_conley)          # Conley SEs (tertile version)

modelsummary(
  list(
    "Catholic (T bins) + Controls, Conley SE"           = sum_cat_t_conley,
    "Religious practice (T bins) + Controls, Conley SE" = sum_rel_t_conley,
    "Couple catholic (T bins) + Controls, Conley SE"    = sum_cou_t_conley
  ),
  title = paste0(
    "LPM with tertile dummies of standardized childhood dry days ",
    "(religious outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Catholic (T bins) + Controls, Conley SE`           = "Yes",
    `Religious practice (T bins) + Controls, Conley SE` = "Yes",
    `Couple catholic (T bins) + Controls, Conley SE`    = "Yes"
  )
)

modelsummary(
  list(
    "Participation (T bins) + Controls, Conley SE" = sum_par_t_conley,
    "Conservative (T bins) + Controls, Conley SE"  = sum_con_t_conley,
    "Left-right (T bins) + Controls, Conley SE"    = sum_lr_t_conley
  ),
  title = paste0(
    "LPM with tertile dummies of standardized childhood dry days ",
    "(political outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Participation (T bins) + Controls, Conley SE` = "Yes",
    `Conservative (T bins) + Controls, Conley SE`  = "Yes",
    `Left-right (T bins) + Controls, Conley SE`    = "Yes"
  )
)


# OTHER outcomes (+ controls) with Conley spatial SEs


# Quartiles (+ controls)


lpm_fe_q_controls_inc_c <- feols(
  INCOME ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_edu_c <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_q_controls_trust_c <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

sum_inc_conley <- summary(
  lpm_fe_q_controls_inc_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_edu_conley <- summary(
  lpm_fe_q_controls_edu_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_trust_conley <- summary(
  lpm_fe_q_controls_trust_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

modelsummary(
  list(
    "Income (Q bins) + Controls, Conley SE"       = sum_inc_conley,
    "Education (Q bins) + Controls, Conley SE"    = sum_edu_conley,
    "Trust people (Q bins) + Controls, Conley SE" = sum_trust_conley
  ),
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days ",
    "(other outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Income (Q bins) + Controls, Conley SE`       = "Yes",
    `Education (Q bins) + Controls, Conley SE`    = "Yes",
    `Trust people (Q bins) + Controls, Conley SE` = "Yes"
  )
)



# Tertiles (+ controls)


lpm_fe_t_controls_inc_c <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_edu_c <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

lpm_fe_t_controls_trust_c <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data_conley
)

sum_inc_t_conley <- summary(
  lpm_fe_t_controls_inc_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_edu_t_conley <- summary(
  lpm_fe_t_controls_edu_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

sum_trust_t_conley <- summary(
  lpm_fe_t_controls_trust_c,
  vcov = conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long
)

modelsummary(
  list(
    "Income (T bins) + Controls, Conley SE"       = sum_inc_t_conley,
    "Education (T bins) + Controls, Conley SE"    = sum_edu_t_conley,
    "Trust people (T bins) + Controls, Conley SE" = sum_trust_t_conley
  ),
  title = paste0(
    "LPM with tertile dummies of standardized childhood dry days ",
    "(other outcomes), Conley spatial SEs (cutoff = ", cutoff_km, " km)"
  ),
  output    = "latex",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Income (T bins) + Controls, Conley SE`       = "Yes",
    `Education (T bins) + Controls, Conley SE`    = "Yes",
    `Trust people (T bins) + Controls, Conley SE` = "Yes"
  )
)


# Placebo: Years 0 to 4 ---------------------------------------------------


# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         !is.na(dry_days_0_4)) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, dry_days_0_4, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION, INCOME, EDUCATION)


# Non-parametric identification: quartile dummies of treatment -----------------------------------------------------------------------

model_data <- model_data %>%
  mutate(
    treat_q = factor(ntile(dry_days_0_4, 4), levels = 1:4),
    treat_t = factor(ntile(dry_days_0_4, 3), levels = 1:3)
  )

# 1) Quartiles of standardized treatment (overall distribution)
model_data <- model_data %>%
  mutate(
    treat_q = ntile(dry_days_0_4, 4)  # 1 = lowest exposure, 4 = highest
  )

table(model_data$treat_q, useNA = "ifany")  # quick sanity check


# Religious outcomes: CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC

# CATHOLIC
lpm_fe_q_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_q_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# COUPLE CATHOLIC
lpm_fe_q_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)


# Joint tests (religious outcomes)
# H0: all quartile dummies = 0  (Q2 = Q3 = Q4 = 0)


wald_cat_ctrl <- wald(lpm_fe_q_controls_cat, keep = "treat_q::")
wald_rel_ctrl <- wald(lpm_fe_q_controls_rel, keep = "treat_q::")
wald_cou_ctrl <- wald(lpm_fe_q_controls_cou, keep = "treat_q::")

wald_cat_ctrl
wald_rel_ctrl
wald_cou_ctrl


modelsummary(
  list(
    "Catholic "                        = lpm_fe_q_nocontrols_cat,
    "Catholic (Q bins) + Controls"             = lpm_fe_q_controls_cat,
    "Religious practice (Q bins)"              = lpm_fe_q_nocontrols_rel,
    "Religious practice (Q bins) + Controls"   = lpm_fe_q_controls_rel,
    "Couple catholic (Q bins)"                 = lpm_fe_q_nocontrols_cou,
    "Couple catholic (Q bins) + Controls"      = lpm_fe_q_controls_cou
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Catholic (Q bins)`                      = "No",
    `Catholic (Q bins) + Controls`           = "Yes",
    `Religious practice (Q bins)`            = "No",
    `Religious practice (Q bins) + Controls` = "Yes",
    `Couple catholic (Q bins)`               = "No",
    `Couple catholic (Q bins) + Controls`    = "Yes"
  )
)


# Political outcomes: PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT

# PARTICIPATION
lpm_fe_q_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# CONSERVATIVE VOTE
lpm_fe_q_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# LEFT-RIGHT SCALE
lpm_fe_q_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_q_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)


# Joint tests (political outcomes)
# H0: all quartile dummies = 0


wald_par_ctrl <- wald(lpm_fe_q_controls_par, keep = "treat_q::")
wald_con_ctrl <- wald(lpm_fe_q_controls_con, keep = "treat_q::")
wald_lr_ctrl  <- wald(lpm_fe_q_controls_lr,  keep = "treat_q::")

wald_par_ctrl
wald_con_ctrl
wald_lr_ctrl

modelsummary(
  list(
    "Participation (Q bins)"                  = lpm_fe_q_nocontrols_par,
    "Participation (Q bins) + Controls"       = lpm_fe_q_controls_par,
    "Conservative (Q bins)"                   = lpm_fe_q_nocontrols_con,
    "Conservative (Q bins) + Controls"        = lpm_fe_q_controls_con,
    "Left-right (Q bins)"                     = lpm_fe_q_nocontrols_lr,
    "Left-right (Q bins) + Controls"          = lpm_fe_q_controls_lr
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Participation (Q bins)`             = "No",
    `Participation (Q bins) + Controls`  = "Yes",
    `Conservative (Q bins)`              = "No",
    `Conservative (Q bins) + Controls`   = "Yes",
    `Left-right (Q bins)`                = "No",
    `Left-right (Q bins) + Controls`     = "Yes"
  )
)



library(broom)
library(dplyr)
library(ggplot2)
library(purrr)


# Religious outcomes: extract estimates

religious_models_q <- list(
  Catholic           = lpm_fe_q_controls_cat,
  ReligiousPractice  = lpm_fe_q_controls_rel,
  CoupleCatholic     = lpm_fe_q_controls_cou
)

coef_religious_q <- map_dfr(
  religious_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )


# Political outcomes: extract estimates

political_models_q <- list(
  Participation = lpm_fe_q_controls_par,
  Conservative  = lpm_fe_q_controls_con,
  LeftRight     = lpm_fe_q_controls_lr
)

coef_political_q <- map_dfr(
  political_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )


# Plot: Religious Outcomes (Quartile Dummies)

ggplot(coef_religious_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Religious Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()


# Plot: Political Outcomes (Quartile Dummies)

ggplot(coef_political_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Political Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()

# Prepare ADRF-style data for religious outcomes
adrf_data_religious <- coef_religious_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(coef_religious_q$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )

# Plot ADRF approximation
ggplot(adrf_data_religious, aes(x = treat_level, y = estimate)) +
  geom_line(aes(group = Outcome), linetype = "solid") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 1:4, labels = paste("Q", 1:4, sep = "")) +
  labs(
    title = "Approximate Dose Response Function (Religious Outcomes)",
    x = "Treatment Quartile",
    y = "Estimated Effect (vs Q1)"
  ) +
  theme_minimal()


adrf_data_political <- coef_political_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(coef_political_q$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )

# Plot
ggplot(adrf_data_political, aes(x = treat_level, y = estimate)) +
  geom_line(aes(group = Outcome), linetype = "solid") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 1:4, labels = paste("Q", 1:4, sep = "")) +
  labs(
    title = "Approximate Dose Response Function (Political Outcomes)",
    x = "Treatment Quartile",
    y = "Estimated Effect (vs Q1)"
  ) +
  theme_minimal()





# 2) OTHER outcomes regressions 

# INCOME
lpm_fe_q_nocontrols_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_q_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_q_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)


# 3) Joint tests (OTHER outcomes): H0: Q2 = Q3 = Q4 = 0 

wald_inc_q_ctrl   <- wald(lpm_fe_q_controls_inc,   keep = "treat_q::")
wald_edu_q_ctrl   <- wald(lpm_fe_q_controls_edu,   keep = "treat_q::")
wald_trust_q_ctrl <- wald(lpm_fe_q_controls_trust, keep = "treat_q::")

wald_inc_q_ctrl
wald_edu_q_ctrl
wald_trust_q_ctrl


# 4) Table (OTHER outcomes) 

modelsummary(
  list(
    "Income (Q bins)"                  = lpm_fe_q_nocontrols_inc,
    "Income (Q bins) + Controls"       = lpm_fe_q_controls_inc,
    "Education (Q bins)"               = lpm_fe_q_nocontrols_edu,
    "Education (Q bins) + Controls"    = lpm_fe_q_controls_edu,
    "Trust people (Q bins)"            = lpm_fe_q_nocontrols_trust,
    "Trust people (Q bins) + Controls" = lpm_fe_q_controls_trust
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Income (Q bins)`                  = "No",
    `Income (Q bins) + Controls`       = "Yes",
    `Education (Q bins)`               = "No",
    `Education (Q bins) + Controls`    = "Yes",
    `Trust people (Q bins)`            = "No",
    `Trust people (Q bins) + Controls` = "Yes"
  )
)


# 5) Plot: coefficients + 95% CI (OTHER outcomes) 

library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

other_models_q <- list(
  Income      = lpm_fe_q_controls_inc,
  Education   = lpm_fe_q_controls_edu,
  TrustPeople = lpm_fe_q_controls_trust
)

coef_other_q <- map_dfr(
  other_models_q,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_q::", term)) %>%
  mutate(
    Quartile = recode(term,
                      "treat_q::2" = "Q2 vs Q1",
                      "treat_q::3" = "Q3 vs Q1",
                      "treat_q::4" = "Q4 vs Q1")
  )

ggplot(coef_other_q, aes(x = Quartile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Dry Days Quartiles on Other Outcomes",
    y = "Coefficient (95% CI)", x = NULL
  ) +
  theme_minimal()


# ADRF-style plot (OTHER outcomes) 

adrf_data_other <- coef_other_q %>%
  mutate(
    treat_level = case_when(
      Quartile == "Q2 vs Q1" ~ 2,
      Quartile == "Q3 vs Q1" ~ 3,
      Quartile == "Q4 vs Q1" ~ 4
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome     = unique(coef_other_q$Outcome),
      treat_level = 1,
      estimate    = 0,
      conf.low    = 0,
      conf.high   = 0
    )
  )

ggplot(adrf_data_other, aes(x = treat_level, y = estimate)) +
  geom_line(aes(group = Outcome)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
  labs(
    title = "Approximate Dose Response Function (Other Outcomes)",
    x = "Treatment Quartile",
    y = "Estimated Effect (vs Q1)"
  ) +
  theme_minimal()




# Non-parametric identification: tertile dummies of treatment -----------------------------

# 1) Tertiles of standardized treatment (overall distribution)
model_data <- model_data %>%
  mutate(
    treat_t = ntile(dry_days_0_4, 3)  # 1 = low, 2 = mid, 3 = high exposure
  )

table(model_data$treat_t, useNA = "ifany")  # sanity check

# Religious outcomes: CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC

# CATHOLIC
lpm_fe_t_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_t_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# COUPLE CATHOLIC
lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_t_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# Joint tests (religious outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_cat_t_ctrl <- wald(lpm_fe_t_controls_cat, keep = "treat_t::")
wald_rel_t_ctrl <- wald(lpm_fe_t_controls_rel, keep = "treat_t::")
wald_cou_t_ctrl <- wald(lpm_fe_t_controls_cou, keep = "treat_t::")

wald_cat_t_ctrl
wald_rel_t_ctrl
wald_cou_t_ctrl

modelsummary(
  list(
    "Catholic (T bins)"                        = lpm_fe_t_nocontrols_cat,
    "Catholic (T bins) + Controls"             = lpm_fe_t_controls_cat,
    "Religious practice (T bins)"              = lpm_fe_t_nocontrols_rel,
    "Religious practice (T bins) + Controls"   = lpm_fe_t_controls_rel,
    "Couple catholic (T bins)"                 = lpm_fe_t_nocontrols_cou,
    "Couple catholic (T bins) + Controls"      = lpm_fe_t_controls_cou
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Catholic (T bins)`                      = "No",
    `Catholic (T bins) + Controls`           = "Yes",
    `Religious practice (T bins)`            = "No",
    `Religious practice (T bins) + Controls` = "Yes",
    `Couple catholic (T bins)`               = "No",
    `Couple catholic (T bins) + Controls`    = "Yes"
  )
)

# Political outcomes: PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT


# PARTICIPATION
lpm_fe_t_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# CONSERVATIVE VOTE
lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# LEFT-RIGHT SCALE
lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


lpm_fe_t_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Joint tests (political outcomes)
# H0: treat_t::2 = treat_t::3 = 0
wald_par_t_ctrl <- wald(lpm_fe_t_controls_par, keep = "treat_t::")
wald_con_t_ctrl <- wald(lpm_fe_t_controls_con, keep = "treat_t::")
wald_lr_t_ctrl <- wald(lpm_fe_t_controls_lr, keep = "treat_t::")


wald_par_t_ctrl
wald_con_t_ctrl
wald_lr_t_ctrl


modelsummary(
  list(
    "Participation (T bins)" = lpm_fe_t_nocontrols_par,
    "Participation (T bins) + Controls" = lpm_fe_t_controls_par,
    "Conservative (T bins)" = lpm_fe_t_nocontrols_con,
    "Conservative (T bins) + Controls" = lpm_fe_t_controls_con,
    "Left-right (T bins)" = lpm_fe_t_nocontrols_lr,
    "Left-right (T bins) + Controls" = lpm_fe_t_controls_lr
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Participation (T bins)` = "No",
    `Participation (T bins) + Controls` = "Yes",
    `Conservative (T bins)` = "No",
    `Conservative (T bins) + Controls` = "Yes",
    `Left-right (T bins)` = "No",
    `Left-right (T bins) + Controls` = "Yes"
  )
)



# Extract coefficients and CIs
religious_models <- list(
  Catholic           = lpm_fe_t_controls_cat,
  ReligiousPractice  = lpm_fe_t_controls_rel,
  CoupleCatholic     = lpm_fe_t_controls_cou
)

coef_df <- purrr::map_dfr(
  religious_models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )

# Plot
ggplot(coef_df, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies)",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()

political_models <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

coef_df <- purrr::map_dfr(
  political_models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )


# Plot
ggplot(coef_df, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies)",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()


# Step 1: Extract coefficients from religious models (with controls)

religious_models_t <- list(
  Catholic           = lpm_fe_t_controls_cat,
  ReligiousPractice  = lpm_fe_t_controls_rel,
  CoupleCatholic     = lpm_fe_t_controls_cou
)

coef_religious_t <- map_dfr(
  religious_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1"),
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(.$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )


# Step 2: Repeat for political models

political_models_t <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

coef_political_t <- map_dfr(
  political_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1"),
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome = unique(.$Outcome),
      treat_level = 1,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  )


# Step 3: Plot ADRF-style graphs


# Religious Outcomes ADRF Plot
ggplot(coef_religious_t, aes(x = treat_level, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(group = Outcome)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 1:3, labels = paste0("T", 1:3)) +
  labs(
    title = "Approximate Dose Response Function (Religious Outcomes)",
    x = "Treatment Tertile",
    y = "Estimated Effect (vs T1)"
  ) +
  theme_minimal()

# Political Outcomes ADRF Plot
ggplot(coef_political_t, aes(x = treat_level, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(group = Outcome)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 1:3, labels = paste0("T", 1:3)) +
  labs(
    title = "Approximate Dose Response Function (Political Outcomes)",
    x = "Treatment Tertile",
    y = "Estimated Effect (vs T1)"
  ) +
  theme_minimal()


# 1) OTHER outcomes regressions 

# INCOME
lpm_fe_t_nocontrols_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  +
    survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)


# 2) Joint tests (OTHER outcomes): H0: T2 = T3 = 0 

wald_inc_t_ctrl   <- wald(lpm_fe_t_controls_inc,   keep = "treat_t::")
wald_edu_t_ctrl   <- wald(lpm_fe_t_controls_edu,   keep = "treat_t::")
wald_trust_t_ctrl <- wald(lpm_fe_t_controls_trust, keep = "treat_t::")

wald_inc_t_ctrl
wald_edu_t_ctrl
wald_trust_t_ctrl


# 3) Table (OTHER outcomes) 

modelsummary(
  list(
    "Income (T bins)"                  = lpm_fe_t_nocontrols_inc,
    "Income (T bins) + Controls"       = lpm_fe_t_controls_inc,
    "Education (T bins)"               = lpm_fe_t_nocontrols_edu,
    "Education (T bins) + Controls"    = lpm_fe_t_controls_edu,
    "Trust people (T bins)"            = lpm_fe_t_nocontrols_trust,
    "Trust people (T bins) + Controls" = lpm_fe_t_controls_trust
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "T2 vs T1",
    "treat_t::3" = "T3 vs T1"
  ),
  add_rows = tibble(
    term = "Controls",
    `Income (T bins)`                  = "No",
    `Income (T bins) + Controls`       = "Yes",
    `Education (T bins)`               = "No",
    `Education (T bins) + Controls`    = "Yes",
    `Trust people (T bins)`            = "No",
    `Trust people (T bins) + Controls` = "Yes"
  )
)


# 4) Plot: coefficients + 95% CI (OTHER outcomes) 

library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

other_models_t <- list(
  Income      = lpm_fe_t_controls_inc,
  Education   = lpm_fe_t_controls_edu,
  TrustPeople = lpm_fe_t_controls_trust
)

coef_other_t <- map_dfr(
  other_models_t,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Outcome"
) %>%
  filter(grepl("^treat_t::", term)) %>%
  mutate(
    Tertile = recode(term,
                     "treat_t::2" = "T2 vs T1",
                     "treat_t::3" = "T3 vs T1")
  )

ggplot(coef_other_t, aes(x = Tertile, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Childhood Dry Days (Tertile Dummies) on Other Outcomes",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal()


# 5) Optional: ADRF-style plot (OTHER outcomes) 

adrf_data_other_t <- coef_other_t %>%
  mutate(
    treat_level = case_when(
      Tertile == "T2 vs T1" ~ 2,
      Tertile == "T3 vs T1" ~ 3
    )
  ) %>%
  select(Outcome, treat_level, estimate, conf.low, conf.high) %>%
  bind_rows(
    tibble(
      Outcome     = unique(coef_other_t$Outcome),
      treat_level = 1,
      estimate    = 0,
      conf.low    = 0,
      conf.high   = 0
    )
  )

ggplot(adrf_data_other_t, aes(x = treat_level, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(group = Outcome)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 1:3, labels = paste0("T", 1:3)) +
  labs(
    title = "Approximate Dose Response Function (Other Outcomes)",
    x = "Treatment Tertile",
    y = "Estimated Effect (vs T1)"
  ) +
  theme_minimal()


# Quartile + Tertile coefficient plots (CONTROLS spec)
# Works for raw bins (like your example) OR residualized bins

library(dplyr)
library(fixest)
library(broom)
library(purrr)
library(ggplot2)


# 1) Choose treatment to bin (controls-residualized)

treat_var <- "treat_resid_ctrl_std"

# Use a clean sample for binning (avoid NA bins)
model_bins <- model_data %>%
  filter(!is.na(.data[[treat_var]])) %>%
  mutate(
    treat_q = ntile(.data[[treat_var]], 4),   # 1..4
    treat_t = ntile(.data[[treat_var]], 3)    # 1..3
  )

table(model_bins$treat_q, useNA = "ifany")
table(model_bins$treat_t, useNA = "ifany")

# 2) Controls + FE spec (match your example)

ctrl_terms <- "FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year"
fe_terms   <- "BIRTH + prov_nac"

fit_bins_controls <- function(data, y, binvar){
  fml <- as.formula(paste0(y, " ~ i(", binvar, ", ref = 1) + ", ctrl_terms, " | ", fe_terms))
  feols(fml, data = data, cluster = ~ prov_nac)
}


# 3) Run models + tidy results

religious_outcomes <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
political_outcomes <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
other_outcomes     <- c("INCOME", "EDUCATION", "TRUST_PEOPLE")

run_tidy_bins <- function(data, outcomes, binvar, binprefix){
  mods <- setNames(lapply(outcomes, function(y) fit_bins_controls(data, y, binvar)), outcomes)
  
  map_dfr(mods, ~ tidy(.x, conf.int = TRUE), .id = "Outcome") %>%
    filter(grepl(paste0("^", binprefix, "::"), term)) %>%
    mutate(
      Level = case_when(
        term == paste0(binprefix, "::2") ~ paste0(substr(binprefix, nchar(binprefix), nchar(binprefix)), "2 vs ", substr(binprefix, nchar(binprefix), nchar(binprefix)), "1"),
        term == paste0(binprefix, "::3") ~ paste0(substr(binprefix, nchar(binprefix), nchar(binprefix)), "3 vs ", substr(binprefix, nchar(binprefix), nchar(binprefix)), "1"),
        term == paste0(binprefix, "::4") ~ paste0(substr(binprefix, nchar(binprefix), nchar(binprefix)), "4 vs ", substr(binprefix, nchar(binprefix), nchar(binprefix)), "1"),
        TRUE ~ term
      ),
      treat_level = as.integer(gsub(paste0(binprefix, "::"), "", term))
    )
}

# Quartiles
coef_religious_q <- run_tidy_bins(model_bins, religious_outcomes, "treat_q", "treat_q")
coef_political_q <- run_tidy_bins(model_bins, political_outcomes, "treat_q", "treat_q")
coef_other_q     <- run_tidy_bins(model_bins, other_outcomes,     "treat_q", "treat_q")

# Tertiles
coef_religious_t <- run_tidy_bins(model_bins, religious_outcomes, "treat_t", "treat_t")
coef_political_t <- run_tidy_bins(model_bins, political_outcomes, "treat_t", "treat_t")
coef_other_t     <- run_tidy_bins(model_bins, other_outcomes,     "treat_t", "treat_t")


# 4) Coefficient plots (pointrange)

plot_pointrange <- function(df, title){
  ggplot(df, aes(x = Level, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    facet_wrap(~ Outcome, scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title, x = NULL, y = "Coefficient (95% CI)") +
    theme_minimal()
}

plot_pointrange(coef_religious_q,
                paste0("Quartile dummies of ", treat_var, " (controls + FE): Religious outcomes"))
plot_pointrange(coef_political_q,
                paste0("Quartile dummies of ", treat_var, " (controls + FE): Political outcomes"))
plot_pointrange(coef_other_q,
                paste0("Quartile dummies of ", treat_var, " (controls + FE): Other outcomes"))

plot_pointrange(coef_religious_t,
                paste0("Tertile dummies of ", treat_var, " (controls + FE): Religious outcomes"))
plot_pointrange(coef_political_t,
                paste0("Tertile dummies of ", treat_var, " (controls + FE): Political outcomes"))
plot_pointrange(coef_other_t,
                paste0("Tertile dummies of ", treat_var, " (controls + FE): Other outcomes"))


# 5) ADRF-style plots (include baseline bin at zero)

make_adrf <- function(df, max_level){
  df0 <- df %>% distinct(Outcome) %>%
    mutate(treat_level = 1, estimate = 0, conf.low = 0, conf.high = 0)
  
  out <- bind_rows(
    df %>% select(Outcome, treat_level, estimate, conf.low, conf.high),
    df0
  ) %>% arrange(Outcome, treat_level)
  
  # keep only 1..max_level (safety)
  out %>% filter(treat_level >= 1, treat_level <= max_level)
}

plot_adrf <- function(df_adrf, title, labels){
  ggplot(df_adrf, aes(x = treat_level, y = estimate)) +
    geom_line(aes(group = Outcome)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
    facet_wrap(~ Outcome, scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq_along(labels), labels = labels) +
    labs(title = title, x = "Treatment bin", y = "Effect vs baseline bin") +
    theme_minimal()
}

adrf_religious_q <- make_adrf(coef_religious_q, 4)
adrf_political_q <- make_adrf(coef_political_q, 4)
adrf_other_q     <- make_adrf(coef_other_q, 4)

plot_adrf(adrf_religious_q,
          paste0("Approx. dose-response (quartiles of ", treat_var, ", controls + FE): Religious"),
          labels = paste0("Q", 1:4))
plot_adrf(adrf_political_q,
          paste0("Approx. dose-response (quartiles of ", treat_var, ", controls + FE): Political"),
          labels = paste0("Q", 1:4))
plot_adrf(adrf_other_q,
          paste0("Approx. dose-response (quartiles of ", treat_var, ", controls + FE): Other"),
          labels = paste0("Q", 1:4))

adrf_religious_t <- make_adrf(coef_religious_t, 3)
adrf_political_t <- make_adrf(coef_political_t, 3)
adrf_other_t     <- make_adrf(coef_other_t, 3)

plot_adrf(adrf_religious_t,
          paste0("Approx. dose-response (tertiles of ", treat_var, ", controls + FE): Religious"),
          labels = paste0("T", 1:3))
plot_adrf(adrf_political_t,
          paste0("Approx. dose-response (tertiles of ", treat_var, ", controls + FE): Political"),
          labels = paste0("T", 1:3))
plot_adrf(adrf_other_t,
          paste0("Approx. dose-response (tertiles of ", treat_var, ", controls + FE): Other"),
          labels = paste0("T", 1:3))



# Regressions with age-of-exposure variables --------------------------------

# Catholic identification
lpm_age_hetero_cat <- feols(
  CATHOLIC ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Religious practice
lpm_age_hetero_rel <- feols(
  RELIGIOUS_PRACTICE ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Couple Catholic
lpm_age_hetero_cou <- feols(
  COUPLE_CATHOLIC ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Participation
lpm_age_hetero_par <- feols(
  PARTICIPATION ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Conservative vote
lpm_age_hetero_con <- feols(
  CONSERVATIVE_VOTE ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Left-right self-placement
lpm_age_hetero_lr <- feols(
  LEFT_RIGHT ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# Export LaTeX summary of all outcomes (age heterogeneity)

modelsummary(
  list(
    "Catholic"       = lpm_age_hetero_cat,
    "Practice"       = lpm_age_hetero_rel,
    "Couple Catholic"= lpm_age_hetero_cou,
    "Participation"  = lpm_age_hetero_par,
    "Conservative"   = lpm_age_hetero_con,
    "Left-right"     = lpm_age_hetero_lr
  ),
  title = "Linear probability models with age-of-exposure treatment heterogeneity",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "dry_days_5_9"   = "Dry Days Age 5–9",
    "dry_days_10_14" = "Dry Days Age 10–14",
    "dry_days_15_18" = "Dry Days Age 15–18"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    Catholic         = "Yes",
    Practice         = "Yes",
    `Couple Catholic`= "Yes",
    Participation    = "Yes",
    Conservative     = "Yes",
    `Left-right`     = "Yes"
  )
)



# Robustness: Clustering standard errors by Province*Birth ----------------


# Linear Probability Models: CATHOLIC 


# 1. With FE, No controls
lpm_fe_nocontrols_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)| BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN  + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: RELIGIOUS PRACTICE 

# 1. With FE, No controls
lpm_fe_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +
    MOTHER_EMPLOYMENT| BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: COUPLE_CATHOLIC


# 1. With FE, No controls
lpm_fe_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN  + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)



# Model summary: Linear (Standardized), single Controls indicator


modelsummary(
  list(
    "Catholic"                        = lpm_fe_nocontrols_cat,
    "Catholic + Controls"             = lpm_fe_controls_cat,
    "Religious practice"              = lpm_fe_nocontrols_rel,
    "Religious practice + Controls"   = lpm_fe_controls_rel,
    "Couple Catholic"                 = lpm_fe_nocontrols_cou,
    "Couple Catholic + Controls"      = lpm_fe_controls_cou
  ),
  title = "LPM Catholic identification, OLS Religious attendance and LPM Couple being catholic",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",   # survey_year always included
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Catholic` = "No",
    `Catholic + Controls` = "Yes",
    `Religious practice` = "No",
    `Religious practice + Controls` = "Yes",
    `Couple Catholic` = "No",
    `Couple Catholic + Controls` = "Yes"
  )
)



# Linear Probability Models: PARTICIPATION


# 1. With FE, No controls
lpm_fe_nocontrols_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + survey_year  + I(childhood_total_dry_days_std^2)    | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN  + MOTHER_EMPLOYMENT + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: CONSERVATIVE_VOTE


# 1. With FE, No controls
lpm_fe_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2)   | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: LEFT_RIGHT


# 1. With FE, No controls
lpm_fe_nocontrols_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2) | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) +
    FATHER_BORN_SPAIN  + 
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT + survey_year  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~birth_prov_cluster
)



# Model summary: Linear (Standardized), single Controls indicator

modelsummary(
  list(
    "Participation"                   = lpm_fe_nocontrols_par,
    "Participation + Controls"        = lpm_fe_controls_par,
    "Conservative"                    = lpm_fe_nocontrols_con,
    "Conservative + Controls"         = lpm_fe_controls_con,
    "Left or Right"                       = lpm_fe_nocontrols_far,
    "Left or Right"            = lpm_fe_controls_far
  ),
  title = "LPM: Participation in last election, probability voting conservative and ideological positioning",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Participation` = "No",
    `Participation + Controls` = "Yes",
    `Conservative` = "No",
    `Conservative + Controls` = "Yes",
    `Far-right` = "No",
    `Far-right + Controls` = "Yes"
  )
)





# Regressions different weather age: quartiles/tertiles + plots -----------

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(broom)
library(stringr)
library(purrr)
library(tidyr)

# Load once
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

# Outcomes to run (same family as your main regressions)
outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")

all_outcomes <- c(outcomes_relig, outcomes_pol)

# Controls (as in your FE + controls specs)
controls <- c("FEMALE", "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
              "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT", "survey_year")

# Weather exposures to iterate over
# (add/remove as you want; must exist in survey file)
treat_vars <- c("dry_days_last_5", "dry_days_0_4", "dry_days_5_9",
                "dry_days_10_14", "dry_days_15_18", "dry_days_8_18")

# Labels for plots/tables
treat_labels <- c(
  dry_days_last_5 = "Dry days (last 5y)",
  dry_days_0_4    = "Dry days (0–4)",
  dry_days_5_9    = "Dry days (5–9)",
  dry_days_10_14  = "Dry days (10–14)",
  dry_days_15_18  = "Dry days (15–18)",
  dry_days_8_18   = "Dry days (8–18)"
)

# Create output folders
dir.create("weather_age_outputs", showWarnings = FALSE)
dir.create("weather_age_outputs/plots", showWarnings = FALSE)
dir.create("weather_age_outputs/tables", showWarnings = FALSE)

# Helper: build model data for a given treatment variable

prep_data <- function(df, treat) {
  df %>%
    filter(
      BORN_SPAIN == 1,
      !is.na(.data[[treat]]),
      .data[[treat]] != 0
    ) %>%
    mutate(
      # standardized treatment
      treat_std = (.data[[treat]] - mean(.data[[treat]], na.rm = TRUE)) /
        sd(.data[[treat]],  na.rm = TRUE),
      treat_q = ntile(treat_std, 4),
      treat_t = ntile(treat_std, 3)
    )
}


# Helper: run models for one outcome and one treatment
#   - continuous + quadratic (as in main)
#   - quartile ADRF
#   - tertile ADRF

run_models <- function(df, y, cluster_var = "prov_nac") {
  
  # continuous + quadratic
  f_cont <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + ",
    paste(controls, collapse = " + "),
    " | BIRTH + prov_nac"
  ))
  
  # quartile ADRF (baseline = Q1)
  f_q <- as.formula(paste0(
    y, " ~ factor(treat_q) + ",
    paste(controls, collapse = " + "),
    " | BIRTH + prov_nac"
  ))
  
  # tertile ADRF (baseline = T1)
  f_t <- as.formula(paste0(
    y, " ~ factor(treat_t) + ",
    paste(controls, collapse = " + "),
    " | BIRTH + prov_nac"
  ))
  
  list(
    cont = feols(f_cont, data = df, cluster = as.formula(paste0("~", cluster_var))),
    q    = feols(f_q,    data = df, cluster = as.formula(paste0("~", cluster_var))),
    t    = feols(f_t,    data = df, cluster = as.formula(paste0("~", cluster_var)))
  )
}


# Helper: coefficient plot for ADRF models (quartile/tertile)

plot_adrf <- function(mod, which = c("q", "t"), title, subtitle, file) {
  which <- match.arg(which)
  
  td <- broom::tidy(mod, conf.int = TRUE)
  
  if (which == "q") {
    # keep factor(treat_q) terms only
    td <- td %>% filter(str_detect(term, "^factor\\(treat_q\\)"))
    # relabel Q2..Q4
    td <- td %>%
      mutate(
        bin = str_extract(term, "\\d+") %>% as.integer(),
        bin_lab = paste0("Q", bin),
        bin_num = bin
      ) %>%
      arrange(bin_num)
  } else {
    td <- td %>% filter(str_detect(term, "^factor\\(treat_t\\)"))
    td <- td %>%
      mutate(
        bin = str_extract(term, "\\d+") %>% as.integer(),
        bin_lab = paste0("T", bin),
        bin_num = bin
      ) %>%
      arrange(bin_num)
  }
  
  # If model drops bins due to collinearity, td might be empty — handle gracefully.
  if (nrow(td) == 0) return(invisible(NULL))
  
  p <- ggplot(td, aes(x = bin_lab, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Coefficient (relative to baseline bin)"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(filename = file, plot = p, width = 7.2, height = 4.6, dpi = 300)
  p
}


# MAIN LOOP: treatment window × outcomes


all_results <- list()

for (treat in treat_vars) {
  
  message("=== Running weather window: ", treat, " ===")
  
  dat <- prep_data(survey, treat)
  
  # safety: ensure sufficient variation
  if (nrow(dat) < 500) {
    message("Too few observations for ", treat, " (n=", nrow(dat), "). Skipping.")
    next
  }
  
  treat_lab <- ifelse(treat %in% names(treat_labels), treat_labels[[treat]], treat)
  
  # Run outcomes
  mods <- list()
  for (y in all_outcomes) {
    if (!y %in% names(dat)) next
    mods[[y]] <- run_models(dat, y)
  }
  
  # --- TABLES (latex) ---
  # Continuous + quadratic (controls) for all outcomes
  cont_list <- lapply(mods, `[[`, "cont")
  q_list    <- lapply(mods, `[[`, "q")
  t_list    <- lapply(mods, `[[`, "t")
  
  # Save LaTeX tables (one per treatment window)
  modelsummary(
    cont_list,
    title = paste0("Weather exposure: ", treat_lab, " — Continuous (std) + quadratic, FE + controls"),
    output = file.path("weather_age_outputs/tables", paste0("cont_", treat, ".tex")),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)|survey_year"
  )
  
  modelsummary(
    q_list,
    title = paste0("Weather exposure: ", treat_lab, " — Quartile ADRF, FE + controls"),
    output = file.path("weather_age_outputs/tables", paste0("quartiles_", treat, ".tex")),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)|survey_year"
  )
  
  modelsummary(
    t_list,
    title = paste0("Weather exposure: ", treat_lab, " — Tertile ADRF, FE + controls"),
    output = file.path("weather_age_outputs/tables", paste0("tertiles_", treat, ".tex")),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)|survey_year"
  )
  
  # --- PLOTS ---
  # Produce plots for each outcome (quartiles + tertiles)
  for (y in names(mods)) {
    
    # Quartile ADRF plot
    plot_adrf(
      mod = mods[[y]]$q,
      which = "q",
      title = paste0(y, " — Quartile ADRF"),
      subtitle = paste0(treat_lab, " (baseline = Q1), FE: birth year + province; controls included"),
      file = file.path("weather_age_outputs/plots", paste0("Q_", treat, "_", y, ".png"))
    )
    
    # Tertile ADRF plot
    plot_adrf(
      mod = mods[[y]]$t,
      which = "t",
      title = paste0(y, " — Tertile ADRF"),
      subtitle = paste0(treat_lab, " (baseline = T1), FE: birth year + province; controls included"),
      file = file.path("weather_age_outputs/plots", paste0("T_", treat, "_", y, ".png"))
    )
  }
  
  all_results[[treat]] <- mods
}

# Optional: keep results in memory for inspection
saveRDS(all_results, file = "weather_age_outputs/all_models_weather_age.rds")


# Regressions adding flexibility and female interaction -------------------------------------------


# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         SAME_LOC_BIRTH == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION)


# Standardize Treatment Variable


model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) / sd(childhood_total_dry_days, na.rm = TRUE)
  )


# Helper: safe modelsummary

safe_modelsummary <- function(mod_list, title, out_file, keep_regex) {
  
  kept <- lapply(mod_list, function(m) {
    cn <- names(coef(m))
    if (any(stringr::str_detect(cn, keep_regex))) m else NULL
  })
  kept <- kept[!vapply(kept, is.null, logical(1))]
  
  if (length(kept) == 0) {
    message("No kept coefficients for: ", out_file, " (regex: ", keep_regex, ")")
    return(invisible(NULL))
  }
  
  modelsummary(
    kept,
    title = title,
    output = out_file,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_keep = keep_regex,
    gof_omit  = "AIC|BIC|RMSE|R2|Within|Pseudo|Std.Errors"
  )
}


# Helper: plot key coefs
#   - for continuous model: plot treat_std and its interactions
#   - for ADRF: plot bin effects and female interactions

plot_terms <- function(mod, keep_regex, title, subtitle, file) {
  
  td <- broom::tidy(mod, conf.int = TRUE) %>%
    filter(str_detect(term, keep_regex))
  
  if (nrow(td) == 0) return(invisible(NULL))
  
  # nicer labels
  td <- td %>%
    mutate(term = str_replace_all(term, "factor\\(treat_q\\)", "Q")) %>%
    mutate(term = str_replace_all(term, "factor\\(treat_t\\)", "T")) %>%
    mutate(term = str_replace_all(term, ":FEMALE", " × Female")) %>%
    mutate(term = str_replace_all(term, "I\\(treat_std\\^2\\)", "treat_std^2"))
  
  p <- ggplot(td, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Coefficient") +
    theme_minimal(base_size = 13)
  
  ggsave(file, p, width = 8.5, height = 5.2, dpi = 300)
  p
}

dir.create("flex_female_outputs", showWarnings = FALSE)
dir.create("flex_female_outputs/plots", showWarnings = FALSE)
dir.create("flex_female_outputs/tables", showWarnings = FALSE)





# If your standardized variable is named childhood_total_dry_days_std, map it:
model_data <- model_data %>%
  mutate(
    treat_std = childhood_total_dry_days_std,
    treat_q   = ntile(treat_std, 4),
    treat_t   = ntile(treat_std, 3)
  )

# Outcomes (NO FAR_RIGHT_VOTE)
outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
all_outcomes   <- c(outcomes_relig, outcomes_pol)

controls <- c("FEMALE", "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
              "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT", "survey_year")


# A) Continuous flexible model with FEMALE interaction
#    (treat + treat^2) × FEMALE


run_cont_models <- function(y, with_controls = TRUE) {
  
  rhs_main <- if (with_controls) paste(controls, collapse = " + ") else "survey_year + FEMALE"
  
  # Explicitly include: treat_std + treat_std^2 + FEMALE + interactions
  f <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + FEMALE + treat_std:FEMALE + I(treat_std^2):FEMALE + ",
    rhs_main,
    " | BIRTH + prov_nac"
  ))
  
  feols(f, data = model_data, cluster = ~prov_nac)
}

cont_nc <- lapply(all_outcomes, run_cont_models, with_controls = FALSE)
names(cont_nc) <- all_outcomes

cont_c <- lapply(all_outcomes, run_cont_models, with_controls = TRUE)
names(cont_c) <- all_outcomes

# Tables (keep only treat/female interaction terms)
safe_modelsummary(
  cont_nc,
  "Flexible (treat + treat^2) × Female — NO controls (FE: birth + province)",
  "flex_female_outputs/tables/cont_nocontrols.tex",
  keep_regex = "treat_std|I\\(treat_std\\^2\\)|FEMALE|:FEMALE"
)

safe_modelsummary(
  cont_c,
  "Flexible (treat + treat^2) × Female — WITH controls (FE: birth + province)",
  "flex_female_outputs/tables/cont_controls.tex",
  keep_regex = "treat_std|I\\(treat_std\\^2\\)|FEMALE|:FEMALE"
)

# Plots for continuous models (one plot per outcome, controls-spec)
for (y in all_outcomes) {
  plot_terms(
    cont_c[[y]],
    keep_regex = "treat_std|I\\(treat_std\\^2\\)|treat_std:FEMALE|I\\(treat_std\\^2\\):FEMALE",
    title = paste0(y, " — Flexible female interaction (controls)"),
    subtitle = "Shows treat, treat^2, and interactions with Female",
    file = file.path("flex_female_outputs/plots", paste0("cont_controls_", y, ".png"))
  )
}


# B) Quartile ADRF × FEMALE (bin effects differ by gender)


run_q_models <- function(y, with_controls = TRUE) {
  rhs <- if (with_controls) paste(controls, collapse = " + ") else "survey_year + FEMALE"
  f <- as.formula(paste0(
    y, " ~ factor(treat_q) * FEMALE + ", rhs, " | BIRTH + prov_nac"
  ))
  feols(f, data = model_data, cluster = ~prov_nac)
}

q_nc <- lapply(all_outcomes, run_q_models, with_controls = FALSE)
names(q_nc) <- all_outcomes

q_c <- lapply(all_outcomes, run_q_models, with_controls = TRUE)
names(q_c) <- all_outcomes

safe_modelsummary(
  q_nc,
  "Quartile ADRF × Female — NO controls (baseline: Q1 male)",
  "flex_female_outputs/tables/quartiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_q\\)|:FEMALE|^FEMALE$"
)

safe_modelsummary(
  q_c,
  "Quartile ADRF × Female — WITH controls (baseline: Q1 male)",
  "flex_female_outputs/tables/quartiles_controls.tex",
  keep_regex = "^factor\\(treat_q\\)|:FEMALE|^FEMALE$"
)

for (y in all_outcomes) {
  plot_terms(
    q_c[[y]],
    keep_regex = "^factor\\(treat_q\\)|:FEMALE",
    title = paste0(y, " — Quartile ADRF × Female (controls)"),
    subtitle = "Bin effects and bin×Female interaction terms",
    file = file.path("flex_female_outputs/plots", paste0("quartiles_controls_", y, ".png"))
  )
}


# C) Tertile ADRF × FEMALE (bin effects differ by gender)


run_t_models <- function(y, with_controls = TRUE) {
  rhs <- if (with_controls) paste(controls, collapse = " + ") else "survey_year + FEMALE"
  f <- as.formula(paste0(
    y, " ~ factor(treat_t) * FEMALE + ", rhs, " | BIRTH + prov_nac"
  ))
  feols(f, data = model_data, cluster = ~prov_nac)
}

t_nc <- lapply(all_outcomes, run_t_models, with_controls = FALSE)
names(t_nc) <- all_outcomes

t_c <- lapply(all_outcomes, run_t_models, with_controls = TRUE)
names(t_c) <- all_outcomes

safe_modelsummary(
  t_nc,
  "Tertile ADRF × Female — NO controls (baseline: T1 male)",
  "flex_female_outputs/tables/tertiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_t\\)|:FEMALE|^FEMALE$"
)

safe_modelsummary(
  t_c,
  "Tertile ADRF × Female — WITH controls (baseline: T1 male)",
  "flex_female_outputs/tables/tertiles_controls.tex",
  keep_regex = "^factor\\(treat_t\\)|:FEMALE|^FEMALE$"
)

for (y in all_outcomes) {
  plot_terms(
    t_c[[y]],
    keep_regex = "^factor\\(treat_t\\)|:FEMALE",
    title = paste0(y, " — Tertile ADRF × Female (controls)"),
    subtitle = "Bin effects and bin×Female interaction terms",
    file = file.path("flex_female_outputs/plots", paste0("tertiles_controls_", y, ".png"))
  )
}



# Regressions with people living same province -------------------------------------------


# Load the data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")


# Prepare model data
model_data <- survey %>%
  filter(BORN_SPAIN == 1,
         SAME_LOC_BIRTH == 1,
         !is.na(childhood_total_dry_days),
         childhood_total_dry_days != 0) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
                FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
                FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
                MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION)


# Standardize Treatment Variable


model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) / sd(childhood_total_dry_days, na.rm = TRUE)
  )


# Regressions with people living same province (SAME_LOC_BIRTH=1)
# - Continuous (std + quadratic)
# - Quartile ADRF
# - Tertile ADRF
# - Tables + plots
# - NO FAR_RIGHT_VOTE



dir.create("same_prov_outputs", showWarnings = FALSE)
dir.create("same_prov_outputs/tables", showWarnings = FALSE)
dir.create("same_prov_outputs/plots", showWarnings = FALSE)


# Helper: safe modelsummary

safe_modelsummary <- function(mod_list, title, out_file, keep_regex) {
  
  kept <- lapply(mod_list, function(m) {
    cn <- names(coef(m))
    if (any(stringr::str_detect(cn, keep_regex))) m else NULL
  })
  kept <- kept[!vapply(kept, is.null, logical(1))]
  
  if (length(kept) == 0) {
    message("No kept coefficients for: ", out_file, " (regex: ", keep_regex, ")")
    return(invisible(NULL))
  }
  
  modelsummary(
    kept,
    title = title,
    output = out_file,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_keep = keep_regex,
    gof_omit  = "AIC|BIC|RMSE|R2|Within|Pseudo|Std.Errors"
  )
}


# Helper: coefficient plotter

plot_terms <- function(mod, keep_regex, title, subtitle, file) {
  
  td <- broom::tidy(mod, conf.int = TRUE) %>%
    filter(str_detect(term, keep_regex))
  
  if (nrow(td) == 0) return(invisible(NULL))
  
  td <- td %>%
    mutate(term = str_replace_all(term, "factor\\(treat_q\\)", "Q")) %>%
    mutate(term = str_replace_all(term, "factor\\(treat_t\\)", "T")) %>%
    mutate(term = str_replace_all(term, "I\\(treat_std\\^2\\)", "treat_std^2"))
  
  p <- ggplot(td, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Coefficient") +
    theme_minimal(base_size = 13)
  
  ggsave(file, p, width = 8.5, height = 5.2, dpi = 300)
  p
}


# Load + restrict sample

survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

model_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    SAME_LOC_BIRTH == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    treat_raw = childhood_total_dry_days,
    treat_std = (treat_raw - mean(treat_raw, na.rm = TRUE)) / sd(treat_raw, na.rm = TRUE),
    # bins (jitter only to avoid tie/pathological ntile issues)
    treat_for_bins = treat_raw + rnorm(n(), 0, 1e-8),
    treat_q = ntile(treat_for_bins, 4),
    treat_t = ntile(treat_for_bins, 3)
  ) %>%
  # IMPORTANT: drop FAR_RIGHT_VOTE from selection
  dplyr::select(
    CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC,
    PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT,
    treat_raw, treat_std, treat_q, treat_t,
    survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC,
    TRUST_PEOPLE, INST_CONFIDENCE, PUBLIC_SECTOR_EMP,
    MERITOCRACY_BELIEF, SUBJECTIVE_CLASS
  )

outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
all_outcomes   <- c(outcomes_relig, outcomes_pol)

controls <- c(
  "FEMALE", "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT", "survey_year",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

# A) Continuous: treat_std + treat_std^2

run_cont <- function(y, with_controls = TRUE) {
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  f <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + ", rhs_ctrl, " | BIRTH + prov_nac"
  ))
  
  feols(f, data = model_data, cluster = ~prov_nac)
}

cont_nc <- lapply(all_outcomes, run_cont, with_controls = FALSE)
names(cont_nc) <- all_outcomes
cont_c  <- lapply(all_outcomes, run_cont, with_controls = TRUE)
names(cont_c) <- all_outcomes

safe_modelsummary(
  cont_nc,
  "Same province (SAME_LOC_BIRTH=1): Continuous (std + quadratic) — NO controls",
  "same_prov_outputs/tables/cont_nocontrols.tex",
  keep_regex = "^(treat_std|I\\(treat_std\\^2\\))$"
)

safe_modelsummary(
  cont_c,
  "Same province (SAME_LOC_BIRTH=1): Continuous (std + quadratic) — WITH controls",
  "same_prov_outputs/tables/cont_controls.tex",
  keep_regex = "^(treat_std|I\\(treat_std\\^2\\))$"
)

for (y in all_outcomes) {
  plot_terms(
    cont_c[[y]],
    keep_regex = "treat_std|I\\(treat_std\\^2\\)",
    title = paste0(y, " — Continuous (controls), SAME_LOC_BIRTH=1"),
    subtitle = "treat_std and treat_std^2; FE: birth year + province",
    file = file.path("same_prov_outputs/plots", paste0("cont_controls_", y, ".png"))
  )
}


# B) Quartile ADRF


run_q <- function(y, with_controls = TRUE) {
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  f <- as.formula(paste0(
    y, " ~ factor(treat_q) + ", rhs_ctrl, " | BIRTH + prov_nac"
  ))
  
  feols(f, data = model_data, cluster = ~prov_nac)
}

q_nc <- lapply(all_outcomes, run_q, with_controls = FALSE)
names(q_nc) <- all_outcomes
q_c  <- lapply(all_outcomes, run_q, with_controls = TRUE)
names(q_c) <- all_outcomes

safe_modelsummary(
  q_nc,
  "Same province (SAME_LOC_BIRTH=1): Quartile ADRF — NO controls (baseline Q1)",
  "same_prov_outputs/tables/quartiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_q\\)"
)

safe_modelsummary(
  q_c,
  "Same province (SAME_LOC_BIRTH=1): Quartile ADRF — WITH controls (baseline Q1)",
  "same_prov_outputs/tables/quartiles_controls.tex",
  keep_regex = "^factor\\(treat_q\\)"
)

for (y in all_outcomes) {
  plot_terms(
    q_c[[y]],
    keep_regex = "^factor\\(treat_q\\)",
    title = paste0(y, " — Quartile ADRF (controls), SAME_LOC_BIRTH=1"),
    subtitle = "Baseline is Q1; FE: birth year + province",
    file = file.path("same_prov_outputs/plots", paste0("quartiles_controls_", y, ".png"))
  )
}


# C) Tertile ADRF

run_t <- function(y, with_controls = TRUE) {
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  f <- as.formula(paste0(
    y, " ~ factor(treat_t) + ", rhs_ctrl, " | BIRTH + prov_nac"
  ))
  
  feols(f, data = model_data, cluster = ~prov_nac)
}

t_nc <- lapply(all_outcomes, run_t, with_controls = FALSE)
names(t_nc) <- all_outcomes
t_c  <- lapply(all_outcomes, run_t, with_controls = TRUE)
names(t_c) <- all_outcomes

safe_modelsummary(
  t_nc,
  "Same province (SAME_LOC_BIRTH=1): Tertile ADRF — NO controls (baseline T1)",
  "same_prov_outputs/tables/tertiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_t\\)"
)

safe_modelsummary(
  t_c,
  "Same province (SAME_LOC_BIRTH=1): Tertile ADRF — WITH controls (baseline T1)",
  "same_prov_outputs/tables/tertiles_controls.tex",
  keep_regex = "^factor\\(treat_t\\)"
)

for (y in all_outcomes) {
  plot_terms(
    t_c[[y]],
    keep_regex = "^factor\\(treat_t\\)",
    title = paste0(y, " — Tertile ADRF (controls), SAME_LOC_BIRTH=1"),
    subtitle = "Baseline is T1; FE: birth year + province",
    file = file.path("same_prov_outputs/plots", paste0("tertiles_controls_", y, ".png"))
  )
}


# Heterogeneity: North vs South -------------------------------------------


library(dplyr)
library(fixest)
library(modelsummary)
library(broom)

# --- 1) Define South provinces (if not already defined) -----

if (!exists("south_prov_nac")) {
  # Andalucía: 04, 11, 14, 18, 21, 23, 29, 41
  # Extremadura: 06, 10
  # Murcia: 30
  south_prov_nac <- c(4, 11, 14, 18, 21, 23, 29, 41, 6, 10, 30)
}

# --- 2) Pooled model data with North/South dummy -----------

model_data_ns <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    south = as.integer(prov_nac %in% south_prov_nac),         # 1 = South, 0 = North
    year  = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    # Standardize treatment in the full sample
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days,  na.rm = TRUE)
  ) %>%
  # Quartiles / tertiles of standardized treatment in the pooled sample
  mutate(
    treat_q = ntile(childhood_total_dry_days_std, 4),  # 1 = lowest, 4 = highest
    treat_t = ntile(childhood_total_dry_days_std, 3)   # 1 = low, 3 = high
  )

# Quick check: counts by North/South and bins
model_data_ns %>%
  count(south, treat_q, name = "n_q") %>%
  arrange(south, treat_q) %>%
  print()

model_data_ns %>%
  count(south, treat_t, name = "n_t") %>%
  arrange(south, treat_t) %>%
  print()


# 2A. Quartile heterogeneity: North vs South (with controls)

# NOTE:
# factor(treat_q) * south = factor(treat_q) + south + factor(treat_q):south
# Baseline: Q1 in the North (treat_q == 1, south == 0)

# -------- Religious outcomes (quartiles × South) -----------

lpm_fe_q_controls_cat_ns <- feols(
  CATHOLIC ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_q_controls_rel_ns <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_q_controls_cou_ns <- feols(
  COUPLE_CATHOLIC ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

# Joint test: ADRF equal in North vs South (religious outcomes)
wald_q_cat_ns <- wald(
  lpm_fe_q_controls_cat_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)
wald_q_rel_ns <- wald(
  lpm_fe_q_controls_rel_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)
wald_q_cou_ns <- wald(
  lpm_fe_q_controls_cou_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)

wald_q_cat_ns
wald_q_rel_ns
wald_q_cou_ns

modelsummary(
  list(
    "Catholic – Q × South"           = lpm_fe_q_controls_cat_ns,
    "Religious practice – Q × South" = lpm_fe_q_controls_rel_ns,
    "Couple Catholic – Q × South"    = lpm_fe_q_controls_cou_ns
  ),
  title = "Heterogeneity North vs South – Quartile ADRF (religious outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# -------- Political outcomes (quartiles × South) -----------

lpm_fe_q_controls_par_ns <- feols(
  PARTICIPATION ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_q_controls_con_ns <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_q_controls_lr_ns <- feols(
  LEFT_RIGHT ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

wald_q_par_ns <- wald(
  lpm_fe_q_controls_par_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)
wald_q_con_ns <- wald(
  lpm_fe_q_controls_con_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)
wald_q_lr_ns <- wald(
  lpm_fe_q_controls_lr_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)

wald_q_par_ns
wald_q_con_ns
wald_q_lr_ns

modelsummary(
  list(
    "Participation – Q × South" = lpm_fe_q_controls_par_ns,
    "Conservative – Q × South"  = lpm_fe_q_controls_con_ns,
    "Left-right – Q × South"    = lpm_fe_q_controls_lr_ns
  ),
  title = "Heterogeneity North vs South – Quartile ADRF (political outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)


# 2B. Tertile heterogeneity: North vs South (with controls)

# NOTE:
# Baseline: T1 in the North (treat_t == 1, south == 0)

# -------- Religious outcomes (tertiles × South) ------------

lpm_fe_t_controls_cat_ns <- feols(
  CATHOLIC ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_t_controls_rel_ns <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_t_controls_cou_ns <- feols(
  COUPLE_CATHOLIC ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

wald_t_cat_ns <- wald(
  lpm_fe_t_controls_cat_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)
wald_t_rel_ns <- wald(
  lpm_fe_t_controls_rel_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)
wald_t_cou_ns <- wald(
  lpm_fe_t_controls_cou_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)

wald_t_cat_ns
wald_t_rel_ns
wald_t_cou_ns

modelsummary(
  list(
    "Catholic – T × South"           = lpm_fe_t_controls_cat_ns,
    "Religious practice – T × South" = lpm_fe_t_controls_rel_ns,
    "Couple Catholic – T × South"    = lpm_fe_t_controls_cou_ns
  ),
  title = "Heterogeneity North vs South – Tertile ADRF (religious outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# -------- Political outcomes (tertiles × South) ------------

lpm_fe_t_controls_par_ns <- feols(
  PARTICIPATION ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_t_controls_con_ns <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

lpm_fe_t_controls_lr_ns <- feols(
  LEFT_RIGHT ~ factor(treat_t) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~prov_nac
)

wald_t_par_ns <- wald(
  lpm_fe_t_controls_par_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)
wald_t_con_ns <- wald(
  lpm_fe_t_controls_con_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)
wald_t_lr_ns <- wald(
  lpm_fe_t_controls_lr_ns,
  "factor(treat_t)2:south = 0 & factor(treat_t)3:south = 0"
)

wald_t_par_ns
wald_t_con_ns
wald_t_lr_ns

modelsummary(
  list(
    "Participation – T × South" = lpm_fe_t_controls_par_ns,
    "Conservative – T × South"  = lpm_fe_t_controls_con_ns,
    "Left-right – T × South"    = lpm_fe_t_controls_lr_ns
  ),
  title = "Heterogeneity North vs South – Tertile ADRF (political outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)


# PLOTS: North vs South ADRF (print to console, no files)
#   - For each outcome: Quartile ADRF + Tertile ADRF
#   - Baseline = Q1/T1 in the North (south == 0)
# Folder to save figures
out_dir <- "north_south_plots"
dir.create(out_dir, showWarnings = FALSE)

plot_adrf_ns_ci <- function(mod, which = c("q","t"),
                            title = "", subtitle = "",
                            file = NULL, level = 0.95) {
  which <- match.arg(which)
  k <- if (which == "q") 4 else 3
  bin_label <- if (which == "q") "Q" else "T"
  fac_name  <- if (which == "q") "factor(treat_q)" else "factor(treat_t)"
  
  beta <- coef(mod)
  V <- vcov(mod)  # clustered vcov already baked in given cluster= in feols
  
  # scalar-safe getter
  getb1 <- function(nm) if (!is.na(nm) && nm %in% names(beta)) unname(beta[[nm]]) else 0
  
  # delta method for linear combination a' beta
  lincomb <- function(terms, weights) {
    # keep only terms that exist in model
    ok <- terms %in% names(beta)
    terms_ok <- terms[ok]
    w_ok <- weights[ok]
    
    est <- sum(w_ok * beta[terms_ok])
    
    if (length(terms_ok) == 0) {
      return(list(est = 0, se = NA_real_))
    }
    
    Vsub <- V[terms_ok, terms_ok, drop = FALSE]
    var <- as.numeric(t(w_ok) %*% Vsub %*% w_ok)
    se <- sqrt(pmax(var, 0))
    
    list(est = est, se = se)
  }
  
  z <- qnorm(1 - (1 - level) / 2)
  
  df <- expand.grid(
    group = c("North", "South"),
    bin = 1:k
  ) |>
    as_tibble() |>
    mutate(
      bin_name = paste0(bin_label, bin),
      
      # Build the linear combination for each point:
      # North bin1: 0
      # North binj: factor(...)j
      # South bin1: south
      # South binj: south + factor(...)j + factor(...)j:south
      comb = pmap(list(group, bin), function(g, j) {
        if (g == "North" && j == 1) {
          return(list(terms = character(0), w = numeric(0)))
        }
        if (g == "North" && j != 1) {
          return(list(terms = c(paste0(fac_name, j)), w = c(1)))
        }
        if (g == "South" && j == 1) {
          return(list(terms = c("south"), w = c(1)))
        }
        # South & j != 1
        return(list(
          terms = c("south", paste0(fac_name, j), paste0(fac_name, j, ":south")),
          w     = c(1, 1, 1)
        ))
      }),
      
      est_se = map(comb, ~ lincomb(.x$terms, .x$w)),
      y = map_dbl(est_se, "est"),
      se = map_dbl(est_se, "se"),
      ci_low  = y - z * se,
      ci_high = y + z * se
    ) |>
    select(group, bin, bin_name, y, se, ci_low, ci_high)
  
  p <- ggplot(df, aes(x = bin, y = y, color = group, fill = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:k, labels = paste0(bin_label, 1:k)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = if (which == "q") "Treatment quartile (baseline = Q1 North)" else "Treatment tertile (baseline = T1 North)",
      y = paste0("Estimated level relative to baseline (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)
  
  if (!is.null(file)) {
    ggsave(filename = file, plot = p, width = 7.2, height = 4.6, dpi = 300)
  }
  
  invisible(p)
}

# ---- Run and save all plots ----
mods_ns <- list(
  Catholic            = list(q = lpm_fe_q_controls_cat_ns, t = lpm_fe_t_controls_cat_ns),
  Religious_practice  = list(q = lpm_fe_q_controls_rel_ns, t = lpm_fe_t_controls_rel_ns),
  Couple_catholic     = list(q = lpm_fe_q_controls_cou_ns, t = lpm_fe_t_controls_cou_ns),
  Participation       = list(q = lpm_fe_q_controls_par_ns, t = lpm_fe_t_controls_par_ns),
  Conservative_vote   = list(q = lpm_fe_q_controls_con_ns, t = lpm_fe_t_controls_con_ns),
  Left_right          = list(q = lpm_fe_q_controls_lr_ns, t = lpm_fe_t_controls_lr_ns)
)

for (y in names(mods_ns)) {
  plot_adrf_ns_ci(
    mod = mods_ns[[y]]$q,
    which = "q",
    title = paste0(y, " — Quartile ADRF (North vs South)"),
    subtitle = "Baseline: Q1 in North; FE: birth year + province; controls included",
    file = file.path(out_dir, paste0("Q_NorthSouth_", y, "_CI.png"))
  )
  
  plot_adrf_ns_ci(
    mod = mods_ns[[y]]$t,
    which = "t",
    title = paste0(y, " — Tertile ADRF (North vs South)"),
    subtitle = "Baseline: T1 in North; FE: birth year + province; controls included",
    file = file.path(out_dir, paste0("T_NorthSouth_", y, "_CI.png"))
  )
}

# Heterogeneity: Number of cofradias --------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(httr2)
library(jsonlite)
library(fixest)
library(modelsummary)

# You already have these from your pipeline:
# - normalize_name()
# - prov_code_map: prov_nac + provincia_official + provincia_norm
# - survey: your merged survey_with_childhood_weather_harmonized.csv loaded


# A) Scrape cofradiasyhermandades localidad pages -> province totals

dir.create("cofradias_cache", showWarnings = FALSE)

fetch_localidad <- function(cc, sleep = 0.25) {
  cache_file <- file.path("cofradias_cache", paste0("cc_", cc, ".html"))
  
  if (file.exists(cache_file)) {
    html <- read_file(cache_file)
  } else {
    url <- paste0(
      "https://www.cofradiasyhermandades.es/fichalocalidad.php?b=1&cc=", cc,
      "&g=1&g0=1&g1=1&g2=1&g3=1&g4=1&g5=1&g6=1&n=1&p=1&r=1&s=1"
    )
    req <- request(url) |>
      req_user_agent("research-academic/1.0 (contact: you@uni.edu)") |>
      req_timeout(30)
    
    resp <- try(req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error") || resp_status(resp) >= 400) return(NULL)
    
    html <- resp_body_string(resp)
    write_file(html, cache_file)
    Sys.sleep(sleep)
  }
  
  # Must contain province and the cofradías count line
  if (!str_detect(html, "provincia de") || !str_detect(html, "COFRAD")) return(NULL)
  
  prov <- str_match(html, "provincia de\\s+([^<\\n\\r]+)")[,2] |> str_squish()
  ncof <- str_match(html, "COFRAD[ÍI]AS Y HERMANDADES\\s*\\|\\s*(\\d+)")[,2] |> as.integer()
  
  if (is.na(prov) || is.na(ncof)) return(NULL)
  
  tibble(cc = cc, provincia_raw = prov, cofradias_localidad = ncof)
}

# Choose an upper bound for cc.
# We know some valid ones are in the thousands (e.g., Madrid has cc=6902) so start with 10000.
cc_max <- 10000
raw_loc <- map_dfr(1:cc_max, fetch_localidad)

prov_cofr <- raw_loc %>%
  mutate(provincia_norm = normalize_name(provincia_raw)) %>%
  group_by(provincia_norm) %>%
  summarise(cofradias_total = sum(cofradias_localidad, na.rm = TRUE),
            n_localidades = n(),
            .groups = "drop") %>%
  left_join(prov_code_map, by = "provincia_norm") %>%   # adds provincia_official, prov_nac
  filter(!is.na(prov_nac))

# B) INE population by province (latest year) via API table 2852

# INE Tempus API: table 2852. nult=1 returns latest period.
# We'll keep Total sex and Total (both sexes) if available.
ine_url <- "https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2852?nult=1"

ine_raw <- fromJSON(paste(readLines(ine_url, warn = FALSE), collapse = ""))

# The API structure is a list of records with fields like Nombre (dimensions) and Valor
ine_pop <- tibble(
  Nombre = ine_raw$Nombre,
  Valor  = as.numeric(gsub(",", ".", ine_raw$Valor)),  # just in case decimals/format
  Periodo = ine_raw$Periodo
) %>%
  # keep "Total" sex series if present
  # (INE often encodes dimension names inside Nombre; we filter robustly)
  filter(str_detect(Nombre, "Total")) %>%
  # extract province label from Nombre (works for "Albacete. Total. Total" style strings)
  mutate(
    provincia = str_trim(str_extract(Nombre, "^[^\\.]+"))
  ) %>%
  group_by(provincia) %>%
  summarise(pop = sum(Valor, na.rm = TRUE),  # should be one row per province already
            .groups = "drop") %>%
  mutate(provincia_norm = normalize_name(provincia)) %>%
  left_join(prov_code_map, by = "provincia_norm") %>%
  select(prov_nac, provincia_official, pop) %>%
  filter(!is.na(prov_nac))


# C) Province index: cofradías per 100k + log index, Top 20 provinces

prov_index <- prov_cofr %>%
  left_join(ine_pop, by = "prov_nac") %>%
  mutate(
    cofr_per_100k = 1e5 * cofradias_total / pop,
    ss_importance_idx = log1p(cofr_per_100k)
  ) %>%
  arrange(desc(ss_importance_idx))

top20_prov_nac <- prov_index %>%
  slice_head(n = 20) %>%
  pull(prov_nac)

print(prov_index %>% slice_head(n = 25) %>% select(prov_nac, provincia_official, cofradias_total, pop, cofr_per_100k, ss_importance_idx))


# D) Run your heterogeneity regressions (Top20 dummy)

model_data_ns <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    high_cofr = as.integer(prov_nac %in% top20_prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days,  na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4),
    treat_t = ntile(childhood_total_dry_days_std, 3)
  )

# Quartiles × high_cofr (religious outcomes) 
lpm_fe_q_controls_cat <- feols(
  CATHOLIC ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

lpm_fe_q_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

lpm_fe_q_controls_cou <- feols(
  COUPLE_CATHOLIC ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

# Joint test: heterogeneity across quartiles
wald(lpm_fe_q_controls_cat,
     "factor(treat_q)2:high_cofr = 0 & factor(treat_q)3:high_cofr = 0 & factor(treat_q)4:high_cofr = 0")

modelsummary(
  list(
    "Catholic – Q × Top20(cofr/pop)"           = lpm_fe_q_controls_cat,
    "Religious practice – Q × Top20(cofr/pop)" = lpm_fe_q_controls_rel,
    "Couple Catholic – Q × Top20(cofr/pop)"    = lpm_fe_q_controls_cou
  ),
  title = "Heterogeneity by cofradías per person (Top 20 provinces) – Quartile ADRF (religious outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# Quartiles × high_cofr (political outcomes) 
lpm_fe_q_controls_par <- feols(
  PARTICIPATION ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

lpm_fe_q_controls_con <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

lpm_fe_q_controls_lr <- feols(
  LEFT_RIGHT ~ factor(treat_q) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

modelsummary(
  list(
    "Participation – Q × Top20(cofr/pop)" = lpm_fe_q_controls_par,
    "Conservative – Q × Top20(cofr/pop)"  = lpm_fe_q_controls_con,
    "Left-right – Q × Top20(cofr/pop)"    = lpm_fe_q_controls_lr
  ),
  title = "Heterogeneity by cofradías per person (Top 20 provinces) – Quartile ADRF (political outcomes)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# Tertiles × high_cofr (optional; mirror your old block)
lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ factor(treat_t) * high_cofr +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year |
    BIRTH + prov_nac,
  data = model_data_ns, cluster = ~prov_nac
)

modelsummary(
  list("Catholic – T × Top20(cofr/pop)" = lpm_fe_t_controls_cat),
  title = "Heterogeneity by cofradías per person (Top 20 provinces) – Tertile ADRF (example)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# Regressions: flexibility + BOTH parents Catholic interaction
#   PARENTS_CATHOLIC = 1{father catholic=1 AND mother catholic=1}

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(stringr)
library(ggplot2)

# Load data
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

# Prepare model data (same sample restriction as your block)
model_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    SAME_LOC_BIRTH == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    
    # Standardize treatment
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days,  na.rm = TRUE),
    
    # NEW interaction dummy
    PARENTS_CATHOLIC = as.integer(FATHER_CATHOLIC == 1 & MOTHER_CATHOLIC == 1)
  ) %>%
  dplyr::select(
    CATHOLIC, childhood_total_dry_days, childhood_total_dry_days_std,
    survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC,
    PARENTS_CATHOLIC,
    birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE,
    PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS,
    CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION
  )

# Helpers (same as yours)
safe_modelsummary <- function(mod_list, title, out_file, keep_regex) {
  kept <- lapply(mod_list, function(m) {
    cn <- names(coef(m))
    if (any(stringr::str_detect(cn, keep_regex))) m else NULL
  })
  kept <- kept[!vapply(kept, is.null, logical(1))]
  
  if (length(kept) == 0) {
    message("No kept coefficients for: ", out_file, " (regex: ", keep_regex, ")")
    return(invisible(NULL))
  }
  
  modelsummary(
    kept,
    title = title,
    output = out_file,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_keep = keep_regex,
    gof_omit  = "AIC|BIC|RMSE|R2|Within|Pseudo|Std.Errors"
  )
}

plot_terms <- function(mod, keep_regex, title, subtitle, file) {
  
  td <- broom::tidy(mod, conf.int = TRUE) %>%
    filter(str_detect(term, keep_regex))
  
  if (nrow(td) == 0) return(invisible(NULL))
  
  td <- td %>%
    mutate(term = str_replace_all(term, "factor\\(treat_q\\)", "Q")) %>%
    mutate(term = str_replace_all(term, "factor\\(treat_t\\)", "T")) %>%
    mutate(term = str_replace_all(term, ":PARENTS_CATHOLIC", " × ParentsCatholic")) %>%
    mutate(term = str_replace_all(term, "I\\(treat_std\\^2\\)", "treat_std^2"))
  
  p <- ggplot(td, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Coefficient") +
    theme_minimal(base_size = 13)
  
  ggsave(file, p, width = 8.5, height = 5.2, dpi = 300)
  p
}

# Output folders
dir.create("flex_parents_outputs", showWarnings = FALSE)
dir.create("flex_parents_outputs/plots", showWarnings = FALSE)
dir.create("flex_parents_outputs/tables", showWarnings = FALSE)

# Map treatment bins
model_data <- model_data %>%
  mutate(
    treat_std = childhood_total_dry_days_std,
    treat_q   = ntile(treat_std + rnorm(n(), 0, 1e-8), 4),
    treat_t   = ntile(treat_std + rnorm(n(), 0, 1e-8), 3)
  )

# Outcomes (NO FAR_RIGHT_VOTE)
outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
all_outcomes   <- c(outcomes_relig, outcomes_pol)

# Controls (keep your baseline controls; DO NOT include FATHER_CATHOLIC/MOTHER_CATHOLIC since we interact them)
controls <- c("FEMALE", "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
              "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT", "survey_year")


# A) Continuous flexible model with PARENTS_CATHOLIC interaction
#     (treat + treat^2) × PARENTS_CATHOLIC

run_cont_models_par <- function(y, with_controls = TRUE) {
  
  rhs_main <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  
  f <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + PARENTS_CATHOLIC + ",
    "treat_std:PARENTS_CATHOLIC + I(treat_std^2):PARENTS_CATHOLIC + ",
    rhs_main,
    " | BIRTH + prov_nac"
  ))
  
  feols(f, data = model_data, cluster = ~prov_nac)
}

cont_nc <- lapply(all_outcomes, run_cont_models_par, with_controls = FALSE)
names(cont_nc) <- all_outcomes

cont_c <- lapply(all_outcomes, run_cont_models_par, with_controls = TRUE)
names(cont_c) <- all_outcomes

safe_modelsummary(
  cont_nc,
  "Flexible (treat + treat^2) × ParentsCatholic — NO controls (FE: birth + province)",
  "flex_parents_outputs/tables/cont_nocontrols.tex",
  keep_regex = "treat_std|I\\(treat_std\\^2\\)|PARENTS_CATHOLIC|:PARENTS_CATHOLIC"
)

safe_modelsummary(
  cont_c,
  "Flexible (treat + treat^2) × ParentsCatholic — WITH controls (FE: birth + province)",
  "flex_parents_outputs/tables/cont_controls.tex",
  keep_regex = "treat_std|I\\(treat_std\\^2\\)|PARENTS_CATHOLIC|:PARENTS_CATHOLIC"
)

for (y in all_outcomes) {
  plot_terms(
    cont_c[[y]],
    keep_regex = "treat_std|I\\(treat_std\\^2\\)|treat_std:PARENTS_CATHOLIC|I\\(treat_std\\^2\\):PARENTS_CATHOLIC",
    title = paste0(y, " — Flexible interaction (ParentsCatholic, controls)"),
    subtitle = "Shows treat, treat^2, and interactions with ParentsCatholic",
    file = file.path("flex_parents_outputs/plots", paste0("cont_controls_", y, ".png"))
  )
}


# B) Quartile ADRF × PARENTS_CATHOLIC

run_q_models_par <- function(y, with_controls = TRUE) {
  rhs <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  f <- as.formula(paste0(
    y, " ~ factor(treat_q) * PARENTS_CATHOLIC + ", rhs, " | BIRTH + prov_nac"
  ))
  feols(f, data = model_data, cluster = ~prov_nac)
}

q_nc <- lapply(all_outcomes, run_q_models_par, with_controls = FALSE)
names(q_nc) <- all_outcomes

q_c <- lapply(all_outcomes, run_q_models_par, with_controls = TRUE)
names(q_c) <- all_outcomes

safe_modelsummary(
  q_nc,
  "Quartile ADRF × ParentsCatholic — NO controls (baseline: Q1 & ParentsCatholic=0)",
  "flex_parents_outputs/tables/quartiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_q\\)|:PARENTS_CATHOLIC|^PARENTS_CATHOLIC$"
)

safe_modelsummary(
  q_c,
  "Quartile ADRF × ParentsCatholic — WITH controls (baseline: Q1 & ParentsCatholic=0)",
  "flex_parents_outputs/tables/quartiles_controls.tex",
  keep_regex = "^factor\\(treat_q\\)|:PARENTS_CATHOLIC|^PARENTS_CATHOLIC$"
)

for (y in all_outcomes) {
  plot_terms(
    q_c[[y]],
    keep_regex = "^factor\\(treat_q\\)|:PARENTS_CATHOLIC",
    title = paste0(y, " — Quartile ADRF × ParentsCatholic (controls)"),
    subtitle = "Bin effects and bin×ParentsCatholic interaction terms",
    file = file.path("flex_parents_outputs/plots", paste0("quartiles_controls_", y, ".png"))
  )
}

# C) Tertile ADRF × PARENTS_CATHOLIC

run_t_models_par <- function(y, with_controls = TRUE) {
  rhs <- if (with_controls) paste(controls, collapse = " + ") else "survey_year"
  f <- as.formula(paste0(
    y, " ~ factor(treat_t) * PARENTS_CATHOLIC + ", rhs, " | BIRTH + prov_nac"
  ))
  feols(f, data = model_data, cluster = ~prov_nac)
}

t_nc <- lapply(all_outcomes, run_t_models_par, with_controls = FALSE)
names(t_nc) <- all_outcomes

t_c <- lapply(all_outcomes, run_t_models_par, with_controls = TRUE)
names(t_c) <- all_outcomes

safe_modelsummary(
  t_nc,
  "Tertile ADRF × ParentsCatholic — NO controls (baseline: T1 & ParentsCatholic=0)",
  "flex_parents_outputs/tables/tertiles_nocontrols.tex",
  keep_regex = "^factor\\(treat_t\\)|:PARENTS_CATHOLIC|^PARENTS_CATHOLIC$"
)

safe_modelsummary(
  t_c,
  "Tertile ADRF × ParentsCatholic — WITH controls (baseline: T1 & ParentsCatholic=0)",
  "flex_parents_outputs/tables/tertiles_controls.tex",
  keep_regex = "^factor\\(treat_t\\)|:PARENTS_CATHOLIC|^PARENTS_CATHOLIC$"
)

for (y in all_outcomes) {
  plot_terms(
    t_c[[y]],
    keep_regex = "^factor\\(treat_t\\)|:PARENTS_CATHOLIC",
    title = paste0(y, " — Tertile ADRF × ParentsCatholic (controls)"),
    subtitle = "Bin effects and bin×ParentsCatholic interaction terms",
    file = file.path("flex_parents_outputs/plots", paste0("tertiles_controls_", y, ".png"))
  )
}
