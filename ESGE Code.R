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
library(data.table)  

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
    "Castellon","Castello","Castellon/Castello","Castellón/Castelló",
    "Coruña, A",
    "Rioja, La",
    "Palmas, Las"
  ),
  ine_name = c(
    rep("Araba/Álava", 3), "Bizkaia","Gipuzkoa",
    "Illes Balears","Illes Balears",
    "Valencia/València","Valencia/València",
    "Santa Cruz de Tenerife","Santa Cruz de Tenerife",
    rep("Alicante/Alacant", 3),
    rep("Castellón/Castelló", 4),
    
    "A Coruña",
    "La Rioja",
    "Las Palmas"
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


# POPULATION LOADING (FIXED for your INE excel structure)


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
      year = suppressWarnings(as.integer(stringr::str_extract(as.character(year), "\\d{4}"))),
      population = clean_pop_num(population),
      
      # strip leading INE numeric code
      provincia_name = stringr::str_trim(stringr::str_remove(provincia, "^\\d{1,2}\\s+")),
      
      # map aliases -> official names (same as rain)
      key = normalize_name(provincia_name)
    ) %>%
    left_join(name_map, by = "key") %>%
    mutate(
      provincia_official = dplyr::coalesce(ine_name, provincia_name),
      provincia_norm = normalize_name(provincia_official)
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
      year = suppressWarnings(as.integer(stringr::str_extract(as.character(year), "\\d{4}"))),
      population = clean_pop_num(population),
      
      provincia_name = stringr::str_trim(stringr::str_remove(provincia, "^\\d{1,2}\\s+")),
      key = normalize_name(provincia_name)
    ) %>%
    left_join(name_map, by = "key") %>%
    mutate(
      provincia_official = dplyr::coalesce(ine_name, provincia_name),
      provincia_norm = normalize_name(provincia_official)
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


# what province codes exist in the canonical map?
all_prov <- prov_code_map %>%
  distinct(prov_nac, provincia_official, provincia_norm) %>%
  arrange(prov_nac)

# what province codes exist in population panel?
pop_prov <- pop_panel %>%
  distinct(prov_nac) %>%
  arrange(prov_nac)

# which are missing from pop_panel?
missing_from_pop <- anti_join(all_prov, pop_prov, by = "prov_nac")

missing_from_pop
nrow(missing_from_pop)


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
          P15M %in% c(1,2) ~ 1,
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
          P14M %in% c(1,2) ~ 1,
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
          P43J %in% c(1,2) ~ 1,
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
          P42J %in% c(1,2) ~ 1,
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
          P27J %in% c(1,2) ~ 1,
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
          P26J %in% c(1,2) ~ 1,
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
          RECUERDO %in% c(2, 6, 9, 10) ~ 1,
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
            RECUERDO %in% c(1, 3, 7, 9) ~ 1,
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
            RECUERDO %in% c(1, 3, 7, 9) ~ 1,
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
    share_cov_lt80  = mean(coverage_ratio < 0.9, na.rm = TRUE),
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
  filter(BIRTH >= 1920
         , BIRTH <= 2008)

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



# ROBUSTNESS: Balance of observables across treatment quartiles --------------------

library(dplyr)
library(tidyr)
library(fixest)
library(kableExtra)

# --- Pretty labels (same as plot) ---
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


# 2) Means by quartile (raw)

means_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Q1 = mean_by_q(model_data[[v]], model_data$treat_q, 1),
    Q2 = mean_by_q(model_data[[v]], model_data$treat_q, 2),
    Q3 = mean_by_q(model_data[[v]], model_data$treat_q, 3),
    Q4 = mean_by_q(model_data[[v]], model_data$treat_q, 4)
  )
}) %>% bind_rows()


# 3) Std diffs vs Q1 (raw)

smd_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Raw_Q2vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 2),
    Raw_Q3vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 3),
    Raw_Q4vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 4)
  )
}) %>% bind_rows()


# BALANCE PLOT + BALANCE TABLE (consistent definitions)


# 0) Load + build analysis sample

survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

model_data <- survey %>%
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
  "FEMALE","age","EDUCATION","INCOME",
  "FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT","MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL","MOTHER_SCHOOL",
  "FATHER_CATHOLIC","MOTHER_CATHOLIC",
  "survey_year","pop_birth_last_census","SAME_LOC_BIRTH"
)
balance_vars <- balance_vars[balance_vars %in% names(model_data)]

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


# 1) PLOT: Absolute SMD Q1 vs Q4 (Raw vs After FE)

std_diff_q1_q4_abs <- function(x, q) {
  x1 <- x[q == 1]
  x4 <- x[q == 4]
  m1 <- mean(x1, na.rm = TRUE)
  m4 <- mean(x4, na.rm = TRUE)
  s  <- sqrt((var(x1, na.rm = TRUE) + var(x4, na.rm = TRUE)) / 2)
  if (is.na(s) || s == 0) return(NA_real_)
  abs((m4 - m1) / s)
}

raw_smd <- sapply(balance_vars, function(v) {
  std_diff_q1_q4_abs(model_data[[v]], model_data$treat_q)
})

fe_smd <- sapply(balance_vars, function(v) {
  m <- feols(as.formula(paste0(v, " ~ 1 | BIRTH + prov_nac")), data = model_data)
  v_resid <- residuals(m)
  std_diff_q1_q4_abs(v_resid, model_data$treat_q)
})

balance_plot <- tibble(
  variable = balance_vars,
  Raw      = raw_smd,
  `After FE (Birth year + province)` = fe_smd
) %>%
  pivot_longer(-variable, names_to = "spec", values_to = "smd") %>%
  mutate(
    variable = recode(variable, !!!pretty_labels),
    variable = factor(variable, levels = rev(pretty_labels[balance_vars])),
    spec = factor(spec, levels = c("Raw", "After FE (Birth year + province)"))
  )

p_balance <- ggplot(balance_plot, aes(x = smd, y = variable, color = spec, shape = spec)) +
  geom_point(size = 2.8) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
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

# optional: export figure for LaTeX
ggsave("balance_plot.png", p_balance, width = 9, height = 5, dpi = 300)


# 2) TABLE: Means Q1–Q4 + Std diff vs Q1 (Raw + After FE)

mean_by_q <- function(x, q, k) mean(x[q == k], na.rm = TRUE)

std_diff_vs_q1 <- function(x, q, k) {
  x1 <- x[q == 1]
  xk <- x[q == k]
  m1 <- mean(x1, na.rm = TRUE)
  mk <- mean(xk, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE)
  if (is.na(s1) || s1 == 0) return(NA_real_)
  (mk - m1) / s1
}

means_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Q1 = mean_by_q(model_data[[v]], model_data$treat_q, 1),
    Q2 = mean_by_q(model_data[[v]], model_data$treat_q, 2),
    Q3 = mean_by_q(model_data[[v]], model_data$treat_q, 3),
    Q4 = mean_by_q(model_data[[v]], model_data$treat_q, 4)
  )
}) %>% bind_rows()

smd_raw_tbl <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Raw_Q2vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 2),
    Raw_Q3vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 3),
    Raw_Q4vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 4)
  )
}) %>% bind_rows()

smd_fe_tbl <- lapply(balance_vars, function(v) {
  fe_mod <- feols(as.formula(paste0(v, " ~ 1 | BIRTH + prov_nac")), data = model_data)
  x_res  <- residuals(fe_mod)
  
  tibble(
    variable = v,
    FE_Q2vsQ1 = std_diff_vs_q1(x_res, model_data$treat_q, 2),
    FE_Q3vsQ1 = std_diff_vs_q1(x_res, model_data$treat_q, 3),
    FE_Q4vsQ1 = std_diff_vs_q1(x_res, model_data$treat_q, 4)
  )
}) %>% bind_rows()

tab <- means_raw %>%
  left_join(smd_raw_tbl, by = "variable") %>%
  left_join(smd_fe_tbl,  by = "variable") %>%
  mutate(
    variable = recode(variable, !!!pretty_labels),
    variable = factor(variable, levels = pretty_labels[balance_vars])
  ) %>%
  arrange(variable) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  mutate(variable = as.character(variable)) %>%
  rename(Variable = variable)

n_by_q <- model_data %>% count(treat_q) %>% arrange(treat_q) %>% pull(n)

means_lab <- sprintf(
  "Means by quartile (n = %d, %d, %d, %d)",
  n_by_q[1], n_by_q[2], n_by_q[3], n_by_q[4]
)

header_vec <- c(
  " " = 1,
  setNames(4, means_lab),
  "Std. diff vs Q1 (Raw)" = 3,
  "Std. diff vs Q1 (After FE)" = 3
)

balance_table_latex <- kbl(
  tab,
  format   = "latex",
  booktabs = TRUE,
  align    = "lrrrrrrr",
  caption  = paste(
    "Balance of observables across quartiles of childhood dry days.",
    "Columns Q1--Q4 report raw means by treatment quartile.",
    "Columns 'Std. diff vs Q1 (Raw)' and 'Std. diff vs Q1 (After FE)' report",
    "standardized differences relative to Q1 using SD(Q1).",
    "After FE residualizes each covariate on birth-year and province fixed effects."
  )
) %>%
  add_header_above(header_vec) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

balance_table_latex


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
                MOTHER_EMPLOYMENT, INCOME, MOTHER_CATHOLIC, birth_prov_cluster, EDUCATION, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION, SIZE_TOWN, pop_birth_last_census, SAME_LOC_BIRTH)

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

library(dplyr)
library(tidyr)

na_balance_overall <- model_data %>%
  summarise(across(
    all_of(balance_vars),
    list(
      n_missing = ~ sum(is.na(.x)),
      share_missing = ~ mean(is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  mutate(
    value = ifelse(stat == "share_missing", round(100 * value, 2), value)
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(desc(n_missing))

print(na_balance_overall)


# =========================================================
# BALANCE TABLE: p-value of difference between Q1 and Q4
# Raw and After FE (Birth year + province)
# =========================================================

library(dplyr)
library(fixest)
library(kableExtra)
library(tibble)

# 1) Build the sample ONCE and consistently
survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

model_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    treat_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(treat_std, 4),
    q4 = if_else(treat_q == 4, 1, 0),
    q1 = if_else(treat_q == 1, 1, 0)
  ) %>%
  filter(treat_q %in% c(1, 4))

# 2) Covariates to test
balance_vars <- c(
  "FEMALE","age","EDUCATION","INCOME",
  "FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT","MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL","MOTHER_SCHOOL",
  "FATHER_CATHOLIC","MOTHER_CATHOLIC",
  "survey_year","pop_birth_last_census","SAME_LOC_BIRTH"
)
balance_vars <- balance_vars[balance_vars %in% names(model_data)]

# 3) Pretty labels
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
  pop_birth_last_census = "Province population at birth",
  SAME_LOC_BIRTH = "Same province at birth"
)

# 4) Helper function: extract Q4 vs Q1 p-value
get_q4_pvals <- function(v, data) {
  
  # keep non-missing sample for this variable
  d <- data %>%
    select(all_of(c(v, "q4", "BIRTH", "prov_nac"))) %>%
    filter(!is.na(.data[[v]]), !is.na(q4), !is.na(BIRTH), !is.na(prov_nac))
  
  # Raw difference: mean(Q4) - mean(Q1)
  m_raw <- feols(as.formula(paste0(v, " ~ q4")), data = d)
  p_raw <- coeftable(m_raw)["q4", "Pr(>|t|)"]
  diff_raw <- coef(m_raw)["q4"]
  
  # FE-residualized variable
  m_fe_resid <- feols(as.formula(paste0(v, " ~ 1 | BIRTH + prov_nac")), data = d)
  d$vresid <- resid(m_fe_resid)
  
  # Difference after FE
  m_fe <- feols(vresid ~ q4, data = d)
  p_fe <- coeftable(m_fe)["q4", "Pr(>|t|)"]
  diff_fe <- coef(m_fe)["q4"]
  
  tibble(
    variable = v,
    mean_q1 = mean(d[[v]][d$q4 == 0], na.rm = TRUE),
    mean_q4 = mean(d[[v]][d$q4 == 1], na.rm = TRUE),
    diff_raw = unname(diff_raw),
    p_raw = unname(p_raw),
    diff_fe = unname(diff_fe),
    p_fe = unname(p_fe),
    n_q1 = sum(d$q4 == 0),
    n_q4 = sum(d$q4 == 1)
  )
}

# 5) Run for all covariates
balance_pval_table <- lapply(balance_vars, get_q4_pvals, data = model_data) %>%
  bind_rows() %>%
  mutate(
    Variable = recode(variable, !!!pretty_labels),
    across(c(mean_q1, mean_q4, diff_raw, diff_fe), ~ round(.x, 3)),
    across(c(p_raw, p_fe), ~ round(.x, 3))
  ) %>%
  select(
    Variable,
    mean_q1,
    mean_q4,
    diff_raw,
    p_raw,
    diff_fe,
    p_fe,
    n_q1,
    n_q4
  )

balance_pval_table

# 6) Export to LaTeX
balance_pval_latex <- kbl(
  balance_pval_table,
  format = "latex",
  booktabs = TRUE,
  align = "lrrrrrrcc",
  caption = paste(
    "Balance of observables between the first and fourth quartiles of childhood dry days.",
    "Columns Mean Q1 and Mean Q4 report raw means in the lowest and highest treatment quartiles.",
    "Raw difference reports the unconditional difference in means (Q4 - Q1).",
    "After FE difference reports the difference after residualizing each covariate on birth-year and province fixed effects.",
    "P-values correspond to tests of equality between Q1 and Q4."
  ),
  col.names = c(
    "Variable", "Mean Q1", "Mean Q4",
    "Raw diff.", "p-value",
    "After FE diff.", "p-value",
    "N Q1", "N Q4"
  )
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

balance_pval_latex

# =========================================================
# PROVINCE-LEVEL SCATTER:
# Catholic share vs Conservative vote share
# Styled to match the presentation aesthetic
# =========================================================

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(stringr)

# Load final harmonized data
survey <- readr::read_csv("survey_with_childhood_weather_harmonized.csv")

# Build model sample for this descriptive province-level graph
plot_data <- survey %>%
  filter(BORN_SPAIN == 1) %>%
  mutate(prov_nac = sprintf("%02d", as.integer(prov_nac))) %>%
  filter(!is.na(CATHOLIC), !is.na(CONSERVATIVE_VOTE)) %>%
  filter(!prov_nac %in% c("51", "52"))   # drop Ceuta and Melilla if desired

# Province labels from your canonical province map
prov_labels <- prov_code_map %>%
  mutate(prov_nac = sprintf("%02d", as.integer(prov_nac))) %>%
  distinct(prov_nac, provincia_official)

# Province-level means
prov_summary <- plot_data %>%
  group_by(prov_nac) %>%
  summarise(
    catholic_share     = mean(CATHOLIC, na.rm = TRUE),
    conservative_share = mean(CONSERVATIVE_VOTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(prov_labels, by = "prov_nac") %>%
  mutate(
    provincia_official = case_when(
      provincia_official == "Araba/Álava"        ~ "Araba/Álava",
      provincia_official == "Valencia/València"  ~ "Valencia/València",
      provincia_official == "Alicante/Alacant"   ~ "Alicante/Alacant",
      provincia_official == "Castellón/Castelló" ~ "Castellón/Castelló",
      TRUE ~ provincia_official
    )
  )

# Optional: inspect
print(prov_summary)

# ---------------------------------------------------------
# Presentation-style theme
# ---------------------------------------------------------
theme_presentation_scatter <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = "#F2F2F2", color = NA),
      panel.background  = element_rect(fill = "#F2F2F2", color = NA),
      panel.grid.major  = element_line(color = "#D9D9D9", linewidth = 0.45),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(face = "bold", color = "black"),
      axis.text         = element_text(color = "black"),
      plot.title        = element_text(face = "bold", color = "#B22222", size = base_size + 4),
      plot.subtitle     = element_text(color = "black", size = base_size),
      plot.caption      = element_text(color = "black", size = base_size - 2),
      legend.position   = "none"
    )
}

p_prov_corr <- ggplot(
  prov_summary,
  aes(x = catholic_share, y = conservative_share)
) +
  geom_point(
    size = 3,
    color = "#3E7CB1",
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#B22222",
    fill = "grey80",
    linewidth = 1.1
  ) +
  ggrepel::geom_text_repel(
    aes(label = provincia_official),
    size = 3.2,
    fontface = "bold",
    color = "black",
    box.padding = 0.25,
    point.padding = 0.18,
    segment.color = NA,
    max.overlaps = Inf,
    seed = 1234
  ) +
  labs(
    x = "Share Identifying as Catholic",
    y = "Share Voting Conservative last election"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )


# Show plot
print(p_prov_corr)

# ---------------------------------------------------------
# Save in high quality
# ---------------------------------------------------------
ggsave(
  "province_catholic_conservative_scatter.png",
  p_prov_corr,
  width = 12,
  height = 7,
  dpi = 600
)
ggsave(
  "province_catholic_conservative_scatter.pdf",
  p_prov_corr,
  width = 12,
  height = 7,
  device = cairo_pdf
)
# Main regressions -------------------------------------------

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
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac)
  ) %>%
  dplyr::select(
    CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster,
    COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP,
    MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE,
    TRUST_PEOPLE, INCOME, EDUCATION, RELIGIOUS_PRACTICE, PARTICIPATION,
    SIZE_TOWN, dry_days_5_9, dry_days_10_14, dry_days_15_18, PP_VOTE,
    pop_birth_last_census
  )

model_data %>% count(BIRTH, name = "n") %>% arrange(BIRTH)

model_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  arrange(desc(n_na), variable) %>%
  print(n = Inf, width = Inf)

# Log population at birth
model_data <- model_data %>%
  mutate(
    log_pop_birth = log(pop_birth_last_census)
  )

# Standardize treatment
model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    childhood_total_dry_days_std_sq = childhood_total_dry_days_std^2
  )

# Helper: mean DV on the estimation sample

dv_mean_from_data <- function(data, fml, fe_vars = c("BIRTH", "prov_nac")) {
  yname <- all.vars(fml[[2]])[1]
  fml_chr <- paste(deparse(fml, width.cutoff = 500), collapse = " ")
  main_part <- trimws(strsplit(fml_chr, "\\|")[[1]][1])
  rhs_vars <- all.vars(as.formula(main_part))
  needed <- unique(c(rhs_vars, fe_vars))
  d_est <- data[stats::complete.cases(data[, needed, drop = FALSE]), , drop = FALSE]
  mean(d_est[[yname]], na.rm = TRUE)
}


# RELIGIOUS OUTCOMES

# CATHOLIC
lpm_fe_linear_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_linear_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# COUPLE_CATHOLIC
lpm_fe_linear_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# Exact formulas for mean DV rows
fml_cat_lin <- CATHOLIC ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_cat_quad <- CATHOLIC ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_rel_lin <- RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_rel_quad <- RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_cou_lin <- COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_cou_quad <- COUPLE_CATHOLIC ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

mean_cat_lin  <- dv_mean_from_data(model_data, fml_cat_lin)
mean_cat_quad <- dv_mean_from_data(model_data, fml_cat_quad)

mean_rel_lin  <- dv_mean_from_data(model_data, fml_rel_lin)
mean_rel_quad <- dv_mean_from_data(model_data, fml_rel_quad)

mean_cou_lin  <- dv_mean_from_data(model_data, fml_cou_lin)
mean_cou_quad <- dv_mean_from_data(model_data, fml_cou_quad)

add_rows_religion <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Catholic: Linear"               = c(sprintf("%.3f", mean_cat_lin),  "Linear"),
  "Catholic: Quadratic"            = c(sprintf("%.3f", mean_cat_quad), "Quadratic"),
  "Religious practice: Linear"     = c(sprintf("%.3f", mean_rel_lin),  "Linear"),
  "Religious practice: Quadratic"  = c(sprintf("%.3f", mean_rel_quad), "Quadratic"),
  "Couple catholic: Linear"        = c(sprintf("%.3f", mean_cou_lin),  "Linear"),
  "Couple catholic: Quadratic"     = c(sprintf("%.3f", mean_cou_quad), "Quadratic")
)

models_religion <- list(
  "Catholic: Linear"              = lpm_fe_linear_cat,
  "Catholic: Quadratic"           = lpm_fe_quadratic_cat,
  "Religious practice: Linear"    = lpm_fe_linear_rel,
  "Religious practice: Quadratic" = lpm_fe_quadratic_rel,
  "Couple catholic: Linear"       = lpm_fe_linear_cou,
  "Couple catholic: Quadratic"    = lpm_fe_quadratic_cou
)

modelsummary(
  models_religion,
  title = "Religious outcomes: linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_religion
)


# POLITICAL OUTCOMES

# PARTICIPATION
lpm_fe_linear_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# CONSERVATIVE_VOTE
lpm_fe_linear_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# LEFT_RIGHT
lpm_fe_linear_lr <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_lr <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

fml_par_lin <- PARTICIPATION ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_par_quad <- PARTICIPATION ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_con_lin <- CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_con_quad <- CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_lr_lin <- LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_lr_quad <- LEFT_RIGHT ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

mean_par_lin  <- dv_mean_from_data(model_data, fml_par_lin)
mean_par_quad <- dv_mean_from_data(model_data, fml_par_quad)

mean_con_lin  <- dv_mean_from_data(model_data, fml_con_lin)
mean_con_quad <- dv_mean_from_data(model_data, fml_con_quad)

mean_lr_lin   <- dv_mean_from_data(model_data, fml_lr_lin)
mean_lr_quad  <- dv_mean_from_data(model_data, fml_lr_quad)

models_politics <- list(
  "Participation: Linear"    = lpm_fe_linear_par,
  "Participation: Quadratic" = lpm_fe_quadratic_par,
  "Conservative: Linear"     = lpm_fe_linear_con,
  "Conservative: Quadratic"  = lpm_fe_quadratic_con,
  "Left-right: Linear"       = lpm_fe_linear_lr,
  "Left-right: Quadratic"    = lpm_fe_quadratic_lr
)

add_rows_politics <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Participation: Linear"    = c(sprintf("%.3f", mean_par_lin),  "Linear"),
  "Participation: Quadratic" = c(sprintf("%.3f", mean_par_quad), "Quadratic"),
  "Conservative: Linear"     = c(sprintf("%.3f", mean_con_lin),  "Linear"),
  "Conservative: Quadratic"  = c(sprintf("%.3f", mean_con_quad), "Quadratic"),
  "Left-right: Linear"       = c(sprintf("%.3f", mean_lr_lin),   "Linear"),
  "Left-right: Quadratic"    = c(sprintf("%.3f", mean_lr_quad),  "Quadratic")
)

modelsummary(
  models_politics,
  title = "Political outcomes: linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_politics
)


# OTHER OUTCOMES / FALSIFICATION

# EDUCATION
lpm_fe_linear_edu <- feols(
  EDUCATION ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_edu <- feols(
  EDUCATION ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# INCOME
lpm_fe_linear_inc <- feols(
  INCOME ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_inc <- feols(
  INCOME ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# TRUST_PEOPLE
lpm_fe_linear_trust <- feols(
  TRUST_PEOPLE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

lpm_fe_quadratic_trust <- feols(
  TRUST_PEOPLE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

fml_edu_lin <- EDUCATION ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_edu_quad <- EDUCATION ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_inc_lin <- INCOME ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_inc_quad <- INCOME ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

fml_tru_lin <- TRUST_PEOPLE ~ childhood_total_dry_days_std + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac
fml_tru_quad <- TRUST_PEOPLE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE + FATHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | BIRTH + prov_nac

mean_edu_lin   <- dv_mean_from_data(model_data, fml_edu_lin)
mean_edu_quad  <- dv_mean_from_data(model_data, fml_edu_quad)

mean_inc_lin   <- dv_mean_from_data(model_data, fml_inc_lin)
mean_inc_quad  <- dv_mean_from_data(model_data, fml_inc_quad)

mean_tru_lin   <- dv_mean_from_data(model_data, fml_tru_lin)
mean_tru_quad  <- dv_mean_from_data(model_data, fml_tru_quad)

models_other <- list(
  "Education: Linear"            = lpm_fe_linear_edu,
  "Education: Quadratic"         = lpm_fe_quadratic_edu,
  "Household income: Linear"     = lpm_fe_linear_inc,
  "Household income: Quadratic"  = lpm_fe_quadratic_inc,
  "Trust people: Linear"         = lpm_fe_linear_trust,
  "Trust people: Quadratic"      = lpm_fe_quadratic_trust
)

add_rows_other <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Education: Linear"           = c(sprintf("%.3f", mean_edu_lin),  "Linear"),
  "Education: Quadratic"        = c(sprintf("%.3f", mean_edu_quad), "Quadratic"),
  "Household income: Linear"    = c(sprintf("%.3f", mean_inc_lin),  "Linear"),
  "Household income: Quadratic" = c(sprintf("%.3f", mean_inc_quad), "Quadratic"),
  "Trust people: Linear"        = c(sprintf("%.3f", mean_tru_lin),  "Linear"),
  "Trust people: Quadratic"     = c(sprintf("%.3f", mean_tru_quad), "Quadratic")
)

modelsummary(
  models_other,
  title = "Other outcomes: linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_other
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
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_rel <- feols(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_cou <- feols(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

# --- Political outcomes ---

lpm_fe_spline_par <- feols(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_con <- feols(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_lr <- feols(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth |
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
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_rel <- lm(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_cou <- lm(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_par <- lm(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_con <- lm(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_lr <- lm(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
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

y_var <- "CONSERVATIVE_VOTE"                     # outcome
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


# 5) Binned means (50 quantile bins — cosmetic change)

n_bins <- 50   

df_bins <- df %>%
  mutate(bin = ntile(d_res, n_bins)) %>%
  group_by(bin) %>%
  summarise(
    d_bin = mean(d_res),
    y_bin = mean(y_res),
    se    = sd(y_res) / sqrt(n()),
    .groups = "drop"
  )


# 6) Plot (cosmetic improvements only)

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

p_quartiles <- p_quartiles +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.position = "right"
  )

p_tertiles <- p_tertiles +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.position = "right"
  )


# Export these 2 plots for LaTeX in highest quality

library(ggplot2)
library(ragg)

# Create output folder
if (!dir.exists("figures")) dir.create("figures")

# Helper: save plot as vector PDF for LaTeX
save_latex_plot <- function(plot_obj, filename,
                            width = 10, height = 5.625,
                            save_png_fallback = FALSE,
                            png_dpi = 600) {
  
  ggsave(
    filename = file.path("figures", paste0(filename, ".pdf")),
    plot = plot_obj,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  if (save_png_fallback) {
    ggsave(
      filename = file.path("figures", paste0(filename, ".png")),
      plot = plot_obj,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "in",
      dpi = png_dpi,
      bg = "white"
    )
  }
}

# Save both plots
save_latex_plot(p_quartiles, "distribution_quartiles")
save_latex_plot(p_tertiles,  "distribution_tertiles")


# Non-parametric identification: quartile and tertile bins of treatment -------------------

library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(purrr)
library(ggplot2)
library(scales)
library(tibble)
library(ragg)


# 1) Prepare treatment bins once


model_data <- model_data %>%
  mutate(
    treat_q = ntile(childhood_total_dry_days_std, 4),
    treat_t = ntile(childhood_total_dry_days_std, 3)
  )

table(model_data$treat_q, useNA = "ifany")
table(model_data$treat_t, useNA = "ifany")


# 2) Helpers


theme_adrf <- function() {
  theme_minimal(base_size = 17) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.45),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold", size = 15),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 13),
      legend.position = "none"
    )
}

plot_adrf <- function(df, nbins, xlabels,
                      ylab = "Effect relative to lowest bin",
                      line_color = "#1f77b4") {
  
  ggplot(df, aes(x = treat_level, y = estimate, group = Outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.65) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.08, color = "grey55", linewidth = 0.65
    ) +
    geom_line(color = line_color, linewidth = 1.1) +
    geom_point(
      aes(color = treat_level == 1),
      size = 2.9,
      show.legend = FALSE
    ) +
    scale_color_manual(values = c(`TRUE` = "grey35", `FALSE` = line_color)) +
    facet_wrap(~ Outcome, scales = "free_y") +
    scale_x_continuous(breaks = 1:nbins, labels = xlabels) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = ylab
    ) +
    theme_adrf()
}

plot_bin_coefficients <- function(df, xvar,
                                  ylab = "Coefficient estimate (95% CI)") {
  ggplot(df, aes(x = .data[[xvar]], y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.65) +
    geom_pointrange(color = "#1f77b4", linewidth = 0.55) +
    facet_wrap(~ Outcome, scales = "free_y") +
    labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = ylab
    ) +
    theme_adrf()
}

extract_binned_effects <- function(models, var_prefix, label_map) {
  map_dfr(models, ~ tidy(.x, conf.int = TRUE), .id = "Outcome") %>%
    filter(grepl(paste0("^", var_prefix, "::"), term)) %>%
    mutate(
      bin_label   = recode(term, !!!label_map),
      treat_level = as.integer(sub(paste0(var_prefix, "::"), "", term))
    )
}

build_adrf_data <- function(coef_df, baseline_level = 1L) {
  out <- coef_df %>%
    select(Outcome, treat_level, estimate, conf.low, conf.high)
  
  bind_rows(
    out,
    tibble(
      Outcome     = unique(out$Outcome),
      treat_level = baseline_level,
      estimate    = 0,
      conf.low    = 0,
      conf.high   = 0
    )
  ) %>%
    arrange(Outcome, treat_level)
}

pretty_outcomes <- c(
  Catholic          = "Catholic",
  ReligiousPractice = "Religious practice",
  CoupleCatholic    = "Catholic partner",
  Participation     = "Participation",
  Conservative      = "Conservative vote",
  LeftRight         = "Left-right scale",
  Income            = "Income",
  Education         = "Education",
  TrustPeople       = "Trust in people"
)

q_labels <- c(
  "treat_q::2" = "Q2 vs Q1",
  "treat_q::3" = "Q3 vs Q1",
  "treat_q::4" = "Q4 vs Q1"
)

t_labels <- c(
  "treat_t::2" = "T2 vs T1",
  "treat_t::3" = "T3 vs T1"
)

save_latex_plot <- function(plot_obj, filename,
                            width = 10, height = 5.625,
                            save_png_fallback = FALSE,
                            png_dpi = 600) {
  
  ggsave(
    filename = file.path("figures", paste0(filename, ".pdf")),
    plot = plot_obj,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  if (save_png_fallback) {
    ggsave(
      filename = file.path("figures", paste0(filename, ".png")),
      plot = plot_obj,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "in",
      dpi = png_dpi,
      bg = "white"
    )
  }
}


# 3) Quartiles: Religious outcomes


lpm_fe_q_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_cat_ctrl <- wald(lpm_fe_q_controls_cat, keep = "treat_q::")
wald_rel_ctrl <- wald(lpm_fe_q_controls_rel, keep = "treat_q::")
wald_cou_ctrl <- wald(lpm_fe_q_controls_cou, keep = "treat_q::")

wald_cat_ctrl
wald_rel_ctrl
wald_cou_ctrl


# Means of dependent variables on estimation samples: quartile models (religious)

fml_q_cat_noc <- CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_cat_con <- CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_rel_noc <- RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_rel_con <- RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_cou_noc <- COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_cou_con <- COUPLE_CATHOLIC ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

mean_q_cat_noc <- dv_mean_from_data(model_data, fml_q_cat_noc)
mean_q_cat_con <- dv_mean_from_data(model_data, fml_q_cat_con)

mean_q_rel_noc <- dv_mean_from_data(model_data, fml_q_rel_noc)
mean_q_rel_con <- dv_mean_from_data(model_data, fml_q_rel_con)

mean_q_cou_noc <- dv_mean_from_data(model_data, fml_q_cou_noc)
mean_q_cou_con <- dv_mean_from_data(model_data, fml_q_cou_con)


# Wald p-values: quartile models (religious)
pval_q_cat_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cat, keep = "treat_q::")[["p"]])
pval_q_cat_con <- sprintf("%.3f", wald_cat_ctrl[["p"]])

pval_q_rel_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_rel, keep = "treat_q::")[["p"]])
pval_q_rel_con <- sprintf("%.3f", wald_rel_ctrl[["p"]])

pval_q_cou_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cou, keep = "treat_q::")[["p"]])
pval_q_cou_con <- sprintf("%.3f", wald_cou_ctrl[["p"]])

add_rows_q_religion <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Catholic (Q bins)"                      = c(sprintf("%.3f", mean_q_cat_noc), "No",  pval_q_cat_noc),
  "Catholic (Q bins) + Controls"           = c(sprintf("%.3f", mean_q_cat_con), "Yes", pval_q_cat_con),
  "Religious practice (Q bins)"            = c(sprintf("%.3f", mean_q_rel_noc), "No",  pval_q_rel_noc),
  "Religious practice (Q bins) + Controls" = c(sprintf("%.3f", mean_q_rel_con), "Yes", pval_q_rel_con),
  "Catholic partner (Q bins)"              = c(sprintf("%.3f", mean_q_cou_noc), "No",  pval_q_cou_noc),
  "Catholic partner (Q bins) + Controls"   = c(sprintf("%.3f", mean_q_cou_con), "Yes", pval_q_cou_con)
)


modelsummary(
  list(
    "Catholic (Q bins)"                      = lpm_fe_q_nocontrols_cat,
    "Catholic (Q bins) + Controls"           = lpm_fe_q_controls_cat,
    "Religious practice (Q bins)"            = lpm_fe_q_nocontrols_rel,
    "Religious practice (Q bins) + Controls" = lpm_fe_q_controls_rel,
    "Catholic partner (Q bins)"              = lpm_fe_q_nocontrols_cou,
    "Catholic partner (Q bins) + Controls"   = lpm_fe_q_controls_cou
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_religion
)

# 4) Quartiles: Political outcomes


lpm_fe_q_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_par_ctrl <- wald(lpm_fe_q_controls_par, keep = "treat_q::")
wald_con_ctrl <- wald(lpm_fe_q_controls_con, keep = "treat_q::")
wald_lr_ctrl  <- wald(lpm_fe_q_controls_lr, keep = "treat_q::")

wald_par_ctrl
wald_con_ctrl
wald_lr_ctrl

# Means of dependent variables on estimation samples: quartile models (political)

fml_q_par_noc <- PARTICIPATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_par_con <- PARTICIPATION ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_con_noc <- CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_con_con <- CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_lr_noc <- LEFT_RIGHT ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_lr_con <- LEFT_RIGHT ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

mean_q_par_noc <- dv_mean_from_data(model_data, fml_q_par_noc)
mean_q_par_con <- dv_mean_from_data(model_data, fml_q_par_con)

mean_q_con_noc <- dv_mean_from_data(model_data, fml_q_con_noc)
mean_q_con_con <- dv_mean_from_data(model_data, fml_q_con_con)

mean_q_lr_noc <- dv_mean_from_data(model_data, fml_q_lr_noc)
mean_q_lr_con <- dv_mean_from_data(model_data, fml_q_lr_con)

# Wald p-values: quartile models (political)
pval_q_par_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_par, keep = "treat_q::")[["p"]])
pval_q_par_con <- sprintf("%.3f", wald_par_ctrl[["p"]])

pval_q_con_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_con, keep = "treat_q::")[["p"]])
pval_q_con_con <- sprintf("%.3f", wald_con_ctrl[["p"]])

pval_q_lr_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_lr, keep = "treat_q::")[["p"]])
pval_q_lr_con <- sprintf("%.3f", wald_lr_ctrl[["p"]])

add_rows_q_politics <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Participation (Q bins)"             = c(sprintf("%.3f", mean_q_par_noc), "No",  pval_q_par_noc),
  "Participation (Q bins) + Controls"  = c(sprintf("%.3f", mean_q_par_con), "Yes", pval_q_par_con),
  "Conservative (Q bins)"              = c(sprintf("%.3f", mean_q_con_noc), "No",  pval_q_con_noc),
  "Conservative (Q bins) + Controls"   = c(sprintf("%.3f", mean_q_con_con), "Yes", pval_q_con_con),
  "Left-right (Q bins)"                = c(sprintf("%.3f", mean_q_lr_noc), "No",  pval_q_lr_noc),
  "Left-right (Q bins) + Controls"     = c(sprintf("%.3f", mean_q_lr_con), "Yes", pval_q_lr_con)
)
modelsummary(
  list(
    "Participation (Q bins)"             = lpm_fe_q_nocontrols_par,
    "Participation (Q bins) + Controls"  = lpm_fe_q_controls_par,
    "Conservative (Q bins)"              = lpm_fe_q_nocontrols_con,
    "Conservative (Q bins) + Controls"   = lpm_fe_q_controls_con,
    "Left-right (Q bins)"                = lpm_fe_q_nocontrols_lr,
    "Left-right (Q bins) + Controls"     = lpm_fe_q_controls_lr
  ),
  title = "LPM with quartile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_politics
)

# 5) Quartiles: Other outcomes


lpm_fe_q_nocontrols_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_inc_q_ctrl   <- wald(lpm_fe_q_controls_inc, keep = "treat_q::")
wald_edu_q_ctrl   <- wald(lpm_fe_q_controls_edu, keep = "treat_q::")
wald_trust_q_ctrl <- wald(lpm_fe_q_controls_trust, keep = "treat_q::")

wald_inc_q_ctrl
wald_edu_q_ctrl
wald_trust_q_ctrl

# Means of dependent variables on estimation samples: quartile models (other outcomes)

fml_q_inc_noc <- INCOME ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_inc_con <- INCOME ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_edu_noc <- EDUCATION ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_edu_con <- EDUCATION ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

fml_q_tru_noc <- TRUST_PEOPLE ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac
fml_q_tru_con <- TRUST_PEOPLE ~ i(treat_q, ref = 1) + FEMALE +
  FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
  FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
  survey_year + log_pop_birth | BIRTH + prov_nac

mean_q_inc_noc <- dv_mean_from_data(model_data, fml_q_inc_noc)
mean_q_inc_con <- dv_mean_from_data(model_data, fml_q_inc_con)

mean_q_edu_noc <- dv_mean_from_data(model_data, fml_q_edu_noc)
mean_q_edu_con <- dv_mean_from_data(model_data, fml_q_edu_con)

mean_q_tru_noc <- dv_mean_from_data(model_data, fml_q_tru_noc)
mean_q_tru_con <- dv_mean_from_data(model_data, fml_q_tru_con)

# Wald p-values: quartile models (other outcomes)
pval_q_inc_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_inc, keep = "treat_q::")[["p"]])
pval_q_inc_con <- sprintf("%.3f", wald_inc_q_ctrl[["p"]])

pval_q_edu_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_edu, keep = "treat_q::")[["p"]])
pval_q_edu_con <- sprintf("%.3f", wald_edu_q_ctrl[["p"]])

pval_q_tru_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_trust, keep = "treat_q::")[["p"]])
pval_q_tru_con <- sprintf("%.3f", wald_trust_q_ctrl[["p"]])


add_rows_q_other <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Income (Q bins)"                  = c(sprintf("%.3f", mean_q_inc_noc), "No",  pval_q_inc_noc),
  "Income (Q bins) + Controls"       = c(sprintf("%.3f", mean_q_inc_con), "Yes", pval_q_inc_con),
  "Education (Q bins)"               = c(sprintf("%.3f", mean_q_edu_noc), "No",  pval_q_edu_noc),
  "Education (Q bins) + Controls"    = c(sprintf("%.3f", mean_q_edu_con), "Yes", pval_q_edu_con),
  "Trust people (Q bins)"            = c(sprintf("%.3f", mean_q_tru_noc), "No",  pval_q_tru_noc),
  "Trust people (Q bins) + Controls" = c(sprintf("%.3f", mean_q_tru_con), "Yes", pval_q_tru_con)
)

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
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_other
)

# 6) Quartiles: Extract coefficients and build plots


religious_models_q <- list(
  Catholic           = lpm_fe_q_controls_cat,
  ReligiousPractice  = lpm_fe_q_controls_rel,
  CoupleCatholic     = lpm_fe_q_controls_cou
)

political_models_q <- list(
  Participation = lpm_fe_q_controls_par,
  Conservative  = lpm_fe_q_controls_con,
  LeftRight     = lpm_fe_q_controls_lr
)

other_models_q <- list(
  Income      = lpm_fe_q_controls_inc,
  Education   = lpm_fe_q_controls_edu,
  TrustPeople = lpm_fe_q_controls_trust
)

coef_religious_q <- extract_binned_effects(religious_models_q, "treat_q", q_labels) %>%
  mutate(
    Outcome  = recode(Outcome, !!!pretty_outcomes),
    Outcome  = factor(
      Outcome,
      levels = c("Catholic", "Religious practice", "Catholic partner")
    ),
    Quartile = recode(term, !!!q_labels)
  )

coef_political_q <- extract_binned_effects(political_models_q, "treat_q", q_labels) %>%
  mutate(
    Outcome  = recode(Outcome, !!!pretty_outcomes),
    Outcome  = factor(
      Outcome,
      levels = c("Participation", "Conservative vote", "Left-right scale")
    ),
    Quartile = recode(term, !!!q_labels)
  )
coef_other_q <- extract_binned_effects(other_models_q, "treat_q", q_labels) %>%
  mutate(
    Outcome  = recode(Outcome, !!!pretty_outcomes),
    Quartile = recode(term, !!!q_labels)
  )

p_coef_q_rel <- plot_bin_coefficients(
  coef_religious_q,
  xvar = "Quartile"
)

p_coef_q_pol <- plot_bin_coefficients(
  coef_political_q,
  xvar = "Quartile"
)

p_coef_q_other <- plot_bin_coefficients(
  coef_other_q,
  xvar = "Quartile"
)

p_coef_q_rel
p_coef_q_pol
p_coef_q_other

adrf_data_religious <- build_adrf_data(coef_religious_q)
adrf_data_political <- build_adrf_data(coef_political_q)
adrf_data_other     <- build_adrf_data(coef_other_q)

p_adrf_q_rel <- plot_adrf(
  df      = adrf_data_religious,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

p_adrf_q_pol <- plot_adrf(
  df      = adrf_data_political,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

p_adrf_q_other <- plot_adrf(
  df      = adrf_data_other,
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

p_adrf_q_rel
p_adrf_q_pol
p_adrf_q_other


# 7) Tertiles: Religious outcomes


lpm_fe_t_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_cat_t_ctrl <- wald(lpm_fe_t_controls_cat, keep = "treat_t::")
wald_rel_t_ctrl <- wald(lpm_fe_t_controls_rel, keep = "treat_t::")
wald_cou_t_ctrl <- wald(lpm_fe_t_controls_cou, keep = "treat_t::")

wald_cat_t_ctrl
wald_rel_t_ctrl
wald_cou_t_ctrl

modelsummary(
  list(
    "Catholic (T bins)"                      = lpm_fe_t_nocontrols_cat,
    "Catholic (T bins) + Controls"           = lpm_fe_t_controls_cat,
    "Religious practice (T bins)"            = lpm_fe_t_nocontrols_rel,
    "Religious practice (T bins) + Controls" = lpm_fe_t_controls_rel,
    "Catholic partner (T bins)"              = lpm_fe_t_nocontrols_cou,
    "Catholic partner (T bins) + Controls"   = lpm_fe_t_controls_cou
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = t_labels,
  add_rows = tibble(
    term = "Controls",
    `Catholic (T bins)`                      = "No",
    `Catholic (T bins) + Controls`           = "Yes",
    `Religious practice (T bins)`            = "No",
    `Religious practice (T bins) + Controls` = "Yes",
    `Catholic partner (T bins)`              = "No",
    `Catholic partner (T bins) + Controls`   = "Yes"
  )
)


# 8) Tertiles: Political outcomes


lpm_fe_t_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_par_t_ctrl <- wald(lpm_fe_t_controls_par, keep = "treat_t::")
wald_con_t_ctrl <- wald(lpm_fe_t_controls_con, keep = "treat_t::")
wald_lr_t_ctrl  <- wald(lpm_fe_t_controls_lr, keep = "treat_t::")

wald_par_t_ctrl
wald_con_t_ctrl
wald_lr_t_ctrl

modelsummary(
  list(
    "Participation (T bins)"             = lpm_fe_t_nocontrols_par,
    "Participation (T bins) + Controls"  = lpm_fe_t_controls_par,
    "Conservative (T bins)"              = lpm_fe_t_nocontrols_con,
    "Conservative (T bins) + Controls"   = lpm_fe_t_controls_con,
    "Left-right (T bins)"                = lpm_fe_t_nocontrols_lr,
    "Left-right (T bins) + Controls"     = lpm_fe_t_controls_lr
  ),
  title = "LPM with tertile dummies of standardized childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = t_labels,
  add_rows = tibble(
    term = "Controls",
    `Participation (T bins)`             = "No",
    `Participation (T bins) + Controls`  = "Yes",
    `Conservative (T bins)`              = "No",
    `Conservative (T bins) + Controls`   = "Yes",
    `Left-right (T bins)`                = "No",
    `Left-right (T bins) + Controls`     = "Yes"
  )
)


# 9) Tertiles: Other outcomes


lpm_fe_t_nocontrols_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

wald_inc_t_ctrl   <- wald(lpm_fe_t_controls_inc, keep = "treat_t::")
wald_edu_t_ctrl   <- wald(lpm_fe_t_controls_edu, keep = "treat_t::")
wald_trust_t_ctrl <- wald(lpm_fe_t_controls_trust, keep = "treat_t::")

wald_inc_t_ctrl
wald_edu_t_ctrl
wald_trust_t_ctrl

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
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = t_labels,
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


# 10) Tertiles: Extract coefficients and build plots


religious_models_t <- list(
  Catholic           = lpm_fe_t_controls_cat,
  ReligiousPractice  = lpm_fe_t_controls_rel,
  CoupleCatholic     = lpm_fe_t_controls_cou
)

political_models_t <- list(
  Participation = lpm_fe_t_controls_par,
  Conservative  = lpm_fe_t_controls_con,
  LeftRight     = lpm_fe_t_controls_lr
)

other_models_t <- list(
  Income      = lpm_fe_t_controls_inc,
  Education   = lpm_fe_t_controls_edu,
  TrustPeople = lpm_fe_t_controls_trust
)

coef_religious_t <- extract_binned_effects(religious_models_t, "treat_t", t_labels) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes),
    Tertile = recode(term, !!!t_labels)
  )

coef_political_t <- extract_binned_effects(political_models_t, "treat_t", t_labels) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes),
    Tertile = recode(term, !!!t_labels)
  )

coef_other_t <- extract_binned_effects(other_models_t, "treat_t", t_labels) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes),
    Tertile = recode(term, !!!t_labels)
  )

p_coef_t_rel <- plot_bin_coefficients(
  coef_religious_t,
  xvar = "Tertile"
)

p_coef_t_pol <- plot_bin_coefficients(
  coef_political_t,
  xvar = "Tertile"
)

p_coef_t_other <- plot_bin_coefficients(
  coef_other_t,
  xvar = "Tertile"
)

p_coef_t_rel
p_coef_t_pol
p_coef_t_other

adrf_data_religious_t <- build_adrf_data(coef_religious_t)
adrf_data_political_t <- build_adrf_data(coef_political_t)
adrf_data_other_t     <- build_adrf_data(coef_other_t)

p_adrf_t_rel <- plot_adrf(
  df      = adrf_data_religious_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  ylab    = "Effect relative to T1"
)

p_adrf_t_pol <- plot_adrf(
  df      = adrf_data_political_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  ylab    = "Effect relative to T1"
)

p_adrf_t_other <- plot_adrf(
  df      = adrf_data_other_t,
  nbins   = 3,
  xlabels = paste0("T", 1:3),
  ylab    = "Effect relative to T1"
)

p_adrf_t_rel
p_adrf_t_pol
p_adrf_t_other


# 11) Export figures for LaTeX


if (!dir.exists("figures")) dir.create("figures")

save_latex_plot(p_adrf_q_rel,   "adrf_quartiles_religious")
save_latex_plot(p_adrf_q_pol,   "adrf_quartiles_political")
save_latex_plot(p_adrf_q_other, "adrf_quartiles_other")

save_latex_plot(p_adrf_t_rel,   "adrf_tertiles_religious")
save_latex_plot(p_adrf_t_pol,   "adrf_tertiles_political")
save_latex_plot(p_adrf_t_other, "adrf_tertiles_other")

save_latex_plot(p_coef_q_rel,   "coef_quartiles_religious")
save_latex_plot(p_coef_q_pol,   "coef_quartiles_political")
save_latex_plot(p_coef_q_other, "coef_quartiles_other")

save_latex_plot(p_coef_t_rel,   "coef_tertiles_religious")
save_latex_plot(p_coef_t_pol,   "coef_tertiles_political")
save_latex_plot(p_coef_t_other, "coef_tertiles_other")

# Rain does not predict migration -----------------------------------------

# ---------------------------------------------------------
# EXTRA OUTCOME: SAME_LOC_BIRTH
#   - Quadratic spec (std + std^2)
#   - Quartile dummies of standardized treatment
#   - Tables like your previous modelsummary blocks:
#       * no DV mean row
#       * no fetch_data / dv_mean helper
# ---------------------------------------------------------

# (safety) make sure SAME_LOC_BIRTH is in model_data
if (!("SAME_LOC_BIRTH" %in% names(model_data))) {
  stop("SAME_LOC_BIRTH is not in model_data. Add it to your dplyr::select(...) when building model_data.")
}

# --------------------------
# A) Quadratic specification
# --------------------------

# 1) With FE, No controls
lpm_fe_nocontrols_same <- feols(
  SAME_LOC_BIRTH ~ childhood_total_dry_days_std + survey_year + I(childhood_total_dry_days_std^2) | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# 2) With FE, With controls
lpm_fe_controls_same <- feols(
  SAME_LOC_BIRTH ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

modelsummary(
  list(
    "Same loc (quad)"               = lpm_fe_nocontrols_same,
    "Same loc (quad) + Controls"    = lpm_fe_controls_same
  ),
  title = "Outcome: SAME_LOC_BIRTH (quadratic specification)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = "Controls",
    `Same loc (quad)`            = "No",
    `Same loc (quad) + Controls` = "Yes"
  )
)

# --------------------------
# B) Quartile specification
# --------------------------

# define quartiles on standardized treatment (same convention as your Q-bins section)
model_data <- model_data %>%
  mutate(treat_q = ntile(childhood_total_dry_days_std, 4))

# 1) With FE, No controls
lpm_fe_q_nocontrols_same <- feols(
  SAME_LOC_BIRTH ~ i(treat_q, ref = 1) + survey_year | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# 2) With FE, With controls
lpm_fe_q_controls_same <- feols(
  SAME_LOC_BIRTH ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# optional joint test: H0 Q2=Q3=Q4=0
wald_same_q_ctrl <- wald(lpm_fe_q_controls_same, keep = "treat_q::")
wald_same_q_ctrl

modelsummary(
  list(
    "Same loc (Q bins)"            = lpm_fe_q_nocontrols_same,
    "Same loc (Q bins) + Controls" = lpm_fe_q_controls_same
  ),
  title = "Outcome: SAME_LOC_BIRTH (quartile dummies of standardized treatment)",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  ),
  add_rows = tibble::tibble(
    term = "Controls",
    `Same loc (Q bins)`             = "No",
    `Same loc (Q bins) + Controls`  = "Yes"
  )
)

# Non-parametric identification: fixed dry-day bins of treatment -----------------------------

# 1) Bins of raw childhood_total_dry_days (4 bins)

model_data <- model_data %>%
  mutate(
    treat_t = case_when(
      childhood_total_dry_days < 96 ~ 1L,                                 # Bin 1: lowest exposure
      childhood_total_dry_days >= 96 & childhood_total_dry_days < 102 ~ 2L, # Bin 2
      childhood_total_dry_days >= 102 & childhood_total_dry_days < 108 ~ 3L,# Bin 3
      childhood_total_dry_days >= 106 ~ 4L,                                 # Bin 4: highest exposure
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
    survey_year + log_pop_birth  | BIRTH + prov_nac,
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
    survey_year + log_pop_birth  | BIRTH + prov_nac,
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
# H0: treat_t::2 = treat_t::3 = treat_t::4 = 0
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
  title = "LPM with four dry-day bins of childhood dry days (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Bin 2 vs Bin 1",
    "treat_t::3" = "Bin 3 vs Bin 1",
    "treat_t::4" = "Bin 4 vs Bin 1"
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
    survey_year + log_pop_birth | BIRTH + prov_nac,
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
    survey_year + log_pop_birth | BIRTH + prov_nac,
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
    survey_year  + log_pop_birth | BIRTH + prov_nac,
  data = model_data,
  cluster = ~ prov_nac
)

# Joint tests (political outcomes)
# H0: treat_t::2 = treat_t::3 = treat_t::4 = 0
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
  title = "LPM with four dry-day bins of childhood dry days (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Bin 2 vs Bin 1",
    "treat_t::3" = "Bin 3 vs Bin 1",
    "treat_t::4" = "Bin 4 vs Bin 1"
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
        term == "treat_t::3" ~ 3L,
        term == "treat_t::4" ~ 4L
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
        levels = 1:4,
        labels = c("Bin 1", "Bin 2", "Bin 3", "Bin 4")
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
    y = "Estimated Effect (vs Bin 1)"
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
    y = "Estimated Effect (vs Bin 1)"
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
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + 
    survey_year  + log_pop_birth | BIRTH + prov_nac,
  data    = model_data,
  cluster = ~ prov_nac
)

# Joint tests (other outcomes)
# H0: treat_t::2 = treat_t::3 = treat_t::4 = 0
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
  title = "LPM with four dry-day bins of childhood dry days (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_t::2" = "Bin 2 vs Bin 1",
    "treat_t::3" = "Bin 3 vs Bin 1",
    "treat_t::4" = "Bin 4 vs Bin 1"
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

coef_other_t <- build_adrf_data(other_models_t)

ggplot(coef_other_t,
       aes(x = exposure_level, y = estimate,
           ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Approximate Dose Response (Other Outcomes)",
    x = "Childhood dry-day exposure",
    y = "Estimated Effect (vs Bin 1)"
  ) +
  theme_minimal()


# Robustness: Conley SEs --------------------------------------------------

library(fixest)
library(mapSpain)
library(sf)
library(dplyr)
library(modelsummary)
library(purrr)
library(broom)
library(tibble)
library(ggplot2)

# ---------------------------------------------------------
# 0) Province coordinates and Conley vcov
# ---------------------------------------------------------
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

model_data_conley <- model_data %>%
  left_join(prov_coords_conley, by = "prov_nac") %>%
  mutate(
    treat_q = ntile(childhood_total_dry_days_std, 4)
  )

cutoff_km <- 100
vcov_conley <- conley(cutoff = cutoff_km, distance = "spherical") ~ lat + long

# ---------------------------------------------------------
# 1) Helpers
# ---------------------------------------------------------
controls_rhs <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "survey_year", "log_pop_birth"
)

fe_rhs <- c("BIRTH", "prov_nac")

fit_lq_models <- function(y, data, treat = "childhood_total_dry_days_std") {
  treat_sq <- paste0(treat, "_sq")
  
  f_lin <- as.formula(
    paste0(
      y, " ~ ", treat, " + ",
      paste(controls_rhs, collapse = " + "),
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  f_quad <- as.formula(
    paste0(
      y, " ~ ", treat, " + ", treat_sq, " + ",
      paste(controls_rhs, collapse = " + "),
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  list(
    linear = feols(f_lin, data = data),
    quad   = feols(f_quad, data = data),
    f_lin  = f_lin,
    f_quad = f_quad
  )
}

fit_q_models <- function(y, data) {
  f_noc <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + survey_year | ",
      paste(fe_rhs, collapse = " + ")
    )
  )
  
  f_con <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + ",
      paste(controls_rhs, collapse = " + "),
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  list(
    noc  = feols(f_noc, data = data),
    con  = feols(f_con, data = data),
    f_noc = f_noc,
    f_con = f_con
  )
}

get_wald_p_print_vcov <- function(mod, keep_pattern, vcov_spec) {
  out <- capture.output(fixest::wald(mod, keep = keep_pattern, vcov = vcov_spec))
  p_line <- grep("p-value =", out, value = TRUE)
  round(as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1])), 3)
}

extract_binned_effects_vcov <- function(models, var_prefix, label_map, vcov_spec) {
  map_dfr(names(models), function(nm) {
    mod <- models[[nm]]
    ct  <- coeftable(mod, vcov = vcov_spec)
    ci  <- confint(mod, vcov = vcov_spec)
    
    terms <- rownames(ct)[grepl(paste0("^", var_prefix, "::"), rownames(ct))]
    
    tibble(
      Outcome  = nm,
      term     = terms,
      estimate = unname(coef(mod)[terms]),
      conf.low = ci[terms, 1],
      conf.high = ci[terms, 2]
    )
  }) %>%
    mutate(
      treat_level = as.integer(sub(paste0(var_prefix, "::"), "", term)),
      bin_label   = recode(term, !!!label_map)
    )
}

# ---------------------------------------------------------
# 2) Outcomes
# ---------------------------------------------------------
outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
outcomes_other <- c("INCOME", "EDUCATION", "TRUST_PEOPLE")

# ---------------------------------------------------------
# 3) LINEAR / QUADRATIC TABLES (same style as main section)
# ---------------------------------------------------------

# Religious
mods_lq_relig_c <- lapply(outcomes_relig, fit_lq_models, data = model_data_conley)
names(mods_lq_relig_c) <- outcomes_relig

models_religion_conley <- list(
  "Catholic: Linear"              = mods_lq_relig_c$CATHOLIC$linear,
  "Catholic: Quadratic"           = mods_lq_relig_c$CATHOLIC$quad,
  "Religious practice: Linear"    = mods_lq_relig_c$RELIGIOUS_PRACTICE$linear,
  "Religious practice: Quadratic" = mods_lq_relig_c$RELIGIOUS_PRACTICE$quad,
  "Couple catholic: Linear"       = mods_lq_relig_c$COUPLE_CATHOLIC$linear,
  "Couple catholic: Quadratic"    = mods_lq_relig_c$COUPLE_CATHOLIC$quad
)

add_rows_religion_conley <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Catholic: Linear"              = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$CATHOLIC$f_lin)),  "Linear"),
  "Catholic: Quadratic"           = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$CATHOLIC$f_quad)), "Quadratic"),
  "Religious practice: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$RELIGIOUS_PRACTICE$f_lin)),  "Linear"),
  "Religious practice: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$RELIGIOUS_PRACTICE$f_quad)), "Quadratic"),
  "Couple catholic: Linear"       = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$COUPLE_CATHOLIC$f_lin)),  "Linear"),
  "Couple catholic: Quadratic"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_relig_c$COUPLE_CATHOLIC$f_quad)), "Quadratic")
)

modelsummary(
  models_religion_conley,
  vcov = vcov_conley,
  title = paste0("Religious outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_religion_conley
)

# Political
mods_lq_pol_c <- lapply(outcomes_pol, fit_lq_models, data = model_data_conley)
names(mods_lq_pol_c) <- outcomes_pol

models_politics_conley <- list(
  "Participation: Linear"    = mods_lq_pol_c$PARTICIPATION$linear,
  "Participation: Quadratic" = mods_lq_pol_c$PARTICIPATION$quad,
  "Conservative: Linear"     = mods_lq_pol_c$CONSERVATIVE_VOTE$linear,
  "Conservative: Quadratic"  = mods_lq_pol_c$CONSERVATIVE_VOTE$quad,
  "Left-right: Linear"       = mods_lq_pol_c$LEFT_RIGHT$linear,
  "Left-right: Quadratic"    = mods_lq_pol_c$LEFT_RIGHT$quad
)

add_rows_politics_conley <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Participation: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$PARTICIPATION$f_lin)),  "Linear"),
  "Participation: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$PARTICIPATION$f_quad)), "Quadratic"),
  "Conservative: Linear"     = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$CONSERVATIVE_VOTE$f_lin)),  "Linear"),
  "Conservative: Quadratic"  = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$CONSERVATIVE_VOTE$f_quad)), "Quadratic"),
  "Left-right: Linear"       = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$LEFT_RIGHT$f_lin)),  "Linear"),
  "Left-right: Quadratic"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_pol_c$LEFT_RIGHT$f_quad)), "Quadratic")
)

modelsummary(
  models_politics_conley,
  vcov = vcov_conley,
  title = paste0("Political outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_politics_conley
)

# Other
mods_lq_other_c <- lapply(outcomes_other, fit_lq_models, data = model_data_conley)
names(mods_lq_other_c) <- outcomes_other

models_other_conley <- list(
  "Education: Linear"           = mods_lq_other_c$EDUCATION$linear,
  "Education: Quadratic"        = mods_lq_other_c$EDUCATION$quad,
  "Household income: Linear"    = mods_lq_other_c$INCOME$linear,
  "Household income: Quadratic" = mods_lq_other_c$INCOME$quad,
  "Trust people: Linear"        = mods_lq_other_c$TRUST_PEOPLE$linear,
  "Trust people: Quadratic"     = mods_lq_other_c$TRUST_PEOPLE$quad
)

add_rows_other_conley <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Education: Linear"           = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$EDUCATION$f_lin)),  "Linear"),
  "Education: Quadratic"        = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$EDUCATION$f_quad)), "Quadratic"),
  "Household income: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$INCOME$f_lin)),  "Linear"),
  "Household income: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$INCOME$f_quad)), "Quadratic"),
  "Trust people: Linear"        = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$TRUST_PEOPLE$f_lin)),  "Linear"),
  "Trust people: Quadratic"     = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_lq_other_c$TRUST_PEOPLE$f_quad)), "Quadratic")
)

modelsummary(
  models_other_conley,
  vcov = vcov_conley,
  title = paste0("Other outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_other_conley
)

# ---------------------------------------------------------
# 4) QUARTILE TABLES (same style as quartile section)
# ---------------------------------------------------------

# Religious
mods_q_relig_c <- lapply(outcomes_relig, fit_q_models, data = model_data_conley)
names(mods_q_relig_c) <- outcomes_relig

add_rows_q_religion_conley <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Catholic (Q bins)"                      = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$CATHOLIC$f_noc)), "No",
                                               sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$CATHOLIC$con, "treat_q::", vcov_conley))),
  "Catholic (Q bins) + Controls"           = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$CATHOLIC$f_con)), "Yes",
                                               sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$CATHOLIC$con, "treat_q::", vcov_conley))),
  "Religious practice (Q bins)"            = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$RELIGIOUS_PRACTICE$f_noc)), "No",
                                               sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$RELIGIOUS_PRACTICE$con, "treat_q::", vcov_conley))),
  "Religious practice (Q bins) + Controls" = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$RELIGIOUS_PRACTICE$f_con)), "Yes",
                                               sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$RELIGIOUS_PRACTICE$con, "treat_q::", vcov_conley))),
  "Catholic partner (Q bins)"              = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$COUPLE_CATHOLIC$f_noc)), "No",
                                               sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$COUPLE_CATHOLIC$con, "treat_q::", vcov_conley))),
  "Catholic partner (Q bins) + Controls"   = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_relig_c$COUPLE_CATHOLIC$f_con)), "Yes",
                                              sprintf("%.3f", get_wald_p_print_vcov(mods_q_relig_c$COUPLE_CATHOLIC$con, "treat_q::", vcov_conley)))
)

modelsummary(
  list(
    "Catholic (Q bins)"                      = mods_q_relig_c$CATHOLIC$noc,
    "Catholic (Q bins) + Controls"           = mods_q_relig_c$CATHOLIC$con,
    "Religious practice (Q bins)"            = mods_q_relig_c$RELIGIOUS_PRACTICE$noc,
    "Religious practice (Q bins) + Controls" = mods_q_relig_c$RELIGIOUS_PRACTICE$con,
    "Catholic partner (Q bins)"              = mods_q_relig_c$COUPLE_CATHOLIC$noc,
    "Catholic partner (Q bins) + Controls"   = mods_q_relig_c$COUPLE_CATHOLIC$con
  ),
  vcov = vcov_conley,
  title = paste0("LPM with quartile dummies of standardized childhood dry days (religious outcomes), Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_religion_conley
)

# Political
mods_q_pol_c <- lapply(outcomes_pol, fit_q_models, data = model_data_conley)
names(mods_q_pol_c) <- outcomes_pol

add_rows_q_politics_conley <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Participation (Q bins)"             = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$PARTICIPATION$f_noc)), "No",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$PARTICIPATION$con, "treat_q::", vcov_conley))),
  "Participation (Q bins) + Controls"  = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$PARTICIPATION$f_con)), "Yes",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$PARTICIPATION$con, "treat_q::", vcov_conley))),
  "Conservative (Q bins)"              = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$CONSERVATIVE_VOTE$f_noc)), "No",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$CONSERVATIVE_VOTE$con, "treat_q::", vcov_conley))),
  "Conservative (Q bins) + Controls"   = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$CONSERVATIVE_VOTE$f_con)), "Yes",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$CONSERVATIVE_VOTE$con, "treat_q::", vcov_conley))),
  "Left-right (Q bins)"                = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$LEFT_RIGHT$f_noc)), "No",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$LEFT_RIGHT$con, "treat_q::", vcov_conley))),
  "Left-right (Q bins) + Controls"     = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_pol_c$LEFT_RIGHT$f_con)), "Yes",
                                           sprintf("%.3f", get_wald_p_print_vcov(mods_q_pol_c$LEFT_RIGHT$con, "treat_q::", vcov_conley)))
)

modelsummary(
  list(
    "Participation (Q bins)"             = mods_q_pol_c$PARTICIPATION$noc,
    "Participation (Q bins) + Controls"  = mods_q_pol_c$PARTICIPATION$con,
    "Conservative (Q bins)"              = mods_q_pol_c$CONSERVATIVE_VOTE$noc,
    "Conservative (Q bins) + Controls"   = mods_q_pol_c$CONSERVATIVE_VOTE$con,
    "Left-right (Q bins)"                = mods_q_pol_c$LEFT_RIGHT$noc,
    "Left-right (Q bins) + Controls"     = mods_q_pol_c$LEFT_RIGHT$con
  ),
  vcov = vcov_conley,
  title = paste0("LPM with quartile dummies of standardized childhood dry days (political outcomes), Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_politics_conley
)

# Other
mods_q_other_c <- lapply(outcomes_other, fit_q_models, data = model_data_conley)
names(mods_q_other_c) <- outcomes_other

add_rows_q_other_conley <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Income (Q bins)"                  = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$INCOME$f_noc)), "No",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$INCOME$con, "treat_q::", vcov_conley))),
  "Income (Q bins) + Controls"       = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$INCOME$f_con)), "Yes",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$INCOME$con, "treat_q::", vcov_conley))),
  "Education (Q bins)"               = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$EDUCATION$f_noc)), "No",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$EDUCATION$con, "treat_q::", vcov_conley))),
  "Education (Q bins) + Controls"    = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$EDUCATION$f_con)), "Yes",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$EDUCATION$con, "treat_q::", vcov_conley))),
  "Trust people (Q bins)"            = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$TRUST_PEOPLE$f_noc)), "No",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$TRUST_PEOPLE$con, "treat_q::", vcov_conley))),
  "Trust people (Q bins) + Controls" = c(sprintf("%.3f", dv_mean_from_data(model_data_conley, mods_q_other_c$TRUST_PEOPLE$f_con)), "Yes",
                                         sprintf("%.3f", get_wald_p_print_vcov(mods_q_other_c$TRUST_PEOPLE$con, "treat_q::", vcov_conley)))
)

modelsummary(
  list(
    "Income (Q bins)"                  = mods_q_other_c$INCOME$noc,
    "Income (Q bins) + Controls"       = mods_q_other_c$INCOME$con,
    "Education (Q bins)"               = mods_q_other_c$EDUCATION$noc,
    "Education (Q bins) + Controls"    = mods_q_other_c$EDUCATION$con,
    "Trust people (Q bins)"            = mods_q_other_c$TRUST_PEOPLE$noc,
    "Trust people (Q bins) + Controls" = mods_q_other_c$TRUST_PEOPLE$con
  ),
  vcov = vcov_conley,
  title = paste0("LPM with quartile dummies of standardized childhood dry days (other outcomes), Conley SEs (cutoff = ", cutoff_km, " km)"),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_other_conley
)

# ---------------------------------------------------------
# 5) ADRF PLOTS (same style as quartile section)
# ---------------------------------------------------------
coef_religious_q_conley <- extract_binned_effects_vcov(
  models = list(
    Catholic           = mods_q_relig_c$CATHOLIC$con,
    ReligiousPractice  = mods_q_relig_c$RELIGIOUS_PRACTICE$con,
    CoupleCatholic     = mods_q_relig_c$COUPLE_CATHOLIC$con
  ),
  var_prefix = "treat_q",
  label_map = q_labels,
  vcov_spec = vcov_conley
) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes),
    Outcome = factor(Outcome, levels = c("Catholic", "Religious practice", "Catholic partner"))
  )

coef_political_q_conley <- extract_binned_effects_vcov(
  models = list(
    Participation = mods_q_pol_c$PARTICIPATION$con,
    Conservative  = mods_q_pol_c$CONSERVATIVE_VOTE$con,
    LeftRight     = mods_q_pol_c$LEFT_RIGHT$con
  ),
  var_prefix = "treat_q",
  label_map = q_labels,
  vcov_spec = vcov_conley
) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes),
    Outcome = factor(Outcome, levels = c("Participation", "Conservative vote", "Left-right scale"))
  )

coef_other_q_conley <- extract_binned_effects_vcov(
  models = list(
    Income      = mods_q_other_c$INCOME$con,
    Education   = mods_q_other_c$EDUCATION$con,
    TrustPeople = mods_q_other_c$TRUST_PEOPLE$con
  ),
  var_prefix = "treat_q",
  label_map = q_labels,
  vcov_spec = vcov_conley
) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes)
  )

p_adrf_q_rel_conley <- plot_adrf(
  df = build_adrf_data(coef_religious_q_conley),
  nbins = 4,
  xlabels = paste0("Q", 1:4),
  ylab = "Effect relative to Q1"
)

p_adrf_q_pol_conley <- plot_adrf(
  df = build_adrf_data(coef_political_q_conley),
  nbins = 4,
  xlabels = paste0("Q", 1:4),
  ylab = "Effect relative to Q1"
)

p_adrf_q_other_conley <- plot_adrf(
  df = build_adrf_data(coef_other_q_conley),
  nbins = 4,
  xlabels = paste0("Q", 1:4),
  ylab = "Effect relative to Q1"
)

if (!dir.exists("figures")) dir.create("figures")

save_latex_plot(p_adrf_q_rel_conley,   "adrf_quartiles_religious_conley")
save_latex_plot(p_adrf_q_pol_conley,   "adrf_quartiles_political_conley")
save_latex_plot(p_adrf_q_other_conley, "adrf_quartiles_other_conley")


# Placebo: Years 0 to 4 ---------------------------------------------------

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)
library(tibble)

# ---------------------------------------------------------
# 0) Build placebo estimation sample
# ---------------------------------------------------------
model_data_p04 <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(dry_days_0_4),
    dry_days_0_4 != 0
  ) %>%
  mutate(
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    log_pop_birth = log(pop_birth_last_census),
    dry_days_0_4_std = (dry_days_0_4 - mean(dry_days_0_4, na.rm = TRUE)) /
      sd(dry_days_0_4, na.rm = TRUE),
    dry_days_0_4_std_sq = dry_days_0_4_std^2,
    treat_q = ntile(dry_days_0_4_std, 4)
  ) %>%
  dplyr::select(
    CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC,
    PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT,
    INCOME, EDUCATION, TRUST_PEOPLE,
    dry_days_0_4, dry_days_0_4_std, dry_days_0_4_std_sq, treat_q,
    survey_year, FEMALE, BIRTH, prov_nac, log_pop_birth,
    FATHER_BORN_SPAIN, MOTHER_BORN_SPAIN,
    FATHER_EMPLOYMENT, MOTHER_EMPLOYMENT
  )

# ---------------------------------------------------------
# 1) Helpers
# ---------------------------------------------------------
controls_rhs_p04 <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "survey_year", "log_pop_birth"
)

fe_rhs_p04 <- c("BIRTH", "prov_nac")

fit_lq_models_p04 <- function(y, data) {
  f_lin <- as.formula(
    paste0(
      y, " ~ dry_days_0_4_std + ",
      paste(controls_rhs_p04, collapse = " + "),
      " | ", paste(fe_rhs_p04, collapse = " + ")
    )
  )
  
  f_quad <- as.formula(
    paste0(
      y, " ~ dry_days_0_4_std + dry_days_0_4_std_sq + ",
      paste(controls_rhs_p04, collapse = " + "),
      " | ", paste(fe_rhs_p04, collapse = " + ")
    )
  )
  
  list(
    linear = feols(f_lin, data = data, cluster = ~ prov_nac),
    quad   = feols(f_quad, data = data, cluster = ~ prov_nac),
    f_lin  = f_lin,
    f_quad = f_quad
  )
}

fit_q_models_p04 <- function(y, data) {
  f_noc <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + survey_year | ",
      paste(fe_rhs_p04, collapse = " + ")
    )
  )
  
  f_con <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + ",
      paste(controls_rhs_p04, collapse = " + "),
      " | ", paste(fe_rhs_p04, collapse = " + ")
    )
  )
  
  list(
    noc  = feols(f_noc, data = data, cluster = ~ prov_nac),
    con  = feols(f_con, data = data, cluster = ~ prov_nac),
    f_noc = f_noc,
    f_con = f_con
  )
}

# ---------------------------------------------------------
# 2) Outcomes
# ---------------------------------------------------------
outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
outcomes_other <- c("INCOME", "EDUCATION", "TRUST_PEOPLE")

# ---------------------------------------------------------
# 3) LINEAR / QUADRATIC TABLES (same style as main section)
# ---------------------------------------------------------

# Religious
mods_lq_relig_p04 <- lapply(outcomes_relig, fit_lq_models_p04, data = model_data_p04)
names(mods_lq_relig_p04) <- outcomes_relig

models_religion_p04 <- list(
  "Catholic: Linear"              = mods_lq_relig_p04$CATHOLIC$linear,
  "Catholic: Quadratic"           = mods_lq_relig_p04$CATHOLIC$quad,
  "Religious practice: Linear"    = mods_lq_relig_p04$RELIGIOUS_PRACTICE$linear,
  "Religious practice: Quadratic" = mods_lq_relig_p04$RELIGIOUS_PRACTICE$quad,
  "Couple catholic: Linear"       = mods_lq_relig_p04$COUPLE_CATHOLIC$linear,
  "Couple catholic: Quadratic"    = mods_lq_relig_p04$COUPLE_CATHOLIC$quad
)

add_rows_religion_p04 <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Catholic: Linear"              = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$CATHOLIC$f_lin)),  "Linear"),
  "Catholic: Quadratic"           = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$CATHOLIC$f_quad)), "Quadratic"),
  "Religious practice: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$RELIGIOUS_PRACTICE$f_lin)),  "Linear"),
  "Religious practice: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$RELIGIOUS_PRACTICE$f_quad)), "Quadratic"),
  "Couple catholic: Linear"       = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$COUPLE_CATHOLIC$f_lin)),  "Linear"),
  "Couple catholic: Quadratic"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_relig_p04$COUPLE_CATHOLIC$f_quad)), "Quadratic")
)

modelsummary(
  models_religion_p04,
  title = "Placebo 0–4: religious outcomes, linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "dry_days_0_4_std"    = "Dry days 0--4 (std.)",
    "dry_days_0_4_std_sq" = "Dry days 0--4 squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_religion_p04
)

# Political
mods_lq_pol_p04 <- lapply(outcomes_pol, fit_lq_models_p04, data = model_data_p04)
names(mods_lq_pol_p04) <- outcomes_pol

models_politics_p04 <- list(
  "Participation: Linear"    = mods_lq_pol_p04$PARTICIPATION$linear,
  "Participation: Quadratic" = mods_lq_pol_p04$PARTICIPATION$quad,
  "Conservative: Linear"     = mods_lq_pol_p04$CONSERVATIVE_VOTE$linear,
  "Conservative: Quadratic"  = mods_lq_pol_p04$CONSERVATIVE_VOTE$quad,
  "Left-right: Linear"       = mods_lq_pol_p04$LEFT_RIGHT$linear,
  "Left-right: Quadratic"    = mods_lq_pol_p04$LEFT_RIGHT$quad
)

add_rows_politics_p04 <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Participation: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$PARTICIPATION$f_lin)),  "Linear"),
  "Participation: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$PARTICIPATION$f_quad)), "Quadratic"),
  "Conservative: Linear"     = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$CONSERVATIVE_VOTE$f_lin)),  "Linear"),
  "Conservative: Quadratic"  = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$CONSERVATIVE_VOTE$f_quad)), "Quadratic"),
  "Left-right: Linear"       = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$LEFT_RIGHT$f_lin)),  "Linear"),
  "Left-right: Quadratic"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_pol_p04$LEFT_RIGHT$f_quad)), "Quadratic")
)

modelsummary(
  models_politics_p04,
  title = "Placebo 0–4: political outcomes, linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "dry_days_0_4_std"    = "Dry days 0--4 (std.)",
    "dry_days_0_4_std_sq" = "Dry days 0--4 squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_politics_p04
)

# Other
mods_lq_other_p04 <- lapply(outcomes_other, fit_lq_models_p04, data = model_data_p04)
names(mods_lq_other_p04) <- outcomes_other

models_other_p04 <- list(
  "Education: Linear"           = mods_lq_other_p04$EDUCATION$linear,
  "Education: Quadratic"        = mods_lq_other_p04$EDUCATION$quad,
  "Household income: Linear"    = mods_lq_other_p04$INCOME$linear,
  "Household income: Quadratic" = mods_lq_other_p04$INCOME$quad,
  "Trust people: Linear"        = mods_lq_other_p04$TRUST_PEOPLE$linear,
  "Trust people: Quadratic"     = mods_lq_other_p04$TRUST_PEOPLE$quad
)

add_rows_other_p04 <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Education: Linear"           = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$EDUCATION$f_lin)),  "Linear"),
  "Education: Quadratic"        = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$EDUCATION$f_quad)), "Quadratic"),
  "Household income: Linear"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$INCOME$f_lin)),  "Linear"),
  "Household income: Quadratic" = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$INCOME$f_quad)), "Quadratic"),
  "Trust people: Linear"        = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$TRUST_PEOPLE$f_lin)),  "Linear"),
  "Trust people: Quadratic"     = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_lq_other_p04$TRUST_PEOPLE$f_quad)), "Quadratic")
)

modelsummary(
  models_other_p04,
  title = "Placebo 0–4: other outcomes, linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "dry_days_0_4_std"    = "Dry days 0--4 (std.)",
    "dry_days_0_4_std_sq" = "Dry days 0--4 squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_other_p04
)

# ---------------------------------------------------------
# 4) QUARTILE TABLES (same style as quartile section)
# ---------------------------------------------------------

mods_q_relig_p04 <- lapply(outcomes_relig, fit_q_models_p04, data = model_data_p04)
names(mods_q_relig_p04) <- outcomes_relig

add_rows_q_religion_p04 <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Catholic (Q bins)"                      = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$CATHOLIC$f_noc)), "No",
                                               sprintf("%.3f", wald(mods_q_relig_p04$CATHOLIC$con, keep = "treat_q::")[["p"]])),
  "Catholic (Q bins) + Controls"           = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$CATHOLIC$f_con)), "Yes",
                                               sprintf("%.3f", wald(mods_q_relig_p04$CATHOLIC$con, keep = "treat_q::")[["p"]])),
  "Religious practice (Q bins)"            = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$RELIGIOUS_PRACTICE$f_noc)), "No",
                                               sprintf("%.3f", wald(mods_q_relig_p04$RELIGIOUS_PRACTICE$con, keep = "treat_q::")[["p"]])),
  "Religious practice (Q bins) + Controls" = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$RELIGIOUS_PRACTICE$f_con)), "Yes",
                                               sprintf("%.3f", wald(mods_q_relig_p04$RELIGIOUS_PRACTICE$con, keep = "treat_q::")[["p"]])),
  "Catholic partner (Q bins)"              = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$COUPLE_CATHOLIC$f_noc)), "No",
                                               sprintf("%.3f", wald(mods_q_relig_p04$COUPLE_CATHOLIC$con, keep = "treat_q::")[["p"]])),
  "Catholic partner (Q bins) + Controls"   = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_relig_p04$COUPLE_CATHOLIC$f_con)), "Yes",
                                               sprintf("%.3f", wald(mods_q_relig_p04$COUPLE_CATHOLIC$con, keep = "treat_q::")[["p"]])))


modelsummary(
  list(
    "Catholic (Q bins)"                      = mods_q_relig_p04$CATHOLIC$noc,
    "Catholic (Q bins) + Controls"           = mods_q_relig_p04$CATHOLIC$con,
    "Religious practice (Q bins)"            = mods_q_relig_p04$RELIGIOUS_PRACTICE$noc,
    "Religious practice (Q bins) + Controls" = mods_q_relig_p04$RELIGIOUS_PRACTICE$con,
    "Catholic partner (Q bins)"              = mods_q_relig_p04$COUPLE_CATHOLIC$noc,
    "Catholic partner (Q bins) + Controls"   = mods_q_relig_p04$COUPLE_CATHOLIC$con
  ),
  title = "Placebo 0–4: quartile dummies of standardized placebo treatment (religious outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_religion_p04
)

mods_q_pol_p04 <- lapply(outcomes_pol, fit_q_models_p04, data = model_data_p04)
names(mods_q_pol_p04) <- outcomes_pol

add_rows_q_politics_p04 <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Participation (Q bins)"             = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$PARTICIPATION$f_noc)), "No",
                                           sprintf("%.3f", wald(mods_q_pol_p04$PARTICIPATION$con, keep = "treat_q::")[["p"]])),
  "Participation (Q bins) + Controls"  = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$PARTICIPATION$f_con)), "Yes",
                                           sprintf("%.3f", wald(mods_q_pol_p04$PARTICIPATION$con, keep = "treat_q::")[["p"]])),
  "Conservative (Q bins)"              = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$CONSERVATIVE_VOTE$f_noc)), "No",
                                           sprintf("%.3f", wald(mods_q_pol_p04$CONSERVATIVE_VOTE$con, keep = "treat_q::")[["p"]])),
  "Conservative (Q bins) + Controls"   = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$CONSERVATIVE_VOTE$f_con)), "Yes",
                                           sprintf("%.3f", wald(mods_q_pol_p04$CONSERVATIVE_VOTE$con, keep = "treat_q::")[["p"]])),
  "Left-right (Q bins)"                = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$LEFT_RIGHT$f_noc)), "No",
                                           sprintf("%.3f", wald(mods_q_pol_p04$LEFT_RIGHT$con, keep = "treat_q::")[["p"]])),
  "Left-right (Q bins) + Controls"     = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_pol_p04$LEFT_RIGHT$f_con)), "Yes",
                                           sprintf("%.3f", wald(mods_q_pol_p04$LEFT_RIGHT$con, keep = "treat_q::")[["p"]])))


modelsummary(
  list(
    "Participation (Q bins)"             = mods_q_pol_p04$PARTICIPATION$noc,
    "Participation (Q bins) + Controls"  = mods_q_pol_p04$PARTICIPATION$con,
    "Conservative (Q bins)"              = mods_q_pol_p04$CONSERVATIVE_VOTE$noc,
    "Conservative (Q bins) + Controls"   = mods_q_pol_p04$CONSERVATIVE_VOTE$con,
    "Left-right (Q bins)"                = mods_q_pol_p04$LEFT_RIGHT$noc,
    "Left-right (Q bins) + Controls"     = mods_q_pol_p04$LEFT_RIGHT$con
  ),
  title = "Placebo 0–4: quartile dummies of standardized placebo treatment (political outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_politics_p04
)

mods_q_other_p04 <- lapply(outcomes_other, fit_q_models_p04, data = model_data_p04)
names(mods_q_other_p04) <- outcomes_other

add_rows_q_other_p04 <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Income (Q bins)"                  = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$INCOME$f_noc)), "No",
                                         sprintf("%.3f", wald(mods_q_other_p04$INCOME$con, keep = "treat_q::")[["p"]])),
  "Income (Q bins) + Controls"       = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$INCOME$f_con)), "Yes",
                                         sprintf("%.3f", wald(mods_q_other_p04$INCOME$con, keep = "treat_q::")[["p"]])),
  "Education (Q bins)"               = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$EDUCATION$f_noc)), "No",
                                         sprintf("%.3f", wald(mods_q_other_p04$EDUCATION$con, keep = "treat_q::")[["p"]])),
  "Education (Q bins) + Controls"    = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$EDUCATION$f_con)), "Yes",
                                         sprintf("%.3f", wald(mods_q_other_p04$EDUCATION$con, keep = "treat_q::")[["p"]])),
  "Trust people (Q bins)"            = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$TRUST_PEOPLE$f_noc)), "No",
                                         sprintf("%.3f", wald(mods_q_other_p04$TRUST_PEOPLE$con, keep = "treat_q::")[["p"]])),
  "Trust people (Q bins) + Controls" = c(sprintf("%.3f", dv_mean_from_data(model_data_p04, mods_q_other_p04$TRUST_PEOPLE$f_con)), "Yes",
                                         sprintf("%.3f", wald(mods_q_other_p04$TRUST_PEOPLE$con, keep = "treat_q::")[["p"]])))


modelsummary(
  list(
    "Income (Q bins)"                  = mods_q_other_p04$INCOME$noc,
    "Income (Q bins) + Controls"       = mods_q_other_p04$INCOME$con,
    "Education (Q bins)"               = mods_q_other_p04$EDUCATION$noc,
    "Education (Q bins) + Controls"    = mods_q_other_p04$EDUCATION$con,
    "Trust people (Q bins)"            = mods_q_other_p04$TRUST_PEOPLE$noc,
    "Trust people (Q bins) + Controls" = mods_q_other_p04$TRUST_PEOPLE$con
  ),
  title = "Placebo 0–4: quartile dummies of standardized placebo treatment (other outcomes)",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_other_p04
)

# ---------------------------------------------------------
# 5) ADRF PLOTS (same style as quartile section)
# ---------------------------------------------------------
religious_models_q_p04 <- list(
  Catholic           = mods_q_relig_p04$CATHOLIC$con,
  ReligiousPractice  = mods_q_relig_p04$RELIGIOUS_PRACTICE$con,
  CoupleCatholic     = mods_q_relig_p04$COUPLE_CATHOLIC$con
)

political_models_q_p04 <- list(
  Participation = mods_q_pol_p04$PARTICIPATION$con,
  Conservative  = mods_q_pol_p04$CONSERVATIVE_VOTE$con,
  LeftRight     = mods_q_pol_p04$LEFT_RIGHT$con
)

other_models_q_p04 <- list(
  Income      = mods_q_other_p04$INCOME$con,
  Education   = mods_q_other_p04$EDUCATION$con,
  TrustPeople = mods_q_other_p04$TRUST_PEOPLE$con
)

coef_religious_q_p04 <- extract_binned_effects(religious_models_q_p04, "treat_q", q_labels) %>%
  mutate(
    Outcome  = recode(Outcome, !!!pretty_outcomes),
    Outcome  = factor(Outcome, levels = c("Catholic", "Religious practice", "Catholic partner"))
  )

coef_political_q_p04 <- extract_binned_effects(political_models_q_p04, "treat_q", q_labels) %>%
  mutate(
    Outcome  = recode(Outcome, !!!pretty_outcomes),
    Outcome  = factor(Outcome, levels = c("Participation", "Conservative vote", "Left-right scale"))
  )

coef_other_q_p04 <- extract_binned_effects(other_models_q_p04, "treat_q", q_labels) %>%
  mutate(
    Outcome = recode(Outcome, !!!pretty_outcomes)
  )

p_adrf_q_rel_p04 <- plot_adrf(
  df      = build_adrf_data(coef_religious_q_p04),
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

p_adrf_q_pol_p04 <- plot_adrf(
  df      = build_adrf_data(coef_political_q_p04),
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

p_adrf_q_other_p04 <- plot_adrf(
  df      = build_adrf_data(coef_other_q_p04),
  nbins   = 4,
  xlabels = paste0("Q", 1:4),
  ylab    = "Effect relative to Q1"
)

if (!dir.exists("figures")) dir.create("figures")

save_latex_plot(p_adrf_q_rel_p04,   "adrf_quartiles_religious_placebo_0_4")
save_latex_plot(p_adrf_q_pol_p04,   "adrf_quartiles_political_placebo_0_4")
save_latex_plot(p_adrf_q_other_p04, "adrf_quartiles_other_placebo_0_4")



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

# Heterogeneity: North vs South (manual province split from selected map) ----

library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)

# =========================================================
# 1) DEFINE SOUTH PROVINCES MANUALLY
# =========================================================
# Replace this vector with the prov_nac codes of the provinces you want to classify
# as "South" according to your picture.
#
# Example of the old broader south definition:
# Andalucía: 04, 11, 14, 18, 21, 23, 29, 41
# Extremadura: 06, 10
# Murcia: 30
# south_prov_nac <- c(4, 11, 14, 18, 21, 23, 29, 41, 6, 10, 30)

south_prov_nac <- c(
  4, 6, 10, 11, 14, 18, 21, 23, 29, 30, 41
)

# =========================================================
# 2) BUILD MODEL DATA WITH SOUTH DUMMY
# =========================================================

model_data_ns <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    south = as.integer(prov_nac %in% south_prov_nac),
    year  = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4),
    log_pop_birth = log(pop_birth_last_census)
  )

# Quick check
model_data_ns %>%
  count(south, treat_q, name = "n_q") %>%
  arrange(south, treat_q) %>%
  print()

# =========================================================
# 3) QUARTILE HETEROGENEITY MODELS
# Baseline: Q1 in North (south == 0)
# =========================================================

# Religious practice
lpm_fe_q_controls_rel_ns <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~ prov_nac
)

# Conservative vote
lpm_fe_q_controls_con_ns <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~ prov_nac
)

# Left-right
lpm_fe_q_controls_lr_ns <- feols(
  LEFT_RIGHT ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~ prov_nac
)

# Joint tests: are quartile profiles different in North vs South?
wald_q_rel_ns <- wald(
  lpm_fe_q_controls_rel_ns,
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

wald_q_rel_ns
wald_q_con_ns
wald_q_lr_ns

# Optional regression table
modelsummary(
  list(
    "Religious practice – Q × South" = lpm_fe_q_controls_rel_ns,
    "Conservative vote – Q × South"  = lpm_fe_q_controls_con_ns,
    "Left-right – Q × South"         = lpm_fe_q_controls_lr_ns
  ),
  title = "Heterogeneity North vs South – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# =========================================================
# 4) PLOT FUNCTION: QUARTILE ADRF, NORTH VS SOUTH
# =========================================================

plot_adrf_ns_ci <- function(mod,
                            title = "",
                            subtitle = "",
                            file_pdf = NULL,
                            file_png = NULL,
                            level = 0.95) {
  
  beta <- coef(mod)
  V <- vcov(mod)
  z <- qnorm(1 - (1 - level) / 2)
  
  lincomb <- function(terms, weights) {
    ok <- terms %in% names(beta)
    terms_ok <- terms[ok]
    w_ok <- weights[ok]
    
    est <- if (length(terms_ok) == 0) 0 else sum(w_ok * beta[terms_ok])
    
    if (length(terms_ok) == 0) {
      return(list(est = 0, se = NA_real_))
    }
    
    Vsub <- V[terms_ok, terms_ok, drop = FALSE]
    var <- as.numeric(t(w_ok) %*% Vsub %*% w_ok)
    se  <- sqrt(pmax(var, 0))
    
    list(est = est, se = se)
  }
  
  df <- expand.grid(
    group = c("North", "South"),
    bin   = 1:4
  ) |>
    as_tibble() |>
    mutate(
      comb = pmap(list(group, bin), function(g, j) {
        if (g == "North" && j == 1) {
          return(list(terms = character(0), w = numeric(0)))
        }
        if (g == "North" && j != 1) {
          return(list(terms = c(paste0("factor(treat_q)", j)), w = c(1)))
        }
        if (g == "South" && j == 1) {
          return(list(terms = c("south"), w = c(1)))
        }
        return(list(
          terms = c("south",
                    paste0("factor(treat_q)", j),
                    paste0("factor(treat_q)", j, ":south")),
          w = c(1, 1, 1)
        ))
      }),
      est_se  = map(comb, ~ lincomb(.x$terms, .x$w)),
      y       = map_dbl(est_se, "est"),
      se      = map_dbl(est_se, "se"),
      ci_low  = y - z * se,
      ci_high = y + z * se
    ) |>
    select(group, bin, y, se, ci_low, ci_high)
  
  p <- ggplot(df, aes(x = bin, y = y, color = group, fill = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile (baseline = Q1 in North)",
      y = paste0("Estimated level relative to baseline (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)
  
  if (!is.null(file_pdf)) {
    ggsave(
      filename = file_pdf,
      plot = p,
      device = cairo_pdf,
      width = 8,
      height = 5,
      units = "in"
    )
  }
  
  if (!is.null(file_png)) {
    ggsave(
      filename = file_png,
      plot = p,
      width = 8,
      height = 5,
      units = "in",
      dpi = 600,
      bg = "white"
    )
  }
  
  invisible(p)
}

# =========================================================
# 5) SAVE THE SAME 3 QUARTILE HETEROGENEITY PLOTS
# =========================================================

out_dir <- "north_south_heterogeneity_quartile_plots"
dir.create(out_dir, showWarnings = FALSE)

# 1. Conservative vote
p_q_conservative_ns <- plot_adrf_ns_ci(
  mod = lpm_fe_q_controls_con_ns,
  title = "Conservative vote — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in North; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_NorthSouth.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_NorthSouth.png")
)

# 2. Religious practice
p_q_religious_ns <- plot_adrf_ns_ci(
  mod = lpm_fe_q_controls_rel_ns,
  title = "Religious practice — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in North; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Religious_practice_NorthSouth.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Religious_practice_NorthSouth.png")
)

# 3. Left-right
p_q_leftright_ns <- plot_adrf_ns_ci(
  mod = lpm_fe_q_controls_lr_ns,
  title = "Left-right — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in North; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Left_right_NorthSouth.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Left_right_NorthSouth.png")
)

# Catholic
lpm_fe_q_controls_cat_ns <- feols(
  CATHOLIC ~ factor(treat_q) * south +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_ns,
  cluster = ~ prov_nac
)

# Optional joint test
wald_q_cat_ns <- wald(
  lpm_fe_q_controls_cat_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)

wald_q_cat_ns

# 4. Catholic
p_q_catholic_ns <- plot_adrf_ns_ci(
  mod = lpm_fe_q_controls_cat_ns,
  title = "Catholic — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in North; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Catholic_NorthSouth.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Catholic_NorthSouth.png")
)

modelsummary(
  list(
    "Catholic – Q × South"           = lpm_fe_q_controls_cat_ns,
    "Religious practice – Q × South" = lpm_fe_q_controls_rel_ns,
    "Conservative vote – Q × South"  = lpm_fe_q_controls_con_ns,
    "Left-right – Q × South"         = lpm_fe_q_controls_lr_ns
  ),
  title = "Heterogeneity North vs South – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj."
)

# Wald tests for interaction terms
# Joint Wald tests: all Q × south interactions = 0
wald_cat_ns <- fixest::wald(lpm_fe_q_controls_cat_ns, keep = "factor\\(treat_q\\)[234]:south")
wald_rel_ns <- fixest::wald(lpm_fe_q_controls_rel_ns, keep = "factor\\(treat_q\\)[234]:south")
wald_con_ns <- fixest::wald(lpm_fe_q_controls_con_ns, keep = "factor\\(treat_q\\)[234]:south")
wald_lr_ns  <- fixest::wald(lpm_fe_q_controls_lr_ns,  keep = "factor\\(treat_q\\)[234]:south")

get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(w <- fixest::wald(mod, keep = keep_pattern))
  p_line <- grep("p-value =", out, value = TRUE)
  as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1]))
}

p_cat_ns <- round(get_wald_p_print(lpm_fe_q_controls_cat_ns, "factor\\(treat_q\\)[234]:south"), 3)
p_rel_ns <- round(get_wald_p_print(lpm_fe_q_controls_rel_ns, "factor\\(treat_q\\)[234]:south"), 3)
p_con_ns <- round(get_wald_p_print(lpm_fe_q_controls_con_ns, "factor\\(treat_q\\)[234]:south"), 3)
p_lr_ns  <- round(get_wald_p_print(lpm_fe_q_controls_lr_ns,  "factor\\(treat_q\\)[234]:south"), 3)
depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

modelsummary(
  list(
    "Catholic – Q × South"          = lpm_fe_q_controls_cat_ns,
    "Conservative vote – Q × South" = lpm_fe_q_controls_con_ns
  ),
  title = "Heterogeneity North vs South – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = c("Controls", "Mean dep. var.", "Wald test p-value"),
    `Catholic – Q × South`          = c("Yes", depvar_mean(lpm_fe_q_controls_cat_ns), p_cat_ns),
    `Conservative vote – Q × South` = c("Yes", depvar_mean(lpm_fe_q_controls_con_ns), p_con_ns)
  )
)

# Heterogeneity: High vs Low brotherhood density (cofradias per 100k above median) ----

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(fixest)
library(modelsummary)
library(broom)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)

# =========================================================
# 0) ASSUMPTION
# =========================================================
# This code assumes you already have these objects in memory from your main script:
#   - survey
#   - normalize_name()
#   - name_map
#   - prov_code_map
#
# If not, run the earlier harmonization / mapping section first.

# =========================================================
# 1) LOAD BROTHERHOODS BY PROVINCE + POPULATION, BUILD DENSITY
# =========================================================

cofradias_raw <- readr::read_csv(
  "cofradias_y_hermandades_por_provincia.csv",
  locale = readr::locale(encoding = "UTF-8")
)

# INE population file you uploaded (2021 province totals)
pop_2021_raw <- readxl::read_excel(
  "2852.xlsx",
  sheet = "tabla-2852",
  col_names = FALSE
)

# Clean province populations from the uploaded INE table
pop_2021 <- pop_2021_raw %>%
  transmute(
    provincia_raw = as.character(...1),
    population    = suppressWarnings(as.numeric(...2))
  ) %>%
  filter(!is.na(population), !is.na(provincia_raw)) %>%
  mutate(
    provincia_name = stringr::str_trim(stringr::str_remove(provincia_raw, "^\\d{1,2}\\s+")),
    key = normalize_name(provincia_name)
  ) %>%
  left_join(name_map, by = "key") %>%
  mutate(
    provincia_official = dplyr::coalesce(ine_name, provincia_name),
    provincia_norm = normalize_name(provincia_official)
  ) %>%
  left_join(
    prov_code_map %>% dplyr::select(prov_nac, provincia_norm),
    by = "provincia_norm"
  ) %>%
  filter(!is.na(prov_nac)) %>%
  distinct(prov_nac, .keep_all = TRUE) %>%
  transmute(
    prov_nac = as.integer(prov_nac),
    pop_2021 = as.numeric(population)
  )

# Clean brotherhood counts by province
cofradias_prov <- cofradias_raw %>%
  mutate(
    key = normalize_name(provincia)
  ) %>%
  left_join(name_map, by = "key") %>%
  mutate(
    provincia_official = dplyr::coalesce(ine_name, provincia),
    provincia_norm = normalize_name(provincia_official)
  ) %>%
  left_join(
    prov_code_map %>% dplyr::select(prov_nac, provincia_norm),
    by = "provincia_norm"
  ) %>%
  filter(!is.na(prov_nac)) %>%
  group_by(prov_nac) %>%
  summarise(
    n_cofradias = sum(n_cofradias, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(pop_2021, by = "prov_nac") %>%
  mutate(
    cofradias_per_100k = 100000 * n_cofradias / pop_2021
  )

# Median split: strictly above median = high
median_cofradias_100k <- median(cofradias_prov$cofradias_per_100k, na.rm = TRUE)

cofradias_prov <- cofradias_prov %>%
  mutate(
    high_cofradias = as.integer(cofradias_per_100k > median_cofradias_100k)
  )

# Quick check
cofradias_prov %>%
  arrange(desc(cofradias_per_100k)) %>%
  print(n = Inf)

cofradias_prov %>%
  summarise(
    n_prov = n(),
    median_cofradias_100k = median(cofradias_per_100k, na.rm = TRUE),
    mean_cofradias_100k = mean(cofradias_per_100k, na.rm = TRUE)
  ) %>%
  print()

cofradias_prov %>%
  count(high_cofradias, name = "n_provinces") %>%
  print()

# =========================================================
# 2) BUILD MODEL DATA WITH HIGH-COFRADIAS DUMMY
# =========================================================

model_data_cof <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  left_join(
    cofradias_prov %>%
      dplyr::select(prov_nac, cofradias_per_100k, high_cofradias),
    by = "prov_nac"
  ) %>%
  filter(!is.na(high_cofradias)) %>%
  mutate(
    year  = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4),
    log_pop_birth = log(pop_birth_last_census)
  )

# Quick check
model_data_cof %>%
  count(high_cofradias, treat_q, name = "n_q") %>%
  arrange(high_cofradias, treat_q) %>%
  print()

model_data_cof %>%
  count(high_cofradias, name = "n_group") %>%
  print()

# =========================================================
# 3) QUARTILE HETEROGENEITY MODELS
# Baseline: Q1 in LOW-cofradias provinces (high_cofradias == 0)
# =========================================================

# Catholic
lpm_fe_q_controls_cat_cof <- feols(
  CATHOLIC ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Religious practice
lpm_fe_q_controls_rel_cof <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Conservative vote
lpm_fe_q_controls_con_cof <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Left-right
lpm_fe_q_controls_lr_cof <- feols(
  LEFT_RIGHT ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# =========================================================
# 4) JOINT WALD TESTS: ARE QUARTILE PROFILES DIFFERENT
#    ACROSS LOW- VS HIGH-COFRADIAS PROVINCES?
# =========================================================

wald_cat_cof <- fixest::wald(
  lpm_fe_q_controls_cat_cof,
  keep = "factor\\(treat_q\\)[234]:high_cofradias"
)

wald_rel_cof <- fixest::wald(
  lpm_fe_q_controls_rel_cof,
  keep = "factor\\(treat_q\\)[234]:high_cofradias"
)

wald_con_cof <- fixest::wald(
  lpm_fe_q_controls_con_cof,
  keep = "factor\\(treat_q\\)[234]:high_cofradias"
)

wald_lr_cof <- fixest::wald(
  lpm_fe_q_controls_lr_cof,
  keep = "factor\\(treat_q\\)[234]:high_cofradias"
)

wald_cat_cof
wald_rel_cof
wald_con_cof
wald_lr_cof

# Helper to extract p-value from printed fixest::wald output
get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(w <- fixest::wald(mod, keep = keep_pattern))
  p_line <- grep("p-value =", out, value = TRUE)
  as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1]))
}

p_cat_cof <- round(get_wald_p_print(lpm_fe_q_controls_cat_cof, "factor\\(treat_q\\)[234]:high_cofradias"), 3)
p_rel_cof <- round(get_wald_p_print(lpm_fe_q_controls_rel_cof, "factor\\(treat_q\\)[234]:high_cofradias"), 3)
p_con_cof <- round(get_wald_p_print(lpm_fe_q_controls_con_cof, "factor\\(treat_q\\)[234]:high_cofradias"), 3)
p_lr_cof  <- round(get_wald_p_print(lpm_fe_q_controls_lr_cof,  "factor\\(treat_q\\)[234]:high_cofradias"), 3)

# Mean DV on estimation sample
depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

# =========================================================
# 5) TABLE
# =========================================================

modelsummary(
  list(
    "Catholic – Q × High cofradias"           = lpm_fe_q_controls_cat_cof,
    "Religious practice – Q × High cofradias" = lpm_fe_q_controls_rel_cof,
    "Conservative vote – Q × High cofradias"  = lpm_fe_q_controls_con_cof,
    "Left-right – Q × High cofradias"         = lpm_fe_q_controls_lr_cof
  ),
  title = "Heterogeneity by cofradias density – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = c("Controls", "Mean dep. var.", "Wald test p-value"),
    `Catholic – Q × High cofradias`           = c("Yes", depvar_mean(lpm_fe_q_controls_cat_cof), p_cat_cof),
    `Religious practice – Q × High cofradias` = c("Yes", depvar_mean(lpm_fe_q_controls_rel_cof), p_rel_cof),
    `Conservative vote – Q × High cofradias`  = c("Yes", depvar_mean(lpm_fe_q_controls_con_cof), p_con_cof),
    `Left-right – Q × High cofradias`         = c("Yes", depvar_mean(lpm_fe_q_controls_lr_cof), p_lr_cof)
  )
)

# Optional shorter table like the one you had at the end
modelsummary(
  list(
    "Catholic – Q × High cofradias"          = lpm_fe_q_controls_cat_cof,
    "Conservative vote – Q × High cofradias" = lpm_fe_q_controls_con_cof
  ),
  title = "Heterogeneity by cofradias density – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = c("Controls", "Mean dep. var.", "Wald test p-value"),
    `Catholic – Q × High cofradias`          = c("Yes", depvar_mean(lpm_fe_q_controls_cat_cof), p_cat_cof),
    `Conservative vote – Q × High cofradias` = c("Yes", depvar_mean(lpm_fe_q_controls_con_cof), p_con_cof)
  )
)

# =========================================================
# 6) PLOT FUNCTION: QUARTILE ADRF, LOW VS HIGH COFRADIAS
# =========================================================

plot_adrf_cof_ci <- function(mod,
                             title = "",
                             subtitle = "",
                             file_pdf = NULL,
                             file_png = NULL,
                             level = 0.95) {
  
  beta <- coef(mod)
  V <- vcov(mod)
  z <- qnorm(1 - (1 - level) / 2)
  
  lincomb <- function(terms, weights) {
    ok <- terms %in% names(beta)
    terms_ok <- terms[ok]
    w_ok <- weights[ok]
    
    est <- if (length(terms_ok) == 0) 0 else sum(w_ok * beta[terms_ok])
    
    if (length(terms_ok) == 0) {
      return(list(est = 0, se = NA_real_))
    }
    
    Vsub <- V[terms_ok, terms_ok, drop = FALSE]
    var <- as.numeric(t(w_ok) %*% Vsub %*% w_ok)
    se  <- sqrt(pmax(var, 0))
    
    list(est = est, se = se)
  }
  
  df <- expand.grid(
    group = c("Low cofradias", "High cofradias"),
    bin   = 1:4
  ) |>
    as_tibble() |>
    mutate(
      comb = pmap(list(group, bin), function(g, j) {
        if (g == "Low cofradias" && j == 1) {
          return(list(terms = character(0), w = numeric(0)))
        }
        if (g == "Low cofradias" && j != 1) {
          return(list(terms = c(paste0("factor(treat_q)", j)), w = c(1)))
        }
        if (g == "High cofradias" && j == 1) {
          return(list(terms = c("high_cofradias"), w = c(1)))
        }
        return(list(
          terms = c("high_cofradias",
                    paste0("factor(treat_q)", j),
                    paste0("factor(treat_q)", j, ":high_cofradias")),
          w = c(1, 1, 1)
        ))
      }),
      est_se  = map(comb, ~ lincomb(.x$terms, .x$w)),
      y       = map_dbl(est_se, "est"),
      se      = map_dbl(est_se, "se"),
      ci_low  = y - z * se,
      ci_high = y + z * se
    ) |>
    select(group, bin, y, se, ci_low, ci_high)
  
  p <- ggplot(df, aes(x = bin, y = y, color = group, fill = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      data = dplyr::filter(df, !is.na(ci_low), !is.na(ci_high)),
      aes(ymin = ci_low, ymax = ci_high),
      alpha = 0.18,
      color = NA,
      inherit.aes = TRUE
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile (baseline = Q1 in low-cofradias provinces)",
      y = paste0("Estimated level relative to baseline (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)
  
  if (!is.null(file_pdf)) {
    ggsave(
      filename = file_pdf,
      plot = p,
      device = cairo_pdf,
      width = 8,
      height = 5,
      units = "in"
    )
  }
  
  if (!is.null(file_png)) {
    ggsave(
      filename = file_png,
      plot = p,
      width = 8,
      height = 5,
      units = "in",
      dpi = 600,
      bg = "white"
    )
  }
  
  invisible(p)
}

# =========================================================
# 7) SAVE THE SAME 4 QUARTILE HETEROGENEITY PLOTS
# =========================================================

out_dir <- "cofradias_density_heterogeneity_quartile_plots"
dir.create(out_dir, showWarnings = FALSE)

# 1. Conservative vote
p_q_conservative_cof <- plot_adrf_cof_ci(
  mod = lpm_fe_q_controls_con_cof,
  title = "Conservative vote — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in low-cofradias provinces; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_CofradiasDensity.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_CofradiasDensity.png")
)

# 2. Religious practice
p_q_religious_cof <- plot_adrf_cof_ci(
  mod = lpm_fe_q_controls_rel_cof,
  title = "Religious practice — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in low-cofradias provinces; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Religious_practice_CofradiasDensity.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Religious_practice_CofradiasDensity.png")
)

# 3. Left-right
p_q_leftright_cof <- plot_adrf_cof_ci(
  mod = lpm_fe_q_controls_lr_cof,
  title = "Left-right — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in low-cofradias provinces; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Left_right_CofradiasDensity.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Left_right_CofradiasDensity.png")
)

# 4. Catholic
p_q_catholic_cof <- plot_adrf_cof_ci(
  mod = lpm_fe_q_controls_cat_cof,
  title = "Catholic — Quartile heterogeneity",
  subtitle = "Baseline: Q1 in low-cofradias provinces; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Catholic_CofradiasDensity.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Catholic_CofradiasDensity.png")
)



# Heterogeneity: Parental Catholicism (at least one parent Catholic) --------

library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)

# =========================================================
# 1) BUILD MODEL DATA WITH PARENTAL CATHOLICISM DUMMY
#    Definition: at least one parent is Catholic
# =========================================================

model_data_pc <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  ) %>%
  mutate(
    f_cath = as.numeric(FATHER_CATHOLIC),
    m_cath = as.numeric(MOTHER_CATHOLIC),
    
    # At least one parent Catholic, only defined if both parental statuses observed
    parent_cath = if_else(
      !is.na(f_cath) & !is.na(m_cath),
      as.integer(f_cath == 1 | m_cath == 1),
      NA_integer_
    ),
    
    year  = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4),
    log_pop_birth = log(pop_birth_last_census)
  ) %>%
  filter(!is.na(parent_cath))

# Quick checks
model_data_pc %>%
  count(parent_cath, treat_q, name = "n_q") %>%
  arrange(parent_cath, treat_q) %>%
  print()

model_data_pc %>%
  count(parent_cath, name = "n_parent_cath") %>%
  print()

# =========================================================
# 2) QUARTILE HETEROGENEITY MODELS
#    Baseline: Q1 among respondents with no Catholic parent
# =========================================================

# Catholic
lpm_fe_q_controls_cat_pc <- feols(
  CATHOLIC ~ factor(treat_q) * parent_cath +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_pc,
  cluster = ~ prov_nac
)

# Religious practice
lpm_fe_q_controls_rel_pc <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * parent_cath +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_pc,
  cluster = ~ prov_nac
)

# Conservative vote
lpm_fe_q_controls_con_pc <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * parent_cath +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_pc,
  cluster = ~ prov_nac
)

# Left-right
lpm_fe_q_controls_lr_pc <- feols(
  LEFT_RIGHT ~ factor(treat_q) * parent_cath +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT +
    survey_year + log_pop_birth |
    BIRTH + prov_nac,
  data    = model_data_pc,
  cluster = ~ prov_nac
)

# =========================================================
# 3) JOINT WALD TESTS: ARE QUARTILE PROFILES DIFFERENT
#    ACROSS PARENTAL CATHOLICISM GROUPS?
# =========================================================

wald_cat_pc <- fixest::wald(
  lpm_fe_q_controls_cat_pc,
  keep = "factor\\(treat_q\\)[234]:parent_cath"
)

wald_rel_pc <- fixest::wald(
  lpm_fe_q_controls_rel_pc,
  keep = "factor\\(treat_q\\)[234]:parent_cath"
)

wald_con_pc <- fixest::wald(
  lpm_fe_q_controls_con_pc,
  keep = "factor\\(treat_q\\)[234]:parent_cath"
)

wald_lr_pc <- fixest::wald(
  lpm_fe_q_controls_lr_pc,
  keep = "factor\\(treat_q\\)[234]:parent_cath"
)

# Helper to extract p-value from printed fixest::wald output
get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(w <- fixest::wald(mod, keep = keep_pattern))
  p_line <- grep("p-value =", out, value = TRUE)
  as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1]))
}

p_cat_pc <- round(get_wald_p_print(lpm_fe_q_controls_cat_pc, "factor\\(treat_q\\)[234]:parent_cath"), 3)
p_rel_pc <- round(get_wald_p_print(lpm_fe_q_controls_rel_pc, "factor\\(treat_q\\)[234]:parent_cath"), 3)
p_con_pc <- round(get_wald_p_print(lpm_fe_q_controls_con_pc, "factor\\(treat_q\\)[234]:parent_cath"), 3)
p_lr_pc  <- round(get_wald_p_print(lpm_fe_q_controls_lr_pc,  "factor\\(treat_q\\)[234]:parent_cath"), 3)

# Mean DV on estimation sample
depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

# Print Wald tests
wald_cat_pc
wald_rel_pc
wald_con_pc
wald_lr_pc

# =========================================================
# 4) TABLE
# =========================================================

modelsummary(
  list(
    "Catholic – Q × Parent Catholic"           = lpm_fe_q_controls_cat_pc,
    "Religious practice – Q × Parent Catholic" = lpm_fe_q_controls_rel_pc,
    "Conservative vote – Q × Parent Catholic"  = lpm_fe_q_controls_con_pc,
    "Left-right – Q × Parent Catholic"         = lpm_fe_q_controls_lr_pc
  ),
  title = "Heterogeneity by parental Catholicism – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = tibble::tibble(
    term = c("Controls", "Mean dep. var.", "Wald test p-value"),
    `Catholic – Q × Parent Catholic`           = c("Yes", depvar_mean(lpm_fe_q_controls_cat_pc), p_cat_pc),
    `Religious practice – Q × Parent Catholic` = c("Yes", depvar_mean(lpm_fe_q_controls_rel_pc), p_rel_pc),
    `Conservative vote – Q × Parent Catholic`  = c("Yes", depvar_mean(lpm_fe_q_controls_con_pc), p_con_pc),
    `Left-right – Q × Parent Catholic`         = c("Yes", depvar_mean(lpm_fe_q_controls_lr_pc), p_lr_pc)
  )
)

# =========================================================
# 5) PLOT FUNCTION: QUARTILE ADRF, PARENTAL CATHOLICISM
# =========================================================

plot_adrf_pc_ci <- function(mod,
                            title = "",
                            subtitle = "",
                            file_pdf = NULL,
                            file_png = NULL,
                            level = 0.95) {
  
  beta <- coef(mod)
  V <- vcov(mod)
  z <- qnorm(1 - (1 - level) / 2)
  
  lincomb <- function(terms, weights) {
    ok <- terms %in% names(beta)
    terms_ok <- terms[ok]
    w_ok <- weights[ok]
    
    est <- if (length(terms_ok) == 0) 0 else sum(w_ok * beta[terms_ok])
    
    if (length(terms_ok) == 0) {
      return(list(est = 0, se = NA_real_))
    }
    
    Vsub <- V[terms_ok, terms_ok, drop = FALSE]
    var <- as.numeric(t(w_ok) %*% Vsub %*% w_ok)
    se  <- sqrt(pmax(var, 0))
    
    list(est = est, se = se)
  }
  
  df <- expand.grid(
    group = c("No Catholic parent", ">=1 Catholic parent"),
    bin   = 1:4
  ) |>
    as_tibble() |>
    mutate(
      comb = pmap(list(group, bin), function(g, j) {
        if (g == "No Catholic parent" && j == 1) {
          return(list(terms = character(0), w = numeric(0)))
        }
        if (g == "No Catholic parent" && j != 1) {
          return(list(terms = c(paste0("factor(treat_q)", j)), w = c(1)))
        }
        if (g == ">=1 Catholic parent" && j == 1) {
          return(list(terms = c("parent_cath"), w = c(1)))
        }
        return(list(
          terms = c("parent_cath",
                    paste0("factor(treat_q)", j),
                    paste0("factor(treat_q)", j, ":parent_cath")),
          w = c(1, 1, 1)
        ))
      }),
      est_se  = map(comb, ~ lincomb(.x$terms, .x$w)),
      y       = map_dbl(est_se, "est"),
      se      = map_dbl(est_se, "se"),
      ci_low  = y - z * se,
      ci_high = y + z * se
    ) |>
    select(group, bin, y, se, ci_low, ci_high)
  
  p <- ggplot(df, aes(x = bin, y = y, color = group, fill = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      data = dplyr::filter(df, !is.na(ci_low), !is.na(ci_high)),
      aes(ymin = ci_low, ymax = ci_high),
      alpha = 0.18,
      color = NA,
      inherit.aes = TRUE
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile (baseline = Q1 among respondents with no Catholic parent)",
      y = paste0("Estimated level relative to baseline (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)
  
  if (!is.null(file_pdf)) {
    ggsave(
      filename = file_pdf,
      plot = p,
      device = cairo_pdf,
      width = 8,
      height = 5,
      units = "in"
    )
  }
  
  if (!is.null(file_png)) {
    ggsave(
      filename = file_png,
      plot = p,
      width = 8,
      height = 5,
      units = "in",
      dpi = 600,
      bg = "white"
    )
  }
  
  invisible(p)
}

# =========================================================
# 6) SAVE THE SAME QUARTILE HETEROGENEITY PLOTS
# =========================================================

out_dir <- "parent_catholicism_heterogeneity_quartile_plots"
dir.create(out_dir, showWarnings = FALSE)

# 1. Catholic
p_q_catholic_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_cat_pc,
  title = "Catholic — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Catholic_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Catholic_ParentCath.png")
)

# 2. Religious practice
p_q_religious_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_rel_pc,
  title = "Religious practice — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Religious_practice_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Religious_practice_ParentCath.png")
)

# 3. Conservative vote
p_q_conservative_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_con_pc,
  title = "Conservative vote — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_ParentCath.png")
)

# 4. Left-right
p_q_leftright_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_lr_pc,
  title = "Left-right — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; FE: birth year + province; controls included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Left_right_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Left_right_ParentCath.png")
)


# MADESTAM GRAPH ----------------------------------------------------------

# ---- PATH TO PLACEBO FILE (SET THIS) ----
placebo_path <- "C:/Users/Saúl/Desktop/Semana Santa project/Grid precipitation/province_placebo_all_shifts_long_8dayblocks_option2_dropCrossYear.csv"  # <-- change

stopifnot(file.exists(placebo_path))

library(dplyr)
library(readr)

survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

# --- 0) Build a clean mapping prov_nac -> provincia_norm (unique, numeric key)
prov_map <- prov_code_map %>%
  dplyr::select(prov_nac, provincia_norm) %>%
  dplyr::mutate(prov_nac = as.integer(prov_nac)) %>%
  dplyr::distinct(prov_nac, .keep_all = TRUE)

stopifnot(all(c("prov_nac","provincia_norm") %in% names(prov_map)))

# --- 1) Construct model_data WITHOUT JOIN (use match)
model_data <- survey %>%
  mutate(
    respondent_id   = row_number(),
    prov_nac        = as.integer(prov_nac),
    childhood_start = as.integer(BIRTH + 5L),
    childhood_end   = as.integer(BIRTH + 18L),
    provincia_norm  = prov_map$provincia_norm[ match(as.integer(prov_nac), prov_map$prov_nac) ]
  ) %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(provincia_norm),
    !is.na(childhood_start),
    !is.na(childhood_end)
  ) %>%
  mutate(
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    log_pop_birth      = log(pop_birth_last_census),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE)
  ) %>%
  dplyr::select(
    respondent_id, provincia_norm, childhood_start, childhood_end,
    CATHOLIC, childhood_total_dry_days, childhood_total_dry_days_std,
    survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC,
    birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP,
    MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE,
    TRUST_PEOPLE, INCOME, EDUCATION, RELIGIOUS_PRACTICE, PARTICIPATION, SIZE_TOWN,
    dry_days_5_9, dry_days_10_14, dry_days_15_18, PP_VOTE,
    pop_birth_last_census, log_pop_birth
  )

# --- 2) Sanity checks (run once)
stopifnot("provincia_norm" %in% names(model_data))
table(is.na(model_data$provincia_norm))

# ============================================================
# C) MADESTAM GRAPHS (paper-style, WIDE 3-panels) + 2 FIXES
#   - 3 sets × 3 outcomes (same outcomes as main regressions)
#   - one dot per shift; vertical line = real estimate
#   - DOT COLOR (both effects + p-values): "#1f77b4"
#   - Effect = "average standardized effect" under quadratic spec:
#       Δy = y(1SD) − y(0) = β1 + β2
#   - FIX #1: standardization uses mean/sd from the *estimation sample*
#            (complete cases on RHS), and is used for BOTH real + placebo
#   - FIX #2: ONE p only (Madestam-style RI share):
#       p_abs = share of placebo |effect| >= |real effect|
#     (used in BOTH effects graph and p-values graph subtitles)
#   - WIDE STYLE:
#       title = outcome; subtitle = "p(|.|)>=..."; y-axis only on left panel
# ============================================================

library(readr)
library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)

DOT_COL <- "#1f77b4"

# ----------------------------
# 0) Load data + build model_data (NO join; use match)
#    Requires: prov_code_map already created earlier
# ----------------------------
survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

prov_map <- prov_code_map %>%
  dplyr::select(prov_nac, provincia_norm) %>%
  dplyr::mutate(prov_nac = as.integer(prov_nac)) %>%
  dplyr::distinct(prov_nac, .keep_all = TRUE)

stopifnot(all(c("prov_nac","provincia_norm") %in% names(prov_map)))

model_data <- survey %>%
  mutate(
    respondent_id   = row_number(),
    prov_nac        = as.integer(prov_nac),
    childhood_start = as.integer(BIRTH + 5L),
    childhood_end   = as.integer(BIRTH + 18L),
    provincia_norm  = prov_map$provincia_norm[match(as.integer(prov_nac), prov_map$prov_nac)],
    log_pop_birth   = log(pop_birth_last_census)
  ) %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(provincia_norm),
    !is.na(childhood_start),
    !is.na(childhood_end),
    !is.na(log_pop_birth)
  )

stopifnot("provincia_norm" %in% names(model_data))
print(table(is.na(model_data$provincia_norm)))

# ----------------------------
# Helpers: coef lookup + combo beta1+beta2 (delta method)
# ----------------------------
find_coef_name <- function(bnames, patterns){
  for(p in patterns){
    idx <- which(grepl(p, bnames))
    if(length(idx) > 0) return(bnames[idx[1]])
  }
  NA_character_
}

combo_from_model <- function(m, lin_patterns, quad_patterns){
  b  <- coef(m)
  V  <- vcov(m)
  bn <- names(b)
  
  lin_name  <- find_coef_name(bn, lin_patterns)
  quad_name <- find_coef_name(bn, quad_patterns)
  
  if (is.na(lin_name))  stop("Could not find linear term. Coef names:\n", paste(bn, collapse=", "))
  if (is.na(quad_name)) stop("Could not find quadratic term. Coef names:\n", paste(bn, collapse=", "))
  
  beta_lin  <- unname(b[lin_name])
  beta_quad <- unname(b[quad_name])
  
  v11 <- V[lin_name,  lin_name]
  v22 <- V[quad_name, quad_name]
  v12 <- V[lin_name,  quad_name]
  
  beta_combo <- beta_lin + beta_quad
  se_combo   <- sqrt(as.numeric(v11 + v22 + 2*v12))
  t_combo    <- beta_combo / se_combo
  p_combo    <- 2 * pt(abs(t_combo), df = df.residual(m), lower.tail = FALSE)
  
  list(beta_combo = beta_combo, se_combo = se_combo, p_combo = p_combo)
}

# ----------------------------
# Wide-friendly plotting style
# ----------------------------
madestam_theme_wide <- function(){
  theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 9, margin = margin(b = 6)),
      axis.title.x = element_text(size = 9, margin = margin(t = 6)),
      axis.title.y = element_text(size = 9, margin = margin(r = 6)),
      axis.text = element_text(size = 8),
      plot.margin = margin(6, 6, 6, 6)
    )
}

strip_y <- function(p){
  p + theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )
}

# ============================================================
# C1) EFFECTS GRAPH: ranked placebo effects, subtitle has ONE p
#     p_abs = share of placebo |effect| >= |real|
# ============================================================
run_madestam_rankplot_combo <- function(
    outcome_var,
    model_data,
    placebo_path,
    normalize_name,
    name_map,
    raw_expo_var = "childhood_total_dry_days",
    fe_birth = "BIRTH",
    fe_prov  = "prov_nac",
    cluster_var = "prov_nac"
){
  
  req <- c(
    "respondent_id","provincia_norm","childhood_start","childhood_end",
    outcome_var, raw_expo_var,
    "BIRTH","prov_nac","survey_year",
    "FEMALE","FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN",
    "FATHER_EMPLOYMENT","MOTHER_EMPLOYMENT","log_pop_birth"
  )
  miss <- setdiff(req, names(model_data))
  if(length(miss) > 0) stop("model_data missing: ", paste(miss, collapse=", "))
  
  md <- as.data.table(copy(model_data))
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  # exact estimation sample (complete cases on RHS)
  base_dt <- md[complete.cases(md[, ..req]), ..req]
  setkey(base_dt, respondent_id)
  
  # FIX #1: mean/sd on estimation sample
  real_raw_mean <- mean(base_dt[[raw_expo_var]], na.rm = TRUE)
  real_raw_sd   <- sd(base_dt[[raw_expo_var]], na.rm = TRUE)
  stopifnot(is.finite(real_raw_sd) && real_raw_sd > 0)
  
  # --- Load placebo from file (your CSV/Excel-exported-to-CSV) ---
  placebo_raw <- read_csv(placebo_path, locale = locale(encoding="UTF-8"), show_col_types = FALSE)
  stopifnot(all(c("provincia","year","placebo_dry_days_10","shift_days") %in% names(placebo_raw)))
  placebo_dt <- as.data.table(placebo_raw)
  
  placebo_dt[, key := normalize_name(provincia)]
  placebo_dt <- merge(placebo_dt, as.data.table(name_map), by="key", all.x=TRUE)
  placebo_dt[, provincia_official := fifelse(is.na(ine_name), provincia, ine_name)]
  placebo_dt[, provincia_norm := normalize_name(provincia_official)]
  placebo_dt[, year := as.integer(year)]
  placebo_dt[, shift_days := as.integer(shift_days)]
  placebo_dt[, placebo_dry_days_10 := as.numeric(placebo_dry_days_10)]
  
  placebo_yearly <- placebo_dt[
    , .(placebo_year_total = sum(placebo_dry_days_10, na.rm=TRUE)),
    by = .(provincia_norm, year, shift_days)
  ]
  setkey(placebo_yearly, provincia_norm, year)
  shift_values <- sort(unique(placebo_yearly$shift_days))
  
  # --- Madestam assignment: childhood panel then sum by shift ---
  child_panel <- base_dt[
    , .(year = seq.int(childhood_start, childhood_end)),
    by = .(respondent_id, provincia_norm)
  ]
  setkey(child_panel, provincia_norm, year)
  
  tmp <- placebo_yearly[
    child_panel,
    on = .(provincia_norm, year),
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  expo_dt <- tmp[
    , .(placebo_childhood_total = sum(placebo_year_total, na.rm=TRUE)),
    by = .(respondent_id, shift_days)
  ]
  
  full_grid <- CJ(respondent_id = unique(base_dt$respondent_id),
                  shift_days    = shift_values,
                  unique = TRUE)
  
  reg_dt <- merge(full_grid, expo_dt, by=c("respondent_id","shift_days"), all.x=TRUE)
  reg_dt[is.na(placebo_childhood_total), placebo_childhood_total := 0]
  reg_dt <- merge(reg_dt, base_dt, by="respondent_id", all.x=TRUE)
  
  # FIX #1 continued: local std treatment for REAL regression
  reg_dt[, treat_std_local := (get(raw_expo_var) - real_raw_mean) / real_raw_sd]
  
  # --- Real regression (controls + quadratic) ---
  real_slice <- reg_dt[shift_days == shift_values[1]]
  
  fml_real <- as.formula(paste0(
    outcome_var, " ~ treat_std_local + I(treat_std_local^2) + ",
    "FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + ",
    "FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | ",
    fe_birth, " + ", fe_prov
  ))
  
  real_m <- feols(fml_real, data = real_slice, cluster = cluster_fml)
  
  real_combo <- combo_from_model(
    real_m,
    lin_patterns  = c("^treat_std_local$"),
    quad_patterns = c("^I\\(treat_std_local\\^2\\)$", "^I\\(treat_std_local\\^2\\)", "treat_std_local\\^2")
  )
  
  # --- Placebo regressions per shift ---
  fml_pl <- as.formula(paste0(
    outcome_var, " ~ placebo_std + I(placebo_std^2) + ",
    "FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + ",
    "FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | ",
    fe_birth, " + ", fe_prov
  ))
  
  estimate_shift <- function(s){
    df_s <- reg_dt[shift_days == s]
    df_s[, placebo_std := (placebo_childhood_total - real_raw_mean) / real_raw_sd]
    
    m <- feols(fml_pl, data = df_s, cluster = cluster_fml)
    
    cmb <- combo_from_model(
      m,
      lin_patterns  = c("^placebo_std$"),
      quad_patterns = c("^I\\(placebo_std\\^2\\)$", "^I\\(placebo_std\\^2\\)", "placebo_std\\^2")
    )
    
    data.table(shift_days = s, beta_combo = cmb$beta_combo, p_combo = cmb$p_combo, n = nobs(m))
  }
  
  placebo_res <- rbindlist(lapply(shift_values, estimate_shift), fill=TRUE)
  
  # FIX #2 (requested): ONE p only = absolute tail share
  p_abs <- mean(abs(placebo_res$beta_combo) >= abs(real_combo$beta_combo), na.rm = TRUE)
  
  # ranked effect plot
  rank_df <- placebo_res[!is.na(beta_combo)][order(beta_combo)]
  rank_df[, rank := seq_len(.N)]
  
  p_rank <- ggplot(rank_df, aes(x = beta_combo, y = rank)) +
    geom_point(size = 1.35, alpha = 0.85, color = DOT_COL) +
    geom_vline(xintercept = real_combo$beta_combo, linewidth = 1.05) +
    labs(
      title = outcome_var,
      subtitle = sprintf("share placebo effects greater or equal than real: %.3f", p_abs),
      x = "Average standardized effect (β1 + β2)",
      y = "Placebo rank"
    ) +
    madestam_theme_wide()
  
  list(
    placebo_res = placebo_res,
    real_model  = real_m,
    real_combo  = real_combo,
    p_abs       = p_abs,
    plot_rank   = p_rank
  )
}

# ============================================================
# C2) P-VALUES GRAPH: ranked placebo p-values, subtitle has ONE p
#     p_abs (same definition as above, but computed on EFFECTS),
#     shown again for comparability across both graph types.
# ============================================================
run_madestam_rankplot_pvals <- function(
    outcome_var,
    model_data,
    placebo_path,
    normalize_name,
    name_map,
    raw_expo_var = "childhood_total_dry_days",
    fe_birth = "BIRTH",
    fe_prov  = "prov_nac",
    cluster_var = "prov_nac"
){
  
  req <- c(
    "respondent_id","provincia_norm","childhood_start","childhood_end",
    outcome_var, raw_expo_var,
    "BIRTH","prov_nac","survey_year",
    "FEMALE","FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN",
    "FATHER_EMPLOYMENT","MOTHER_EMPLOYMENT","log_pop_birth"
  )
  miss <- setdiff(req, names(model_data))
  if(length(miss) > 0) stop("model_data missing: ", paste(miss, collapse=", "))
  
  md <- as.data.table(copy(model_data))
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  # exact estimation sample (complete cases on RHS)
  base_dt <- md[complete.cases(md[, ..req]), ..req]
  setkey(base_dt, respondent_id)
  
  # FIX #1: mean/sd on estimation sample
  real_raw_mean <- mean(base_dt[[raw_expo_var]], na.rm = TRUE)
  real_raw_sd   <- sd(base_dt[[raw_expo_var]], na.rm = TRUE)
  stopifnot(is.finite(real_raw_sd) && real_raw_sd > 0)
  
  # --- Load placebo ---
  placebo_raw <- read_csv(placebo_path, locale = locale(encoding="UTF-8"), show_col_types = FALSE)
  stopifnot(all(c("provincia","year","placebo_dry_days_10","shift_days") %in% names(placebo_raw)))
  placebo_dt <- as.data.table(placebo_raw)
  
  placebo_dt[, key := normalize_name(provincia)]
  placebo_dt <- merge(placebo_dt, as.data.table(name_map), by="key", all.x=TRUE)
  placebo_dt[, provincia_official := fifelse(is.na(ine_name), provincia, ine_name)]
  placebo_dt[, provincia_norm := normalize_name(provincia_official)]
  placebo_dt[, year := as.integer(year)]
  placebo_dt[, shift_days := as.integer(shift_days)]
  placebo_dt[, placebo_dry_days_10 := as.numeric(placebo_dry_days_10)]
  
  placebo_yearly <- placebo_dt[
    , .(placebo_year_total = sum(placebo_dry_days_10, na.rm=TRUE)),
    by = .(provincia_norm, year, shift_days)
  ]
  setkey(placebo_yearly, provincia_norm, year)
  shift_values <- sort(unique(placebo_yearly$shift_days))
  
  # --- Madestam assignment ---
  child_panel <- base_dt[
    , .(year = seq.int(childhood_start, childhood_end)),
    by = .(respondent_id, provincia_norm)
  ]
  setkey(child_panel, provincia_norm, year)
  
  tmp <- placebo_yearly[
    child_panel,
    on = .(provincia_norm, year),
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  expo_dt <- tmp[
    , .(placebo_childhood_total = sum(placebo_year_total, na.rm=TRUE)),
    by = .(respondent_id, shift_days)
  ]
  
  full_grid <- CJ(respondent_id = unique(base_dt$respondent_id),
                  shift_days    = shift_values,
                  unique = TRUE)
  
  reg_dt <- merge(full_grid, expo_dt, by=c("respondent_id","shift_days"), all.x=TRUE)
  reg_dt[is.na(placebo_childhood_total), placebo_childhood_total := 0]
  reg_dt <- merge(reg_dt, base_dt, by="respondent_id", all.x=TRUE)
  
  # local standardized treatment for REAL regression
  reg_dt[, treat_std_local := (get(raw_expo_var) - real_raw_mean) / real_raw_sd]
  
  # --- Real regression ---
  real_slice <- reg_dt[shift_days == shift_values[1]]
  
  fml_real <- as.formula(paste0(
    outcome_var, " ~ treat_std_local + I(treat_std_local^2) + ",
    "FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + ",
    "FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | ",
    fe_birth, " + ", fe_prov
  ))
  
  real_m <- feols(fml_real, data = real_slice, cluster = cluster_fml)
  
  real_combo <- combo_from_model(
    real_m,
    lin_patterns  = c("^treat_std_local$"),
    quad_patterns = c("^I\\(treat_std_local\\^2\\)$", "^I\\(treat_std_local\\^2\\)", "treat_std_local\\^2")
  )
  real_p <- real_combo$p_combo
  
  # --- Placebo regressions per shift ---
  fml_pl <- as.formula(paste0(
    outcome_var, " ~ placebo_std + I(placebo_std^2) + ",
    "FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + ",
    "FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + survey_year + log_pop_birth | ",
    fe_birth, " + ", fe_prov
  ))
  
  estimate_shift <- function(s){
    df_s <- reg_dt[shift_days == s]
    df_s[, placebo_std := (placebo_childhood_total - real_raw_mean) / real_raw_sd]
    
    m <- feols(fml_pl, data = df_s, cluster = cluster_fml)
    
    cmb <- combo_from_model(
      m,
      lin_patterns  = c("^placebo_std$"),
      quad_patterns = c("^I\\(placebo_std\\^2\\)$", "^I\\(placebo_std\\^2\\)", "placebo_std\\^2")
    )
    
    data.table(shift_days = s, beta_combo = cmb$beta_combo, p_combo = cmb$p_combo, n = nobs(m))
  }
  
  placebo_res <- rbindlist(lapply(shift_values, estimate_shift), fill=TRUE)
  
  # ONE p (same as effects): absolute tail share on |effect|
  p_abs <- mean(abs(placebo_res$beta_combo) >= abs(real_combo$beta_combo), na.rm = TRUE)
  
  # ranked p-value plot
  rank_df <- placebo_res[!is.na(p_combo)][order(p_combo)]
  rank_df[, rank := seq_len(.N)]
  
  p_rank_p <- ggplot(rank_df, aes(x = p_combo, y = rank)) +
    geom_point(size = 1.35, alpha = 0.85, color = DOT_COL) +
    geom_vline(xintercept = real_p, linewidth = 1.05) +
    labs(
      title = outcome_var,
      subtitle = sprintf("share placebo p-values greater or equal than real: %.3f", p_abs),
      x = "p-value for combo effect (β1 + β2)",
      y = "Placebo rank"
    ) +
    madestam_theme_wide()
  
  list(
    placebo_res = placebo_res,
    real_model  = real_m,
    real_p      = real_p,
    p_abs       = p_abs,
    plot_pvals  = p_rank_p
  )
}

# ============================================================
# Build 3 sets × 3 outcomes (same as your main regressions)
# ============================================================
set1 <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
set2 <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
set3 <- c("EDUCATION", "INCOME", "TRUST_PEOPLE")

run_set_effects <- function(outcomes){
  lapply(outcomes, function(y){
    run_madestam_rankplot_combo(
      outcome_var    = y,
      model_data     = model_data,
      placebo_path   = placebo_path,
      normalize_name = normalize_name,
      name_map       = name_map
    )
  })
}

run_set_pvals <- function(outcomes){
  lapply(outcomes, function(y){
    run_madestam_rankplot_pvals(
      outcome_var    = y,
      model_data     = model_data,
      placebo_path   = placebo_path,
      normalize_name = normalize_name,
      name_map       = name_map
    )
  })
}

# --- run
out_set1  <- run_set_effects(set1)
out_set2  <- run_set_effects(set2)
out_set3  <- run_set_effects(set3)

outp_set1 <- run_set_pvals(set1)
outp_set2 <- run_set_pvals(set2)
outp_set3 <- run_set_pvals(set3)

# ============================================================
# Save individual PNGs (optional)
# ============================================================
save_one <- function(out_list, names_vec, prefix, which_plot = c("plot_rank","plot_pvals")){
  which_plot <- match.arg(which_plot)
  for(i in seq_along(out_list)){
    ggsave(
      filename = paste0(prefix, "_", names_vec[i], ".png"),
      plot = out_list[[i]][[which_plot]],
      width = 7.2, height = 4.2, dpi = 300
    )
  }
}

save_one(out_set1,  set1, "madestam_effects_set1", "plot_rank")
save_one(out_set2,  set2, "madestam_effects_set2", "plot_rank")
save_one(out_set3,  set3, "madestam_effects_set3", "plot_rank")

save_one(outp_set1, set1, "madestam_pvals_set1",   "plot_pvals")
save_one(outp_set2, set2, "madestam_pvals_set2",   "plot_pvals")
save_one(outp_set3, set3, "madestam_pvals_set3",   "plot_pvals")

# ============================================================
# Combine into 3-panel figures (HORIZONTAL: 1 row × 3 columns)
#   - keep y-axis only in left panel
# ============================================================
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  
  # ---- effects rows
  fig_set1 <- out_set1[[1]]$plot_rank + strip_y(out_set1[[2]]$plot_rank) + strip_y(out_set1[[3]]$plot_rank) +
    plot_layout(ncol = 3)
  fig_set2 <- out_set2[[1]]$plot_rank + strip_y(out_set2[[2]]$plot_rank) + strip_y(out_set2[[3]]$plot_rank) +
    plot_layout(ncol = 3)
  fig_set3 <- out_set3[[1]]$plot_rank + strip_y(out_set3[[2]]$plot_rank) + strip_y(out_set3[[3]]$plot_rank) +
    plot_layout(ncol = 3)
  
  ggsave("madestam_3panel_effects_set1.png", fig_set1, width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_set2.png", fig_set2, width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_set3.png", fig_set3, width = 12.8, height = 4.2, dpi = 300)
  
  # ---- p-values rows
  figp_set1 <- outp_set1[[1]]$plot_pvals + strip_y(outp_set1[[2]]$plot_pvals) + strip_y(outp_set1[[3]]$plot_pvals) +
    plot_layout(ncol = 3)
  figp_set2 <- outp_set2[[1]]$plot_pvals + strip_y(outp_set2[[2]]$plot_pvals) + strip_y(outp_set2[[3]]$plot_pvals) +
    plot_layout(ncol = 3)
  figp_set3 <- outp_set3[[1]]$plot_pvals + strip_y(outp_set3[[2]]$plot_pvals) + strip_y(outp_set3[[3]]$plot_pvals) +
    plot_layout(ncol = 3)
  
  ggsave("madestam_3panel_pvals_set1.png", figp_set1, width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_set2.png", figp_set2, width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_set3.png", figp_set3, width = 12.8, height = 4.2, dpi = 300)
  
} else if (requireNamespace("gridExtra", quietly = TRUE)) {
  library(gridExtra)
  
  # effects
  fig_set1 <- gridExtra::grid.arrange(out_set1[[1]]$plot_rank,
                                      strip_y(out_set1[[2]]$plot_rank),
                                      strip_y(out_set1[[3]]$plot_rank),
                                      ncol = 3)
  fig_set2 <- gridExtra::grid.arrange(out_set2[[1]]$plot_rank,
                                      strip_y(out_set2[[2]]$plot_rank),
                                      strip_y(out_set2[[3]]$plot_rank),
                                      ncol = 3)
  fig_set3 <- gridExtra::grid.arrange(out_set3[[1]]$plot_rank,
                                      strip_y(out_set3[[2]]$plot_rank),
                                      strip_y(out_set3[[3]]$plot_rank),
                                      ncol = 3)
  
  png("madestam_3panel_effects_set1.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(fig_set1); dev.off()
  
  png("madestam_3panel_effects_set2.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(fig_set2); dev.off()
  
  png("madestam_3panel_effects_set3.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(fig_set3); dev.off()
  
  # p-values
  figp_set1 <- gridExtra::grid.arrange(outp_set1[[1]]$plot_pvals,
                                       strip_y(outp_set1[[2]]$plot_pvals),
                                       strip_y(outp_set1[[3]]$plot_pvals),
                                       ncol = 3)
  figp_set2 <- gridExtra::grid.arrange(outp_set2[[1]]$plot_pvals,
                                       strip_y(outp_set2[[2]]$plot_pvals),
                                       strip_y(outp_set2[[3]]$plot_pvals),
                                       ncol = 3)
  figp_set3 <- gridExtra::grid.arrange(outp_set3[[1]]$plot_pvals,
                                       strip_y(outp_set3[[2]]$plot_pvals),
                                       strip_y(outp_set3[[3]]$plot_pvals),
                                       ncol = 3)
  
  png("madestam_3panel_pvals_set1.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(figp_set1); dev.off()
  
  png("madestam_3panel_pvals_set2.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(figp_set2); dev.off()
  
  png("madestam_3panel_pvals_set3.png", width = 12.8, height = 4.2, units = "in", res = 300)
  grid::grid.draw(figp_set3); dev.off()
  
} else {
  message("Install either 'patchwork' (recommended) or 'gridExtra' to create 3-panel figures.")
}
