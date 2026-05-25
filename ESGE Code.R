# Survey data -------------------------------------------------------------
# Load libraries
library(haven)
library(tidyverse)
library(readxl)
library(writexl)
library(sf)
library(mapSpain)
library(fixest)
library(modelsummary)
library(stringi)
library(data.table)

# Clean environment
rm(list = ls())

setwd("C:/Users/Saúl/Documents")


# Load datasets -----------------------------------------------------------
ESGE_2013 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2013.csv") %>%
  mutate(
    survey_year = 2013,
    survey_wave = "2013",
    wave_2024 = NA_integer_
  )

ESGE_2015 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2015.csv") %>%
  mutate(
    survey_year = 2015,
    survey_wave = "2015",
    wave_2024 = NA_integer_
  )

ESGE_2017 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2017.sav") %>%
  mutate(
    survey_year = 2017,
    survey_wave = "2017",
    wave_2024 = NA_integer_
  )

ESGE_2023 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2023.sav") %>%
  mutate(
    survey_year = 2023,
    survey_wave = "2023",
    wave_2024 = NA_integer_
  )

ESGE_2024_w1 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2024.sav") %>%
  mutate(
    survey_year = 2024,
    survey_wave = "2024_w1",
    wave_2024 = 1
  )

ESGE_2024_w2 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2024_2.sav") %>%
  mutate(
    survey_year = 2025,
    survey_wave = "2024_w2",
    wave_2024 = 2
  )

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


valid_prov_codes <- sort(unique(as.integer(prov_code_map$prov_nac)))

clean_prov_code <- function(x) {
  x <- haven::zap_labels(x)
  suppressWarnings(as.integer(as.character(x)))
}

same_prov <- function(current_prov, birth_prov, valid_codes = valid_prov_codes) {
  current_prov <- clean_prov_code(current_prov)
  birth_prov   <- clean_prov_code(birth_prov)
  
  dplyr::case_when(
    is.na(current_prov) | is.na(birth_prov) ~ NA_integer_,
    !(current_prov %in% valid_codes) ~ NA_integer_,
    !(birth_prov %in% valid_codes) ~ NA_integer_,
    current_prov == birth_prov ~ 1L,
    TRUE ~ 0L
  )
}


# Harmonization helper patch

to_num <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

clean_birth_year <- function(x, min_year = 1900, max_year = Inf) {
  x <- to_num(x)
  dplyr::if_else(!is.na(x) & x >= min_year & x <= max_year, x, NA_real_)
}

clean_scale <- function(x, lo, hi) {
  x <- to_num(x)
  dplyr::if_else(!is.na(x) & x >= lo & x <= hi, x, NA_real_)
}

clean_income11 <- function(x) {
  x <- to_num(x)
  dplyr::if_else(!is.na(x) & x >= 1 & x <= 11, as.integer(x), NA_integer_)
}

clean_generic_num <- function(x, missing = c(0, 7, 8, 9, 77, 90, 94, 95, 96, 97, 98, 99, 998, 999, 9998, 9999)) {
  x <- to_num(x)
  dplyr::if_else(is.na(x) | x %in% missing, NA_real_, x)
}

born_spain_bin <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE  ~ NA_integer_
  )
}

sex_bin <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 2 ~ 1,
    x == 1 ~ 0,
    TRUE   ~ NA_real_
  )
}

catholic_bin <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 1 ~ 1L,
    x %in% c( 0, 8, 9, 97, 98, 99) ~ NA_integer_,
    TRUE ~ 0L
  )
}

catholic_bin_2023_2024 <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 1 ~ 1L,
    x %in% c(97, 98, 99) ~ NA_integer_,
    TRUE ~ 0
  )
}

relig_practice_old <- function(catholic, attend) {
  attend <- to_num(attend)
  dplyr::case_when(
    catholic == 0 ~ 0,
    is.na(catholic) ~ NA_real_,
    attend %in% 1:5 ~ as.numeric(attend),
    TRUE ~ NA_real_
  )
}

relig_practice_2023_2024 <- function(catholic, attend) {
  attend <- to_num(attend)
  dplyr::case_when(
    catholic == 0 ~ 0,
    is.na(catholic) ~ NA_real_,
    attend == 1 ~ 5,
    attend == 2 ~ 4,
    attend %in% c(3, 4) ~ 3,
    attend %in% c(5, 6) ~ 2,
    attend %in% c(7, 8) ~ 1,
    TRUE ~ NA_real_
  )
}

school_attended_old <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 3 ~ 1L,
    x %in% c(0, 1, 2) ~ 0L,
    TRUE ~ NA_integer_
  )
}

religious_school_bin <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x %in% c(1, 2) ~ 1L,
    x == 3 ~ 0L,
    x %in% c(0, 8, 9, 98, 99) ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

public_sector_bin <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x %in% c(1, 2) ~ 1L,
    x %in% c(0, 8, 9, 97, 98, 99) ~ NA_integer_,
    TRUE ~ 0L
  )
}

parent_school_2023_2024 <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x == 1 ~ 0L,
    x %in% 2:13 ~ 1L,
    x %in% c(0, 98, 99) ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

parent_educ_2023_2024 <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x %in% c(1, 2) ~ 1,
    x == 3 ~ 2,
    x == 4 ~ 3,
    x == 5 ~ 4,
    x %in% 6:10 ~ 5,
    x == 11 ~ 6,
    x == 12 ~ 7,
    x == 13 ~ 8,
    x %in% c(0, 98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

parent_employed_2023_2024 <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x %in% c(1, 2) ~ 1L,
    x == 3 ~ 0L,
    x %in% c(0, 7, 8, 9, 97, 98, 99) ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

trust_1_5_to_0_10 <- function(x) {
  x <- to_num(x)
  dplyr::case_when(
    x %in% 1:5 ~ (x-1) * 2.5,
    TRUE ~ NA_real_
  )
}

clean_parent_prov <- function(x) {
  x <- clean_prov_code(x)
  dplyr::if_else(x %in% valid_prov_codes, x, NA_integer_)
}

get_col <- function(data, vars, default = NA_real_) {
  hit <- vars[vars %in% names(data)]
  if (length(hit) == 0) {
    rep(default, nrow(data))
  } else {
    data[[hit[1]]]
  }
}

make_ecologist <- function(x1, x2 = NA_real_) {
  x1 <- to_num(x1)
  x2 <- to_num(x2)
  
  dplyr::case_when(
    x1 == 10 | x2 == 10 ~ 1,
    x1 %in% c(1:9, 11:27, 96, 97) | x2 %in% c(1:9, 11:27, 96, 97) ~ 0,
    x1 %in% c(98, 99) | x2 %in% c(98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

conservative_no_far_right <- function(conservative_vote, far_right_vote) {
  dplyr::case_when(
    is.na(conservative_vote) ~ NA_real_,
    !is.na(far_right_vote) & far_right_vote == 1 ~ 0,
    TRUE ~ as.numeric(conservative_vote)
  )
}


# Harmonize surveys


harmonize <- function(df, year, survey_year = year, wave_2024 = NA_integer_) {
  
  year_in         <- year
  survey_year_in  <- survey_year
  wave_2024_in    <- wave_2024
  
  if (year_in == 2024 && is.na(wave_2024_in)) {
    stop("For year == 2024 you must pass wave_2024 = 1 or 2.")
  }
  
  survey_wave_in <- if (year_in == 2024) {
    paste0("2024_w", wave_2024_in)
  } else {
    as.character(survey_year_in)
  }
  
  wave_2024_value <- if (year_in == 2024) wave_2024_in else NA_integer_
  
  df <- df %>%
    mutate(
      year = year_in,
      survey_year = survey_year_in,
      wave_2024 = wave_2024_value,
      survey_wave = survey_wave_in
    )
  
  
  # 2013
  
  
  if (year == 2013) {
    
    df <- df %>%
      mutate(
        BIRTH = clean_birth_year(P2802, max_year = survey_year_in - 18),
        AGE = survey_year - BIRTH,
        prov_nac = clean_parent_prov(P30A),
        SIZE_TOWN = to_num(P16A),
        BORN_SPAIN = born_spain_bin(P30),
        
        CATHOLIC = catholic_bin(P61),
        RELIGIOUS_PRACTICE = relig_practice_old(CATHOLIC, P61C),
        
        PRIMARY_SCHOOL_TYPE = case_when(
          to_num(P32H) %in% 1:3 ~ to_num(P32H),
          TRUE ~ NA_real_
        ),
        CATHOLIC_SCHOOL = religious_school_bin(P32I),
        
        CONSERVATIVE_VOTE = case_when(
          to_num(P62A) %in% c(0, 77, 97, 98, 99) ~ NA_real_,
          to_num(P62A) %in% c(2, 5, 7, 12) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          to_num(P62A) %in% c(0, 77, 97, 98, 99) ~ NA_real_,
          to_num(P62A) == 2 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        CONSERVATIVE_NO_FAR_RIGHT = conservative_no_far_right(CONSERVATIVE_VOTE, FAR_RIGHT_VOTE),
        PARTICIPATION = case_when(
          to_num(P62) %in% c(6, 8, 2, 9) ~ NA_real_,
          to_num(P62) == 1 ~ 1,
          TRUE ~ 0
        ),
        
        FEMALE = sex_bin(P27),
        INCOME = clean_income11(P66),
        HH_INCOME = clean_income11(P65),
        
        SCHOOL = school_attended_old(P32),
        EDUCATION = case_when(
          to_num(P32) %in% c(1, 2) ~ 1,
          to_num(P32A01) == 1 ~ 1,
          to_num(P32A01) == 2 ~ 2,
          to_num(P32A01) %in% c(3, 4) ~ 3,
          to_num(P32A01) %in% c(5, 6) ~ 4,
          to_num(P32A01) %in% c(7, 8, 9) ~ 5,
          to_num(P32A01) %in% c(10, 11, 12) ~ 6,
          to_num(P32A01) == 13 ~ 7,
          to_num(P32A01) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_BORN_SPAIN = born_spain_bin(P15C),
        MOTHER_BORN_SPAIN = born_spain_bin(P14C),
        FATHER_PROV_NAC = if_else(FATHER_BORN_SPAIN == 1L, clean_parent_prov(P15D), NA_integer_),
        MOTHER_PROV_NAC = if_else(MOTHER_BORN_SPAIN == 1L, clean_parent_prov(P14D), NA_integer_),
        
        FATHER_SCHOOL = case_when(
          to_num(P15M) == 3 ~ 1L,
          to_num(P15M) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        FATHER_EDUCATION = case_when(
          to_num(P15M) %in% c(1, 2) ~ 1,
          to_num(P15N02) == 1 ~ 1,
          to_num(P15N02) == 2 ~ 2,
          to_num(P15N02) %in% c(3, 4) ~ 3,
          to_num(P15N02) %in% c(5, 6) ~ 4,
          to_num(P15N02) %in% c(7, 8, 9) ~ 5,
          to_num(P15N02) %in% c(10, 11, 12) ~ 6,
          to_num(P15N02) == 13 ~ 7,
          to_num(P15N02) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(
          to_num(P14M) == 3 ~ 1L,
          to_num(P14M) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        MOTHER_EDUCATION = case_when(
          to_num(P14M) %in% c(1, 2) ~ 1,
          to_num(P14N02) == 1 ~ 1,
          to_num(P14N02) == 2 ~ 2,
          to_num(P14N02) %in% c(3, 4) ~ 3,
          to_num(P14N02) %in% c(5, 6) ~ 4,
          to_num(P14N02) %in% c(7, 8, 9) ~ 5,
          to_num(P14N02) %in% c(10, 11, 12) ~ 6,
          to_num(P14N02) == 13 ~ 7,
          to_num(P14N02) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_EMPLOYMENT = case_when(
          to_num(P15O) %in% c(98, 99) ~ NA_integer_,
          to_num(P15O) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        FATHER_EMPLOYMENT_TYPE = clean_generic_num(P15Q),
        MOTHER_EMPLOYMENT = case_when(
          to_num(P14O) %in% c(98, 99) ~ NA_integer_,
          to_num(P14O) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        MOTHER_EMPLOYMENT_TYPE = clean_generic_num(P14Q),
        
        FATHER_CATHOLIC = catholic_bin(P15S),
        MOTHER_CATHOLIC = catholic_bin(P14S),
        FATHER_RELIGIOUS_PRACTICE = case_when(to_num(P15T) %in% 1:5 ~ to_num(P15T), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(to_num(P14T) %in% 1:5 ~ to_num(P14T), TRUE ~ NA_real_),
        
        SAME_LOC_BIRTH = same_prov(PROV, P30A),
        MOTHER_IDEOLOGY_LR = clean_scale(P14V01, 1, 10),
        FATHER_IDEOLOGY_LR = clean_scale(P15V01, 1, 10),
        COUPLE_IDEOLOGY_LR = clean_scale(P7801, 1, 10),
        IDEOLOGY_LR = clean_scale(P60, 1, 10),
        SUBJECTIVE_CLASS = clean_scale(P18, 0, 10),
        TRUST_PEOPLE = clean_scale(P3, 0, 10),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = clean_scale(P19, 0, 10),
        PUBLIC_SECTOR_EMP = public_sector_bin(P39C),
        LIFE_SATISFACTION = clean_scale(P1, 0, 10),
        CIVIL_RELATION = clean_scale(P29, 0, 10),
        
        COUPLE_BORN_SPAIN = born_spain_bin(P73),
        HAS_A_COUPLE = case_when(
          to_num(P67) %in% c(1, 2) ~ 1L,
          to_num(P67) %in% c(8, 9, 98, 99) ~ NA_integer_,
          TRUE ~ 0L
        ),
        LEFT_RIGHT = clean_scale(P59, 0, 10),
        FATHER_LEFT_RIGHT = clean_scale(P15U, 0, 10),
        MOTHER_LEFT_RIGHT = clean_scale(P14U, 0, 10),
        COUPLE_CATHOLIC = catholic_bin(P79),
        COUPLE_LEFT_RIGHT = clean_scale(P77, 0, 10),
        
        ECOLOGIST_SELF = make_ecologist(P60),
        ENV_CONCERN = NA_real_,
        SAME_TOWN_AT16 = case_when(
          to_num(P16) == 1 ~ 1,
          to_num(P16) == 2 ~ 0,
          TRUE ~ NA_real_
        )
      )
    
    return(df)
  }
  
  
  # 2015
  
  
  if (year == 2015) {
    
    df <- df %>%
      mutate(
        BIRTH = clean_birth_year(P5402, max_year = survey_year_in - 18),
        AGE = survey_year - BIRTH,
        prov_nac = clean_parent_prov(P56A),
        SIZE_TOWN = to_num(P44A),
        BORN_SPAIN = born_spain_bin(P56),
        
        CATHOLIC = catholic_bin(P79),
        RELIGIOUS_PRACTICE = relig_practice_old(CATHOLIC, P79B),
        
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        
        CONSERVATIVE_VOTE = case_when(
          to_num(P80AR) %in% c(0, 77, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(P80AR) %in% c(1, 9, 11) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          to_num(P80AR) %in% c(0, 77, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(P80AR) == 1 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        CONSERVATIVE_NO_FAR_RIGHT = conservative_no_far_right(CONSERVATIVE_VOTE, FAR_RIGHT_VOTE),
        PARTICIPATION = case_when(
          to_num(P80) %in% c(9, 8, 6, 2) ~ NA_real_,
          to_num(P80) == 1 ~ 1,
          TRUE ~ 0
        ),
        
        FEMALE = sex_bin(P53),
        INCOME = clean_income11(P84),
        HH_INCOME = clean_income11(P83),
        
        SCHOOL = school_attended_old(P58),
        EDUCATION = case_when(
          to_num(P58) %in% c(1, 2) ~ 1,
          to_num(P58A) == 1 ~ 1,
          to_num(P58A) == 2 ~ 2,
          to_num(P58A) %in% c(3, 4) ~ 3,
          to_num(P58A) %in% c(5, 6) ~ 4,
          to_num(P58A) %in% c(7, 8, 9) ~ 5,
          to_num(P58A) %in% c(10, 11, 12) ~ 6,
          to_num(P58A) == 13 ~ 7,
          to_num(P58A) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_BORN_SPAIN = born_spain_bin(P43C),
        MOTHER_BORN_SPAIN = born_spain_bin(P42C),
        FATHER_PROV_NAC = if_else(FATHER_BORN_SPAIN == 1L, clean_parent_prov(P43D), NA_integer_),
        MOTHER_PROV_NAC = if_else(MOTHER_BORN_SPAIN == 1L, clean_parent_prov(P42D), NA_integer_),
        
        FATHER_SCHOOL = case_when(
          to_num(P43J) == 3 ~ 1L,
          to_num(P43J) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        FATHER_EDUCATION = case_when(
          to_num(P43J) %in% c(1, 2) ~ 1,
          to_num(P43K) == 1 ~ 1,
          to_num(P43K) == 2 ~ 2,
          to_num(P43K) %in% c(3, 4) ~ 3,
          to_num(P43K) %in% c(5, 6) ~ 4,
          to_num(P43K) %in% c(7, 8, 9) ~ 5,
          to_num(P43K) %in% c(10, 11, 12) ~ 6,
          to_num(P43K) == 13 ~ 7,
          to_num(P43K) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(
          to_num(P42J) == 3 ~ 1L,
          to_num(P42J) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        MOTHER_EDUCATION = case_when(
          to_num(P42J) %in% c(1, 2) ~ 1,
          to_num(P42K) == 1 ~ 1,
          to_num(P42K) == 2 ~ 2,
          to_num(P42K) %in% c(3, 4) ~ 3,
          to_num(P42K) %in% c(5, 6) ~ 4,
          to_num(P42K) %in% c(7, 8, 9) ~ 5,
          to_num(P42K) %in% c(10, 11, 12) ~ 6,
          to_num(P42K) == 13 ~ 7,
          to_num(P42K) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_EMPLOYMENT = case_when(
          to_num(P43L) %in% c(98, 99) ~ NA_integer_,
          to_num(P43L) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        FATHER_EMPLOYMENT_TYPE = clean_generic_num(P43N),
        MOTHER_EMPLOYMENT = case_when(
          to_num(P42L) %in% c(98, 99) ~ NA_integer_,
          to_num(P42L) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        MOTHER_EMPLOYMENT_TYPE = clean_generic_num(P42N),
        
        FATHER_CATHOLIC = catholic_bin(P43P),
        MOTHER_CATHOLIC = catholic_bin(P42P),
        FATHER_RELIGIOUS_PRACTICE = case_when(to_num(P43R) %in% 1:5 ~ to_num(P43R), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(to_num(P42R) %in% 1:5 ~ to_num(P42R), TRUE ~ NA_real_),
        
        SAME_LOC_BIRTH = same_prov(PROV, P56A),
        MOTHER_IDEOLOGY_LR = clean_scale(P42T, 1, 10),
        FATHER_IDEOLOGY_LR = clean_scale(P43T, 1, 10),
        COUPLE_IDEOLOGY_LR = clean_scale(P95, 1, 10),
        IDEOLOGY_LR = clean_scale(P78, 1, 10),
        SUBJECTIVE_CLASS = clean_scale(P45, 0, 10),
        TRUST_PEOPLE = clean_scale(P3, 0, 10),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = clean_scale(P46, 0, 10),
        PUBLIC_SECTOR_EMP = public_sector_bin(P63C),
        LIFE_SATISFACTION = clean_scale(P1, 0, 10),
        CIVIL_RELATION = clean_scale(P55, 0, 10),
        
        COUPLE_BORN_SPAIN = born_spain_bin(P73),
        HAS_A_COUPLE = case_when(
          to_num(P86) %in% c(1, 2) ~ 1L,
          to_num(P86) %in% c(8, 9, 98, 99) ~ NA_integer_,
          TRUE ~ 0L
        ),
        LEFT_RIGHT = clean_scale(P77, 0, 10),
        FATHER_LEFT_RIGHT = clean_scale(P43S, 0, 10),
        MOTHER_LEFT_RIGHT = clean_scale(P42S, 0, 10),
        COUPLE_CATHOLIC = catholic_bin(P96),
        COUPLE_LEFT_RIGHT = clean_scale(P94, 0, 10),
        
        ECOLOGIST_SELF = make_ecologist(P78),
        ENV_CONCERN = NA_real_,
        SAME_TOWN_AT16 = case_when(
          to_num(P44) == 1 ~ 1,
          to_num(P44) == 2 ~ 0,
          TRUE ~ NA_real_
        )
      )
    
    return(df)
  }
  
  
  # 2017
  
  
  if (year == 2017) {
    
    df <- df %>%
      mutate(
        BIRTH = clean_birth_year(FNACIMANYO2, max_year = survey_year_in - 18),
        AGE = survey_year - BIRTH,
        prov_nac = clean_parent_prov(P38A),
        SIZE_TOWN = to_num(P28A),
        BORN_SPAIN = born_spain_bin(P38),
        
        CATHOLIC = catholic_bin(P63),
        RELIGIOUS_PRACTICE = relig_practice_old(CATHOLIC, P63B),
        
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        
        # Ciudadanos/C's is code 4 and is intentionally excluded.
        CONSERVATIVE_VOTE = case_when(
          to_num(RECUERDO) %in% c(0, 77, 93, 94, 97, 98, 99) ~ NA_real_,
          to_num(RECUERDO) %in% c(1, 8, 10) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          to_num(RECUERDO) %in% c(0, 77, 93, 94, 97, 98, 99) ~ NA_real_,
          to_num(RECUERDO) == 1 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        CONSERVATIVE_NO_FAR_RIGHT = conservative_no_far_right(CONSERVATIVE_VOTE, FAR_RIGHT_VOTE),
        PARTICIPATION = case_when(
          to_num(P64) %in% c(9, 2, 6, 7) ~ NA_real_,
          to_num(P64) == 1 ~ 1,
          TRUE ~ 0
        ),
        
        FEMALE = sex_bin(P0),
        INCOME = clean_income11(P68),
        HH_INCOME = clean_income11(P67),
        
        SCHOOL = school_attended_old(P40),
        EDUCATION = case_when(
          to_num(P40) %in% c(1, 2) ~ 1,
          to_num(P401) == 1 ~ 1,
          to_num(P401) == 2 ~ 2,
          to_num(P401) %in% c(3, 4) ~ 3,
          to_num(P401) %in% c(5, 6) ~ 4,
          to_num(P401) %in% c(7, 8, 9) ~ 5,
          to_num(P401) %in% c(10, 11, 12) ~ 6,
          to_num(P401) == 13 ~ 7,
          to_num(P401) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_BORN_SPAIN = born_spain_bin(P27C),
        MOTHER_BORN_SPAIN = born_spain_bin(P26C),
        FATHER_PROV_NAC = if_else(FATHER_BORN_SPAIN == 1L, clean_parent_prov(P27D), NA_integer_),
        MOTHER_PROV_NAC = if_else(MOTHER_BORN_SPAIN == 1L, clean_parent_prov(P26D), NA_integer_),
        
        FATHER_SCHOOL = case_when(
          to_num(P27J) == 3 ~ 1L,
          to_num(P27J) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        FATHER_EDUCATION = case_when(
          to_num(P27J) %in% c(1, 2) ~ 1,
          to_num(P27K) == 1 ~ 1,
          to_num(P27K) == 2 ~ 2,
          to_num(P27K) %in% c(3, 4) ~ 3,
          to_num(P27K) %in% c(5, 6) ~ 4,
          to_num(P27K) %in% c(7, 8, 9) ~ 5,
          to_num(P27K) %in% c(10, 11, 12) ~ 6,
          to_num(P27K) == 13 ~ 7,
          to_num(P27K) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        MOTHER_SCHOOL = case_when(
          to_num(P26J) == 3 ~ 1L,
          to_num(P26J) %in% c(0, 1, 2) ~ 0L,
          TRUE ~ NA_integer_
        ),
        MOTHER_EDUCATION = case_when(
          to_num(P26J) %in% c(1, 2) ~ 1,
          to_num(P26K) == 1 ~ 1,
          to_num(P26K) == 2 ~ 2,
          to_num(P26K) %in% c(3, 4) ~ 3,
          to_num(P26K) %in% c(5, 6) ~ 4,
          to_num(P26K) %in% c(7, 8, 9) ~ 5,
          to_num(P26K) %in% c(10, 11, 12) ~ 6,
          to_num(P26K) == 13 ~ 7,
          to_num(P26K) %in% c(14, 15) ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_EMPLOYMENT = case_when(
          to_num(P27L) %in% c(98, 99) ~ NA_integer_,
          to_num(P27L) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        FATHER_EMPLOYMENT_TYPE = clean_generic_num(P27N),
        MOTHER_EMPLOYMENT = case_when(
          to_num(P26L) %in% c(98, 99) ~ NA_integer_,
          to_num(P26L) == 1 ~ 1L,
          TRUE ~ 0L
        ),
        MOTHER_EMPLOYMENT_TYPE = clean_generic_num(P26N),
        
        FATHER_CATHOLIC = catholic_bin(P27P),
        MOTHER_CATHOLIC = catholic_bin(P26P),
        FATHER_RELIGIOUS_PRACTICE = case_when(to_num(P27R) %in% 1:5 ~ to_num(P27R), TRUE ~ NA_real_),
        MOTHER_RELIGIOUS_PRACTICE = case_when(to_num(P26R) %in% 1:5 ~ to_num(P26R), TRUE ~ NA_real_),
        
        SAME_LOC_BIRTH = same_prov(PROV, P38A),
        MOTHER_IDEOLOGY_LR = clean_scale(P26T, 1, 10),
        FATHER_IDEOLOGY_LR = clean_scale(P27T, 1, 10),
        COUPLE_IDEOLOGY_LR = clean_scale(P80_1, 1, 10),
        IDEOLOGY_LR = clean_scale(P62_1, 1, 10),
        SUBJECTIVE_CLASS = clean_scale(P29, 0, 10),
        TRUST_PEOPLE = clean_scale(P3, 0, 10),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = clean_scale(P30, 0, 10),
        PUBLIC_SECTOR_EMP = public_sector_bin(P82D),
        LIFE_SATISFACTION = clean_scale(P1, 0, 10),
        CIVIL_RELATION = clean_scale(P37, 0, 10),
        
        COUPLE_BORN_SPAIN = born_spain_bin(P76),
        HAS_A_COUPLE = case_when(
          to_num(P70) %in% c(1, 2) ~ 1L,
          to_num(P70) %in% c(8, 9, 98, 99) ~ NA_integer_,
          TRUE ~ 0L
        ),
        LEFT_RIGHT = clean_scale(P61, 0, 10),
        FATHER_LEFT_RIGHT = clean_scale(P27S, 0, 10),
        MOTHER_LEFT_RIGHT = clean_scale(P26S, 0, 10),
        COUPLE_CATHOLIC = catholic_bin(P81),
        COUPLE_LEFT_RIGHT = clean_scale(P79, 0, 10),
        
        ECOLOGIST_SELF = make_ecologist(P62_1),
        ENV_CONCERN = NA_real_,
        SAME_TOWN_AT16 = case_when(
          to_num(P28) == 1 ~ 1,
          to_num(P28) == 2 ~ 0,
          TRUE ~ NA_real_
        )
      )
    
    return(df)
  }
  
  
  # 2023
  
  
  if (year == 2023) {
    
    party_2023 <- get_col(df, c("RECUERDO", "NAT_PRTY"))
    ideol1_2023 <- get_col(df, c("IDEOL_CATEG_01", "IDEOL_CATEG_1"))
    ideol2_2023 <- get_col(df, c("IDEOL_CATEG_02", "IDEOL_CATEG_2"))
    
    df <- df %>%
      mutate(
        BIRTH = clean_birth_year(BIRTH, max_year = survey_year_in - 18),
        AGE = survey_year - BIRTH,
        prov_nac = clean_parent_prov(PROV_NAC),
        SIZE_TOWN = NA_real_,
        BORN_SPAIN = born_spain_bin(LUGAR_NAC),
        
        CATHOLIC = catholic_bin_2023_2024(NAT_RELIG),
        RELIGIOUS_PRACTICE = relig_practice_2023_2024(CATHOLIC, ATTEND),
        
        PRIMARY_SCHOOL_TYPE = case_when(
          to_num(TIPO_COLEGIO) %in% 1:3 ~ to_num(TIPO_COLEGIO),
          TRUE ~ NA_real_
        ),
        CATHOLIC_SCHOOL = NA_real_,
        
        CONSERVATIVE_VOTE = case_when(
          to_num(party_2023) %in% c(0, 16, 17, 77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2023) %in% c(2, 6, 9, 10) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          to_num(party_2023) %in% c(0, 16, 17, 77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2023) == 2 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = case_when(
          to_num(party_2023) %in% c(0, 16, 17, 77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2023) == 6 ~ 1,
          TRUE ~ 0
        ),
        CONSERVATIVE_NO_FAR_RIGHT = conservative_no_far_right(CONSERVATIVE_VOTE, FAR_RIGHT_VOTE),
        PARTICIPATION = case_when(
          to_num(VOTE_LE) %in% c(4, 5, 9) ~ NA_real_,
          to_num(VOTE_LE) == 1 ~ 1,
          TRUE ~ 0
        ),
        
        FEMALE = sex_bin(SEX),
        INCOME = NA_real_,
        HH_INCOME = NA_real_,
        SCHOOL = NA_real_,
        EDUCATION = case_when(
          to_num(NAT_DEGR) %in% c(1, 2) ~ 1,
          to_num(NAT_DEGR) == 3 ~ 2,
          to_num(NAT_DEGR) %in% 4:6 ~ 3,
          to_num(NAT_DEGR) %in% 7:10 ~ 4,
          to_num(NAT_DEGR) %in% 11:15 ~ 5,
          to_num(NAT_DEGR) == 16 ~ 6,
          to_num(NAT_DEGR) == 17 ~ 7,
          to_num(NAT_DEGR) == 18 ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_BORN_SPAIN = born_spain_bin(F_BORN),
        MOTHER_BORN_SPAIN = born_spain_bin(M_BORN),
        FATHER_PROV_NAC = NA_integer_,
        MOTHER_PROV_NAC = NA_integer_,
        
        FATHER_SCHOOL = parent_school_2023_2024(FATH_NAT_DEGR),
        FATHER_EDUCATION = parent_educ_2023_2024(FATH_NAT_DEGR),
        MOTHER_SCHOOL = parent_school_2023_2024(MOTH_NAT_DEGR),
        MOTHER_EDUCATION = parent_educ_2023_2024(MOTH_NAT_DEGR),
        
        FATHER_EMPLOYMENT = parent_employed_2023_2024(FATH_WORK),
        FATHER_EMPLOYMENT_TYPE = clean_generic_num(FATH_WORK),
        MOTHER_EMPLOYMENT = parent_employed_2023_2024(MOTH_WORK),
        MOTHER_EMPLOYMENT_TYPE = clean_generic_num(MOTH_WORK),
        
        FATHER_CATHOLIC = NA_real_,
        MOTHER_CATHOLIC = NA_real_,
        FATHER_RELIGIOUS_PRACTICE = NA_real_,
        MOTHER_RELIGIOUS_PRACTICE = NA_real_,
        
        SAME_LOC_BIRTH = same_prov(PROV, PROV_NAC),
        MOTHER_IDEOLOGY_LR = NA_real_,
        FATHER_IDEOLOGY_LR = NA_real_,
        COUPLE_IDEOLOGY_LR = NA_real_,
        IDEOLOGY_LR = clean_scale(IDEOL_CATEG_01, 1, 10),
        SUBJECTIVE_CLASS = NA_real_,
        TRUST_PEOPLE = trust_1_5_to_0_10(V10),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = clean_scale(MERIT, 0, 10),
        PUBLIC_SECTOR_EMP = public_sector_bin(TYPORG1),
        LIFE_SATISFACTION = clean_scale(C_SATISFVIDA, 0, 10),
        CIVIL_RELATION = clean_scale(MARITAL, 0, 10),
        
        COUPLE_BORN_SPAIN = NA_real_,
        HAS_A_COUPLE = case_when(
          to_num(PARTLIV) %in% c(1, 2) ~ 1L,
          to_num(PARTLIV) %in% c(0, 8, 9, 98, 99) ~ NA_integer_,
          TRUE ~ 0L
        ),
        LEFT_RIGHT = clean_scale(LEFT_RIGHT, 0, 10),
        FATHER_LEFT_RIGHT = NA_real_,
        MOTHER_LEFT_RIGHT = NA_real_,
        COUPLE_CATHOLIC = NA_real_,
        COUPLE_LEFT_RIGHT = NA_real_,
        
        ECOLOGIST_SELF = make_ecologist(ideol1_2023, ideol2_2023),
        ENV_CONCERN = case_when(
          to_num(V15) %in% 1:5 ~ to_num(V15),
          TRUE ~ NA_real_
        ),
        SAME_TOWN_AT16 = NA_real_
      )
    
    return(df)
  }
  
  
  # 2024, both waves
  
  
  if (year == 2024) {
    
    if (is.na(wave_2024)) {
      stop("For year == 2024 you must pass wave_2024 = 1 or 2.")
    }
    
    party_2024 <- get_col(df, c("RECUERDO", "NAT_PRTY"))
    ideol1_2024 <- get_col(df, c("IDEOL_CATEG_1", "IDEOL_CATEG_01"))
    ideol2_2024 <- get_col(df, c("IDEOL_CATEG_2", "IDEOL_CATEG_02"))
    org_2024 <- if (wave_2024 == 1) {
      get_col(df, c("TYPORG1", "TYPORG"))
    } else {
      get_col(df, c("TYPORG", "TYPORG1"))
    }
    satis_2024 <- get_col(df, c("C_SATISFVIDA_24", "C_SATISFVIDA"))
    
    df <- df %>%
      mutate(
        BIRTH = clean_birth_year(BIRTH, max_year = survey_year_in - 18),
        AGE = survey_year - BIRTH,
        prov_nac = clean_parent_prov(PROV_NAC),
        SIZE_TOWN = NA_real_,
        BORN_SPAIN = born_spain_bin(LUGAR_NAC),
        
        CATHOLIC = catholic_bin_2023_2024(NAT_RELIG),
        RELIGIOUS_PRACTICE = relig_practice_2023_2024(CATHOLIC, ATTEND),
        
        PRIMARY_SCHOOL_TYPE = case_when(
          to_num(TIPO_COLEGIO) %in% 1:3 ~ to_num(TIPO_COLEGIO),
          TRUE ~ NA_real_
        ),
        CATHOLIC_SCHOOL = religious_school_bin(TIPO_COLEGIO_2),
        
        CONSERVATIVE_VOTE = case_when(
          to_num(party_2024) %in% c(0, 77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2024) %in% c(1, 3, 7, 9, 12) ~ 1,
          TRUE ~ 0
        ),
        PP_VOTE = case_when(
          to_num(party_2024) %in% c(0, 77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2024) == 1 ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = case_when(
          to_num(party_2024) %in% c(0,  77, 90, 94, 95, 97, 98, 99) ~ NA_real_,
          to_num(party_2024) == 3 ~ 1,
          TRUE ~ 0
        ),
        CONSERVATIVE_NO_FAR_RIGHT = conservative_no_far_right(CONSERVATIVE_VOTE, FAR_RIGHT_VOTE),
        PARTICIPATION = case_when(
          to_num(VOTE_LE) %in% c(4, 5, 9) ~ NA_real_,
          to_num(VOTE_LE) == 1 ~ 1,
          TRUE ~ 0
        ),
        
        FEMALE = sex_bin(SEXO),
        INCOME = NA_real_,
        HH_INCOME = NA_real_,
        SCHOOL = NA_real_,
        EDUCATION = case_when(
          to_num(NAT_DEGR) %in% c(1, 2) ~ 1,
          to_num(NAT_DEGR) == 3 ~ 2,
          to_num(NAT_DEGR) %in% 4:6 ~ 3,
          to_num(NAT_DEGR) %in% 7:10 ~ 4,
          to_num(NAT_DEGR) %in% 11:15 ~ 5,
          to_num(NAT_DEGR) == 16 ~ 6,
          to_num(NAT_DEGR) == 17 ~ 7,
          to_num(NAT_DEGR) == 18 ~ 8,
          TRUE ~ NA_real_
        ),
        
        FATHER_BORN_SPAIN = born_spain_bin(F_BORN),
        MOTHER_BORN_SPAIN = born_spain_bin(M_BORN),
        FATHER_PROV_NAC = NA_integer_,
        MOTHER_PROV_NAC = NA_integer_,
        
        FATHER_SCHOOL = parent_school_2023_2024(FATH_NAT_DEGR),
        FATHER_EDUCATION = parent_educ_2023_2024(FATH_NAT_DEGR),
        MOTHER_SCHOOL = parent_school_2023_2024(MOTH_NAT_DEGR),
        MOTHER_EDUCATION = parent_educ_2023_2024(MOTH_NAT_DEGR),
        
        FATHER_EMPLOYMENT = parent_employed_2023_2024(FATH_WORK),
        FATHER_EMPLOYMENT_TYPE = clean_generic_num(FATH_WORK),
        MOTHER_EMPLOYMENT = parent_employed_2023_2024(MOTH_WORK),
        MOTHER_EMPLOYMENT_TYPE = clean_generic_num(MOTH_WORK),
        
        FATHER_CATHOLIC = NA_real_,
        MOTHER_CATHOLIC = NA_real_,
        FATHER_RELIGIOUS_PRACTICE = NA_real_,
        MOTHER_RELIGIOUS_PRACTICE = NA_real_,
        
        SAME_LOC_BIRTH = same_prov(PROV, PROV_NAC),
        MOTHER_IDEOLOGY_LR = NA_real_,
        FATHER_IDEOLOGY_LR = NA_real_,
        COUPLE_IDEOLOGY_LR = NA_real_,
        IDEOLOGY_LR = NA_real_,
        SUBJECTIVE_CLASS = NA_real_,
        TRUST_PEOPLE = NA_real_,
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = clean_scale(MERIT, 0, 10),
        PUBLIC_SECTOR_EMP = public_sector_bin(org_2024),
        LIFE_SATISFACTION = clean_scale(satis_2024, 0, 10),
        CIVIL_RELATION = clean_scale(MARITAL, 0, 10),
        
        COUPLE_BORN_SPAIN = NA_real_,
        HAS_A_COUPLE = case_when(
          to_num(PARTLIV) %in% c(1, 2) ~ 1L,
          to_num(PARTLIV) %in% c(0, 8, 9, 98, 99) ~ NA_integer_,
          TRUE ~ 0L
        ),
        LEFT_RIGHT = clean_scale(LEFT_RIGHT, 0, 10),
        FATHER_LEFT_RIGHT = NA_real_,
        MOTHER_LEFT_RIGHT = NA_real_,
        COUPLE_CATHOLIC = NA_real_,
        COUPLE_LEFT_RIGHT = NA_real_,
        
        ECOLOGIST_SELF = make_ecologist(ideol1_2024, ideol2_2024),
        ENV_CONCERN = NA_real_,
        SAME_TOWN_AT16 = NA_real_
      )
    
    return(df)
  }
  
  stop("Year not supported in harmonize().")
}

# Harmonize surveys 
ESGE_2013_h <- harmonize(ESGE_2013, 2013)
ESGE_2015_h <- harmonize(ESGE_2015, 2015)
ESGE_2017_h <- harmonize(ESGE_2017, 2017)
ESGE_2023_h <- harmonize(ESGE_2023, 2023)
ESGE_2024_w1_h <- harmonize(ESGE_2024_w1, 2024, wave_2024 = 1)
ESGE_2024_w2_h <- harmonize(
  ESGE_2024_w2,
  year = 2024,
  survey_year = 2025,
  wave_2024 = 2
)

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
  "survey_year","survey_wave","wave_2024","year","BIRTH","AGE","prov_nac","SIZE_TOWN","BORN_SPAIN","CATHOLIC",
  "RELIGIOUS_PRACTICE","PRIMARY_SCHOOL_TYPE","CATHOLIC_SCHOOL","CONSERVATIVE_VOTE",
  "FAR_RIGHT_VOTE","PARTICIPATION","FEMALE","INCOME","SCHOOL","EDUCATION",
  "FATHER_BORN_SPAIN","MOTHER_BORN_SPAIN","FATHER_SCHOOL","FATHER_EDUCATION",
  "MOTHER_SCHOOL","MOTHER_EDUCATION","FATHER_EMPLOYMENT","FATHER_EMPLOYMENT_TYPE",
  "MOTHER_EMPLOYMENT","MOTHER_EMPLOYMENT_TYPE","FATHER_CATHOLIC","MOTHER_CATHOLIC",
  "FATHER_RELIGIOUS_PRACTICE","MOTHER_RELIGIOUS_PRACTICE","SAME_LOC_BIRTH",
  "MOTHER_IDEOLOGY_LR","FATHER_IDEOLOGY_LR","COUPLE_IDEOLOGY_LR","IDEOLOGY_LR",
  "SUBJECTIVE_CLASS","TRUST_PEOPLE","INST_CONFIDENCE","MERITOCRACY_BELIEF", "CONSERVATIVE_NO_FAR_RIGHT",
  "PUBLIC_SECTOR_EMP","LIFE_SATISFACTION","CIVIL_RELATION","COUPLE_BORN_SPAIN",
  "HAS_A_COUPLE","LEFT_RIGHT","FATHER_LEFT_RIGHT","MOTHER_LEFT_RIGHT",
  "COUPLE_CATHOLIC","COUPLE_LEFT_RIGHT","PROV","MUN","PP_VOTE",
  "ECOLOGIST_SELF","ENV_CONCERN","SAME_TOWN_AT16"
)

survey <- dplyr::bind_rows(
  dplyr::select(ESGE_2013_h, dplyr::all_of(harmonized_vars)),
  dplyr::select(ESGE_2015_h, dplyr::all_of(harmonized_vars)),
  dplyr::select(ESGE_2017_h, dplyr::all_of(harmonized_vars)),
  dplyr::select(ESGE_2023_h, dplyr::all_of(harmonized_vars)),
  dplyr::select(ESGE_2024_h, dplyr::all_of(harmonized_vars))
) %>%
  dplyr::mutate(respondent_id = dplyr::row_number())

# Province -> Comunidad Autónoma crosswalk -------------------------------

prov_to_ccaa <- function(prov) {
  prov <- clean_prov_code(prov)
  
  dplyr::case_when(
    prov %in% c(4, 11, 14, 18, 21, 23, 29, 41) ~ 1L,   # Andalucía
    prov %in% c(22, 44, 50)                     ~ 2L,   # Aragón
    prov == 33                                  ~ 3L,   # Asturias
    prov == 7                                   ~ 4L,   # Illes Balears
    prov %in% c(35, 38)                         ~ 5L,   # Canarias
    prov == 39                                  ~ 6L,   # Cantabria
    prov %in% c(5, 9, 24, 34, 37, 40, 42, 47, 49) ~ 7L, # Castilla y León
    prov %in% c(2, 13, 16, 19, 45)              ~ 8L,   # Castilla-La Mancha
    prov %in% c(8, 17, 25, 43)                  ~ 9L,   # Cataluña
    prov %in% c(3, 12, 46)                      ~ 10L,  # Comunitat Valenciana
    prov %in% c(6, 10)                          ~ 11L,  # Extremadura
    prov %in% c(15, 27, 32, 36)                 ~ 12L,  # Galicia
    prov == 28                                  ~ 13L,  # Madrid
    prov == 30                                  ~ 14L,  # Murcia
    prov == 31                                  ~ 15L,  # Navarra
    prov %in% c(1, 20, 48)                      ~ 16L,  # País Vasco
    prov == 26                                  ~ 17L,  # La Rioja
    prov == 51                                  ~ 18L,  # Ceuta
    prov == 52                                  ~ 19L,  # Melilla
    TRUE                                        ~ NA_integer_
  )
}

survey <- survey %>%
  mutate(
    birth_ccaa = prov_to_ccaa(prov_nac),  # Comunidad Autónoma of birth
    res_ccaa   = prov_to_ccaa(PROV)       # Comunidad Autónoma of residence/interview
  )

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
    dry_days_last_5        = get_summary(provincia_norm, survey_year - 5, survey_year - 1)[2],
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
  filter(BIRTH >= 1930
         , BIRTH <= 2002)

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
library(kableExtra)
library(haven)

survey <- readr::read_csv(
  "survey_with_childhood_weather_harmonized.csv",
  show_col_types = FALSE
)

analysis_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0
  )

# Check that 2024_w2 is present as survey_year 2025
analysis_data %>%
  count(survey_year, survey_wave, wave_2024) %>%
  arrange(survey_year, survey_wave) %>%
  print(n = Inf)

vars_all <- c(
  "CATHOLIC",
  "RELIGIOUS_PRACTICE",
  "COUPLE_CATHOLIC",
  "PARTICIPATION",
  "CONSERVATIVE_VOTE",
  "LEFT_RIGHT",
  "childhood_total_dry_days",
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

vars_all <- vars_all[vars_all %in% names(analysis_data)]

pretty_labels <- c(
  CATHOLIC = "Catholic",
  RELIGIOUS_PRACTICE = "Church attendance",
  COUPLE_CATHOLIC = "Catholic partner",
  PARTICIPATION = "Participation",
  CONSERVATIVE_VOTE = "Conservative vote",
  LEFT_RIGHT = "Ideological positioning",
  childhood_total_dry_days = "Dry days",
  FEMALE = "Female",
  age = "Age",
  EDUCATION = "Education",
  INCOME = "Income",
  FATHER_BORN_SPAIN = "Father born Spain",
  MOTHER_BORN_SPAIN = "Mother born Spain",
  FATHER_EMPLOYMENT = "Father employment",
  MOTHER_EMPLOYMENT = "Mother employment",
  FATHER_SCHOOL = "Father school",
  MOTHER_SCHOOL = "Mother school",
  FATHER_CATHOLIC = "Father Catholic",
  MOTHER_CATHOLIC = "Mother Catholic",
  survey_year = "Survey year",
  pop_birth_last_census = "Province population at birth",
  SAME_LOC_BIRTH = "Dummy living province of birth"
)

desc_map <- c(
  CATHOLIC = "Respondent identifies as Catholic (dummy).",
  RELIGIOUS_PRACTICE = "Religious practice frequency (higher = more frequent).",
  COUPLE_CATHOLIC = "Respondent identifies partner as Catholic (dummy).",
  PARTICIPATION = "Voted in the last election (dummy).",
  CONSERVATIVE_VOTE = "Voted for a conservative party in the last election (dummy).",
  LEFT_RIGHT = "Self-placement on left-right scale (0 = left, 10 = right).",
  childhood_total_dry_days = "Total number of dry days during childhood (5--18) in province of birth.",
  FEMALE = "Respondent gender (dummy).",
  age = "Age at survey (years).",
  EDUCATION = "Education category/level.",
  INCOME = "Income category.",
  FATHER_BORN_SPAIN = "Father born in Spain (dummy).",
  MOTHER_BORN_SPAIN = "Mother born in Spain (dummy).",
  FATHER_EMPLOYMENT = "Father employed during respondent's youth (dummy).",
  MOTHER_EMPLOYMENT = "Mother employed during respondent's youth (dummy).",
  FATHER_SCHOOL = "Father attended school (dummy).",
  MOTHER_SCHOOL = "Mother attended school (dummy).",
  FATHER_CATHOLIC = "Father identifies as Catholic (dummy).",
  MOTHER_CATHOLIC = "Mother identifies as Catholic (dummy).",
  survey_year = "Survey year.",
  pop_birth_last_census = "Province population at birth (census-based).",
  SAME_LOC_BIRTH = "Lives in same province as birth (dummy)."
)

summ_one <- function(x) {
  x <- haven::zap_labels(x)
  x <- suppressWarnings(as.numeric(x))
  
  tibble(
    N = sum(!is.na(x)),
    Min = ifelse(all(is.na(x)), NA_real_, min(x, na.rm = TRUE)),
    Mean = ifelse(all(is.na(x)), NA_real_, mean(x, na.rm = TRUE)),
    Max = ifelse(all(is.na(x)), NA_real_, max(x, na.rm = TRUE))
  )
}

tab1 <- lapply(vars_all, function(v) {
  out <- summ_one(analysis_data[[v]])
  out$variable <- v
  out
}) %>%
  bind_rows() %>%
  mutate(
    Variable = unname(pretty_labels[variable]),
    Variable = if_else(is.na(Variable), variable, Variable),
    Description = unname(desc_map[variable]),
    Description = if_else(is.na(Description), "", Description)
  ) %>%
  select(Variable, N, Min, Mean, Max, Description) %>%
  mutate(
    across(c(Min, Mean, Max), ~ round(.x, 2))
  )

tab1

variable_table_latex <- kbl(
  tab1,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary statistics (analysis sample).",
  align = c("l", "r", "r", "r", "r", "l"),
  escape = TRUE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  ) %>%
  column_spec(6, width = "5cm")

variable_table_latex


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



# 3) Std diffs vs Q1 (raw)

smd_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Raw_Q2vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 2),
    Raw_Q3vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 3),
    Raw_Q4vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 4)
  )
}) %>% bind_rows()


# BALANCE PLOT + BALANCE TABLE
# Raw balance and balance after birth-year + province FE


suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(fixest)
  library(haven)
  library(kableExtra)
})


# 0) Load + build analysis sample


survey <- readr::read_csv(
  "survey_with_childhood_weather_harmonized.csv",
  show_col_types = FALSE
)

model_data <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(BIRTH),
    !is.na(prov_nac)
  ) %>%
  mutate(
    treat_std = (
      childhood_total_dry_days -
        mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(treat_std, 4)
  )

balance_vars <- c(
  "FEMALE", "age", "EDUCATION", "INCOME",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL",
  "FATHER_CATHOLIC", "MOTHER_CATHOLIC",
  "survey_year", "pop_birth_last_census", "SAME_LOC_BIRTH"
)

balance_vars <- balance_vars[balance_vars %in% names(model_data)]

# Drop variables that are entirely missing
balance_vars <- balance_vars[
  sapply(model_data[balance_vars], function(x) !all(is.na(haven::zap_labels(x))))
]

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

label_var <- function(x) {
  out <- unname(pretty_labels[x])
  ifelse(is.na(out), x, out)
}


# 1) Helpers


as_numeric_clean <- function(x) {
  x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

mean_by_q <- function(x, q, k) {
  x <- as_numeric_clean(x)
  mean(x[q == k], na.rm = TRUE)
}

std_diff_q1_q4_abs <- function(x, q) {
  x <- as_numeric_clean(x)
  
  x1 <- x[q == 1]
  x4 <- x[q == 4]
  
  m1 <- mean(x1, na.rm = TRUE)
  m4 <- mean(x4, na.rm = TRUE)
  
  s <- sqrt((var(x1, na.rm = TRUE) + var(x4, na.rm = TRUE)) / 2)
  
  if (!is.finite(s) || s == 0) return(NA_real_)
  
  abs((m4 - m1) / s)
}

std_diff_vs_q1 <- function(x, q, k) {
  x <- as_numeric_clean(x)
  
  x1 <- x[q == 1]
  xk <- x[q == k]
  
  m1 <- mean(x1, na.rm = TRUE)
  mk <- mean(xk, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE)
  
  if (!is.finite(s1) || s1 == 0) return(NA_real_)
  
  (mk - m1) / s1
}

residualize_birth_prov <- function(v, data) {
  x <- as_numeric_clean(data[[v]])
  
  tmp <- data %>%
    mutate(.x = x) %>%
    filter(
      !is.na(.x),
      !is.na(BIRTH),
      !is.na(prov_nac),
      !is.na(treat_q)
    )
  
  if (nrow(tmp) == 0) {
    return(rep(NA_real_, nrow(data)))
  }
  
  if (sd(tmp$.x, na.rm = TRUE) == 0) {
    return(rep(NA_real_, nrow(data)))
  }
  
  fe_mod <- feols(
    .x ~ 1 | BIRTH + prov_nac,
    data = tmp,
    notes = FALSE
  )
  
  out <- rep(NA_real_, nrow(data))
  out[as.integer(rownames(tmp))] <- residuals(fe_mod)
  out
}


# 2) PLOT: Absolute SMD Q1 vs Q4


raw_smd <- sapply(balance_vars, function(v) {
  std_diff_q1_q4_abs(model_data[[v]], model_data$treat_q)
})

fe_smd <- sapply(balance_vars, function(v) {
  x_resid <- residualize_birth_prov(v, model_data)
  std_diff_q1_q4_abs(x_resid, model_data$treat_q)
})

balance_plot <- tibble(
  variable = balance_vars,
  Raw = as.numeric(raw_smd),
  `After FE (Birth year + province)` = as.numeric(fe_smd)
) %>%
  pivot_longer(
    cols = -variable,
    names_to = "spec",
    values_to = "smd"
  ) %>%
  mutate(
    variable_label = label_var(variable),
    variable_label = factor(
      variable_label,
      levels = rev(label_var(balance_vars))
    ),
    spec = factor(
      spec,
      levels = c("Raw", "After FE (Birth year + province)")
    )
  )

p_balance <- ggplot(
  balance_plot,
  aes(x = smd, y = variable_label, color = spec, shape = spec)
) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.45),
    na.rm = TRUE
  ) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  labs(
    title = "Balance of observables across treatment quartiles",
    subtitle = "Absolute standardized difference between Q1 and Q4, raw and after FE",
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

ggsave(
  filename = "balance_plot.png",
  plot = p_balance,
  width = 9,
  height = 5,
  dpi = 300
)


# 3) TABLE: Means Q1–Q4 + SMD vs Q1


means_raw <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Q1 = mean_by_q(model_data[[v]], model_data$treat_q, 1),
    Q2 = mean_by_q(model_data[[v]], model_data$treat_q, 2),
    Q3 = mean_by_q(model_data[[v]], model_data$treat_q, 3),
    Q4 = mean_by_q(model_data[[v]], model_data$treat_q, 4)
  )
}) %>%
  bind_rows()

smd_raw_tbl <- lapply(balance_vars, function(v) {
  tibble(
    variable = v,
    Raw_Q2vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 2),
    Raw_Q3vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 3),
    Raw_Q4vsQ1 = std_diff_vs_q1(model_data[[v]], model_data$treat_q, 4)
  )
}) %>%
  bind_rows()

smd_fe_tbl <- lapply(balance_vars, function(v) {
  x_resid <- residualize_birth_prov(v, model_data)
  
  tibble(
    variable = v,
    FE_Q2vsQ1 = std_diff_vs_q1(x_resid, model_data$treat_q, 2),
    FE_Q3vsQ1 = std_diff_vs_q1(x_resid, model_data$treat_q, 3),
    FE_Q4vsQ1 = std_diff_vs_q1(x_resid, model_data$treat_q, 4)
  )
}) %>%
  bind_rows()

tab <- means_raw %>%
  left_join(smd_raw_tbl, by = "variable") %>%
  left_join(smd_fe_tbl, by = "variable") %>%
  mutate(
    variable_label = label_var(variable),
    variable_label = factor(
      variable_label,
      levels = label_var(balance_vars)
    )
  ) %>%
  arrange(variable_label) %>%
  select(-variable) %>%
  rename(Variable = variable_label) %>%
  mutate(
    Variable = as.character(Variable),
    across(where(is.numeric), ~ round(.x, 3))
  )

n_by_q <- model_data %>%
  count(treat_q) %>%
  arrange(treat_q) %>%
  pull(n)

if (length(n_by_q) < 4) {
  stop("Less than four treatment quartiles were created. Check treat_q.")
}

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
  format = "latex",
  booktabs = TRUE,
  align = c("l", rep("r", ncol(tab) - 1)),
  caption = paste(
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

# Covariates
covars <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "CATHOLIC", "FATHER_EMPLOYMENT",
  "MOTHER_BORN_SPAIN", "CONSERVATIVE_VOTE", "MOTHER_EMPLOYMENT",
  "survey_year", "EDUCATION", "INCOME", "TRUST_PEOPLE",
  "pop_birth_last_census", "SAME_LOC_BIRTH"
)

# Optional: pretty labels
# If you already defined pretty_labels elsewhere, this will not overwrite it.
if (!exists("pretty_labels")) {
  pretty_labels <- c(
    FEMALE = "Female",
    FATHER_BORN_SPAIN = "Father born in Spain",
    CATHOLIC = "Catholic",
    FATHER_EMPLOYMENT = "Father employed",
    MOTHER_BORN_SPAIN = "Mother born in Spain",
    CONSERVATIVE_VOTE = "Conservative vote",
    MOTHER_EMPLOYMENT = "Mother employed",
    survey_year = "Survey year",
    EDUCATION = "Education",
    INCOME = "Income",
    TRUST_PEOPLE = "Trust in people",
    pop_birth_last_census = "Population at birth, last census",
    SAME_LOC_BIRTH = "Same location as birth"
  )
}

# Create quartiles of treatment
dat <- survey_final %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days)
  ) %>%
  mutate(
    dry_q = ntile(childhood_total_dry_days, 4)
  )

# Function for SMD: Q1 vs Q4
smd_fun <- function(x, g) {
  x <- as.numeric(x)
  
  x1 <- x[g == 1]
  x4 <- x[g == 4]
  
  m1 <- mean(x1, na.rm = TRUE)
  m4 <- mean(x4, na.rm = TRUE)
  
  v1 <- var(x1, na.rm = TRUE)
  v4 <- var(x4, na.rm = TRUE)
  
  s <- sqrt((v1 + v4) / 2)
  
  if (is.na(s) || s == 0) return(NA_real_)
  
  (m1 - m4) / s
}

# Compute means by quartile
means_by_q <- dat %>%
  group_by(dry_q) %>%
  summarise(
    across(
      all_of(covars),
      ~ mean(as.numeric(.x), na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -dry_q,
    names_to = "variable",
    values_to = "mean"
  ) %>%
  pivot_wider(
    names_from = dry_q,
    values_from = mean,
    names_prefix = "Q"
  )

# Compute SMDs
smd_table <- tibble(
  variable = covars,
  `SMD (Q1 vs Q4)` = vapply(
    covars,
    function(v) smd_fun(dat[[v]], dat$dry_q),
    numeric(1)
  )
)

# Merge means and SMDs
final_balance <- means_by_q %>%
  left_join(smd_table, by = "variable") %>%
  mutate(
    variable_raw = as.character(variable),
    variable = unname(pretty_labels[variable_raw]),
    variable = if_else(is.na(variable), variable_raw, variable)
  ) %>%
  select(
    variable,
    Q1, Q2, Q3, Q4,
    `SMD (Q1 vs Q4)`
  )

# View table
final_balance

# Optional LaTeX table
kable(
  final_balance,
  format = "latex",
  digits = 3,
  caption = "Covariate Balance by Quartiles of Childhood Dry Days"
) %>%
  kable_styling(latex_options = "hold_position")

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

# --- Means by quartile 
means_by_q <- dat %>%
  group_by(dry_q) %>%
  summarise(across(all_of(covars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-dry_q, names_to = "variable", values_to = "mean") %>%
  pivot_wider(names_from = dry_q, values_from = mean, names_prefix = "Q")

# --- SMDs (Q1 vs Q4) 
smd_table <- tibble(
  variable = covars,
  `SMD (Q1 vs Q4)` = sapply(covars, function(v) smd_fun(dat[[v]], dat$dry_q))
)

# --- Merge & order 
final_balance <- means_by_q %>%
  left_join(smd_table, by = "variable") %>%
  mutate(variable = factor(variable, levels = covars)) %>%
  arrange(variable)

# --- Pretty labels (optional)
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

# --- LaTeX table 
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


# BALANCE TABLE: p-value of difference between Q1 and Q4
# Raw and After FE (Birth year + province)

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
  pop_birth_last_census = "Province population at birth",
  SAME_LOC_BIRTH = "Same province at birth"
)

# 4) Helper function: extract Q4 vs Q1 p-value

get_q4_pvals <- function(v, data, cluster_se = TRUE) {
  
  d <- data %>%
    dplyr::select(dplyr::all_of(c(v, "q4", "BIRTH", "prov_nac", "survey_year"))) %>%
    dplyr::filter(
      !is.na(.data[[v]]),
      !is.na(q4),
      !is.na(BIRTH),
      !is.na(prov_nac),
      !is.na(survey_year)
    )
  
  # Safety: if there is no variation in q4, return NA
  if (dplyr::n_distinct(d$q4) < 2) {
    return(tibble::tibble(
      variable = v,
      mean_q1 = mean(d[[v]][d$q4 == 0], na.rm = TRUE),
      mean_q4 = mean(d[[v]][d$q4 == 1], na.rm = TRUE),
      diff_raw = NA_real_,
      p_raw = NA_real_,
      diff_fe = NA_real_,
      p_fe = NA_real_,
      n_q1 = sum(d$q4 == 0),
      n_q4 = sum(d$q4 == 1),
      n_raw = nrow(d),
      n_fe = NA_integer_
    ))
  }
  
  # Formulae
  fml_raw <- stats::as.formula(paste0(v, " ~ q4"))
  fml_fe  <- stats::as.formula(paste0(v, " ~ q4 | BIRTH + prov_nac + survey_year"))
  
  # Estimation
  if (cluster_se) {
    m_raw <- fixest::feols(fml_raw, data = d, cluster = ~ prov_nac)
    m_fe  <- fixest::feols(fml_fe,  data = d, cluster = ~ prov_nac)
  } else {
    m_raw <- fixest::feols(fml_raw, data = d)
    m_fe  <- fixest::feols(fml_fe,  data = d)
  }
  
  # Safe coefficient extraction
  ct_raw <- fixest::coeftable(m_raw)
  ct_fe  <- fixest::coeftable(m_fe)
  
  diff_raw <- if ("q4" %in% names(coef(m_raw))) unname(coef(m_raw)["q4"]) else NA_real_
  p_raw    <- if ("q4" %in% rownames(ct_raw))  unname(ct_raw["q4", "Pr(>|t|)"]) else NA_real_
  
  diff_fe  <- if ("q4" %in% names(coef(m_fe)))  unname(coef(m_fe)["q4"]) else NA_real_
  p_fe     <- if ("q4" %in% rownames(ct_fe))    unname(ct_fe["q4", "Pr(>|t|)"]) else NA_real_
  
  tibble::tibble(
    variable = v,
    mean_q1 = mean(d[[v]][d$q4 == 0], na.rm = TRUE),
    mean_q4 = mean(d[[v]][d$q4 == 1], na.rm = TRUE),
    diff_raw = diff_raw,
    p_raw = p_raw,
    diff_fe = diff_fe,
    p_fe = p_fe,
    n_q1 = sum(d$q4 == 0),
    n_q4 = sum(d$q4 == 1),
    n_raw = nobs(m_raw),
    n_fe = nobs(m_fe)
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


# PROVINCE-LEVEL SCATTER:
# Catholic share vs Conservative vote share
# Styled to match the presentation aesthetic


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


# Presentation-style theme

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


# Province-level means, excluding Barcelona and Araba/Álava
prov_summary_no_bar_alava <- prov_summary %>%
  filter(!provincia_official %in% c("Barcelona", "Araba/Álava"))

# Optional: inspect
print(prov_summary_no_bar_alava)

p_prov_corr_no_bar_alava <- ggplot(
  prov_summary_no_bar_alava,
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
print(p_prov_corr_no_bar_alava)

# Save if needed
ggsave(
  "province_catholic_conservative_scatter_no_Barcelona_alava.png",
  p_prov_corr_no_bar_alava,
  width = 12,
  height = 7,
  dpi = 600
)

ggsave(
  "province_catholic_conservative_scatter_no_Barcelona_alava.pdf",
  p_prov_corr_no_bar_alava,
  width = 12,
  height = 7,
  device = cairo_pdf
)


# Save in high quality

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


# Main regressions — no controls

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
    CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age,
    BIRTH, prov_nac, birth_ccaa, res_ccaa, survey_wave,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster,
    COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP,
    MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE,
    TRUST_PEOPLE, INCOME, EDUCATION, RELIGIOUS_PRACTICE, PARTICIPATION,
    SIZE_TOWN, dry_days_5_9, dry_days_10_14, dry_days_15_18, PP_VOTE,
    pop_birth_last_census, ECOLOGIST_SELF, ENV_CONCERN,
    SAME_TOWN_AT16, SAME_LOC_BIRTH
  )

# Log population at birth
model_data <- model_data %>%
  mutate(log_pop_birth = log(pop_birth_last_census))

# Standardize treatment
model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    childhood_total_dry_days_std_sq = childhood_total_dry_days_std^2
  )


# Minimal control set: FEMALE and log_pop_birth only.
# No missing-indicator method: observations with missing FEMALE are dropped
# by feols (log_pop_birth is non-missing by construction).

ctrls    <- c()
ctrl_str <- paste(ctrls, collapse = "  ")
fe_str   <- "BIRTH + prov_nac + survey_year"

make_fml <- function(y, spec = c("linear", "quadratic")) {
  spec  <- match.arg(spec)
  treat <- if (spec == "linear") {
    "childhood_total_dry_days_std"
  } else {
    "childhood_total_dry_days_std + childhood_total_dry_days_std_sq"
  }
  stats::as.formula(paste0(y, " ~ ", treat, " | ", fe_str))
}

fit <- function(y, spec) {
  feols(make_fml(y, spec), data = model_data, cluster = ~prov_nac)
}

# Mean DV on the estimation sample
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

lpm_fe_linear_cat    <- fit("CATHOLIC",           "linear")
lpm_fe_quadratic_cat <- fit("CATHOLIC",           "quadratic")
lpm_fe_linear_rel    <- fit("RELIGIOUS_PRACTICE", "linear")
lpm_fe_quadratic_rel <- fit("RELIGIOUS_PRACTICE", "quadratic")
lpm_fe_linear_cou    <- fit("COUPLE_CATHOLIC",    "linear")
lpm_fe_quadratic_cou <- fit("COUPLE_CATHOLIC",    "quadratic")

mean_cat_lin  <- dv_mean_from_data(model_data, make_fml("CATHOLIC",           "linear"))
mean_cat_quad <- dv_mean_from_data(model_data, make_fml("CATHOLIC",           "quadratic"))
mean_rel_lin  <- dv_mean_from_data(model_data, make_fml("RELIGIOUS_PRACTICE", "linear"))
mean_rel_quad <- dv_mean_from_data(model_data, make_fml("RELIGIOUS_PRACTICE", "quadratic"))
mean_cou_lin  <- dv_mean_from_data(model_data, make_fml("COUPLE_CATHOLIC",    "linear"))
mean_cou_quad <- dv_mean_from_data(model_data, make_fml("COUPLE_CATHOLIC",    "quadratic"))

add_rows_religion <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Catholic: Linear"              = c(sprintf("%.3f", mean_cat_lin),  "Linear"),
  "Catholic: Quadratic"           = c(sprintf("%.3f", mean_cat_quad), "Quadratic"),
  "Religious practice: Linear"    = c(sprintf("%.3f", mean_rel_lin),  "Linear"),
  "Religious practice: Quadratic" = c(sprintf("%.3f", mean_rel_quad), "Quadratic"),
  "Couple catholic: Linear"       = c(sprintf("%.3f", mean_cou_lin),  "Linear"),
  "Couple catholic: Quadratic"    = c(sprintf("%.3f", mean_cou_quad), "Quadratic")
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
  title = "Religious outcomes (no controls): linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_religion
)


# POLITICAL OUTCOMES

lpm_fe_linear_par    <- fit("PARTICIPATION",     "linear")
lpm_fe_quadratic_par <- fit("PARTICIPATION",     "quadratic")
lpm_fe_linear_con    <- fit("CONSERVATIVE_VOTE", "linear")
lpm_fe_quadratic_con <- fit("CONSERVATIVE_VOTE", "quadratic")
lpm_fe_linear_lr     <- fit("LEFT_RIGHT",        "linear")
lpm_fe_quadratic_lr  <- fit("LEFT_RIGHT",        "quadratic")

mean_par_lin  <- dv_mean_from_data(model_data, make_fml("PARTICIPATION",     "linear"))
mean_par_quad <- dv_mean_from_data(model_data, make_fml("PARTICIPATION",     "quadratic"))
mean_con_lin  <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_VOTE", "linear"))
mean_con_quad <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_VOTE", "quadratic"))
mean_lr_lin   <- dv_mean_from_data(model_data, make_fml("LEFT_RIGHT",        "linear"))
mean_lr_quad  <- dv_mean_from_data(model_data, make_fml("LEFT_RIGHT",        "quadratic"))

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
  title = "Political outcomes (no controls): linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_politics
)


# ENVIRONMENTAL CONCERNS AND MIGRATION

lpm_fe_linear_eco    <- fit("ECOLOGIST_SELF", "linear")
lpm_fe_quadratic_eco <- fit("ECOLOGIST_SELF", "quadratic")
lpm_fe_linear_env    <- fit("ENV_CONCERN",    "linear")
lpm_fe_quadratic_env <- fit("ENV_CONCERN",    "quadratic")
lpm_fe_linear_mig    <- fit("SAME_LOC_BIRTH", "linear")
lpm_fe_quadratic_mig <- fit("SAME_LOC_BIRTH", "quadratic")

mean_eco_lin  <- dv_mean_from_data(model_data, make_fml("ECOLOGIST_SELF", "linear"))
mean_eco_quad <- dv_mean_from_data(model_data, make_fml("ECOLOGIST_SELF", "quadratic"))
mean_env_lin  <- dv_mean_from_data(model_data, make_fml("ENV_CONCERN",    "linear"))
mean_env_quad <- dv_mean_from_data(model_data, make_fml("ENV_CONCERN",    "quadratic"))
mean_mig_lin  <- dv_mean_from_data(model_data, make_fml("SAME_LOC_BIRTH", "linear"))
mean_mig_quad <- dv_mean_from_data(model_data, make_fml("SAME_LOC_BIRTH", "quadratic"))

models_envmig <- list(
  "Environmental: Linear"    = lpm_fe_linear_env,
  "Environmental: Quadratic" = lpm_fe_quadratic_env
)

add_rows_envmig <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Environmental: Linear"    = c(sprintf("%.3f", mean_env_lin),  "Linear"),
  "Environmental: Quadratic" = c(sprintf("%.3f", mean_env_quad), "Quadratic")
)

modelsummary(
  models_envmig,
  title = "Environmental and migration outcomes (no controls): linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_envmig
)


# OTHER OUTCOMES / FALSIFICATION

lpm_fe_linear_edu      <- fit("EDUCATION",    "linear")
lpm_fe_quadratic_edu   <- fit("EDUCATION",    "quadratic")
lpm_fe_linear_inc      <- fit("INCOME",       "linear")
lpm_fe_quadratic_inc   <- fit("INCOME",       "quadratic")
lpm_fe_linear_trust    <- fit("TRUST_PEOPLE", "linear")
lpm_fe_quadratic_trust <- fit("TRUST_PEOPLE", "quadratic")

mean_edu_lin  <- dv_mean_from_data(model_data, make_fml("EDUCATION",    "linear"))
mean_edu_quad <- dv_mean_from_data(model_data, make_fml("EDUCATION",    "quadratic"))
mean_inc_lin  <- dv_mean_from_data(model_data, make_fml("INCOME",       "linear"))
mean_inc_quad <- dv_mean_from_data(model_data, make_fml("INCOME",       "quadratic"))
mean_tru_lin  <- dv_mean_from_data(model_data, make_fml("TRUST_PEOPLE", "linear"))
mean_tru_quad <- dv_mean_from_data(model_data, make_fml("TRUST_PEOPLE", "quadratic"))

models_falsi <- list(
  "Education: Linear"            = lpm_fe_linear_edu,
  "Education: Quadratic"         = lpm_fe_quadratic_edu,
  "Household income: Linear"     = lpm_fe_linear_inc,
  "Household income: Quadratic"  = lpm_fe_quadratic_inc,
  "Trust people: Linear"         = lpm_fe_linear_trust,
  "Trust people: Quadratic"      = lpm_fe_quadratic_trust
)

add_rows_falsi <- data.frame(
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
  models_falsi,
  title = "Other outcomes (no controls): linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_falsi
)


# PARTY-SPECIFIC POLITICAL OUTCOMES:
# Conservative vote excluding far-right and far-right vote
# Note: FAR_RIGHT_VOTE is only observed in 2023/2024/2025 waves.

model_data <- model_data %>%
  mutate(
    CONSERVATIVE_NO_FAR_RIGHT = case_when(
      is.na(CONSERVATIVE_VOTE) ~ NA_real_,
      !is.na(FAR_RIGHT_VOTE) & FAR_RIGHT_VOTE == 1 ~ 0,
      TRUE ~ as.numeric(CONSERVATIVE_VOTE)
    )
  )

lpm_fe_linear_con_nofr    <- fit("CONSERVATIVE_NO_FAR_RIGHT", "linear")
lpm_fe_quadratic_con_nofr <- fit("CONSERVATIVE_NO_FAR_RIGHT", "quadratic")

lpm_fe_linear_far         <- fit("FAR_RIGHT_VOTE", "linear")
lpm_fe_quadratic_far      <- fit("FAR_RIGHT_VOTE", "quadratic")

mean_con_nofr_lin  <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_NO_FAR_RIGHT", "linear"))
mean_con_nofr_quad <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_NO_FAR_RIGHT", "quadratic"))

mean_far_lin       <- dv_mean_from_data(model_data, make_fml("FAR_RIGHT_VOTE", "linear"))
mean_far_quad      <- dv_mean_from_data(model_data, make_fml("FAR_RIGHT_VOTE", "quadratic"))

models_party_vote <- list(
  "Conservative excl. far-right: Linear"    = lpm_fe_linear_con_nofr,
  "Conservative excl. far-right: Quadratic" = lpm_fe_quadratic_con_nofr,
  "Far-right: Linear"                       = lpm_fe_linear_far,
  "Far-right: Quadratic"                    = lpm_fe_quadratic_far
)

add_rows_party_vote <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Conservative excl. far-right: Linear"    = c(sprintf("%.3f", mean_con_nofr_lin),  "Linear"),
  "Conservative excl. far-right: Quadratic" = c(sprintf("%.3f", mean_con_nofr_quad), "Quadratic"),
  "Far-right: Linear"                       = c(sprintf("%.3f", mean_far_lin),       "Linear"),
  "Far-right: Quadratic"                    = c(sprintf("%.3f", mean_far_quad),      "Quadratic")
)

modelsummary(
  models_party_vote,
  title = "Party-specific political outcomes (minimal controls): conservative vote excluding far-right and far-right vote",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_party_vote,
  notes = "Conservative excl. far-right removes far-right voters from the conservative-vote category. Far-right vote is observed only in the 2023/2024/2025 survey waves."
)


# Main regressions MIM controls -------------------------------------------

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
    CATHOLIC, childhood_total_dry_days, survey_year, FEMALE, age,
    BIRTH, prov_nac, birth_ccaa, res_ccaa, survey_wave,
    FATHER_BORN_SPAIN, FATHER_SCHOOL, FATHER_EDUCATION,
    FATHER_EMPLOYMENT, FATHER_EMPLOYMENT_TYPE, FATHER_CATHOLIC,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL, MOTHER_EDUCATION,
    MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster,
    COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP,
    MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE,
    TRUST_PEOPLE, INCOME, EDUCATION, RELIGIOUS_PRACTICE, PARTICIPATION,
    SIZE_TOWN, dry_days_5_9, dry_days_10_14, dry_days_15_18, PP_VOTE, 
    pop_birth_last_census, ECOLOGIST_SELF, ENV_CONCERN,
    SAME_TOWN_AT16, SAME_LOC_BIRTH
  )

# Log population at birth
model_data <- model_data %>%
  mutate(log_pop_birth = log(pop_birth_last_census))

# Standardize treatment
model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    childhood_total_dry_days_std_sq = childhood_total_dry_days_std^2
  )


# MISSING-INDICATOR METHOD for all individual + parental controls

mim_vars <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_SCHOOL",     "MOTHER_SCHOOL",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

model_data <- model_data %>%
  # 1) create *_MISS dummies BEFORE imputing
  mutate(across(all_of(mim_vars),
                ~ as.integer(is.na(.x)),
                .names = "{.col}_MISS")) %>%
  # 2) fill the original variable with 0 where missing
  mutate(across(all_of(mim_vars),
                ~ as.integer(ifelse(is.na(.x), 0L, .x))))

# Sanity check: zero missings everywhere
model_data %>%
  summarise(across(all_of(c(mim_vars, paste0(mim_vars, "_MISS"))),
                   ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  print(n = Inf)


# Single control set + helpers used by every regression

ctrls <- c(
  mim_vars,
  paste0(mim_vars, "_MISS"),
  "log_pop_birth"
)
ctrl_str <- paste(ctrls, collapse = " + ")
fe_str   <- "BIRTH + prov_nac + survey_year"

make_fml <- function(y, spec = c("linear", "quadratic")) {
  spec  <- match.arg(spec)
  treat <- if (spec == "linear") {
    "childhood_total_dry_days_std"
  } else {
    "childhood_total_dry_days_std + childhood_total_dry_days_std_sq"
  }
  stats::as.formula(paste0(y, " ~ ", treat, " + ", ctrl_str, " | ", fe_str))
}

fit <- function(y, spec) {
  feols(make_fml(y, spec), data = model_data, cluster = ~prov_nac)
}

# Mean DV on the estimation sample
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

lpm_fe_linear_cat    <- fit("CATHOLIC",           "linear")
lpm_fe_quadratic_cat <- fit("CATHOLIC",           "quadratic")
lpm_fe_linear_rel    <- fit("RELIGIOUS_PRACTICE", "linear")
lpm_fe_quadratic_rel <- fit("RELIGIOUS_PRACTICE", "quadratic")
lpm_fe_linear_cou    <- fit("COUPLE_CATHOLIC",    "linear")
lpm_fe_quadratic_cou <- fit("COUPLE_CATHOLIC",    "quadratic")

mean_cat_lin  <- dv_mean_from_data(model_data, make_fml("CATHOLIC",           "linear"))
mean_cat_quad <- dv_mean_from_data(model_data, make_fml("CATHOLIC",           "quadratic"))
mean_rel_lin  <- dv_mean_from_data(model_data, make_fml("RELIGIOUS_PRACTICE", "linear"))
mean_rel_quad <- dv_mean_from_data(model_data, make_fml("RELIGIOUS_PRACTICE", "quadratic"))
mean_cou_lin  <- dv_mean_from_data(model_data, make_fml("COUPLE_CATHOLIC",    "linear"))
mean_cou_quad <- dv_mean_from_data(model_data, make_fml("COUPLE_CATHOLIC",    "quadratic"))

add_rows_religion <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Catholic: Linear"              = c(sprintf("%.3f", mean_cat_lin),  "Linear"),
  "Catholic: Quadratic"           = c(sprintf("%.3f", mean_cat_quad), "Quadratic"),
  "Religious practice: Linear"    = c(sprintf("%.3f", mean_rel_lin),  "Linear"),
  "Religious practice: Quadratic" = c(sprintf("%.3f", mean_rel_quad), "Quadratic"),
  "Couple catholic: Linear"       = c(sprintf("%.3f", mean_cou_lin),  "Linear"),
  "Couple catholic: Quadratic"    = c(sprintf("%.3f", mean_cou_quad), "Quadratic")
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

lpm_fe_linear_par    <- fit("PARTICIPATION",     "linear")
lpm_fe_quadratic_par <- fit("PARTICIPATION",     "quadratic")
lpm_fe_linear_con    <- fit("CONSERVATIVE_VOTE", "linear")
lpm_fe_quadratic_con <- fit("CONSERVATIVE_VOTE", "quadratic")
lpm_fe_linear_lr     <- fit("LEFT_RIGHT",        "linear")
lpm_fe_quadratic_lr  <- fit("LEFT_RIGHT",        "quadratic")

mean_par_lin  <- dv_mean_from_data(model_data, make_fml("PARTICIPATION",     "linear"))
mean_par_quad <- dv_mean_from_data(model_data, make_fml("PARTICIPATION",     "quadratic"))
mean_con_lin  <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_VOTE", "linear"))
mean_con_quad <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_VOTE", "quadratic"))
mean_lr_lin   <- dv_mean_from_data(model_data, make_fml("LEFT_RIGHT",        "linear"))
mean_lr_quad  <- dv_mean_from_data(model_data, make_fml("LEFT_RIGHT",        "quadratic"))

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


# ENVIRONMENTAL CONCERNS AND MIGRATION

lpm_fe_linear_eco    <- fit("ECOLOGIST_SELF", "linear")
lpm_fe_quadratic_eco <- fit("ECOLOGIST_SELF", "quadratic")
lpm_fe_linear_env    <- fit("ENV_CONCERN",    "linear")
lpm_fe_quadratic_env <- fit("ENV_CONCERN",    "quadratic")
lpm_fe_linear_mig    <- fit("SAME_LOC_BIRTH", "linear")
lpm_fe_quadratic_mig <- fit("SAME_LOC_BIRTH", "quadratic")

mean_eco_lin  <- dv_mean_from_data(model_data, make_fml("ECOLOGIST_SELF", "linear"))
mean_eco_quad <- dv_mean_from_data(model_data, make_fml("ECOLOGIST_SELF", "quadratic"))
mean_env_lin  <- dv_mean_from_data(model_data, make_fml("ENV_CONCERN",    "linear"))
mean_env_quad <- dv_mean_from_data(model_data, make_fml("ENV_CONCERN",    "quadratic"))
mean_mig_lin  <- dv_mean_from_data(model_data, make_fml("SAME_LOC_BIRTH", "linear"))
mean_mig_quad <- dv_mean_from_data(model_data, make_fml("SAME_LOC_BIRTH", "quadratic"))

models_envmig <- list(
  "Environmental: Linear"    = lpm_fe_linear_env,
  "Environmental: Quadratic" = lpm_fe_quadratic_env
)

add_rows_envmig <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,

  "Environmental: Linear"    = c(sprintf("%.3f", mean_env_lin),  "Linear"),
  "Environmental: Quadratic" = c(sprintf("%.3f", mean_env_quad), "Quadratic")
)

modelsummary(
  models_envmig,
  title = "Environmental and migration outcomes: linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_envmig
)


# OTHER OUTCOMES / FALSIFICATION

lpm_fe_linear_edu    <- fit("EDUCATION",    "linear")
lpm_fe_quadratic_edu <- fit("EDUCATION",    "quadratic")
lpm_fe_linear_inc    <- fit("INCOME",       "linear")
lpm_fe_quadratic_inc <- fit("INCOME",       "quadratic")
lpm_fe_linear_trust  <- fit("TRUST_PEOPLE", "linear")
lpm_fe_quadratic_trust <- fit("TRUST_PEOPLE", "quadratic")

mean_edu_lin  <- dv_mean_from_data(model_data, make_fml("EDUCATION",    "linear"))
mean_edu_quad <- dv_mean_from_data(model_data, make_fml("EDUCATION",    "quadratic"))
mean_inc_lin  <- dv_mean_from_data(model_data, make_fml("INCOME",       "linear"))
mean_inc_quad <- dv_mean_from_data(model_data, make_fml("INCOME",       "quadratic"))
mean_tru_lin  <- dv_mean_from_data(model_data, make_fml("TRUST_PEOPLE", "linear"))
mean_tru_quad <- dv_mean_from_data(model_data, make_fml("TRUST_PEOPLE", "quadratic"))

models_falsi <- list(
  "Education: Linear"            = lpm_fe_linear_edu,
  "Education: Quadratic"         = lpm_fe_quadratic_edu,
  "Household income: Linear"     = lpm_fe_linear_inc,
  "Household income: Quadratic"  = lpm_fe_quadratic_inc,
  "Trust people: Linear"         = lpm_fe_linear_trust,
  "Trust people: Quadratic"      = lpm_fe_quadratic_trust
)

add_rows_falsi <- data.frame(
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
  models_falsi,
  title = "Other outcomes: linear and quadratic treatment specifications",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_falsi
)


# PARTY-SPECIFIC POLITICAL OUTCOMES:
# Conservative vote excluding far-right and far-right vote
# Note: FAR_RIGHT_VOTE is only observed in 2023/2024/2025 waves.

model_data <- model_data %>%
  mutate(
    CONSERVATIVE_NO_FAR_RIGHT = case_when(
      is.na(CONSERVATIVE_VOTE) ~ NA_real_,
      !is.na(FAR_RIGHT_VOTE) & FAR_RIGHT_VOTE == 1 ~ 0,
      TRUE ~ as.numeric(CONSERVATIVE_VOTE)
    )
  )

lpm_fe_linear_con_nofr    <- fit("CONSERVATIVE_NO_FAR_RIGHT", "linear")
lpm_fe_quadratic_con_nofr <- fit("CONSERVATIVE_NO_FAR_RIGHT", "quadratic")

lpm_fe_linear_far         <- fit("FAR_RIGHT_VOTE", "linear")
lpm_fe_quadratic_far      <- fit("FAR_RIGHT_VOTE", "quadratic")

mean_con_nofr_lin  <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_NO_FAR_RIGHT", "linear"))
mean_con_nofr_quad <- dv_mean_from_data(model_data, make_fml("CONSERVATIVE_NO_FAR_RIGHT", "quadratic"))

mean_far_lin       <- dv_mean_from_data(model_data, make_fml("FAR_RIGHT_VOTE", "linear"))
mean_far_quad      <- dv_mean_from_data(model_data, make_fml("FAR_RIGHT_VOTE", "quadratic"))

models_party_vote <- list(
  "Conservative excl. far-right: Linear"    = lpm_fe_linear_con_nofr,
  "Conservative excl. far-right: Quadratic" = lpm_fe_quadratic_con_nofr,
  "Far-right: Linear"                       = lpm_fe_linear_far,
  "Far-right: Quadratic"                    = lpm_fe_quadratic_far
)

add_rows_party_vote <- data.frame(
  term = c("Mean dep. var.", "Treatment form"),
  check.names = FALSE,
  "Conservative excl. far-right: Linear"    = c(sprintf("%.3f", mean_con_nofr_lin),  "Linear"),
  "Conservative excl. far-right: Quadratic" = c(sprintf("%.3f", mean_con_nofr_quad), "Quadratic"),
  "Far-right: Linear"                       = c(sprintf("%.3f", mean_far_lin),       "Linear"),
  "Far-right: Quadratic"                    = c(sprintf("%.3f", mean_far_quad),      "Quadratic")
)

modelsummary(
  models_party_vote,
  title = "Party-specific political outcomes: conservative vote excluding far-right and far-right vote",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_party_vote,
  notes = "Conservative excl. far-right removes far-right voters from the conservative-vote category. Far-right vote is observed only in the 2023/2024/2025 survey waves."
)


# Shared missing-indicator helpers ----------------------------------------

mim_vars <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_SCHOOL",     "MOTHER_SCHOOL",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

apply_mim_controls <- function(data, mim_vars) {
  mim_vars <- mim_vars[mim_vars %in% names(data)]
  
  for (v in mim_vars) {
    miss_v <- paste0(v, "_MISS")
    
    # Only create missing dummy if it does not already exist.
    # This avoids overwriting true missing dummies after imputation.
    if (!miss_v %in% names(data)) {
      data[[miss_v]] <- as.integer(is.na(data[[v]]))
    }
    
    data[[v]] <- ifelse(is.na(data[[v]]), 0, as.numeric(data[[v]]))
  }
  
  data
}

get_mim_controls <- function(data, mim_vars, extra_controls = "log_pop_birth") {
  mim_vars <- mim_vars[mim_vars %in% names(data)]
  miss_vars <- paste0(mim_vars, "_MISS")
  miss_vars <- miss_vars[miss_vars %in% names(data)]
  extra_controls <- extra_controls[extra_controls %in% names(data)]
  
  c(mim_vars, miss_vars, extra_controls)
}

# Ensure model_data has the same MIM structure
model_data <- apply_mim_controls(model_data, mim_vars)

ctrls <- get_mim_controls(model_data, mim_vars, extra_controls = "log_pop_birth")
ctrl_str <- paste(ctrls, collapse = " + ")
fe_str <- "BIRTH + prov_nac + survey_year"

make_fml <- function(y, spec = c("linear", "quadratic")) {
  spec <- match.arg(spec)
  
  treat <- if (spec == "linear") {
    "childhood_total_dry_days_std"
  } else {
    "childhood_total_dry_days_std + childhood_total_dry_days_std_sq"
  }
  
  stats::as.formula(
    paste0(y, " ~ ", treat, " + ", ctrl_str, " | ", fe_str)
  )
}

fit_cluster <- function(y, spec = c("linear", "quadratic"), cluster_var = "prov_nac") {
  spec <- match.arg(spec)
  
  d <- model_data %>%
    filter(!is.na(.data[[cluster_var]]))
  
  feols(
    make_fml(y, spec),
    data = d,
    cluster = as.formula(paste0("~", cluster_var)),
    notes = FALSE
  )
}


# Treatment not correlated with missing controls --------------------------

# A.X — Balance: is treatment correlated with missingness in controls?

miss_vars <- paste0(mim_vars, "_MISS")

# Mean share missing per variable (on the estimation sample)
miss_shares <- sapply(miss_vars, function(v) mean(model_data[[v]], na.rm = TRUE))

# Individual balance regressions: each missingness indicator on treatment + FE
balance_fit <- function(v) {
  fml <- stats::as.formula(
    paste0(v, " ~ childhood_total_dry_days_std | ", fe_str)
  )
  feols(fml, data = model_data, cluster = ~prov_nac)
}
balance_models <- lapply(miss_vars, balance_fit)
names(balance_models) <- c(
  "FEMALE missing",
  "Father born Spain missing",
  "Mother born Spain missing",
  "Father school missing",
  "Mother school missing",
  "Father employment missing",
  "Mother employment missing"
)

# Add a "share missing" row to the table
add_rows_balance <- as.data.frame(rbind(
  c("Share missing", sprintf("%.3f", miss_shares))
))
names(add_rows_balance) <- c("term", names(balance_models))

modelsummary(
  balance_models,
  title = "Balance: treatment vs. missingness in predetermined controls",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_rename = c("childhood_total_dry_days_std" = "Dry days (std.)"),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = add_rows_balance,
  notes = "Each column is a separate OLS regression of the missingness indicator on standardized childhood Holy Week dry days, with birth-year, birth-province, and survey-year fixed effects. Standard errors clustered at the province of birth."
)

# Joint test: regress treatment on ALL missingness indicators + FE,
# then test that they are jointly zero.
joint_fml <- stats::as.formula(
  paste0("childhood_total_dry_days_std ~ ",
         paste(miss_vars, collapse = " + "),
         " | ", fe_str)
)
joint_model <- feols(joint_fml, data = model_data, cluster = ~prov_nac)
joint_test  <- wald(joint_model, keep = miss_vars)
print(joint_test)

# Clustering SEs by Comunidad ---------------------------------------------

lpm_fe_linear_cat_ccaa    <- fit_cluster("CATHOLIC",           "linear",    "birth_ccaa")
lpm_fe_quadratic_cat_ccaa <- fit_cluster("CATHOLIC",           "quadratic", "birth_ccaa")
lpm_fe_linear_rel_ccaa    <- fit_cluster("RELIGIOUS_PRACTICE", "linear",    "birth_ccaa")
lpm_fe_quadratic_rel_ccaa <- fit_cluster("RELIGIOUS_PRACTICE", "quadratic", "birth_ccaa")
lpm_fe_linear_cou_ccaa    <- fit_cluster("COUPLE_CATHOLIC",    "linear",    "birth_ccaa")
lpm_fe_quadratic_cou_ccaa <- fit_cluster("COUPLE_CATHOLIC",    "quadratic", "birth_ccaa")

models_religion_ccaa <- list(
  "Catholic: Linear"              = lpm_fe_linear_cat_ccaa,
  "Catholic: Quadratic"           = lpm_fe_quadratic_cat_ccaa,
  "Religious practice: Linear"    = lpm_fe_linear_rel_ccaa,
  "Religious practice: Quadratic" = lpm_fe_quadratic_rel_ccaa,
  "Couple catholic: Linear"       = lpm_fe_linear_cou_ccaa,
  "Couple catholic: Quadratic"    = lpm_fe_quadratic_cou_ccaa
)

modelsummary(
  models_religion_ccaa,
  title = "Robustness: Religious outcomes with SE clustered by birth Comunidad Autónoma",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_religion,
  notes = "Missing values in predetermined controls are retained using missing-value indicators. Outcomes, treatment, birth year, birth province, and survey-year fixed effects are not imputed."
)

# ROBUSTNESS: Political outcomes with SE clustered by birth CCAA 

# PARTICIPATION
lpm_fe_linear_par_ccaa <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

lpm_fe_quadratic_par_ccaa <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

# CONSERVATIVE_VOTE
lpm_fe_linear_con_ccaa <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

lpm_fe_quadratic_con_ccaa <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

# LEFT_RIGHT
lpm_fe_linear_lr_ccaa <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

lpm_fe_quadratic_lr_ccaa <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + childhood_total_dry_days_std_sq + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ birth_ccaa
)

models_politics_ccaa <- list(
  "Participation: Linear"    = lpm_fe_linear_par_ccaa,
  "Participation: Quadratic" = lpm_fe_quadratic_par_ccaa,
  "Conservative: Linear"     = lpm_fe_linear_con_ccaa,
  "Conservative: Quadratic"  = lpm_fe_quadratic_con_ccaa,
  "Left-right: Linear"       = lpm_fe_linear_lr_ccaa,
  "Left-right: Quadratic"    = lpm_fe_quadratic_lr_ccaa
)

modelsummary(
  models_politics_ccaa,
  title = "Robustness: Political outcomes with SE clustered by birth Comunidad Autónoma",
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


# Interact effect with time: treatment x birth cohort ----------------------

library(dplyr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(tibble)
library(kableExtra)
library(tidyr)


# 1) Prepare interaction variables


# Store this once so the centering is consistent everywhere
birth_mean_global <- mean(model_data$BIRTH, na.rm = TRUE)

model_data <- model_data %>%
  mutate(
    # Log population at birth
    log_pop_birth = if_else(
      !is.na(pop_birth_last_census) & pop_birth_last_census > 0,
      log(pop_birth_last_census),
      NA_real_
    ),
    
    # Standardized treatment
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    
    # Quadratic treatment, if needed elsewhere
    childhood_total_dry_days_std_sq = childhood_total_dry_days_std^2,
    
    # Centered birth year in decades
    # The interaction coefficient = change in treatment effect per 10 birth years
    birth_decade_c = (BIRTH - birth_mean_global) / 10
  )


# 2) Controls used in main regressions: same MIM method --------------------

model_data <- apply_mim_controls(model_data, mim_vars)

main_controls <- get_mim_controls(
  model_data,
  mim_vars,
  extra_controls = "log_pop_birth"
)

make_birth_interaction_formula <- function(y) {
  rhs <- paste(
    c(
      "childhood_total_dry_days_std",
      "childhood_total_dry_days_std:birth_decade_c",
      main_controls
    ),
    collapse = " + "
  )
  
  as.formula(
    paste0(
      y, " ~ ", rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
}

run_birth_interaction <- function(y) {
  fml <- make_birth_interaction_formula(y)
  
  vars_needed <- unique(c(
    y,
    "childhood_total_dry_days_std",
    "birth_decade_c",
    main_controls,
    "BIRTH",
    "prov_nac",
    "survey_year"
  ))
  
  d <- model_data %>%
    dplyr::select(all_of(vars_needed)) %>%
    filter(
      !is.na(.data[[y]]),
      !is.na(childhood_total_dry_days_std),
      !is.na(birth_decade_c),
      !is.na(BIRTH),
      !is.na(prov_nac),
      !is.na(survey_year)
    )
  
  if (nrow(d) == 0) {
    warning(paste("No usable observations for outcome:", y))
    return(NULL)
  }
  
  feols(
    fml,
    data = d,
    cluster = ~ prov_nac,
    notes = FALSE
  )
}


# 3) Function to estimate treatment x birth-cohort interaction


make_birth_interaction_formula <- function(y) {
  rhs <- paste(
    c(
      "childhood_total_dry_days_std",
      "childhood_total_dry_days_std:birth_decade_c",
      main_controls
    ),
    collapse = " + "
  )
  
  as.formula(
    paste0(
      y, " ~ ", rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
}

run_birth_interaction <- function(y) {
  fml <- make_birth_interaction_formula(y)
  
  vars_needed <- unique(c(
    y,
    "childhood_total_dry_days_std",
    "birth_decade_c",
    main_controls,
    "BIRTH",
    "prov_nac",
    "survey_year"
  ))
  
  d <- model_data %>%
    dplyr::select(all_of(vars_needed)) %>%
    filter(
      !is.na(.data[[y]]),
      !is.na(childhood_total_dry_days_std),
      !is.na(birth_decade_c),
      !is.na(BIRTH),
      !is.na(prov_nac),
      !is.na(survey_year)
    )
  
  if (nrow(d) == 0) {
    warning(paste("No usable observations for outcome:", y))
    return(NULL)
  }
  
  feols(
    fml,
    data = d,
    cluster = ~ prov_nac,
    notes = FALSE
  )
}


# 4) Outcome labels


outcome_labels <- c(
  CATHOLIC = "Catholic",
  RELIGIOUS_PRACTICE = "Religious practice",
  COUPLE_CATHOLIC = "Couple Catholic",
  
  PARTICIPATION = "Participation",
  CONSERVATIVE_VOTE = "Conservative vote",
  LEFT_RIGHT = "Left-right",
  
  ECOLOGIST_SELF = "Ecologist",
  ENV_CONCERN = "Environmental concern",
  SAME_LOC_BIRTH = "Same province",
  
  EDUCATION = "Education",
  INCOME = "Household income",
  TRUST_PEOPLE = "Trust people"
)

# Clean coefficient labels for LaTeX
coef_labels_birth_interaction <- c(
  "childhood_total_dry_days_std" = "Dry days (std.)",
  "childhood_total_dry_days_std:birth_decade_c" =
    "Dry days (std.) x birth cohort"
)


# 5) Religious outcomes


religion_outcomes <- c(
  "CATHOLIC",
  "RELIGIOUS_PRACTICE",
  "COUPLE_CATHOLIC"
)

religion_outcomes <- religion_outcomes[religion_outcomes %in% names(model_data)]

models_religion_birth_interact <- lapply(
  religion_outcomes,
  run_birth_interaction
)

names(models_religion_birth_interact) <- unname(outcome_labels[religion_outcomes])

models_religion_birth_interact <- models_religion_birth_interact[
  !sapply(models_religion_birth_interact, is.null)
]

modelsummary(
  models_religion_birth_interact,
  title = "Religious outcomes: treatment effect interacted with birth cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_birth_interaction,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj."
)


# 6) Political outcomes


political_outcomes <- c(
  "PARTICIPATION",
  "CONSERVATIVE_VOTE",
  "LEFT_RIGHT"
)

political_outcomes <- political_outcomes[political_outcomes %in% names(model_data)]

models_politics_birth_interact <- lapply(
  political_outcomes,
  run_birth_interaction
)

names(models_politics_birth_interact) <- unname(outcome_labels[political_outcomes])

models_politics_birth_interact <- models_politics_birth_interact[
  !sapply(models_politics_birth_interact, is.null)
]

modelsummary(
  models_politics_birth_interact,
  title = "Political outcomes: treatment effect interacted with birth cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_birth_interaction,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj."
)


# 7) Environmental and migration outcomes


environment_outcomes <- c(
  "ECOLOGIST_SELF",
  "ENV_CONCERN",
  "SAME_LOC_BIRTH"
)

environment_outcomes <- environment_outcomes[environment_outcomes %in% names(model_data)]

models_environment_birth_interact <- lapply(
  environment_outcomes,
  run_birth_interaction
)

names(models_environment_birth_interact) <- unname(outcome_labels[environment_outcomes])

models_environment_birth_interact <- models_environment_birth_interact[
  !sapply(models_environment_birth_interact, is.null)
]

modelsummary(
  models_environment_birth_interact,
  title = "Environmental and migration outcomes: treatment effect interacted with birth cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_birth_interaction,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj."
)


# 8) Other outcomes / falsification outcomes


other_outcomes <- c(
  "EDUCATION",
  "INCOME",
  "TRUST_PEOPLE"
)

other_outcomes <- other_outcomes[other_outcomes %in% names(model_data)]

models_other_birth_interact <- lapply(
  other_outcomes,
  run_birth_interaction
)

names(models_other_birth_interact) <- unname(outcome_labels[other_outcomes])

models_other_birth_interact <- models_other_birth_interact[
  !sapply(models_other_birth_interact, is.null)
]

modelsummary(
  models_other_birth_interact,
  title = "Other outcomes: treatment effect interacted with birth cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_birth_interaction,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj."
)


# 9) Marginal effects of dry days at selected birth years


star_fun <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

marginal_effect_birthyears <- function(model, years, outcome_name = "") {
  
  b <- coef(model)
  V <- vcov(model)
  
  treat_name <- "childhood_total_dry_days_std"
  int_name   <- "childhood_total_dry_days_std:birth_decade_c"
  
  if (!treat_name %in% names(b) | !int_name %in% names(b)) {
    return(NULL)
  }
  
  out <- lapply(years, function(y) {
    
    birth_decade_c <- (y - birth_mean_global) / 10
    
    est <- b[treat_name] + b[int_name] * birth_decade_c
    
    se <- sqrt(
      V[treat_name, treat_name] +
        birth_decade_c^2 * V[int_name, int_name] +
        2 * birth_decade_c * V[treat_name, int_name]
    )
    
    pval <- 2 * pnorm(abs(est / se), lower.tail = FALSE)
    
    tibble(
      Outcome = outcome_name,
      Birth_year = y,
      Estimate = est,
      SE = se,
      p_value = pval,
      Cell = paste0(
        sprintf("%.3f", est),
        star_fun(pval),
        " (",
        sprintf("%.3f", se),
        ")"
      )
    )
  }) %>%
    bind_rows()
  
  out
}

make_marginal_effect_table <- function(models_list, years = c(1940, 1960, 1980, 2000)) {
  
  out <- lapply(names(models_list), function(nm) {
    marginal_effect_birthyears(
      model = models_list[[nm]],
      years = years,
      outcome_name = nm
    )
  }) %>%
    bind_rows()
  
  out %>%
    select(Outcome, Birth_year, Cell) %>%
    pivot_wider(
      names_from = Birth_year,
      values_from = Cell,
      names_prefix = "Born "
    )
}

# Religious marginal effects
me_religion <- make_marginal_effect_table(
  models_religion_birth_interact,
  years = c(1940, 1960, 1980, 2000)
)

kbl(
  me_religion,
  format = "latex",
  booktabs = TRUE,
  caption = "Marginal effect of childhood dry days by birth cohort: religious outcomes.",
  align = "lcccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Political marginal effects
me_politics <- make_marginal_effect_table(
  models_politics_birth_interact,
  years = c(1940, 1960, 1980, 2000)
)

kbl(
  me_politics,
  format = "latex",
  booktabs = TRUE,
  caption = "Marginal effect of childhood dry days by birth cohort: political outcomes.",
  align = "lcccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Environmental and migration marginal effects
me_environment <- make_marginal_effect_table(
  models_environment_birth_interact,
  years = c(1940, 1960, 1980, 2000)
)

kbl(
  me_environment,
  format = "latex",
  booktabs = TRUE,
  caption = "Marginal effect of childhood dry days by birth cohort: environmental and migration outcomes.",
  align = "lcccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Other / falsification marginal effects
me_other <- make_marginal_effect_table(
  models_other_birth_interact,
  years = c(1940, 1960, 1980, 2000)
)

kbl(
  me_other,
  format = "latex",
  booktabs = TRUE,
  caption = "Marginal effect of childhood dry days by birth cohort: other outcomes.",
  align = "lcccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))


# 10) Optional plot: marginal effect by birth year


plot_birth_interaction <- function(model, title = "") {
  
  b <- coef(model)
  V <- vcov(model)
  
  treat_name <- "childhood_total_dry_days_std"
  int_name   <- "childhood_total_dry_days_std:birth_decade_c"
  
  if (!treat_name %in% names(b)) {
    stop("Treatment coefficient not found.")
  }
  
  if (!int_name %in% names(b)) {
    stop("Interaction coefficient not found.")
  }
  
  beta_treat <- b[treat_name]
  beta_int   <- b[int_name]
  
  var_treat <- V[treat_name, treat_name]
  var_int   <- V[int_name, int_name]
  covar     <- V[treat_name, int_name]
  
  plot_df <- tibble(
    BIRTH = seq(
      min(model_data$BIRTH, na.rm = TRUE),
      max(model_data$BIRTH, na.rm = TRUE),
      by = 1
    )
  ) %>%
    mutate(
      birth_decade_c = (BIRTH - birth_mean_global) / 10,
      effect = beta_treat + beta_int * birth_decade_c,
      se = sqrt(
        var_treat +
          birth_decade_c^2 * var_int +
          2 * birth_decade_c * covar
      ),
      ci_low = effect - 1.96 * se,
      ci_high = effect + 1.96 * se
    )
  
  ggplot(plot_df, aes(x = BIRTH, y = effect)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = title,
      x = "Birth year",
      y = "Marginal effect of childhood dry days, std."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# Example plot: Catholic outcome
p_birth_cat <- plot_birth_interaction(
  models_religion_birth_interact[["Catholic"]],
  title = "Effect of childhood dry days on Catholic identification by birth cohort"
)

print(p_birth_cat)

ggsave(
  "birth_interaction_catholic.png",
  p_birth_cat,
  width = 8,
  height = 5,
  dpi = 300
)


# Effect of a fully rained-out Holy Week --------------------------
# Independent section. Does NOT overwrite original model_data.

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(modelsummary)
library(kableExtra)


# 0) Load survey data independently


survey_hw <- readr::read_csv(
  "survey_with_childhood_weather_harmonized.csv",
  show_col_types = FALSE
)

if (!exists("rain_summary")) {
  rain_summary <- readr::read_csv(
    "C:/Users/Saúl/Desktop/Semana Santa project/Grid precipitation/province_holy_week_summary.csv",
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
}


# 1) Choose dry-day variable


dry_var_hw <- "dry_days_10"

if (!dry_var_hw %in% names(rain_summary)) {
  stop(paste0(dry_var_hw, " not found in rain_summary."))
}

if (!"provincia_norm" %in% names(survey_hw)) {
  stop("provincia_norm not found in survey_hw. It must be in the exported CSV.")
}

# Make sure childhood years exist
if (!"childhood_start" %in% names(survey_hw)) {
  survey_hw <- survey_hw %>%
    mutate(childhood_start = BIRTH + 5)
}

if (!"childhood_end" %in% names(survey_hw)) {
  survey_hw <- survey_hw %>%
    mutate(childhood_end = BIRTH + 18)
}


# 2) Province-year indicators


rain_summary_hw <- rain_summary %>%
  mutate(
    # Strict: zero dry days during Holy Week
    full_rain_hw_0dry = case_when(
      is.na(.data[[dry_var_hw]]) ~ NA_integer_,
      .data[[dry_var_hw]] == 0 ~ 1L,
      TRUE ~ 0L
    ),
    
    # Softer: at most 1 dry day during Holy Week
    full_rain_hw_1dry = case_when(
      is.na(.data[[dry_var_hw]]) ~ NA_integer_,
      .data[[dry_var_hw]] <= 1 ~ 1L,
      TRUE ~ 0L
    ),
    
    # Softer: at most 2 dry days during Holy Week
    full_rain_hw_2dry = case_when(
      is.na(.data[[dry_var_hw]]) ~ NA_integer_,
      .data[[dry_var_hw]] <= 2 ~ 1L,
      TRUE ~ 0L
    )
  )

# Quick province-year diagnostics
rain_summary_hw %>%
  summarise(
    n_prov_year = n(),
    share_0dry = mean(full_rain_hw_0dry, na.rm = TRUE),
    share_1dry = mean(full_rain_hw_1dry, na.rm = TRUE),
    share_2dry = mean(full_rain_hw_2dry, na.rm = TRUE)
  ) %>%
  print()


# 3) Helper to compute childhood exposure


get_hw_exposure <- function(prov_norm, start_year, end_year) {
  
  if (is.na(prov_norm) || is.na(start_year) || is.na(end_year)) {
    return(c(
      n_0dry = NA_real_,
      any_0dry = NA_real_,
      share_0dry = NA_real_,
      
      n_1dry = NA_real_,
      any_1dry = NA_real_,
      share_1dry = NA_real_,
      
      n_2dry = NA_real_,
      any_2dry = NA_real_,
      share_2dry = NA_real_
    ))
  }
  
  yrs <- seq(start_year, end_year)
  
  df <- rain_summary_hw %>%
    filter(
      provincia_norm == prov_norm,
      year %in% yrs
    )
  
  # Strict coverage rule
  if (
    nrow(df) < length(yrs) ||
    any(is.na(df$full_rain_hw_0dry)) ||
    any(is.na(df$full_rain_hw_1dry)) ||
    any(is.na(df$full_rain_hw_2dry))
  ) {
    return(c(
      n_0dry = NA_real_,
      any_0dry = NA_real_,
      share_0dry = NA_real_,
      
      n_1dry = NA_real_,
      any_1dry = NA_real_,
      share_1dry = NA_real_,
      
      n_2dry = NA_real_,
      any_2dry = NA_real_,
      share_2dry = NA_real_
    ))
  }
  
  n_0dry <- sum(df$full_rain_hw_0dry == 1, na.rm = TRUE)
  n_1dry <- sum(df$full_rain_hw_1dry == 1, na.rm = TRUE)
  n_2dry <- sum(df$full_rain_hw_2dry == 1, na.rm = TRUE)
  
  c(
    n_0dry = n_0dry,
    any_0dry = as.numeric(n_0dry > 0),
    share_0dry = n_0dry / length(yrs),
    
    n_1dry = n_1dry,
    any_1dry = as.numeric(n_1dry > 0),
    share_1dry = n_1dry / length(yrs),
    
    n_2dry = n_2dry,
    any_2dry = as.numeric(n_2dry > 0),
    share_2dry = n_2dry / length(yrs)
  )
}


# 4) Attach childhood exposure to survey_hw


hw_exposure <- survey_hw %>%
  dplyr::select(
    respondent_id,
    provincia_norm,
    childhood_start,
    childhood_end
  ) %>%
  rowwise() %>%
  mutate(
    hw_vals = list(
      get_hw_exposure(
        provincia_norm,
        childhood_start,
        childhood_end
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    hw_n_0dry     = map_dbl(hw_vals, 1),
    hw_any_0dry   = map_dbl(hw_vals, 2),
    hw_share_0dry = map_dbl(hw_vals, 3),
    
    hw_n_1dry     = map_dbl(hw_vals, 4),
    hw_any_1dry   = map_dbl(hw_vals, 5),
    hw_share_1dry = map_dbl(hw_vals, 6),
    
    hw_n_2dry     = map_dbl(hw_vals, 7),
    hw_any_2dry   = map_dbl(hw_vals, 8),
    hw_share_2dry = map_dbl(hw_vals, 9)
  ) %>%
  dplyr::select(
    respondent_id,
    hw_n_0dry, hw_any_0dry, hw_share_0dry,
    hw_n_1dry, hw_any_1dry, hw_share_1dry,
    hw_n_2dry, hw_any_2dry, hw_share_2dry
  )

survey_hw <- survey_hw %>%
  dplyr::select(
    -any_of(c(
      "hw_n_0dry", "hw_any_0dry", "hw_share_0dry",
      "hw_n_1dry", "hw_any_1dry", "hw_share_1dry",
      "hw_n_2dry", "hw_any_2dry", "hw_share_2dry"
    ))
  ) %>%
  left_join(hw_exposure, by = "respondent_id")

# Respondent-level diagnostics
survey_hw %>%
  summarise(
    n_total = n(),
    
    n_nonmissing_0dry = sum(!is.na(hw_any_0dry)),
    share_any_0dry = mean(hw_any_0dry, na.rm = TRUE),
    mean_n_0dry = mean(hw_n_0dry, na.rm = TRUE),
    
    n_nonmissing_1dry = sum(!is.na(hw_any_1dry)),
    share_any_1dry = mean(hw_any_1dry, na.rm = TRUE),
    mean_n_1dry = mean(hw_n_1dry, na.rm = TRUE),
    
    n_nonmissing_2dry = sum(!is.na(hw_any_2dry)),
    share_any_2dry = mean(hw_any_2dry, na.rm = TRUE),
    mean_n_2dry = mean(hw_n_2dry, na.rm = TRUE)
  ) %>%
  print()


# 5) Build independent model dataset


# Helper to standardize only if there is real variation
std_if_var <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  
  if (!is.finite(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  (x - m) / s
}

model_data_hw <- survey_hw %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(BIRTH),
    !is.na(prov_nac)
  ) %>%
  mutate(
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    
    log_pop_birth = if_else(
      !is.na(pop_birth_last_census) & pop_birth_last_census > 0,
      log(pop_birth_last_census),
      NA_real_
    ),
    
    birth_mean_hw = mean(BIRTH, na.rm = TRUE),
    birth_decade_c = (BIRTH - birth_mean_hw) / 10,
    
    hw_n_0dry_std = std_if_var(hw_n_0dry),
    hw_n_1dry_std = std_if_var(hw_n_1dry),
    hw_n_2dry_std = std_if_var(hw_n_2dry)
  ) %>%
  dplyr::select(
    CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC,
    PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT,
    ECOLOGIST_SELF, ENV_CONCERN, SAME_LOC_BIRTH,
    EDUCATION, INCOME, TRUST_PEOPLE,
    
    hw_any_0dry, hw_n_0dry, hw_n_0dry_std, hw_share_0dry,
    hw_any_1dry, hw_n_1dry, hw_n_1dry_std, hw_share_1dry,
    hw_any_2dry, hw_n_2dry, hw_n_2dry_std, hw_share_2dry,
    
    survey_year, survey_wave,
    FEMALE, age, BIRTH, prov_nac, birth_ccaa, res_ccaa,
    FATHER_BORN_SPAIN, MOTHER_BORN_SPAIN,
    FATHER_SCHOOL, MOTHER_SCHOOL,
    FATHER_EMPLOYMENT, MOTHER_EMPLOYMENT,
    pop_birth_last_census, log_pop_birth,
    birth_decade_c, birth_prov_cluster
  ) %>%
  apply_mim_controls(mim_vars)

main_controls_hw <- get_mim_controls(
  model_data_hw,
  mim_vars,
  extra_controls = "log_pop_birth"
)

# Estimation-sample treatment diagnostics
model_data_hw %>%
  summarise(
    n = n(),
    
    mean_any_0dry = mean(hw_any_0dry, na.rm = TRUE),
    sd_any_0dry = sd(hw_any_0dry, na.rm = TRUE),
    
    mean_any_1dry = mean(hw_any_1dry, na.rm = TRUE),
    sd_any_1dry = sd(hw_any_1dry, na.rm = TRUE),
    
    mean_any_2dry = mean(hw_any_2dry, na.rm = TRUE),
    sd_any_2dry = sd(hw_any_2dry, na.rm = TRUE),
    
    mean_n_0dry = mean(hw_n_0dry, na.rm = TRUE),
    sd_n_0dry = sd(hw_n_0dry, na.rm = TRUE),
    
    mean_n_1dry = mean(hw_n_1dry, na.rm = TRUE),
    sd_n_1dry = sd(hw_n_1dry, na.rm = TRUE),
    
    mean_n_2dry = mean(hw_n_2dry, na.rm = TRUE),
    sd_n_2dry = sd(hw_n_2dry, na.rm = TRUE)
  ) %>%
  print()


# 6) Controls and labels



main_controls_hw <- main_controls_hw[main_controls_hw %in% names(model_data_hw)]

main_controls_hw <- main_controls_hw[
  sapply(model_data_hw[main_controls_hw], function(x) !all(is.na(x)))
]

outcome_labels_hw <- c(
  CATHOLIC = "Catholic",
  RELIGIOUS_PRACTICE = "Religious practice",
  COUPLE_CATHOLIC = "Couple Catholic",
  
  PARTICIPATION = "Participation",
  CONSERVATIVE_VOTE = "Conservative vote",
  LEFT_RIGHT = "Left-right",
  
  ECOLOGIST_SELF = "Ecologist",
  ENV_CONCERN = "Environmental concern",
  SAME_LOC_BIRTH = "Same province",
  
  EDUCATION = "Education",
  INCOME = "Household income",
  TRUST_PEOPLE = "Trust people"
)

religion_outcomes_hw <- c(
  "CATHOLIC",
  "RELIGIOUS_PRACTICE",
  "COUPLE_CATHOLIC"
)

political_outcomes_hw <- c(
  "PARTICIPATION",
  "CONSERVATIVE_VOTE",
  "LEFT_RIGHT"
)

environment_outcomes_hw <- c(
  "ECOLOGIST_SELF",
  "ENV_CONCERN",
  "SAME_LOC_BIRTH"
)

other_outcomes_hw <- c(
  "EDUCATION",
  "INCOME",
  "TRUST_PEOPLE"
)


# 7) Helper functions


make_hw_formula <- function(y, treatment_var, interact_birth = FALSE) {
  
  rhs_terms <- c(treatment_var)
  
  if (interact_birth) {
    rhs_terms <- c(
      rhs_terms,
      paste0(treatment_var, ":birth_decade_c")
    )
  }
  
  rhs_terms <- c(rhs_terms, main_controls_hw)
  
  as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
}

run_hw_model <- function(y, treatment_var, interact_birth = FALSE) {
  
  fml <- make_hw_formula(
    y = y,
    treatment_var = treatment_var,
    interact_birth = interact_birth
  )
  
  vars_needed <- unique(c(
    y,
    treatment_var,
    if (interact_birth) "birth_decade_c",
    main_controls_hw,
    "BIRTH",
    "prov_nac",
    "survey_year"
  ))
  
  d <- model_data_hw %>%
    dplyr::select(all_of(vars_needed)) %>%
    filter(
      !is.na(.data[[y]]),
      !is.na(.data[[treatment_var]]),
      !is.na(BIRTH),
      !is.na(prov_nac),
      !is.na(survey_year)
    )
  
  if (interact_birth) {
    d <- d %>% filter(!is.na(birth_decade_c))
  }
  
  if (nrow(d) == 0) {
    warning(paste("No usable observations for:", y, "using", treatment_var))
    return(NULL)
  }
  
  if (dplyr::n_distinct(d[[treatment_var]]) < 2) {
    warning(paste("No variation in treatment for:", y, "using", treatment_var))
    return(NULL)
  }
  
  feols(
    fml,
    data = d,
    cluster = ~ prov_nac,
    notes = FALSE
  )
}

make_hw_model_list <- function(outcomes, treatment_var, interact_birth = FALSE) {
  
  outcomes <- outcomes[outcomes %in% names(model_data_hw)]
  
  out <- lapply(
    outcomes,
    run_hw_model,
    treatment_var = treatment_var,
    interact_birth = interact_birth
  )
  
  names(out) <- unname(outcome_labels_hw[outcomes])
  
  out[!sapply(out, is.null)]
}

safe_modelsummary_hw <- function(models, title, coef_rename) {
  
  if (length(models) == 0) {
    message("No models estimated for table: ", title)
    return(invisible(NULL))
  }
  
  modelsummary(
    models,
    title = title,
    output = "latex",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
    coef_rename = coef_rename,
    gof_omit = "AIC|BIC|R2 Within|R2 Within Adj."
  )
}


# 8) Choose treatment with actual variation


# Main strict definition: hw_any_0dry
# If that has no variation, use hw_any_1dry or hw_any_2dry instead.

candidate_treatments_hw <- c(
  "hw_any_0dry",
  "hw_any_1dry",
  "hw_any_2dry"
)

treatment_variation_hw <- tibble(
  treatment = candidate_treatments_hw,
  n_nonmissing = sapply(candidate_treatments_hw, function(v) sum(!is.na(model_data_hw[[v]]))),
  mean = sapply(candidate_treatments_hw, function(v) mean(model_data_hw[[v]], na.rm = TRUE)),
  sd = sapply(candidate_treatments_hw, function(v) sd(model_data_hw[[v]], na.rm = TRUE)),
  n_distinct = sapply(candidate_treatments_hw, function(v) n_distinct(model_data_hw[[v]], na.rm = TRUE))
)

print(treatment_variation_hw)

available_treatments_hw <- treatment_variation_hw %>%
  filter(n_distinct >= 2, is.finite(sd), sd > 0) %>%
  pull(treatment)

if (length(available_treatments_hw) == 0) {
  stop(
    "None of the Holy Week rain dummy treatments has variation. ",
    "Try a less strict definition, for example dry_days_10 <= 3, or use the count/share variables."
  )
}

# Use the strictest available dummy
main_hw_treatment <- available_treatments_hw[1]

message("Using treatment: ", main_hw_treatment)

coef_labels_by_treatment <- list(
  hw_any_0dry = c(
    "hw_any_0dry" = "Any Holy Week with zero dry days",
    "hw_any_0dry:birth_decade_c" = "Zero-dry-day Holy Week x birth cohort"
  ),
  hw_any_1dry = c(
    "hw_any_1dry" = "Any Holy Week with <= 1 dry day",
    "hw_any_1dry:birth_decade_c" = "<= 1-dry-day Holy Week x birth cohort"
  ),
  hw_any_2dry = c(
    "hw_any_2dry" = "Any Holy Week with <= 2 dry days",
    "hw_any_2dry:birth_decade_c" = "<= 2-dry-day Holy Week x birth cohort"
  )
)

coef_labels_main_hw <- coef_labels_by_treatment[[main_hw_treatment]]

# Two tables: Holy Week rain dummy, dry days, and interaction 


# 0) Check that Holy Week exposure variables exist


if (!exists("survey_hw")) {
  stop("survey_hw does not exist. Run the Holy Week exposure section first.")
}

candidate_hw_treatments <- c(
  "hw_any_0dry",
  "hw_any_1dry",
  "hw_any_2dry"
)

candidate_hw_treatments <- candidate_hw_treatments[
  candidate_hw_treatments %in% names(survey_hw)
]

if (length(candidate_hw_treatments) == 0) {
  stop("No Holy Week rain dummy found. Run the exposure creation section first.")
}

# Choose strictest Holy Week rain dummy with actual variation
hw_variation_table <- tibble(
  treatment = candidate_hw_treatments,
  n_nonmissing = sapply(candidate_hw_treatments, function(v) {
    sum(!is.na(survey_hw[[v]]))
  }),
  mean = sapply(candidate_hw_treatments, function(v) {
    mean(survey_hw[[v]], na.rm = TRUE)
  }),
  sd = sapply(candidate_hw_treatments, function(v) {
    sd(survey_hw[[v]], na.rm = TRUE)
  }),
  n_distinct = sapply(candidate_hw_treatments, function(v) {
    n_distinct(survey_hw[[v]], na.rm = TRUE)
  })
)

print(hw_variation_table)

available_hw_treatments <- hw_variation_table %>%
  filter(
    n_distinct >= 2,
    is.finite(sd),
    sd > 0
  ) %>%
  pull(treatment)

if (length(available_hw_treatments) == 0) {
  stop("None of the Holy Week rain dummy variables has variation.")
}

# This picks the strictest available definition:
# hw_any_0dry first, then hw_any_1dry, then hw_any_2dry.
main_hw_treatment <- available_hw_treatments[1]

message("Using Holy Week rain treatment: ", main_hw_treatment)

# Labels depending on selected definition
hw_label <- dplyr::case_when(
  main_hw_treatment == "hw_any_0dry" ~ "Any Holy Week with zero dry days",
  main_hw_treatment == "hw_any_1dry" ~ "Any Holy Week with <= 1 dry day",
  main_hw_treatment == "hw_any_2dry" ~ "Any Holy Week with <= 2 dry days",
  TRUE ~ main_hw_treatment
)

hw_inter_label <- paste0(hw_label, " x dry days")


# 1) Build separate estimation dataset


model_data_hw_tables <- survey_hw %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(BIRTH),
    !is.na(prov_nac),
    !is.na(childhood_total_dry_days)
  ) %>%
  mutate(
    log_pop_birth = if_else(
      !is.na(pop_birth_last_census) & pop_birth_last_census > 0,
      log(pop_birth_last_census),
      NA_real_
    ),
    
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    
    hw_treatment = .data[[main_hw_treatment]]
  ) %>%
  apply_mim_controls(mim_vars)

# Diagnostics for the selected treatment
model_data_hw_tables %>%
  summarise(
    n = n(),
    mean_hw_treatment = mean(hw_treatment, na.rm = TRUE),
    sd_hw_treatment = sd(hw_treatment, na.rm = TRUE),
    mean_dry_days_std = mean(childhood_total_dry_days_std, na.rm = TRUE),
    sd_dry_days_std = sd(childhood_total_dry_days_std, na.rm = TRUE)
  ) %>%
  print()


# 2) Controls

main_controls_hw_tables <- get_mim_controls(
  model_data_hw_tables,
  mim_vars,
  extra_controls = "log_pop_birth"
)

controls_rhs_hw <- paste(main_controls_hw_tables, collapse = " + ")

main_controls_hw_tables <- main_controls_hw_tables[
  main_controls_hw_tables %in% names(model_data_hw_tables)
]

main_controls_hw_tables <- main_controls_hw_tables[
  sapply(model_data_hw_tables[main_controls_hw_tables], function(x) !all(is.na(x)))
]

controls_rhs_hw <- paste(main_controls_hw_tables, collapse = " + ")


# 3) Formula builder


make_three_spec_formulas <- function(y) {
  
  fml_1 <- as.formula(
    paste0(
      y,
      " ~ hw_treatment + ",
      controls_rhs_hw,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  fml_2 <- as.formula(
    paste0(
      y,
      " ~ hw_treatment + childhood_total_dry_days_std + ",
      controls_rhs_hw,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  fml_3 <- as.formula(
    paste0(
      y,
      " ~ hw_treatment * childhood_total_dry_days_std + ",
      controls_rhs_hw,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  list(
    "Rained-out HW only" = fml_1,
    "Add dry days" = fml_2,
    "Interaction" = fml_3
  )
}

run_three_specs <- function(y) {
  
  formulas <- make_three_spec_formulas(y)
  
  models <- lapply(formulas, function(fml) {
    feols(
      fml,
      data = model_data_hw_tables,
      cluster = ~ prov_nac,
      notes = FALSE
    )
  })
  
  models
}


# 4) Estimate models


models_hw_catholic <- run_three_specs("CATHOLIC")

models_hw_conservative <- run_three_specs("CONSERVATIVE_VOTE")


# 5) Clean coefficient labels


coef_labels_hw_tables <- c(
  "hw_treatment" = hw_label,
  "childhood_total_dry_days_std" = "Dry days (std.)",
  "hw_treatment:childhood_total_dry_days_std" = hw_inter_label,
  "childhood_total_dry_days_std:hw_treatment" = hw_inter_label
)


# 6) Add rows with treatment definition and FE information


add_rows_catholic_hw <- data.frame(
  term = c(
    "Outcome",
    "Treatment definition",
    "Birth-year FE",
    "Province FE",
    "Survey-year FE",
    "Missing-control indicators"
  ),
  check.names = FALSE,
  "Rained-out HW only" = c(
    "Catholic identification",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  ),
  "Add dry days" = c(
    "Catholic identification",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  ),
  "Interaction" = c(
    "Catholic identification",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  )
)

add_rows_conservative_hw <- data.frame(
  term = c(
    "Outcome",
    "Treatment definition",
    "Birth-year FE",
    "Province FE",
    "Survey-year FE",
    "Missing-control indicators"
  ),
  check.names = FALSE,
  "Rained-out HW only" = c(
    "Conservative vote",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  ),
  "Add dry days" = c(
    "Conservative vote",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  ),
  "Interaction" = c(
    "Conservative vote",
    hw_label,
    "Yes",
    "Yes",
    "Yes",
    "Yes"
  )
)


# 7) Table 1: Catholic identification


modelsummary(
  models_hw_catholic,
  title = "Catholic identification: rained-out Holy Week, dry days, and interaction",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_hw_tables,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_catholic_hw
)


# 8) Table 2: Conservative vote


modelsummary(
  models_hw_conservative,
  title = "Conservative vote: rained-out Holy Week, dry days, and interaction",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = coef_labels_hw_tables,
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_conservative_hw
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
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_rel <- feols(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_cou <- feols(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~prov_nac
)

# --- Political outcomes ---

lpm_fe_spline_par <- feols(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_con <- feols(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~prov_nac
)

lpm_fe_spline_lr <- feols(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
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
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) + 
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_rel <- lm(
  RELIGIOUS_PRACTICE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_cou <- lm(
  COUPLE_CATHOLIC ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_par <- lm(
  PARTICIPATION ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_con <- lm(
  CONSERVATIVE_VOTE ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) +
    factor(BIRTH) + factor(prov_nac) + log_pop_birth,
  data = model_data
)

lm_spline_lr <- lm(
  LEFT_RIGHT ~ ns(childhood_total_dry_days_std, knots = knots_treat) +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + factor(survey_year) +
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
library(nprobust)
library(ggplot2)
library(scales)


# 0) USER INPUTS

y_var <- "CONSERVATIVE_VOTE"
d_var <- "childhood_total_dry_days_std"

controls_rhs <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL",
  "log_pop_birth"
  
)

fe_rhs <- c("BIRTH", "prov_nac", "survey_year")

n_bins <- 40


# 1) Build ONE consistent estimation sample

vars_needed <- unique(c(y_var, d_var, controls_rhs, fe_rhs, "prov_nac"))

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
df$y_res <- NA_real_
df$y_res[obs(m_y)] <- resid(m_y)


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
df$d_res <- NA_real_
df$d_res[obs(m_d)] <- resid(m_d)

# Drop rows that any FE singleton removed
df <- df %>% filter(!is.na(y_res), !is.na(d_res))


# 4) Local quadratic regression with cluster-robust CIs
#    Two-step: (a) pick bandwidth unclustered, (b) re-fit at fixed h, b with cluster
#    This sidesteps an internal bug in nprobust's clustered bandwidth selector.

grid_x <- seq(
  quantile(df$d_res, 0.01, na.rm = TRUE),
  quantile(df$d_res, 0.99, na.rm = TRUE),
  length.out = 300
)

# (a) MSE-optimal bandwidth
bw_sel <- lpbwselect(
  y        = df$y_res,
  x        = df$d_res,
  eval     = grid_x,
  p        = 2,
  deriv    = 0,
  kernel   = "epa",
  bwselect = "mse-dpi"
)

h_used <- median(bw_sel$bws[, "h"], na.rm = TRUE)
b_used <- median(bw_sel$bws[, "b"], na.rm = TRUE)

# (b) Fit at fixed h, b with province clustering
lp_fit <- lprobust(
  y       = df$y_res,
  x       = df$d_res,
  eval    = grid_x,
  p       = 2,        # set to 1 to reproduce local-linear (your old "ll")
  deriv   = 0,
  kernel  = "epa",
  h       = h_used,
  b       = b_used,
  cluster = as.integer(df$prov_nac)
)

est <- as.data.frame(lp_fit$Estimate)
grid <- tibble(
  d_res = est$eval,
  yhat  = est$tau.us,                          # conventional point estimate
  lwr   = est$tau.bc - 1.96 * est$se.rb,       # robust bias-corrected band
  upr   = est$tau.bc + 1.96 * est$se.rb
)


# 5) Binned means (transparency device)

df_bins <- df %>%
  mutate(bin = ntile(d_res, n_bins)) %>%
  group_by(bin) %>%
  summarise(
    d_bin = mean(d_res),
    y_bin = mean(y_res),
    .groups = "drop"
  )


# 6) Plot

p_lpr <- ggplot() +
  geom_ribbon(
    data = grid,
    aes(d_res, ymin = lwr, ymax = upr),
    fill = "grey70", alpha = 0.4
  ) +
  geom_line(
    data = grid,
    aes(d_res, yhat),
    linewidth = 1.4, color = "black"
  ) +
  geom_point(
    data = df_bins,
    aes(d_bin, y_bin),
    size = 1.8, color = "black", alpha = 0.85
  ) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Residualized childhood dry days (standardized)",
    y = "Conservative vote",
    title = "Local quadratic regression (residualized; province-clustered band)"
  ) +
  theme_minimal(base_size = 13)

print(p_lpr)


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

# MIM controls for binned-treatment regressions ----------------------------

# Assumes you already created:
# - mim_vars
# - apply_mim_controls()
# - *_MISS variables

model_data <- apply_mim_controls(model_data, mim_vars)

# Preserve your existing control choices:
# full controls = what most of your controlled bin models use
controls_full <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "log_pop_birth"
)



make_mim_rhs <- function(vars, data = model_data) {
  vars <- vars[vars %in% names(data)]
  miss_vars <- paste0(vars, "_MISS")
  miss_vars <- miss_vars[miss_vars %in% names(data)]
  paste(c(vars, miss_vars), collapse = " + ")
}

make_bin_fml <- function(y, bin_var, controls = NULL) {
  rhs <- paste0("i(", bin_var, ", ref = 1)")
  
  if (!is.null(controls) && length(controls) > 0) {
    ctrl_rhs <- make_mim_rhs(controls)
    if (nzchar(ctrl_rhs)) {
      rhs <- paste(rhs, ctrl_rhs, sep = " + ")
    }
  }
  
  as.formula(
    paste0(y, " ~ ", rhs, " | BIRTH + prov_nac + survey_year")
  )
}

fit_bin <- function(y, bin_var, controls = NULL) {
  feols(
    make_bin_fml(y, bin_var, controls),
    data = model_data,
    cluster = ~ prov_nac
  )
}



# 3) Quartiles: Religious outcomes


lpm_fe_q_nocontrols_cat <- feols(
  CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



lpm_fe_q_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)


lpm_fe_q_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)




# Means of dependent variables on estimation samples: quartile models (religious)

fml_q_cat_noc <- CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year 

fml_q_rel_noc <- RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1)  | BIRTH + prov_nac + survey_year 

fml_q_cou_noc <- COUPLE_CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year 

mean_q_cat_noc <- dv_mean_from_data(model_data, fml_q_cat_noc)

mean_q_rel_noc <- dv_mean_from_data(model_data, fml_q_rel_noc)

mean_q_cou_noc <- dv_mean_from_data(model_data, fml_q_cou_noc)


# Wald p-values: quartile models (religious)
pval_q_cat_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cat, keep = "treat_q::")[["p"]])


pval_q_rel_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_rel, keep = "treat_q::")[["p"]])


pval_q_cou_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cou, keep = "treat_q::")[["p"]])

wald_cat_ctrl <- wald(lpm_fe_q_controls_cat, keep = "treat_q::")
wald_rel_ctrl <- wald(lpm_fe_q_controls_rel, keep = "treat_q::")
wald_cou_ctrl <- wald(lpm_fe_q_controls_cou, keep = "treat_q::")

# Means of dependent variables on estimation samples: quartile models

fml_q_cat_noc <- CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_rel_noc <- RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_cou_noc <- COUPLE_CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year

mean_q_cat_noc <- dv_mean_from_data(model_data, fml_q_cat_noc)
mean_q_rel_noc <- dv_mean_from_data(model_data, fml_q_rel_noc)
mean_q_cou_noc <- dv_mean_from_data(model_data, fml_q_cou_noc)


# Wald p-values: no-control quartile models

pval_q_cat_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cat, keep = "treat_q::")[["p"]])
pval_q_rel_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_rel, keep = "treat_q::")[["p"]])
pval_q_cou_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cou, keep = "treat_q::")[["p"]])


# Quartiles: Religious outcomes with MIM controls --------------------------

lpm_fe_q_controls_cat <- fit_bin(
  "CATHOLIC",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_rel <- fit_bin(
  "RELIGIOUS_PRACTICE",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_cou <- fit_bin(
  "COUPLE_CATHOLIC",
  "treat_q",
  controls_full
)

# Matching formulas for mean dependent variable
# IMPORTANT: use the same control sets as the actual models above

fml_q_cat_con <- make_bin_fml("CATHOLIC", "treat_q", controls_full)
fml_q_rel_con <- make_bin_fml("RELIGIOUS_PRACTICE", "treat_q", controls_full)
fml_q_cou_con <- make_bin_fml("COUPLE_CATHOLIC", "treat_q", controls_full)

mean_q_cat_con <- dv_mean_from_data(model_data, fml_q_cat_con)
mean_q_rel_con <- dv_mean_from_data(model_data, fml_q_rel_con)
mean_q_cou_con <- dv_mean_from_data(model_data, fml_q_cou_con)


# Wald p-values: controlled quartile models

wald_cat_ctrl <- wald(lpm_fe_q_controls_cat, keep = "treat_q::")
wald_rel_ctrl <- wald(lpm_fe_q_controls_rel, keep = "treat_q::")
wald_cou_ctrl <- wald(lpm_fe_q_controls_cou, keep = "treat_q::")

pval_q_cat_con <- sprintf("%.3f", wald_cat_ctrl[["p"]])
pval_q_rel_con <- sprintf("%.3f", wald_rel_ctrl[["p"]])
pval_q_cou_con <- sprintf("%.3f", wald_cou_ctrl[["p"]])


# No-control formulas for mean dependent variables

fml_q_cat_noc <- CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_rel_noc <- RELIGIOUS_PRACTICE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_cou_noc <- COUPLE_CATHOLIC ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year

mean_q_cat_noc <- dv_mean_from_data(model_data, fml_q_cat_noc)
mean_q_rel_noc <- dv_mean_from_data(model_data, fml_q_rel_noc)
mean_q_cou_noc <- dv_mean_from_data(model_data, fml_q_cou_noc)

pval_q_cat_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cat, keep = "treat_q::")[["p"]])
pval_q_rel_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_rel, keep = "treat_q::")[["p"]])
pval_q_cou_noc <- sprintf("%.3f", wald(lpm_fe_q_nocontrols_cou, keep = "treat_q::")[["p"]])

# Add rows

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

# 4) Quartiles: Political outcomes ----------------------------------------


# No-control quartile models

lpm_fe_q_nocontrols_par <- feols(
  PARTICIPATION ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)


# No-control formulas for mean dependent variables

fml_q_par_noc <- PARTICIPATION ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_con_noc <- CONSERVATIVE_VOTE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_lr_noc  <- LEFT_RIGHT ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year

mean_q_par_noc <- dv_mean_from_data(model_data, fml_q_par_noc)
mean_q_con_noc <- dv_mean_from_data(model_data, fml_q_con_noc)
mean_q_lr_noc  <- dv_mean_from_data(model_data, fml_q_lr_noc)


# Wald p-values: no-control quartile models

pval_q_par_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_par, keep = "treat_q::")[["p"]]
)

pval_q_con_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_con, keep = "treat_q::")[["p"]]
)

pval_q_lr_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_lr, keep = "treat_q::")[["p"]]
)


# Controlled quartile models with MIM controls ----------------------------

lpm_fe_q_controls_par <- fit_bin(
  "PARTICIPATION",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_con <- fit_bin(
  "CONSERVATIVE_VOTE",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_lr <- fit_bin(
  "LEFT_RIGHT",
  "treat_q",
  controls_full
)


# Controlled formulas for mean dependent variables
# IMPORTANT: controls_full is used everywhere.

fml_q_par_con <- make_bin_fml("PARTICIPATION", "treat_q", controls_full)
fml_q_con_con <- make_bin_fml("CONSERVATIVE_VOTE", "treat_q", controls_full)
fml_q_lr_con  <- make_bin_fml("LEFT_RIGHT", "treat_q", controls_full)

mean_q_par_con <- dv_mean_from_data(model_data, fml_q_par_con)
mean_q_con_con <- dv_mean_from_data(model_data, fml_q_con_con)
mean_q_lr_con  <- dv_mean_from_data(model_data, fml_q_lr_con)


# Wald p-values: controlled quartile models

wald_par_ctrl <- wald(lpm_fe_q_controls_par, keep = "treat_q::")
wald_con_ctrl <- wald(lpm_fe_q_controls_con, keep = "treat_q::")
wald_lr_ctrl  <- wald(lpm_fe_q_controls_lr,  keep = "treat_q::")

pval_q_par_con <- sprintf("%.3f", wald_par_ctrl[["p"]])
pval_q_con_con <- sprintf("%.3f", wald_con_ctrl[["p"]])
pval_q_lr_con  <- sprintf("%.3f", wald_lr_ctrl[["p"]])


# Add rows for modelsummary

add_rows_q_politics <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Participation (Q bins)" = c(
    sprintf("%.3f", mean_q_par_noc),
    "No",
    pval_q_par_noc
  ),
  "Participation (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_par_con),
    "Yes",
    pval_q_par_con
  ),
  "Conservative (Q bins)" = c(
    sprintf("%.3f", mean_q_con_noc),
    "No",
    pval_q_con_noc
  ),
  "Conservative (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_con_con),
    "Yes",
    pval_q_con_con
  ),
  "Left-right (Q bins)" = c(
    sprintf("%.3f", mean_q_lr_noc),
    "No",
    pval_q_lr_noc
  ),
  "Left-right (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_lr_con),
    "Yes",
    pval_q_lr_con
  )
)


# Table

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


# 5) Quartiles: Other outcomes --------------------------------------------


# No-control quartile models

lpm_fe_q_nocontrols_inc <- feols(
  INCOME ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_q_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)


# No-control formulas for mean dependent variables

fml_q_inc_noc <- INCOME ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_edu_noc <- EDUCATION ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year
fml_q_tru_noc <- TRUST_PEOPLE ~ i(treat_q, ref = 1) | BIRTH + prov_nac + survey_year

mean_q_inc_noc <- dv_mean_from_data(model_data, fml_q_inc_noc)
mean_q_edu_noc <- dv_mean_from_data(model_data, fml_q_edu_noc)
mean_q_tru_noc <- dv_mean_from_data(model_data, fml_q_tru_noc)


# Wald p-values: no-control quartile models

pval_q_inc_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_inc, keep = "treat_q::")[["p"]]
)

pval_q_edu_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_edu, keep = "treat_q::")[["p"]]
)

pval_q_tru_noc <- sprintf(
  "%.3f",
  wald(lpm_fe_q_nocontrols_trust, keep = "treat_q::")[["p"]]
)


# Controlled quartile models with MIM controls ----------------------------

lpm_fe_q_controls_inc <- fit_bin(
  "INCOME",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_edu <- fit_bin(
  "EDUCATION",
  "treat_q",
  controls_full
)

lpm_fe_q_controls_trust <- fit_bin(
  "TRUST_PEOPLE",
  "treat_q",
  controls_full
)


# Controlled formulas for mean dependent variables
# IMPORTANT: controls_full is used everywhere.

fml_q_inc_con <- make_bin_fml("INCOME", "treat_q", controls_full)
fml_q_edu_con <- make_bin_fml("EDUCATION", "treat_q", controls_full)
fml_q_tru_con <- make_bin_fml("TRUST_PEOPLE", "treat_q", controls_full)

mean_q_inc_con <- dv_mean_from_data(model_data, fml_q_inc_con)
mean_q_edu_con <- dv_mean_from_data(model_data, fml_q_edu_con)
mean_q_tru_con <- dv_mean_from_data(model_data, fml_q_tru_con)


# Wald p-values: controlled quartile models

wald_inc_q_ctrl   <- wald(lpm_fe_q_controls_inc,   keep = "treat_q::")
wald_edu_q_ctrl   <- wald(lpm_fe_q_controls_edu,   keep = "treat_q::")
wald_trust_q_ctrl <- wald(lpm_fe_q_controls_trust, keep = "treat_q::")

pval_q_inc_con <- sprintf("%.3f", wald_inc_q_ctrl[["p"]])
pval_q_edu_con <- sprintf("%.3f", wald_edu_q_ctrl[["p"]])
pval_q_tru_con <- sprintf("%.3f", wald_trust_q_ctrl[["p"]])


# Add rows for modelsummary

add_rows_q_other <- data.frame(
  term = c("Mean dep. var.", "Controls", "Wald test p-value"),
  check.names = FALSE,
  "Income (Q bins)" = c(
    sprintf("%.3f", mean_q_inc_noc),
    "No",
    pval_q_inc_noc
  ),
  "Income (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_inc_con),
    "Yes",
    pval_q_inc_con
  ),
  "Education (Q bins)" = c(
    sprintf("%.3f", mean_q_edu_noc),
    "No",
    pval_q_edu_noc
  ),
  "Education (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_edu_con),
    "Yes",
    pval_q_edu_con
  ),
  "Trust people (Q bins)" = c(
    sprintf("%.3f", mean_q_tru_noc),
    "No",
    pval_q_tru_noc
  ),
  "Trust people (Q bins) + Controls" = c(
    sprintf("%.3f", mean_q_tru_con),
    "Yes",
    pval_q_tru_con
  )
)


# Table

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
  CATHOLIC ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



# Tertiles: Religious outcomes with MIM controls ---------------------------

lpm_fe_t_controls_cat <- fit_bin(
  "CATHOLIC",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_rel <- fit_bin(
  "RELIGIOUS_PRACTICE",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_cou <- fit_bin(
  "COUPLE_CATHOLIC",
  "treat_t",
  controls_full
)

wald_cat_t_ctrl <- wald(lpm_fe_t_controls_cat, keep = "treat_t::")
wald_rel_t_ctrl <- wald(lpm_fe_t_controls_rel, keep = "treat_t::")
wald_cou_t_ctrl <- wald(lpm_fe_t_controls_cou, keep = "treat_t::")


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
  PARTICIPATION ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)


lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)


# Tertiles: Political outcomes with MIM controls ---------------------------

lpm_fe_t_controls_par <- fit_bin(
  "PARTICIPATION",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_con <- fit_bin(
  "CONSERVATIVE_VOTE",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_lr <- fit_bin(
  "LEFT_RIGHT",
  "treat_t",
  controls_full
)

wald_par_t_ctrl <- wald(lpm_fe_t_controls_par, keep = "treat_t::")
wald_con_t_ctrl <- wald(lpm_fe_t_controls_con, keep = "treat_t::")
wald_lr_t_ctrl  <- wald(lpm_fe_t_controls_lr,  keep = "treat_t::")

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
  INCOME ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)



lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~ prov_nac
)

# Tertiles: Other outcomes with MIM controls -------------------------------

lpm_fe_t_controls_inc <- fit_bin(
  "INCOME",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_edu <- fit_bin(
  "EDUCATION",
  "treat_t",
  controls_full
)

lpm_fe_t_controls_trust <- fit_bin(
  "TRUST_PEOPLE",
  "treat_t",
  controls_full
)

wald_inc_t_ctrl   <- wald(lpm_fe_t_controls_inc,   keep = "treat_t::")
wald_edu_t_ctrl   <- wald(lpm_fe_t_controls_edu,   keep = "treat_t::")
wald_trust_t_ctrl <- wald(lpm_fe_t_controls_trust, keep = "treat_t::")



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


p_religion_coef <- coef_religious_q %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("Catholic", "Religious practice", "Catholic partner")
    ),
    Quartile = factor(
      treat_level,
      levels = c(2, 3, 4),
      labels = c("Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1")
    )
  ) %>%
  ggplot(aes(x = estimate, y = Quartile)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.12,
    color = "grey45",
    linewidth = 0.55
  ) +
  geom_point(color = "#1f77b4", size = 2.4) +
  facet_wrap(~ Outcome, scales = "free_x", ncol = 1) +
  labs(
    x = "Coefficient estimate relative to Q1",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )


# New figures non-parametric ---------------------------------------------------

# Simple vertical ADRF plots like the reference image
# Quartiles + Tertiles, all outcome groups
# Saved in high resolution to /figures

library(ggplot2)
library(dplyr)
library(grid)


# 1. Clean theme matching your preferred style


theme_vertical_simple <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
      
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = element_text(
        angle = 0,
        face = "bold",
        size = 12,
        hjust = 1
      ),
      
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_blank(),   # remove left-side y-axis title
      axis.text = element_text(size = 10, color = "grey20"),
      
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      
      panel.spacing.y = unit(0.8, "lines"),
      
      # tighter left margin to reduce wasted space
      plot.margin = margin(10, 12, 10, 2)
    )
}


# 2. Helper to keep outcome order safe


clean_outcome_order <- function(data, preferred_order) {
  present <- unique(as.character(data$Outcome))
  c(preferred_order[preferred_order %in% present],
    setdiff(present, preferred_order))
}


# 3. Function for simple vertical ADRF plots


make_vertical_adrf_simple <- function(data,
                                      outcome_order,
                                      x_breaks,
                                      x_labels,
                                      x_title = "") {
  
  outcome_order <- clean_outcome_order(data, outcome_order)
  
  data %>%
    mutate(
      Outcome = factor(Outcome, levels = outcome_order)
    ) %>%
    ggplot(aes(x = treat_level, y = estimate, group = 1)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.45
    ) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.08,
      color = "grey45",
      linewidth = 0.55
    ) +
    geom_line(
      color = "#1f77b4",
      linewidth = 0.9
    ) +
    geom_point(
      color = "#1f77b4",
      size = 2.3
    ) +
    facet_grid(
      rows = vars(Outcome),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = x_title,
      y = NULL   # removes "Effect relative to Q1" / "Effect relative to T1"
    ) +
    coord_cartesian(clip = "off") +
    theme_vertical_simple()
}


# 4. High-resolution save function


save_highres_plot <- function(plot, filename,
                              width = 7.2,
                              height = 5.0,
                              dpi = 600) {
  
  figures_dir <- file.path(getwd(), "figures")
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
  
  pdf_path <- file.path(figures_dir, paste0(filename, ".pdf"))
  png_path <- file.path(figures_dir, paste0(filename, ".png"))
  
  # Vector version for LaTeX
  ggsave(
    filename = pdf_path,
    plot = plot,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  # High-resolution PNG
  ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
  
  message("Saved PDF: ", pdf_path)
  message("Saved PNG: ", png_path)
}


# 5. Outcome orders


outcome_order_rel <- c(
  "Catholic",
  "Religious practice",
  "Catholic partner"
)

outcome_order_pol <- c(
  "Participation",
  "Conservative vote",
  "Left-right scale"
)

outcome_order_other <- c(
  "Education",
  "Income",
  "Trust in people"
)


# 6. Quartile plots


p_adrf_q_rel <- make_vertical_adrf_simple(
  data = adrf_data_religious,
  outcome_order = outcome_order_rel,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)

p_adrf_q_pol <- make_vertical_adrf_simple(
  data = adrf_data_political,
  outcome_order = outcome_order_pol,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)

p_adrf_q_other <- make_vertical_adrf_simple(
  data = adrf_data_other,
  outcome_order = outcome_order_other,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)


# 7. Tertile plots


p_adrf_t_rel <- make_vertical_adrf_simple(
  data = adrf_data_religious_t,
  outcome_order = outcome_order_rel,
  x_breaks = 1:3,
  x_labels = paste0("T", 1:3),
  x_title = ""
)

p_adrf_t_pol <- make_vertical_adrf_simple(
  data = adrf_data_political_t,
  outcome_order = outcome_order_pol,
  x_breaks = 1:3,
  x_labels = paste0("T", 1:3),
  x_title = ""
)

p_adrf_t_other <- make_vertical_adrf_simple(
  data = adrf_data_other_t,
  outcome_order = outcome_order_other,
  x_breaks = 1:3,
  x_labels = paste0("T", 1:3),
  x_title = ""
)


# 8. Show plots


p_adrf_q_rel
p_adrf_q_pol
p_adrf_q_other

p_adrf_t_rel
p_adrf_t_pol
p_adrf_t_other


# 9. Save all in high resolution


save_highres_plot(p_adrf_q_rel,   "adrf_quartiles_religious", width = 7.2, height = 5.0)
save_highres_plot(p_adrf_q_pol,   "adrf_quartiles_political", width = 7.2, height = 5.0)
save_highres_plot(p_adrf_q_other, "adrf_quartiles_other",     width = 7.2, height = 5.0)

save_highres_plot(p_adrf_t_rel,   "adrf_tertiles_religious",  width = 7.2, height = 5.0)
save_highres_plot(p_adrf_t_pol,   "adrf_tertiles_political",  width = 7.2, height = 5.0)
save_highres_plot(p_adrf_t_other, "adrf_tertiles_other",      width = 7.2, height = 5.0)


# Rain does not predict migration -----------------------------------------


# EXTRA OUTCOME: SAME_LOC_BIRTH
#   - Quadratic spec (std + std^2)
#   - Quartile dummies of standardized treatment
#   - Tables like your previous modelsummary blocks:
#       * no DV mean row
#       * no fetch_data / dv_mean helper

# (safety) make sure SAME_LOC_BIRTH is in model_data
if (!("SAME_LOC_BIRTH" %in% names(model_data))) {
  stop("SAME_LOC_BIRTH is not in model_data. Add it to your dplyr::select(...) when building model_data.")
}

# A) Quadratic specification

# 1) With FE, No controls
lpm_fe_nocontrols_same <- feols(
  SAME_LOC_BIRTH ~ childhood_total_dry_days_std  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# 2) With FE, With controls
lpm_fe_controls_same <- feols(
  SAME_LOC_BIRTH ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac + survey_year,
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


# B) Quartile specification

# define quartiles on standardized treatment (same convention as your Q-bins section)
model_data <- model_data %>%
  mutate(treat_q = ntile(childhood_total_dry_days_std, 4))

# 1) With FE, No controls
lpm_fe_q_nocontrols_same <- feols(
  SAME_LOC_BIRTH ~ i(treat_q, ref = 1)  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# 2) With FE, With controls
lpm_fe_q_controls_same <- feols(
  SAME_LOC_BIRTH ~ i(treat_q, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth | BIRTH + prov_nac + survey_year,
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
  CATHOLIC ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cat <- feols(
  CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# RELIGIOUS PRACTICE
lpm_fe_t_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# COUPLE CATHOLIC
lpm_fe_t_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_cou <- feols(
  COUPLE_CATHOLIC ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT | BIRTH + prov_nac + survey_year,
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
  PARTICIPATION ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_par <- feols(
  PARTICIPATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

# CONSERVATIVE VOTE
lpm_fe_t_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_con <- feols(
  CONSERVATIVE_VOTE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

# LEFT-RIGHT SCALE
lpm_fe_t_nocontrols_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1)  | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_lr <- feols(
  LEFT_RIGHT ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth | BIRTH + prov_nac + survey_year,
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
  INCOME ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_inc <- feols(
  INCOME ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# EDUCATION
lpm_fe_t_nocontrols_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_edu <- feols(
  EDUCATION ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

# TRUST IN PEOPLE
lpm_fe_t_nocontrols_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) | BIRTH + prov_nac + survey_year,
  data    = model_data,
  cluster = ~ prov_nac
)

lpm_fe_t_controls_trust <- feols(
  TRUST_PEOPLE ~ i(treat_t, ref = 1) + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth | BIRTH + prov_nac + survey_year,
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
library(tibble)
library(ggplot2)


# 0) Province coordinates -------------------------------------------------

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
    lat,
    long
  )


# 1) Build Conley model data ----------------------------------------------

model_data_conley <- model_data %>%
  left_join(prov_coords_conley, by = "prov_nac") %>%
  filter(
    !is.na(lat),
    !is.na(long),
    !is.na(BIRTH),
    !is.na(prov_nac),
    !is.na(survey_year),
    !is.na(childhood_total_dry_days_std)
  )

# Make sure log population exists
if (!"log_pop_birth" %in% names(model_data_conley) &&
    "pop_birth_last_census" %in% names(model_data_conley)) {
  model_data_conley <- model_data_conley %>%
    mutate(log_pop_birth = log(pop_birth_last_census))
}

# Make sure squared treatment exists
if (!"childhood_total_dry_days_std_sq" %in% names(model_data_conley)) {
  model_data_conley <- model_data_conley %>%
    mutate(childhood_total_dry_days_std_sq = childhood_total_dry_days_std^2)
}

# Quartiles of standardized treatment
model_data_conley <- model_data_conley %>%
  mutate(
    treat_q = ntile(childhood_total_dry_days_std, 4)
  )


# 2) Full MIM controls ----------------------------------------------------

# Fallback definition, only used if apply_mim_controls() is not already loaded
if (!exists("apply_mim_controls")) {
  apply_mim_controls <- function(data, mim_vars) {
    mim_vars <- mim_vars[mim_vars %in% names(data)]
    
    for (v in mim_vars) {
      miss_v <- paste0(v, "_MISS")
      
      if (!miss_v %in% names(data)) {
        data[[miss_v]] <- as.integer(is.na(data[[v]]))
      }
      
      data[[v]] <- ifelse(is.na(data[[v]]), 0, as.numeric(data[[v]]))
    }
    
    data
  }
}

# Full predetermined control set
mim_vars_conley <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_SCHOOL",     "MOTHER_SCHOOL",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

mim_vars_conley <- mim_vars_conley[mim_vars_conley %in% names(model_data_conley)]

model_data_conley <- apply_mim_controls(model_data_conley, mim_vars_conley)

controls_rhs <- c(
  mim_vars_conley,
  paste0(mim_vars_conley, "_MISS"),
  "log_pop_birth"
)

controls_rhs <- unique(controls_rhs[controls_rhs %in% names(model_data_conley)])

fe_rhs <- c(
  "BIRTH",
  "prov_nac",
  "survey_year"
)

q_labels <- c(
  "treat_q::2" = "Q2 vs Q1",
  "treat_q::3" = "Q3 vs Q1",
  "treat_q::4" = "Q4 vs Q1"
)

cutoff_km <- 100

vcov_conley <- conley(
  cutoff = cutoff_km,
  distance = "spherical"
) ~ lat + long


# 3) Helper functions -----------------------------------------------------

dv_mean_from_fml <- function(data, fml) {
  yname <- all.vars(fml[[2]])[1]
  
  fml_chr <- paste(deparse(fml, width.cutoff = 500), collapse = " ")
  parts <- strsplit(fml_chr, "\\|")[[1]]
  
  main_part <- trimws(parts[1])
  fe_part <- if (length(parts) > 1) trimws(parts[2]) else ""
  
  rhs_vars <- all.vars(as.formula(main_part))
  fe_vars <- if (nzchar(fe_part)) all.vars(as.formula(paste0("~", fe_part))) else character(0)
  
  needed <- unique(c(rhs_vars, fe_vars))
  needed <- needed[needed %in% names(data)]
  
  d_est <- data[stats::complete.cases(data[, needed, drop = FALSE]), , drop = FALSE]
  
  mean(d_est[[yname]], na.rm = TRUE)
}

get_wald_p_vcov <- function(mod, keep_pattern, vcov_spec) {
  out <- capture.output(
    w <- fixest::wald(
      mod,
      keep = keep_pattern,
      vcov = vcov_spec
    )
  )
  
  if (!is.null(w$p)) {
    return(sprintf("%.3f", w$p))
  }
  
  p_line <- grep("p-value =", out, value = TRUE)
  
  if (length(p_line) == 0) {
    return(NA_character_)
  }
  
  p_raw <- sub(".*p-value = ([0-9.eE+-]+).*", "\\1", p_line[1])
  sprintf("%.3f", as.numeric(p_raw))
}

fit_lq_models <- function(y, data, treat = "childhood_total_dry_days_std") {
  treat_sq <- paste0(treat, "_sq")
  
  controls_part <- if (length(controls_rhs) > 0) {
    paste0(" + ", paste(controls_rhs, collapse = " + "))
  } else {
    ""
  }
  
  f_lin <- as.formula(
    paste0(
      y, " ~ ", treat,
      controls_part,
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  f_quad <- as.formula(
    paste0(
      y, " ~ ", treat, " + ", treat_sq,
      controls_part,
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  list(
    linear = feols(f_lin, data = data, data.save = TRUE),
    quad   = feols(f_quad, data = data, data.save = TRUE),
    f_lin  = f_lin,
    f_quad = f_quad
  )
}

fit_q_models <- function(y, data) {
  controls_part <- if (length(controls_rhs) > 0) {
    paste0(" + ", paste(controls_rhs, collapse = " + "))
  } else {
    ""
  }
  
  f_noc <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) | ",
      paste(fe_rhs, collapse = " + ")
    )
  )
  
  f_con <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1)",
      controls_part,
      " | ", paste(fe_rhs, collapse = " + ")
    )
  )
  
  list(
    noc   = feols(f_noc, data = data, data.save = TRUE),
    con   = feols(f_con, data = data, data.save = TRUE),
    f_noc = f_noc,
    f_con = f_con
  )
}

build_lq_model_list <- function(mods, labels) {
  out <- list()
  
  for (y in names(labels)) {
    out[[paste0(labels[[y]], ": Linear")]]    <- mods[[y]]$linear
    out[[paste0(labels[[y]], ": Quadratic")]] <- mods[[y]]$quad
  }
  
  out
}

build_lq_add_rows <- function(mods, labels, data) {
  out <- data.frame(
    term = c("Mean dep. var.", "Treatment form"),
    check.names = FALSE
  )
  
  for (y in names(labels)) {
    out[[paste0(labels[[y]], ": Linear")]] <- c(
      sprintf("%.3f", dv_mean_from_fml(data, mods[[y]]$f_lin)),
      "Linear"
    )
    
    out[[paste0(labels[[y]], ": Quadratic")]] <- c(
      sprintf("%.3f", dv_mean_from_fml(data, mods[[y]]$f_quad)),
      "Quadratic"
    )
  }
  
  out
}

build_q_model_list <- function(mods, labels) {
  out <- list()
  
  for (y in names(labels)) {
    out[[paste0(labels[[y]], " (Q bins)")]]              <- mods[[y]]$noc
    out[[paste0(labels[[y]], " (Q bins) + Controls")]]  <- mods[[y]]$con
  }
  
  out
}

build_q_add_rows <- function(mods, labels, data, vcov_spec) {
  out <- data.frame(
    term = c("Mean dep. var.", "Controls", "Wald test p-value"),
    check.names = FALSE
  )
  
  for (y in names(labels)) {
    out[[paste0(labels[[y]], " (Q bins)")]] <- c(
      sprintf("%.3f", dv_mean_from_fml(data, mods[[y]]$f_noc)),
      "No",
      get_wald_p_vcov(mods[[y]]$noc, "treat_q::", vcov_spec)
    )
    
    out[[paste0(labels[[y]], " (Q bins) + Controls")]] <- c(
      sprintf("%.3f", dv_mean_from_fml(data, mods[[y]]$f_con)),
      "Yes",
      get_wald_p_vcov(mods[[y]]$con, "treat_q::", vcov_spec)
    )
  }
  
  out
}

extract_quartile_effects_conley <- function(models, vcov_spec, outcome_labels) {
  purrr::map_dfr(names(models), function(outcome_name) {
    mod <- models[[outcome_name]]
    
    ct <- fixest::coeftable(mod, vcov = vcov_spec)
    ci <- confint(mod, vcov = vcov_spec)
    
    terms <- rownames(ct)[grepl("^treat_q::", rownames(ct))]
    
    out <- tibble(
      Outcome = outcome_name,
      term = terms,
      q = as.integer(sub("^treat_q::", "", terms)),
      estimate = unname(coef(mod)[terms]),
      conf.low = ci[terms, 1],
      conf.high = ci[terms, 2]
    )
    
    bind_rows(
      tibble(
        Outcome = outcome_name,
        term = "treat_q::1",
        q = 1L,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      ),
      out
    )
  }) %>%
    mutate(
      q_label = factor(
        paste0("Q", q),
        levels = paste0("Q", 1:4)
      ),
      Outcome = dplyr::recode(Outcome, !!!outcome_labels),
      Outcome = factor(Outcome, levels = unname(outcome_labels))
    )
}

plot_quartile_effects_conley <- function(df, title = NULL) {
  ggplot(df, aes(x = q, y = estimate, group = 1)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey50") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    scale_x_continuous(
      breaks = 1:4,
      labels = paste0("Q", 1:4)
    ) +
    facet_wrap(~ Outcome, scales = "free_y", ncol = 1) +
    labs(
      title = title,
      x = NULL,
      y = "Effect relative to Q1"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


# 4) Define outcomes ------------------------------------------------------

vote_outcome <- if ("CONSERVATIVE_NO_FAR_RIGHT" %in% names(model_data_conley)) {
  "CONSERVATIVE_NO_FAR_RIGHT"
} else {
  "CONSERVATIVE_VOTE"
}

vote_label <- if (vote_outcome == "CONSERVATIVE_NO_FAR_RIGHT") {
  "Conservative excl. far-right"
} else {
  "Conservative vote"
}

labels_relig <- c(
  CATHOLIC = "Catholic",
  RELIGIOUS_PRACTICE = "Religious practice",
  COUPLE_CATHOLIC = "Catholic partner"
)

labels_pol <- c(
  PARTICIPATION = "Participation",
  setNames(vote_label, vote_outcome),
  LEFT_RIGHT = "Left-right"
)

labels_other <- c(
  EDUCATION = "Education",
  INCOME = "Income",
  TRUST_PEOPLE = "Trust people"
)

labels_relig <- labels_relig[names(labels_relig) %in% names(model_data_conley)]
labels_pol   <- labels_pol[names(labels_pol) %in% names(model_data_conley)]
labels_other <- labels_other[names(labels_other) %in% names(model_data_conley)]


# 5) Linear / quadratic Conley tables -------------------------------------

mods_lq_relig_c <- lapply(names(labels_relig), fit_lq_models, data = model_data_conley)
names(mods_lq_relig_c) <- names(labels_relig)

models_religion_conley <- build_lq_model_list(mods_lq_relig_c, labels_relig)
add_rows_religion_conley <- build_lq_add_rows(mods_lq_relig_c, labels_relig, model_data_conley)

modelsummary(
  models_religion_conley,
  vcov = vcov_conley,
  title = paste0(
    "Religious outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_religion_conley
)


mods_lq_pol_c <- lapply(names(labels_pol), fit_lq_models, data = model_data_conley)
names(mods_lq_pol_c) <- names(labels_pol)

models_politics_conley <- build_lq_model_list(mods_lq_pol_c, labels_pol)
add_rows_politics_conley <- build_lq_add_rows(mods_lq_pol_c, labels_pol, model_data_conley)

modelsummary(
  models_politics_conley,
  vcov = vcov_conley,
  title = paste0(
    "Political outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_politics_conley
)


mods_lq_other_c <- lapply(names(labels_other), fit_lq_models, data = model_data_conley)
names(mods_lq_other_c) <- names(labels_other)

models_other_conley <- build_lq_model_list(mods_lq_other_c, labels_other)
add_rows_other_conley <- build_lq_add_rows(mods_lq_other_c, labels_other, model_data_conley)

modelsummary(
  models_other_conley,
  vcov = vcov_conley,
  title = paste0(
    "Other outcomes: linear and quadratic treatment specifications, Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "childhood_total_dry_days_std"    = "Dry days (std.)",
    "childhood_total_dry_days_std_sq" = "Dry days squared"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  add_rows = add_rows_other_conley
)


# 6) Quartile Conley tables -----------------------------------------------

mods_q_relig_c <- lapply(names(labels_relig), fit_q_models, data = model_data_conley)
names(mods_q_relig_c) <- names(labels_relig)

models_q_religion_conley <- build_q_model_list(mods_q_relig_c, labels_relig)
add_rows_q_religion_conley <- build_q_add_rows(
  mods_q_relig_c,
  labels_relig,
  model_data_conley,
  vcov_conley
)

modelsummary(
  models_q_religion_conley,
  vcov = vcov_conley,
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days (religious outcomes), Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_religion_conley
)


mods_q_pol_c <- lapply(names(labels_pol), fit_q_models, data = model_data_conley)
names(mods_q_pol_c) <- names(labels_pol)

models_q_politics_conley <- build_q_model_list(mods_q_pol_c, labels_pol)
add_rows_q_politics_conley <- build_q_add_rows(
  mods_q_pol_c,
  labels_pol,
  model_data_conley,
  vcov_conley
)

modelsummary(
  models_q_politics_conley,
  vcov = vcov_conley,
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days (political outcomes), Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_politics_conley
)


mods_q_other_c <- lapply(names(labels_other), fit_q_models, data = model_data_conley)
names(mods_q_other_c) <- names(labels_other)

models_q_other_conley <- build_q_model_list(mods_q_other_c, labels_other)
add_rows_q_other_conley <- build_q_add_rows(
  mods_q_other_c,
  labels_other,
  model_data_conley,
  vcov_conley
)

modelsummary(
  models_q_other_conley,
  vcov = vcov_conley,
  title = paste0(
    "LPM with quartile dummies of standardized childhood dry days (other outcomes), Conley SEs (cutoff = ",
    cutoff_km,
    " km)"
  ),
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE|R2 Adj.",
  coef_rename = q_labels,
  add_rows = add_rows_q_other_conley
)


# 7) Quartile plots with Conley SEs ----------------------------------------

coef_religious_q_conley <- extract_quartile_effects_conley(
  models = purrr::map(mods_q_relig_c, "con"),
  vcov_spec = vcov_conley,
  outcome_labels = labels_relig
)

p_q_religious_conley <- plot_quartile_effects_conley(
  coef_religious_q_conley,
  title = paste0(
    "Religious outcomes: quartile effects with Conley SEs, cutoff = ",
    cutoff_km,
    " km"
  )
)

print(p_q_religious_conley)


coef_political_q_conley <- extract_quartile_effects_conley(
  models = purrr::map(mods_q_pol_c, "con"),
  vcov_spec = vcov_conley,
  outcome_labels = labels_pol
)

p_q_political_conley <- plot_quartile_effects_conley(
  coef_political_q_conley,
  title = paste0(
    "Political outcomes: quartile effects with Conley SEs, cutoff = ",
    cutoff_km,
    " km"
  )
)

print(p_q_political_conley)


coef_other_q_conley <- extract_quartile_effects_conley(
  models = purrr::map(mods_q_other_c, "con"),
  vcov_spec = vcov_conley,
  outcome_labels = labels_other
)

p_q_other_conley <- plot_quartile_effects_conley(
  coef_other_q_conley,
  title = paste0(
    "Other outcomes: quartile effects with Conley SEs, cutoff = ",
    cutoff_km,
    " km"
  )
)

print(p_q_other_conley)


# 8) Save plots ------------------------------------------------------------

if (!dir.exists("figures")) {
  dir.create("figures")
}

ggsave(
  filename = "figures/quartiles_religious_conley.png",
  plot = p_q_religious_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_political_conley.png",
  plot = p_q_political_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_other_conley.png",
  plot = p_q_other_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_religious_conley.pdf",
  plot = p_q_religious_conley,
  width = 7,
  height = 8
)

ggsave(
  filename = "figures/quartiles_political_conley.pdf",
  plot = p_q_political_conley,
  width = 7,
  height = 8
)

ggsave(
  filename = "figures/quartiles_other_conley.pdf",
  plot = p_q_other_conley,
  width = 7,
  height = 8
)

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

# 0) Build placebo estimation sample with same MIM controls ----------------

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

# Same predetermined controls as original placebo section
mim_vars_p04 <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

mim_vars_p04 <- mim_vars_p04[mim_vars_p04 %in% names(model_data_p04)]

# Apply shared missing-indicator method
model_data_p04 <- apply_mim_controls(model_data_p04, mim_vars_p04)

# Controls now include original controls + their *_MISS indicators + log population
controls_rhs_p04 <- c(
  mim_vars_p04,
  paste0(mim_vars_p04, "_MISS"),
  "log_pop_birth"
)

controls_rhs_p04 <- controls_rhs_p04[controls_rhs_p04 %in% names(model_data_p04)]

fe_rhs_p04 <- c(
  "BIRTH",
  "prov_nac",
  "survey_year"
)


# 1) Helpers ---------------------------------------------------------------

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
    noc   = feols(f_noc, data = data, cluster = ~ prov_nac),
    con   = feols(f_con, data = data, cluster = ~ prov_nac),
    f_noc = f_noc,
    f_con = f_con
  )
}

# 2) Outcomes

outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
outcomes_other <- c("INCOME", "EDUCATION", "TRUST_PEOPLE")


# 3) LINEAR / QUADRATIC TABLES (same style as main section)


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


# 4) QUARTILE TABLES (same style as quartile section)


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

# Placebo 0–4: quartile plots in new non-parametric style -----------------
# Uses placebo treatment: dry_days_0_4_std
# Uses full MIM controls everywhere


library(dplyr)
library(fixest)
library(ggplot2)
library(purrr)
library(tibble)
library(grid)


# 0) Build placebo estimation sample --------------------------------------

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
    dry_days_0_4_std = (
      dry_days_0_4 - mean(dry_days_0_4, na.rm = TRUE)
    ) / sd(dry_days_0_4, na.rm = TRUE),
    dry_days_0_4_std_sq = dry_days_0_4_std^2,
    treat_q_p04 = ntile(dry_days_0_4_std, 4)
  ) %>%
  dplyr::select(
    CATHOLIC, RELIGIOUS_PRACTICE, COUPLE_CATHOLIC,
    PARTICIPATION, CONSERVATIVE_VOTE, LEFT_RIGHT,
    INCOME, EDUCATION, TRUST_PEOPLE,
    dry_days_0_4, dry_days_0_4_std, dry_days_0_4_std_sq, treat_q_p04,
    survey_year, FEMALE, BIRTH, prov_nac, log_pop_birth,
    FATHER_BORN_SPAIN, MOTHER_BORN_SPAIN,
    FATHER_SCHOOL, MOTHER_SCHOOL,
    FATHER_EMPLOYMENT, MOTHER_EMPLOYMENT
  )


# 1) Apply full MIM controls ----------------------------------------------

if (!exists("apply_mim_controls")) {
  apply_mim_controls <- function(data, mim_vars) {
    mim_vars <- mim_vars[mim_vars %in% names(data)]
    
    for (v in mim_vars) {
      miss_v <- paste0(v, "_MISS")
      
      if (!miss_v %in% names(data)) {
        data[[miss_v]] <- as.integer(is.na(data[[v]]))
      }
      
      data[[v]] <- ifelse(is.na(data[[v]]), 0, as.numeric(data[[v]]))
    }
    
    data
  }
}

mim_vars_p04 <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_SCHOOL",     "MOTHER_SCHOOL",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

mim_vars_p04 <- mim_vars_p04[mim_vars_p04 %in% names(model_data_p04)]

model_data_p04 <- apply_mim_controls(model_data_p04, mim_vars_p04)

controls_full_p04 <- c(
  mim_vars_p04,
  paste0(mim_vars_p04, "_MISS"),
  "log_pop_birth"
)

controls_full_p04 <- controls_full_p04[
  controls_full_p04 %in% names(model_data_p04)
]

controls_full_p04_rhs <- paste(controls_full_p04, collapse = " + ")

fe_rhs_p04 <- "BIRTH + prov_nac + survey_year"


# 2) Fit controlled placebo quartile models -------------------------------

fit_q_p04_controls <- function(y) {
  fml <- as.formula(
    paste0(
      y,
      " ~ i(treat_q_p04, ref = 1) + ",
      controls_full_p04_rhs,
      " | ",
      fe_rhs_p04
    )
  )
  
  feols(
    fml,
    data = model_data_p04,
    cluster = ~ prov_nac
  )
}


models_p04_religious <- list(
  "Catholic"            = fit_q_p04_controls("CATHOLIC"),
  "Religious practice" = fit_q_p04_controls("RELIGIOUS_PRACTICE"),
  "Catholic partner"   = fit_q_p04_controls("COUPLE_CATHOLIC")
)

models_p04_political <- list(
  "Participation"      = fit_q_p04_controls("PARTICIPATION"),
  "Conservative vote" = fit_q_p04_controls("CONSERVATIVE_VOTE"),
  "Left-right scale"  = fit_q_p04_controls("LEFT_RIGHT")
)

models_p04_other <- list(
  "Education"        = fit_q_p04_controls("EDUCATION"),
  "Income"           = fit_q_p04_controls("INCOME"),
  "Trust in people"  = fit_q_p04_controls("TRUST_PEOPLE")
)


# 3) Extract placebo quartile effects -------------------------------------
# Q1 is added explicitly as the omitted baseline.

extract_q_effects_p04 <- function(models_list, var_prefix = "treat_q_p04") {
  purrr::map_dfr(names(models_list), function(outcome_name) {
    
    mod <- models_list[[outcome_name]]
    
    ct <- fixest::coeftable(mod)
    ci <- confint(mod)
    
    terms <- rownames(ct)[grepl(paste0("^", var_prefix, "::"), rownames(ct))]
    
    out <- tibble(
      Outcome = outcome_name,
      term = terms,
      treat_level = as.integer(sub(paste0("^", var_prefix, "::"), "", terms)),
      estimate = unname(coef(mod)[terms]),
      conf.low = ci[terms, 1],
      conf.high = ci[terms, 2]
    )
    
    bind_rows(
      tibble(
        Outcome = outcome_name,
        term = paste0(var_prefix, "::1"),
        treat_level = 1L,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      ),
      out
    )
  }) %>%
    arrange(Outcome, treat_level)
}


adrf_data_p04_religious <- extract_q_effects_p04(models_p04_religious)
adrf_data_p04_political <- extract_q_effects_p04(models_p04_political)
adrf_data_p04_other     <- extract_q_effects_p04(models_p04_other)


# 4) Plot theme and plotting function -------------------------------------

theme_vertical_simple <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
      
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = element_text(
        angle = 0,
        face = "bold",
        size = 12,
        hjust = 1
      ),
      
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 10, color = "grey20"),
      
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      
      panel.spacing.y = unit(0.8, "lines"),
      plot.margin = margin(10, 12, 10, 2)
    )
}

clean_outcome_order <- function(data, preferred_order) {
  present <- unique(as.character(data$Outcome))
  c(
    preferred_order[preferred_order %in% present],
    setdiff(present, preferred_order)
  )
}

make_vertical_adrf_simple <- function(data,
                                      outcome_order,
                                      x_breaks = 1:4,
                                      x_labels = paste0("Q", 1:4),
                                      x_title = "") {
  
  outcome_order <- clean_outcome_order(data, outcome_order)
  
  data %>%
    mutate(
      Outcome = factor(Outcome, levels = outcome_order)
    ) %>%
    ggplot(aes(x = treat_level, y = estimate, group = 1)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.45
    ) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.08,
      color = "grey45",
      linewidth = 0.55
    ) +
    geom_line(
      color = "#1f77b4",
      linewidth = 0.9
    ) +
    geom_point(
      color = "#1f77b4",
      size = 2.3
    ) +
    facet_grid(
      rows = vars(Outcome),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      x = x_title,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_vertical_simple()
}


# 5) Outcome orders -------------------------------------------------------

outcome_order_rel <- c(
  "Catholic",
  "Religious practice",
  "Catholic partner"
)

outcome_order_pol <- c(
  "Participation",
  "Conservative vote",
  "Left-right scale"
)

outcome_order_other <- c(
  "Education",
  "Income",
  "Trust in people"
)


# 6) Create placebo quartile plots ----------------------------------------

p_adrf_q_p04_rel <- make_vertical_adrf_simple(
  data = adrf_data_p04_religious,
  outcome_order = outcome_order_rel,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)

p_adrf_q_p04_pol <- make_vertical_adrf_simple(
  data = adrf_data_p04_political,
  outcome_order = outcome_order_pol,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)

p_adrf_q_p04_other <- make_vertical_adrf_simple(
  data = adrf_data_p04_other,
  outcome_order = outcome_order_other,
  x_breaks = 1:4,
  x_labels = paste0("Q", 1:4),
  x_title = ""
)


# 7) Show plots -----------------------------------------------------------

p_adrf_q_p04_rel
p_adrf_q_p04_pol
p_adrf_q_p04_other


# 8) Save high-resolution plots -------------------------------------------

save_highres_plot <- function(plot, filename,
                              width = 7.2,
                              height = 5.0,
                              dpi = 600) {
  
  figures_dir <- file.path(getwd(), "figures")
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
  
  pdf_path <- file.path(figures_dir, paste0(filename, ".pdf"))
  png_path <- file.path(figures_dir, paste0(filename, ".png"))
  
  ggsave(
    filename = pdf_path,
    plot = plot,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
  
  ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
  
  message("Saved PDF: ", pdf_path)
  message("Saved PNG: ", png_path)
}

save_highres_plot(
  p_adrf_q_p04_rel,
  "adrf_placebo_0_4_quartiles_religious",
  width = 7.2,
  height = 5.0
)

save_highres_plot(
  p_adrf_q_p04_pol,
  "adrf_placebo_0_4_quartiles_political",
  width = 7.2,
  height = 5.0
)

save_highres_plot(
  p_adrf_q_p04_other,
  "adrf_placebo_0_4_quartiles_other",
  width = 7.2,
  height = 5.0
)



# 5) QUARTILE PLOTS: Conley SEs ------------------------------------------


# Helper: extract quartile coefficients and add Q1 = 0 --------------------

extract_quartile_effects_conley <- function(models, vcov_spec, outcome_labels) {
  
  purrr::map_dfr(names(models), function(outcome_name) {
    
    mod <- models[[outcome_name]]
    
    ct <- fixest::coeftable(mod, vcov = vcov_spec)
    ci <- confint(mod, vcov = vcov_spec)
    
    terms <- rownames(ct)[grepl("^treat_q::", rownames(ct))]
    
    out <- tibble(
      Outcome = outcome_name,
      term = terms,
      q = as.integer(sub("^treat_q::", "", terms)),
      estimate = unname(coef(mod)[terms]),
      conf.low = ci[terms, 1],
      conf.high = ci[terms, 2]
    )
    
    # Add omitted baseline Q1 = 0
    bind_rows(
      tibble(
        Outcome = outcome_name,
        term = "treat_q::1",
        q = 1L,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      ),
      out
    )
  }) %>%
    mutate(
      q_label = factor(
        paste0("Q", q),
        levels = paste0("Q", 1:4)
      ),
      Outcome = dplyr::recode(Outcome, !!!outcome_labels),
      Outcome = factor(Outcome, levels = unname(outcome_labels))
    )
}


# Helper: plot quartile effects -------------------------------------------

plot_quartile_effects_conley <- function(df, title = NULL) {
  
  ggplot(df, aes(x = q_label, y = estimate, group = 1)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey50") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    facet_wrap(~ Outcome, scales = "free_y", ncol = 1) +
    labs(
      title = title,
      x = NULL,
      y = "Effect relative to Q1"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Helper for Conley quartile models ---------------------------------------

fit_q_models <- function(y, data) {
  
  # No-controls model
  f_noc <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) | ",
      paste(fe_rhs, collapse = " + ")
    )
  )
  
  # Controls model
  f_con <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + ",
      paste(controls_rhs, collapse = " + "),
      " | ",
      paste(fe_rhs, collapse = " + ")
    )
  )
  
  list(
    noc   = feols(f_noc, data = data),
    con   = feols(f_con, data = data),
    f_noc = f_noc,
    f_con = f_con
  )
}


# Re-create Conley quartile models before plotting ------------------------

outcomes_relig <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
outcomes_pol   <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
outcomes_other <- c("INCOME", "EDUCATION", "TRUST_PEOPLE")

mods_q_relig_c <- lapply(outcomes_relig, fit_q_models, data = model_data_conley)
names(mods_q_relig_c) <- outcomes_relig

mods_q_pol_c <- lapply(outcomes_pol, fit_q_models, data = model_data_conley)
names(mods_q_pol_c) <- outcomes_pol

mods_q_other_c <- lapply(outcomes_other, fit_q_models, data = model_data_conley)
names(mods_q_other_c) <- outcomes_other

# Religious outcomes ------------------------------------------------------

coef_religious_q_conley <- extract_quartile_effects_conley(
  models = list(
    CATHOLIC = mods_q_relig_c$CATHOLIC$con,
    RELIGIOUS_PRACTICE = mods_q_relig_c$RELIGIOUS_PRACTICE$con,
    COUPLE_CATHOLIC = mods_q_relig_c$COUPLE_CATHOLIC$con
  ),
  vcov_spec = vcov_conley,
  outcome_labels = c(
    CATHOLIC = "Catholic",
    RELIGIOUS_PRACTICE = "Religious practice",
    COUPLE_CATHOLIC = "Catholic partner"
  )
)

p_q_religious_conley <- plot_quartile_effects_conley(
  coef_religious_q_conley,
  title = paste0("Religious outcomes: quartile effects with Conley SEs, cutoff = ", cutoff_km, " km")
)

print(p_q_religious_conley)


# Political outcomes ------------------------------------------------------

coef_political_q_conley <- extract_quartile_effects_conley(
  models = list(
    PARTICIPATION = mods_q_pol_c$PARTICIPATION$con,
    CONSERVATIVE_VOTE = mods_q_pol_c$CONSERVATIVE_VOTE$con,
    LEFT_RIGHT = mods_q_pol_c$LEFT_RIGHT$con
  ),
  vcov_spec = vcov_conley,
  outcome_labels = c(
    PARTICIPATION = "Participation",
    CONSERVATIVE_VOTE = "Conservative vote",
    LEFT_RIGHT = "Left-right scale"
  )
)

p_q_political_conley <- plot_quartile_effects_conley(
  coef_political_q_conley,
  title = paste0("Political outcomes: quartile effects with Conley SEs, cutoff = ", cutoff_km, " km")
)

print(p_q_political_conley)


# Other outcomes ----------------------------------------------------------

coef_other_q_conley <- extract_quartile_effects_conley(
  models = list(
    EDUCATION = mods_q_other_c$EDUCATION$con,
    INCOME = mods_q_other_c$INCOME$con,
    TRUST_PEOPLE = mods_q_other_c$TRUST_PEOPLE$con
  ),
  vcov_spec = vcov_conley,
  outcome_labels = c(
    EDUCATION = "Education",
    INCOME = "Income",
    TRUST_PEOPLE = "Trust in people"
  )
)

p_q_other_conley <- plot_quartile_effects_conley(
  coef_other_q_conley,
  title = paste0("Other outcomes: quartile effects with Conley SEs, cutoff = ", cutoff_km, " km")
)

print(p_q_other_conley)


# Save plots --------------------------------------------------------------

if (!dir.exists("figures")) dir.create("figures")

ggsave(
  filename = "figures/quartiles_religious_conley.png",
  plot = p_q_religious_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_political_conley.png",
  plot = p_q_political_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_other_conley.png",
  plot = p_q_other_conley,
  width = 7,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "figures/quartiles_religious_conley.pdf",
  plot = p_q_religious_conley,
  width = 7,
  height = 8
)

ggsave(
  filename = "figures/quartiles_political_conley.pdf",
  plot = p_q_political_conley,
  width = 7,
  height = 8
)

ggsave(
  filename = "figures/quartiles_other_conley.pdf",
  plot = p_q_other_conley,
  width = 7,
  height = 8
)

# Regressions with age-of-exposure variables --------------------------------

# Catholic identification
lpm_age_hetero_cat <- feols(
  CATHOLIC ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~prov_nac
)

# Religious practice
lpm_age_hetero_rel <- feols(
  RELIGIOUS_PRACTICE ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~prov_nac
)

# Couple Catholic
lpm_age_hetero_cou <- feols(
  COUPLE_CATHOLIC ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~prov_nac
)

# Participation
lpm_age_hetero_par <- feols(
  PARTICIPATION ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~prov_nac
)

# Conservative vote
lpm_age_hetero_con <- feols(
  CONSERVATIVE_VOTE ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  |
    BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~prov_nac
)

# Left-right self-placement
lpm_age_hetero_lr <- feols(
  LEFT_RIGHT ~ dry_days_5_9 + dry_days_10_14 + dry_days_15_18 + FEMALE +
    FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  |
    BIRTH + prov_nac + survey_year,
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
  CATHOLIC ~ childhood_total_dry_days_std + I(childhood_total_dry_days_std^2)| BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN  +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: RELIGIOUS PRACTICE 

# 1. With FE, No controls
lpm_fe_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN + FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +
    MOTHER_EMPLOYMENT| BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: COUPLE_CATHOLIC


# 1. With FE, No controls
lpm_fe_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + I(childhood_total_dry_days_std^2)  | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN  + 
    MOTHER_EMPLOYMENT  | BIRTH + prov_nac + survey_year,
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
  PARTICIPATION ~ childhood_total_dry_days_std + I(childhood_total_dry_days_std^2)    | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2)  + 
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN  + MOTHER_EMPLOYMENT | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: CONSERVATIVE_VOTE


# 1. With FE, No controls
lpm_fe_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std  + I(childhood_total_dry_days_std^2)   | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) + 
    FATHER_BORN_SPAIN +  
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EMPLOYMENT  | BIRTH + prov_nac + survey_year ,
  data = model_data,
  cluster = ~birth_prov_cluster
)


# Linear Probability Models: LEFT_RIGHT


# 1. With FE, No controls
lpm_fe_nocontrols_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std  + I(childhood_total_dry_days_std^2) | BIRTH + prov_nac + survey_year,
  data = model_data,
  cluster = ~birth_prov_cluster
)

# 2. With FE, With controls
lpm_fe_controls_far <- feols(
  LEFT_RIGHT ~ childhood_total_dry_days_std + FEMALE + I(childhood_total_dry_days_std^2) +
    FATHER_BORN_SPAIN  + 
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN +  
    MOTHER_EMPLOYMENT   | BIRTH + prov_nac + survey_year,
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
              "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT")

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
    " | BIRTH + prov_nac + survey_year"
  ))
  
  # quartile ADRF (baseline = Q1)
  f_q <- as.formula(paste0(
    y, " ~ factor(treat_q) + ",
    paste(controls, collapse = " + "),
    " | BIRTH + prov_nac + survey_year"
  ))
  
  # tertile ADRF (baseline = T1)
  f_t <- as.formula(paste0(
    y, " ~ factor(treat_t) + ",
    paste(controls, collapse = " + "),
    " | BIRTH + prov_nac + survey_year"
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
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)"
  )
  
  modelsummary(
    q_list,
    title = paste0("Weather exposure: ", treat_lab, " — Quartile ADRF, FE + controls"),
    output = file.path("weather_age_outputs/tables", paste0("quartiles_", treat, ".tex")),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)"
  )
  
  modelsummary(
    t_list,
    title = paste0("Weather exposure: ", treat_lab, " — Tertile ADRF, FE + controls"),
    output = file.path("weather_age_outputs/tables", paste0("tertiles_", treat, ".tex")),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    coef_omit = "^(FATHER_|MOTHER_|FEMALE)"
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
              "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT")


# A) Continuous flexible model with FEMALE interaction
#    (treat + treat^2) × FEMALE


run_cont_models <- function(y, with_controls = TRUE) {
  
  rhs_main <- if (with_controls) paste(controls, collapse = " + ") else "FEMALE"
  
  # Explicitly include: treat_std + treat_std^2 + FEMALE + interactions
  f <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + FEMALE + treat_std:FEMALE + I(treat_std^2):FEMALE + ",
    rhs_main,
    " | BIRTH + prov_nac + survey_year"
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
  rhs <- if (with_controls) paste(controls, collapse = " + ") else " FEMALE"
  f <- as.formula(paste0(
    y, " ~ factor(treat_q) * FEMALE + ", rhs, " | BIRTH + prov_nac + survey_year"
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
  rhs <- if (with_controls) paste(controls, collapse = " + ") else " FEMALE"
  f <- as.formula(paste0(
    y, " ~ factor(treat_t) * FEMALE + ", rhs, " | BIRTH + prov_nac + survey_year"
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
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

# A) Continuous: treat_std + treat_std^2

run_cont <- function(y, with_controls = TRUE) {
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else ""
  f <- as.formula(paste0(
    y, " ~ treat_std + I(treat_std^2) + ", rhs_ctrl, " | BIRTH + prov_nac + survey_year"
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
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else ""
  f <- as.formula(paste0(
    y, " ~ factor(treat_q) + ", rhs_ctrl, " | BIRTH + prov_nac + survey_year"
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
  
  rhs_ctrl <- if (with_controls) paste(controls, collapse = " + ") else ""
  f <- as.formula(paste0(
    y, " ~ factor(treat_t) + ", rhs_ctrl, " | BIRTH + prov_nac + survey_year"
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


# 1) DEFINE SOUTH PROVINCES MANUALLY

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


# 2) BUILD MODEL DATA WITH SOUTH DUMMY + SAME MIM CONTROLS

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

# Predetermined controls used in this heterogeneity section
# Now includes parental school covariates
mim_vars_ns <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_ns <- mim_vars_ns[mim_vars_ns %in% names(model_data_ns)]

# Apply your shared missing-indicator method
model_data_ns <- apply_mim_controls(model_data_ns, mim_vars_ns)

controls_ns <- c(
  mim_vars_ns,
  paste0(mim_vars_ns, "_MISS"),
  "log_pop_birth"
)

controls_ns <- controls_ns[controls_ns %in% names(model_data_ns)]

controls_ns_rhs <- paste(controls_ns, collapse = " + ")

# Quick check
model_data_ns %>%
  count(south, treat_q, name = "n_q") %>%
  arrange(south, treat_q) %>%
  print()

# Optional MIM sanity check
model_data_ns %>%
  summarise(
    across(
      all_of(c(mim_vars_ns, paste0(mim_vars_ns, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)


# 3) QUARTILE HETEROGENEITY MODELS
# Baseline: Q1 in North (south == 0)

fit_ns_interaction <- function(y) {
  fml <- as.formula(
    paste0(
      y, " ~ factor(treat_q) * south + ",
      controls_ns_rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data    = model_data_ns,
    cluster = ~ prov_nac
  )
}

# Religious practice
lpm_fe_q_controls_rel_ns <- fit_ns_interaction("RELIGIOUS_PRACTICE")

# Conservative vote
lpm_fe_q_controls_con_ns <- fit_ns_interaction("CONSERVATIVE_VOTE")

# Left-right
lpm_fe_q_controls_lr_ns <- fit_ns_interaction("LEFT_RIGHT")

# Catholic
lpm_fe_q_controls_cat_ns <- fit_ns_interaction("CATHOLIC")

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

wald_q_cat_ns <- wald(
  lpm_fe_q_controls_cat_ns,
  "factor(treat_q)2:south = 0 & factor(treat_q)3:south = 0 & factor(treat_q)4:south = 0"
)

wald_q_rel_ns
wald_q_con_ns
wald_q_lr_ns
wald_q_cat_ns

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


# 4) PLOT FUNCTION: QUARTILE ADRF, NORTH VS SOUTH


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


# 5) SAVE THE SAME 3 QUARTILE HETEROGENEITY PLOTS


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


# ============================================================
# LINEAR NORTH-SOUTH HETEROGENEITY
#
# Two tables:
#   1) Full sample with Treatment × South interaction
#   2) Separate samples: North and South
#
# Outcomes:
#   - CATHOLIC
#   - CONSERVATIVE_VOTE
#
# Treatment:
#   - childhood_total_dry_days_std
#
# FE:
#   - BIRTH
#   - prov_nac
#   - survey_year
#
# SE:
#   - clustered by province
# ============================================================


# ------------------------------------------------------------
# 0) Helper functions
# ------------------------------------------------------------

depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

make_add_rows <- function(models) {
  
  rows <- tibble::tibble(
    term = c(
      "Controls",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var."
    )
  )
  
  for (nm in names(models)) {
    rows[[nm]] <- c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      as.character(depvar_mean(models[[nm]]))
    )
  }
  
  rows
}


# ------------------------------------------------------------
# 1) TABLE 1: Full sample with Treatment × South interaction
# ------------------------------------------------------------

fit_full_interaction_linear <- function(y) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    "childhood_total_dry_days_std:south",
    controls_ns
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data    = model_data_ns,
    cluster = ~ prov_nac
  )
}


# Estimate full-sample interaction models

lpm_fe_lin_cat_interaction_ns <- fit_full_interaction_linear("CATHOLIC")

lpm_fe_lin_con_interaction_ns <- fit_full_interaction_linear("CONSERVATIVE_VOTE")


# Wald tests:
# H0: Treatment effect is the same in South and North
# Equivalent to: Treatment × South = 0

wald_lin_cat_interaction_ns <- fixest::wald(
  lpm_fe_lin_cat_interaction_ns,
  "childhood_total_dry_days_std:south = 0"
)

wald_lin_con_interaction_ns <- fixest::wald(
  lpm_fe_lin_con_interaction_ns,
  "childhood_total_dry_days_std:south = 0"
)

wald_lin_cat_interaction_ns
wald_lin_con_interaction_ns


# Optional: extract p-values from interaction test

get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(w <- fixest::wald(mod, keep = keep_pattern))
  p_line <- grep("p-value =", out, value = TRUE)
  as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1]))
}

p_cat_interaction_ns <- round(
  get_wald_p_print(
    lpm_fe_lin_cat_interaction_ns,
    "childhood_total_dry_days_std:south"
  ),
  3
)

p_con_interaction_ns <- round(
  get_wald_p_print(
    lpm_fe_lin_con_interaction_ns,
    "childhood_total_dry_days_std:south"
  ),
  3
)


# Optional: compute implied South slope = North slope + interaction

south_slope <- function(mod) {
  
  b <- coef(mod)
  
  b_north <- b["childhood_total_dry_days_std"]
  b_inter <- b["childhood_total_dry_days_std:south"]
  
  round(b_north + b_inter, 3)
}


models_full_interaction_ns <- list(
  "Catholic"          = lpm_fe_lin_cat_interaction_ns,
  "Conservative vote" = lpm_fe_lin_con_interaction_ns
)


# Table 1

modelsummary(
  models_full_interaction_ns,
  title = "Linear Specification: Full Sample with North-South Interaction",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std"       = "Dry days, standardized",
    "childhood_total_dry_days_std:south" = "Dry days, standardized × South"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = tibble::tibble(
    term = c(
      "Controls",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var.",
      "Implied South slope",
      "p-value: Dry days × South = 0"
    ),
    `Catholic` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      depvar_mean(lpm_fe_lin_cat_interaction_ns),
      south_slope(lpm_fe_lin_cat_interaction_ns),
      p_cat_interaction_ns
    ),
    `Conservative vote` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      depvar_mean(lpm_fe_lin_con_interaction_ns),
      south_slope(lpm_fe_lin_con_interaction_ns),
      p_con_interaction_ns
    )
  )
)


# ------------------------------------------------------------
# 2) TABLE 2: Separate samples by North and South
# ------------------------------------------------------------

fit_separate_sample_linear <- function(y, region_value) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    controls_ns
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data    = model_data_ns %>% filter(south == region_value),
    cluster = ~ prov_nac
  )
}


# Estimate separate-sample models

lpm_fe_lin_cat_north_ns <- fit_separate_sample_linear("CATHOLIC", 0)
lpm_fe_lin_cat_south_ns <- fit_separate_sample_linear("CATHOLIC", 1)

lpm_fe_lin_con_north_ns <- fit_separate_sample_linear("CONSERVATIVE_VOTE", 0)
lpm_fe_lin_con_south_ns <- fit_separate_sample_linear("CONSERVATIVE_VOTE", 1)


models_separate_samples_ns <- list(
  "Catholic – North"           = lpm_fe_lin_cat_north_ns,
  "Catholic – South"           = lpm_fe_lin_cat_south_ns,
  "Conservative vote – North"  = lpm_fe_lin_con_north_ns,
  "Conservative vote – South"  = lpm_fe_lin_con_south_ns
)


# Optional sample-size check

model_data_ns %>%
  count(south, name = "n") %>%
  mutate(region = ifelse(south == 1, "South", "North")) %>%
  select(region, n) %>%
  print()


# Table 2

modelsummary(
  models_separate_samples_ns,
  title = "Linear Specification: Separate North and South Samples",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" = "Dry days, standardized"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = make_add_rows(models_separate_samples_ns)
)
# ============================================================
# NORTH VS SOUTH SEPARATE-SAMPLE QUARTILE PLOTS
# Same plot style as before: line + ribbon CI
# Uses MIM controls
# ============================================================

library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)
library(stringr)


# ------------------------------------------------------------
# 1. Define South provinces
# ------------------------------------------------------------

south_prov_nac <- c(
  4, 6, 10, 11, 14, 18, 21, 23, 29, 30, 41
)


# ------------------------------------------------------------
# 2. Build model data
# Quartiles are defined in the pooled sample, so Q1-Q4 mean
# the same treatment bins in North and South.
# ------------------------------------------------------------

model_data_ns <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(pop_birth_last_census),
    pop_birth_last_census > 0
  ) %>%
  mutate(
    south = as.integer(prov_nac %in% south_prov_nac),
    region = if_else(south == 1, "South", "North"),
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    log_pop_birth = log(pop_birth_last_census),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4)
  )


# ------------------------------------------------------------
# 3. MIM controls
# ------------------------------------------------------------

mim_vars_ns <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_ns <- mim_vars_ns[mim_vars_ns %in% names(model_data_ns)]

# Apply shared missing-indicator method
model_data_ns <- apply_mim_controls(model_data_ns, mim_vars_ns)

controls_ns <- c(
  mim_vars_ns,
  paste0(mim_vars_ns, "_MISS"),
  "log_pop_birth"
)

controls_ns <- controls_ns[controls_ns %in% names(model_data_ns)]

controls_ns_rhs <- paste(controls_ns, collapse = " + ")


# MIM sanity check
model_data_ns %>%
  summarise(
    across(
      all_of(c(mim_vars_ns, paste0(mim_vars_ns, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)


# Check sample sizes by region and quartile
model_data_ns %>%
  count(region, treat_q, name = "n") %>%
  arrange(region, treat_q) %>%
  print()


# ------------------------------------------------------------
# 4. Outcomes and labels
# Only Catholic and Conservative vote
# ------------------------------------------------------------

outcomes_ns <- c(
  "CATHOLIC",
  "CONSERVATIVE_VOTE"
)

outcome_labels_ns <- c(
  CATHOLIC = "Catholic",
  CONSERVATIVE_VOTE = "Conservative vote"
)

# ------------------------------------------------------------
# 5. Fit separate North and South models
# Baseline is Q1 within each separate sample.
# ------------------------------------------------------------

fit_split_ns <- function(y, data) {
  
  fml <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + ",
      controls_ns_rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  list(
    North = feols(
      fml,
      data = data %>% filter(south == 0),
      cluster = ~ prov_nac
    ),
    South = feols(
      fml,
      data = data %>% filter(south == 1),
      cluster = ~ prov_nac
    )
  )
}

models_split_ns <- lapply(outcomes_ns, fit_split_ns, data = model_data_ns)
names(models_split_ns) <- outcomes_ns


# ------------------------------------------------------------
# 6. Pooled interaction models for formal North-South tests
# These are not used for the plotted coefficients, only for
# the Wald p-values.
# ------------------------------------------------------------

fit_interaction_ns <- function(y, data) {
  
  fml <- as.formula(
    paste0(
      y, " ~ factor(treat_q) * south + ",
      controls_ns_rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data = data,
    cluster = ~ prov_nac
  )
}

models_interaction_ns <- lapply(outcomes_ns, fit_interaction_ns, data = model_data_ns)
names(models_interaction_ns) <- outcomes_ns

get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(fixest::wald(mod, keep = keep_pattern))
  p_line <- grep("p-value =", out, value = TRUE)
  as.numeric(sub(".*p-value = ([0-9.]+).*", "\\1", p_line[1]))
}

wald_p_ns <- tibble(
  outcome = outcomes_ns,
  outcome_label = unname(outcome_labels_ns[outcomes_ns]),
  wald_p = map_dbl(
    models_interaction_ns,
    ~ get_wald_p_print(.x, "factor\\(treat_q\\)[234]:south")
  )
)

wald_p_ns


# ------------------------------------------------------------
# 7. Extract separate-sample quartile estimates
# Q1 is normalized to 0 within each regional sample.
# ------------------------------------------------------------

extract_split_q_effects <- function(mod, group_name, outcome_name, level = 0.95) {
  
  z <- qnorm(1 - (1 - level) / 2)
  
  td <- broom::tidy(mod) %>%
    filter(str_detect(term, "^treat_q::")) %>%
    mutate(
      treat_level = as.integer(str_remove(term, "^treat_q::")),
      group = group_name,
      outcome = outcome_name,
      estimate = estimate,
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error
    ) %>%
    select(outcome, group, treat_level, estimate, conf.low, conf.high)
  
  bind_rows(
    tibble(
      outcome = outcome_name,
      group = group_name,
      treat_level = 1L,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    ),
    td
  ) %>%
    arrange(group, treat_level)
}


coef_ns <- map_dfr(names(models_split_ns), function(y) {
  
  bind_rows(
    extract_split_q_effects(models_split_ns[[y]]$North, "North", y),
    extract_split_q_effects(models_split_ns[[y]]$South, "South", y)
  )
}) %>%
  mutate(
    outcome_label = recode(outcome, !!!outcome_labels_ns),
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels_ns[outcomes_ns])
    ),
    group = factor(group, levels = c("North", "South"))
  )


# ------------------------------------------------------------
# 8. Plot function: same style as previous ADRF plots
# Line + ribbon CI, no offset error bars.
# ------------------------------------------------------------

plot_ns_separate_ribbon <- function(data,
                                    outcome_keep,
                                    title = "",
                                    subtitle = "",
                                    filename = NULL,
                                    width = 8,
                                    height = 5,
                                    dpi = 600,
                                    level = 0.95) {
  
  plot_data <- data %>%
    filter(outcome %in% outcome_keep) %>%
    mutate(
      outcome_label = factor(
        outcome_label,
        levels = unname(outcome_labels_ns[outcome_keep])
      )
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = treat_level,
      y = estimate,
      color = group,
      fill = group,
      group = group
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    facet_wrap(
      ~ outcome_label,
      scales = "free_y",
      ncol = 1
    ) +
    scale_x_continuous(
      breaks = 1:4,
      labels = paste0("Q", 1:4),
      limits = c(1, 4)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile",
      y = paste0("Estimate relative to Q1 within region (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  print(p)
  
  if (!is.null(filename)) {
    
    out_dir <- file.path(getwd(), "figures")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    pdf_path <- file.path(out_dir, paste0(filename, ".pdf"))
    png_path <- file.path(out_dir, paste0(filename, ".png"))
    
    ggsave(
      filename = pdf_path,
      plot = p,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "in",
      bg = "white"
    )
    
    ggsave(
      filename = png_path,
      plot = p,
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      bg = "white"
    )
    
    message("Saved PDF: ", pdf_path)
    message("Saved PNG: ", png_path)
  }
  
  invisible(p)
}


# ------------------------------------------------------------
# 9. Make and save plots
# Only Catholic and Conservative vote
# ------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)


# Catholic only
p_ns_catholic <- plot_ns_separate_ribbon(
  data = coef_ns,
  outcome_keep = c("CATHOLIC"),
  title = "Catholic — North vs South separate samples",
  subtitle = "Quartile estimates from separate regional regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "north_south_separate_catholic_ribbon",
  width = 8,
  height = 4.2
)


# Conservative vote only
p_ns_conservative <- plot_ns_separate_ribbon(
  data = coef_ns,
  outcome_keep = c("CONSERVATIVE_VOTE"),
  title = "Conservative vote — North vs South separate samples",
  subtitle = "Quartile estimates from separate regional regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "north_south_separate_conservative_vote_ribbon",
  width = 8,
  height = 4.2
)


# Combined Catholic + Conservative vote plot
p_ns_catholic_conservative <- plot_ns_separate_ribbon(
  data = coef_ns,
  outcome_keep = c("CATHOLIC", "CONSERVATIVE_VOTE"),
  title = "Catholic and Conservative vote — North vs South separate samples",
  subtitle = "Quartile estimates from separate regional regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "north_south_separate_catholic_conservative_ribbon",
  width = 8,
  height = 5.2
)


# ------------------------------------------------------------
# 10. Optional table: separate-sample quartile estimates
# Only Catholic and Conservative vote
# ------------------------------------------------------------

modelsummary(
  list(
    "Catholic – North" = models_split_ns$CATHOLIC$North,
    "Catholic – South" = models_split_ns$CATHOLIC$South,
    "Conservative vote – North" = models_split_ns$CONSERVATIVE_VOTE$North,
    "Conservative vote – South" = models_split_ns$CONSERVATIVE_VOTE$South
  ),
  title = "North and South separate-sample quartile estimates",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.",
  coef_rename = c(
    "treat_q::2" = "Q2 vs Q1",
    "treat_q::3" = "Q3 vs Q1",
    "treat_q::4" = "Q4 vs Q1"
  )
)


# ------------------------------------------------------------
# 11. Optional table: pooled interaction p-values
# ------------------------------------------------------------

wald_p_ns

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

# 0) ASSUMPTION

# This code assumes you already have these objects in memory from your main script:
#   - survey
#   - normalize_name()
#   - name_map
#   - prov_code_map
#
# If not, run the earlier harmonization / mapping section first.


# 1) LOAD BROTHERHOODS BY PROVINCE + POPULATION, BUILD DENSITY


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


# 2) BUILD MODEL DATA WITH HIGH-COFRADIAS DUMMY


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


# 3) QUARTILE HETEROGENEITY MODELS
# Baseline: Q1 in LOW-cofradias provinces (high_cofradias == 0)


# Catholic
lpm_fe_q_controls_cat_cof <- feols(
  CATHOLIC ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT  + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Religious practice
lpm_fe_q_controls_rel_cof <- feols(
  RELIGIOUS_PRACTICE ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Conservative vote
lpm_fe_q_controls_con_cof <- feols(
  CONSERVATIVE_VOTE ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data_cof,
  cluster = ~ prov_nac
)

# Left-right
lpm_fe_q_controls_lr_cof <- feols(
  LEFT_RIGHT ~ factor(treat_q) * high_cofradias +
    FEMALE + FATHER_BORN_SPAIN + MOTHER_BORN_SPAIN +
    FATHER_EMPLOYMENT + MOTHER_EMPLOYMENT + log_pop_birth |
    BIRTH + prov_nac + survey_year,
  data    = model_data_cof,
  cluster = ~ prov_nac
)


# 4) JOINT WALD TESTS: ARE QUARTILE PROFILES DIFFERENT
#    ACROSS LOW- VS HIGH-COFRADIAS PROVINCES?


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


# 5) TABLE


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


# 6) PLOT FUNCTION: QUARTILE ADRF, LOW VS HIGH COFRADIAS


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


# 7) SAVE THE SAME 4 QUARTILE HETEROGENEITY PLOTS


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

# ============================================================
# HETEROGENEITY: HIGH VS LOW COFRADIAS DENSITY
# MIM controls + linear tables + Catholic/Conservative graphs only
# ============================================================


# ------------------------------------------------------------
# 2) BUILD MODEL DATA WITH HIGH-COFRADIAS DUMMY + MIM CONTROLS
# ------------------------------------------------------------

model_data_cof <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(pop_birth_last_census),
    pop_birth_last_census > 0
  ) %>%
  left_join(
    cofradias_prov %>%
      dplyr::select(prov_nac, cofradias_per_100k, high_cofradias),
    by = "prov_nac"
  ) %>%
  filter(!is.na(high_cofradias)) %>%
  mutate(
    group_cof = if_else(high_cofradias == 1, "High cofradias", "Low cofradias"),
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    log_pop_birth = log(pop_birth_last_census),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE),
    treat_q = ntile(childhood_total_dry_days_std, 4)
  )


# ------------------------------------------------------------
# 3) MIM CONTROLS
# ------------------------------------------------------------

mim_vars_cof <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_cof <- mim_vars_cof[mim_vars_cof %in% names(model_data_cof)]

# Apply shared missing-indicator method
model_data_cof <- apply_mim_controls(model_data_cof, mim_vars_cof)

controls_cof <- c(
  mim_vars_cof,
  paste0(mim_vars_cof, "_MISS"),
  "log_pop_birth"
)

controls_cof <- controls_cof[controls_cof %in% names(model_data_cof)]
controls_cof_rhs <- paste(controls_cof, collapse = " + ")


# MIM sanity check
model_data_cof %>%
  summarise(
    across(
      all_of(c(mim_vars_cof, paste0(mim_vars_cof, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)


# Sample checks
model_data_cof %>%
  count(group_cof, treat_q, name = "n_q") %>%
  arrange(group_cof, treat_q) %>%
  print()

model_data_cof %>%
  count(group_cof, name = "n_group") %>%
  print()


# ------------------------------------------------------------
# 4) OUTCOMES: ONLY CATHOLIC AND CONSERVATIVE VOTE
# ------------------------------------------------------------

outcomes_cof <- c(
  "CATHOLIC",
  "CONSERVATIVE_VOTE"
)

outcome_labels_cof <- c(
  CATHOLIC = "Catholic",
  CONSERVATIVE_VOTE = "Conservative vote"
)


# ============================================================
# PART A. LINEAR TABLE 1:
# FULL SAMPLE WITH TREATMENT × HIGH COFRADIAS INTERACTION
# ============================================================

depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 2)
}

get_coef_p <- function(mod, pattern) {
  ct <- coeftable(mod)
  term <- grep(pattern, rownames(ct), value = TRUE)[1]
  if (is.na(term)) return(NA_real_)
  round(ct[term, "Pr(>|t|)"], 3)
}

interaction_term_name <- function(mod) {
  grep(
    "childhood_total_dry_days_std:high_cofradias|high_cofradias:childhood_total_dry_days_std",
    names(coef(mod)),
    value = TRUE
  )[1]
}

high_slope <- function(mod) {
  b <- coef(mod)
  int_term <- interaction_term_name(mod)
  round(b["childhood_total_dry_days_std"] + b[int_term], 3)
}

fit_full_interaction_linear_cof <- function(y) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    "childhood_total_dry_days_std:high_cofradias",
    controls_cof
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data = model_data_cof,
    cluster = ~ prov_nac
  )
}


# Estimate full-sample interaction models

lpm_fe_lin_cat_interaction_cof <- fit_full_interaction_linear_cof("CATHOLIC")
lpm_fe_lin_con_interaction_cof <- fit_full_interaction_linear_cof("CONSERVATIVE_VOTE")


# Interaction p-values

p_cat_interaction_cof <- get_coef_p(
  lpm_fe_lin_cat_interaction_cof,
  "childhood_total_dry_days_std:high_cofradias|high_cofradias:childhood_total_dry_days_std"
)

p_con_interaction_cof <- get_coef_p(
  lpm_fe_lin_con_interaction_cof,
  "childhood_total_dry_days_std:high_cofradias|high_cofradias:childhood_total_dry_days_std"
)


models_full_interaction_cof <- list(
  "Catholic" = lpm_fe_lin_cat_interaction_cof,
  "Conservative vote" = lpm_fe_lin_con_interaction_cof
)


# Table 1: full sample interaction

modelsummary(
  models_full_interaction_cof,
  title = "Linear Specification: Full Sample with Cofradias-Density Interaction",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" = "Dry days, standardized",
    "childhood_total_dry_days_std:high_cofradias" = "Dry days, standardized × High cofradias",
    "high_cofradias:childhood_total_dry_days_std" = "Dry days, standardized × High cofradias"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = tibble::tibble(
    term = c(
      "Controls",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var.",
      "Implied high-cofradias slope",
      "p-value: Dry days × High cofradias = 0"
    ),
    `Catholic` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      depvar_mean(lpm_fe_lin_cat_interaction_cof),
      high_slope(lpm_fe_lin_cat_interaction_cof),
      p_cat_interaction_cof
    ),
    `Conservative vote` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      depvar_mean(lpm_fe_lin_con_interaction_cof),
      high_slope(lpm_fe_lin_con_interaction_cof),
      p_con_interaction_cof
    )
  )
)


# ============================================================
# PART B. LINEAR TABLE 2:
# SEPARATE LOW- AND HIGH-COFRADIAS SAMPLES
# ============================================================

make_add_rows <- function(models) {
  
  rows <- tibble::tibble(
    term = c(
      "Controls",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var."
    )
  )
  
  for (nm in names(models)) {
    rows[[nm]] <- c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      as.character(depvar_mean(models[[nm]]))
    )
  }
  
  rows
}

fit_separate_sample_linear_cof <- function(y, group_value) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    controls_cof
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data = model_data_cof %>% filter(high_cofradias == group_value),
    cluster = ~ prov_nac
  )
}


# Estimate separate-sample models

lpm_fe_lin_cat_low_cof  <- fit_separate_sample_linear_cof("CATHOLIC", 0)
lpm_fe_lin_cat_high_cof <- fit_separate_sample_linear_cof("CATHOLIC", 1)

lpm_fe_lin_con_low_cof  <- fit_separate_sample_linear_cof("CONSERVATIVE_VOTE", 0)
lpm_fe_lin_con_high_cof <- fit_separate_sample_linear_cof("CONSERVATIVE_VOTE", 1)


models_separate_samples_cof <- list(
  "Catholic – Low cofradias" = lpm_fe_lin_cat_low_cof,
  "Catholic – High cofradias" = lpm_fe_lin_cat_high_cof,
  "Conservative vote – Low cofradias" = lpm_fe_lin_con_low_cof,
  "Conservative vote – High cofradias" = lpm_fe_lin_con_high_cof
)


# Table 2: separate samples

modelsummary(
  models_separate_samples_cof,
  title = "Linear Specification: Separate Low- and High-Cofradias Samples",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" = "Dry days, standardized"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = make_add_rows(models_separate_samples_cof)
)


# ============================================================
# PART C. QUARTILE GRAPHS:
# ONLY CATHOLIC AND CONSERVATIVE VOTE
# Separate-sample quartile estimates, same line + ribbon style
# ============================================================

fit_split_q_cof <- function(y, data) {
  
  fml <- as.formula(
    paste0(
      y, " ~ i(treat_q, ref = 1) + ",
      controls_cof_rhs,
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  list(
    Low = feols(
      fml,
      data = data %>% filter(high_cofradias == 0),
      cluster = ~ prov_nac
    ),
    High = feols(
      fml,
      data = data %>% filter(high_cofradias == 1),
      cluster = ~ prov_nac
    )
  )
}

models_split_q_cof <- lapply(outcomes_cof, fit_split_q_cof, data = model_data_cof)
names(models_split_q_cof) <- outcomes_cof


extract_split_q_effects_cof <- function(mod, group_name, outcome_name, level = 0.95) {
  
  z <- qnorm(1 - (1 - level) / 2)
  
  td <- broom::tidy(mod) %>%
    filter(str_detect(term, "^treat_q::")) %>%
    mutate(
      treat_level = as.integer(str_remove(term, "^treat_q::")),
      group = group_name,
      outcome = outcome_name,
      estimate = estimate,
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error
    ) %>%
    select(outcome, group, treat_level, estimate, conf.low, conf.high)
  
  bind_rows(
    tibble(
      outcome = outcome_name,
      group = group_name,
      treat_level = 1L,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    ),
    td
  ) %>%
    arrange(group, treat_level)
}


coef_cof <- map_dfr(names(models_split_q_cof), function(y) {
  
  bind_rows(
    extract_split_q_effects_cof(models_split_q_cof[[y]]$Low,  "Low cofradias",  y),
    extract_split_q_effects_cof(models_split_q_cof[[y]]$High, "High cofradias", y)
  )
}) %>%
  mutate(
    outcome_label = recode(outcome, !!!outcome_labels_cof),
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels_cof[outcomes_cof])
    ),
    group = factor(group, levels = c("Low cofradias", "High cofradias"))
  )


plot_cof_separate_ribbon <- function(data,
                                     outcome_keep,
                                     title = "",
                                     subtitle = "",
                                     filename = NULL,
                                     width = 8,
                                     height = 5,
                                     dpi = 600,
                                     level = 0.95) {
  
  plot_data <- data %>%
    filter(outcome %in% outcome_keep) %>%
    mutate(
      outcome_label = factor(
        outcome_label,
        levels = unname(outcome_labels_cof[outcome_keep])
      )
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = treat_level,
      y = estimate,
      color = group,
      fill = group,
      group = group
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    facet_wrap(
      ~ outcome_label,
      scales = "free_y",
      ncol = 1
    ) +
    scale_x_continuous(
      breaks = 1:4,
      labels = paste0("Q", 1:4),
      limits = c(0.95, 4.05)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile",
      y = paste0("Estimate relative to Q1 within group (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  print(p)
  
  if (!is.null(filename)) {
    
    out_dir <- file.path(getwd(), "figures")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    pdf_path <- file.path(out_dir, paste0(filename, ".pdf"))
    png_path <- file.path(out_dir, paste0(filename, ".png"))
    
    ggsave(
      filename = pdf_path,
      plot = p,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "in",
      bg = "white"
    )
    
    ggsave(
      filename = png_path,
      plot = p,
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      bg = "white"
    )
    
    message("Saved PDF: ", pdf_path)
    message("Saved PNG: ", png_path)
  }
  
  invisible(p)
}


# ------------------------------------------------------------
# Make and save graphs: Catholic and Conservative vote only
# ------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)


# Catholic only
p_cof_catholic <- plot_cof_separate_ribbon(
  data = coef_cof,
  outcome_keep = c("CATHOLIC"),
  title = "Catholic — Low vs High cofradias separate samples",
  subtitle = "Quartile estimates from separate group regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "cofradias_separate_catholic_ribbon",
  width = 8,
  height = 4.2
)


# Conservative vote only
p_cof_conservative <- plot_cof_separate_ribbon(
  data = coef_cof,
  outcome_keep = c("CONSERVATIVE_VOTE"),
  title = "Conservative vote — Low vs High cofradias separate samples",
  subtitle = "Quartile estimates from separate group regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "cofradias_separate_conservative_vote_ribbon",
  width = 8,
  height = 4.2
)


# Combined Catholic + Conservative vote plot
p_cof_catholic_conservative <- plot_cof_separate_ribbon(
  data = coef_cof,
  outcome_keep = c("CATHOLIC", "CONSERVATIVE_VOTE"),
  title = "Catholic and Conservative vote — Low vs High cofradias separate samples",
  subtitle = "Quartile estimates from separate group regressions; FE: birth year + province + survey year; MIM controls included",
  filename = "cofradias_separate_catholic_conservative_ribbon",
  width = 8,
  height = 5.2
)


# ============================================================
# ROBUSTNESS: CONTINUOUS COFRADIAS DENSITY MODERATOR
# ============================================================

model_data_cof <- model_data_cof %>%
  mutate(
    cofradias_per_100k_std =
      as.numeric(scale(cofradias_per_100k))
  )


fit_full_interaction_linear_cof_cont <- function(y) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    "cofradias_per_100k_std",
    "childhood_total_dry_days_std:cofradias_per_100k_std",
    controls_cof
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data = model_data_cof,
    cluster = ~ prov_nac
  )
}


lpm_fe_lin_cat_interaction_cof_cont <-
  fit_full_interaction_linear_cof_cont("CATHOLIC")

lpm_fe_lin_con_interaction_cof_cont <-
  fit_full_interaction_linear_cof_cont("CONSERVATIVE_VOTE")


modelsummary(
  list(
    "Catholic" = lpm_fe_lin_cat_interaction_cof_cont,
    "Conservative vote" = lpm_fe_lin_con_interaction_cof_cont
  ),
  title = "Linear Specification: Continuous Cofradias-Density Interaction",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" = "Dry days, standardized",
    "cofradias_per_100k_std" = "Cofradias per 100k, standardized",
    "childhood_total_dry_days_std:cofradias_per_100k_std" =
      "Dry days × Cofradias density"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE"
)



# Heterogeneity: Parental Catholicism -------------------------------------
# Definition: at least one parent Catholic
# Full controls + missing-indicator method

library(dplyr)
library(fixest)
library(modelsummary)
library(broom)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)


# 0) Fallback MIM helper --------------------------------------------------

if (!exists("apply_mim_controls")) {
  apply_mim_controls <- function(data, mim_vars) {
    mim_vars <- mim_vars[mim_vars %in% names(data)]
    
    for (v in mim_vars) {
      miss_v <- paste0(v, "_MISS")
      
      if (!miss_v %in% names(data)) {
        data[[miss_v]] <- as.integer(is.na(data[[v]]))
      }
      
      data[[v]] <- ifelse(is.na(data[[v]]), 0, as.numeric(data[[v]]))
    }
    
    data
  }
}


# 1) Build model data with parental Catholicism dummy ---------------------

model_data_pc <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(BIRTH),
    !is.na(prov_nac),
    !is.na(survey_year)
  ) %>%
  mutate(
    f_cath = as.numeric(FATHER_CATHOLIC),
    m_cath = as.numeric(MOTHER_CATHOLIC),
    
    # At least one parent Catholic.
    # Keep parent_cath missing if either parental Catholic status is missing.
    parent_cath = if_else(
      !is.na(f_cath) & !is.na(m_cath),
      as.integer(f_cath == 1 | m_cath == 1),
      NA_integer_
    ),
    
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    
    childhood_total_dry_days_std = (
      childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)
    ) / sd(childhood_total_dry_days, na.rm = TRUE),
    
    treat_q = ntile(childhood_total_dry_days_std, 4),
    
    log_pop_birth = log(pop_birth_last_census)
  ) %>%
  filter(!is.na(parent_cath))


# 2) Apply full MIM controls ----------------------------------------------

mim_vars_pc <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_SCHOOL",     "MOTHER_SCHOOL",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT"
)

mim_vars_pc <- mim_vars_pc[mim_vars_pc %in% names(model_data_pc)]

model_data_pc <- apply_mim_controls(model_data_pc, mim_vars_pc)

controls_pc <- c(
  mim_vars_pc,
  paste0(mim_vars_pc, "_MISS"),
  "log_pop_birth"
)

controls_pc <- controls_pc[controls_pc %in% names(model_data_pc)]

controls_pc_rhs <- paste(controls_pc, collapse = " + ")

fe_pc_rhs <- "BIRTH + prov_nac + survey_year"


# Quick checks ------------------------------------------------------------

model_data_pc %>%
  count(parent_cath, treat_q, name = "n_q") %>%
  arrange(parent_cath, treat_q) %>%
  print()

model_data_pc %>%
  count(parent_cath, name = "n_parent_cath") %>%
  print()

model_data_pc %>%
  summarise(
    across(
      all_of(c(mim_vars_pc, paste0(mim_vars_pc, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)


# 3) Outcome choice -------------------------------------------------------

vote_outcome_pc <- if ("CONSERVATIVE_NO_FAR_RIGHT" %in% names(model_data_pc)) {
  "CONSERVATIVE_NO_FAR_RIGHT"
} else {
  "CONSERVATIVE_VOTE"
}

vote_label_pc <- if (vote_outcome_pc == "CONSERVATIVE_NO_FAR_RIGHT") {
  "Conservative excl. far-right"
} else {
  "Conservative vote"
}


# 4) Quartile heterogeneity model helper ----------------------------------

fit_pc_interaction <- function(y) {
  fml <- as.formula(
    paste0(
      y,
      " ~ factor(treat_q) * parent_cath + ",
      controls_pc_rhs,
      " | ",
      fe_pc_rhs
    )
  )
  
  feols(
    fml,
    data = model_data_pc,
    cluster = ~ prov_nac
  )
}


# 5) Estimate models ------------------------------------------------------

lpm_fe_q_controls_cat_pc <- fit_pc_interaction("CATHOLIC")
lpm_fe_q_controls_rel_pc <- fit_pc_interaction("RELIGIOUS_PRACTICE")
lpm_fe_q_controls_con_pc <- fit_pc_interaction(vote_outcome_pc)
lpm_fe_q_controls_lr_pc  <- fit_pc_interaction("LEFT_RIGHT")


# 6) Wald tests -----------------------------------------------------------
# H0: quartile profile does not differ by parental Catholicism

pc_interaction_pattern <- "factor\\(treat_q\\)[234]:parent_cath"

get_wald_p_print <- function(mod, keep_pattern) {
  out <- capture.output(w <- fixest::wald(mod, keep = keep_pattern))
  
  if (!is.null(w$p)) {
    return(round(w$p, 3))
  }
  
  p_line <- grep("p-value =", out, value = TRUE)
  
  if (length(p_line) == 0) {
    return(NA_real_)
  }
  
  round(
    as.numeric(sub(".*p-value = ([0-9.eE+-]+).*", "\\1", p_line[1])),
    3
  )
}

wald_cat_pc <- fixest::wald(lpm_fe_q_controls_cat_pc, keep = pc_interaction_pattern)
wald_rel_pc <- fixest::wald(lpm_fe_q_controls_rel_pc, keep = pc_interaction_pattern)
wald_con_pc <- fixest::wald(lpm_fe_q_controls_con_pc, keep = pc_interaction_pattern)
wald_lr_pc  <- fixest::wald(lpm_fe_q_controls_lr_pc,  keep = pc_interaction_pattern)

p_cat_pc <- get_wald_p_print(lpm_fe_q_controls_cat_pc, pc_interaction_pattern)
p_rel_pc <- get_wald_p_print(lpm_fe_q_controls_rel_pc, pc_interaction_pattern)
p_con_pc <- get_wald_p_print(lpm_fe_q_controls_con_pc, pc_interaction_pattern)
p_lr_pc  <- get_wald_p_print(lpm_fe_q_controls_lr_pc,  pc_interaction_pattern)

wald_cat_pc
wald_rel_pc
wald_con_pc
wald_lr_pc


# 7) Mean dependent variable helper ---------------------------------------

depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 3)
}


# 8) Table ----------------------------------------------------------------

models_pc <- list(
  "Catholic – Q × Parent Catholic"           = lpm_fe_q_controls_cat_pc,
  "Religious practice – Q × Parent Catholic" = lpm_fe_q_controls_rel_pc,
  "Conservative vote – Q × Parent Catholic"  = lpm_fe_q_controls_con_pc,
  "Left-right – Q × Parent Catholic"         = lpm_fe_q_controls_lr_pc
)

# If using conservative excluding far-right, rename the third model safely
names(models_pc)[3] <- paste0(vote_label_pc, " – Q × Parent Catholic")


add_rows_pc <- tibble::tibble(
  term = c(
    "Controls",
    "Missing-control indicators",
    "Mean dep. var.",
    "Wald test p-value"
  ),
  `Catholic – Q × Parent Catholic` = c(
    "Yes",
    "Yes",
    sprintf("%.3f", depvar_mean(lpm_fe_q_controls_cat_pc)),
    sprintf("%.3f", p_cat_pc)
  ),
  `Religious practice – Q × Parent Catholic` = c(
    "Yes",
    "Yes",
    sprintf("%.3f", depvar_mean(lpm_fe_q_controls_rel_pc)),
    sprintf("%.3f", p_rel_pc)
  ),
  `Conservative vote – Q × Parent Catholic` = c(
    "Yes",
    "Yes",
    sprintf("%.3f", depvar_mean(lpm_fe_q_controls_con_pc)),
    sprintf("%.3f", p_con_pc)
  ),
  `Left-right – Q × Parent Catholic` = c(
    "Yes",
    "Yes",
    sprintf("%.3f", depvar_mean(lpm_fe_q_controls_lr_pc)),
    sprintf("%.3f", p_lr_pc)
  )
)

# Rename the add_rows third column to match the model name
names(add_rows_pc)[4] <- names(models_pc)[3]


modelsummary(
  models_pc,
  title = "Heterogeneity by parental Catholicism – Quartiles",
  output = "latex",
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  coef_omit = "^(FATHER_|MOTHER_|FEMALE|log_pop_birth)",
  coef_rename = c(
    "factor(treat_q)2" = "Q2",
    "factor(treat_q)3" = "Q3",
    "factor(treat_q)4" = "Q4",
    "parent_cath" = "At least one Catholic parent",
    "factor(treat_q)2:parent_cath" = "Q2 × Catholic parent",
    "factor(treat_q)3:parent_cath" = "Q3 × Catholic parent",
    "factor(treat_q)4:parent_cath" = "Q4 × Catholic parent"
  ),
  gof_omit  = "AIC|BIC|R2 Within|R2 Within Adj.",
  add_rows = add_rows_pc,
  notes = "Controls include respondent gender, parental birthplace, parental school attendance, parental employment, log province population at birth, and missing-control indicators. All specifications include birth-year, birth-province, and survey-year fixed effects. Standard errors are clustered by birth province."
)


# 9) Plot function --------------------------------------------------------

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
  ) %>%
    as_tibble() %>%
    mutate(
      comb = pmap(list(group, bin), function(g, j) {
        
        if (g == "No Catholic parent" && j == 1) {
          return(list(terms = character(0), w = numeric(0)))
        }
        
        if (g == "No Catholic parent" && j != 1) {
          return(list(
            terms = paste0("factor(treat_q)", j),
            w = 1
          ))
        }
        
        if (g == ">=1 Catholic parent" && j == 1) {
          return(list(
            terms = "parent_cath",
            w = 1
          ))
        }
        
        list(
          terms = c(
            "parent_cath",
            paste0("factor(treat_q)", j),
            paste0("factor(treat_q)", j, ":parent_cath")
          ),
          w = c(1, 1, 1)
        )
      }),
      est_se  = map(comb, ~ lincomb(.x$terms, .x$w)),
      y       = map_dbl(est_se, "est"),
      se      = map_dbl(est_se, "se"),
      ci_low  = y - z * se,
      ci_high = y + z * se
    ) %>%
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
    scale_x_continuous(
      breaks = 1:4,
      labels = paste0("Q", 1:4)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Treatment quartile",
      y = paste0("Estimated level relative to Q1, no Catholic parent (", round(level * 100), "% CI)"),
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
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


# 10) Save plots ----------------------------------------------------------

out_dir <- "parent_catholicism_heterogeneity_quartile_plots"
dir.create(out_dir, showWarnings = FALSE)

p_q_catholic_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_cat_pc,
  title = "Catholic — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; full controls and missing indicators included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Catholic_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Catholic_ParentCath.png")
)

p_q_religious_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_rel_pc,
  title = "Religious practice — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; full controls and missing indicators included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Religious_practice_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Religious_practice_ParentCath.png")
)

p_q_conservative_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_con_pc,
  title = paste0(vote_label_pc, " — Quartile heterogeneity"),
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; full controls and missing indicators included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Conservative_vote_ParentCath.png")
)

p_q_leftright_pc <- plot_adrf_pc_ci(
  mod = lpm_fe_q_controls_lr_pc,
  title = "Left-right — Quartile heterogeneity",
  subtitle = "Baseline: Q1 among respondents with no Catholic parent; full controls and missing indicators included",
  file_pdf = file.path(out_dir, "Q_Heterogeneity_Left_right_ParentCath.pdf"),
  file_png = file.path(out_dir, "Q_Heterogeneity_Left_right_ParentCath.png")
)


# Heterogeneity by birth cohort -------------------------------------------

# ============================================================
# HETEROGENEITY: BIRTH COHORT
# Linear treatment × birth-cohort group
#
# Outcomes:
#   - CATHOLIC
#   - CONSERVATIVE_VOTE
#
# Treatment:
#   - childhood_total_dry_days_std
#
# Cohorts:
#   - 1930–1959 baseline
#   - 1960–1979
#   - 1980–2002
#
# Controls:
#   - Full MIM controls
#
# FE:
#   - BIRTH
#   - prov_nac
#   - survey_year
#
# SE:
#   - clustered by province of birth
# ============================================================


# ------------------------------------------------------------
# 0) Fallback MIM helper, only if not already defined
# ------------------------------------------------------------

if (!exists("apply_mim_controls")) {
  apply_mim_controls <- function(data, mim_vars) {
    
    mim_vars <- mim_vars[mim_vars %in% names(data)]
    
    for (v in mim_vars) {
      miss_v <- paste0(v, "_MISS")
      
      if (!miss_v %in% names(data)) {
        data[[miss_v]] <- as.integer(is.na(data[[v]]))
      }
      
      data[[v]] <- ifelse(is.na(data[[v]]), 0, as.numeric(data[[v]]))
    }
    
    data
  }
}


# ------------------------------------------------------------
# 1) Build model data
# ------------------------------------------------------------

model_data_cohort <- survey %>%
  filter(
    BORN_SPAIN == 1,
    !is.na(childhood_total_dry_days),
    childhood_total_dry_days != 0,
    !is.na(BIRTH),
    !is.na(prov_nac),
    !is.na(survey_year),
    !is.na(pop_birth_last_census),
    pop_birth_last_census > 0
  ) %>%
  mutate(
    birth_cohort = case_when(
      BIRTH >= 1930 & BIRTH <= 1959 ~ "1930–1959",
      BIRTH >= 1960 & BIRTH <= 1979 ~ "1960–1979",
      BIRTH >= 1980 & BIRTH <= 2002 ~ "1980–2002",
      TRUE ~ NA_character_
    ),
    birth_cohort = factor(
      birth_cohort,
      levels = c("1930–1959", "1960–1979", "1980–2002")
    ),
    
    # Cohort dummies.
    # The omitted category is 1930–1959.
    cohort_1960_1979 = as.integer(birth_cohort == "1960–1979"),
    cohort_1980_2002 = as.integer(birth_cohort == "1980–2002"),
    
    year = BIRTH,
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    log_pop_birth = log(pop_birth_last_census),
    
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE)
  ) %>%
  filter(!is.na(birth_cohort))


# ------------------------------------------------------------
# 2) MIM controls
# ------------------------------------------------------------

mim_vars_cohort <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_cohort <- mim_vars_cohort[
  mim_vars_cohort %in% names(model_data_cohort)
]

model_data_cohort <- apply_mim_controls(
  model_data_cohort,
  mim_vars_cohort
)

controls_cohort <- c(
  mim_vars_cohort,
  paste0(mim_vars_cohort, "_MISS"),
  "log_pop_birth"
)

controls_cohort <- controls_cohort[
  controls_cohort %in% names(model_data_cohort)
]

controls_cohort_rhs <- paste(controls_cohort, collapse = " + ")


# ------------------------------------------------------------
# 3) Quick checks
# ------------------------------------------------------------

# MIM sanity check: all MIM controls and missing dummies should have zero NA values
model_data_cohort %>%
  summarise(
    across(
      all_of(c(mim_vars_cohort, paste0(mim_vars_cohort, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)


# Sample size by cohort
model_data_cohort %>%
  count(birth_cohort, name = "n") %>%
  print()


# Outcome sample sizes by cohort
model_data_cohort %>%
  group_by(birth_cohort) %>%
  summarise(
    n_catholic = sum(!is.na(CATHOLIC)),
    n_conservative = sum(!is.na(CONSERVATIVE_VOTE)),
    .groups = "drop"
  ) %>%
  print()


# ------------------------------------------------------------
# 4) Helper functions
# ------------------------------------------------------------

depvar_mean <- function(mod) {
  round(mean(fitted(mod) + resid(mod), na.rm = TRUE), 3)
}


# Robust Wald p-value extractor
get_wald_p <- function(wald_obj) {
  
  # Case 1: list with element p
  if (is.list(wald_obj) && "p" %in% names(wald_obj)) {
    return(as.numeric(wald_obj$p))
  }
  
  # Case 2: named atomic vector
  if (is.atomic(wald_obj) && !is.null(names(wald_obj))) {
    p_name <- grep("^p$|p.value|p-value|Pr", names(wald_obj), value = TRUE)
    if (length(p_name) > 0) {
      return(as.numeric(wald_obj[p_name[1]]))
    }
  }
  
  # Case 3: printed output fallback
  out <- capture.output(print(wald_obj))
  p_line <- grep("p-value", out, value = TRUE)
  
  if (length(p_line) > 0) {
    return(
      as.numeric(
        sub(".*p-value = ([0-9.eE+-]+).*", "\\1", p_line[1])
      )
    )
  }
  
  return(NA_real_)
}


# ------------------------------------------------------------
# 5) Linear interaction model
# ------------------------------------------------------------

fit_cohort_interaction <- function(y) {
  
  rhs_terms <- c(
    "childhood_total_dry_days_std",
    "childhood_total_dry_days_std:cohort_1960_1979",
    "childhood_total_dry_days_std:cohort_1980_2002",
    controls_cohort
  )
  
  rhs_terms <- rhs_terms[rhs_terms != ""]
  
  fml <- as.formula(
    paste0(
      y, " ~ ",
      paste(rhs_terms, collapse = " + "),
      " | BIRTH + prov_nac + survey_year"
    )
  )
  
  feols(
    fml,
    data = model_data_cohort,
    cluster = ~ prov_nac
  )
}


# ------------------------------------------------------------
# 6) Estimate Catholic and Conservative vote models
# ------------------------------------------------------------

lpm_fe_lin_cat_cohort <- fit_cohort_interaction("CATHOLIC")

lpm_fe_lin_con_cohort <- fit_cohort_interaction("CONSERVATIVE_VOTE")


# ------------------------------------------------------------
# 7) Wald tests:
# H0: cohort interactions are jointly zero
# ------------------------------------------------------------

# Robust manual Wald test for a set of coefficients
# Uses the model's clustered vcov, so it matches your inference structure.
joint_wald_p <- function(mod, term_patterns) {
  
  b_all <- coef(mod)
  V_all <- vcov(mod)
  
  terms <- unique(unlist(
    lapply(term_patterns, function(pat) {
      grep(pat, names(b_all), value = TRUE)
    })
  ))
  
  if (length(terms) == 0) {
    warning("No matching terms found for Wald test.")
    return(NA_real_)
  }
  
  b <- b_all[terms]
  V <- V_all[terms, terms, drop = FALSE]
  
  # Wald statistic: b' V^{-1} b
  W <- tryCatch(
    as.numeric(t(b) %*% solve(V, b)),
    error = function(e) {
      as.numeric(t(b) %*% MASS::ginv(V) %*% b)
    }
  )
  
  df <- length(terms)
  p_value <- pchisq(W, df = df, lower.tail = FALSE)
  
  tibble::tibble(
    statistic = W,
    df = df,
    p_value = p_value,
    terms = paste(terms, collapse = "; ")
  )
}


# Joint Wald tests for Catholic and Conservative vote
# H0: Dry days × Born 1960–1979 = 0 AND Dry days × Born 1980–2002 = 0

wald_cat_cohort <- joint_wald_p(
  lpm_fe_lin_cat_cohort,
  term_patterns = c(
    "childhood_total_dry_days_std:cohort_1960_1979|cohort_1960_1979:childhood_total_dry_days_std",
    "childhood_total_dry_days_std:cohort_1980_2002|cohort_1980_2002:childhood_total_dry_days_std"
  )
)

wald_con_cohort <- joint_wald_p(
  lpm_fe_lin_con_cohort,
  term_patterns = c(
    "childhood_total_dry_days_std:cohort_1960_1979|cohort_1960_1979:childhood_total_dry_days_std",
    "childhood_total_dry_days_std:cohort_1980_2002|cohort_1980_2002:childhood_total_dry_days_std"
  )
)

wald_cat_cohort
wald_con_cohort

p_cat_cohort <- round(wald_cat_cohort$p_value, 3)
p_con_cohort <- round(wald_con_cohort$p_value, 3)

p_cat_cohort
p_con_cohort

# ------------------------------------------------------------
# 8) Main table with Wald p-values
# ------------------------------------------------------------

modelsummary(
  list(
    "Catholic" = lpm_fe_lin_cat_cohort,
    "Conservative vote" = lpm_fe_lin_con_cohort
  ),
  title = "Linear Specification: Heterogeneity by Birth Cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" =
      "Dry days, standardized",
    "childhood_total_dry_days_std:cohort_1960_1979" =
      "Dry days × Born 1960–1979",
    "cohort_1960_1979:childhood_total_dry_days_std" =
      "Dry days × Born 1960–1979",
    "childhood_total_dry_days_std:cohort_1980_2002" =
      "Dry days × Born 1980–2002",
    "cohort_1980_2002:childhood_total_dry_days_std" =
      "Dry days × Born 1980–2002"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = tibble::tibble(
    term = c(
      "Controls",
      "Missing-control indicators",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var.",
      "Joint Wald p-value: cohort interactions = 0"
    ),
    `Catholic` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      sprintf("%.3f", depvar_mean(lpm_fe_lin_cat_cohort)),
      sprintf("%.3f", p_cat_cohort)
    ),
    `Conservative vote` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      sprintf("%.3f", depvar_mean(lpm_fe_lin_con_cohort)),
      sprintf("%.3f", p_con_cohort)
    )
  ),
  notes = "The omitted cohort is 1930–1959. The Wald test jointly tests whether the two treatment-by-cohort interactions are equal to zero. Controls include respondent gender, parental birthplace, parental school attendance, parental employment, log province population at birth, and missing-control indicators. All specifications include birth-year, birth-province, and survey-year fixed effects. Standard errors are clustered by birth province."
)

# ============================================================
# 9) IMPLIED TREATMENT EFFECT BY COHORT
# ============================================================

extract_cohort_slopes <- function(mod, outcome_label) {
  
  b <- coef(mod)
  V <- vcov(mod)
  
  base <- "childhood_total_dry_days_std"
  int_60 <- "childhood_total_dry_days_std:cohort_1960_1979"
  int_80 <- "childhood_total_dry_days_std:cohort_1980_2002"
  
  slope_and_se <- function(terms) {
    
    terms <- terms[terms %in% names(b)]
    
    est <- sum(b[terms])
    
    Vsub <- V[terms, terms, drop = FALSE]
    w <- rep(1, length(terms))
    
    se <- sqrt(as.numeric(t(w) %*% Vsub %*% w))
    
    tibble::tibble(
      estimate = est,
      se = se
    )
  }
  
  bind_rows(
    slope_and_se(c(base)) %>%
      mutate(birth_cohort = "1930–1959"),
    
    slope_and_se(c(base, int_60)) %>%
      mutate(birth_cohort = "1960–1979"),
    
    slope_and_se(c(base, int_80)) %>%
      mutate(birth_cohort = "1980–2002")
  ) %>%
    mutate(
      outcome = outcome_label,
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )
}


cohort_slopes <- bind_rows(
  extract_cohort_slopes(lpm_fe_lin_cat_cohort, "Catholic"),
  extract_cohort_slopes(lpm_fe_lin_con_cohort, "Conservative vote")
) %>%
  mutate(
    birth_cohort = factor(
      birth_cohort,
      levels = c("1930–1959", "1960–1979", "1980–2002")
    )
  )


# Print implied slopes
cohort_slopes %>%
  mutate(
    estimate = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3)
  ) %>%
  print(n = Inf)


# ------------------------------------------------------------
# 10) Plot implied slopes
# ------------------------------------------------------------

p_cohort_slopes <- ggplot(
  cohort_slopes,
  aes(x = birth_cohort, y = estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(ymin = ci_low, ymax = ci_high),
    linewidth = 0.7
  ) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    x = "Birth cohort",
    y = "Effect of one SD increase in childhood dry days",
    title = "Treatment effect by birth cohort"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p_cohort_slopes)


# Optional save
out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

ggsave(
  filename = file.path(out_dir, "birth_cohort_heterogeneity_linear.pdf"),
  plot = p_cohort_slopes,
  device = cairo_pdf,
  width = 8,
  height = 4.5,
  units = "in"
)

ggsave(
  filename = file.path(out_dir, "birth_cohort_heterogeneity_linear.png"),
  plot = p_cohort_slopes,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# 8) Cell sizes by cohort and outcome
# ------------------------------------------------------------

get_cohort_n <- function(data, y) {
  
  data %>%
    filter(!is.na(.data[[y]])) %>%
    count(birth_cohort, name = "n") %>%
    mutate(birth_cohort = as.character(birth_cohort)) %>%
    tidyr::pivot_wider(
      names_from = birth_cohort,
      values_from = n,
      values_fill = 0
    )
}

n_cat_cohort <- get_cohort_n(model_data_cohort, "CATHOLIC")
n_con_cohort <- get_cohort_n(model_data_cohort, "CONSERVATIVE_VOTE")

n_cat_1930 <- n_cat_cohort[["1930–1959"]]
n_cat_1960 <- n_cat_cohort[["1960–1979"]]
n_cat_1980 <- n_cat_cohort[["1980–2002"]]

n_con_1930 <- n_con_cohort[["1930–1959"]]
n_con_1960 <- n_con_cohort[["1960–1979"]]
n_con_1980 <- n_con_cohort[["1980–2002"]]


# ------------------------------------------------------------
# 9) Main table with Wald p-values and cohort cell sizes
# ------------------------------------------------------------

modelsummary(
  list(
    "Catholic" = lpm_fe_lin_cat_cohort,
    "Conservative vote" = lpm_fe_lin_con_cohort
  ),
  title = "Linear Specification: Heterogeneity by Birth Cohort",
  output = "latex",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "childhood_total_dry_days_std" =
      "Dry days, standardized",
    "childhood_total_dry_days_std:cohort_1960_1979" =
      "Dry days × Born 1960–1979",
    "cohort_1960_1979:childhood_total_dry_days_std" =
      "Dry days × Born 1960–1979",
    "childhood_total_dry_days_std:cohort_1980_2002" =
      "Dry days × Born 1980–2002",
    "cohort_1980_2002:childhood_total_dry_days_std" =
      "Dry days × Born 1980–2002"
  ),
  gof_omit = "AIC|BIC|R2 Within|R2 Within Adj.|RMSE",
  add_rows = tibble::tibble(
    term = c(
      "Controls",
      "Missing-control indicators",
      "Birth-year FE",
      "Province FE",
      "Survey-year FE",
      "Mean dep. var.",
      "N: born 1930–1959",
      "N: born 1960–1979",
      "N: born 1980–2002",
      "Joint Wald p-value: cohort interactions = 0"
    ),
    `Catholic` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      sprintf("%.3f", depvar_mean(lpm_fe_lin_cat_cohort)),
      format(n_cat_1930, big.mark = ","),
      format(n_cat_1960, big.mark = ","),
      format(n_cat_1980, big.mark = ","),
      sprintf("%.3f", p_cat_cohort)
    ),
    `Conservative vote` = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      sprintf("%.3f", depvar_mean(lpm_fe_lin_con_cohort)),
      format(n_con_1930, big.mark = ","),
      format(n_con_1960, big.mark = ","),
      format(n_con_1980, big.mark = ","),
      sprintf("%.3f", p_con_cohort)
    )
  ),
  notes = "The omitted cohort is 1930–1959. The Wald test jointly tests whether the two treatment-by-cohort interactions are equal to zero. Cohort cell sizes are computed separately for each outcome sample. Controls include respondent gender, parental birthplace, parental school attendance, parental employment, log province population at birth, and missing-control indicators. All specifications include birth-year, birth-province, and survey-year fixed effects. Standard errors are clustered by birth province."
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

# 0) Load data + build model_data with MIM controls ------------------------

prov_map <- prov_code_map %>%
  dplyr::select(prov_nac, provincia_norm) %>%
  dplyr::mutate(prov_nac = as.integer(prov_nac)) %>%
  dplyr::distinct(prov_nac, .keep_all = TRUE)

stopifnot(all(c("prov_nac", "provincia_norm") %in% names(prov_map)))

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
  ) %>%
  mutate(
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE)
  ) %>%
  dplyr::select(
    respondent_id, provincia_norm, childhood_start, childhood_end,
    CATHOLIC, childhood_total_dry_days, childhood_total_dry_days_std,
    survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL,
    FATHER_EMPLOYMENT,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL,
    MOTHER_EMPLOYMENT,
    birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT,
    CONSERVATIVE_VOTE, TRUST_PEOPLE, INCOME, EDUCATION,
    RELIGIOUS_PRACTICE, PARTICIPATION,
    pop_birth_last_census, log_pop_birth
  )

# MIM controls for Madestam placebo regressions
mim_vars_madestam <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_madestam <- mim_vars_madestam[
  mim_vars_madestam %in% names(model_data)
]

model_data <- apply_mim_controls(model_data, mim_vars_madestam)

controls_madestam <- c(
  mim_vars_madestam,
  paste0(mim_vars_madestam, "_MISS"),
  "log_pop_birth"
)

controls_madestam <- controls_madestam[
  controls_madestam %in% names(model_data)
]

controls_madestam_str <- paste(controls_madestam, collapse = " + ")

# Sanity check
model_data %>%
  summarise(
    across(
      all_of(c(mim_vars_madestam, paste0(mim_vars_madestam, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)

stopifnot("provincia_norm" %in% names(model_data))
print(table(is.na(model_data$provincia_norm)))

# C) MADESTAM GRAPHS (paper-style, WIDE 3-panels)
#   - 3 sets × 3 outcomes (same outcomes as main regressions)
#   - one dot per placebo shift; vertical line = observed estimate
#   - DOT COLOR (both effects + p-values): "#1f77b4"
#   - LINEAR spec  : effect = β1, p = p-value on β1
#   - QUADRATIC spec: effect = β1+β2 (1-SD combo), p = p-value on combo (delta method)
#   - Standardization uses mean/sd from the *estimation sample* (complete cases on RHS),
#     used for BOTH observed + placebo
#   - Subtitle reports BOTH placebo shares:
#       p1 = share of placebos strictly higher than observed effect (signed)
#       p2 = share of placebos with strictly larger absolute effect than observed
#   - WIDE STYLE: title = outcome; y-axis only on left panel

library(readr)
library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)

DOT_COL <- "#1f77b4"


# 0) Load data + build model_data with MIM controls ------------------------

survey <- read_csv("survey_with_childhood_weather_harmonized.csv")

prov_map <- prov_code_map %>%
  dplyr::select(prov_nac, provincia_norm) %>%
  dplyr::mutate(prov_nac = as.integer(prov_nac)) %>%
  dplyr::distinct(prov_nac, .keep_all = TRUE)

stopifnot(all(c("prov_nac", "provincia_norm") %in% names(prov_map)))

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
  ) %>%
  mutate(
    birth_prov_cluster = interaction(BIRTH, prov_nac),
    childhood_total_dry_days_std =
      (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) /
      sd(childhood_total_dry_days, na.rm = TRUE)
  ) %>%
  dplyr::select(
    respondent_id, provincia_norm, childhood_start, childhood_end,
    CATHOLIC, childhood_total_dry_days, childhood_total_dry_days_std,
    survey_year, FEMALE, age, BIRTH, prov_nac,
    FATHER_BORN_SPAIN, FATHER_SCHOOL,
    FATHER_EMPLOYMENT,
    MOTHER_BORN_SPAIN, MOTHER_SCHOOL,
    MOTHER_EMPLOYMENT,
    birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT,
    CONSERVATIVE_VOTE, TRUST_PEOPLE, INCOME, EDUCATION,
    RELIGIOUS_PRACTICE, PARTICIPATION,
    pop_birth_last_census, log_pop_birth
  )

# MIM controls for Madestam placebo regressions
mim_vars_madestam <- c(
  "FEMALE",
  "FATHER_BORN_SPAIN", "MOTHER_BORN_SPAIN",
  "FATHER_EMPLOYMENT", "MOTHER_EMPLOYMENT",
  "FATHER_SCHOOL", "MOTHER_SCHOOL"
)

mim_vars_madestam <- mim_vars_madestam[
  mim_vars_madestam %in% names(model_data)
]

model_data <- apply_mim_controls(model_data, mim_vars_madestam)

controls_madestam <- c(
  mim_vars_madestam,
  paste0(mim_vars_madestam, "_MISS"),
  "log_pop_birth"
)

controls_madestam <- controls_madestam[
  controls_madestam %in% names(model_data)
]

controls_madestam_str <- paste(controls_madestam, collapse = " + ")

# Sanity check
model_data %>%
  summarise(
    across(
      all_of(c(mim_vars_madestam, paste0(mim_vars_madestam, "_MISS"))),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = Inf)

stopifnot("provincia_norm" %in% names(model_data))
print(table(is.na(model_data$provincia_norm)))


# Helpers: coef lookup + combo β1+β2 (delta method)

find_coef_name <- function(bnames, patterns) {
  for (p in patterns) {
    idx <- which(grepl(p, bnames))
    if (length(idx) > 0) return(bnames[idx[1]])
  }
  NA_character_
}

combo_from_model <- function(m, lin_patterns, quad_patterns) {
  b  <- coef(m); V <- vcov(m); bn <- names(b)
  lin_name  <- find_coef_name(bn, lin_patterns)
  quad_name <- find_coef_name(bn, quad_patterns)
  if (is.na(lin_name))  stop("Could not find linear term. Coef names:\n",  paste(bn, collapse = ", "))
  if (is.na(quad_name)) stop("Could not find quadratic term. Coef names:\n", paste(bn, collapse = ", "))
  
  beta_lin  <- unname(b[lin_name])
  beta_quad <- unname(b[quad_name])
  v11 <- V[lin_name, lin_name]; v22 <- V[quad_name, quad_name]; v12 <- V[lin_name, quad_name]
  
  beta_combo <- beta_lin + beta_quad
  se_combo   <- sqrt(as.numeric(v11 + v22 + 2 * v12))
  t_combo    <- beta_combo / se_combo
  p_combo    <- 2 * pt(abs(t_combo), df = df.residual(m), lower.tail = FALSE)
  list(beta_combo = beta_combo, se_combo = se_combo, p_combo = p_combo)
}

# Linear-only extractor (just β1 and its p-value)
linear_from_model <- function(m, lin_patterns) {
  bn <- names(coef(m))
  lin_name <- find_coef_name(bn, lin_patterns)
  if (is.na(lin_name)) stop("Could not find linear term. Coef names:\n", paste(bn, collapse = ", "))
  ct <- summary(m)$coeftable
  list(
    beta_combo = unname(ct[lin_name, "Estimate"]),
    se_combo   = unname(ct[lin_name, "Std. Error"]),
    p_combo    = unname(ct[lin_name, "Pr(>|t|)"])
  )
}


# Wide-friendly plotting style

madestam_theme_wide <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title    = element_text(face = "bold", size = 11, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 9, margin = margin(b = 6)),
      axis.title.x  = element_text(size = 9, margin = margin(t = 6)),
      axis.title.y  = element_text(size = 9, margin = margin(r = 6)),
      axis.text     = element_text(size = 8),
      plot.margin   = margin(6, 6, 6, 6)
    )
}

strip_y <- function(p) {
  p + theme(axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
}


# C1) EFFECTS GRAPH: ranked placebo effects
#     `spec` controls linear vs quadratic.

run_madestam_rankplot_combo <- function(
    outcome_var,
    model_data,
    placebo_path,
    normalize_name,
    name_map,
    spec        = c("quadratic", "linear"),
    raw_expo_var = "childhood_total_dry_days",
    fe_birth    = "BIRTH",
    fe_prov     = "prov_nac",
    fe_survey   = "survey_year",
    cluster_var = "prov_nac"
) {
  spec <- match.arg(spec)
  
  req <- unique(c(
    "respondent_id", "provincia_norm", "childhood_start", "childhood_end",
    outcome_var, raw_expo_var, fe_birth, fe_prov, fe_survey,
    controls_madestam
  ))
  miss <- setdiff(req, names(model_data))
  if (length(miss) > 0) stop("model_data missing: ", paste(miss, collapse = ", "))
  
  md <- as.data.table(copy(model_data))
  cluster_fml <- as.formula(paste0("~", cluster_var))
  fe_rhs <- paste(c(fe_birth, fe_prov, fe_survey), collapse = " + ")
  
  base_dt <- md[complete.cases(md[, ..req]), ..req]
  setkey(base_dt, respondent_id)
  
  obs_raw_mean <- mean(base_dt[[raw_expo_var]], na.rm = TRUE)
  obs_raw_sd   <- sd(  base_dt[[raw_expo_var]], na.rm = TRUE)
  stopifnot(is.finite(obs_raw_sd) && obs_raw_sd > 0)
  
  # Placebo file prep
  placebo_raw <- read_csv(placebo_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  stopifnot(all(c("provincia", "year", "placebo_dry_days_10", "shift_days") %in% names(placebo_raw)))
  placebo_dt <- as.data.table(placebo_raw)
  placebo_dt[, key := normalize_name(provincia)]
  placebo_dt <- merge(placebo_dt, as.data.table(name_map), by = "key", all.x = TRUE)
  placebo_dt[, provincia_official := fifelse(is.na(ine_name), provincia, ine_name)]
  placebo_dt[, provincia_norm := normalize_name(provincia_official)]
  placebo_dt[, `:=`(
    year = as.integer(year),
    shift_days = as.integer(shift_days),
    placebo_dry_days_10 = as.numeric(placebo_dry_days_10)
  )]
  
  placebo_yearly <- placebo_dt[,
                               .(placebo_year_total = sum(placebo_dry_days_10, na.rm = TRUE)),
                               by = .(provincia_norm, year, shift_days)
  ]
  setkey(placebo_yearly, provincia_norm, year)
  shift_values <- sort(unique(placebo_yearly$shift_days))
  
  child_panel <- base_dt[,
                         .(year = seq.int(childhood_start, childhood_end)),
                         by = .(respondent_id, provincia_norm)
  ]
  setkey(child_panel, provincia_norm, year)
  
  tmp <- placebo_yearly[child_panel, on = .(provincia_norm, year),
                        allow.cartesian = TRUE, nomatch = 0L]
  expo_dt <- tmp[, .(placebo_childhood_total = sum(placebo_year_total, na.rm = TRUE)),
                 by = .(respondent_id, shift_days)]
  
  full_grid <- CJ(respondent_id = unique(base_dt$respondent_id),
                  shift_days    = shift_values, unique = TRUE)
  reg_dt <- merge(full_grid, expo_dt, by = c("respondent_id", "shift_days"), all.x = TRUE)
  reg_dt[is.na(placebo_childhood_total), placebo_childhood_total := 0]
  reg_dt <- merge(reg_dt, base_dt, by = "respondent_id", all.x = TRUE)
  reg_dt[, treat_std_obs := (get(raw_expo_var) - obs_raw_mean) / obs_raw_sd]
  
  # Formulae: linear or quadratic
  controls_str <- controls_madestam_str
  rhs_obs <- if (spec == "quadratic") "treat_std_obs + I(treat_std_obs^2)" else "treat_std_obs"
  rhs_pl  <- if (spec == "quadratic") "placebo_std   + I(placebo_std^2)"   else "placebo_std"
  fml_obs <- as.formula(paste0(outcome_var, " ~ ", rhs_obs, " + ", controls_str, " | ", fe_rhs))
  fml_pl  <- as.formula(paste0(outcome_var, " ~ ", rhs_pl,  " + ", controls_str, " | ", fe_rhs))
  
  # Observed regression
  obs_slice <- reg_dt[shift_days == shift_values[1]]
  obs_m <- feols(fml_obs, data = obs_slice, cluster = cluster_fml)
  obs_combo <- if (spec == "quadratic") {
    combo_from_model(obs_m,
                     lin_patterns  = "^treat_std_obs$",
                     quad_patterns = c("^I\\(treat_std_obs\\^2\\)$", "treat_std_obs\\^2"))
  } else {
    linear_from_model(obs_m, "^treat_std_obs$")
  }
  
  # Placebo regressions
  estimate_shift <- function(s) {
    df_s <- reg_dt[shift_days == s]
    df_s[, placebo_std := (placebo_childhood_total - obs_raw_mean) / obs_raw_sd]
    m <- feols(fml_pl, data = df_s, cluster = cluster_fml)
    cmb <- if (spec == "quadratic") {
      combo_from_model(m,
                       lin_patterns  = "^placebo_std$",
                       quad_patterns = c("^I\\(placebo_std\\^2\\)$", "placebo_std\\^2"))
    } else {
      linear_from_model(m, "^placebo_std$")
    }
    data.table(shift_days = s, beta_combo = cmb$beta_combo,
               p_combo = cmb$p_combo, n = nobs(m))
  }
  placebo_res <- rbindlist(lapply(shift_values, estimate_shift), fill = TRUE)
  
  # BOTH shares
  p1_signed <- mean(placebo_res$beta_combo  >  obs_combo$beta_combo,    na.rm = TRUE)
  p2_abs    <- mean(abs(placebo_res$beta_combo) > abs(obs_combo$beta_combo), na.rm = TRUE)
  
  rank_df <- placebo_res[!is.na(beta_combo)][order(beta_combo)]
  rank_df[, rank := seq_len(.N)]
  
  x_lab <- if (spec == "quadratic") "Average standardized effect (β1 + β2)" else "Standardized effect (β1)"
  
  p_rank <- ggplot(rank_df, aes(x = beta_combo, y = rank)) +
    geom_point(size = 1.35, alpha = 0.85, color = DOT_COL) +
    geom_vline(xintercept = obs_combo$beta_combo, linewidth = 1.05) +
    labs(
      title = outcome_var,
      subtitle = sprintf("p1 (signed > obs): %.3f   |   p2 (|·| > |obs|): %.3f", p1_signed, p2_abs),
      x = x_lab, y = "Placebo rank"
    ) +
    madestam_theme_wide()
  
  list(
    placebo_res = placebo_res,
    obs_model   = obs_m,
    obs_combo   = obs_combo,
    p1_signed   = p1_signed,
    p2_abs      = p2_abs,
    plot_rank   = p_rank
  )
}


# C2) P-VALUES GRAPH: ranked placebo p-values

run_madestam_rankplot_pvals <- function(
    outcome_var,
    model_data,
    placebo_path,
    normalize_name,
    name_map,
    spec        = c("quadratic", "linear"),
    raw_expo_var = "childhood_total_dry_days",
    fe_birth    = "BIRTH",
    fe_prov     = "prov_nac",
    fe_survey   = "survey_year",
    cluster_var = "prov_nac"
) {
  spec <- match.arg(spec)
  
  # Reuse the effects function: it already returns placebo_res with both beta and p
  out <- run_madestam_rankplot_combo(
    outcome_var, model_data, placebo_path, normalize_name, name_map,
    spec = spec, raw_expo_var = raw_expo_var,
    fe_birth = fe_birth, fe_prov = fe_prov, fe_survey = fe_survey,
    cluster_var = cluster_var
  )
  
  placebo_res <- out$placebo_res
  obs_p       <- out$obs_combo$p_combo
  
  rank_df <- placebo_res[!is.na(p_combo)][order(p_combo)]
  rank_df[, rank := seq_len(.N)]
  
  x_lab <- if (spec == "quadratic") "p-value for combo effect (β1 + β2)" else "p-value for β1"
  
  p_rank_p <- ggplot(rank_df, aes(x = p_combo, y = rank)) +
    geom_point(size = 1.35, alpha = 0.85, color = DOT_COL) +
    geom_vline(xintercept = obs_p, linewidth = 1.05) +
    labs(
      title = outcome_var,
      subtitle = sprintf("p1 (signed > obs): %.3f   |   p2 (|·| > |obs|): %.3f", out$p1_signed, out$p2_abs),
      x = x_lab, y = "Placebo rank"
    ) +
    madestam_theme_wide()
  
  list(
    placebo_res = placebo_res,
    obs_model   = out$obs_model,
    obs_p       = obs_p,
    obs_combo   = out$obs_combo,
    p1_signed   = out$p1_signed,
    p2_abs      = out$p2_abs,
    plot_pvals  = p_rank_p
  )
}


# Build 3 sets × 3 outcomes (same as your main regressions)

set1 <- c("CATHOLIC", "RELIGIOUS_PRACTICE", "COUPLE_CATHOLIC")
set2 <- c("PARTICIPATION", "CONSERVATIVE_VOTE", "LEFT_RIGHT")
set3 <- c("EDUCATION", "INCOME", "TRUST_PEOPLE")

run_set_effects <- function(outcomes, spec) {
  lapply(outcomes, function(y) {
    run_madestam_rankplot_combo(
      outcome_var    = y,
      model_data     = model_data,
      placebo_path   = placebo_path,
      normalize_name = normalize_name,
      name_map       = name_map,
      spec           = spec
    )
  })
}

run_set_pvals <- function(outcomes, spec) {
  lapply(outcomes, function(y) {
    run_madestam_rankplot_pvals(
      outcome_var    = y,
      model_data     = model_data,
      placebo_path   = placebo_path,
      normalize_name = normalize_name,
      name_map       = name_map,
      spec           = spec
    )
  })
}

# --- run for BOTH specs
out_set1_q  <- run_set_effects(set1, "quadratic")
out_set2_q  <- run_set_effects(set2, "quadratic")
out_set3_q  <- run_set_effects(set3, "quadratic")
outp_set1_q <- run_set_pvals(set1, "quadratic")
outp_set2_q <- run_set_pvals(set2, "quadratic")
outp_set3_q <- run_set_pvals(set3, "quadratic")

out_set1_l  <- run_set_effects(set1, "linear")
out_set2_l  <- run_set_effects(set2, "linear")
out_set3_l  <- run_set_effects(set3, "linear")
outp_set1_l <- run_set_pvals(set1, "linear")
outp_set2_l <- run_set_pvals(set2, "linear")
outp_set3_l <- run_set_pvals(set3, "linear")


# Save individual PNGs

save_one <- function(out_list, names_vec, prefix, which_plot = c("plot_rank", "plot_pvals")) {
  which_plot <- match.arg(which_plot)
  for (i in seq_along(out_list)) {
    ggsave(
      filename = paste0(prefix, "_", names_vec[i], ".png"),
      plot = out_list[[i]][[which_plot]],
      width = 7.2, height = 4.2, dpi = 300
    )
  }
}

# quadratic
save_one(out_set1_q,  set1, "madestam_effects_quad_set1", "plot_rank")
save_one(out_set2_q,  set2, "madestam_effects_quad_set2", "plot_rank")
save_one(out_set3_q,  set3, "madestam_effects_quad_set3", "plot_rank")
save_one(outp_set1_q, set1, "madestam_pvals_quad_set1",   "plot_pvals")
save_one(outp_set2_q, set2, "madestam_pvals_quad_set2",   "plot_pvals")
save_one(outp_set3_q, set3, "madestam_pvals_quad_set3",   "plot_pvals")

# linear
save_one(out_set1_l,  set1, "madestam_effects_lin_set1", "plot_rank")
save_one(out_set2_l,  set2, "madestam_effects_lin_set2", "plot_rank")
save_one(out_set3_l,  set3, "madestam_effects_lin_set3", "plot_rank")
save_one(outp_set1_l, set1, "madestam_pvals_lin_set1",   "plot_pvals")
save_one(outp_set2_l, set2, "madestam_pvals_lin_set2",   "plot_pvals")
save_one(outp_set3_l, set3, "madestam_pvals_lin_set3",   "plot_pvals")


# Combine into 3-panel figures (HORIZONTAL: 1 row × 3 columns)
#   - keep y-axis only in left panel

make_3panel <- function(plot_list, which_plot) {
  plot_list[[1]][[which_plot]] +
    strip_y(plot_list[[2]][[which_plot]]) +
    strip_y(plot_list[[3]][[which_plot]]) +
    plot_layout(ncol = 3)
}

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  
  # quadratic
  ggsave("madestam_3panel_effects_quad_set1.png", make_3panel(out_set1_q,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_quad_set2.png", make_3panel(out_set2_q,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_quad_set3.png", make_3panel(out_set3_q,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_quad_set1.png",   make_3panel(outp_set1_q, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_quad_set2.png",   make_3panel(outp_set2_q, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_quad_set3.png",   make_3panel(outp_set3_q, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
  
  # linear
  ggsave("madestam_3panel_effects_lin_set1.png",  make_3panel(out_set1_l,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_lin_set2.png",  make_3panel(out_set2_l,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_effects_lin_set3.png",  make_3panel(out_set3_l,  "plot_rank"),  width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_lin_set1.png",    make_3panel(outp_set1_l, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_lin_set2.png",    make_3panel(outp_set2_l, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
  ggsave("madestam_3panel_pvals_lin_set3.png",    make_3panel(outp_set3_l, "plot_pvals"), width = 12.8, height = 4.2, dpi = 300)
} else {
  message("Install 'patchwork' to create 3-panel figures.")
}
