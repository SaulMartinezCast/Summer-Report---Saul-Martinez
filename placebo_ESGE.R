

# Survey data -------------------------------------------------------------
# Load libraries
library(haven)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(writexl)

# Clean environment
rm(list = ls())

# Load datasets
ESGE_2013 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2013.csv") %>%
  mutate(survey_year = 2013)
ESGE_2015 <- read_csv("C:/Users/Saúl/Documents/holy_week_data/ESGE_2015.csv") %>%
  mutate(survey_year = 2015)
ESGE_2017 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2017.sav") %>%
  mutate(survey_year = 2017)
ESGE_2023 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2023.sav") %>%
  mutate(survey_year = 2023)
ESGE_2024 <- read_sav("C:/Users/Saúl/Documents/holy_week_data/ESGE_2024.sav") %>%
  mutate(survey_year = 2024)


rain_summary <- read_csv("C:/Users/Saúl/Documents/placebo_rain_data/province_placebo.csv")



# Normalize province names
normalize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("[^a-z]", "")
}

# Province mapping (codes 1 to 52)
prov_code_map <- tibble::tibble(
  prov_nac = 1:52,
  provincia = c(
    "Araba/Álava", "Albacete", "Alicante/Alacant", "Almería", "Ávila",
    "Badajoz", "Balears (Illes)", "Barcelona", "Burgos", "Cáceres",
    "Cádiz", "Castellón/Castelló", "Ciudad Real", "Córdoba", "Coruña (A)",
    "Cuenca", "Girona", "Granada", "Guadalajara", "Gipuzkoa",
    "Huelva", "Huesca", "Jaén", "León", "Lleida", "La Rioja",
    "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Ourense",
    "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca",
    "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria",
    "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid",
    "Bizkaia", "Zamora", "Zaragoza", "Ceuta", "Melilla"
  )
) %>%
  mutate(provincia_norm = normalize_name(provincia))

rain_summary <- rain_summary %>%
  mutate(provincia_norm = normalize_name(provincia))

rain_summary <- rain_summary %>%
  mutate(provincia_norm = normalize_name(provincia)) %>%
  mutate(provincia_norm = case_when(
    provincia_norm == "alicante" ~ "alicantealacant",
    provincia_norm == "valencia" ~ "valenciavalncia",
    provincia_norm == "illesbalears" ~ "balearsilles",
    provincia_norm == "acoruna" ~ "corunaa",
    provincia_norm == "castellon" ~ "castelloncastello",
    TRUE ~ provincia_norm
  ))



harmonize <- function(df, year, survey_year = NULL) {
  df <- df %>%
    mutate(year = year)
  
  if (year == 2013) {
    df <- df %>%
      mutate(
        BIRTH = as.numeric(P2802),
        AGE = survey_year - BIRTH,
        prov_nac = as.numeric(P30A),
        SIZE_HOMETOWN = as.numeric(P16A),
        BORN_SPAIN = if_else(P30 == 1, 1, 0),
        CATHOLIC = if_else(P61 == 1, 1, 0),
        RELIGIOUS_PRACTICE = case_when(
          P61C == 1 ~ 1, P61C == 2 ~ 2, P61C == 3 ~ 3,
          P61C == 4 ~ 4, P61C == 5 ~ 5, TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = as.numeric(P32H),
        CATHOLIC_SCHOOL = if_else(P32I == 1, 1, 0),
        CONSERVATIVE_VOTE = case_when(
          P62A %in% c(77, 0, 97) ~ NA_real_,
          P62A %in% c(2, 5) ~ 1, TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P62 %in% c(6, 8, 2, 9) ~ NA_real_,
          P62 %in% c(1) ~ 1, TRUE ~ 0
        ),
        FEMALE = if_else(P27 == 2, 1, 0),
        HH_INC = case_when(P66 %in% 1:10 ~ P66, P66 %in% c(98, 99) ~ 99, TRUE ~ NA_real_),
        SCHOOL = if_else(P32 == 1, 1, 0),
        EDUCATION = if_else(SCHOOL == 1, as.numeric(P32A01), 0),
        FATHER_BORN_SPAIN = if_else(P15C == 1, 1, 0),
        MOTHER_BORN_SPAIN = if_else(P14C == 1, 1, 0),
        FATHER_SCHOOL = if_else(P15M == 1, 1, 0),
        FATHER_EDUCATION = if_else(FATHER_SCHOOL == 1, as.numeric(P15N01), 0),
        MOTHER_SCHOOL = if_else(P14M == 1, 1, 0),
        MOTHER_EDUCATION = if_else(MOTHER_SCHOOL == 1, as.numeric(P14N01), 0),
        FATHER_EMPLOYMENT = as.numeric(P15O),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P15Q),
        MOTHER_EMPLOYMENT = as.numeric(P14O),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P14Q),
        FATHER_CATHOLIC = if_else(P15S == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P14S == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(
          P15T == 1 ~ 1, P15T == 2 ~ 2, P15T == 3 ~ 3,
          P15T == 4 ~ 4, P15T == 5 ~ 5, TRUE ~ NA_real_
        ),
        MOTHER_RELIGIOUS_PRACTICE = case_when(
          P14T == 1 ~ 1, P14T == 2 ~ 2, P14T == 3 ~ 3,
          P14T == 4 ~ 4, P14T == 5 ~ 5, TRUE ~ NA_real_
        ),
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
        SIZE_HOMETOWN = as.numeric(P44A),
        BORN_SPAIN = if_else(P56 == 1, 1, 0),
        CATHOLIC = if_else(P79 == 1, 1, 0),
        RELIGIOUS_PRACTICE = case_when(
          P79B == 1 ~ 1, P79B == 2 ~ 2, P79B == 3 ~ 3,
          P79B == 4 ~ 4, P79B == 5 ~ 5, TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(97, 95, 94, 77, 0) ~ NA_real_,
          RECUERDO %in% c(1, 9) ~ 1, TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P80 %in% c(9, 8, 6, 2) ~ NA_real_,
          P80 %in% c(1) ~ 1, TRUE ~ 0
        ),
        FEMALE = if_else(P53 == 2, 1, 0),
        HH_INC = case_when(P84 %in% 1:10 ~ P84, P84 %in% c(98, 99) ~ 99, TRUE ~ NA_real_),
        SCHOOL = if_else(P58 == 3, 1, 0),
        EDUCATION = as.numeric(P58A),
        FATHER_BORN_SPAIN = if_else(P43C == 1, 1, 0),
        MOTHER_BORN_SPAIN = if_else(P42C == 1, 1, 0),
        FATHER_SCHOOL = if_else(P43J == 3, 1, 0),
        FATHER_EDUCATION = if_else(FATHER_SCHOOL == 1, as.numeric(P43K), 0),
        MOTHER_SCHOOL = if_else(P42J == 3, 1, 0),
        MOTHER_EDUCATION = if_else(MOTHER_SCHOOL == 1, as.numeric(P42K), 0),
        FATHER_EMPLOYMENT = as.numeric(P43L),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P43N),
        MOTHER_EMPLOYMENT = as.numeric(P42L),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P42N),
        FATHER_CATHOLIC = if_else(P43P == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P42P == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(
          P43R == 1 ~ 1, P43R == 2 ~ 2, P43R == 3 ~ 3,
          P43R == 4 ~ 4, P43R == 5 ~ 5, TRUE ~ NA_real_
        ),
        MOTHER_RELIGIOUS_PRACTICE = case_when(
          P42R == 1 ~ 1, P42R == 2 ~ 2, P42R == 3 ~ 3,
          P42R == 4 ~ 4, P42R == 5 ~ 5, TRUE ~ NA_real_
        ),
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
        AGE = survey_year - FNACIMANYO2,
        prov_nac = as.numeric(P38A),
        SIZE_HOMETOWN = as.numeric(P28A),
        BORN_SPAIN = if_else(P38 == 1, 1, 0),
        CATHOLIC = if_else(P63 == 1, 1, 0),
        RELIGIOUS_PRACTICE = case_when(
          P63B == 1 ~ 1, P63B == 2 ~ 2, P63B == 3 ~ 3,
          P63B == 4 ~ 4, P63B == 5 ~ 5, TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = NA_real_,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c( 93, 94, 97, 77, 0) ~ NA_real_,
          RECUERDO %in% c(1, 8) ~ 1, TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = NA_real_,
        PARTICIPATION = case_when(
          P64 %in% c(9, 2, 6) ~ NA_real_,
          P64 %in% c(1) ~ 1, TRUE ~ 0
        ),
        FEMALE = if_else(P0 == 2, 1, 0),
        HH_INC = case_when(P68 %in% 1:11 ~ P68, P68 %in% c(98, 99) ~ 99, TRUE ~ NA_real_),
        SCHOOL = if_else(P40 == 3, 1, 0),
        EDUCATION = as.numeric(P401),
        FATHER_BORN_SPAIN = if_else(P27C == 1, 1, 0),
        MOTHER_BORN_SPAIN = if_else(P26C == 1, 1, 0),
        FATHER_SCHOOL = if_else(P27J == 3, 1, 0),
        FATHER_EDUCATION = if_else(FATHER_SCHOOL == 1, as.numeric(P27K), 0),
        MOTHER_SCHOOL = if_else(P26J == 3, 1, 0),
        MOTHER_EDUCATION = if_else(MOTHER_SCHOOL == 1, as.numeric(P26K), 0),
        FATHER_EMPLOYMENT = as.numeric(P27L),
        FATHER_EMPLOYMENT_TYPE = as.numeric(P27N),
        MOTHER_EMPLOYMENT = as.numeric(P26L),
        MOTHER_EMPLOYMENT_TYPE = as.numeric(P26N),
        FATHER_CATHOLIC = if_else(P27P == 1, 1, 0),
        MOTHER_CATHOLIC = if_else(P26P == 1, 1, 0),
        FATHER_RELIGIOUS_PRACTICE = case_when(
          P27R == 1 ~ 1, P27R == 2 ~ 2, P27R == 3 ~ 3,
          P27R == 4 ~ 4, P27R == 5 ~ 5, TRUE ~ NA_real_
        ),
        MOTHER_RELIGIOUS_PRACTICE = case_when(
          P26R == 1 ~ 1, P26R == 2 ~ 2, P26R == 3 ~ 3,
          P26R == 4 ~ 4, P26R == 5 ~ 5, TRUE ~ NA_real_
        ),
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
        SIZE_HOMETOWN = as.numeric(TAMUNI),
        BORN_SPAIN = if_else(LUGAR_NAC == 1, 1, 0),
        CATHOLIC = if_else(NAT_RELIG == 1, 1, 0),
        RELIGIOUS_PRACTICE = case_when(
          ATTEND == 1 ~ 5,
          ATTEND == 2 ~ 4,
          ATTEND == 3 ~ 3,
          ATTEND == 4 ~ 3,
          ATTEND == 5 ~ 2,
          ATTEND == 6 ~ 2,
          ATTEND == 7 ~ 1,
          ATTEND == 8 ~ 1,
          TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = TIPO_COLEGIO,
        CATHOLIC_SCHOOL = NA_real_,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(97, 98, 99, 77, 0) ~ NA_real_,
          RECUERDO %in% c(2, 6, 9) ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = case_when(
          RECUERDO %in% c(97, 98, 99, 77, 0) ~ NA_real_,
          RECUERDO %in% c(6) ~ 1,
          TRUE ~ 0
        ),
        PARTICIPATION = case_when(
          VOTE_LE %in% c(4, 5, 9) ~ NA_real_,
          VOTE_LE %in% c(1) ~ 1,
          TRUE ~ 0
        ),
        FEMALE = if_else(SEX == 2, 1, 0),
        NAT_RINC = case_when(
          NAT_RINC %in% 1:10 ~ NAT_RINC,
          NAT_RINC %in% c(97, 99) ~ 99,
          TRUE ~ NA_real_
        ),
        SCHOOL = NA_real_,
        EDUCATION = as.numeric(NAT_DEGR),
        FATHER_BORN_SPAIN = if_else(F_BORN == 1, 1, 0),
        MOTHER_BORN_SPAIN = if_else(M_BORN == 1, 1, 0),
        FATHER_SCHOOL = if_else(FATH_NAT_DEGR == 0, 0, 1),
        FATHER_EDUCATION = as.numeric(FATH_NAT_DEGR),
        MOTHER_SCHOOL = if_else(MOTH_NAT_DEGR == 0, 0, 1),
        MOTHER_EDUCATION = as.numeric(MOTH_NAT_DEGR),
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
    
  }   else if (year == 2024) {
    df <- df %>%
      mutate(
        BIRTH = as.numeric(BIRTH),
        AGE = survey_year - BIRTH,
        prov_nac = as.numeric(PROV),
        SIZE_HOMETOWN = as.numeric(TAMUNI),
        BORN_SPAIN = if_else(NACIONALIDAD == 1, 1, 0),
        CATHOLIC = if_else(NAT_RELIG == 1, 1, 0),
        RELIGIOUS_PRACTICE = case_when(
          ATTEND == 1 ~ 5,
          ATTEND == 2 ~ 4,
          ATTEND == 3 ~ 3,
          ATTEND == 4 ~ 3,
          ATTEND == 5 ~ 2,
          ATTEND == 6 ~ 2,
          ATTEND == 7 ~ 1,
          ATTEND == 8 ~ 1,
          TRUE ~ NA_real_
        ),
        PRIMARY_SCHOOL_TYPE = TIPO_COLEGIO,
        CATHOLIC_SCHOOL = TIPO_COLEGIO_2,
        CONSERVATIVE_VOTE = case_when(
          RECUERDO %in% c(95, 94, 90, 77, 0) ~ NA_real_,
          RECUERDO %in% c(1, 3, 7) ~ 1,
          TRUE ~ 0
        ),
        FAR_RIGHT_VOTE = case_when(
          RECUERDO %in% c(95, 94, 90, 77, 0) ~ NA_real_,
          RECUERDO %in% c(3) ~ 1,
          TRUE ~ 0
        ),
        PARTICIPATION = case_when(
          VOTE_LE %in% c(4, 5, 9) ~ NA_real_,
          VOTE_LE %in% c(1) ~ 1,
          TRUE ~ 0
        ),
        FEMALE = if_else(SEXO == 2, 1, 0),
        NAT_RINC = case_when(
          NAT_RINC %in% 1:10 ~ NAT_RINC,
          NAT_RINC %in% c(97, 99) ~ 99,
          TRUE ~ NA_real_
        ),
        SCHOOL = NA_real_,
        EDUCATION = as.numeric(NAT_DEGR),
        FATHER_BORN_SPAIN = if_else(F_BORN == 1, 1, 0),
        MOTHER_BORN_SPAIN = if_else(M_BORN == 1, 1, 0),
        FATHER_SCHOOL = if_else(FATH_NAT_DEGR == 0, 0, 1),
        FATHER_EDUCATION = as.numeric(FATH_NAT_DEGR),
        MOTHER_SCHOOL = if_else(MOTH_NAT_DEGR == 0, 0, 1),
        MOTHER_EDUCATION = as.numeric(MOTH_NAT_DEGR),
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
        TRUST_PEOPLE = case_when(
          V10 == 8 ~ NA_real_,
          V10 %in% 1:5 ~ V10 * 2,
          TRUE ~ NA_real_
        ),
        INST_CONFIDENCE = NA_real_,
        MERITOCRACY_BELIEF = if_else(MERIT %in% 0:10, MERIT, NA_real_),,
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
  }
  
}

# Harmonize surveys (your harmonize() function remains unchanged)
ESGE_2013_h <- harmonize(ESGE_2013, 2013)
ESGE_2015_h <- harmonize(ESGE_2015, 2015)
ESGE_2017_h <- harmonize(ESGE_2017, 2017)
ESGE_2023_h <- harmonize(ESGE_2023, 2023)
ESGE_2024_h <- harmonize(ESGE_2024, 2024)

# Add PROV and MUN columns from original datasets
# === 4️⃣ Add PROV and MUN columns ===

ESGE_2013_h <- ESGE_2013_h %>%
  mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

ESGE_2015_h <- ESGE_2015_h %>%
  mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

ESGE_2017_h <- ESGE_2017_h %>%
  mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

ESGE_2023_h <- ESGE_2023_h %>%
  mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

ESGE_2024_h <- ESGE_2024_h %>%
  mutate(PROV = as.numeric(PROV), MUN = as.numeric(MUN))

# === 5️⃣ Combine all surveys ===
# Harmonized variable list (include PROV and MUN!)
harmonized_vars <- c(
  "survey_year", "year", "BIRTH", "AGE", "prov_nac", "SIZE_HOMETOWN", "BORN_SPAIN", "CATHOLIC", 
  "RELIGIOUS_PRACTICE", "PRIMARY_SCHOOL_TYPE", "CATHOLIC_SCHOOL", "CONSERVATIVE_VOTE", 
  "FAR_RIGHT_VOTE", "PARTICIPATION", "FEMALE", "HH_INC", "SCHOOL", "EDUCATION", "FATHER_BORN_SPAIN", 
  "MOTHER_BORN_SPAIN", "FATHER_SCHOOL", "FATHER_EDUCATION", "MOTHER_SCHOOL", "MOTHER_EDUCATION", 
  "FATHER_EMPLOYMENT", "FATHER_EMPLOYMENT_TYPE", "MOTHER_EMPLOYMENT", "MOTHER_EMPLOYMENT_TYPE", 
  "FATHER_CATHOLIC", "MOTHER_CATHOLIC", "FATHER_RELIGIOUS_PRACTICE", "MOTHER_RELIGIOUS_PRACTICE", 
  "SAME_LOC_BIRTH", "MOTHER_IDEOLOGY_LR", "FATHER_IDEOLOGY_LR", "COUPLE_IDEOLOGY_LR", 
  "IDEOLOGY_LR", "SUBJECTIVE_CLASS", "TRUST_PEOPLE", "INST_CONFIDENCE", "MERITOCRACY_BELIEF", 
  "PUBLIC_SECTOR_EMP", "LIFE_SATISFACTION", "CIVIL_RELATION", "COUPLE_BORN_SPAIN", "HAS_A_COUPLE", 
  "LEFT_RIGHT", "FATHER_LEFT_RIGHT", "MOTHER_LEFT_RIGHT", "COUPLE_CATHOLIC", "COUPLE_LEFT_RIGHT",
  "PROV", "MUN"   # Important! Include these
)


# Combine harmonized datasets while keeping only the selected variables
survey <- dplyr::bind_rows(
  dplyr::select(ESGE_2013_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2015_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2017_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2023_h, dplyr::any_of(harmonized_vars)),
  dplyr::select(ESGE_2024_h, dplyr::any_of(harmonized_vars))
) %>%
  dplyr::mutate(respondent_id = dplyr::row_number())


# Clean and prepare survey data
survey_clean <- survey %>%
  mutate(
    childhood_start = BIRTH + 5,
    childhood_end = BIRTH + 18
  ) %>%
  left_join(prov_code_map, by = "prov_nac") %>%
  mutate(provincia_norm = normalize_name(provincia)) %>%
  filter(!is.na(provincia_norm), !is.na(BIRTH), BIRTH > 1900)

# Rainfall summary lookup function
get_childhood_summary <- function(prov_norm, start_year, end_year) {
  df <- rain_summary %>%
    filter(provincia_norm == prov_norm, year >= start_year, year <= end_year)
  if (nrow(df) == 0) return(c(NA_real_, NA_integer_))
  c(mean(df$avg_precip, na.rm = TRUE), sum(df$dry_days, na.rm = TRUE))
}

# Identify unmatched provinces
missing_provinces <- setdiff(survey_clean$provincia_norm, rain_summary$provincia_norm)
print(missing_provinces)

# Compute childhood weather metrics
results <- survey_clean %>%
  rowwise() %>%
  mutate(
    childhood_avg_precip = get_childhood_summary(provincia_norm, childhood_start, childhood_end)[1],
    childhood_total_dry_days = get_childhood_summary(provincia_norm, childhood_start, childhood_end)[2]
  ) %>%
  ungroup() %>%
  dplyr::select(respondent_id, childhood_avg_precip, childhood_total_dry_days)


# Combine weather metrics with full survey and drop those born before 1920
survey_final <- left_join(survey_clean, results, by = "respondent_id") %>%
  mutate(age = survey_year - BIRTH) %>%
  filter(BIRTH >= 1920)


survey_final <- survey_final %>%
  filter(BIRTH >= 1920, BIRTH <= 2020)

survey_final %>%
  summarise(
    n_total = n(),
    n_missing_childhood_avg_precip = sum(is.na(childhood_avg_precip)),
    n_missing_childhood_total_dry_days = sum(is.na(childhood_total_dry_days))
  )
# Export result
write_csv(survey_final, "C:/Users/Saúl/Documents/placebo_rain_data/ESGE_placebo.csv")


# 1st Stage  --------------------------------------------------------------
library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(tidyr)
library(ggplot2)



# Outcomes (Standardized) -------------------------------------------------


# Load the data
survey <- read_csv("C:/Users/Saúl/Documents/placebo_rain_data/ESGE_placebo.csv")


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
                MOTHER_EMPLOYMENT, MOTHER_CATHOLIC, birth_prov_cluster, COUPLE_CATHOLIC, LEFT_RIGHT, INST_CONFIDENCE, PUBLIC_SECTOR_EMP, MERITOCRACY_BELIEF, SUBJECTIVE_CLASS, FAR_RIGHT_VOTE, CONSERVATIVE_VOTE, TRUST_PEOPLE, RELIGIOUS_PRACTICE, PARTICIPATION)


# ---------------------------
# Standardize Treatment Variable
# ---------------------------

model_data <- model_data %>%
  mutate(
    childhood_total_dry_days_std = (childhood_total_dry_days - mean(childhood_total_dry_days, na.rm = TRUE)) / 
      sd(childhood_total_dry_days, na.rm = TRUE)
  )


# ---------------------------
# Linear Probability Models: CATHOLIC 
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cat <- feols(
  CATHOLIC ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EDUCATION + MOTHER_EMPLOYMENT | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# ---------------------------
# Linear Probability Models: RELIGIOUS PRACTICE 
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_rel <- feols(
  RELIGIOUS_PRACTICE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EDUCATION + 
    MOTHER_EMPLOYMENT | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# ---------------------------
# Linear Probability Models: COUPLE_CATHOLIC (survey_year 2013, 2015, 2017 only)
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_cou <- feols(
  COUPLE_CATHOLIC ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + survey_year +
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EDUCATION + 
    MOTHER_EMPLOYMENT | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)


# ---------------------------
# Model summary: Linear (Standardized), single Controls indicator
# ---------------------------

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


# ---------------------------
# Linear Probability Models: PARTICIPATION
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + survey_year  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_par <- feols(
  PARTICIPATION ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + 
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EDUCATION + MOTHER_EMPLOYMENT + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# ---------------------------
# Linear Probability Models: CONSERVATIVE_VOTE
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + survey_year  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_con <- feols(
  CONSERVATIVE_VOTE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + 
    FATHER_EMPLOYMENT +
    MOTHER_BORN_SPAIN + MOTHER_EDUCATION + MOTHER_EMPLOYMENT + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# ---------------------------
# Linear Probability Models: FAR_RIGHT_VOTE
# ---------------------------

# 1. With FE, No controls
lpm_fe_nocontrols_far <- feols(
  FAR_RIGHT_VOTE ~ childhood_total_dry_days_std + survey_year  | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)

# 2. With FE, With controls
lpm_fe_controls_far <- feols(
  FAR_RIGHT_VOTE ~ childhood_total_dry_days_std + FEMALE +
    FATHER_BORN_SPAIN + FATHER_EDUCATION + 
    FATHER_EMPLOYMENT + MOTHER_BORN_SPAIN + MOTHER_EDUCATION + 
    MOTHER_EMPLOYMENT + survey_year | BIRTH + prov_nac,
  data = model_data,
  cluster = ~prov_nac
)



# ---------------------------
# Model summary: Linear (Standardized), single Controls indicator
# ---------------------------

modelsummary(
  list(
    "Participation"                   = lpm_fe_nocontrols_par,
    "Participation + Controls"        = lpm_fe_controls_par,
    "Conservative"                    = lpm_fe_nocontrols_con,
    "Conservative + Controls"         = lpm_fe_controls_con,
    "Far-right"                       = lpm_fe_nocontrols_far,
    "Far-right + Controls"            = lpm_fe_controls_far
  ),
  title = "LPM: Participation in last election and probability voting for Conservative or Far-right parties",
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



