# Rain data processing ------------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# Clean environment
rm(list = ls())

# Set working directory (change this path as needed)
setwd("C:/Users/Saúl/Documents/holy_week_data")  # Update path

# List all CSV files with year in the filename
files <- list.files(pattern = "precip_holy_week_\\d{4}\\.csv")

# Container for yearly results
all_data <- list()

for (file in files) {
  year_value <- str_extract(file, "\\d{4}") |> as.integer()
  message("\n📂 Processing file: ", file)
  
  # Read CSV, treat all columns as character
  data <- read_csv(file, col_types = cols(.default = "c")) |> 
    rename_with(tolower)
  
  # Validate required columns
  if (!all(c("fecha", "provincia", "prec") %in% names(data))) {
    warning("❌ Missing columns in file: ", file)
    next
  }
  
  # Clean and process data
  data_clean <- data %>%
    mutate(
      fecha = ymd(fecha),  # Correct date format
      year = year_value,
      prec_trimmed = str_trim(prec),
      prec_clean = case_when(
        prec_trimmed == "Ip" ~ 0,
        str_detect(prec_trimmed, "^\\d+(,\\d+)?$") ~ as.numeric(str_replace(prec_trimmed, ",", ".")),
        str_detect(prec_trimmed, "^\\d+(\\.\\d+)?$") ~ as.numeric(prec_trimmed),
        TRUE ~ NA_real_
      )
    )
  
  # Optional: quick debug print
  message("🔎 Sample parsed rows:")
  print(head(dplyr::select(data_clean, provincia, fecha, prec, prec_clean)))
  
  # Filter valid data
  data_filtered <- data_clean %>%
    filter(!is.na(prec_clean), !is.na(fecha))
  
  if (nrow(data_filtered) == 0) {
    message("⚠️ No valid data in this file. Skipping.")
    next
  }
  
  # Average precipitation per province/year
  avg_precip <- data_filtered %>%
    group_by(provincia, year) %>%
    summarise(
      avg_precip = mean(prec_clean),
      .groups = "drop"
    )
  
  # Count dry days (days where all stations in province had 0 mm)
  dry_days <- data_filtered %>%
    group_by(provincia, fecha, year) %>%
    summarise(
      is_dry = all(prec_clean == 0),
      .groups = "drop"
    ) %>%
    group_by(provincia, year) %>%
    summarise(
      dry_days = sum(is_dry),
      .groups = "drop"
    )
  
  # Combine summary tables
  summary <- left_join(avg_precip, dry_days, by = c("provincia", "year"))
  
  # Store in list
  all_data[[as.character(year_value)]] <- summary
}

# Combine all years
final_result <- bind_rows(all_data)

# Save result
write_csv(final_result, "province_holy_week_summary_final.csv")

# Display summary
message("\n✅ All files processed. Results saved to 'province_holy_week_summary_finalcsv'")
print(final_result)
