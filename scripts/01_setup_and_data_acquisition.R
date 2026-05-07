# ==============================================================================
# 01_setup_and_data_acquisition.R — ROBUST DATA IMPORT
# ==============================================================================
library(tidyverse)

# --- 1. SETUP PATHS -----------------------------------------------------------
project_root <- "C:/Documents/Education-Project2"
paths <- list(
  data_raw  = file.path(project_root, "data", "raw"), 
  data_proc = file.path(project_root, "data", "processed")
)

# Ensure folders exist
if(!dir.exists(paths$data_raw)) dir.create(paths$data_raw, recursive = TRUE)
if(!dir.exists(paths$data_proc)) dir.create(paths$data_proc, recursive = TRUE)

# --- 2. DYNAMIC FILE DETECTION ------------------------------------------------
# This will find ANY csv file in your raw folder
cfcs_files <- list.files(path = paths$data_raw, pattern = "\\.csv$", full.names = TRUE)

# CRITICAL CHECK: If this still fails, your folder is empty!
if(length(cfcs_files) == 0) {
  cat("\n📂 Checking folder contents for:", paths$data_raw, "\n")
  stop("❌ No CSV files found. Please move your CFCS data into the folder above.")
}

# --- 3. HARMONIZED LOADING ----------------------------------------------------
load_and_tag <- function(file_path) {
  # Get the name of the file for reference
  file_name <- basename(file_path)
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(source_file = file_name)
  
  message(sprintf("✅ Loaded %s: %d rows", file_name, nrow(df)))
  return(df)
}

# --- 4. MERGE & SAVE ----------------------------------------------------------
master_dataset <- map_df(cfcs_files, load_and_tag)

if(nrow(master_dataset) > 0) {
  write_csv(master_dataset, file.path(paths$data_proc, "master_analysis_dataset.csv"))
  message("\n✨ Success! Master dataset created with ", nrow(master_dataset), " observations.")
  message("This solves your 'Small-N Caveat' by merging all available microdata.")
}