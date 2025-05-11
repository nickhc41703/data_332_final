library(readr)
library(purrr)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(fs) 

data_dir <- "data/" 

csv_files <- dir_ls(path = data_dir, regexp = "\\.csv$", recurse = FALSE)

for (csv_path in csv_files) {

  df <- read_csv(csv_path)
  
  rds_path <- path_ext_set(csv_path, "rds")
  
  write_rds(df, rds_path, compress = "xz")
  
  message("Converted ", basename(csv_path), " → ", basename(rds_path))
}

saveRDS(uber_data, file = rds_path, compress = "xz")