# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   e56b3660 
# LICENSE:  MIT
# DATE:     2024-08-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(janitor)
library(openxlsx)
library(readxl)
library(glue)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "e56b3660"
  
  path_raw_map_indicator <- "Documents/OU Activity Indicator Results Report - Export All (4).xlsx"
  sys_date <- Sys.Date()
  path_new_map_indicator <- glue("Documents/indicator_mapper_{sys_date}.csv")

# LOAD DATA ------------------------------------------------------------------
  
  df <- read_excel(path_raw_map_indicator, 
                   sheet = "Disag type")
  
  # MUNGE -------------------------------------------------------------------

  df1 <- df |> 
    clean_names() |> 
    select(indicator_code, disaggregate_code, disaggregate_name, disag_type) |> 
    mutate(data_element_id = str_c(indicator_code, disaggregate_code, disaggregate_name, sep = "-")) |> 
    distinct(data_element_id, disag_type) |> 
    relocate(disag_type, .after = everything()) |> 
    rename(disaggregate_type = disag_type)
  
  
  df1 <- df |> 
    clean_names() |> 
    select(indicator_code, indicator_name, disaggregate_code, disaggregate_name, disag_type) |> 
    mutate(data_element_id = str_c(indicator_code, disaggregate_code, disaggregate_name, sep = "-")) |> 
    distinct(data_element_id, disag_type, indicator_name) |> 
    relocate(disag_type, .after = everything()) |> 
    rename(disaggregate_type = disag_type)
  
# DATAOUT -----------------------------------------------------------

  df1 |> 
    write_csv(path_new_map_indicator)
  