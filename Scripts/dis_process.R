r # PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   fc252b5c 
# LICENSE:  MIT
# DATE:     2024-08-12
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(janitor)
library(openxlsx)
library(readxl)

path_dis_export <- "Data/OU Activity Indicator Results Report - Export All.xlsx"
path_map_indicator <- "Documents/indicator_mapper_2024-08-13.csv"
path_dis_processed <- "Dataout/"

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "fc252b5c"

# LOAD DATA ------------------------------------------------------------------

df <- read_excel(path_dis_export,
                 sheet = "OU Activity Indicator Results")

map_indicator <- read_csv(path_map_indicator) |> 
  clean_names() |> 
  select(data_element_id, disag_type) |> 
  filter(disag_type != "")


# MUNGE -------------------------------------------------------------------

df1 <- df |>
  
  # clean data frame
  clean_names() |> 
  select(!c("reporting_organization", 
            "operating_unit",
            "disaggregate_country",
            "disaggregate_commodity",
            "collection_period_sort_order",
            "initiative_review_status",
            "is_disaggregate_blank",
            "collection_period_comments")) |>
  rename(period = collection_period_name,
         review_status = collection_review_status) |> 
  
  # create unique data element id variable
  mutate(data_element_id = str_c(indicator_code, disaggregate_code, disaggregate_name, sep = "-")) |> 
  
  # create period variables
  mutate(period_type = case_when(str_detect(period, "Annual") ~ "Annual",
                                 str_detect(period, "Qtr") ~ "Quarter"),
         period_temp = case_when(str_detect(period_type, "Annual") ~ "",
                                 str_detect(period, "Qtr1") ~ "Q1",
                                 str_detect(period, "Qtr2") ~ "Q2",
                                 str_detect(period, "Qtr3") ~ "Q3",
                                 str_detect(period, "Qtr4") ~ "Q4"),
         period = case_when(str_detect(period_type, "Annual") ~ NA,
                            str_detect(period_type, "Quarter") ~ str_c(fiscal_year, period_temp, sep = " "))) |>
  select(!period_temp) |> 
  
  # join to code for disaggregate_type and remove NA
  left_join(map_indicator, join_by(data_element_id)) |> 
  filter(disag_type != "") |> 
  
  # final data frame cleaning
  relocate(indicator_origin, .after = activity_code) |>
  relocate(udn, .before = indicator_code) |> 
  relocate(disag_type, .after = disaggregate_name) |> 
  relocate(data_element_id, .after = disag_type) |> 
  relocate(period_type, .before = fiscal_year)


# DATAOUT -----------------------------------------------------------------

df1 |> 
  write_csv()
