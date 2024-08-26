# PROJECT:  
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
path_map_indicator <- "Documents/indicator_mapper_2024-08-14.csv"
path_dis_processed <- "Dataout/dis_processed.csv"
path_dis_indicator_shortname <- "Documents/indicator_shortname.xlsx"

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "fc252b5c"

# LOAD DATA ------------------------------------------------------------------

source("Scripts/utilities.R")

df <- read_excel(path_dis_export,
                 sheet = "OU Activity Indicator Results")

map_indicator <- read_csv(path_map_indicator) |> 
  filter(disaggregate_type != "")

indicator_shortname <- read_excel(path_dis_indicator_shortname) |> 
  select(-length) |> 
  clean_names()

# MUNGE -------------------------------------------------------------------


map_indicator_shortname <- map_indicator |> 
 left_join(indicator_shortname, by = "indicator_name")


df_processed <- df |>
  
  # clean and filter data frame
  clean_names() |> 
  select(!all_of(vars_remove)) |> 
  rename(period = collection_period_name,
         review_status = collection_review_status) |> 
  mutate(
    activity_code = str_replace(activity_code, "^0+", "")) |> 
  filter(activity_code %in% iho_activities) |> 
  
  mutate(
    # create unique data element id variable
    data_element_id = str_c(indicator_code, disaggregate_code, disaggregate_name, sep = "-"),
    
    # coerce actuals and targets to numeric
    across(.cols = c(actual, target), 
           .fns = ~ str_remove_all(.x, ",")
           ),
    
    # create period type and period values
    period_type = case_when(
      str_detect(period, "Annual") ~ "Annual",
      str_detect(period, "Qtr") ~ "Quarter"),
    
    period_temp = case_when(
      str_detect(period_type, "Annual") ~ "",
      str_detect(period, "Qtr1") ~ "Q1",
      str_detect(period, "Qtr2") ~ "Q2",
      str_detect(period, "Qtr3") ~ "Q3",
      str_detect(period, "Qtr4") ~ "Q4"),
    
    period = case_when(
      str_detect(period_type, "Annual") ~ NA,
      str_detect(period_type, "Quarter") ~ str_c(fiscal_year, 
                                                 period_temp, 
                                                 sep = " ")
      ),
    
    indicator_origin_sub = case_when(
      indicator_origin == "FTF" ~ "PPR",
      indicator_origin == "Std FA" ~ "PPR",
      .default = indicator_origin),
    
    review_status = case_when(
      review_status == "DE In-Progress" ~ "Data Entry In-Progress",
      review_status == "DE Not Started" ~ "Data Entry Not Started",
      .default = review_status
    )
  ) |> 
  
  select(!period_temp) |> 
  
  # rename activities
  mutate(activity_name = case_when(
    activity_code ==  "333" ~ "MTAPS",
    activity_code ==  "510" ~ "SBBC",
    activity_code ==  "1884" ~ "Resilent Gorongosa",
    activity_code ==  "2157" ~ "PQM+",
    activity_code ==  "2407" ~ "Transform Nutrition",
    activity_code ==  "2951" ~ "Alcancar",
    activity_code ==  "3150" ~ "CEFM/CVE",
    activity_code ==  "3213" ~ "Last Mile Supply Chain",
    activity_code ==  "4893" ~ "SPEED",
    activity_code ==  "4940" ~ "IFPI",
    activity_code ==  "5046" ~ "M-RITE",
    activity_code ==  "5250" ~ "Amostra",
    activity_code ==  "5446" ~ "Feed the Future",
    activity_code ==  "6226" ~ "MCAPS",
    activity_code ==  "5472" ~ "Advancing Girls' Ed.",
    activity_code ==  "6366" ~ "MSSFPO",
    activity_code ==  "6769" ~ "MPHD",
    .default = activity_name)
  ) |> 


 

  # join to code for disaggregate_type and remove NA
  inner_join(map_indicator_shortname, join_by(data_element_id)) |>
  filter(disaggregate_type %in% c("Age", "Sex", "Province", "Total", "Treatment Type", "Vaccine")) |> 
  mutate(disaggregate_name = case_when(
    disaggregate_type == "Total" ~ "Total",
    .default = disaggregate_name)
  ) |>
  # final data frame cleaning
  relocate(indicator_origin, .before = indicator_code) |>
  relocate(indicator_name, .before = udn) |>
  relocate(disaggregate_type, .before = disaggregate_name) |> 
  relocate(data_element_id, .before = disaggregate_code) |> 
  relocate(period_type, .before = fiscal_year) |> 
  relocate(review_status, .after = everything())


  

###
### Note that 2 data_element_ids are duplicated which generate additional observations
### HL.9.4-8-CUST-HL.9.4-8-CUST-L9-Number of male children under five reached by USG-supported nutrition programs
### HL.9.4-8-CUST-HL.9.4-8-CUST-L10-Number of female children under five reached by USG-supported nutrition programs
###

# Code chunk below identifies additional observations created by many-to-many join
# temp <- df1 |> 
#   filter(data_element_id %in% c(
#     "HL.9.4-8-CUST-HL.9.4-8-CUST-L9-Number of male children under five reached by USG-supported nutrition programs",
#     "HL.9.4-8-CUST-HL.9.4-8-CUST-L10-Number of female children under five reached by USG-supported nutrition programs"
#   ))

# DATAOUT -----------------------------------------------------------------

df_processed |> 
  write_csv(path_dis_processed)
  