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

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "fc252b5c"

# LOAD DATA ------------------------------------------------------------------

source("Scripts/utilities.R")

df <- read_excel(path_dis_export,
                 sheet = "OU Activity Indicator Results")

map_indicator <- read_csv(path_map_indicator) |> 
  clean_names() |> 
  select(data_element_id, disaggregate_type) |> 
  filter(disaggregate_type != "")


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
  
  mutate(activity_code = str_replace(activity_code, "^0+", "")) |> 
  filter(activity_code %in% iho_activities) |> 
  
  # create unique data element id variable and coerce actuals and targets to numerics
  mutate(data_element_id = str_c(indicator_code, disaggregate_code, disaggregate_name, sep = "-"),
         actual = as.numeric(actual),
         target = as.numeric(target)) |> 
  
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
    activity_code ==  "5046" ~ "Momentum Routine Immun.",
    activity_code ==  "5250" ~ "Amostra",
    activity_code ==  "5446" ~ "Feed the Future",
    activity_code ==  "6226" ~ "MCAPS",
    activity_code ==  "5472" ~ "Advancing Girls' Ed.",
    activity_code ==  "6366" ~ "MSSFPO",
    activity_code ==  "6769" ~ "Momentum Delivery",
    .default = activity_name
  )) |> 

  # join to code for disaggregate_type and remove NA
  left_join(map_indicator, join_by(data_element_id)) |> 
  filter(disaggregate_type != "") |> 
  
  # final data frame cleaning
  relocate(indicator_origin, .after = activity_code) |>
  relocate(udn, .before = indicator_code) |> 
  relocate(disaggregate_type, .after = disaggregate_name) |> 
  relocate(data_element_id, .after = disaggregate_type) |> 
  relocate(period_type, .before = fiscal_year)

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

df1 |> 
  write_csv(path_dis_processed)
