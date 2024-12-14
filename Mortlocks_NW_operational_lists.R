## TB-Free Chuuk R code
## Mortlocks and NW Operational Lists for LTBI

## Goal: Make LTBI list for local health workers and program coordinators

#------------------------------------------------------------------------------
#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#PACKAGES
library(tidyverse) #pipes, stringr, lubridate, scales
library(readxl) #excel load-in
library(openxlsx) #export excel files

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)
#------------------------------------------------------------------------------

##DATA
#load in clean flatfile
mortlocks_flatfile <- read_excel("Data/mortlocks_analysis_data.xlsx")

###Make LTBI list for CHOWs at each dispensary and tracking database
ltbi_mortlocks <- mortlocks_flatfile %>%
  filter(is.not.na(ltbi_treatment_id)) %>%
  mutate(treatment_start_date_db = case_when(is.na(treatment_start_date_db) & 
                                               treatment_started == "Y" ~ date_screening,
                                             .default = treatment_start_date_db),
         most_recent_dose = treatment_start_date_db,
         doses_completed = case_when(treatment_started == "Y" ~ 1,
                                     .default = 0),
         date_last_allowable_completion = treatment_start_date_db + (15*7),
         treatment_status = case_when(treatment_started == "Y" ~ "Currently treating",
                                      treatment_started == "N" ~ "Treatment not started",
                                      .default = NA),
         treatment_stop_reason = NA,
         weeks_since_start = case_when(is.na(treatment_start_date_db) ~ NA,
                                       .default = (today()-treatment_start_date_db)/7),
         weeks_remaining = 12-weeks_since_start,
         doses_remaining = 12-doses_completed,
         weeks_remaining_minus_doses_remaining = weeks_remaining-doses_remaining,
         doses_missed = case_when(weeks_since_start < 1 ~ 0,
                                  as.numeric(weeks_since_start)-doses_completed>-1 
                                  ~ as.numeric(weeks_since_start)-doses_completed,
                                  .default = 0),
         epi_status = case_when(treatment_stop_reason == "Completed all treatment" |
                                  (weeks_remaining < 0 & doses_completed >= 11 &
                                     most_recent_dose <= date_last_allowable_completion) 
                                ~ "Completed all treatment",
                                treatment_status == "Treatment stopped" 
                                ~ "Treatment stopped",
                                is.na(treatment_start_date_db) 
                                ~ "Treatment not started",
                                doses_missed > 4 
                                ~ "Treatment restart needed",
                                weeks_remaining_minus_doses_remaining > -1 
                                ~ "On track",
                                weeks_remaining_minus_doses_remaining >= -5 
                                ~ paste0("Possible completion: ", doses_missed," dose(s) missed or late"),
                                weeks_remaining_minus_doses_remaining < -5 
                                ~ paste0(doses_missed," doses missed or late, unlikely to complete in 16 weeks")),
         notes = other_medical_history
  ) %>%
  select(registration_no,last_name,first_name,date_of_birth,age,sex,
         weight,municipality,date_of_visit1, tst_result,
         treatment_start_date_db,medication_order_full,most_recent_dose,
         doses_completed,date_last_allowable_completion,treatment_status,
         treatment_stop_reason, epi_status, weeks_since_start, weeks_remaining,
         doses_remaining, weeks_remaining_minus_doses_remaining, doses_missed,
         notes)

#EXPORT LTBI LIST TO EXCEL
write.xlsx(ltbi_mortlocks, "Mortlocks_NW/ltbi_mortlocks.xlsx")

#clean the workspace
rm(list = ls())