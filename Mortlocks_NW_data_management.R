## TB-Free Chuuk R code
## Flatfile

## Goal: Wrangle data from TB-Free screening database into flatfile for analysis
##  Use data from Mortlocks and Northwest Islands only

#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#PACKAGES
library(tidyverse) #pipes
library(readxl) #excel load-in
library(openxlsx) #excel load-out

#FORMULAS
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#DATASETS
#REGISTRATION SHEET
registration <- read_excel("Data/REGISTRATION.xlsx",
                           guess_max = 20000, col_names = TRUE) %>%
  mutate(contact_no = case_when(is.not.na(head_of_household_contact_no) ~ head_of_household_contact_no,
                                is.not.na(phone_mobile) ~ phone_mobile, 
                                is.not.na(phone_home) ~ phone_home)) %>%
  select("registration_id", "registration_no", "onsite_id", "last_name", "first_name", 
         "date_of_birth", "sex", "state", "municipality", "village", 
         "date_created", "date_updated", "contact_no", "notes") %>%
  filter(date_created > "2024-05-01")

#PATIENT PASSPORT SHEET
passport <- read_excel("Data/PATIENT_PASSPORT.xlsx", 
                       guess_max = 20000, col_names = TRUE) %>%
  rename(tb_disease_exposure=tb_disease_treated, 
         tb_disease_exposure_name=tb_disease_treated_name) %>%
  mutate(xray_result_preliminary = case_when(xray_result_preliminary == 1 ~ "Likely TB",
                                             xray_result_preliminary == 2 ~ "Possible TB",
                                             xray_result_preliminary == 3 ~ "Unlikely TB - appears normal",
                                             xray_result_preliminary == 4 ~ "Unlikely TB - other pathology",
                                             xray_result_preliminary == 5 ~ "Unlikely TB - old/inactive TB",
                                             xray_result_preliminary == 6 ~ "Not done"),
         active_tb = case_when(active_tb == 1 ~ "Likely/Possible TB", 
                               active_tb == 2 ~ "Currently on TB treatment",
                               active_tb == 3 ~ "Unlikely/Negative for TB"),
         ltbi_actions = case_when(ltbi_actions == 1 ~ "Refer for prev consult", 
                                  ltbi_actions == 2 ~ "Prevention consult not indicated")
  ) %>%
  filter(date_created > "2024-05-01") %>%
  select("registration_id", "patient_passport_id", "tst_place_visit", 
         "has_consent", "screening_site", "date_of_visit1", "visit_type", 
         "tst_no", "tst_location", "tst_date_read", "tst_result", 
         "tst_interpretation", "tb_disease_exposure", "tb_disease_exposure_name", 
         "treated_tb_ltbi", "treated_tb_ltbi_program", "current_tb_symptoms_none", 
         "current_tb_symptoms_2weeks", "current_tb_symptoms_any_duration", 
         "current_tb_symptoms_coughing_blood", "current_tb_symptoms_fatigue", 
         "current_tb_symptoms_fever", "current_tb_symptoms_weight_loss", 
         "current_tb_symptoms_swollen_lymph", "current_tb_symptoms_sweats", 
         "current_tb_symptoms_other", "current_tb_symptoms_other_text", 
         "symptoms_start_date", "weight", "height", "is_pregnant", 
         "lymphadenopathy", "tb_exam", "failure_to_thrive", "hd_exposure", 
         "hd_exposure_name", "skin_lesions", "result_hd_assessment", 
         "xray_indicated", "xray_indicated_age_10", "xray_indicated_exposed", 
         "xray_indicated_signs", "xray_indicated_findings", 
         "xray_indicated_tst_positve", "xray_result_preliminary", 
         "active_tb", "ltbi_actions", "tb_sputum", "sputum_date_collected", 
         "a1c", "history_diabetes", "history_diabetes_age", 
         "medications_for_diabetes", "medications_for_diabetes_specify",
         "other_factor_none", "other_factor_kidney", "other_factor_cancer", 
         "other_factor_steroid", "medications_for_diabetes_other", 
         "medications_for_diabetes_metformin", "smoking_history", 
         "smoking_current_text", "how_many", "how_many_years", 
         "current_alcohol", "hd_prevention", "hd_prevention_text")

#LTBI HEALTH HISTORY SHEET
ltbi_hh <- read_excel("Data/HEALTH_HISTORY.xlsx", 
                      guess_max = 20000, col_names = TRUE)%>%
  select("registration_id", "hepa_or_liver_disease",
         "hepa_or_liver_disease_specify", "seizure_medications", 
         "blood_medications", "current_medications", "drug_allergies", 
         "drug_allergies_specify", "pregnant_or_planning_soon", 
         "using_birth_control", "using_birth_control_specify", 
         "other_medical_history")

#LTBI TREATMENT INFORMATION SHEET
ltbi_tx <- read_excel("Data/LTBI_TREATMENT.xlsx", 
                      guess_max = 20000, col_names = TRUE) %>%
  mutate_at(c('medication_order', 'medication_3hp', 'medication_3hr', 
              "medication_3r", 'medication_6h_dose', 'medication_9h_dose'), 
            ~str_replace_na(., "")) %>%
  mutate(medication_order_full = paste0(medication_order, medication_3hp, 
                                        medication_3hr, medication_3r, 
                                        as.character(medication_6h_dose), 
                                        as.character(medication_9h_dose)),
         dopt_day_txt = case_when(dopt_day == 0 ~ "Monday", 
                                  dopt_day == 1 ~ "Tuesday",
                                  dopt_day == 2 ~ "Wednesday",
                                  dopt_day == 3 ~ "Thursday",
                                  dopt_day == 4 ~ "Friday",
                                  dopt_day == 5 ~ "Saturday",
                                  dopt_day == 6 ~ "Sunday")
  ) %>%
  rename(treatment_start_date_db = treatment_start_date)  %>%
  filter(date_created > "2024-05-01" & ltbi_treatment_id != 2447) %>%
  select("registration_id", "ltbi_treatment_id",
         "treatment_recommended", "treatment_recommended_later", 
         "treatment_started", "medication_order_full", "other_regimen", 
         "treatment_start_date_db", "prescriber_name", "supervisor", "district", 
         "dopt_day_txt")

#ACTIVE TB DISEASE FOLLOWUP SHEET
tb_followup <- read_excel("Data/LAB_AND_DIAGNOSTIC.xlsx", 
                          guess_max = 20000, col_names = TRUE) %>%
  mutate(result_xpert = case_when(xpert_result == 1 ~ "MTB detected, Rif not",
                                  xpert_result == 2 ~ "MTB detected, Rif detected",
                                  xpert_result == 3 ~ "MTB detected, Rif ind",
                                  xpert_result == 4 ~ "MTB not detected",
                                  xpert_result == 5 ~ "Invalid",
                                  xpert_result == 6 ~ "Not done"),
         outcome_case_conference = case_when(outcome_case_conference == 1 ~ "Active TB",
                                             outcome_case_conference == 2 ~ "Possible TB - further workup now", 
                                             outcome_case_conference == 3 ~ "Unlikely TB - fup x-ray/consult later",
                                             outcome_case_conference == 4 ~ "Current TB - on active tx",
                                             outcome_case_conference == 5 ~ "Not TB"),
         actions_not_active_tb = 
           case_when(actions_taken_not_active_tb == 1 ~ "Refer for prevention consult",
                     actions_taken_not_active_tb == 2 ~ "Other, specify",
                     actions_taken_not_active_tb == 3 ~ "No further action"),
         xray_conference_result = case_when(xray_conference_result == 1 ~ "Likely TB",
                                            xray_conference_result == 2 ~ "Possible TB",
                                            xray_conference_result == 3 ~ "Unlikely TB - appears normal",
                                            xray_conference_result == 4 ~ "Unlikely TB - other pathology",
                                            xray_conference_result == 5 ~ "Unlikely TB - old/inactive TB")
  ) %>%
  filter(date_created > "2024-05-01") %>%
  rename(notes_case_conference = notes) %>%
  select("registration_id", "lab_and_diagnostic_id", "is_xpert_testing", "sputum_date_collected", 
         "result_xpert", "xray_conference_result", "outcome_case_conference", 
         "date_case_conference", "actions_not_active_tb", "notes_case_conference")

#MERGE COMPONENTS FROM TB-FREE DATABASE INTO FLATFILE
mortlocks_flatfile_raw <- list(registration, passport, ltbi_hh, ltbi_tx, tb_followup) %>%
  reduce(left_join, by = 'registration_id') %>%
  # filter(date_created > "2024-05-01") %>%
  as_tibble() %>%
  modify_if(is.POSIXt, as_date)

#EXPORT FLATFILE TO EXCEL
write.xlsx(mortlocks_flatfile_raw, "Data/mortlocks_flatfile_raw.xlsx")

##--------------------------------------------------------------------------

#clean flatfile to prepare for analysis
mortlocks_analysis_data <- read_excel("Data/mortlocks_flatfile.xlsx",
                                 guess_max = 20000, col_names = TRUE) %>%
  modify_if(is.POSIXt, as_date) %>%
  mutate(date_of_birth = case_when(date_of_birth < as_date("1930-01-01") ~ NA,
                                   .default = date_of_birth)) %>%
  mutate(age = trunc((date_of_birth %--% date_created) / years(1)),
         age_group = case_when(age <= 4 ~ "0-4",
                               age <= 9 & age > 4 ~ "5-9",
                               age <= 19 & age > 9 ~ "10-19",
                               age <= 39 & age > 19 ~ "20-39",
                               age <= 59 & age > 39 ~ "40-59",
                               age > 59 ~ "60+"),
         age_group = factor(age_group,
                            levels=c("0-4","5-9","10-19","20-39","40-59","60+")),
         
         tst_result = case_when(tst_result < 0 ~ NA,
                                .default = tst_result),
         
         date_onsite_id = mdy(str_extract(`onsite_id`,"(\\d{6,6})")),
         date_of_visit_adj = as.Date(date_of_visit1)+2,
         date_screening = case_when(is.not.na(tst_date_read) ~ tst_date_read,
                                    is.not.na(date_onsite_id) ~ date_onsite_id,
                                    is.not.na(weight) ~ date_of_visit_adj),
         #making counts
         screened_at_clinic = if_else(is.not.na(tst_date_read) |
                                        is.not.na(active_tb) |
                                        is.not.na(result_hd_assessment) |
                                        is.not.na(weight) |
                                        is.not.na(date_screening),
                                      1, 0,
                                      missing = 0),
         
         tst_read = if_else(is.not.na(tst_date_read) | is.not.na(tst_result) | is.not.na(tst_interpretation), 1, 0,
                            missing = 0),
         
         ltbi_diagnosis = if_else( (is.not.na(ltbi_treatment_id)),
                                   1, 0,
                                   missing = 0),
         
         active_tb_tx = if_else(outcome_case_conference == "Active TB", 1, 0,
                                missing = 0),
         
         ltbi_tx_indicated = if_else(ltbi_diagnosis == 1 &
                                       (treatment_recommended == "Y" |
                                          treatment_recommended_later == "Y"),
                                     1,0,
                                     missing = 0),
         
         ltbi_tx_started = case_when(ltbi_diagnosis == 1 & treatment_started == "Y" ~ 1,
                                     .default = 0),
         
         sputum_sent = if_else(tb_sputum == "Y", 1, 0,
                               missing = 0),
         
         hd_further_assessment = if_else(result_hd_assessment == 2, 1, 0,
                                         missing = 0),
         # hd_confirmed = if_else(hd_program_assesment_result == "Leprosy", 1, 0,
         #                        missing = 0),
         hd_prev_given = if_else(hd_prevention == "Y", 1, 0,
                                 missing = 0),
         
         dm_a1c_result = if_else(a1c >= 6.5, 1, 0),
         dm_a1c_or_hx = case_when(dm_a1c_result == 1 ~ 1,
                                  history_diabetes == "Y" ~ 1,
                                  .default = 0),
         new_dm_result = case_when(dm_a1c_result == 1 & history_diabetes != "Y" ~ 1,
                                   .default = 0),
         
         tst_result_10 = case_when(tst_result >= 10 ~ ">= 10 mm TST",
                                   tst_result < 10 ~ "<10 mm TST"),
         tst_pos = if_else(tst_result >= 10, 1, 0,
                           missing = 0),
         tst_neg = if_else(tst_result < 10 & tst_place_visit == "Y", 1, 0,
                           missing = 0),
         
         tst_read_elapse = interval(date_of_visit1, tst_date_read) |>
           as.duration() |> as.numeric(units = "days"),
         tst_read_cat = if_else(tst_read_elapse == 2 | tst_read_elapse == 3,"On time",
                                if_else(tst_read_elapse > 3,"Late",
                                        "Invalid")),
         tst_read_yn =if_else(tst_place_visit == "N", "No TST",
                              if_else(tst_place_visit == "Y" &
                                        (is.not.na(tst_interpretation) |
                                           is.not.na(tst_result)) ,"Read",
                                      "Not read")),
         
         village = str_to_upper(village),
         village = if_else(grepl("KUCHUWA",village), "KUCHUWA",
                           if_else(grepl("NANTAKU",village), "NEPUKOS",
                                   if_else(grepl("FASON", village), "FOSON",
                                           if_else(grepl("FOSON", village), "FOSON",
                                                   village)))),
         
         weight = as.numeric(weight),
         height = as.numeric(height),
         bmi = case_when(age >= 18 ~ round(weight/(height^2)*10000,1),
                         .default = NA),
         bmi = case_when(bmi > 100 | bmi < 13.0 ~ NA,
                         .default = bmi),
         bmi_cat = case_when(bmi >= 30 & bmi < 100 ~ "obese",
                             bmi >= 25 & bmi < 30 ~ "overweight",
                             bmi >= 18.5 & bmi < 25 ~ "normal",
                             bmi >= 13.0 & bmi < 18.5 ~  "underweight"),
         bmi_cat = factor(bmi_cat,
                          levels=c("underweight","normal","overweight","obese")),
         
         current_smoker = case_when(smoking_history == "Current" ~ 1,
                                    .default=0),
         
         
         known_tb_exposure = case_when(tb_disease_exposure == "Y" ~ 1,
                                       .default=0),
         prior_tb = case_when(treated_tb_ltbi == "TB" ~ 1,
                              .default=0),
         
         al_one_symptom = case_when(current_tb_symptoms_2weeks == "Y" |
                                      current_tb_symptoms_any_duration == "Y" |
                                      current_tb_symptoms_coughing_blood == "Y" |
                                      current_tb_symptoms_fatigue == "Y" |
                                      current_tb_symptoms_fever == "Y" |
                                      current_tb_symptoms_weight_loss	 == "Y" |
                                      current_tb_symptoms_sweats == "Y" |
                                      current_tb_symptoms_swollen_lymph	 == "Y" ~ 1,
                                    .default=0),
         
         abnormal_xray = case_when(xray_result_preliminary == "Likely TB" |
                                     xray_result_preliminary == "Possible TB" ~ 1,
                                   .default=0)
  ) %>%
  # select(-date_of_visit_adj, -date_onsite_id) %>%
  modify_if(is.POSIXt, as_date) %>%
  mutate(age_group = case_when(age <= 1 ~ "0-1",
                               age <= 4 & age > 1 ~ "2-4",
                               age <= 9 & age > 4 ~ "5-9",
                               age <= 19 & age > 9 ~ "10-19",
                               age <= 39 & age > 19 ~ "20-39",
                               age <= 59 & age > 39 ~ "40-59",
                               age > 59 ~ "60+"),
         age_group = factor(age_group, 
                            levels=c("0-1","2-4","5-9",
                                     "10-19","20-39","40-59",
                                     "60+")),
         
         tst_result_5 = case_when(tst_result >= 5 ~ ">= 5 mm TST",
                                  tst_result < 5 ~ "<5 mm TST")
  )

write.xlsx(mortlocks_analysis_data, "Data/mortlocks_analysis_data.xlsx")

#CLEAN WORKSPACE
rm(list = ls())