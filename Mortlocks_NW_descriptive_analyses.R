## TB-Free Chuuk R code
## Mortlocks and NW Descriptive Analysis Code

## Goal: Analyze TB-Free screening data from the Mortlocks and Northwest Islands
##        Calculate descriptive statistics for Table 1, TB/LTBI rates by TBST result
##        for Table 2 and additional situation report tables for reporting

#------------------------------------------------------------------------------
#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#PACKAGES
library(tidyverse) #pipes, stringr, lubridate, scales
library(readxl) #excel load-in
library(openxlsx) #export excel files
library(vtable) #allows sumtable
library(janitor) #tabyl and variable name cleaning

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)
#------------------------------------------------------------------------------

##DATA
#load in clean flatfile
mortlocks_flatfile <- read_excel("Data/mortlocks_analysis_data.xlsx")

#TBST demographics
# make subset of data for only people 2 and up who received a TBST
tbst_only<-mortlocks_flatfile %>%
  filter(age_group != "0-1" & is.not.na(tst_result_10)) %>%
  select(sex,age_group, municipality, tb_disease_exposure, treated_tb_ltbi,
         current_tb_symptoms_none, active_tb, smoking_history,tst_result_10,
         active_tb_tx, ltbi_diagnosis, age
  )

#TABLE 1
#demographic summary table of people screened with a TBST
sumtable(tbst_only)

#TABLE 2
#determine number of active TB cases by TBST result
table(tbst_only$tst_result_10,tbst_only$active_tb_tx)

#---------------------------------------------------
#SITREP

#Make TB outcome summary tables for all patients screened at clinics
##BY AGE AGROUP
sitrep_pivots_age <-   mortlocks_flatfile %>%
  group_by(age_group) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            # tested
            no_tested = sum(tst_place_visit == "Y"),
            
            # read
            no_read = sum(tst_read_yn == "Read"),
            pct_read = no_read/no_tested,
            
            no_neg = sum(tst_result_10 == "<10 mm TST", na.rm=T),
            
            # pos 10 mm
            no_pos_10 = sum(tst_result_10 == ">= 10 mm TST", na.rm=T),
            pct_pos_10 = no_pos_10/no_read,
            
            no_active_tb = sum(active_tb_tx == 1),
            # latent
            no_latent = sum(ltbi_diagnosis == 1),
            pct_latent = no_latent/no_read,
            
            no_ltbi = sum(tst_pos),
            no_ltbi_recommended = sum(ltbi_tx_indicated),
            no_ltbi_started = sum(ltbi_tx_started),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_result, na.rm=T),
            no_new_diabetes = sum(new_dm_result)
  ) %>%
  adorn_totals()

##BY SEX
sitrep_pivots_sex<- mortlocks_flatfile %>%
  group_by(sex) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),
            
            no_ltbi = sum(tst_pos),
            no_ltbi_recommended = sum(ltbi_tx_indicated),
            no_ltbi_started = sum(ltbi_tx_started),
            
            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_result, na.rm=T),
            no_new_diabetes = sum(new_dm_result)
  ) %>%
  adorn_totals()


#clean the workspace
rm(list = ls())
