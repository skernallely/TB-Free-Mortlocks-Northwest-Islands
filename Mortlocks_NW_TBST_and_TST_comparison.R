## TB-Free Chuuk R code
## TST vs. TBST comparison

# --------------------------
## Goal for analysis:

## COMPARE TST POSITIVITY BETWEEN PHASE I+II and PHASE IV
## Match Phase I and II islands by 9 year TB case rate to find comparison group
##  Case rate of TBST group (Oneop, Lekinioch, Satawan, Murillo and Ruo)
##  Matches to case rate of TST group (Weno, Tonoas, Fefan)

## Calculate and compare absolute risk difference and rate ratios 
##  for test positivity between cohorts by age group and by sex

## 2-4 y/o TBST group should not be included in this analysis 
##  as they lack comparison group in the TST group
# --------------------------

#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#PACKAGES
library(tidyverse) #pipes, stringr, lubridate
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(fmsb) #risk diff and risk ratios calcs
library(patchwork) #add arrangement for forest plot

#FORMULAS
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#STANDARDS
lagoon_region <- tribble(
  ~municipality, ~region,
  "WENO", "NORTHERN NAMONEAS",
  "FEFEN","LAGOON", 
  "TONOAS","LAGOON",
  "MURILLO", "NORTHWEST",
  "RUO", "NORTHWEST",
  "LEKINIOCH", "MORTLOCKS",
  "ONEOP", "MORTLOCKS",
  "SATOWAN", "MORTLOCKS"
)

#----------------
#DATASETS

## MORTLOCKS AND NORTHWEST DATA SET
## Contains test results for patients who received TBST

tbst_mortlocks_nw <- read_excel("Data/mortlocks_flatfile.xlsx",
           guess_max = 20000, col_names = TRUE) %>%
  mutate(municipality = str_to_upper(municipality),
         test_type = "TBST"
  ) %>%
  merge(lagoon_region, by="municipality") %>%
  select("registration_no", "age", "sex", "municipality", "region",
         "tst_result", "tst_result_10","test_type")

## TB-FREE CHUUK IN WENO, TONOAS AND FEFAN DATA SET
## Contains test results for patients who received TST from 9-year TB rate matched islands

tst_weno_tonoas_fefen <- read_excel("~/PIHOA/TBFC/R Analysis/Data/flatfile_clean.xlsx",
                            guess_max = 20000, col_names = TRUE) %>%
  mutate(age = trunc((date_of_birth %--% date_created) / years(1)),
         municipality = str_to_upper(municipality),
         municipality = case_when(municipality %in% c("MURILLO","RUO",
                                                      "ONEOP","SATOWAN",
                                                      "LEKINIOCH") ~ "WENO",
                                  .default = municipality),
         tst_result = case_when(tst_result < 0 ~ NA,
                                .default = tst_result),
         tst_result_10 = case_when(tst_result >= 10 ~ ">= 10 mm TST",
                                   tst_result < 10 ~ "<10 mm TST"),
         test_type = "TST"
  ) %>%
  filter(municipality %in% c("TONOAS","WENO","FEFEN")) %>%
  merge(lagoon_region, by="municipality")  %>%
  select("registration_no", "age", "sex", "municipality", "region",
         "tst_result", "tst_result_10", "test_type") 

###Combine case-matched cohorts into one dataset
matched_cohort <- tst_weno_tonoas_fefen %>%
  rbind(tbst_mortlocks_nw) %>%
  filter(age > 4 & is.not.na(tst_result_10)) %>%
  mutate(age_group = case_when(age <= 19 ~ "5-19",
                               age <= 39 & age > 19 ~ "20-39",
                               age <= 59 & age > 39 ~ "40-59",
                               age > 59 ~ "60+"),
         age_group = factor(age_group, 
                            levels=c("5-19","20-39","40-59",
                                     "60+")))

#Calculate totals for each test type by age group
match_by_age<-matched_cohort %>%
  group_by(test_type) %>%
  tabyl(age_group, tst_result_10, test_type) %>%
  adorn_totals(c('row','col')) 

#Calculate totals for each test type by sex
match_by_sex <- matched_cohort %>%
  group_by(test_type) %>%
  tabyl(sex, tst_result_10, test_type) %>%
  adorn_totals(c('row','col'))

#----------------
#RISK CALCULATIONS
#calculate absolute risk difference and risk ratio for TBST vs. TST results
#calculate point estimates, 95% confidence intervals and p-values

#Overall
#risk difference 
overall_rd<-riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[5], 
                           match_by_age$`TST`$`>= 10 mm TST`[5], 
                           match_by_age$`TBST`$`Total`[5], 
                           match_by_age$`TST`$`Total`[5], CRC=FALSE, conf.level=0.95) 
#risk ratio
overall_rr<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[5], 
                      match_by_age$`TST`$`>= 10 mm TST`[5], 
                      match_by_age$`TBST`$`Total`[5], 
                      match_by_age$`TST`$`Total`[5], 
                      conf.level=0.95, p.calc.by.independence=TRUE) 

#5-19 years old
#risk difference 
riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[1], 
               match_by_age$`TST`$`>= 10 mm TST`[1], 
               match_by_age$`TBST`$`Total`[1], 
               match_by_age$`TST`$`Total`[1], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_519<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[1], 
                  match_by_age$`TST`$`>= 10 mm TST`[1], 
                  match_by_age$`TBST`$`Total`[1], 
                  match_by_age$`TST`$`Total`[1], 
                  conf.level=0.95, p.calc.by.independence=TRUE) 

#20-39 years old
#risk difference 
riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[2], 
               match_by_age$`TST`$`>= 10 mm TST`[2], 
               match_by_age$`TBST`$`Total`[2], 
               match_by_age$`TST`$`Total`[2], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_2039<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[2], 
                   match_by_age$`TST`$`>= 10 mm TST`[2], 
                   match_by_age$`TBST`$`Total`[2], 
                   match_by_age$`TST`$`Total`[2], 
                   conf.level=0.95, p.calc.by.independence=TRUE) 

#40-59 years old
#risk difference 
riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[3], 
               match_by_age$`TST`$`>= 10 mm TST`[3], 
               match_by_age$`TBST`$`Total`[3], 
               match_by_age$`TST`$`Total`[3], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_4059<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[3], 
                   match_by_age$`TST`$`>= 10 mm TST`[3], 
                   match_by_age$`TBST`$`Total`[3], 
                   match_by_age$`TST`$`Total`[3], 
                   conf.level=0.95, p.calc.by.independence=TRUE) 

#60+ years old
#risk difference 
riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[4], 
               match_by_age$`TST`$`>= 10 mm TST`[4], 
               match_by_age$`TBST`$`Total`[4], 
               match_by_age$`TST`$`Total`[4], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_60up<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[4], 
                   match_by_age$`TST`$`>= 10 mm TST`[4], 
                   match_by_age$`TBST`$`Total`[4], 
                   match_by_age$`TST`$`Total`[4], 
                   conf.level=0.95, p.calc.by.independence=TRUE) 

#MALE
#risk difference 
riskdifference(match_by_sex$`TBST`$`>= 10 mm TST`[2], 
               match_by_sex$`TST`$`>= 10 mm TST`[2], 
               match_by_sex$`TBST`$`Total`[2], 
               match_by_sex$`TST`$`Total`[2], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_male<-riskratio(match_by_sex$`TBST`$`>= 10 mm TST`[2], 
                   match_by_sex$`TST`$`>= 10 mm TST`[2], 
                   match_by_sex$`TBST`$`Total`[2], 
                   match_by_sex$`TST`$`Total`[2], conf.level=0.95, p.calc.by.independence=TRUE) 

#FEMALE
#risk difference 
riskdifference(match_by_sex$`TBST`$`>= 10 mm TST`[1], 
               match_by_sex$`TST`$`>= 10 mm TST`[1], 
               match_by_sex$`TBST`$`Total`[1], 
               match_by_sex$`TST`$`Total`[1], CRC=FALSE, conf.level=0.95)
#risk ratio
rr_female<-riskratio(match_by_sex$`TBST`$`>= 10 mm TST`[1], 
                     match_by_sex$`TST`$`>= 10 mm TST`[1], 
                     match_by_sex$`TBST`$`Total`[1], 
                     match_by_sex$`TST`$`Total`[1], conf.level=0.95, p.calc.by.independence=TRUE) 

#----------------
## PLOTTING RISK ESTIMATES

##Make table with estimates by group with 95% confidence intervals and p-values
risk_ratio_estimates <- tibble(
  model=c("Total","Age Group","5-19","20-39","40-59","60+","Sex","Male","Female"),
  estimate=c(overall_rr$estimate,NA	,
             rr_519$estimate,	rr_2039$estimate,	rr_4059$estimate,	rr_60up$estimate,	NA,
             rr_male$estimate	,	rr_female$estimate),
  log.conf.low=c(overall_rr$conf.int[1],NA	,
                 rr_519$conf.int[1],	rr_2039$conf.int[1],	rr_4059$conf.int[1],	rr_60up$conf.int[1],	NA,
                 rr_male$conf.int[1]	,	rr_female$conf.int[1]),
  log.conf.high=c(overall_rr$conf.int[2],NA	,
                  rr_519$conf.int[2],	rr_2039$conf.int[2],	rr_4059$conf.int[2],	rr_60up$conf.int[2],	NA,
                  rr_male$conf.int[2]	,	rr_female$conf.int[2]),
  p.value=c(overall_rr$p.value,NA,
            rr_519$p.value,	rr_2039$p.value,	rr_4059$p.value,	rr_60up$p.value,	NA,
            rr_male$p.value	,	rr_female$p.value)
  ) %>%
  mutate(model = factor(model, 
                     levels=c("Total","Age Group","5-19","20-39","40-59","60+","Sex","Male","Female"))
  )
  
#Double check raw forest plot data looks correct
risk_ratio_estimates %>%
  ggplot(aes(y = fct_rev(model))) + 
  theme_classic() +
  geom_point(aes(x=estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=log.conf.low, xmax=log.conf.high)) 

# create forest plot on log scale (middle section of figure)
p_mid <-
  risk_ratio_estimates %>%
  filter(model !="-")%>%
  ggplot(aes(y = fct_rev(model))) +
  theme_classic() +
  geom_point(aes(x=estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=log.conf.low, xmax=log.conf.high)) +
  labs(x="Positivity Rate Ratio") +
  coord_cartesian(ylim=c(1,10), xlim=c(0, 2))+
  geom_vline(xintercept = 1, linetype="dashed") +
  annotate("text", x = .4, y = 10, label = "TBST positivity rate < TST positivity rate") +
  annotate("text", x = 1.6, y = 10, label = "TBST positivity rate > TST positivity rate") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())

# wrangle results into pre-plotting table form
res_plot <- risk_ratio_estimates %>%
  mutate(across(c(estimate, log.conf.low, log.conf.high), ~str_pad(round(.x, 2), width=4, pad="0", side="right")),
         estimate_lab = paste0(estimate, " (", log.conf.low, "-", log.conf.high,")"),
         color = c("gray","white","gray","white","gray","white","gray","white","gray")) |>
  mutate(p.value = case_when(p.value < .01 ~ "<0.01", TRUE ~ str_pad(as.character(round(p.value, 2)),width=4,pad="0",side="right"))) |>
  bind_rows(data.frame(model = "", estimate_lab = "Rate Ratio (95% CI)", log.conf.low = "", log.conf.high="",p.value="p-value")) |>
  mutate(model = fct_rev(factor(model, 
                                levels=c("","Total","Age Group","5-19",
                                         "20-39","40-59","60+","Sex","Male","Female"))),
         estimate_lab = case_when(model %in% c("Age Group","Sex") ~ "",
                              .default = estimate_lab)
  )

# left side of plot - hazard ratios
p_left <-
  res_plot  |>
  ggplot(aes(y = model)) + 
  geom_text(aes(x=0, label=model), hjust=0, fontface = "bold") +
  geom_text(aes(x=1, label=estimate_lab), hjust=0, fontface = ifelse(res_plot$estimate_lab == "Rate Ratio (95% CI)", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,4))

# right side of plot - pvalues
p_right <-
  res_plot  |>
  ggplot() +
  geom_text(aes(x=0, y=model, label=p.value), hjust=0, fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
  theme_void() 

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 3),
  area(t = 1, l = 4, b = 30, r = 9),
  area(t = 0, l = 9, b = 30, r = 11))

# final plot arrangement
p_left + p_mid + p_right + plot_layout(design = layout)

## save final figure
ggsave("Figures/forest_plot.png", width=12, height=4)

#clean the workspace
rm(list = ls())
