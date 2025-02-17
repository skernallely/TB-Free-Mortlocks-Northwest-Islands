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

#----WORKING DIRECTORY----
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#---PACKAGES----
library(tidyverse) #pipes, stringr, lubridate
library(readxl) #excel load-in
library(janitor) #allows tabyl & cleaning names
library(ggplot2) #make graphs
library(fmsb) #risk diff and risk ratios calcs
library(patchwork) #add arrangement for forest plot

#----FORMULAS----
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

#----STANDARDS----
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

#-------{DATASETS}---------
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

tst_weno_tonoas_fefen <- read_excel("~/PIHOA/TBFC/R Analysis/Weno_Chuuk_Lagoon/Data/tbfc_analysis_dataset.xlsx",
                            guess_max = 20000, col_names = TRUE) %>%
  mutate(test_type = "TST"
  ) %>%
  filter(municipality %in% c("TONOAS","WENO","FEFEN")) %>%
  rename(lagoon_region = region) %>%
  merge(lagoon_region, by="municipality")  %>%
  select("registration_no", "age", "sex", "municipality", "region",
         "tst_result", "tst_result_10", "test_type") %>%
  filter(age > 4 & is.not.na(tst_result_10))

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
                                     "60+")),
         tst_result_10_bin = case_when(tst_result_10 == ">= 10 mm TST" ~ 1,
                                       tst_result_10 == "<10 mm TST" ~ 0),
         test_type_bin = case_when(test_type == "TBST" ~ 1,
                                   test_type == "TST" ~ 0)
  )

#-------{MATCHED COHORT CHARACTERISTICS}--------
#examine characteristics of matched groups on confounding factors
#determine if cohort is good match across demographics or if
#weighting is required

#take a look at age and sex distribution for tst and tbst
library(tableone)
table2<-CreateTableOne(vars = c('sex','tst_result_10_bin','age','age_group'),
               data=matched_cohort,
               factorVars=c('age_group','sex','tst_result_10_bin'),
               strata=c('test_type'))
table2
# significantly more men and older individuals tested with tbst
# likely due to reduction in clinic return bias seen in lagoon
# both male sex and older age are associated with inc risk of tst pos
# need to match on these variables

# take a look at propensity score now
#make model of the likelihood of being positive by age and sex
ps_model = glm(test_type_bin ~ sex + age, 
      data = matched_cohort, family=binomial)
summary(ps_model)

p_score = fitted(ps_model)

match_with_ps_scores <- matched_cohort %>%
  select(tst_result_10_bin, sex, age, test_type) %>%
  tibble(p.score=p_score)

ggplot(match_with_ps_scores, 
       aes(x=p_score, color=test_type, fill=test_type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 
#graph of propensity for positive test shows more 
#positivity in tbst model

#----{MATCHING}----
#match using MatchIt!
library(MatchIt)

# exact full matching
match_exact <- matchit(test_type_bin ~ sex + age_group, 
                    data=matched_cohort,
                    method = "exact")

summary(match_exact)

match_sum <- summary(match_exact)
plot(match_sum, var.order = "unmatched", xlim=c(0,1))
#matched data looks good

#select matched dataset to calculate positivity and ratios
matched_data <- match.data(match_exact)

#take a look at the distributions of the matched groups
#verify that we removed difference in age and sex distribution we observed earlier
#between tbst and tst groups

#density comparison
plot(match_exact, type = "density", interactive = FALSE,
     test_type_bin ~ sex + age_group)

#significance testing of difference by groups in table
CreateTableOne(vars = c('sex','tst_result_10_bin','age','age_group'),
               data=matched_data,
               factorVars=c('age_group','sex','tst_result_10_bin'),
               strata=c('test_type'))

#looks great! the tst group is now well matched to the tbst group by age and sex

#----{RISK CALC WITH LOG MODELS}---------
#RISK CALCULATIONS with logistic models and estimations for matched data
#Logistic regression model with covariates
fit <- glm(tst_result_10_bin ~ test_type_bin * (sex + age_group),
            data = matched_data,
            weights = weights,
            family = quasibinomial())

#gimme those RR
library(marginaleffects)

#Compute effects; Positivity, PR (CI), RD (CI)
#overall positivity
overall_pos <- avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1)
                )
#overall RR
overall_rr <- avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1),
                comparison = "lnratioavg",
                transform = "exp")
#overall RD
overall_rd <- avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1)
                )
##AGE GROUP

#age 5-19
# positivity
pos_519<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "5-19")
                )
#RR
rr_519<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "5-19"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_519<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "5-19"),
                )

#age 20-39
# positivity
pos_2039<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "20-39")
                )
#RR
rr_2039<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "20-39"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_2039<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "20-39"),
                )

#age 40-59
# positivity
pos_4059<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "40-59")
)
#RR
rr_4059<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "40-59"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_4059<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "40-59"),
)

#age 60 up
# positivity
pos_60up<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "60+"),
)
#RR
rr_60up<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "60+"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_60up<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & age_group == "60+"),
)

#male
# positivity
pos_male<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "M"),
) 
#RR
rr_male<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "M"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_male<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "M"),
)

#female
# positivity
pos_female<-avg_predictions(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "F"),
) 
#RR
rr_female<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "F"),
                comparison = "lnratioavg",
                transform = "exp")
#RD
rd_female<-avg_comparisons(fit,
                variables = c("test_type_bin"),
                vcov = "HC3",
                newdata = subset(test_type_bin == 1 & sex == "F"),
)


#----{#TST and TBST POSITIVITY TABLE}------------
#calculate TST and TBST positivity by age and sex and make a nice table for the top
#of the figure
library(pacman)
p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse,
  scales)      # data management, summary, and visualization

#Make table of positivity and risk difference by group
pos_rd_table <- tibble(
  type=c("Total","Age Group (years)","Age Group (years)","Age Group (years)","Age Group (years)",
         "Sex","Sex"),
  group=c("Total","5-19","20-39","40-59","60+","Female","Male"),
  pos_tbst=c(overall_pos$estimate[2],
             pos_519$estimate[2],	pos_2039$estimate[2],	pos_4059$estimate[2],	pos_60up$estimate[2],
             pos_female$estimate[2]	,	pos_male$estimate[2]),
  pos_conf_low_tbst=c(overall_pos$conf.low[2],
                 pos_519$conf.low[2],	pos_2039$conf.low[2],	pos_4059$conf.low[2],	pos_60up$conf.low[2],
                 pos_female$conf.low[2]	,	pos_male$conf.low[2]),
  pos_conf_high_tbst=c(overall_pos$conf.high[2],
                  pos_519$conf.high[2],	pos_2039$conf.high[2],	pos_4059$conf.high[2],	pos_60up$conf.high[2],
                  pos_female$conf.high[2]	,	pos_male$conf.high[2]),
  pos_tst=c(overall_pos$estimate[1],
             pos_519$estimate[1],	pos_2039$estimate[1],	pos_4059$estimate[1],	pos_60up$estimate[1],
             pos_female$estimate[1]	,	pos_male$estimate[1]),
  pos_conf_low_tst=c(overall_pos$conf.low[1],
                      pos_519$conf.low[1],	pos_2039$conf.low[1],	pos_4059$conf.low[1],	pos_60up$conf.low[1],
                      pos_female$conf.low[1]	,	pos_male$conf.low[1]),
  pos_conf_high_tst=c(overall_pos$conf.high[1],
                       pos_519$conf.high[1],	pos_2039$conf.high[1],	pos_4059$conf.high[1],	pos_60up$conf.high[1],
                       pos_female$conf.high[1]	,	pos_male$conf.high[1]),
  rd=c(overall_rd$estimate[1],
            rd_519$estimate[1],	rd_2039$estimate[1],	rd_4059$estimate[1],	rd_60up$estimate[1],
            rd_female$estimate[1]	,	rd_male$estimate[1]),
  rd_conf_low=c(overall_rd$conf.low[1],
                     rd_519$conf.low[1],	rd_2039$conf.low[1],	rd_4059$conf.low[1],	rd_60up$conf.low[1],
                     rd_female$conf.low[1]	,	rd_male$conf.low[1]),
  rd_conf_high=c(overall_rd$conf.high[1],
                      rd_519$conf.high[1],	rd_2039$conf.high[1],	rd_4059$conf.high[1],	rd_60up$conf.high[1],
                      rd_female$conf.high[1]	,	rd_male$conf.high[1])
  ) %>%
  mutate_if(is.numeric,label_percent(.1)) %>%
  mutate(
    pos_conf_tbst=paste0("[",pos_conf_low_tbst,", ",pos_conf_high_tbst,"]"),
    pos_conf_tst=paste0("[",pos_conf_low_tst,", ",pos_conf_high_tst,"]"),
    rd_conf=paste0("[",rd_conf_low,", ",rd_conf_high,"]")
  ) %>%
  select(type, group, pos_tbst, pos_conf_tbst, pos_tst, pos_conf_tst, rd, rd_conf)

# small border style
border_style = officer::fp_border(color="black", width=.5)
border_style_thick = officer::fp_border(color="black", width=1)


#make the table pretty
sectiona_table <-
  flextable(pos_rd_table) %>%
  autofit() %>%
  width(j=1, width = .9) %>% 
  width(j=c(3,5,7), width = 1.2) %>% 
    
  merge_v(j = 1) %>%
  valign(j = 1, valign = "top") %>%
    
  add_header_row(
    top = TRUE,                # New header goes on top of existing header row
    values = c("",     # Header values for each column below
               "",
               paste0("TBST-tested group", "\n", "(ESS=1,267)"), 
               "",
               paste0("TST-tested group", "\n", "(ESS=7,701)"), 
               "",
               "Absolute Difference",
               "")) %>% 
  
  set_header_labels(         # Rename the columns in original header row
    type = "",
    group = "", 
    pos_tbst = "Test positivity", 
    pos_conf_tbst = "95% CI", 
    pos_tst = "Test positivity", 
    pos_conf_tst = "95% CI",
    rd = "Estimate", 
    rd_conf = "95% CI*")  %>% 
  
  merge_at(i = 1, j = 3:4, part = "header") %>% # Horizontally merge columns 2 to 4 in new header row
  merge_at(i = 1, j = 5:6, part = "header") %>% # Horizontally merge columns 2 to 4 in new header row
  merge_at(i = 1, j = 7:8, part = "header") %>% # Horizontally merge columns 2 to 4 in new header row
  merge_at(i = 1, j = 1:2, part = "body") %>% 

  #align headers for gropus in center, headers for body and text to right
  flextable::align(align = "center", i=1,j=1:8, part = "header") %>%
  flextable::align(align = "right", i=2,j=1:8, part = "header") %>%
  flextable::align(align = "right", i=1:7,j=3:8, part = "body") %>%
    
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bold(i = 1, bold = TRUE, part = "body")  %>%      # adjust bold face of total row
  
  # add_footer_lines(paste0(
  #   "*Estimated test positivity, absolute difference and respective 95% CIs from g-computation in the matched sample, inclusive of weighting")) %>%
  
  # Remove all existing borders
  border_remove() %>%  
  
  # add header horizontal lines
  hline_top(part = "header", border = border_style_thick) %>%   # at top of header
  hline_bottom(part = "header", border = border_style_thick) %>%   # at bottom of header 
  hline_bottom(border = border_style_thick) %>%   # at bottom
  
  # add vertical lines to separate TBST, TST and total sections
  vline(part = "all", j = 2, border = border_style) %>%   # at column 2 
  vline(part = "all", j = 4, border = border_style) %>%   # at column 4
  vline(part = "all", j = 6, border = border_style) %>% # at column 6
  
  # add horizontal lines to separate overall, age group and sex sections
  hline(part = "body", i = 1, border = border_style) %>%   # at row 1 
  hline(part = "body", i = 5, border = border_style)       # at row 5
  
    
sectiona_table

#------{PLOTTING RISK ESTIMATES}----------
## PLOTTING RISK ESTIMATES

##Make table with estimates by group with 95% confidence intervals and p-values
risk_ratio_estimates <- tibble(
  model=c("Total","Age Group","5-19","20-39","40-59","60+","Sex","Female","Male"),
  estimate=c(overall_rr$estimate,NA	,
             rr_519$estimate,	rr_2039$estimate,	rr_4059$estimate,	rr_60up$estimate,	NA,
             rr_female$estimate	,	rr_male$estimate),
  log.conf.low=c(overall_rr$conf.low,NA	,
                 rr_519$conf.low,	rr_2039$conf.low,	rr_4059$conf.low,	rr_60up$conf.low,	NA,
                 rr_female$conf.low	,	rr_male$conf.low),
  log.conf.high=c(overall_rr$conf.high,NA	,
                  rr_519$conf.high,	rr_2039$conf.high,	rr_4059$conf.high,	rr_60up$conf.high,	NA,
                  rr_female$conf.high	,	rr_male$conf.high),
  p.value=c(overall_rr$p.value,NA,
            rr_519$p.value,	rr_2039$p.value,	rr_4059$p.value,	rr_60up$p.value,	NA,
            rr_female$p.value	,	rr_male$p.value)
  ) %>%
  mutate(model = factor(model, 
                     levels=c("Total","Age Group","5-19","20-39","40-59","60+","Sex","Female","Male"))
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
  labs(x="Prevalence Ratio") +
  coord_cartesian(ylim=c(1,10), xlim=c(0, 2))+
  geom_vline(xintercept = 1, linetype="dashed") +
  annotate("text", x = .4, y = 10, label = "TBST positivity < TST positivity") +
  annotate("text", x = 1.6, y = 10, label = "TBST positivity > TST positivity") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())

# wrangle results into pre-plotting table form
res_plot <- risk_ratio_estimates %>%
  mutate(across(c(estimate, log.conf.low, log.conf.high), ~str_pad(round(.x, 2), width=4, pad="0", side="right")),
         estimate_lab = paste0(estimate, " (", log.conf.low, "-", log.conf.high,")"),
         color = c("gray","white","gray","white","gray","white","gray","white","gray")) |>
  mutate(p.value = case_when(p.value < .001 ~ "<0.001", TRUE ~ str_pad(as.character(round(p.value, 3)),width=4,pad="0",side="right"))) |>
  bind_rows(data.frame(model = "", estimate_lab = "PR (95% CI)", log.conf.low = "", log.conf.high="",p.value="p-value")) |>
  mutate(model = fct_rev(factor(model, 
                                levels=c("","Total","Age Group","5-19",
                                         "20-39","40-59","60+","Sex","Female","Male"))),
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
  geom_text(aes(x=0, y=model, label=p.value), hjust=1, 
            fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
  theme_void() 

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 13, l = 0, b = 30, r = 4),
  area(t = 13, l = 4, b = 30, r = 11),
  area(t = 13, l = 12, b = 30, r = 13),
  area(t = 1, l = 1, b = 13, r = 12.5))

# final plot arrangement
p_left + p_mid + p_right + gen_grob(sectiona_table,
                                    fit="width") +
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = list(c("B","","","A")),
                  caption=paste0(
                    "TBST: tuberculosis antigen-based skin test; ESS: effective sample size; TST: tuberculin skin test; CI: confidence interval; PR: prevalence ratio",
                    "\n",
                    "*Estimated test positivity, absolute difference, prevalence ratio and respective 95% CIs from g-computation in the matched sample, inclusive of weighting"),
                  theme = theme(plot.caption = element_text(size=10, hjust = 0))
  )

## save final figure
ggsave("Figures/forest_plot_with_age_sex_matching.png", width=10, height=8)

#clean the workspace
rm(list = ls())



#-----{RISK CALC WITH GTSUMMARY}-----------
#RISK CALCULATIONS with GTSummary for unmatched data
#calculate absolute risk difference and risk ratio for TBST vs. TST results
#calculate point estimates, 95% confidence intervals and p-values

#Calculate totals for each test type by age group
# match_by_age<-matched_data %>%
#   group_by(test_type) %>%
#   tabyl(age_group, tst_result_10, test_type) %>%
#   adorn_totals(c('row','col')) 
# 
# #Calculate totals for each test type by sex
# match_by_sex <- matched_data %>%
#   group_by(test_type) %>%
#   tabyl(sex, tst_result_10, test_type) %>%
#   adorn_totals(c('row','col'))
# 
# #Overall
# #risk difference 
# overall_rd<-riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[5], 
#                            match_by_age$`TST`$`>= 10 mm TST`[5], 
#                            match_by_age$`TBST`$`Total`[5], 
#                            match_by_age$`TST`$`Total`[5], CRC=FALSE, conf.level=0.95) 
# #risk ratio
# overall_rr<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[5], 
#                       match_by_age$`TST`$`>= 10 mm TST`[5], 
#                       match_by_age$`TBST`$`Total`[5], 
#                       match_by_age$`TST`$`Total`[5], 
#                       conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #5-19 years old
# #risk difference 
# riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[1], 
#                match_by_age$`TST`$`>= 10 mm TST`[1], 
#                match_by_age$`TBST`$`Total`[1], 
#                match_by_age$`TST`$`Total`[1], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_519<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[1], 
#                   match_by_age$`TST`$`>= 10 mm TST`[1], 
#                   match_by_age$`TBST`$`Total`[1], 
#                   match_by_age$`TST`$`Total`[1], 
#                   conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #20-39 years old
# #risk difference 
# riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[2], 
#                match_by_age$`TST`$`>= 10 mm TST`[2], 
#                match_by_age$`TBST`$`Total`[2], 
#                match_by_age$`TST`$`Total`[2], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_2039<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[2], 
#                    match_by_age$`TST`$`>= 10 mm TST`[2], 
#                    match_by_age$`TBST`$`Total`[2], 
#                    match_by_age$`TST`$`Total`[2], 
#                    conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #40-59 years old
# #risk difference 
# riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[3], 
#                match_by_age$`TST`$`>= 10 mm TST`[3], 
#                match_by_age$`TBST`$`Total`[3], 
#                match_by_age$`TST`$`Total`[3], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_4059<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[3], 
#                    match_by_age$`TST`$`>= 10 mm TST`[3], 
#                    match_by_age$`TBST`$`Total`[3], 
#                    match_by_age$`TST`$`Total`[3], 
#                    conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #60+ years old
# #risk difference 
# riskdifference(match_by_age$`TBST`$`>= 10 mm TST`[4], 
#                match_by_age$`TST`$`>= 10 mm TST`[4], 
#                match_by_age$`TBST`$`Total`[4], 
#                match_by_age$`TST`$`Total`[4], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_60up<-riskratio(match_by_age$`TBST`$`>= 10 mm TST`[4], 
#                    match_by_age$`TST`$`>= 10 mm TST`[4], 
#                    match_by_age$`TBST`$`Total`[4], 
#                    match_by_age$`TST`$`Total`[4], 
#                    conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #MALE
# #risk difference 
# riskdifference(match_by_sex$`TBST`$`>= 10 mm TST`[2], 
#                match_by_sex$`TST`$`>= 10 mm TST`[2], 
#                match_by_sex$`TBST`$`Total`[2], 
#                match_by_sex$`TST`$`Total`[2], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_male<-riskratio(match_by_sex$`TBST`$`>= 10 mm TST`[2], 
#                    match_by_sex$`TST`$`>= 10 mm TST`[2], 
#                    match_by_sex$`TBST`$`Total`[2], 
#                    match_by_sex$`TST`$`Total`[2], conf.level=0.95, p.calc.by.independence=TRUE) 
# 
# #FEMALE
# #risk difference 
# riskdifference(match_by_sex$`TBST`$`>= 10 mm TST`[1], 
#                match_by_sex$`TST`$`>= 10 mm TST`[1], 
#                match_by_sex$`TBST`$`Total`[1], 
#                match_by_sex$`TST`$`Total`[1], CRC=FALSE, conf.level=0.95)
# #risk ratio
# rr_female<-riskratio(match_by_sex$`TBST`$`>= 10 mm TST`[1], 
#                      match_by_sex$`TST`$`>= 10 mm TST`[1], 
#                      match_by_sex$`TBST`$`Total`[1], 
#                      match_by_sex$`TST`$`Total`[1], conf.level=0.95, p.calc.by.independence=TRUE) 
