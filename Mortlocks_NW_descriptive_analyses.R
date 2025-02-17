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
library(gtsummary) #allows summary tabyl and p-value
library(ggplot2) #make graphs
library(ggthemes) #makes prettier graphs
library(patchwork) #add arrangement

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)
#------------------------------------------------------------------------------

##---DATA----
#load in clean flatfile
mortlocks_flatfile <- read_excel("Data/mortlocks_analysis_data.xlsx")

#TBST demographics
# make subset of data for only people 2 and up who received a TBST
tbst_only<-mortlocks_flatfile %>%
  filter(age_group != "0-1" & is.not.na(tst_result_10)) %>%
  select(sex,age_group, municipality, tb_disease_exposure, treated_tb_ltbi,
         current_tb_symptoms_none, active_tb, smoking_history,tst_result_10,
         active_tb_tx, ltbi_diagnosis, age, prior_tb
  ) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("2-4","5-9","10-19","20-39","40-59","60+")),)
##----Demographic tables------
#TABLE 1
#demographic summary table of people screened with a TBST
sumtable(tbst_only)

#TABLE 2
#determine number of active TB cases by TBST result
tbst_only %>%
  group_by(tst_result_10) %>%
  summarise(screened = n(),
            active_tb = sum(active_tb_tx == 1),
            ltbi = sum(tst_result_10 == ">= 10 mm TST" & active_tb_tx != 1 &
                         prior_tb != 1)
  ) %>%  
  pivot_longer(-tst_result_10) %>% 
  pivot_wider(names_from=tst_result_10, values_from=value) 
  

#----{TST POSITIVITY COUNTS and RATES}-----
#number of people with TSTs placed
tbst_only %>%
  count()

#number of people with TST results
tbst_only %>%
  count(is.not.na(tst_result_10))

#number of people with TBST positive >=10mm
tbst_only %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(tst_result_10)

#tst positivity by age group
tbst_only %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(age_group, tst_result_10) %>%
  adorn_totals() %>%
  adorn_percentages() %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(tst_result_10 != "<10 mm TST" &
           age_group != "0-1" & is.not.na(age_group))

#significance testing for age group and sex
tbst_only %>%
  select(tst_result_10, sex, age_group) %>%
  filter(is.not.na(tst_result_10)) %>%
  tbl_strata(
    strata = age_group,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = sex, missing = "no") %>%
      add_n()  %>%
      add_p(),
    .header = "**{strata}**, N = {n}"
  )

#tst positivity by sex
tbst_only %>%
  filter(is.not.na(tst_result_10)) %>%
  tbl_summary(by = sex, include = c(tst_result_10),
              digits = ~ 1) %>%
  add_p()

#tst positivity by municipality
tbst_only %>%
  filter(is.not.na(tst_result_10)) %>%
  tabyl(municipality, tst_result_10) %>%
  adorn_totals(c('row','col')) %>%
  filter(Total > 30) %>%
  adorn_percentages() %>%
  select(-Total) %>%
  pivot_longer(cols=2:3, names_to="tst_result_10", values_to = "pct") %>%
  filter(tst_result_10 != "<10 mm TST")

##positivity by sex, age_group and island
tbst_only %>%
  filter(is.not.na(tst_result_10) & age_group != "0-4" & is.not.na(age_group)) %>%
  group_by(sex,municipality,age_group) %>%
  mutate(sex = case_when(sex == 'F' ~ 'Female',
                         sex == 'M' ~ 'Male',
                         .default = sex)
  ) %>%
  summarise(num_tst = n(), 
            tst_pos = sum(tst_result_10 == ">= 10 mm TST"),
            pct = tst_pos / num_tst) %>%
  arrange(desc(pct))

####-----{TST positivity graph template}----

##make test positivity graphs
make_tbst_pos_graphs <- function(data, fill) {
  ggplot(data = data, 
         aes(x=age_group, y=pct, fill = {{fill}})) +
    geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
    labs(
      y="% of TSTs read",
      x="Age Group") +  # title and caption
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.background = element_blank(),
          legend.title = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
    scale_fill_tableau(palette = "Superfishel Stone") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(labels = percent) # add pct
}

##----POSITIVITY GRAPH----
#tst positivity by age group and sex
tbst_pos_age_sex <- tbst_only %>%
  filter(is.not.na(tst_result_10) & age_group != "0-4" & is.not.na(age_group)) %>%
  group_by(sex,age_group) %>%
  mutate(sex = case_when(sex == 'F' ~ 'Female',
                         sex == 'M' ~ 'Male',
                         .default = sex)
  ) %>%
  summarise(num_tbst = n(), 
            tbst_pos = sum(tst_result_10 == ">= 10 mm TST"),
            pct = tbst_pos / num_tbst)

##make graph by sex grouped
grouped_sex_pos <-
  ggplot(data = tbst_pos_age_sex, 
         aes(x=age_group, y=pct, fill = sex)) +
  geom_bar(stat="identity",position = "dodge") +
  theme_hc() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.background = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        text = element_text(size = 14))  + # turn off minor 
  labs(
    x="Age group (years)",
    fill = "Sex") +  # title and caption
  scale_fill_manual(values=c("#70ad47","#7cafdd")) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(name="TBST positivity (≥ 10 mm)",
                     labels = percent)
grouped_sex_pos

#Save grouped bar chart with positivity by age and sex
ggsave(plot=grouped_sex_pos,
       "Figures/Grouped TBST positivity rate by age and sex.png",
       width = 1280, height = 1024, units = "px", scale = 2, dpi=300)


#clean the workspace
rm(list = ls())

##----table of age group and sex-----
library(pacman)
p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse,
  scales)      # data management, summary, and visualization

#Make table of population table by age and sex
pop_table <- tbst_pos_age_sex %>%
  select(-tbst_pos,-pct) %>%
  pivot_wider(names_from=age_group,values_from = num_tbst) %>%
  adorn_totals(c("row")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position ="front")

# small border style
border_style = officer::fp_border(color="black", width=.5)
border_style_thick = officer::fp_border(color="black", width=1)

#make the table pretty
sectiona_table_pos <-
  flextable(pop_table) %>%
  autofit() %>%
    
  add_header_row(
    top = TRUE,                # New header goes on top of existing header row
    values = c("Sex",     # Header values for each column below
               "Age Group (years)","","","","","")) %>% 
  
  set_header_labels(         # Rename the columns in original header row
    sex = "")  %>% 
  merge_at(i = 1, j = 2:7, part = "header") %>% # Horizontally merge columns 2 to 4 in new header row
  merge_at(i = 1:2, j = 1, part = "header") %>% 

  #align headers for gropus in center, headers for body and text to right
  flextable::align(align = "center", i=1,j=2, part = "header") %>%
  flextable::align(align = "right", i=2,j=1:7, part = "header") %>%
  flextable::align(align = "right", i=1:3,j=2:7, part = "body") %>%
  
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bold(i = 3, bold = TRUE, part = "body")  %>%      # adjust bold face of total row

  # Remove all existing borders
  border_remove() %>%
  
  # add header horizontal lines
  hline_top(part = "header", border = border_style_thick) %>%   # at top of header
  hline_bottom(part = "header", border = border_style_thick) %>%   # at bottom of header 
  hline_bottom(border = border_style_thick) %>%
  
  #add padding?
  padding(part="all",j = 1, padding.left = 30)
  
sectiona_table_pos

##----putting together table and positivity graph----

# layout design (top, left, bottom, right)
layout <- c(
  area(t = 15, l = 0, b = 40, r = 1),
  area(t = 1, l = 0, b = 13, r = 1))

# final plot arrangement
grouped_sex_pos / gen_grob(sectiona_table_pos,
                                    fit="auto") +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = list(c("B","A")),
                  caption=paste0(
                    "TBST: tuberculosis antigen-based skin test"),
                  theme = theme(plot.caption = element_text(size=10, hjust = 0))
  )

## save final figure
ggsave("Figures/positivity_by_age_sex_with_sample_sizes.png",
       width = 1280, height = 1280, units = "px", scale = 1.5, dpi=300)
