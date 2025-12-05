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
library(scales) # get nice formatting for percents and commas
library(flextable) #make a nice table 1 or 2


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
         active_tb_tx, ltbi_diagnosis, age, prior_tb, xray_indicated,
         xray_result_preliminary,is_xpert_testing, lab_and_diagnostic_id
  ) %>%
  mutate(age_group = factor(age_group, 
                            levels=c("2-4","5-9","10-19","20-39","40-59","60+")),
         ltbi = case_when(
           tst_result_10 == ">= 10 mm TST" & active_tb_tx != 1 &
           prior_tb != 1 ~ 1,
           .default = 0)
  )


##----Algorithm numbers----
#number tbst placed, read and not read
mortlocks_flatfile %>%
  filter(tst_read_yn != "No TST" & age_group != "0-1") %>%
  tabyl(tst_read_yn) %>%
  adorn_totals()

#number positive and negative
tbst_only %>%
  tabyl(tst_result_10)

#among negative, how many referred for xray and how many reassuring exam?
#among all screened, how many referred for xray?
tbst_only %>%
  tabyl(tst_result_10,xray_indicated) %>%
  adorn_totals()

#among all screened, how many actually received xray?
tbst_only %>%
  filter(xray_indicated == "Y") %>%
  group_by(tst_result_10) %>%
  summarise(done = sum(xray_result_preliminary != "Not done"),
            not_done  = sum(xray_result_preliminary == "Not done"))

#among all screened, how with/without concerning xray/sent for CC?
tbst_only %>%
  filter(xray_indicated == "Y") %>%
  mutate(sent_for_case_conference = case_when(is.not.na(lab_and_diagnostic_id) ~ "Y",
                                              .default = "N")) %>%
  tabyl(sent_for_case_conference) %>%
  adorn_totals()

#how many sputa collected? 
#how many diagnosed with tb?
tbst_only %>%
  tabyl(is_xpert_testing,active_tb_tx) %>%
  adorn_totals(c("row","col"))


#how many xray but not treated for anything
tbst_only %>%
  filter(xray_result_preliminary != "Not done" & xray_indicated == "Y" &
           is.na(lab_and_diagnostic_id)) %>%
  summarise(n = n(),
            pos =  sum(tst_result_10 == ">= 10 mm TST"),
            neg =  sum(tst_result_10 == "<10 mm TST"),
            ltbi_diagnosis = sum(tst_result_10 == ">= 10 mm TST" &
                                   prior_tb != 1),
            not_retreated = sum(tst_result_10 == ">= 10 mm TST" &
                                         prior_tb == 1),
            nada = sum(tst_result_10 == "<10 mm TST")
  )

#how many ltbi
tbst_only %>%
  summarise(ltbi = sum(tst_result_10 == ">= 10 mm TST" & active_tb_tx != 1 &
                         prior_tb != 1))

#how many positive but no treatment
tbst_only %>%
  filter(tst_result_10 == ">= 10 mm TST" & active_tb_tx != 1 &
           prior_tb == 1) %>%
   View()
  summarise(not_retreated = sum(tst_result_10 == ">= 10 mm TST" & active_tb_tx != 1 &
                         prior_tb == 1))

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
  
#NEW TABLE 2 WITH OUTCOMES

## Make functions to quickly summarize each type of variable for table 1
#categorical and binary variables
table_1_cat <- function(variable,level,population){
  sprintf("%s (%2.1f)",
          format(sum(variable == level,na.rm = T), big.mark=","),
          sum(variable == level,na.rm = T)/(population-sum(is.na(variable)))*100)
}
## Make label function for table 1 variable names
labeller <- function(var,string){
  stringr::str_detect(var,string)
}

total_label <- paste0("Total",common::supsc(2))
tb_label <- paste0("Outcomes",common::supsc(3))
tbdx_label <- paste0("Diagnosed with TB disease",common::supsc(4))
tbi_label <- paste0("Diagnosed with TB infection",common::supsc(4))


row_totals <- tbst_only |>
  summarise(n_total = length(sex))

## Create table 1
table_2 <- tbst_only |>
  dplyr::mutate(group = "total", #assign data for all births descriptive stats
                group = factor(group,
                               levels = c("total", "pos", "neg"),
                               labels = c("total", "pos", "neg")) )|>
  rbind(tbst_only |> # add data for positive TBST
          dplyr::mutate(group = "pos") |>
          dplyr::filter(tst_result_10 == ">= 10 mm TST")) |>
  rbind(tbst_only |> # add data for negative TBST
          dplyr::mutate(group = "neg") |>
          dplyr::filter(tst_result_10 == "<10 mm TST")) |> 
  dplyr::group_by(group) |>
  dplyr::summarise(
    #overall counts
    overall_n = length(sex),
    total_n = sprintf("%s (%2.1f)",
                          format(overall_n, big.mark=","),
                          overall_n/(row_totals$n_total)*100),

    #TB data
    tb_label = tb_label,
    #tb disease
    tb_Y = table_1_cat(active_tb_tx,1,overall_n),
    #tbi
    tbi_Y = table_1_cat(ltbi,1,overall_n),

    #make overall count character after it's been useful as numeric in above calculations
    overall_n = sprintf("%d",overall_n)
  ) |>
  tidyr::pivot_longer( # pivot longer for rearranging
    cols=c(everything(),-group),
    names_to ="label",
    values_to = "value") |>
  tidyr::pivot_wider( # pivot wider to descriptive statistics by groups with pretty labels
    names_from ="group",
    values_from = "value"
  ) |>
  dplyr::filter(label!="overall_n") |>
  dplyr::mutate(
    Variable = dplyr::case_when( # add cleaned up variable labels for final table 1
      labeller(label,"total_n") ~ total_label,

      labeller(label,"tb_label") ~ tb_label,
      labeller(label,"tb_Y") ~ tbdx_label,
      labeller(label,"tbi_Y") ~ tbi_label,
      .default = label)) |>
  dplyr::select(Variable,pos,neg,total) #get arrange table1

#make table 2
flex_table_2 <- table_2 |>
  flextable() |>
  theme_booktabs(bold_header = F) |> #add basic table format
  merge_h(i = 1:4) |> #merge headers for groups
  set_header_labels(Variable = NA, #clean up header
                    total = paste("Total screened with TBST","n (%)",sep = "\n"),
                    pos = paste(paste0("TBST positive",common::supsc(1)),"n (%)",sep = "\n"),
                    neg = paste("TBST negative","n (%)",sep = "\n")) |>
  align(align = "center", part="header") |> #align header
  align(align = "center", j=2:4) |> #align data cells  
  align(align = "right", i = c(3:4), j=1) |> #align row sublabels
  add_footer_lines( #add footer
    as_paragraph(
      "Abbreviations: TBST - ",
      as_i("Mycobacterium tuberculosis"),
      " antigen-based skin test; TST - Tuberculin skin test; TB - Tuberculosis",
      "\n",
      as_sup("1")," Positive TBST result could indicate new or prior TB infection or TB disease; TB disease diagnosis relied on further evaluations following TBST.",
      "\n",
      as_sup("2")," Percentages are presented as row percentages.",
      "\n",
      as_sup("3")," 12 people with a positive TBST result were not diagnosed with TB infection or TB disease because they had received treatment for TB infection or TB disease in the past 2 years; these people were not retreated.",
      "\n",
      as_sup("4")," Percentages are presented as column percentages."      
    )) |>
  set_table_properties(layout = "autofit") |> #autofit widths
  line_spacing(space = .5, i = c(3:4)) |> #get compact spacing for sublabels
  fontsize(size=10, part="all") |> #set fontsizes to match table in Project2 word doc
  fontsize(size=9, part="footer") |>
  add_header_lines(values=as_paragraph(as_i("Table 2.")," Mycobacterium tuberculosis antigen-based skin tests (TBSTs) results and TB infection and disease diagnoses among participants screened, Mortlock and Northwest Islands Community-Wide TB Active Case Finding, May-June 2024")) |># add title
  border(i=1,part="header",border.top  = officer::fp_border(color = "white")) |> #remove top bar
  border(i=2,j=1,part="header",border.bottom  = officer::fp_border(color = "white")) #remove bottom bar in header

#view beautiful table
flex_table_2

# Save table1 as word doc
flex_table_2 |> save_as_docx(path="Figures/table2.docx")


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
##totals by age group and sex
tbst_labels <- tbst_only %>%
  filter(is.not.na(tst_result_10) & age_group != "0-4" & is.not.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(num_tbst = n()) %>%
  mutate(labels = paste(age_group,paste0("n=",num_tbst),sep="\n")
  ) %>%
  mutate(labels = factor(labels, levels = labels))

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
            pct = tbst_pos / num_tbst) %>%
  merge(tbst_labels, by="age_group")

##make graph by sex grouped
grouped_sex_pos <-
  ggplot(data = tbst_pos_age_sex, 
         aes(x=labels, y=pct, fill = sex, 
             # label=round(pct*100, digits = 1))) +
             label=percent(pct, accuracy = 0.1))) +
  geom_bar(stat="identity",position = "dodge") +
  geom_text(vjust=-.5,
            position = position_dodge(width=1)) +
  theme_hc() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.background = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        text = element_text(size = 14),
        plot.caption = element_text(hjust = 0))  +
  labs(
    x="Age group (years)",
    fill = "Sex",
    caption=paste0(
      "TBST: tuberculosis antigen-based skin test")) +  # title and caption
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



###---{SITREP}----

mortlocks_flatfile %>%
  group_by(age_group) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            
            no_active_tb = sum(active_tb_tx),

            no_ltbi = sum(ltbi_diagnosis),

            no_hd_referrals = sum(hd_further_assessment),
            no_hd_prevention = sum(hd_prev_given),
            
            no_diabetes = sum(dm_a1c_or_hx, na.rm = TRUE),
            no_new_diabetes = sum(new_dm_result, na.rm = TRUE)
  ) %>%
  adorn_totals() %>%
  select(age_group, no_registered, no_screened,  
         no_active_tb, no_ltbi, 
         no_hd_referrals, 
         no_hd_prevention, 
         no_diabetes, no_new_diabetes) %>%
  t() %>%
  as.data.frame() %>%
  row_to_names(1)
