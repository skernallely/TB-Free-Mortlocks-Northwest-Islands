## TB-Free Chuuk R code
## Mortlocks and NW Treatment Cascades Code

## Goal:Make and visualize treatment cascades for LTBI patients from
##      Mortlocks and NW TB-Free screening and follow-up data

#------------------------------------------------------------------------------
#WORKING DIRECTORY
setwd("~/PIHOA/TBFC/R Analysis/Mortlocks_NW")

#PACKAGES
library(tidyverse) #pipes, stringr, lubridate, scales
library(readxl) #excel load-in
library(vtable) #allows sumtable
library(janitor) #tabyl and variable name cleaning
library(ggplot2) #make graphs

#formulas
`%notin%` <- Negate(`%in%`)
is.not.na <- function(x) !is.na(x)

##STANDARDS

#population and rate estimates
rate_estimates_age <- read_excel("Data/TB rate estimates MT NW.xlsx", col_names=TRUE,
                                 sheet="overall_rates") %>%
  pivot_longer(3:23, names_to="age_group", values_to="rate") %>%
  pivot_wider(id_cols = c("area","age_group"), names_from="measure", values_from="rate") %>%
  rename(est_ltbi_rate = pct_tst_pos_HI) %>%
  mutate(est_tb_rate = est_tb_rate/100000,
         est_tb_cases = round(est_tb_rate*pop),
         est_ltbi_cases = round(est_ltbi_rate*pop)
  )

rate_estimates_village <- read_excel("Data/TB rate estimates MT NW.xlsx", col_names=TRUE,
                                     sheet="village_rates")%>%
  mutate(est_tb_cases = round(est_tb_rate*pop),
         est_ltbi_cases = round(est_ltbi_rate*pop),
         island = str_to_upper(island),
         village = str_to_upper(village),
         full_village_name = paste(island,village)
  )

#island regions
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

#cascade graph order
graph_order <- c("pop","no_registered","no_screened","no_active_tb",
                 "no_ltbi","no_hd_prevention")

graph_order_long <- c("Total Population","No. Registered", "No. Screened", 
                      "No. Active TB", "No. LTBI", "No. HD Prevention")
#------------------------------------------------------------------------------

##DATA
#load in clean flatfile
mortlocks_flatfile <- read_excel("Data/mortlocks_analysis_data.xlsx")

##Create table for screening outcome indicators and rates by village/municipality
complete_village<- mortlocks_flatfile %>%
  # mutate(full_village_name = paste(toupper(municipality),toupper(village))) %>%
  # group_by(full_village_name) %>%
  group_by(municipality) %>%
  summarise(no_registered = n(),
            no_screened = sum(screened_at_clinic),
            pct_seen_of_registered = no_screened/no_registered,
            
            no_active_tb = sum(active_tb_tx),
            pct_active_of_screened = no_active_tb/no_screened,
            
            no_tst_pos = sum(tst_pos),
            pct_tst_pos_of_screened = no_tst_pos/no_screened,
            
            no_hd_prevention = sum(hd_prev_given),
            pct_hd_prev_of_screened = no_hd_prevention/no_screened,
            
  ) %>%
  #merge with rate_estimates by age group dataset for all ages, total rates
  # merge(rate_estimates_village %>% 
  #         select(full_village_name, pop),by="full_village_name") %>%
  # 
  # mutate(pct_registered_of_pop = no_registered/pop
  # ) %>%
  mutate_at(c("pct_seen_of_registered","pct_active_of_screened","pct_tst_pos_of_screened",
              "pct_hd_prev_of_screened"
              # ,
              #"pct_registered_of_pop"
  ), label_percent()) %>%
  adorn_totals(c("row"))

complete_village %>%
  select(full_village_name,pop,no_registered,no_screened,no_active_tb,
         no_ltbi,no_hd_prevention) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:7, names_to="category", values_to = "count") %>%
  group_by(full_village_name) %>%
  #calculate cascade pcts by full_village_name
  mutate(municipality = gsub( " .*$", "", full_village_name),
         category = factor(category, 
                           levels=graph_order))


complete_village %>%
  select(full_village_name,pop,no_registered,no_screened,no_active_tb,
         no_ltbi,no_hd_prevention) %>%
  #rearrange into long form for ggplot
  pivot_longer(cols=2:7, names_to="category", values_to = "count") %>%
  #calculate cascade pcts by full_village_name
  mutate(municipality = gsub( " .*$", "", full_village_name),
         category = factor(category, 
                           levels=graph_order)) %>%
  group_by(municipality) %>%
  ggplot(aes(x=category, y=count, fill=category)) +
  geom_bar(stat="identity",position = position_stack(reverse = FALSE)) +
  labs(x = "",
       y="No. of people") +  # title and caption
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm")) +# turn off minor 
  scale_fill_brewer(palette="Set1") + 
  scale_x_discrete(labels = str_wrap(graph_order_long, width = 10)) +
  facet_wrap( ~ municipality, scales='free') +
  labs(
    title = "Clinic Screening Numbers for TB-Free Mortlocks and NW Islands, 2024",
    subtitle = "All Ages, Chuuk"
  )


#clean the workspace
rm(list = ls())