---
title: "Inclusion Exclusion Calculation Script"
subtitle: "Inclusion Exclusion Calculation Script - 02/08/2022"
author: "Mohammad Aljawamees, Mohammed Elmahairi, Kelly Kurz, Laksiri Nanayakkara, William McFall"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: 
  html_document:
    theme: flatly
    highlight: espresso
    toc: true
    toc_float: true
    smooth_scroll: true 
    number_sections: true
    
---


```{r setup, include=FALSE, fig.align = "center", fig.width= 16, fig.height= 11, fig.asp= 0.618, out.width="100%"}

knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)

knitr::opts_knit$set(root.dir = 'C:/Users/william.mcfall/Desktop/Inclusion Exclusion Error Calculations') 

```

  
```{r}

rm(list = ls())

## creates UN theme for ggplots
## https://rpubs.com/mclaire19/ggplot2-custom-themes
## https://ggplot2.tidyverse.org/reference/theme.html

theme_wfp <- function(){ 
  font <- "Open Sans"   
  
  theme_minimal() %+replace%   
    
    theme(
      
      # grid elements
      panel.grid.major = element_blank(),    
      panel.grid.minor = element_blank(),    
      axis.ticks = element_blank(), 
      
      # text elements
      plot.title = element_text(
        family = font,            
        size = 12,                
        face = 'bold',            
        colour = 'black',         
        hjust = 0),               
      
      #subtitle elements
      plot.subtitle = element_text(   
        family = font,            
        colour = 'grey',
        size = 10,
        face = 'bold'),               
      
      #legend elements
      legend.title = element_text(
        family = font,            
        colour = 'black',
        size = 8,
        face = 'bold'
      ),
      
      # caption elements
      plot.caption = element_text(           
        family = font,           
        size = 9,                 
        hjust = 1),               
      
      # axis title elements
      axis.title = element_text(             
        family = font,
        face = 'bold', 
        size = 8),               
      
      # axis text elements
      axis.text = element_text(              
        family = font,            
        size = 8),                
      
      # axis text margins
      axis.text.x = element_text(margin=margin(10, b = 10)),
      plot.margin = margin(1, 1, 1, 1, "cm"),

)
      
}


setwd("C:/Users/william.mcfall/Desktop/Inclusion Exclusion Error Calculations")

library(gtsummary)
library(readxl)
library(dplyr)
library(tidyr)
library(mosaic)
library(tidyverse)
library(modelsummary)
library(kableExtra) 
library(foreign)
library(arsenal)
library(desctable)
library(survey)
library(srvyr) 
library(naniar)
library(rmarkdown)
library(haven)
library(labelled)
library(sjlabelled)
library(xlsx)
library(rlang)
library(skimr)
library(ggrepel)
library(janitor)
library(skimr)
library(gmodels)
library(pollster)

## import retargeting data
retargeting_data <- read_excel("./UNHCR_Targeting data_SYR_05122021.xlsx", sheet = "UNHCR_Targeting data_SYR_051220") 

retargeting_data %>% mutate(ID = progres_registrationgroupidname) %>% select(!c(targeting_classification)) -> retargeting_data

## import FSOM data
assessment_data <- read.spss("./FSOM_Q3_Calculating_Data.sav", use.value.labels = TRUE, to.data.frame = TRUE) 

## keeps only the needed variables
assessment_data %>% mutate(ID = hh_id) %>% select(c(ID, wts_commu_all, Vulnerability_WFP_Asst_Adj, Vulnerability_Assistance_Adj)) -> assessment_data

## joins relevant datasets
retargeting_data %>% inner_join(.,assessment_data, by = "ID") -> merged_data

## create 2 point vulnerability metric
merged_data$Vulnerability_WFP_Asst_Adj_2pt <- ifelse((merged_data$Vulnerability_WFP_Asst_Adj == "High" | merged_data$Vulnerability_WFP_Asst_Adj == "Medium"), 1, 0)


rm(retargeting_data, assessment_data)

```


## Individual Error Analysis

```{r}

print("Targeting Criteria Coding - #1 - Dependency Ratio > 1.5")

crosstab(merged_data, x = `targeting_criteria_1`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #2 - Case Size > 4")

crosstab(merged_data, x = `targeting_criteria_2`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #3 - Eduction < 6 Years")

crosstab(merged_data, x = `targeting_criteria_3`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #4 - Household Disability")

crosstab(merged_data, x = `targeting_criteria_4`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #5 - Female Headed Household")

crosstab(merged_data, x = `targeting_criteria_5`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #6 - Elderly Headed Household")

crosstab(merged_data, x = `targeting_criteria_6`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #7 - Single Male Household")

crosstab(merged_data, x = `targeting_criteria_7`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #8 - Child Headed Household")

crosstab(merged_data, x = `targeting_criteria_8`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")

print("Targeting Criteria Coding - #9 - Post Inclusion Household")

crosstab(merged_data, x = `Post Inc case (scenario_8_dis_elder_fhh_dsw_no_adult_m_elderlychildrenonly)`, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")



```


## Cumulative Error Analysis

```{r}

print("Targeting Criteria Coding - #1 - Dependency Ratio > 1.5 - Cumulative")

merged_data %>% mutate(cum_1 = if_else(`targeting_criteria_1` > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_1, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_1, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")

print("Targeting Criteria Coding - #2 - Case Size > 4 - Cumulative")

merged_data %>% mutate(cum_2 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_2`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_2, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_2, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


print("Targeting Criteria Coding - #3 - Eduction < 6 Years - Cumulative")

merged_data %>% mutate(cum_3 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_3`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_3, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_3, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


print("Targeting Criteria Coding - #4 - Household Disability - Cumulative")

merged_data %>% mutate(cum_4 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_4`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_4, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_4, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")

print("Targeting Criteria Coding - #5 - Female Headed Household - Cumulative")

merged_data %>% mutate(cum_5 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_5`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_5, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_5, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


print("Targeting Criteria Coding - #6 - Elderly Headed Household - Cumulative")

merged_data %>% mutate(cum_6 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_6`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_6, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_6, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


print("Targeting Criteria Coding - #7 - Single Male Household - Cumulative")

merged_data %>% mutate(cum_7 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_7`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_7, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_7, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


print("Targeting Criteria Coding - #8 - Child Headed Household - Cumulative")

merged_data %>% mutate(cum_8 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_8`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_8, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_8, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")

print("Targeting Criteria Coding - #9 - Post Inclusion Household - Cumulative")

merged_data %>% mutate(cum_9 = if_else((select(., `targeting_criteria_1`:`targeting_criteria_8`,`Post Inc case (scenario_8_dis_elder_fhh_dsw_no_adult_m_elderlychildrenonly)`) %>% rowSums(na.rm = TRUE)) > 0, 1 , 0)) -> merged_data

crosstab(merged_data, x = cum_9, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "row")
crosstab(merged_data, x = cum_9, y = `Vulnerability_WFP_Asst_Adj_2pt`, weight = wts_commu_all, pct_type = "column")


```

## Inclusion / Exclusion Error Profiling

```{r}

## create new column with inclusion / exclusion error classification

merged_data$error_class <- NA

merged_data$error_class[merged_data$Vulnerability_WFP_Asst_Adj_2pt == 0 & merged_data$targeting_classification_1 == 1] <- "Inclusion Error"

merged_data$error_class[merged_data$Vulnerability_WFP_Asst_Adj_2pt == 1 & merged_data$targeting_classification_1 == 0] <- "Exclusion Error"

merged_data$error_class[merged_data$Vulnerability_WFP_Asst_Adj_2pt == 1 & merged_data$targeting_classification_1 == 1] <- "No Error - Targeted"

merged_data$error_class[merged_data$Vulnerability_WFP_Asst_Adj_2pt == 0 & merged_data$targeting_classification_1 == 0] <- "No Error - Not Targeted"


merged_data$ANY_MEMBER_IN_CASE_DS <- as.factor(merged_data$ANY_MEMBER_IN_CASE_DS)

# glimpse(merged_data$ANY_MEMBER_IN_CASE_DS)

## summary statistics by inclusion / exclusion error classification

merged_data %>%
  tbl_summary(by = error_class,
              include = c("CASE_SIZE":"TOTAL_ELDERLY","Members05":"Members017","PA_ARRIVAL_YEAR", "PA_AGE":"ANY_MEMBER_IN_CASE_SM", "ELDER_PA_REC":"DEPENDENCY_RATIO"),
                type = list(where(is.logical) ~ "categorical", where(is.numeric) ~ "continuous"),
                 statistic = list(all_continuous() ~ "{mean}",
                                  all_categorical() ~ "{N_nonmiss}({p}%)"),
                 missing = "no")%>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall()%>%
  modify_header(update = list(label ~ "**Variable**",p.value ~ "**P**"))%>%
  bold_labels()%>%
  italicize_labels()%>%
  italicize_levels() %>%
  modify_spanning_header(c("stat_1":"stat_4") ~ "**Household Error Classification**")


## summary plots by inclusion / exclusion error classification

ggplot(merged_data, aes(x=CASE_SIZE, color=error_class, fill=error_class)) + geom_density(aes(group=error_class), alpha=0.15) + labs(title="Average Case Size by Error Classification") + theme_wfp()


ggplot(merged_data, aes(x=DEPENDENCY_RATIO, color=error_class, fill=error_class)) + geom_density(aes(group=error_class), alpha=0.15) + labs(title="Dependency Ratio by Error Classification") + theme_wfp()


ggplot(merged_data, aes(x=PA_AGE, color=error_class, fill=error_class)) + geom_density(aes(group=error_class), alpha=0.15) + labs(title="PA Age by Error Classification") + theme_wfp()


```
