setwd("/Users/bc/Documents/NHANES/caunca/")

# Run script to merge data
#source("./code/NHANES-bioage-merge.R")

# load tidyverse
library(tidyverse)

# load survey package (analyzes complex design surveys)
library(survey)  
options( survey.lonely.psu = "adjust" )

# tbl_regression package for presentable results
library(gtsummary)
library(gt)

# calling data
load("./d_ses.Rdata") #n=2532, v=120
view(names(d_ses))
					
############################
############################

# list of outcomes
epigen <- c("HorvathAge", "HannumAge", "SkinBloodAge", "PhenoAge", "GDF15Mort", "B2MMort",
            "CystatinCMort", "TIMP1Mort", "ADMMort", "PAI1Mort", "LeptinMort", "PACKYRSMort",
            "GrimAgeMort", "GrimAge2Mort", "HorvathTelo", "YangCell", "ZhangAge", "LinAge", "TELOMEAN")
# "DunedinPoAm" already represents years/chron age in years, so should not be in age-adjusted models

d_ses %>% 
  as.data.frame() %>% 
  tbl_summary(include = c(RIDAGEYR, sex, race, married, INDFMPIR, educ_hs, home, occ_bw_bin, BMXBMI,
                          smoke, packyears, total_met_hr_wk, htn, dm, etoh, cad, cancer),
              by = ses,
              type = list(
                RIDAGEYR ~ "continuous",
                sex ~ "dichotomous",
                race ~ "categorical",
                married ~ "dichotomous",
                INDFMPIR ~ "continuous",
                educ_hs ~ "categorical",
                home ~ "dichotomous",
                occ_bw_bin ~ "categorical",
                BMXBMI ~ "continuous",
                smoke ~ "categorical",
                packyears ~ "continuous",
                total_met_hr_wk ~ "continuous",
                htn ~ "dichotomous",
                dm ~ "dichotomous",
                etoh ~ "dichotomous",
                cad ~ "dichotomous",
                cancer ~ "dichotomous"),
              value = list(
                sex ~ "F",
                home ~ "Own",
                married ~ "Married",
                htn ~ 1,
                dm ~ 1, 
                etoh ~ 1,
                cad ~ 1,
                cancer ~ 1), 
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)",
                all_dichotomous() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 0,
              label = list(
                RIDAGEYR ~ "Age (years)",
                sex ~ "Women",
                race ~ "Race/Ethnicity",
                married ~ "Married",
                INDFMPIR ~ "Family Poverty Income Ratio",
                educ_hs ~ "Educational Attainment",
                home ~ "Home Ownership",
                occ_bw_bin ~ "Occupational Status",
                BMXBMI ~ "Body Mass Index",
                smoke ~ "Smoking Status",
                packyears ~ "Smoking Pack Years",
                total_met_hr_wk ~ "Total METS (hours/week)",
                htn ~ "Hypertension",
                dm ~ "Diabetes",
                etoh ~ "Alcohol Use of >5 Drinks per Day",
                cad ~ "Coronary Artery Disease",
                cancer ~ "Cancer") ) %>% 
  
  # Removes missings 
  remove_row_type(variables = everything(), 
                  type = "missing") -> tbl1

tbl1 %>% as_gt() %>% gtsave("./tbl1.docx")

# Weighted table 1
svydesign(id =      ~SDMVPSU,
          strata =  ~SDMVSTRA,
          weights = ~WTDN4YR, 
          nest = TRUE,
          data = d_ses) %>% 
  tbl_svysummary(include = c(RIDAGEYR, sex, race, married, INDFMPIR, educ_hs, home, occ_bw_bin, BMXBMI,
                          smoke, packyears, total_met_hr_wk, htn, dm, etoh, cad, cancer),
              by = ses,
              type = list(
                RIDAGEYR ~ "continuous",
                sex ~ "dichotomous",
                race ~ "categorical",
                married ~ "dichotomous",
                INDFMPIR ~ "continuous",
                educ_hs ~ "categorical",
                home ~ "dichotomous",
                occ_bw_bin ~ "categorical",
                BMXBMI ~ "continuous",
                smoke ~ "categorical",
                packyears ~ "continuous",
                total_met_hr_wk ~ "continuous",
                htn ~ "dichotomous",
                dm ~ "dichotomous",
                etoh ~ "dichotomous",
                cad ~ "dichotomous",
                cancer ~ "dichotomous"),
              value = list(
                sex ~ "F",
                home ~ "Own",
                married ~ "Married",
                htn ~ 1,
                dm ~ 1, 
                etoh ~ 1,
                cad ~ 1,
                cancer ~ 1), 
              statistic = list(
                all_continuous() ~ "{mean} ({mean.std.error})",
                all_categorical() ~ "{p} ({p.std.error})",
                all_dichotomous() ~ "{p} ({p.std.error})"),
              digits = all_continuous() ~ 0,
              label = list(
                RIDAGEYR ~ "Age (years)",
                sex ~ "Sex/Gender",
                race ~ "Race/Ethnicity",
                married ~ "Marital Status",
                INDFMPIR ~ "Family Poverty Income Ratio",
                educ_hs ~ "Educational Attainment",
                home ~ "Home Ownership",
                occ_bw_bin ~ "Occupational Status",
                BMXBMI ~ "Body Mass Index",
                smoke ~ "Smoking Status",
                packyears ~ "Smoking Pack Years",
                total_met_hr_wk ~ "Total METS (hours/week)",
                htn ~ "Hypertension",
                dm ~ "Diabetes",
                etoh ~ "Alcohol Use of >5 Drinks per Day",
                cad ~ "Coronary Artery Disease",
                cancer ~ "Cancer") ) %>% 
  
  # Removes missings 
  remove_row_type(variables = everything(), 
                  type = "missing") -> tbl1_weighted

tbl1_weighted %>% as_gt() %>% gtsave("./tbl1_weighted.docx")
