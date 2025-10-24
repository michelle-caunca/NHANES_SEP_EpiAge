setwd("/Users/bc/Documents/NHANES/caunca/")

# Run script to merge data
source("./code/NHANES-bioage-merge.R")

# load tidyverse
library(tidyverse)

# load survey package (analyzes complex design surveys)
library(survey)  
options( survey.lonely.psu = "adjust" )

# tbl_regression package for presentable results
library(gtsummary)
library(gt)

# excel for outputting results
# library(openxlsx)
# all_wb <- createWorkbook()
# all_ses_wb <- createWorkbook()

## if coming back, run this
#all_wb <- loadWorkbook("./all.xlsx")
#all_ses_wb <- loadWorkbook("./all_ses.xlsx")

# calling data
load("./d_ses.Rdata") #n=2532, v=105
view(names(d_ses))

# Create a survey design object (stratified sample)
# dstrat <- svydesign(id =      ~SDMVPSU,
# 					strata =  ~SDMVSTRA, 
# 					weights = ~WTDN4YR, 
# 					nest = TRUE,
# 					data = d_ses)
					
############################
############################

# list of outcomes
epigen <- c("HorvathAge", "HannumAge", "SkinBloodAge", "PhenoAge", "GDF15Mort", "B2MMort",
            "CystatinCMort", "TIMP1Mort", "ADMMort", "PAI1Mort", "LeptinMort", "PACKYRSMort",
            "GrimAgeMort", "GrimAge2Mort", "HorvathTelo", "YangCell", "ZhangAge", "LinAge", "TELOMEAN")
# "DunedinPoAm" already represents years/chron age in years, so should not be in age-adjusted models

d_ses %>% 
  as.data.frame() %>% 
  tbl_summary(include = c(RIDAGEYR, sex, race, married, INDFMPIR, educ3, home, occ, BMXBMI,
                          smoke, packyears, total_met_hr_wk, htn, dm, etoh),
              by = ses,
              type = list(
                RIDAGEYR ~ "continuous",
                sex ~ "dichotomous",
                race ~ "categorical",
                married ~ "categorical",
                INDFMPIR ~ "continuous",
                educ3 ~ "categorical",
                home ~ "dichotomous",
                occ ~ "categorical",
                BMXBMI ~ "continuous",
                smoke ~ "categorical",
                packyears ~ "continuous",
                total_met_hr_wk ~ "continuous",
                htn ~ "dichotomous",
                dm ~ "dichotomous",
                etoh ~ "dichotomous"),
              value = list(
                sex ~ "F",
                home ~ "Own",
                married ~ "Married",
                htn ~ 1,
                dm ~ 1, 
                etoh ~ 1), 
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)",
                all_dichotomous() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 0,
              label = list(
                RIDAGEYR ~ "Age (years)",
                sex ~ "Sex/Gender",
                race ~ "Race/Ethnicity",
                married ~ "Marital Status",
                INDFMPIR ~ "Family Poverty Income Ratio",
                educ3 ~ "Educational Attainment",
                home ~ "Home Ownership",
                occ ~ "Occupational Status",
                BMXBMI ~ "Body Mass Index",
                smoke ~ "Smoking Status",
                packyears ~ "Smoking Pack Years",
                total_met_hr_wk ~ "Total METS (hours/week)",
                htn ~ "Hypertension",
                dm ~ "Diabetes",
                etoh ~ "Alcohol Use of >5 Drinks per Day") ) %>% 
  
  # Removes missings 
  remove_row_type(variables = everything(), 
                  type = "missing") -> tbl1

tbl1 %>% as_gt() %>% gtsave("./tbl1.docx")

