setwd("/Users/bc/Documents/NHANES/caunca/")

# load packages
library(tidyverse)

## survey package (analyzes complex design surveys)
library(survey)  
options( survey.lonely.psu = "adjust" )
options(scipen = 999) # turn off scientific notation 

## tbl_regression package for presentable results
library(gtsummary)
library(gt)

# getting nice output
library(jtools)

# managing imputed data
library(mice)

# excel for outputting results
library(openxlsx)
all_wb <- createWorkbook()
all_ses_wb <- createWorkbook()

## if wanting to edit workbooks, run below 
#all_wb <- loadWorkbook("./all.xlsx")
#all_ses_wb <- loadWorkbook("./all_ses.xlsx")

# calling data
load("./d_ses_imp.Rdata") 

# creating dataset with imputed datasets stacked together
imp_stacked <- complete(d_ses_imp, "long") # n=50640, v=45
str(imp_stacked)
summary(imp_stacked$".imp")
table(imp_stacked$".imp")

############################

# List of outcomes
## To run models 
epigen <- c("HorvathAge", "HannumAge", "SkinBloodAge", 
            "PhenoAge", "GDF15Mort", "B2MMort",
            "CystatinCMort", "TIMP1Mort", "ADMMort", 
            "PAI1Mort", "LeptinMort", "PACKYRSMort", 
            "logA1CMort", "GrimAgeMort", "GrimAge2Mort",  
            "HorvathTelo", "VidalBraloAge", "YangCell",  
            "ZhangAge", "WeidnerAge", "LinAge", "TELOMEAN")
# "DunedinPoAm" already represents years/chron age in years, so should not be in age-adjusted models

## For indexing after running if needed 
epigen_all <- c("HorvathAge", "HannumAge", "SkinBloodAge", 
                "PhenoAge", "GDF15Mort", "B2MMort",
                "CystatinCMort", "TIMP1Mort", "ADMMort", 
                "PAI1Mort", "LeptinMort", "PACKYRSMort", 
                "logA1CMort", "GrimAgeMort", "GrimAge2Mort",  
                "HorvathTelo", "VidalBraloAge", "YangCell",  
                "ZhangAge", "WeidnerAge", "LinAge", "TELOMEAN", "DunedinPoAm")

############################

### Unadjusted Model ----
# RUNNING MODELS
m <- max(imp_stacked$".imp") # number of imputed datasets 
m0_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_0_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses))
    f_unadj <- as.formula(paste0(y, " ~ ses"))
    fit <- svyglm(f_unadj, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_0_ses_tbls <- map(epigen, model_0_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses))
  formula <- as.formula("DunedinPoAm ~ ses")
  model_0_ses_dunedin <- svyglm(formula, 
                                design=dstrat_complete, 
                                na.action=na.exclude) 
  model_0_ses_tbls[[23]] <- model_0_ses_dunedin
  
  m0_ses_out[[i]] <- model_0_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  data <- pluck(m0_ses_out, x, y)
  summ(data, transform.response = TRUE)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(epigen_all) # number of imputed datasets 
m0_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m0_ses_mod[[i]] <- map2(1:length(m0_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m0_ses_pool <- map(m0_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m0_results <- map(m0_ses_pool, org_results) 
names(m0_results) <- epigen_all
m0_output <- do.call(rbind, m0_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m0_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "unadj")
writeDataTable(all_ses_wb,
               sheet = "unadj",
               x = m0_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### Model 1: Adjusted for sociodemographics ----
# RUNNING MODELS
m <- max(imp_stacked$".imp") # number of imputed datasets 
m1_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_1_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses))
    f_m1 <- as.formula(paste0(y, " ~ ses + RIDAGEYR + sex + race + married"))
    fit <- svyglm(f_m1, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_1_ses_tbls <- map(epigen, model_1_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses))
  formula <- as.formula("DunedinPoAm ~ ses + sex + race + married")
  model_1_ses_dunedin <- svyglm(formula, 
                                design=dstrat_complete, 
                                na.action=na.exclude) 
  model_1_ses_tbls[[23]] <- model_1_ses_dunedin
  
  m1_ses_out[[i]] <- model_1_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m1_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(epigen_all) # number of outcomes
m1_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m1_ses_mod[[i]] <- map2(1:length(m1_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m1_ses_pool <- map(m1_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m1_results <- map(m1_ses_pool, org_results) 
names(m1_results) <- epigen_all
m1_output <- do.call(rbind, m1_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m1_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m1")
writeDataTable(all_ses_wb,
               sheet = "m1",
               x = m1_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### Model 2: m2 + risk factors, health behaviors ----
# RUNNING MODELS
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses))
    f_m2 <- as.formula(paste0(y, " ~ ses + RIDAGEYR + sex + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2_ses_tbls <- map(epigen, model_2_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses))
  formula <- as.formula("DunedinPoAm ~ ses + sex + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2_ses_dunedin <- svyglm(formula, 
                                design=dstrat_complete, 
                                na.action=na.exclude) 
  model_2_ses_tbls[[23]] <- model_2_ses_dunedin
  
  m2_ses_out[[i]] <- model_2_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(epigen_all) # number of outcomes  
m2_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2_ses_mod[[i]] <- map2(1:length(m2_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2_ses_pool <- map(m2_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2_results <- map(m2_ses_pool, org_results) 
names(m2_results) <- epigen_all
m2_output <- do.call(rbind, m2_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2")
writeDataTable(all_ses_wb,
               sheet = "m2",
               x = m2_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### Model 2a: with components ----
## To run models 
sig_epigen <- c("HannumAge","PhenoAge", "GrimAgeMort", 
                "GrimAge2Mort", "DunedinPoAm")

# RUNNING MODELS
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2a_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2a_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & 
                               !is.na(pir5) & 
                               !is.na(educ3) &
                               !is.na(home) &
                               !is.na(occ))
    f_m2a <- as.formula(paste0(y, " ~ factor(pir5) + educ3 + home + occ + RIDAGEYR + sex + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2a, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2a_ses_tbls <- map(sig_epigen, model_2a_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses))
  formula <- as.formula("DunedinPoAm ~ factor(pir5) + educ3 + home + occ + sex + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2a_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2a_ses_tbls[[5]] <- model_2a_ses_dunedin
  
  m2a_ses_out[[i]] <- model_2a_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2a_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes
m2a_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2a_ses_mod[[i]] <- map2(1:length(m2a_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2a_ses_pool <- map(m2a_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>%
    mutate("Group" = case_when(
      term == "factor(pir5)1" ~ "PIR 1 - <2",
      term == "factor(pir5)2" ~ "PIR 2 - <3",
      term == "factor(pir5)3" ~ "PIR 3 - <4",
      term == "factor(pir5)4" ~ "PIR 4 - <5",
      term == "factor(pir5)5" ~ "PIR >5+",
      term == "educ3College grad" ~ "College Graduate",
      term == "educ3HS grad" ~ "HS Graduate",
      term == "homeOwn" ~ "Owns Home",
      term == "occEmployed" ~ "Employed",
      term == "occHomemaker" ~ "Homemaker",
      term == "occRetired" ~ "Retired",
      term == "occStudent" ~ "Student",
      term == "occUnable to work due to health/disability" ~ 
        "Unable to work due to health/disability")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>%
    filter(Group %in% c("PIR 1 - <2",
                        "PIR 2 - <3",
                        "PIR 3 - <4",
                        "PIR 4 - <5",
                        "PIR >5+",
                        "College Graduate",
                        "HS Graduate",
                        "Owns Home",
                        "Employed",
                        "Homemaker",
                        "Retired",
                        "Student",
                        "Unable to work due to health/disability")) 
}

m2a_results <- map(m2a_ses_pool, org_results) 
names(m2a_results) <- sig_epigen
m2a_output <- do.call(rbind, m2a_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2a_output)

# SAVING IN EXCEL 
addWorksheet(all_wb, sheetName = "m2a")
writeDataTable(all_wb,
               sheet = "m2a",
               x = m2a_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_wb,
             "./all_wb.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

# Model 2b: interactions ----
sig_epigen <- c("HannumAge","PhenoAge", "GrimAgeMort", 
                "GrimAge2Mort", "DunedinPoAm")

m <- max(imp_stacked$".imp") # number of imputed datasets 
m2b_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2b_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses))
    f_m2b <- as.formula(paste0(y, " ~ ses*sex + ses*race + RIDAGEYR + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2b, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2b_ses_tbls <- map(sig_epigen, model_2b_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses))
  formula <- as.formula("DunedinPoAm ~ ses*sex + ses*race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2b_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2b_ses_tbls[[5]] <- model_2b_ses_dunedin
  
  m2b_ses_out[[i]] <- model_2b_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2b_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2b_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2b_ses_mod[[i]] <- map2(1:length(m2b_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2b_ses_pool <- map(m2b_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle:sexF" ~ "Middle*Woman",
      term == "sesUpper:sexF" ~ "Upper*Woman",
      term == "sesMiddle:raceBlack" ~ "Middle*Black",
      term == "sesUpper:raceBlack" ~ "Upper*Black",
      term == "sesMiddle:raceHispanic" ~ "Middle*Hispanic",
      term == "sesUpper:raceHispanic" ~ "Upper*Hispanic",
      term == "sesMiddle:raceOther" ~ "Middle*Other",
      term == "sesUpper:raceOther" ~ "Upper*Other",)) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle*Woman",
                        "Upper*Woman",
                        "Middle*Black",
                        "Upper*Black",
                        "Middle*Hispanic",
                        "Upper*Hispanic",
                        "Middle*Other",
                        "Upper*Other") )
}

m2b_results <- map(m2b_ses_pool, org_results) 
names(m2b_results) <- sig_epigen
m2b_output <- do.call(rbind, m2b_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2b_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2b")
writeDataTable(all_ses_wb,
               sheet = "m2b",
               x = m2b_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

## sex-stratified analyses
sig_epigen <- c("GrimAgeMort", "GrimAge2Mort", "DunedinPoAm")

### men 
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2c_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2c_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses) & sex == "M")
    f_m2c <- as.formula(paste0(y, " ~ ses + RIDAGEYR + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2c, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2c_ses_tbls <- map(sig_epigen, model_2c_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses) & sex == "M")
  formula <- as.formula("DunedinPoAm ~ ses + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2c_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2c_ses_tbls[[3]] <- model_2c_ses_dunedin
  
  m2c_ses_out[[i]] <- model_2c_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2c_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2c_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2c_ses_mod[[i]] <- map2(1:length(m2c_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2c_ses_pool <- map(m2c_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2c_results <- map(m2c_ses_pool, org_results) 
names(m2c_results) <- sig_epigen
m2c_output <- do.call(rbind, m2c_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2c_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2c_men")
writeDataTable(all_ses_wb,
               sheet = "m2c_men",
               x = m2c_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)


# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### women 
sig_epigen <- c("GrimAgeMort", "GrimAge2Mort", "DunedinPoAm")
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2d_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2d_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses) & sex == "F")
    f_m2d <- as.formula(paste0(y, " ~ ses + RIDAGEYR + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2d, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2d_ses_tbls <- map(sig_epigen, model_2d_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses) & sex == "F")
  formula <- as.formula("DunedinPoAm ~ ses + race + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2d_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2d_ses_tbls[[3]] <- model_2d_ses_dunedin
  
  m2d_ses_out[[i]] <- model_2d_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2d_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2d_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2d_ses_mod[[i]] <- map2(1:length(m2d_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2d_ses_pool <- map(m2d_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2d_results <- map(m2d_ses_pool, org_results) 
names(m2d_results) <- sig_epigen
m2d_output <- do.call(rbind, m2d_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2d_output)


# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2d_women")
writeDataTable(all_ses_wb,
               sheet = "m2d_women",
               x = m2d_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environwhitet
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

## race-stratified analyses
sig_epigen <- c("GrimAgeMort", "GrimAge2Mort", "DunedinPoAm")

### white 
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2e_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2e_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses) & race == "White")
    f_m2e <- as.formula(paste0(y, " ~ ses + RIDAGEYR + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2e, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2e_ses_tbls <- map(sig_epigen, model_2e_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses) & race == "White")
  formula <- as.formula("DunedinPoAm ~ ses + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2e_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2e_ses_tbls[[3]] <- model_2e_ses_dunedin
  
  m2e_ses_out[[i]] <- model_2e_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2e_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2e_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2e_ses_mod[[i]] <- map2(1:length(m2e_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2e_ses_pool <- map(m2e_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2e_results <- map(m2e_ses_pool, org_results) 
names(m2e_results) <- sig_epigen
m2e_output <- do.call(rbind, m2e_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2e_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2e_white")
writeDataTable(all_ses_wb,
               sheet = "m2e_white",
               x = m2e_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)

# cleaning up environment
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### black 
sig_epigen <- c("GrimAgeMort", "GrimAge2Mort", "DunedinPoAm")
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2f_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2f_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses) & race == "Black")
    f_m2f <- as.formula(paste0(y, " ~ ses + RIDAGEYR + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2f, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2f_ses_tbls <- map(sig_epigen, model_2f_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses) & race == "Black")
  formula <- as.formula("DunedinPoAm ~ ses + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2f_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2f_ses_tbls[[3]] <- model_2f_ses_dunedin
  
  m2f_ses_out[[i]] <- model_2f_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2f_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2f_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2f_ses_mod[[i]] <- map2(1:length(m2f_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2f_ses_pool <- map(m2f_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2f_results <- map(m2f_ses_pool, org_results) 
names(m2f_results) <- sig_epigen
m2f_output <- do.call(rbind, m2f_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2f_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2f_black")
writeDataTable(all_ses_wb,
               sheet = "m2f_black",
               x = m2f_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)


# cleaning up environblackt
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))

### hispanic
sig_epigen <- c("GrimAgeMort", "GrimAge2Mort", "DunedinPoAm")
m <- max(imp_stacked$".imp") # number of imputed datasets 
m2g_ses_out <- list() # for the excel sheets 

for (i in seq_along(1:m)) {
  
  # Extract one imputed dataset at a time
  data <- imp_stacked %>% filter(`.imp` == i)
  
  # Create a survey design object (stratified sample)
  dstrat <- svydesign(id =      ~SDMVPSU,
                      strata =  ~SDMVSTRA, 
                      weights = ~WTDN4YR, 
                      nest = TRUE,
                      data = data)
  
  # Function for unadjusted model 
  model_2g_ses <- function(y){
    
    dstrat_complete = subset(dstrat, !is.na(y) & !is.na(ses) & race == "Hispanic")
    f_m2g <- as.formula(paste0(y, " ~ ses + RIDAGEYR + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh"))
    fit <- svyglm(f_m2g, design=dstrat_complete, na.action=na.exclude)
    fit
  }
  
  # running models 
  model_2g_ses_tbls <- map(sig_epigen, model_2g_ses)
  dstrat_complete = subset(dstrat, !is.na(DunedinPoAm) & !is.na(ses) & race == "Hispanic")
  formula <- as.formula("DunedinPoAm ~ ses + sex + married + BMXBMI + smoke + packyears + total_met_hr_wk + htn + dm + etoh")
  model_2g_ses_dunedin <- svyglm(formula, 
                                 design=dstrat_complete, 
                                 na.action=na.exclude) 
  model_2g_ses_tbls[[3]] <- model_2g_ses_dunedin
  
  m2g_ses_out[[i]] <- model_2g_ses_tbls
  
}

# GET ESTIMATES BY OUTCOME
get_model <- function(x, y){
  pluck(m2g_ses_out, x, y)
} # Function to pull each imputation, then the yth model per outcome 

m <- length(sig_epigen) # number of outcomes  
m2g_ses_mod <- list() 
for (i in seq_along(1:m)) {
  m2g_ses_mod[[i]] <- map2(1:length(m2g_ses_out), i, get_model)
}

# POOLING ESTIMATES ACROSS IMPUTATIONS 
get_pool <- function(x){
  as.mira(x) %>% pool()
} # Function to pool estimates per imputation 

m2g_ses_pool <- map(m2g_ses_mod, get_pool)

# ORGANIZING RESULTS
org_results <- function(x){
  
  results <- summary(x, conf.int = TRUE) %>%
    as.data.frame() %>% 
    mutate("Estimate" = estimate) %>% 
    mutate("LCL" = conf.low) %>% 
    mutate("UCL" = conf.high) %>%
    rename("p-value" = p.value) %>% 
    mutate("Group" = case_when(
      term == "sesMiddle" ~ "Middle",
      term == "sesUpper" ~ "Upper")) %>% 
    select(Group, Estimate, LCL, UCL, "p-value") %>% 
    filter(Group %in% c("Middle", "Upper")) 
}

m2g_results <- map(m2g_ses_pool, org_results) 
names(m2g_results) <- sig_epigen
m2g_output <- do.call(rbind, m2g_results) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(outcome = rowname) %>% 
  mutate(outcome = str_remove(outcome, pattern = "\\.[0-9]*$")) %>%
  mutate_if(is.numeric, ~round(., 2))
view(m2g_output)

# SAVING IN EXCEL 
addWorksheet(all_ses_wb, sheetName = "m2g_hispanic")
writeDataTable(all_ses_wb,
               sheet = "m2g_hispanic",
               x = m2g_output,
               colNames = TRUE,
               rowNames = TRUE)
saveWorkbook(all_ses_wb,
             "./all_ses.xlsx",
             overwrite = TRUE)


# cleaning up environhispanict
rm(list = setdiff(ls(), c("d_ses_imp", "imp_stacked", 
                          "all_ses_wb", "all_wb",
                          "epigen", "epigen_all")))
