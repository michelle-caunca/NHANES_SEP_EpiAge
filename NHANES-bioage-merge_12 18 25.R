setwd("/Users/bc/Documents/NHANES/caunca/")
 
# Load files
load("./d9900.Rdata") # n=2156, v=118
load("./d0102.Rdata") # n=2293, v=118

# loading packages
library(tidyverse)
library(mice)
library(naniar)

############################

### Prep for merge
col2change = which(names(d9900) %in% setdiff(names(d9900), names(d0102)))
names(d9900)[col2change]
names(d0102)[col2change]

# Harmonize variable names in d9900 to match d0102
d9900_2 <- d9900 %>% 
  rename(OCD150 = OCQ150)

# Check column names match
identical(names(d9900_2), names(d0102))

# Label waves
d9900_2$wave = "99-00"
d0102$wave = "01-02"

# Append/merge
d = rbind(d9900_2, d0102)

# Exclude participants with weight=0  (Confirmed:  Missing HorvathAge, too)
d = d[d$WTDN4YR!=0,]  #n=2532, v=119

############################

# Corrections for missing values
d$income[d$income==""] = NA
d$educ3[  d$educ3==""] = NA
d$married[d$married==""]=NA
d$home[      d$home==""]=NA
d$occ[      d$occ==""]=NA
d$smoke[d$smoke==""] = NA

############################

### Set reference value for factor variables
d$sex     = relevel(as.factor(d$sex),    ref="M")
d$race    = relevel(as.factor(d$race),   ref="White")
d$educ3   = relevel(as.factor(d$educ3),  ref="<HS")
d$occ     = relevel(as.factor(d$occ), ref ="Unemployed/Other" )
d$occ_bin = relevel(as.factor(d$occ_bin), ref ="Not Currently Working")
d$married = relevel(as.factor(d$married), ref="Married")
d$home    = relevel(as.factor(d$home),   ref="Don't Own")
d$smoke   = relevel(as.factor(d$smoke),  ref="never")

save(d, file = "./d.Rdata") # n=2532, v=119

############
### Creating socioeconomic position variable 
load("./d.Rdata")
d_ses <- d %>% 
  mutate(ses = case_when(
    educ_hs == "Greater than High School" & pirhi == 1 & home == "Own" & occ_bw_bin == "White Collar" ~ "Higher",
    educ_hs == "High School or Less" & pirhi == 0 & home == "Don't Own" & occ_bw_bin == "Blue Collar/Not Working" ~ "Lower", 
    is.na(educ_hs) & is.na(pirhi) & is.na(home) & is.na(occ_bw_bin) ~ NA,
    TRUE ~ "Middle")) %>%
  mutate(ses = factor(ses, 
                      levels = c("Lower",
                                 "Middle",
                                 "Higher")))

table(d_ses$ses, d_ses$educ_hs, exclude=NULL)
table(d_ses$ses, d_ses$pirhi, exclude=NULL)
table(d_ses$ses, d_ses$home, exclude=NULL)
table(d_ses$ses, d_ses$occ_bw_bin, exclude=NULL)
summary(as.factor(d_ses$ses))

save(d_ses, file = "./d_ses.Rdata") # n=2532, v=120

############
load("./d_ses.Rdata") # n=2532, v=120

# Imputing dataset
## missingness analysis 
miss_var_summary(d_ses) %>% view()
gg_miss_var(d_ses)

## selecting variables in the analysis
d_ses_1 <- d_ses %>% 
  select(SEQN,
         # outcomes
         HorvathAge, 
         HannumAge, 
         SkinBloodAge, 
         PhenoAge, 
         GDF15Mort, 
         B2MMort,
         CystatinCMort, 
         TIMP1Mort, 
         ADMMort, 
         PAI1Mort, 
         LeptinMort, 
         PACKYRSMort,
         logA1CMort,
         GrimAgeMort, 
         GrimAge2Mort, 
         HorvathTelo, 
         VidalBraloAge, 
         YangCell, 
         ZhangAge, 
         LinAge, 
         WeidnerAge,
         TELOMEAN,
         DunedinPoAm,
         # exposures
         ses,
         INDFMPIR,
         pirhi,
         pir5,
         educ,
         educ2,
         educ3,
         educ_hs,
         home,
         occ_bw, 
         occ_bw_bin,
         # covariates
         RIDAGEYR,
         sex, 
         race,
         married,
         BMXBMI,
         smoke,
         packyears,
         total_met_hr_wk,
         htn,
         dm,
         etoh,
         cad,
         cancer,
         LBXLYPCT,          
         LBXMOPCT,        
         LBXNEPCT,        
         LBXEOPCT,
         LBXBAPCT, 
         # weights/cluster vars
         WTDN4YR,
         SDMVPSU,
         SDMVSTRA) # n=2532, v=56

## missingness analysis
miss_var_summary(d_ses_1) %>% view()

## defining categorical variables
catvars <- c("ses",
             "pir5",
             "educ",
             "educ2",
             "educ3",
             "educ_hs",
             "home",
             "occ_bw",
             "occ_bw_bin",
             "sex", 
             "race",
             "married",
             "smoke",
             "htn",
             "dm",
             "etoh",
             "cad",
             "cancer")

notimpute <- c("SEQN", 
               "WTDN4YR",
               "SDMVPSU",
               "SDMVSTRA") # main exposure and outcomes have 0 missing except telomere length (n miss=2)

d_ses_2 <- d_ses_1 %>%
  mutate_at(catvars, as.factor)
str(d_ses_2)

## defining which variables to impute (basically just covariates for now)
d_ses_3 <- mice(d_ses_2, maxit = 0)
method <- d_ses_3$method
method[names(method) %in% notimpute] <- ""
#method[names(method) %in% catvars] <- "cart"
method

## multiple imputation, requiring a min of 25% usable cases and correlation of at least 0.4 to be included in the model 
d_ses_imp <- mice(d_ses_2, 
                  pred = quickpred(d_ses_2,
                                   minpuc = 0.25,
                                   mincor = 0.4, 
                                   exclude = c("SEQN","WTDN4YR","SDMVPSU","SDMVSTRA")),
                        method = method, 
                        m = 20,
                        seed = 123) 

test <- complete(d_ses_imp, "long")
summary(test)
miss_var_summary(test) %>% view()

save(d_ses_imp, file = "./d_ses_imp.Rdata")
