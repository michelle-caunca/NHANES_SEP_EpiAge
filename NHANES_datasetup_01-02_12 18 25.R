

###################################################################
# Socioeconomic Position and Epigenetic Age: Prepping Data 01-02 #
###################################################################

### Specify your local working directory.
setwd( "/Users/bc/Documents/NHANES/caunca/" )

##############################

### Load necessary packages
library(foreign) # file conversions
library(haven) # file conversion
library(tidyverse) # data wrangling 

##############################

### Functions to download and import 

pulldown <-	         							 # function name
function( ftp.filepath ){ 			             # input url
 tf <- tempfile()   					             # temporary object for download to local drive
 download.file(ftp.filepath , tf , mode = "wb")  # Download via FTP (save in binary format 'wb')
 read.xport( tf )					             # Returns R dataframe
}


pulldownSAS <-	         					     # function name
function( ftp.filepath ){ 			             # input url
 tf <- tempfile()   					             # temporary object for download to local drive
 download.file(ftp.filepath , tf , mode = "wb")  # Download via FTP (save in binary format 'wb')
 read_sas( tf )					                 # Returns R dataframe
}

##############################
### Specify data URLs (2001-2002)
# Demographics
DEMO <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/DEMO_B.xpt"

# BMI
BODY <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/BMX_B.xpt"

# Smoking
SMK <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/SMQ_B.xpt"

# Physical Activity Questionnaire
PA <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/PAQ_B.xpt"

# Physical Activity Individual Activities
PAI <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/PAQIAF_B.xpt"

# Housing Characteristics
HOME <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/HOQ_B.xpt"

# DNAm Epigenetic Biomarkers
DNAM <-  "https://wwwn.cdc.gov/Nchs/data/nhanes/dnam/dnmepi.sas7bdat"

# Telomere
TELO <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/TELO_B.xpt"

# HTN
HTN <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/BPQ_B.xpt"

# DM
DM <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/DIQ_B.xpt"

# ETOH use
ETOH <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/ALQ_B.xpt"

# Occupation
OCC <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/OCQ_B.xpt"

# Medical conditions
MED <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/MCQ_B.xpt"

# CBC w/ diff 
CBC <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/L25_B.xpt"

##############################

# Specify variables to keep (per dataset)

DEMOvars <- c(
"SEQN", 	      # unique identifier
"SDMVPSU",    # Masked Variance Pseudo-PSU (Primary Sampling Units)
"SDMVSTRA",   # Masked Variance Pseudo-Stratum 
"RIAGENDR",   # Gender
"RIDAGEYR",   # Age in years at screening
"RIDRETH2",   # Race  
"DMDEDUC2",   # Education
"DMDSCHOL",   # Now attending school? 
"DMDMARTL",   # Marital status
"DMDHHSIZ",   # Total number in household
"INDHHINC",   # Annual household income
"INDFMINC",   # Annual family income
"INDFMPIR",   # Family Poverty Income Ratio (PIR)
"RIDEXPRG"    # Pregnancy status at exam
)

BODYvars <- c(
"SEQN",
"BMXBMI"   # BMI
)

SMKvars <- c(
"SEQN",
"SMQ020",   # Smoking >= 100 cigs in life
"SMQ040",   # Smoke cigs now?
"SMD030",   # Age started smoking cigs
"SMD055",   # Age stopped smoking cigs
"SMD075",   # Years smoked
"SMD057",   # Number of cigs/day when quit
"SMD070"    # Number of cigs/day now
)

PAvars <- c(
"SEQN",
"PAD020",   # Walked or biked in past 30 days
"PAD080",   # Minutes per day (walk or bike)
"PAQ050Q",  # Number of times walk/bike 
"PAQ050U",  # Unit of measure (day/week/month)
"PAQ100",   # Tasks around home/yard in past 30 days
"PAD160",   # Minutes each time
"PAD120"    # Number of times in past 30 days
)

HOMEvars <- c(
"SEQN",
"HOQ065"    # Is this [home] owned or rented?
)
 
HTNvars <- c("SEQN",
             "BPQ020") # Ever told you had high blood pressure

DMvars <- c("SEQN",
            "DIQ010") # Doctor told you have diabetes

ETOHvars <- c("SEQN",
              "ALQ150") # Ever have 5 or more drinks every day?

OCCvars <- c(
  "SEQN",
  "OCD150",   # Type of work done last week 
  "OCQ380",    # Main reason did not work last week 
  "OCD230",   # Industry group code: current job
  "OCD240",   # Occupation group code: current job
  "OCD390"   #  Occupation group code: longest job
)

MEDvars <- c(
  "SEQN",
  "MCQ160C", # CAD
  "MCQ220" # Cancer 
)

CBCvars <- c(
  "SEQN",
  "LBXLYPCT", # Lymphocyte percent (%)
  "LBXMOPCT", # Monocyte percent (%)
  "LBXNEPCT", # Segmented neutrophils percent (%)
  "LBXEOPCT", # Eosinophils percent (%)
  "LBXBAPCT" # Basophils percent (%)
)


##############################
### Execute function to download and import
dat.DEMO <- pulldown(DEMO)
dat.BODY <- pulldown(BODY)
dat.SMK  <- pulldown(SMK)
dat.PA   <- pulldown(PA)
dat.PAI  <- pulldown(PAI)
dat.HOME <- pulldown(HOME)
dat.DNAM <- pulldownSAS(DNAM)
dat.TELO <- pulldown(TELO)
dat.HTN <- pulldown(HTN)
dat.DM <- pulldown(DM)
dat.ETOH <- pulldown(ETOH)
dat.OCC  <- pulldown(OCC)
dat.MED  <- pulldown(MED)
dat.CBC  <- pulldown(CBC)

### Loading physical activity variables, see separate R file for code to generate these variables 
load("./mvpa0102.Rdata") # n=4952, v=2

##############################

# Limit the data objects to only the variables needed for the analysis
dat.DEMO   <- dat.DEMO[,DEMOvars]
dat.BODY   <- dat.BODY[,BODYvars]
dat.SMK    <- dat.SMK[,SMKvars]
dat.PA     <- dat.PA[,PAvars]
dat.HOME   <- dat.HOME[,HOMEvars]
dat.HTN    <- dat.HTN[,HTNvars]
dat.DM     <- dat.DM[,DMvars]
dat.ETOH   <- dat.ETOH[,ETOHvars]
dat.OCC    <- dat.OCC[,OCCvars]
dat.MED   <- dat.MED[,MEDvars]
dat.CBC   <- dat.CBC[,CBCvars]

##############################

### Merge data objects (step-wise) by "SEQN"
d <- merge(dat.DNAM, dat.DEMO,  by="SEQN")			; dim(d); 
d <- merge(d,        dat.BODY,  by="SEQN", all.x=T); dim(d); 
d <- merge(d,        dat.SMK,   by="SEQN", all.x=T); dim(d); 
d <- merge(d,        dat.PA,    by="SEQN", all.x=T); dim(d); 
d <- merge(d,        dat.HOME,  by="SEQN", all.x=T)	; dim(d); 
d <- merge(d,        dat.TELO,  by="SEQN", all.x=T)	; dim(d); 
d <- merge(d,        dat.HTN,  by="SEQN", all.x=T)	
d <- merge(d,        dat.DM,  by="SEQN", all.x=T)	
d <- merge(d,        dat.ETOH,  by="SEQN", all.x=T)	
d <- merge(d,        dat.OCC,   by="SEQN", all.x=T)	
d <- merge(d,        dat.MED,   by="SEQN", all.x=T)
d <- merge(d,        dat.CBC,   by="SEQN", all.x=T)
d <- merge(d,        mvpa0102,   by="SEQN", all.x=T)	

# Check final object 'd'
dim(d) # n=2293, v=79

##############################

### Recode variables
# Race
d$race = ifelse(d$RIDRETH2 ==1, "White",
                ifelse(d$RIDRETH2 ==2, "Black",
                       ifelse(d$RIDRETH2 %in% c(3,5), "Hispanic",
                              ifelse(d$RIDRETH2 ==4, "Other", NA))))
table(d$RIDRETH2,d$race,exclude=NULL)
		 
# Smoking status		 
d$smoke = ifelse(d$SMQ020==1 &  d$SMQ040 %in% c(1,2), "current",
		  ifelse(d$SMQ020==1 & !d$SMQ040 %in% c(1,2), "former",
		  ifelse(d$SMQ020==2                        , "never", NA)))
table(d$SMQ020, d$SMQ040, d$smoke, exclude=NULL)


# Smoking pack-years
d$SMD075[d$SMD075 %in% c(777,999)] = NA
d$SMD057[d$SMD057 %in% c(777,999)] = NA
d$SMD030[d$SMD030 %in% c(0,777,999)] = NA
d$SMD055[d$SMD055 %in% c(777,999)] = NA
d$smkdur = d$SMD055-d$SMD030

d$packyears = 	ifelse(d$smoke=="never", 0,
				ifelse(d$smoke=="current", (d$SMD070/20)*d$SMD075,
			  	ifelse(d$smoke=="former",  (d$SMD057/20)*d$smkdur,
			     NA)))
tapply(d$packyears, d$smoke, function(x) summary(x,na.rm=TRUE))

####
### Physical Activity -> MET hours/week
# 1. List ALL required columns for this calculation
required_cols <- c("PAD020", "PAD080", "PAQ050U", "PAQ050Q",
                   "PAQ100", "PAD160", "PAD120")

# 2. Recode specific values for the main activity questions
# Code 3 ("Unable") as 2 ("No")
d$PAD020[d$PAD020 == 3] <- 2
d$PAQ100[d$PAQ100 == 3] <- 2

# Code 7 ("Refused") and 9 ("Don't know") as NA
d$PAD020[d$PAD020 %in% c(7, 9)] <- NA
d$PAQ100[d$PAQ100 %in% c(7, 9)] <- NA


# 3. Clean numeric special codes for all other variables
special_codes <- c(777, 999, 77777, 99999)
for (col in required_cols) {
  if (col %in% names(d)) {
    d[[col]][d[[col]] %in% special_codes] <- NA
  }
}

# 4. Calculate METs for Transportation (Walk/Bike)
# This section uses the newly cleaned PAD020.
d$freq_transport_wk <- ifelse(d$PAQ050U == 1, d$PAQ050Q * 7,
                       ifelse(d$PAQ050U == 2, d$PAQ050Q,
                       ifelse(d$PAQ050U == 3, d$PAQ050Q / 4.33,
                       0)))

d$met_hr_transport <- ifelse(!is.na(d$PAD020) & d$PAD020 == 1,
                             4.0 * (d$PAD080 / 60) * d$freq_transport_wk,
                             0)

# 5. Calculate METs for Home/Yard Tasks
# This section uses the newly cleaned PAQ100.
freq_home_wk <- d$PAD120 * (7/30)
d$met_hr_home <- ifelse(!is.na(d$PAQ100) & d$PAQ100 == 1,
                         4.5 * (d$PAD160 / 60) * freq_home_wk,
                         0)

# 6. Sum the MET-hours and clean up any NAs produced during calculations
d$total_met_hr_wk <- d$met_hr_transport + d$met_hr_home + d$mvpa_met_hr_wk
d$total_met_hr_wk[is.na(d$total_met_hr_wk)] <- 0


####

# Marital status
d$married = ifelse(d$DMDMARTL %in% c(1,6), "Married", # (+ Living with partner)
			ifelse(d$DMDMARTL %in% c(2:5), "Not married", # Widow+Divorced+Separated+NeverMarried
			ifelse(d$DMDMARTL %in% c(77,99), NA, NA)))


# Age categories
d$agecat = ifelse(d$RIDAGEYR>=12 & d$RIDAGEYR <20,"12-19",
		   ifelse(d$RIDAGEYR>=20 & d$RIDAGEYR <40,"20-39",
		   ifelse(d$RIDAGEYR>=40 & d$RIDAGEYR <60,"40-59",
		   ifelse(d$RIDAGEYR>=60 & d$RIDAGEYR <80,"60-79",
		   ifelse(d$RIDAGEYR>=80, "80+", NA)))))
tapply(d$RIDAGEYR,d$agecat,range)

# Sex
d$sex = ifelse(d$RIAGENDR ==1, "M",
		ifelse(d$RIAGENDR ==2, "F", NA))
table(d$RIAGENDR, d$sex, exclude=NULL)

# Education (5 levels)
d$educ = ifelse(d$DMDEDUC2==1, "<9th grade",
		 ifelse(d$DMDEDUC2==2, "9-11th grade",
		 ifelse(d$DMDEDUC2==3, "HS grad",	
		 ifelse(d$DMDEDUC2==4, "Some college",
 		 ifelse(d$DMDEDUC2==5, "College grad",
		 ifelse(d$DMDEDUC2==7, NA,
		 ifelse(d$DMDEDUC2==9, NA, NA)))))))
d$educ = factor(d$educ, levels=c("<9th grade","9-11th grade","HS grad","Some college","College grad"))
table(d$DMDEDUC2, d$educ, exclude=NULL)

# Education (3 levels)
d$educ3 = ifelse(d$DMDEDUC2 %in% c(1:2), "<HS",
		 ifelse(d$DMDEDUC2 %in% c(3:4), "HS grad",	#(+ Some college)
 		 ifelse(d$DMDEDUC2==5, "College grad",
		 ifelse(d$DMDEDUC2==7, NA,
		 ifelse(d$DMDEDUC2==9, NA, NA)))))
d$educ3 = factor(d$educ3, levels=c("<HS","HS grad","College grad"))
table(d$DMDEDUC2, d$educ3, exclude=NULL)

# Education (2 levels)
d$educ2 = ifelse(d$DMDEDUC2 %in% c(1:4), "No college",
 		 ifelse(d$DMDEDUC2==5, "College",
		 ifelse(d$DMDEDUC2==7, NA,
		 ifelse(d$DMDEDUC2==9, NA, NA))))
d$educ2 = factor(d$educ2, levels=c("No college","College"))
table(d$DMDEDUC2, d$educ2, exclude=NULL)

# Income (11 levels)
d$income = 	ifelse(d$INDHHINC==1, "0-4999",
			ifelse(d$INDHHINC==2, "5000-9999",
			ifelse(d$INDHHINC==3, "10000-14999",			
			ifelse(d$INDHHINC==4, "15000-19999",			
			ifelse(d$INDHHINC==5, "20000-24999",			
			ifelse(d$INDHHINC==6, "25000-34999",			
			ifelse(d$INDHHINC==7, "35000-44999",			
			ifelse(d$INDHHINC==8, "45000-54999",			
			ifelse(d$INDHHINC==9, "55000-64999",			
			ifelse(d$INDHHINC==10, "65000-74999",			
			ifelse(d$INDHHINC==11, "75000+",				
			ifelse(d$INDHHINC==12, NA,			
			ifelse(d$INDHHINC==13, NA,			
			ifelse(d$INDHHINC==77, NA,			
			ifelse(d$INDHHINC==99, NA, NA)))))))))))))))
d$income = factor(d$income, levels=c("0-4999", "5000-9999", "10000-14999", "15000-19999", "20000-24999", "25000-34999", "35000-44999", "45000-54999",
"55000-64999", "65000-74999", "75000+"))
table(d$INDHHINC, d$income, exclude=NULL)

d <- d %>%
  # Medical risk factors
  mutate(htn = case_when(
    BPQ020 == 1 ~ 1,
    BPQ020 == 2 ~ 0,
    TRUE ~ NA)) %>% 
  mutate(dm = case_when(
    DIQ010 == 1 ~ 1,
    DIQ010 == 2 ~ 0,
    TRUE ~ NA)) %>% 
  mutate(etoh = case_when(
    ALQ150 == 1 ~ 1,
    ALQ150 == 2 ~ 0,
    TRUE ~ NA)) %>% 
  mutate(cad = case_when(
    MCQ160C == 1 ~ 1,
    MCQ160C == 2 ~ 0, 
    TRUE ~ NA)) %>% 
  mutate(cancer = case_when(
    MCQ220 == 1 ~ 1,
    MCQ220 == 2 ~ 0, 
    TRUE ~ NA)) %>% 
  
  # Education: high school binary 
  mutate(educ_hs = case_when(
    DMDEDUC2 %in% c(1:3) ~ "High School or Less",
    DMDEDUC2 %in% c(4:5) ~ "Greater than High School",
    TRUE ~ NA)) %>% 
  mutate(educ_hs = factor(educ_hs, levels = c("High School or Less",
                                              "Greater than High School"))) %>%
  
  # Occupational variables 
  mutate(occ = case_when(
    OCD150 %in% c(1,2) ~ "Employed",
    OCD150 %in% c(3,4) & OCQ380 == 1 ~ "Homemaker", 
    OCD150 %in% c(3,4) & OCQ380 == 2 ~ "Student",
    OCD150 %in% c(3,4) & OCQ380 == 3 ~ "Retired", 
    OCD150 %in% c(3,4) & OCQ380 %in% c(4,6) ~ "Unable to work due to health/disability", 
    OCD150 %in% c(3,4) & OCQ380 %in% c(5,7) ~ "Unemployed/Other", 
    TRUE ~ NA)) %>% 
  mutate(occ = factor(occ, levels = c("Unemployed/Other",
                                      "Unable to work due to health/disability",
                                      "Retired",
                                      "Student", 
                                      "Homemaker", 
                                      "Employed"))) %>% 
  mutate(occ_bin = case_when(
    occ == "Employed" ~ "Currently Working",
    occ %in% c("Unemployed/Other",
               "Unable to work due to health/disability",
               "Retired",
               "Student", 
               "Homemaker") ~ "Not Currently Working",
    TRUE ~ NA)) %>%
  mutate(occ_bin = factor(occ_bin, levels = c("Not Currently Working",
                                              "Currently Working"))) %>% 
  
  # Blue/white collar based on David & Hanyang's code 
  mutate(occ_bw = case_when(
    OCD390 %in% c(1:7,9,25) | (is.na(OCD390) & OCD240 %in% c(1:7,9,25)) ~ "High, White Collar",
    OCD390 %in% c(8,10,12:16,22) | (is.na(OCD390) & OCD240 %in% c(8,10,12:16,22)) ~ "Low, White Collar",
    OCD390 %in% c(28:31,41) | (is.na(OCD390) & OCD240 %in% c(28:31,41)) ~ "High, Blue Collar",
    OCD390 %in% c(11,17:21,23,24,26,27,32:40) | (is.na(OCD390) & OCD240 %in% c(11,17:21,23,24,26,27,32:40)) ~ "Low, Blue Collar",
    OCD390 == 98 | (is.na(OCD390) & OCD240 == 98) ~ "Never Worked",
    TRUE ~ NA)) %>%
  mutate(occ_bw = factor(occ_bw, levels = c("Never Worked",
                                            "Low, Blue Collar",
                                            "High, Blue Collar",
                                            "Low, White Collar",
                                            "High, White Collar"))) %>% 
  
  mutate(occ_bw_bin = case_when(
    occ_bw %in% c("High, White Collar", "Low, White Collar") ~ "White Collar",
    occ_bw %in% c("High, Blue Collar", "Low, Blue Collar", "Never Worked") ~ "Blue Collar/Not Working",
    TRUE ~ NA)) %>% 
  mutate(occ_bw_bin = factor(occ_bw_bin, levels = c("Blue Collar/Not Working",
                                                    "White Collar"))) 

table(d$BPQ020, d$htn, exclude=NULL)
table(d$DIQ010, d$dm, exclude=NULL)
table(d$ALQ150, d$etoh, exclude=NULL)
table(d$OCD150, d$occ, exclude=NULL)
table(d$OCD150, d$occ_bin, exclude=NULL)
table(d$occ, d$occ_bin, exclude=NULL)
table(d$occ_bw, d$OCD390, exclude=NULL)
table(d$occ_bw, d$OCD240, exclude=NULL)
table(d$occ_bw_bin, d$occ_bw, exclude=NULL)
summary(d$educ_hs)
summary(d$occ_bw)
summary(d$occ_bw_bin)

#####
# Home ownership
d$home = ifelse(d$HOQ065 == 1, "Own",
		 ifelse(d$HOQ065 %in% c(2:3), "Don't Own",
		 ifelse(d$HOQ065 %in% c(7,9), NA, NA)))


# PIR (5 [highest] as reference)
d$pir5 = ifelse(d$INDFMPIR >= 0 & d$INDFMPIR <1, 0, 
		 ifelse(d$INDFMPIR >= 1 & d$INDFMPIR <2, 1, 
		 ifelse(d$INDFMPIR >= 2 & d$INDFMPIR <3, 2, 
		 ifelse(d$INDFMPIR >= 3 & d$INDFMPIR <4, 3, 
		 ifelse(d$INDFMPIR >= 4 & d$INDFMPIR <5, 4, 
		 ifelse(d$INDFMPIR >= 5                , 5, NA)))))) 

# PIR (dichotomous: split at median)		 
d$pirhi = ifelse(d$INDFMPIR >= median(d$INDFMPIR, na.rm=T), 1,
		  ifelse(d$INDFMPIR <  median(d$INDFMPIR, na.rm=T), 0, NA))

#####

# Convert Mean T/S Ratio (Telomeres) into base pairs 
# Per formula in documentation: https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/TELO_B.htm
d$telobp = 3274+2413*d$TELOMEAN

### Generate age-adjusted residuals
# Initialize column for residuals
d$grimres    = NA
d$horvathres = NA
d$phenores   = NA
d$telobpres  = NA

# Fit residuals
d$grimres[    complete.cases(d[,c("GrimAgeMort","RIDAGEYR")] )] = lm(GrimAgeMort ~ RIDAGEYR, data=d, na.action=na.exclude)$residuals
d$horvathres[ complete.cases(d[,c("HorvathAge", "RIDAGEYR")] )] = lm(HorvathAge  ~ RIDAGEYR, data=d, na.action=na.exclude)$residuals
d$phenores[   complete.cases(d[,c("PhenoAge",   "RIDAGEYR")] )] = lm(PhenoAge    ~ RIDAGEYR, data=d, na.action=na.exclude)$residuals
d$telobpres[  complete.cases(d[,c("telobp",     "RIDAGEYR")] )] = lm(telobp      ~ RIDAGEYR, data=d, na.action=na.exclude)$residuals

### Generate multi-variable-adjusted residuals
# Fit models (for plotting)
grimfit    = lm(GrimAgeMort ~ RIDAGEYR + factor(sex) + factor(race) + factor(educ3) + factor(married), data=d, na.action=na.exclude)
horvathfit = lm(HorvathAge  ~ RIDAGEYR + factor(sex) + factor(race) + factor(educ3) + factor(married), data=d, na.action=na.exclude)
phenofit   = lm(PhenoAge    ~ RIDAGEYR + factor(sex) + factor(race) + factor(educ3) + factor(married), data=d, na.action=na.exclude)
telofit    = lm(telobp      ~ RIDAGEYR + factor(sex) + factor(race) + factor(educ3) + factor(married), data=d, na.action=na.exclude)
dunedinfit = lm(DunedinPoAm ~            factor(sex) + factor(race) + factor(educ3) + factor(married), data=d, na.action=na.exclude)

# Initialize column for residuals
d$grimresadj    = NA
d$horvathresadj = NA
d$phenoresadj   = NA
d$telobpresadj  = NA
d$dunedinresadj = NA

# Add residuals (only to complete cases)
covarlist = c("RIDAGEYR", "sex", "race", "educ3", "married")
d$grimresadj[   complete.cases( d[,c("GrimAgeMort", covarlist)] )] <- residuals(grimfit)
d$horvathresadj[complete.cases( d[,c("HorvathAge",  covarlist)] )] <- residuals(horvathfit)
d$phenoresadj[  complete.cases( d[,c("PhenoAge",    covarlist)] )] <- residuals(phenofit)
d$telobpresadj[ complete.cases( d[,c("telobp",      covarlist)] )] <- residuals(telofit)
d$dunedinadj[   complete.cases( d[,c("DunedinPoAm", covarlist)] )] <- residuals(dunedinfit)

dim(d) # n=2293, v=118

### Output
d0102 <- d
save(d0102, file = "./d0102.Rdata")
write.csv(d, "NHANES-01-02-bioage.csv", row.names=F, quote=F, na="")


