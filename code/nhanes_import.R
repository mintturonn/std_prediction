
library(cowplot)
library(here)
library(nhanesA)
library(survey)
library(tidyverse)
library(labelled)

source(here('code/nhanes_import_funs.R'))

# install.packages("SASxport")
# require(SASxport)
# library(foreign)
# to read downloaded files:
# DEMO <- read.xport("SurveyData\\DEMO_I.XPT")


#install.packages("nhanesA")

# Within the CDC website, NHANES data are available in 5 categories
# Demographics (DEMO)
# Dietary (DIET)
# Examination (EXAM)
# Laboratory (LAB)
# Questionnaire (Q)

nhanesTables(data_group='DEMO', year=1999)
nhanesTables(data_group='DEMO', year=2015)

nhanesTables(data_group='DEMO', year=2018)
nhanesTables(data_group='LAB', year=2016)
nhanesTables(data_group='Q', year=2018)


# names(demo)
# browseNHANES(year = 2018)

# cycle18_19 <- cycle_impmerge_fun("DEMO_K_R", "UCPREG_K_R", "ALQ_K_R", "RHQ_K_R",
#                                  "HIV_K_R", FALSE)

# cycle17_18 <- cycle_impmerge_fun("DEMO_J", "UCPREG_J", "SXQ_J", "RHQ_J",
#                                  "HIV_J", FALSE)

cycle15_16 <- cycle_impmerge_fun("DEMO_I", "UCPREG_I", "SXQ_I", "RHQ_I",
                                   "HIV_I", addvar=TRUE, abvar=TRUE, "CHLMDA_I", "HSV_I", "TRICH_I", "SSCT_I")

cycle13_14 <- cycle_impmerge_fun("DEMO_H", "UCPREG_H", "SXQ_H", "RHQ_H",
                                 "HIV_H", addvar=TRUE, "CHLMDA_H", "HSV_H", "TRICH_H", "SSCT_H")

cycle11_12 <- cycle_impmerge_fun("DEMO_G", "UCPREG_G", "SXQ_G", "RHQ_G",
                                 "HIV_G", addvar=TRUE, "CHLMDA_G", "HSV_G", "HEPB_S_G") # n.b. no trich, replaced with hep-B

cycle09_10 <- cycle_impmerge_fun("DEMO_F", "UCPREG_F", "SXQ_F", "RHQ_F",
                                 "HIV_F", addvar=TRUE, "CHLMDA_F", "HSV_F", "HEPB_S_F") # n.b. no trich, replaced with hep-B

cycle07_08 <- cycle_impmerge_fun("DEMO_E", "UCPREG_E", "SXQ_E", "RHQ_E",
                                 "HIV_E", addvar=TRUE, "CHLMDA_E", "HSV_E", "HEPB_S_E") # n.b. no trich, replaced with hep-B

cycle05_06 <- cycle_impmerge_fun("DEMO_D", "UCPREG_D", "SXQ_D", "RHQ_D",
                                 "HIV_D", addvar=TRUE, "CHLMDA_D", "HSV_D", "HEPC_D") # n.b. no trich, replaced with hep-C

cycle03_04 <- cycle_impmerge_fun("DEMO_C", "UC_C", "SXQ_C", "RHQ_C",
                                 "L03_C", addvar=TRUE, "L05_C", "l09_c", "L37SWA_C") # n.b. no trich, replaced with hep-C

cycle01_02 <- cycle_impmerge_fun("DEMO_B", "UC_B", "SXQ_B", "RHQ_B",
                                 "L03_B", addvar=TRUE, "L05_B", "l09_b", "l34_b") # n.b. no trich, replaced with hep-C

cycle99_00 <- cycle_impmerge_fun("DEMO", "UC", "SXQ", "RHQ",
                                 "LAB03", addvar=TRUE, "LAB05", "lab09", "Lab02") # n.b. no trich, replaced with hep-C

# names99_00 <- colnames(cycle99_00)
# names01_02 <- colnames(cycle01_02)
# 
# 
# names13_14 <- colnames(cycle13_14)
# names15_16 <- colnames(cycle15_16)
# 
# 

# cycle15_16 %>%
#     full_join(cycle13_14, by = names15_16[!(names15_16 %in% names13_14)]) -> test    # %>%
# 


cycle99_00[[1]] %>%    
  bind_rows(., cycle01_02[[1]]) %>%    
  bind_rows(., cycle03_04[[1]]) %>%    
  bind_rows(., cycle05_06[[1]]) %>%    
  bind_rows(., cycle07_08[[1]]) %>%  
  bind_rows(., cycle09_10[[1]]) %>% 
  bind_rows(., cycle11_12[[1]]) %>% 
  bind_rows(., cycle13_14[[1]]) %>% 
  bind_rows(., cycle15_16[[1]]) -> nhns 

# store the vars and labels from each cycle

nhns.labs <- list(cycle99_00 = cycle99_00[[2]], cycle01_02 = cycle01_02[[2]], 
                  cycle03_04 = cycle03_04[[2]], cycle05_06 = cycle05_06[[2]], 
                  cycle07_08 = cycle07_08[[2]], cycle09_10 = cycle09_10[[2]], 
                  cycle11_12 = cycle11_12[[2]], cycle13_14 = cycle13_14[[2]],
                  cycle15_16 = cycle15_16[[2]]) 

rm(cycle99_00, cycle01_02, cycle03_04, cycle05_06, cycle07_08, cycle09_10, cycle11_12,
   cycle13_14, cycle15_16)

nhns$ct[nhns$URXUCL==1] <- 1
nhns$ct[nhns$URXUCL==2] <- 0

# N.B. there are a few responses as 7 (refused) and 9 (dont know) - these are now NA
nhns$ctd[nhns$SXQ272==1] <- 1
nhns$ctd[nhns$SXQ272==2] <- 0

# Chlamydia Pgp3 ELISA
nhns$ct_elisa[nhns$SSCTEIA==0] <- 0
nhns$ct_elisa[nhns$SSCTEIA==1] <- 1

# Chlamydia Pgp3 MBA
nhns$ct_mba[nhns$SSCTMBA==0] <- 0
nhns$ct_mba[nhns$SSCTMBA==1] <- 1

nhns$ct_ab[nhns$ct_elisa==1 & nhns$ct_mba==1] <- 1
nhns$ct_ab[nhns$ct_elisa==0 & nhns$ct_mba==1] <- 0
nhns$ct_ab[nhns$ct_elisa==1 & nhns$ct_mba==0] <- 0
nhns$ct_ab[nhns$ct_elisa==0 & nhns$ct_mba==0] <- 0

## combined CT&diag
nhns$anyct[nhns$ct==0 & nhns$ctd==0] <- "No CT"
nhns$anyct[nhns$ct==0 & nhns$ctd==1] <- "CT diag."
nhns$anyct[nhns$ct==1 & nhns$ctd==0] <- "prev. CT"
nhns$anyct[nhns$ct==1 & nhns$ctd==1] <- "pr&di CT"
nhns$anyct <- factor(nhns$anyct)

## GC
nhns$gc[nhns$URXUGC==1] <- 1
nhns$gc[nhns$URXUGC==2] <- 0

# N.B. there are a few responses as 7 (refused) and 9 (dont know) - these are now NA
nhns$gcd[nhns$SXQ270==1] <- 1
nhns$gcd[nhns$SXQ270==2] <- 0

# combined
nhns$anygc[nhns$gc==0 & nhns$gcd==0] <- 0
nhns$anygc[nhns$gc==0 & nhns$gcd==1] <- 1
nhns$anygc[nhns$gc==1 & nhns$gcd==0] <- 2
nhns$anygc[nhns$gc==1 & nhns$gcd==1] <- 3

# recode race/ethnicity
nhns$race[nhns$RIDRETH1 == 3] <- "white"
nhns$race[nhns$RIDRETH1 == 1 | nhns$RIDRETH1 == 2] <- "hispanic"
nhns$race[nhns$RIDRETH1 == 4] <- "black"
nhns$race[nhns$RIDRETH1 == 5] <- "other"

# recode gender
nhns$gender[nhns$RIAGENDR == 1] <- "male"
nhns$gender[nhns$RIAGENDR == 2] <- "female"

# recode age
nhns$age[nhns$RIDAGEYR < 25] <- "u25"
nhns$age[nhns$RIDAGEYR > 24 & nhns$RIDAGEYR < 40] <- "o25"

# recode MSM status
nhns$msm[nhns$SXQ292 == 1] <- "msw"
nhns$msm[nhns$SXQ292 == 2 | nhns$SXQ292 == 3] <- "msm"




# $SDDSRVYR
# [1] "Data release cycle"
# 
# $RIAGENDR
# [1] "Gender"
# 
# $RIDAGEYR
# [1] "Age in years at screening"
# 
# $RIDRETH1
# [1] "Race/Hispanic origin"

# $SXQ270 "MD ever told you had gonorrhea (B)"
# 
# $SXQ272 "MD ever told you had chlamydia (B)"

# $SXQ296 "Describe sexual identity (F)"
# $SXQ296 "Describe sexual identity (M)"
#  "Ever treated for a pelvic infection/PID?" $RHQ078
# pregancy $RHD143


# d %>%    
#   bind_rows(., e) -> test2 
# 
# 
# 
#     full_join(cycle03_04, by = "SEQN") %>%
#     full_join(cycle05_06, by = "SEQN") %>%
#     full_join(cycle07_08, by = "SEQN") %>%
#     full_join(cycle09_10, by = "SEQN") %>%
#     full_join(cycle11_12, by = "SEQN") %>%
#     full_join(cycle13_14, by = "SEQN") %>%
#       
# 

# 
# nhns$weight <- XXX
# 

