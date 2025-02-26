
### FROM STATA
# 
# *Generate survey weight
# * To combine 99-14 (16 years) of data, denominator needs to be 8
# * See http://www.cdc.gov/nchs/tutorials/NHANES/SurveyDesign/Weighting/Task2.htm
# 
# gen  mec16year=.
# * Different survey weight for 99-02 (4 year)
# replace mec16year  = 1/4 * wtmec4yr if sddsrvyr==1 | sddsrvyr==2
# * 2 year syrvey weight 
# replace mec16year  = 1/8 * wtmec2yr if sddsrvyr>=3
# 
# 
# * Set survey dataset (save after, and the survey setup should be retained in stata memort)
# 
# svyset [w= mec16year], psu(sdmvpsu) strata(sdmvstra) vce(linearized)

nhns %>%
  group_by(msm_active, SDDSRVYR) %>%
  summarise(n=n()) %>%
  spread(SDDSRVYR, n) %>%
  knitr::kable()

nhns %>%
  group_by(gc, SDDSRVYR) %>%
  summarise(n=n()) %>%
  spread(SDDSRVYR, n) %>%
  knitr::kable()

nhns %>%
  group_by(gcd, SDDSRVYR) %>%
  summarise(n=n()) %>%
  spread(SDDSRVYR, n) %>%
  knitr::kable()

nhns %>%
 # filter(SDDSRVYR < 6) %>% # no GC measures after 
  group_by(msm_actcat, msm_active) %>%
  summarise(n=n()) %>%
  spread(msm_active, n) %>%
  knitr::kable() 

##### TABULATE

# prevalence
nhns %>%
  group_by(ct, SDDSRVYR, gender, age) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(ct, n) %>%
  arrange(gender, age) -> tab

nhns %>%
  group_by(ctd, SDDSRVYR, gender) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(ctd, n) %>%
  arrange(gender) -> tab2

###### NSFG 
nsg %>%
  group_by(anyct, year, age) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(anyct, n) %>%
  arrange(year, age) -> tab

nsg  %>%
  group_by(anyct, year) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(anyct, n) %>%
  arrange(year) -> tab2


# CHECKS
vdat %>%
  filter(date>ymd("2021-02-28")) %>%
  group_by(ever_pos, status) %>%
  summarise(n=n()) %>%
  spread(status, n) %>%
  knitr::kable()