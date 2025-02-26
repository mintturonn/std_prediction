
# states() %>%
#   select(STUSPS, NAME) %>%
#   rename(state_2 = STUSPS) -> state_names

state_names <- data.frame(
                state_2 = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                            "DC", "PR"), 
                NAME = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                         "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                         "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                         "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                         "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                         "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                         "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                         "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                         "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                         "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                         "District of Columbia", "Puerto Rico") 
)


read.csv(here("data/cdc_gc_ct_state.csv"), skip=7) %>%
  mutate(rate = as.numeric(gsub(",", "", Rate.per.100000))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  rename(NAME = Geography)  %>%
  mutate(Gender = ifelse(Sex=="Female", "F", "M")) %>%
  mutate(age_c = ifelse(Age.Group=="15-19" | Age.Group=="20-24", "15-24", 
                        ifelse(Age.Group=="25-29" | Age.Group=="30-34", "25-34", 
                               ifelse(Age.Group=="35-39" | Age.Group=="40-44", "35-44", 
                                      ifelse(Age.Group=="45-54", "45-54",
                                             ifelse(Age.Group=="55-64" | Age.Group=="65+", ">=55", NA)))))) %>%
  mutate(index_year = case_when(
    Year == "2020 (COVID-19 Pandemic)" ~ 2020,
    TRUE ~ as.numeric(gsub("[^0-9]", "", Year))
  )) %>%
  mutate(infection = ifelse(Indicator == "Chlamydia", "ct", "gc")) %>%
  group_by(NAME, Gender, age_c, index_year, infection) %>%
  summarize(cases = sum(cases),
            population = sum(population)) %>%
  ungroup() %>%
  mutate(rate = cases / population) %>%
  filter(!is.na(age_c)) %>%
  pivot_wider(names_from = "infection", values_from = c("cases", "population", "rate")) %>%
  select(index_year, NAME, Gender, age_c, cases_ct, cases_gc, population_ct, population_gc, rate_ct, rate_gc) -> cdc_diag_ct_gc

# read.csv(here("data/cdc_gc_ct_hiv_state.csv"), skip=8) %>%
#   mutate(rate_gc = as.numeric(gsub(",", "", Rate.per.100000))) %>%
#   mutate(population_gc = as.numeric(gsub(",", "", Population))) %>%
#   mutate(cases_gc = as.numeric(gsub(",", "", Cases))) %>%
#   rename(NAME = Geography)  %>%
#   mutate(Gender = ifelse(Sex=="Female", "F", "M")) %>%
#   mutate(age_c = ifelse(Age.Group=="13-24", "15-24", 
#                         ifelse(Age.Group=="25-34", "25-34", 
#                                ifelse(Age.Group=="35-44", "35-44", 
#                                       ifelse(Age.Group=="45-54", "45-54",
#                                              ifelse(Age.Group=="55+", ">=55", NA)))))) %>%
#   mutate(index_year = ifelse(Year=="2021", 2021, 
#                              ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, 
#                                     ifelse(Year=="2019", 2019, 
#                                            ifelse(Year=="2018", 2018, NA))))) %>%
#   filter(Indicator == "Gonorrhea") %>%
#   select(index_year, NAME, Gender, age_c, rate_gc, population_gc, cases_gc) -> cdc_diag_gc
  

# Data are only 
read.csv("/Users/minttu/restricted_files/ct_year_state_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) -> ct_age_sex


read.csv("/Users/minttu/restricted_files/gc_year_state_sex_age.csv") %>%
  mutate(positivity_gc = num_test/den_test) %>%
  rename(num_test_gc= num_test,
         den_test_gc= den_test) -> gc_age_sex

cdc_diag_ct_gc %>%
  left_join(state_names, by="NAME") %>% 
  left_join(ct_age_sex, by= c("index_year", "state_2", "Gender", "age_c")) %>%
  left_join(gc_age_sex, by= c("index_year", "state_2", "Gender", "age_c")) %>%
  mutate(pop_prop = den_test_ct/population_ct) %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  # filter(index_year > 2009) %>%
  filter(index_year == 2023) -> gc_ct_age_sex

############################################
## Reformatting the data for Stan
# WOMEN
gc_ct_age_sex %>%
  filter(Gender == "F") %>%
  filter(NAME != "American Samoa" &  
         NAME != "Guam" &
         NAME != "Northern Mariana Islands" &
         NAME != "U.S. Virgin Islands" &
         NAME != "Palau") %>%
  mutate(num_test_ct2 = ifelse(is.na(num_test_ct) & !is.na(den_test_ct), 1, num_test_ct)) %>%
  mutate(num_test_gc2 = ifelse(is.na(num_test_gc) & !is.na(den_test_gc), 1, num_test_gc)) %>%
  arrange(state_2, age_c, index_year)  -> dat_f

years_num <- length(unique(dat_f$index_year))
age_num <- length(unique(dat_f$age_c))
state_num <- length(unique(dat_f$state_2))

# chlamydia claims data
# ctnum_f_array <- array(dat_f$num_test_ct2, dim = c(years_num, age_num, state_num))
ctnum_f_array <- array(dat_f$num_test_ct, dim = c(years_num, age_num, state_num))
ctden_f_array <- array(dat_f$den_test_ct, dim = c(years_num, age_num, state_num))
# which(is.na(ctden_f_array)) %in% which(is.na(ctnum_f_array)) 

f_ct_id   <- as.data.frame(which(!is.na(ctnum_f_array), arr.ind = TRUE))
ctden_f_array[which(is.na(ctnum_f_array))] <- -999
ctnum_f_array[which(is.na(ctnum_f_array))] <- -999

# gonorrhea claims data
# gcnum_f_array <- array(dat_f$num_test_gc2, dim = c(years_num, age_num, state_num))
gcnum_f_array <- array(dat_f$num_test_gc, dim = c(years_num, age_num, state_num))
gcden_f_array <- array(dat_f$den_test_gc, dim = c(years_num, age_num, state_num))
# which(is.na(gcden_f_array)) %in% which(is.na(gcnum_f_array)) 

f_gc_id   <- as.data.frame(which(!is.na(gcnum_f_array), arr.ind = TRUE))
#f_gc_na   <- as.data.frame(which(is.na(gcnum_f_array), arr.ind = TRUE))

gcden_f_array[which(is.na(gcnum_f_array))] <- -999
gcnum_f_array[which(is.na(gcnum_f_array))] <- -999

# for (i in 1:nrow(f_gc_na) ){
#  test <- rbind(test, gcnum_f_array[f_gc_id[i,1] , f_gc_id[i,2], f_gc_id[i,3]] )
# }

# chlamydia diagnosis data
ctdnum_f_array <- array(dat_f$cases_ct, dim = c(years_num, age_num, state_num))
ctdden_f_array <- array(dat_f$population_ct, dim = c(years_num, age_num, state_num))
# ! HERE denominator has 
#   which(is.na(ctdden_f_array)) %in%  which(is.na(ctdnum_f_array)) 

f_ctd_id   <- as.data.frame(which(!is.na(ctdnum_f_array), arr.ind = TRUE))
ctdden_f_array[which(is.na(ctdnum_f_array))] <- -999
ctdnum_f_array[which(is.na(ctdnum_f_array))] <- -999

# gonorrhea diagnosis data
gcdnum_f_array <- array(dat_f$cases_gc, dim = c(years_num, age_num, state_num))
gcdden_f_array <- array(dat_f$population_gc, dim = c(years_num, age_num, state_num))
# which(is.na(gcdden_f_array)) %in% which(is.na(gcdnum_f_array)) 

f_gcd_id   <- as.data.frame(which(!is.na(gcdnum_f_array), arr.ind = TRUE))
gcdden_f_array[which(is.na(gcdnum_f_array))] <- -999
gcdnum_f_array[which(is.na(gcdnum_f_array))] <- -999

# population
pop_f_array <- array(dat_f$population_ct, dim = c(years_num, age_num, state_num))


#########
# MEN
gc_ct_age_sex %>%
  filter(Gender == "M") %>%
  filter(NAME != "American Samoa" &  
           NAME != "Guam" &
           NAME != "Northern Mariana Islands" &
           NAME != "U.S. Virgin Islands" &
           NAME != "Palau") %>%
  mutate(num_test_ct2 = ifelse(is.na(num_test_ct) & !is.na(den_test_ct), 1, num_test_ct)) %>%
  mutate(num_test_gc2 = ifelse(is.na(num_test_gc) & !is.na(den_test_gc), 1, num_test_gc)) %>%
  arrange(state_2, age_c, index_year) -> dat_m

# chlamydia claims data
# ctnum_m_array <- array(dat_m$num_test_ct2, dim = c(years_num, age_num, state_num))
ctnum_m_array <- array(dat_m$num_test_ct, dim = c(years_num, age_num, state_num))
ctden_m_array <- array(dat_m$den_test_ct, dim = c(years_num, age_num, state_num))
# which(is.na(ctden_m_array)) %in% which(is.na(ctnum_m_array)) 

m_ct_id   <- as.data.frame(which(!is.na(ctnum_m_array), arr.ind = TRUE))
ctden_m_array[which(is.na(ctnum_m_array))] <- -999
ctnum_m_array[which(is.na(ctnum_m_array))] <- -999

# gonorrhea claims data
# gcnum_m_array <- array(dat_m$num_test_gc2, dim = c(years_num, age_num, state_num))
gcnum_m_array <- array(dat_m$num_test_gc, dim = c(years_num, age_num, state_num))
gcden_m_array <- array(dat_m$den_test_gc, dim = c(years_num, age_num, state_num))
# which(is.na(gcden_m_array)) %in% which(is.na(gcnum_m_array)) 

m_gc_id   <- as.data.frame(which(!is.na(gcnum_m_array), arr.ind = TRUE))
#m_gc_na   <- as.data.frame(which(is.na(gcnum_m_array), arr.ind = TRUE))

gcden_m_array[which(is.na(gcnum_m_array))] <- -999
gcnum_m_array[which(is.na(gcnum_m_array))] <- -999

# for (i in 1:nrow(m_gc_na) ){
#  test <- rbind(test, gcnum_m_array[m_gc_id[i,1] , m_gc_id[i,2], m_gc_id[i,3]] )
# }

# chlamydia diagnosis data
ctdnum_m_array <- array(dat_m$cases_ct, dim = c(years_num, age_num, state_num))
ctdden_m_array <- array(dat_m$population_ct, dim = c(years_num, age_num, state_num))
# ! HERE denominator has 
#   which(is.na(ctdden_m_array)) %in%  which(is.na(ctdnum_m_array)) 

m_ctd_id   <- as.data.frame(which(!is.na(ctdnum_m_array), arr.ind = TRUE))
ctdden_m_array[which(is.na(ctdnum_m_array))] <- -999
ctdnum_m_array[which(is.na(ctdnum_m_array))] <- -999

# gonorrhea diagnosis data
gcdnum_m_array <- array(dat_m$cases_gc, dim = c(years_num, age_num, state_num))
gcdden_m_array <- array(dat_m$population_gc, dim = c(years_num, age_num, state_num))
# which(is.na(gcdden_m_array)) %in% which(is.na(gcdnum_m_array)) 

m_gcd_id   <- as.data.frame(which(!is.na(gcdnum_m_array), arr.ind = TRUE))
gcdden_m_array[which(is.na(gcdnum_m_array))] <- -999
gcdnum_m_array[which(is.na(gcdnum_m_array))] <- -999

# population
pop_m_array <- array(dat_m$population_ct, dim = c(years_num, age_num, state_num))




