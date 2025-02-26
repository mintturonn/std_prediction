
read.csv(here("data/cdc_gc_ct_national.csv"), skip=7) %>%
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
  select(index_year, NAME, Gender, age_c, cases_ct, cases_gc, population_ct, population_gc, rate_ct, rate_gc) -> ctdat


