

read_excel(here("data/chlamydia_national.xlsx"), skip=6) %>%
  select(Year, `Age Group`, Sex, Cases, `Rate per 100000`, Population) %>%
  pivot_wider(names_from = `Age Group`, values_from = c(Cases, `Rate per 100000`, `Population`)) %>%
  mutate(year = as.numeric( ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year ) )) %>%
  mutate( cases_15_24 = `Cases_20-24` + `Cases_15-19`) %>%
  mutate( cases_25_39 = `Cases_35-39` + `Cases_30-34` + `Cases_25-29` ) %>%
  mutate( pop_15_24 = as.numeric(`Population_20-24`) + as.numeric(`Population_15-19`)) %>%
  mutate( pop_25_39 = as.numeric(`Population_35-39`) + as.numeric(`Population_30-34`) + as.numeric(`Population_25-29`) )  %>%
  mutate( pc_15_24 = cases_15_24 / pop_15_24) %>%
  mutate( pc_25_39 = cases_25_39 / pop_25_39) %>%
  arrange(Sex, Year) -> ctdat


ctdat %>%
  ggplot()+
  geom_point(aes(x = year, y = pc_15_24, color = Sex), shape = 2) +
  geom_line(aes(x = year, y = pc_15_24, color = Sex)) +
  geom_point(aes(x = year, y = pc_25_39, color = Sex), shape = 3 ) +
  geom_line(aes(x = year, y = pc_25_39, color = Sex)) +
  ylim(c(0, 0.05))