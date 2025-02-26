
## Surveillance data change in 2020

##########################################################################
# ct
read.csv("/Users/minttu/restricted_files/ct_week_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  mutate(index_year = as.numeric(substr(index_year_week, 1, 4)), 
         index_week = as.numeric(substr(index_year_week, 5, 40))) -> wk_ct

wk_ct %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=index_week, y=positivity_ct, color=as.factor(index_year))) +
  geom_line(linewidth=0.5) +
  #geom_vline(xintercept = 2020, linewidth=0.1, linetype = "dashed") +
  mytheme3 + #theme(legend.position = "none") +
  facet_wrap(~ age_c+Gender) + ylab("positivity in samples") + xlab("week") -> p0

ggsave(filename = here("figs/positivity_by_week.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

##########################################################################
# ct
read.csv("/Users/minttu/restricted_files/ct_week_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  mutate(index_year = as.numeric(substr(index_year_week, 1, 4)), 
         index_week = as.numeric(substr(index_year_week, 5, 40))) -> wk_ct

wk_ct %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=index_week, y=positivity_ct, color=as.factor(index_year))) +
  geom_line(linewidth=0.5) +
  #geom_vline(xintercept = 2020, linewidth=0.1, linetype = "dashed") +
  mytheme3 + #theme(legend.position = "none") +
  facet_wrap(~ age_c+Gender) + ylab("positivity in samples") + xlab("week") -> p0

ggsave(filename = here("figs/positivity_by_week_ct.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()
##########################################################################
# gc
read.csv("/Users/minttu/restricted_files/gc_week_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  mutate(index_year = as.numeric(substr(index_year_week, 1, 4)), 
         index_week = as.numeric(substr(index_year_week, 5, 40))) -> wk_gc

wk_gc %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=index_week, y=positivity_ct, color=as.factor(index_year))) +
  geom_line(linewidth=0.5) +
  #geom_vline(xintercept = 2020, linewidth=0.1, linetype = "dashed") +
  mytheme3 + #theme(legend.position = "none") +
  facet_wrap(~ age_c+Gender) + ylab("positivity in samples") + xlab("week") -> p1

ggsave(filename = here("figs/positivity_by_week_gc.png"),
       plot = p1,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

##########################################################################
# CDC Data 
# states() %>%
#   select(STUSPS, NAME) %>%
#   rename(state_2 = STUSPS) -> state_names
# 
# 
# read.csv(here("data/cdc_gc_ct_state.csv"), skip=7) %>%
#   mutate(rate = as.numeric(gsub(",", "", Rate.per.100000))) %>%
#   mutate(population = as.numeric(gsub(",", "", Population))) %>%
#   mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
#   rename(NAME = Geography)  %>%
#   mutate(Gender = ifelse(Sex=="Female", "F", "M")) %>%
#   mutate(age_c = ifelse(Age.Group=="15-19" | Age.Group=="20-24", "15-24", 
#                         ifelse(Age.Group=="25-29" | Age.Group=="30-34", "25-34", 
#                                ifelse(Age.Group=="35-39" | Age.Group=="40-44", "35-44", 
#                                       ifelse(Age.Group=="45-54", "45-54",
#                                              ifelse(Age.Group=="55-64" | Age.Group=="65+", ">=55", NA)))))) %>%
#   mutate(index_year = ifelse(Year=="2021", 2021, 
#                              ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, 
#                                     ifelse(Year=="2019", 2019, 
#                                            ifelse(Year=="2018", 2018, 
#                                                   ifelse(Year=="2022", 2022, NA)))))) %>%
#   mutate(infection = ifelse(Indicator == "Chlamydia", "ct", "gc")) %>%
#   group_by(NAME, Gender, age_c, index_year, infection) %>%
#   summarize(cases = sum(cases),
#             population = sum(population)) %>%
#   ungroup() %>%
#   mutate(rate = cases / population) %>%
#   filter(!is.na(age_c)) %>%
#   pivot_wider(names_from = "infection", values_from = c("cases", "population", "rate")) %>%
#   select(index_year, NAME, Gender, age_c, cases_ct, cases_gc, population_ct, population_gc, rate_ct, rate_gc) %>%
#   pivot_wider(names_from = index_year, values_from = c( "cases_ct", "cases_gc", "population_ct", "population_gc", "rate_ct",  "rate_gc"  )) %>%
#   mutate(ct_red20_19 = (rate_ct_2019-rate_ct_2020)/rate_ct_2019,
#          gc_red20_19 = (rate_gc_2019-rate_gc_2020)/rate_gc_2019,
#          ct_red21_19 = (rate_ct_2019-rate_ct_2021)/rate_ct_2019,
#          gc_red21_19 = (rate_gc_2019-rate_gc_2021)/rate_gc_2019) -> diag_df


cdc_diag_ct_gc %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=index_year, y=rate_ct, color=NAME)) +
  geom_line(linewidth=0.1) +
  geom_vline(xintercept = 2020, linewidth=0.1, linetype = "dashed") +
  mytheme3 + theme(legend.position = "none") +
  facet_wrap(~ age_c+Gender, scales="free_y") + ylab("diagnosis rate per capita") -> p2

ggsave(filename = here("figs/diagrate_ct_state.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()


# this is from read claims data
cdc_diag_ct_gc %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=index_year, y=rate_gc, color=NAME)) +
  geom_line(linewidth=0.1) +
  geom_vline(xintercept = 2020, linewidth=0.1, linetype = "dashed") +
  mytheme3 + theme(legend.position = "none") +
  facet_wrap(~ age_c+Gender, scales="free_y") + ylab("diagnosis rate per capita") -> p3

ggsave(filename = here("figs/diagrate_gc_state.png"),
       plot = p3,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

#####################################
# Diagnosis relative change
# chlamydia
diag_df %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=age_c, y=-100*ct_red20_19, fill=Gender)) +
  geom_bar(stat="identity", width=0.9, position = "dodge") +
  mytheme2 + # theme(legend.position = "none") +
  facet_geo(~ NAME, grid = "us_state_with_DC_PR_grid1", scales="free_y") + ylab("percentage change, relative to 2019")

diag_df %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=age_c, y=-100*ct_red21_19, fill=Gender)) +
  geom_bar(stat="identity", width=.9, position = "dodge") +
  mytheme2 + # theme(legend.position = "none") +
  facet_geo(~ NAME, grid = "us_state_with_DC_PR_grid1", scales="free_y") + ylab("percentage change, relative to 2019")

# gonorrhea
diag_df %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=age_c, y=-100*gc_red20_19, fill=Gender)) +
  geom_bar(stat="identity", width=0.9, position = "dodge") +
  mytheme2 + # theme(legend.position = "none") +
  facet_geo(~ NAME, grid = "us_state_with_DC_PR_grid1", scales="free_y") + ylab("percentage change, relative to 2019")

diag_df %>%
  mutate(age_c = factor(age_c, levels= c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(x=age_c, y=-100*gc_red21_19, fill=Gender)) +
  geom_bar(stat="identity", width=.9, position = "dodge") +
  mytheme2 + # theme(legend.position = "none") +
  facet_geo(~ NAME, grid = "us_state_with_DC_PR_grid1", scales="free_y") + ylab("percentage change, relative to 2019")





