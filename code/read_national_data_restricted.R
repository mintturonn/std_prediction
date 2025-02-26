

# Data are only 
read.csv("/Users/minttu/restricted_files/ct_year_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) -> ct_age_sex_nat


read.csv("/Users/minttu/restricted_files/gc_year_sex_age.csv") %>%
  mutate(positivity_gc = num_test/den_test) %>%
  rename(num_test_gc= num_test,
         den_test_gc= den_test) -> gc_age_sex_nat


ct_age_sex_nat %>%
  ggplot()+
  geom_point(aes(x = index_year, y = positivity_ct, color = age_c), shape = 2) +
  geom_line(aes(x = index_year, y = positivity_ct, color = age_c)) +
  facet_wrap(~Gender) +
  ylim(c(0, NA))

gc_age_sex_nat %>%
  ggplot()+
  geom_point(aes(x = index_year, y = positivity_gc, color = age_c), shape = 2) +
  geom_line(aes(x = index_year, y = positivity_gc, color = age_c)) +
  facet_wrap(~Gender) +
  ylim(c(0, NA))