

library(here)
library(rstan)
library(bayesplot)
library(geofacet)
library(ggplot2)
library(readxl)
library(reshape2)
library(tigris)
library(tidyverse)


# Data are only 
read.csv("/Users/minttu/restricted_files/ct_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) -> ct_age_sex_nat


read.csv("/Users/minttu/restricted_files/gc_sex_age.csv") %>%
  mutate(positivity_gc = num_test/den_test) %>%
  rename(num_test_gc= num_test,
         den_test_gc= den_test) -> gc_age_sex_nat

read_excel(here("data/nhanes_nsfg.xlsx"), sheet = "nhanes") %>%
  filter(age != "all"  ) %>%
  # filter(year > 2009) %>%
  filter(year == 2016) %>% 
  mutate(p = ct/ct_N) -> nhnsd_nat

# Data by year 
read.csv("/Users/minttu/restricted_files/ct_year_sex_age.csv") %>%
  mutate(positivity_ct = num_test/den_test) %>% 
  rename(num_test_ct= num_test,
         den_test_ct= den_test) -> ct_year_age_sex_nat


read.csv("/Users/minttu/restricted_files/gc_year_sex_age.csv") %>%
  mutate(positivity_gc = num_test/den_test) %>%
  rename(num_test_gc= num_test,
         den_test_gc= den_test) -> gc_year_age_sex_nat

ct_year_age_sex_nat %>%
    filter(Gender=="F") %>%
     ggplot()+
     geom_point(aes(x = index_year, y = positivity_ct, color = age_c), shape = 2) +
     geom_line(aes(x = index_year, y = positivity_ct, color = age_c)) + ggtitle("Chlamydia") +
     ylim(c(0, NA)) + theme_bw() + ylab("Positivity per person tested") + 
    labs(color = NULL) + xlab("Year") + ylim(c(0,0.15)) -> p00

ggsave(filename = here("figs/calib-S1-A.png"),
       plot = p00,
       device = png(),
       scale = 1, 
       width = 12,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()


gc_year_age_sex_nat %>%
  filter(Gender=="F") %>%
  ggplot()+
  geom_point(aes(x = index_year, y = positivity_gc, color = age_c), shape = 2) +
  geom_line(aes(x = index_year, y = positivity_gc, color = age_c))  + ggtitle("Gonorrhea") +
  ylim(c(0, NA)) + theme_bw() + ylab("Positivity per person tested") + 
  labs(color = NULL) + xlab("Year") + ylim(c(0,0.15)) -> p01

ggsave(filename = here("figs/calib-S1-B.png"),
       plot = p01,
       device = png(),
       scale = 1, 
       width = 12,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

