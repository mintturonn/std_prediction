
library(geofacet)
source("~/std_prediction/code/figure_specs.R")

timespan <- 3
gen_groups <- 1
state_num <- length(unique(dat_f$state_2))

age_string <- c(rep("15-24",state_num), rep("25-34",state_num), rep("35-44", state_num), rep("45-54",state_num), rep("55+",state_num))

dat_f %>%
  rowwise() %>%
  mutate(pr_ct = num_test_ct/den_test_ct,
         pr_gc = num_test_gc / den_test_gc,
         ll_ct = ifelse( is.na(num_test_ct) | is.na(den_test_ct), NA, binom.test(num_test_ct, den_test_ct)$conf.int[1]),
         ul_ct = ifelse( is.na(num_test_ct) | is.na(den_test_ct), NA, binom.test(num_test_ct, den_test_ct)$conf.int[2]),
         ll_gc = ifelse(is.na(num_test_gc) | is.na(den_test_gc), NA, binom.test(num_test_gc, den_test_gc)$conf.int[1]),
         ul_gc = ifelse( is.na(num_test_gc) | is.na(den_test_gc),  NA, binom.test(num_test_gc, den_test_gc)$conf.int[2])) %>%
  select( state_2, NAME, age_c , den_test_ct, num_test_ct, den_test_gc, num_test_gc,  cases_gc, cases_gc, cases_ct, cases_ct, population_ct, population_gc,
         pr_ct, pr_gc, ll_ct, ll_gc, ul_ct, ul_gc) %>%
  arrange( age_c, state_2) -> fig_dat_f
  

dat_m %>%
  select(state_2, NAME, age_c , den_test_ct, num_test_ct, den_test_gc, num_test_gc,  cases_gc, cases_gc, cases_ct, cases_ct, population_ct, population_gc) %>%
  arrange( age_c, state_2) -> fig_dat_m


ct_data2 <- data_frame(year = nhnsd$year[nhnsd$gender=="female"],
                       age0 = nhnsd$age[nhnsd$gender=="female"],
                       CT = nhnsd$ct[nhnsd$gender=="female"], 
                       CTN =  nhnsd$ct_N[nhnsd$gender=="female"], 
                       CTm = nhnsd$ct[nhnsd$gender=="male"], 
                       CTNm = nhnsd$ct_N[nhnsd$gender=="male"])

ct_data1 <- data_frame(year = nsfgd$year[nsfgd$gender=="female"],
                       age0 = nsfgd$age[nsfgd$gender=="female"],
                       TnD =  nsfgd$j_tnd[nsfgd$gender=="female"], 
                       TnDN =  nsfgd$j_N[nsfgd$gender=="female"], 
                       TnDm =  nsfgd$j_tnd[nsfgd$gender=="male"], 
                       TnDNm =  nsfgd$j_N[nsfgd$gender=="male"], 
                       Te = nsfgd$t[nsfgd$gender=="female"], 
                       TN = nsfgd$t_N[nsfgd$gender=="female"],
                       Tem = nsfgd$t[nsfgd$gender=="male"], 
                       TNm = nsfgd$t_N[nsfgd$gender=="male"],
                       Te_hedis = c(ctgc_data_main$Te_hedis, NA),
                       TN_hedis = c(ctgc_data_main$TN_hedis, NA))

# ct_data2$year <- rep(c(2012,2014,2016),2)
# ct_data2$year_centered <- rep(c(-2,0,2),2)
ct_data2$age <- rep(c("15-24", "25-34"),each = 1)
ct_data1$age <- rep(c("15-24", "25-34"),each = 1)

ct_data2$CTp   <- ct_data2$CT / ct_data2$CTN
ct_data2$CTmp <- ct_data2$CTm / ct_data2$CTNm

ct_data2$CTse <- sqrt(ct_data2$CTp*(1-ct_data2$CTp)/ct_data2$CTN)
ct_data2$CTmse <- sqrt(ct_data2$CTmp*(1-ct_data2$CTmp)/ct_data2$CTNm)
ct_data1$Tp   <- ct_data1$Te / ct_data1$TN
ct_data1$Tp_hedis   <- ct_data1$Te_hedis / ct_data1$TN_hedis
ct_data1$Tmp <- ct_data1$Tem / ct_data1$TNm

ct_data1$Tse <- sqrt(ct_data1$Tp*(1-ct_data1$Tp)/ct_data1$TN)
ct_data1$Tse_hedis <- sqrt(ct_data1$Tp_hedis*(1-ct_data1$Tp_hedis)/ct_data1$TN_hedis)
ct_data1$Tmse <- sqrt(ct_data1$Tmp*(1-ct_data1$Tmp)/ct_data1$TNm)

# correcct in stan so the order is the same
# state_2, age_c, index_year
################################
# Chlamydia prevalence

clb_ct <- cbind( as.data.frame(summary(fit_main, pars = "ct", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023,
                 sex = rep(c("F"), each = 5) )
colnames(clb_ct) <- make.names(colnames(clb_ct)) # to remove % in the col names

ggplot() +
  geom_pointrange( data = clb_ct, aes(x = as.numeric(factor(age)) - 0.1, y = X50., ymin = X2.5., ymax = X97.5., color = "Model"), shape = 21) +
  geom_pointrange( data = ct_data2, aes(x = as.numeric(factor(age)) + 0.1, y = CTp, ymin = CTp - 1.96 * CTse, ymax = CTp + 1.96 * CTse, color = "NHANES")) +
  scale_x_continuous( breaks = seq_along(unique(clb_ct$age)), labels = unique(clb_ct$age)) +
  scale_color_manual( name = "Source", values = c("Model" = "orange", "NHANES" = "maroon")) +
  labs(x = "Age", y = "CT prevalence per capita") +
  theme_light() + mytheme2 + ylim(c(0, 0.06)) -> p0

ggsave(filename = here("figs/calib-ct-prev-1yr-2.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

################################
# Gonorrhea prevalence

clb_gc <- cbind( as.data.frame(summary(fit_main, pars = "gc", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023,
                 sex = rep(c("F"), each = 5) )
colnames(clb_gc) <- make.names(colnames(clb_gc)) # to remove % in the col names


clb_gc %>%
  filter(sex=="F") %>%
  ggplot() +
  geom_pointrange( aes(x = age, y = X50., ymin=  X2.5.,  ymax=X97.5.), color="orange") +
  labs(x = "Age", y = "GC prevalence per capita") + theme_light() + mytheme2  +ylim(c(0, 0.02)) -> p0

ggsave(filename = here("figs/calib-gc-prev-1yr-2.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

clb_te <- cbind( as.data.frame(summary(fit_main, pars = "te", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023 )
# age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, data = fig_dat_f$cases_ct/fig_dat_f$population_ct
colnames(clb_te) <- make.names(colnames(clb_te)) # to remove % in the col names

clb_te %>%
  ggplot() +
  geom_pointrange( aes(x = as.numeric(factor(age)) - 0.01, y = X50., ymin=  X2.5.,  ymax=X97.5., , color="Model")) +
  geom_pointrange(data =ct_data1, aes(x = as.numeric(factor(age)) + 0.1, y = Tp, ymin=Tp-1.96*Tse,  ymax=Tp+1.96*Tse, color="NSFG"), shape=21) +
  geom_pointrange(data =ct_data1, aes(x = as.numeric(factor(age)) + 0.15, y = Tp_hedis, ymin=Tp_hedis-1.96*Tse_hedis,  ymax=Tp_hedis+1.96*Tse_hedis, color="HEDIS"),shape=21) +
  scale_x_continuous( breaks = seq_along(unique(clb_ct$age)), labels = unique(clb_ct$age)) +
  scale_color_manual( name = "Source", values = c("Model" = "orange", "NSFG" = "maroon", "HEDIS"="steelblue3")) +
  labs(x = "Age", y = "CT testing coverage per capita") + theme_light() + mytheme2 +ylim(c(0, 0.6)) -> p1

ggsave(filename = here("figs/calib-test-1yr-2.png"),
       plot = p1,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

# Calibration, diagnoses 
####### CT
## women
clb_diag_ct <- cbind( as.data.frame(summary(fit_main, pars = "d_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                      age = fig_dat_f$age_c, state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                      data = fig_dat_f$cases_ct/fig_dat_f$population_ct, data_ll=NA, data_ul=NA,
                      infection = "chlamydia", type="diagnoses")
colnames(clb_diag_ct) <- make.names(colnames(clb_diag_ct)) # to remove % in the col names

clb_diag_gc <- cbind( as.data.frame(summary(fit_main, pars = "d_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c,  state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                   data = fig_dat_f$cases_gc/fig_dat_f$population_gc,  data_ll=NA, data_ul=NA,
                   infection = "gonorrhea", type="diagnoses")
colnames(clb_diag_gc) <- make.names(colnames(clb_diag_gc)) # to remove % in the col names

## positivity
## women
clb_posit_ct <- cbind( as.data.frame(summary(fit_main, pars = "q_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                       age = fig_dat_f$age_c,  state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                       data = fig_dat_f$pr_ct, data_ll=fig_dat_f$ll_ct, data_ul=fig_dat_f$ul_ct,
                       infection = "chlamydia", type="positivity")
colnames(clb_posit_ct) <- make.names(colnames(clb_posit_ct)) # to remove % in the col names

clb_posit_gc <- cbind( as.data.frame(summary(fit_main, pars = "q_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                       age = fig_dat_f$age_c,  state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                       data = fig_dat_f$pr_gc, data_ll=fig_dat_f$ll_gc, data_ul=fig_dat_f$ul_gc,
                       infection = "gonorrhea", type="positivity")
colnames(clb_posit_gc) <- make.names(colnames(clb_posit_gc)) # to remove % in the col names

## prevalence
## women
clb_pr_ct <- cbind( as.data.frame(summary(fit_main, pars = "p_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),  
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "chlamydia", type="prevalence")
colnames(clb_pr_ct) <- make.names(colnames(clb_pr_ct)) # to remove % in the col names

clb_pr_gc <- cbind( as.data.frame(summary(fit_main, pars = "p_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),   
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "gonorrhea", type="prevalence")
colnames(clb_pr_gc) <- make.names(colnames(clb_pr_gc)) # to remove % in the col names

## incidence
## women
clb_i_ct <- cbind( as.data.frame(summary(fit_main, pars = "i_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),  
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "chlamydia", type="incidence")
colnames(clb_i_ct) <- make.names(colnames(clb_i_ct)) # to remove % in the col names

clb_i_gc <- cbind( as.data.frame(summary(fit_main, pars = "i_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),   
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "gonorrhea", type="incidence")
colnames(clb_i_gc) <- make.names(colnames(clb_i_gc)) # to remove % in the col names

## pr_detected
## women
clb_prdet_ct <- cbind( as.data.frame(summary(fit_main, pars = "pr_det_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),  
                   state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                   data=NA,   data_ll=NA, data_ul=NA,
                   infection = "chlamydia", type="pr detected")
colnames(clb_prdet_ct) <- make.names(colnames(clb_prdet_ct)) # to remove % in the col names

clb_prdet_gc <- cbind( as.data.frame(summary(fit_main, pars = "pr_det_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = rep(c("15-24", "25-34", "35-44", "45-54", ">=55"), each=length(unique(fig_dat_f$NAME))),   
                   state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                   data=NA,   data_ll=NA, data_ul=NA,
                   infection = "gonorrhea", type="pr detected")
colnames(clb_prdet_gc) <- make.names(colnames(clb_prdet_gc)) # to remove % in the col names

## rank by prevalence

clb_pr_ct %>%
  group_by(age) %>%
  arrange(X50.) %>%
  mutate(state_rank = row_number()) %>%
  ungroup() %>%
  select(state2, age, state_rank) -> pr_rank

#############

clb_diag_ct %>% 
  left_join(clb_i_ct, by = c("state", "state2", "age")) %>% 
  mutate(ratio = `X50..y` / `X50..x`) %>%
  mutate(ratio2 = `X50..y` / `data.x`) -> test

quantile(test$ratio)
quantile(test$ratio2, na.rm=T)

clb_diag_gc %>% 
  left_join(clb_i_gc, by = c("state", "state2", "age")) %>% 
  mutate(ratio = `X50..y` / `X50..x`) %>%
  mutate(ratio2 = `X50..y` / `data.x`) -> test2

quantile(test2$ratio)
quantile(test2$ratio2, na.rm=T)


rbind(clb_diag_ct, clb_i_ct, clb_prdet_ct, clb_posit_ct, clb_pr_ct) %>% # 
  left_join(pr_rank, by=c("age", "state2")) %>%
  filter(age =="15-24") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1A.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_ct, clb_i_ct, clb_prdet_ct, clb_posit_ct, clb_pr_ct) %>% # 
  left_join(pr_rank, by=c("age", "state2")) %>%
  filter(age =="25-34") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1B.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_ct, clb_i_ct, clb_prdet_ct, clb_posit_ct, clb_pr_ct) %>% # 
  left_join(pr_rank, by=c("age", "state2")) %>%
  filter(age =="35-44") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1C.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_ct, clb_i_ct, clb_prdet_ct, clb_posit_ct, clb_pr_ct) %>% # 
  left_join(pr_rank, by=c("age", "state2")) %>%
  filter(age =="45-54") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1D.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()



rbind(clb_diag_ct, clb_i_ct, clb_prdet_ct, clb_posit_ct, clb_pr_ct) %>% # 
  left_join(pr_rank, by=c("age", "state2")) %>%
  filter(age ==">=55") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1E.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()


##########################################################################################


clb_pr_gc %>%
  group_by(age) %>%
  arrange(X50.) %>%
  mutate(state_rank = row_number()) %>%
  ungroup() %>%
  select(state2, age, state_rank) -> pr_rank2


rbind(clb_diag_gc, clb_i_gc, clb_prdet_gc, clb_posit_gc, clb_pr_gc) %>% # 
  left_join(pr_rank2, by=c("age", "state2")) %>%
  filter(age =="15-24") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1A.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_gc, clb_i_gc, clb_prdet_gc, clb_posit_gc, clb_pr_gc) %>% # 
  left_join(pr_rank2, by=c("age", "state2")) %>%
  filter(age =="25-34") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1B.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_gc, clb_i_gc, clb_prdet_gc, clb_posit_gc, clb_pr_gc) %>% # 
  left_join(pr_rank2, by=c("age", "state2")) %>%
  filter(age =="35-44") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1C.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_gc, clb_i_gc, clb_prdet_gc, clb_posit_gc, clb_pr_gc) %>% # 
  left_join(pr_rank2, by=c("age", "state2")) %>%
  filter(age =="45-54") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1D.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()



rbind(clb_diag_gc, clb_i_gc, clb_prdet_gc, clb_posit_gc, clb_pr_gc) %>% # 
  left_join(pr_rank2, by=c("age", "state2")) %>%
  filter(age ==">=55") %>%
  filter(state!="MD") %>%
  mutate(type = factor(type, levels = c("diagnoses", "positivity", "incidence", "prevalence", "pr detected"))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", size=0.2) + 
  geom_pointrange(aes(y=reorder(state2, state_rank), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.25) +
  geom_pointrange(aes(y=reorder(state2, state_rank), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type, scales="free") + xlab("Estimate per capita") + mytheme4  + ylab("") -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1E.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

###############################################
# estimation, testing

clb_screen<- cbind( as.data.frame(summary(fit_main, pars = "test_asympt_ctgc", probs = c(0.025, 0.5, 0.975))$summary) , 
                       age = fig_dat_f$age_c,  state = fig_dat_f$state_2, state2 = fig_dat_f$NAME,  
                       data_d = fig_dat_f$cases_ct/fig_dat_f$population_ct, data_p = fig_dat_f$pr_ct, 
                        type="screening rate")
colnames(clb_screen) <- make.names(colnames(clb_screen)) 

age_group <- c("15-24", "25-34", "35-44", "45-54", ">=55")
n_ages <- length(age_group)
base_screen <- rbeta(1e6, 6.398, 8.722)

multipliers <- matrix(rbeta(1e6 * (n_ages-1), 2, 2), ncol = n_ages-1)

priors <- matrix(nrow = 1e6, ncol = n_ages)
priors[, 1] <- base_screen
for(i in 2:n_ages){
  priors[, i] <- priors[, i-1] * multipliers[, i-1]
}

prior_df <- data.frame(
  age = factor(age_group, levels = c("15-24", "25-34", "35-44", "45-54", ">=55")),
  prior2.5 = apply(priors, 2, quantile, probs = 0.025),
  prior97.5 = apply(priors, 2, quantile, probs = 0.975)
)

clb_screen %>%
  filter(state != "MD") %>%
  mutate(age = factor(age, levels = c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  ggplot(aes(y = reorder(state2, X50.), x = X50.)) +
  geom_rect(
    data = prior_df,
    inherit.aes = FALSE,
    aes(xmin = prior2.5, xmax = prior97.5, ymin = -Inf, ymax = Inf, group = age, fill = NULL, color = NULL), 
    fill = "steelblue4", alpha = 0.2) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  geom_pointrange(aes(xmin = X2.5., xmax = X97.5., color = type), size = 0.25) +
  facet_grid(~age) +
  xlab("Annual screening rate") + mytheme4 + ylab("") -> p2

ggsave(filename = here("figs/calib-screen.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()



#############################################

prior_df <- data.frame(
  params =  c("prsympt_ct","prsympt_gc","test_sympt_ct","test_sympt_gc","clear_ct", "clear_gc"),
  estimate = "Prior",
  `X2.5.` = c(quantile(rbeta(10^6, 430, 1260), probs = 0.025),
               quantile(rbeta(10^6, 136, 293), probs = 0.025),
               quantile(rgamma(10^6, 234,32), probs = 0.025),
               quantile(rgamma(10^6, 425,  37), probs = 0.025),
               quantile(rgamma(10^6, 2598.217, 2831.49), probs = 0.025),
               quantile(rgamma(10^6, 266.468,  66.484), probs = 0.025)),
  `X50.` = c(quantile(rbeta(10^6, 430, 1260), probs = 0.5),
              quantile(rbeta(10^6, 136, 293), probs = 0.5),
              quantile(rgamma(10^6, 234,32), probs = 0.5),
              quantile(rgamma(10^6, 425,  37), probs = 0.5),
              quantile(rgamma(10^6, 2598.217, 2831.49), probs = 0.5),
              quantile(rgamma(10^6, 266.468,  66.484), probs = 0.5)),
  `X97.5.` = c(quantile(rbeta(10^6, 430, 1260), probs = 0.975),
                quantile(rbeta(10^6, 136, 293), probs = 0.975),
                quantile(rgamma(10^6, 234,32), probs = 0.975),
                quantile(rgamma(10^6, 425,  37), probs = 0.975),
                quantile(rgamma(10^6, 2598.217, 2831.49), probs = 0.975),
                quantile(rgamma(10^6, 266.468,  66.484), probs = 0.975)) )

post_df <- cbind( as.data.frame(summary(fit_main, 
                                          pars = c("prsympt_ct","prsympt_gc","test_sympt_ct","test_sympt_gc","clear_ct", "clear_gc"), 
                                          probs = c(0.025, 0.5, 0.975))$summary))
colnames(post_df) <- make.names(colnames(post_df)) 

post_df %>%
  mutate(estimate = "Posterior",
         params = rownames(post_df)) %>%
  select(estimate, params, X2.5., X50., X97.5.) %>%
  bind_rows(prior_df) %>%
  mutate(parameters = rep(c("Prob.symptoms,CT", "Prob.symptoms,GC", 
                          "Testing, symptomatic CT", "Testing , symptomatic GC",
                          "Natural clearance , symptomatic CT", "Natural clearance, symptomatic GC"),2)) %>%
  ggplot(aes(x = parameters, y = X50., color = estimate)) +
  geom_pointrange(aes(ymin = X2.5., ymax = X97.5.), 
                  position = position_dodge(width = 0.7),
                  size = 0.6) +
  scale_color_manual(values = c("Prior" = "steelblue4", "Posterior" = "maroon")) +
  labs(x = "",y = "",color = "") + ylim(c(0,NA)) +
  theme_bw(base_size = 10) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~parameters, ncol=2, scales="free") -> p3

ggsave(filename = here("figs/calib-params2.png"),
       plot = p3,
       device = png(),
       scale = 1, 
       width = 15,
       height = 20, 
       units = "cm",
       dpi = 300)

dev.off()

#############################################  


prior_df <- data.frame(
  Age = factor(c("15-24", "25-34", "35-44", "45-54", ">=55"), levels = c("15-24", "25-34", "35-44", "45-54", ">=55")),
  X2.5. = replicate(5, 1+quantile(rgamma(1e6, 2.712, 1.989), c(0.025, 0.5, 0.975)))[1,],
  X50. = replicate(5, 1+quantile(rgamma(1e6, 2.712, 1.989), c(0.025, 0.5, 0.975)))[2,],
  X97.5. = replicate(5, 1+quantile(rgamma(1e6, 2.712, 1.989), c(0.025, 0.5, 0.975)))[3,],
  Estimate = "Prior" )

post2_df <- cbind( as.data.frame(summary(fit_main, 
                                        pars = c("test_inf_RR"), 
                                        probs = c(0.025, 0.5, 0.975))$summary))
colnames(post2_df) <- make.names(colnames(post2_df)) 

post2_df %>%
  mutate(Estimate = "Posterior",
         Age = factor(c("15-24", "25-34", "35-44", "45-54", ">=55"), 
                      levels = c("15-24", "25-34", "35-44", "45-54", ">=55"))) %>%
  select(X2.5., X50., X97.5., Estimate, Age) %>%
  bind_rows(prior_df) %>%
  ggplot(aes(x = Age, y = X50., color = Estimate)) +
  geom_pointrange(aes(ymin = X2.5., ymax = X97.5.), 
                  position = position_dodge(width = 0.4),
                  size = 0.6) +
  scale_color_manual(values = c("Prior" = "steelblue4", "Posterior" = "maroon")) +
  labs(x = "",y = "",color = "") + ylim(c(0,NA)) +
  theme_bw(base_size = 10) + ggtitle("Testing RR in women with asymptomatic infection") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "bottom") -> p4

ggsave(filename = here("figs/calib-params3.png"),
       plot = p4,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

#############################################
# estimation, RR testing in infected
clb_rr_f <- cbind( as.data.frame(summary(fit_main, pars = "RR_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "F")
colnames(clb_rr_f) <- make.names(colnames(clb_rr_f)) # to remove % in the col names

clb_rr_m <- cbind( as.data.frame(summary(fit_main, pars = "RR_m", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "M")
colnames(clb_rr_m) <- make.names(colnames(clb_rr_m)) # to remove % in the col names


rbind(clb_rr_f, clb_rr_m) %>%
  ggplot(aes(y = X50., ymin = X2.5., ymax = X97.5.,x = age, color=age, shape=sex)) +
  geom_pointrange(position = position_dodge(width=0.2), size=0.3, alpha = 4/10 ) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") + mytheme2 + ylim(c(0,NA))


# # estimation, proportion of incident that are tested
# clb_idet <- cbind( as.data.frame(summary(fit_main, pars = "calc_propi_tested", probs = c(0.025, 0.5, 0.975))$summary) , 
#                  age = age_string, state = fig_dat_f$NAME[fig_dat_f$index_year==2019 & fig_dat_f$age_c=="15-24"])
# colnames(clb_idet ) <- make.names(colnames(clb_idet )) # to remove % in the col names
# 
# ggplot(clb_idet , mapping = aes(x = state, color=age)) +
#   geom_pointrange(aes(y = X50., ymin = X2.5., ymax = X97.5.,)) +
#   facet_wrap(~age, ncol = 5) + labs(x = "", y = "proportion of incident infection detected") +
#   theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# estimation, incidence
# clb_inc <- cbind( as.data.frame(summary(fit_main, pars = "i", probs = c(0.025, 0.5, 0.975))$summary) , 
#                   age = age_string, state = fig_dat_f$NAME[fig_dat_f$index_year==2019 & fig_dat_f$age_c=="15-24"])
# colnames(clb_inc) <- make.names(colnames(clb_inc)) # to remove % in the col names
# 
# ggplot(clb_inc, mapping = aes(x = state, color=age)) +
#   geom_pointrange(aes(y = X50., ymin = X2.5., ymax = X97.5.,)) +
#   facet_wrap(~age, ncol = 5) + labs(x = "", y = "incidence per capita") +
#   theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


