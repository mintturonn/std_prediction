
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
  select(index_year, state_2, NAME, age_c , den_test_ct, num_test_ct, den_test_gc, num_test_gc,  cases_gc, cases_gc, cases_ct, cases_ct, population_ct, population_gc,
         pr_ct, pr_gc, ll_ct, ll_gc, ul_ct, ul_gc) %>%
  arrange(index_year, age_c, state_2) -> fig_dat_f
  

dat_m %>%
  select(index_year, state_2, NAME, age_c , den_test_ct, num_test_ct, den_test_gc, num_test_gc,  cases_gc, cases_gc, cases_ct, cases_ct, population_ct, population_gc) %>%
  arrange(index_year, age_c, state_2) -> fig_dat_m


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
                       TNm = nsfgd$t_N[nsfgd$gender=="male"])

# ct_data2$year <- rep(c(2012,2014,2016),2)
# ct_data2$year_centered <- rep(c(-2,0,2),2)
ct_data2$age <- rep(c("15-24", "25-34"),each = 1)
ct_data1$age <- rep(c("15-24", "25-34"),each = 1)

ct_data2$CTp   <- ct_data2$CT / ct_data2$CTN
ct_data2$CTmp <- ct_data2$CTm / ct_data2$CTNm

ct_data2$CTse <- sqrt(ct_data2$CTp*(1-ct_data2$CTp)/ct_data2$CTN)
ct_data2$CTmse <- sqrt(ct_data2$CTmp*(1-ct_data2$CTmp)/ct_data2$CTNm)
ct_data1$Tp   <- ct_data1$Te / ct_data1$TN
ct_data1$Tmp <- ct_data1$Tem / ct_data1$TNm

ct_data1$Tse <- sqrt(ct_data1$Tp*(1-ct_data1$Tp)/ct_data1$TN)
ct_data1$Tmse <- sqrt(ct_data1$Tmp*(1-ct_data1$Tmp)/ct_data1$TNm)

# correcct in stan so the order is the same
# state_2, age_c, index_year
################################
# Chlamydia prevalence

clb_ct <- cbind( as.data.frame(summary(fit_ct, pars = "ct", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023,
                 sex = rep(c("F", "M"), each = 5) )
colnames(clb_ct) <- make.names(colnames(clb_ct)) # to remove % in the col names

clb_ct %>%
  filter(sex=="F") %>%
ggplot() +
  geom_pointrange( aes(x = age, y = X50., ymin=  X2.5.,  ymax=X97.5.), color="orange") +
  geom_pointrange(data =ct_data2, aes(x = age, y = CTp, ymin=CTp-1.96*CTse,  ymax=CTp+1.96*CTse), color="maroon", 
                  shape=21, alpha=0.2) +
  labs(x = "Age", y = "CT prevalence per capita") + theme_light() + mytheme2  +ylim(c(0, 0.06)) -> p0

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

clb_gc <- cbind( as.data.frame(summary(fit_ct, pars = "gc", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023,
                 sex = rep(c("F", "M"), each = 5) )
colnames(clb_gc) <- make.names(colnames(clb_gc)) # to remove % in the col names


clb_gc %>%
  filter(sex=="F") %>%
  ggplot() +
  geom_pointrange( aes(x = age, y = X50., ymin=  X2.5.,  ymax=X97.5.), color="orange") +
  labs(x = "Age", y = "GC prevalence per capita") + theme_light() + mytheme2  +ylim(c(0, NA)) -> p0

ggsave(filename = here("figs/calib-gc-prev-1yr-2.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

clb_te <- cbind( as.data.frame(summary(fit_ct, pars = "te", probs = c(0.025, 0.5, 0.975))$summary),
                 age = c("15-24", "25-34", "35-44", "45-54", "55+"), year= 2023 )
# age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, data = fig_dat_f$cases_ct/fig_dat_f$population_ct
colnames(clb_te) <- make.names(colnames(clb_te)) # to remove % in the col names

clb_te %>%
  ggplot() +
  geom_pointrange( aes(x = age, y = X50., ymin=  X2.5.,  ymax=X97.5.), color="orange") +
  geom_pointrange(data =ct_data1, aes(x = age, y = Tp, ymin=Tp-1.96*Tse,  ymax=Tp+1.96*Tse), color="maroon", 
                  shape=21, alpha=0.2) +
  labs(x = "Age", y = "CT prevalence per capita") + theme_light() + mytheme2 +ylim(c(0, 0.5)) -> p1

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
clb_diag_ct <- cbind( as.data.frame(summary(fit_ct, pars = "d_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                      age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                      data = fig_dat_f$cases_ct/fig_dat_f$population_ct, data_ll=NA, data_ul=NA,
                      infection = "chlamydia", type="diagnoses")
colnames(clb_diag_ct) <- make.names(colnames(clb_diag_ct)) # to remove % in the col names

clb_diag_gc <- cbind( as.data.frame(summary(fit_ct, pars = "d_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                   data = fig_dat_f$cases_gc/fig_dat_f$population_gc,  data_ll=NA, data_ul=NA,
                   infection = "gonorrhea", type="diagnoses")
colnames(clb_diag_gc) <- make.names(colnames(clb_diag_gc)) # to remove % in the col names

## positivity
## women
clb_posit_ct <- cbind( as.data.frame(summary(fit_ct, pars = "q_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                       age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                       data = fig_dat_f$pr_ct, data_ll=fig_dat_f$ll_ct, data_ul=fig_dat_f$ul_ct,
                       infection = "chlamydia", type="positivity")
colnames(clb_posit_ct) <- make.names(colnames(clb_posit_ct)) # to remove % in the col names

clb_posit_gc <- cbind( as.data.frame(summary(fit_ct, pars = "q_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                       age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, state2 = fig_dat_f$NAME, 
                       data = fig_dat_f$pr_gc, data_ll=fig_dat_f$ll_gc, data_ul=fig_dat_f$ul_gc,
                       infection = "gonorrhea", type="positivity")
colnames(clb_posit_gc) <- make.names(colnames(clb_posit_gc)) # to remove % in the col names

## prevalence
## women
clb_pr_ct <- cbind( as.data.frame(summary(fit_ct, pars = "p_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", "55+"), each=52),  t= fig_dat_f$index_year,
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "chlamydia", type="prevalence")
colnames(clb_pr_ct) <- make.names(colnames(clb_pr_ct)) # to remove % in the col names

clb_pr_gc <- cbind( as.data.frame(summary(fit_ct, pars = "p_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", "55+"), each=52),   t= fig_dat_f$index_year,
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME,  
                    data=NA,   data_ll=NA, data_ul=NA,
                    infection = "gonorrhea", type="prevalence")
colnames(clb_pr_gc) <- make.names(colnames(clb_pr_gc)) # to remove % in the col names


rbind(clb_diag_ct, clb_posit_ct, clb_pr_ct) %>%
  filter(age =="15-24" | age =="25-34") %>%
  filter(state!="HI" & state!="PR" ) %>%
  # filter(state!="ME" & state!="PR" & state!="VT" & state!="NH" & state!="RI" & state!="DE" & state!="CT" &
  #          state!="NH" & state!="HI" & state!="NE" & state!="ID"  & state!="ND" & state!="SD" & state!="WY") %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.4) +
  geom_pointrange(aes(y=reorder(state2, X50.), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type) + xlab("Estimate per capita") + mytheme4  + ylab("")  -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1yr-A.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 25, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_ct, clb_posit_ct, clb_pr_ct) %>%
  filter(age !="15-24" & age !="25-34") %>%
  filter(state!="HI" & state!="PR" & state!="ND" & state!="SD" & state!="ME" & state!="VT" & state!="NH" & state!="CT" ) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.4) +
  geom_pointrange(aes(y=reorder(state2, X50.), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type) + xlab("Estimate per capita") + mytheme4  + ylab("")  -> p2

ggsave(filename = here("figs/calib-st-ct-diagn-1yr-B.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 20, 
       units = "cm",
       dpi = 300)

dev.off()


rbind(clb_diag_gc, clb_posit_gc, clb_pr_gc) %>%
  filter(age =="15-24" | age =="25-34") %>%
  filter(state!="HI" & state!="PR" & state!="ND" & state!="SD" & state!="ME" & state!="VT" & state!="NH" & state!="CT" ) %>%
  # filter(state!="ME" & state!="PR" & state!="VT" & state!="NH" & state!="RI" & state!="DE" & state!="CT" &
  #          state!="NH" & state!="HI" & state!="NE" & state!="ID"  & state!="ND" & state!="SD" & state!="WY") %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.4) +
  geom_pointrange(aes(y=reorder(state2, X50.), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type) + xlab("Estimate per capita") + mytheme4  + ylab("")  -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1yr-2-A.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 30,
       height = 25, 
       units = "cm",
       dpi = 300)

dev.off()

rbind(clb_diag_gc, clb_posit_gc, clb_pr_gc) %>%
  filter(age !="15-24" & age !="25-34") %>%
  filter(state!="HI" & state!="PR" & state!="ND" & state!="SD" & state!="ME" & state!="VT" & state!="NH" & state!="CT" ) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = X50., xmin = X2.5., xmax = X97.5., color=type), 
                  size = 0.4) +
  geom_pointrange(aes(y=reorder(state2, X50.), x = data, xmin=data_ll, xmax=data_ul),
                  color="black", shape=1,  alpha=0.4) +
  facet_grid(~age+type) + xlab("Estimate per capita") + mytheme4  + ylab("")  -> p2

ggsave(filename = here("figs/calib-st-gc-diagn-1yr-2-B.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 25,
       height = 20, 
       units = "cm",
       dpi = 300)

dev.off()



#### RATIO OF PREVALENCE TO CASE RATE
rbind(clb_pr_ct) %>%
  mutate(state_2 = state,
         age_c = ifelse(age=="55+", ">=55", age )) %>%
  left_join(fig_dat_f, by = c("state_2", "age_c" )) %>%
  filter(state!="HI" & state!="PR" & state!="ND" & state!="SD" & state!="ME" & state!="VT" & state!="NH" & state!="CT" ) %>%
 # filter(age =="15-24" | age =="25-34") %>%
  mutate(RR_2.5 = X2.5./(cases_ct/population_ct),
         RR_50 = X50./(cases_ct/population_ct),
         RR_97.5 = X97.5./(cases_ct/population_ct)) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_vline(xintercept = 1, color = "red") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = RR_50, xmin = RR_2.5, xmax = RR_97.5, color=type),  size = 0.2, color="steelblue") +
  facet_grid(~age, scales="free") + xlab("Ratio of prevalence to diagnosis rate") + mytheme4  + ylab("") + xlim(c(0,NA))
  
  

# ggplot(clb_pr_ct) +
#   geom_tile(aes(x = age, y = reorder(state2, X50.), fill = X50.), color = "white") +
#   scale_fill_gradient2(name = "X50. Value",     
#                        low = "blue",     # Color for low end
#                        mid = "orange",    # Color for middle
#                        high = "red",     # Color for high end
#                        midpoint = quantile(clb_pr_ct$X50., probs = 0.75),  # Midpoint value
#                        limits = c(min(clb_pr_ct$X50.), max(clb_pr_ct$X50.)),  # Adjust limits to your data's range
#                        breaks = round(seq(min(clb_pr_ct$X50.), max(clb_pr_ct$X50.), by = 0.001), 3) ) +
#   theme_minimal() +
#   labs(title = "X50. Values by State and Age Group",
#        x = "Age Group",
#        y = "State") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### test coverage

clb_testcov <- cbind( as.data.frame(summary(fit_ct, pars = "test_cov", probs = c(0.025, 0.5, 0.975))$summary) , 
                    age = rep(c("15-24", "25-34", "35-44", "45-54", "55+"), each=52),  t= fig_dat_f$index_year,
                    state = rep(unique(fig_dat_f$state_2),5), state2 = fig_dat_f$NAME)
colnames(clb_testcov) <- make.names(colnames(clb_testcov)) # to remove % in the col names

rbind(clb_testcov) %>%
  filter(state!="HI" & state!="PR" ) %>%
  # filter(state!="ME" & state!="PR" & state!="VT" & state!="NH" & state!="RI" & state!="DE" & state!="CT" &
  #          state!="NH" & state!="HI" & state!="NE" & state!="ID"  & state!="ND" & state!="SD" & state!="WY") %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black") + 
  geom_pointrange(aes(y=reorder(state2, X50.), x = X50., xmin = X2.5., xmax = X97.5.),  size = 0.2, color="darkmagenta") +
  facet_wrap(~age, ncol=5) + xlab("Test coverage") + mytheme4  + ylab("") 





## men
# clb_diag <- cbind( as.data.frame(summary(fit_ct, pars = "d_ct_m", probs = c(0.025, 0.5, 0.975))$summary) , 
#                    age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_m$state_2, data = fig_dat_m$cases_ct/fig_dat_m$population_ct)
# colnames(clb_diag) <- make.names(colnames(clb_diag)) # to remove % in the col names
# 
# ggplot(clb_diag, mapping = aes(x = t)) +
#   geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=age), alpha = 0.35) +
#   geom_line(mapping = aes(x = t, y = X50., color=age)) + 
#   geom_point(mapping = aes(y = data, color=age), shape=21) +
#   facet_geo(~state, grid = "us_state_with_DC_PR_grid1") + ylim(c(0, NA)) +
#   labs(x = "Year", y = "Diagnoses per capita") + theme_light() + mytheme2 




rbind(clb_posit_ct, clb_posit_gc) %>%
  # filter(state!="ME" & state!="PR" & state!="VT" & state!="NH" & state!="RI" & state!="DE" & state!="CT" &
  #          state!="NH" & state!="HI" & state!="NE" & state!="ID"  & state!="ND" & state!="SD" & state!="WY") %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, X50.), x = X50., xmin = X2.5., xmax = X97.5., color=infection, shape=infection),  size = 0.2, alpha = 4/10, shape=1) +
  geom_point(aes(y=reorder(state, X50.), x = data, color=infection, shape=infection),  size = 2, shape=1) +
  facet_wrap(~infection+age, ncol=5) + xlab("Prevalence per capita") + mytheme2  + ylab("")  -> p2 # , scales="free"

ggsave(filename = here("figs/calib-st-ct-posit-1yr-2.png"),
       plot = p2,
       device = png(),
       scale = 1, 
       width = 35,
       height = 35, 
       units = "cm",
       dpi = 100)

dev.off()


clb_pos_ct <- cbind( as.data.frame(summary(fit_ct, pars = "q_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                  age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, data = fig_dat_f$num_test_ct/fig_dat_f$den_test_ct)
colnames(clb_pos_ct) <- make.names(colnames(clb_pos_ct)) # to remove % in the col names

ggplot(clb_pos_ct, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=age), alpha = 0.45) +
  geom_line(mapping = aes(x = t, y = X50., color=age)) + 
  geom_point(mapping = aes(y = data, color=age), shape=21) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") + # , scales="free"
  labs(x = "Year", y = "positivty among tested") + mytheme2 -> p3

ggsave(filename = here("figs/calib-st-ct-posit.png"),
       plot = p3,
       device = png(),
       scale = 1, 
       width = 25,
       height = 20, 
       units = "cm",
       dpi = 100)

dev.off()


####### GC
## women
clb_diag <- cbind( as.data.frame(summary(fit_ct, pars = "d_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, data = fig_dat_f$cases_gc/fig_dat_f$population_gc)
colnames(clb_diag) <- make.names(colnames(clb_diag)) # to remove % in the col names

ggplot(clb_diag, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=age), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=age)) + 
  geom_point(mapping = aes(y = data, color=age), shape=21) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1", scales="free") +
  labs(x = "Year", y = "Diagnoses per capita") + theme_light() + mytheme2 -> p4

ggsave(filename = here("figs/calib-st-gc-diagn.png"),
       plot = p4,
       device = png(),
       scale = 1, 
       width = 35,
       height = 30, 
       units = "cm",
       dpi = 100)

dev.off()


##
## women
clb_pos_gc <- cbind( as.data.frame(summary(fit_ct, pars = "q_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                  age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, data = fig_dat_f$num_test_gc/fig_dat_f$den_test_gc)
colnames(clb_pos_gc) <- make.names(colnames(clb_pos_gc)) # to remove % in the col names

ggplot(clb_pos_gc, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=age), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=age)) + 
  geom_point(mapping = aes(y = data, color=age), shape=21) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") +
  labs(x = "Year", y = "positivty among tested") + mytheme2 -> p5

ggsave(filename = here("figs/calib-st-gc-posit.png"),
       plot = p5,
       device = png(),
       scale = 1, 
       width = 35,
       height = 30, 
       units = "cm",
       dpi = 100)

dev.off()


##########################################################################################

##############################
# estimation, testing
clb_tr_f <- cbind( as.data.frame(summary(fit_ct, pars = "tr_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                 age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "F")
colnames(clb_tr_f) <- make.names(colnames(clb_tr_f)) # to remove % in the col names

clb_tr_m <- cbind( as.data.frame(summary(fit_ct, pars = "tr_m", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "M")
colnames(clb_tr_m) <- make.names(colnames(clb_tr_m)) # to remove % in the col names


rbind(clb_tr_f, clb_tr_m) %>%
  filter(age != ">=55") %>%
ggplot(aes(y = X50., ymin = X2.5., ymax = X97.5.,x = age, color=age, shape=sex)) +
  geom_pointrange(position = position_dodge(width=0.2), size=0.2, alpha = 4/10 ) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") + ylim(c(0,1)) + mytheme2
  



# estimation, RR testing in infected
clb_rr_f <- cbind( as.data.frame(summary(fit_ct, pars = "RR_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "F")
colnames(clb_rr_f) <- make.names(colnames(clb_rr_f)) # to remove % in the col names

clb_rr_m <- cbind( as.data.frame(summary(fit_ct, pars = "RR_m", probs = c(0.025, 0.5, 0.975))$summary) , 
                   age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2, sex = "M")
colnames(clb_rr_m) <- make.names(colnames(clb_rr_m)) # to remove % in the col names


rbind(clb_rr_f, clb_rr_m) %>%
  ggplot(aes(y = X50., ymin = X2.5., ymax = X97.5.,x = age, color=age, shape=sex)) +
  geom_pointrange(position = position_dodge(width=0.2), size=0.3, alpha = 4/10 ) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") + mytheme2 + ylim(c(0,NA))


# # estimation, proportion of incident that are tested
# clb_idet <- cbind( as.data.frame(summary(fit_ct, pars = "calc_propi_tested", probs = c(0.025, 0.5, 0.975))$summary) , 
#                  age = age_string, state = fig_dat_f$NAME[fig_dat_f$index_year==2019 & fig_dat_f$age_c=="15-24"])
# colnames(clb_idet ) <- make.names(colnames(clb_idet )) # to remove % in the col names
# 
# ggplot(clb_idet , mapping = aes(x = state, color=age)) +
#   geom_pointrange(aes(y = X50., ymin = X2.5., ymax = X97.5.,)) +
#   facet_wrap(~age, ncol = 5) + labs(x = "", y = "proportion of incident infection detected") +
#   theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# estimation, incidence
# clb_inc <- cbind( as.data.frame(summary(fit_ct, pars = "i", probs = c(0.025, 0.5, 0.975))$summary) , 
#                   age = age_string, state = fig_dat_f$NAME[fig_dat_f$index_year==2019 & fig_dat_f$age_c=="15-24"])
# colnames(clb_inc) <- make.names(colnames(clb_inc)) # to remove % in the col names
# 
# ggplot(clb_inc, mapping = aes(x = state, color=age)) +
#   geom_pointrange(aes(y = X50., ymin = X2.5., ymax = X97.5.,)) +
#   facet_wrap(~age, ncol = 5) + labs(x = "", y = "incidence per capita") +
#   theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


