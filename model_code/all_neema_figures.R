

# estimation, prevalence

####### CT
clb_pr_f_ct_nat <- cbind( as.data.frame(summary(fit_ct, pars = "ct", probs = c(0.025, 0.5, 0.975))$summary) , 
                      age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2)
colnames(clb_pr_f_ct_nat) <- make.names(colnames(clb_pr_f_ct_nat)) # to remove % in the col names

clb_pr_f_ct <- cbind( as.data.frame(summary(fit_ct, pars = "p_ct_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                      age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2)
colnames(clb_pr_f_ct) <- make.names(colnames(clb_pr_f_ct)) # to remove % in the col names

# clb_pos_ct %>%
#   filter(is.na(data)) -> check
# table(check$state, check$age) -> check1
# names(rowSums(check1)[rowSums(check1) == 20])

clb_pr_f_ct %>%
  filter(!(state %in% c("HI", "PR", "VT"))) %>%
  filter(!(state %in% c("CT", "ME", "RI", "ND") & (age == "35-44" | age == "45-54" | age == ">=55"))) %>%
  filter(!(state %in% c("WY", "SD", "NH", "MT", "ID") & (age == "45-54" | age == ">=55"))) %>%
  filter(!(state %in% c("WV","OK", "NM","NE","MN", "MI", "MA", "KS", "IA", "DE", "AK") & age == ">=55")) -> clb_pr_f_ct1


# clb_pr_m_ct <- cbind( as.data.frame(summary(fit_ct, pars = "p_ct_m", probs = c(0.025, 0.5, 0.975))$summary) , 
#                       age = fig_dat_m$age_c, t= fig_dat_m$index_year, state = fig_dat_m$state_2)
# colnames(clb_pr_m_ct) <- make.names(colnames(clb_pr_m_ct)) 

clb_pr_f_ct %>% filter(t==2023) -> clb_pr_f_ct2
# clb_pr_m_ct %>% filter(t==2022) -> clb_pr_m_ct2

# ggplot() +
#   geom_pointrange(data=clb_pr_f_ct2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="darkorange", size = 0.2, alpha = 4/10, shape=1) +
#   geom_pointrange(data=clb_pr_m_ct2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="blue", size = 0.2, alpha = 3/10, shape=3) +
#   facet_wrap(~age, ncol=5) + xlab("Prevalence per capita") + theme_bw()


####### GC
clb_pr_f_gc <- cbind( as.data.frame(summary(fit_ct, pars = "p_gc_f", probs = c(0.025, 0.5, 0.975))$summary) , 
                      age = fig_dat_f$age_c, t= fig_dat_f$index_year, state = fig_dat_f$state_2)
colnames(clb_pr_f_gc) <- make.names(colnames(clb_pr_f_gc)) # to remove % in the col names

# clb_pos_gc %>%
#   filter(is.na(data)) -> check
# table(check$state, check$age) -> check1
# names(rowSums(check1)[rowSums(check1) == 20])

clb_pr_f_gc %>%
  filter(!(state %in% c("CT", "HI", "ME", "ND", "PR", "VT"))) %>%
  filter(!(state %in% c("SD", "RI", "NH", "ID") & (age == "35-44" | age == "45-54" | age == ">=55"))) %>%
  filter(!(state %in% c("WY", "NE", "MT", "MN", "MA", "AK") & (age == "45-54" | age == ">=55"))) %>%
  filter(!(state %in% c("WI", "WV", "UT", "OK", "OR", "NM", "MI", "DE","KY", "KS", "IA") & age == ">=55")) -> clb_pr_f_gc1

# clb_pr_m_gc <- cbind( as.data.frame(summary(fit_ct, pars = "p_gc_m", probs = c(0.025, 0.5, 0.975))$summary) , 
#                       age = fig_dat_m$age_c, t= fig_dat_m$index_year, state = fig_dat_m$state_2)
# colnames(clb_pr_m_gc) <- make.names(colnames(clb_pr_m_gc)) 

clb_pr_f_gc %>% filter(t==2023) -> clb_pr_f_gc2
# clb_pr_m_gc %>% filter(t==2022) -> clb_pr_m_gc2

# ggplot() +
#   geom_pointrange(data=clb_pr_f_gc2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="darkorange", size = 0.2, alpha = 4/10, shape=1) +
#   geom_pointrange(data=clb_pr_m_gc2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="blue", size = 0.2, alpha = 3/10, shape=3) +
#   facet_wrap(~age, ncol=5) + xlab("Prevalence per capita") + theme_bw()

### CT and GC in women

clb_pr_f_gc2$inf <- "gonorrhea"
clb_pr_f_ct2$inf <- "chlamydia"

ggplot() +
  geom_pointrange(data=clb_pr_f_gc2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="darkorange", size = 0.2, alpha = 4/10, shape=1) +
  geom_pointrange(data=clb_pr_f_ct2, aes(y=state, x = X50., xmin = X2.5., xmax = X97.5.), color="blue", size = 0.2, alpha = 3/10, shape=3) +
  facet_wrap(~age, ncol=5) + xlab("Prevalence per capita") + theme_bw()

#################### ALL NEEMA FIGURE

clb_pr_f_gc1$inf <- "gonorrhea"
clb_pr_f_ct1$inf <- "chlamydia"

rbind(clb_pr_f_gc1, clb_pr_f_ct1) %>%
  ggplot() +
  # geom_pointrange(aes(x=t, y = X50., ymin = X2.5., ymax = X97.5., color=state), 
  #                 position = position_jitter(width = 0.15, height = 0), size = 0.1, alpha = 3/10, shape=1) +
  geom_line(aes(x=t, y = 100*X50., color=state), size=0.2) + #ylim(c(0, 12)) +
  facet_wrap(~inf+age, ncol=5) + xlab("Prevalence per capita") + mytheme2 + theme(legend.position = "none")

#################### ALL NEEMA FIGURE

natest <- data.frame(inf = c("gonorrhea", "chlamydia", "chlamydia","gonorrhea", "chlamydia"),
                     author = c("Kreisel 2021", "Kreisel 2021", "Rönn 2019", "Learner 2020", "Learner 2018"),
                     year = c(2018, 2018, 2015, 2017, 2012),
                     ll =  c(0.342, 4.276, 2.5, 2.41, 11.5),
                     est = c(0.428, 4.709, 2.8, 2.68, 12),
                     ul =  c(0.547,5.156, 3.9, 2.96, 12.4) )


clb_pr_f_gc1$inf <- "gonorrhea"
clb_pr_f_ct1$inf <- "chlamydia"

rbind(clb_pr_f_gc1, clb_pr_f_ct1) %>%
  filter(age == "15-24") %>%
  ggplot() +
  # geom_pointrange(aes(x=t, y = X50., ymin = X2.5., ymax = X97.5., color=state),
  #                 position = position_jitter(width = 0.15, height = 0), size = 0.1, alpha = 3/10, shape=1) +
  geom_line(aes(x=t, y = 100*X50., color=state), size=0.2) + 
  geom_pointrange(data=natest,aes(x=year, y=est, ymin=ll, ymax=ul, color=author), size=0.5, shape=3) +
   mytheme2 + theme(legend.position = "none") + xlim(c(2011, 2022)) +
  facet_wrap(~inf, ncol=1)


#################### ALL NEEMA FIGURE

# rbind(clb_pr_f_ct1,clb_pr_f_gc1) %>%
#   filter(age=="15-24" & t ==2022) %>%
#   ggplot() +
#   geom_pointrange(aes(y=reorder(state, X50.), x = 100*X50., xmin = 100*X2.5., xmax = 100*X97.5., color = X50.), alpha = 4/10) +
#   xlab("Prevalence per capita") + mytheme3 + theme(legend.position = "top") +
#   scale_color_gradient(low = "blue", high = "orange") +
#   geom_text(
#     aes(y = reorder(state, X50.), x = 100*X50., label = round(100*X50., 1)),vjust = -0.4, hjust = 1,size = 3 ) +
#   labs( title = "Prevalence in 15-24 yos", x = "Prevalence per 100 persons", y = "State",color = "Prevalence" ) +
#   theme_minimal() +
#   theme( legend.position = "bottom", axis.text.y = element_text(size = 8),axis.title.y = element_blank()) +
#   facet_wrap(~inf, ncol=2, scales = "free") 

rbind(clb_pr_f_ct1,clb_pr_f_gc1) %>%
  filter(age=="15-24" & t ==2022) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, X50.), x = 100*X50., xmin = 100*X2.5., xmax = 100*X97.5., color = inf), alpha = 4/10) +
  xlab("Prevalence per capita") + mytheme3 + theme(legend.position = "top") +
  #scale_color_gradient(low = "blue", high = "orange") +
  geom_text(
    aes(y = reorder(state, X50.), x = 100*X50., label = round(100*X50., 1)),vjust = 0.3, hjust = -0.7, size = 3 ) +
  labs( title = "15-24 yos, 2022", x = "Prevalence per 100 persons", y = "State",color = "Infection" ) +
  theme_minimal() +
  theme( legend.position = "top", axis.text.y = element_text(size = 8),axis.title.y = element_blank()) #+
  #facet_wrap(~inf, ncol=2, scales = "free") 


# 
# rbind(clb_pr_f_gc1) %>%
#   filter(age=="15-24" & t ==2022) %>%
#   ggplot() +
#   geom_pointrange(aes(y=reorder(state, X50.), x = 100*X50., xmin = 100*X2.5., xmax = 100*X97.5., color = X50.), alpha = 4/10) +
#   xlab("Prevalence per capita") + mytheme3 + theme(legend.position = "top") +
#   scale_color_gradient(low = "blue", high = "orange") +
#   geom_text(
#     aes(y = reorder(state, X50.), x = X50., label = round(100*X50., 1)),vjust = -0.4, hjust = 1,size = 3 ) +
#   labs( title = "Gonorrhea prevalence in 15-24 yos", x = "Prevalence", y = "State",color = "Prevalence" ) +
#   theme_minimal() +
#   theme( legend.position = "none", axis.text.y = element_text(size = 8),axis.title.y = element_blank())
# 

### test age X state

rbind(clb_pr_f_ct1) %>%
  filter(t ==2022) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=X50.), size=0.1, alpha = 4/10) +
  scale_color_gradient(low = "blue", high = "red") +
  labs( title = "Chlamydia prevalence in 15-24 yos", x = "Age", y = "Prevalence",color = "Prevalence" ) +
  theme_minimal() +
  theme( legend.position = "right", axis.text.y = element_text(size = 8),axis.title.y = element_blank()) +
  facet_geo(~state, grid = "us_state_with_DC_PR_grid1") 


########################################
# comparison

fig_dat_f[,c("cases_ct", "population_ct", "index_year", "state_2", "age_c")] %>% 
  rename(age=age_c) %>%
  rename(t=index_year) %>%
  rename(state=state_2) %>%
  left_join(clb_pr_f_ct1, by=c("age", "t", "state")) %>%
  mutate(diagn = cases_ct/population_ct) %>%
  filter(age=="15-24" & t ==2022) %>%
  filter(!is.na(X50.)) %>%
  select(t:diagn)-> tempct


fig_dat_f[,c("cases_gc", "population_gc", "index_year", "state_2", "age_c")] %>% 
  rename(age=age_c) %>%
  rename(t=index_year) %>%
  rename(state=state_2) %>%
  left_join(clb_pr_f_gc1, by=c("age", "t", "state")) %>%
  mutate(diagn = cases_gc/population_gc) %>%
  filter(age=="15-24" & t ==2022) %>%
  filter(!is.na(X50.)) %>%
  select(t:diagn) -> tempgc

rbind(tempct, tempgc) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, X50.), x = 100*X50., xmin = 100*X2.5., xmax = 100*X97.5., color = inf), alpha = 4/10) +
  xlab("Prevalence per 100 persons") + mytheme3 + theme(legend.position = "top") +
  # scale_color_gradient(low = "blue", high = "orange") +
  geom_point(aes(y=state, x=100*diagn, color=inf), shape=4, size=3) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank())



########################################
# absolute difference

rbind(tempct, tempgc) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, -(diagn-X50.)), x = 100*(X50.-diagn), xmin = 100*(X2.5.-diagn), xmax = 100*(X97.5.-diagn), color = inf), alpha = 4/10) +
  xlab("Absolute difference") + mytheme3 + theme(legend.position = "top") +
  geom_vline(xintercept = 0, color = "black", size=0.1) +
  # geom_point(aes(y=state, x=data), color = "red", shape=1) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank())


# relative difference (almost) the same given both prevalence are linked to diagnosis via the same measure!
rbind(tempct, tempgc) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, X50./diagn), x = X50./diagn, xmin = X2.5./diagn, xmax = X97.5./diagn, color = inf), alpha = 4/10) +
  xlab("Absolute difference") + mytheme3 + theme(legend.position = "top") +
  geom_vline(xintercept = 0, color = "black", size=0.1) +
  # geom_point(aes(y=state, x=data), color = "red", shape=1) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank())


fig_dat_f[,c("cases_ct", "population_ct", "index_year", "state_2", "age_c")] %>% 
  rename(age=age_c) %>%
  rename(t=index_year) %>%
  rename(state=state_2) %>%
  left_join(clb_pr_f_ct1, by=c("age", "t", "state")) %>%
  mutate(diagn = cases_ct/population_ct) %>%
  filter(age=="15-24" & t ==2022) %>%
  filter(!is.na(X50.)) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, diagn-X50.), x = diagn-X50., xmin = diagn-X2.5., xmax = diagn-X97.5., color = diagn-X50.), alpha = 4/10) +
  xlab("Absolute difference") + mytheme3 + theme(legend.position = "top") +
  geom_vline(xintercept = 0, color = "black", size=1) +
  scale_color_gradient(high= "blue", low = "orange") +
 # geom_point(aes(y=state, x=data), color = "red", shape=1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank())

fig_dat_f[,c("cases_gc", "population_gc", "index_year", "state_2", "age_c")] %>% 
  rename(age=age_c) %>%
  rename(t=index_year) %>%
  rename(state=state_2) %>%
  left_join(clb_pr_f_gc1, by=c("age", "t", "state")) %>%
  mutate(diagn = cases_gc/population_gc) %>%
  filter(age=="15-24" & t ==2022) %>%
  filter(!is.na(X50.)) %>%
  ggplot() +
  geom_pointrange(aes(y=reorder(state, diagn-X50.), x = diagn-X50., xmin = diagn-X2.5., xmax = diagn-X97.5., color = diagn-X50.), alpha = 4/10) +
  xlab("Prevalence per capita") + mytheme3 + theme(legend.position = "top") +
  geom_vline(xintercept = 0, color = "black", size=1) +
  scale_color_gradient(high= "blue", low = "orange") +
  # geom_point(aes(y=state, x=data), color = "red", shape=1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank())
