

library(lme4)

glmer(CTp ~ age + year_centered + (1 | age) , data = ct_data2, family = binomial)

######### Calbriation fit figure

ct_data2 <- data_frame(year = nhnsd$year[nhnsd$gender=="female"],
                       age0 = nhnsd$age[nhnsd$gender=="female"],
                      CT = nhnsd$ct[nhnsd$gender=="female"], 
                      CTN =  nhnsd$ct_N[nhnsd$gender=="female"], 
                      CTm = nhnsd$ct[nhnsd$gender=="male"], 
                      CTNm = nhnsd$ct_N[nhnsd$gender=="male"],
                       D = nhnsd$d[nhnsd$gender=="female"],
                       DN = nhnsd$d_N[nhnsd$gender=="female"],
                       Dm = nhnsd$d[nhnsd$gender=="male"] ,
                       DNm = nhnsd$d_N[nhnsd$gender=="male"])

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
ct_data2$age <- rep(c("18-24", "25-39"),each = 18)
ct_data1$age <- rep(c("18-24", "25-39"),each = 8)

ct_data2$CTp   <- ct_data2$CT / ct_data2$CTN
ct_data2$CTmp <- ct_data2$CTm / ct_data2$CTNm


ct_data2$Dp    <- ct_data2$D / ct_data2$DN
ct_data2$Dmp <- ct_data2$Dm / ct_data2$DNm

ct_data2$CTse <- sqrt(ct_data2$CTp*(1-ct_data2$CTp)/ct_data2$CTN)
ct_data2$Dse <- sqrt(ct_data2$Dp*(1-ct_data2$Dp)/ct_data2$DN)

ct_data2$CTmse <- sqrt(ct_data2$CTmp*(1-ct_data2$CTmp)/ct_data2$CTNm)
ct_data2$Dmse <- sqrt(ct_data2$Dmp*(1-ct_data2$Dmp)/ct_data2$DNm)

ct_data1$Tp   <- ct_data1$Te / ct_data1$TN
ct_data1$Tmp <- ct_data1$Tem / ct_data1$TNm

ct_data1$Tse <- sqrt(ct_data1$Tp*(1-ct_data1$Tp)/ct_data1$TN)
ct_data1$Tmse <- sqrt(ct_data1$Tmp*(1-ct_data1$Tmp)/ct_data1$TNm)

#ct_data2$CTnDp <- ct_data2$CTnD / ct_data2$CTn
# ct_data2$CTnDse <- sqrt(ct_data2$CTnDp*(1-ct_data2$CTnDp)/ct_data2$CTnDN)
# ct_data2$TnDse <- sqrt(ct_data2$TnDp*(1-ct_data2$TnDp)/ct_data2$TnDN)
# ct_data2$CTnDmse <- sqrt(ct_data2$CTnDmp*(1-ct_data2$CTnDp)/ct_data2$CTnDNm)
# ct_data2$TnDmse <- sqrt(ct_data2$TnDmp*(1-ct_data2$TnDmp)/ct_data2$TnDNm)


ct_data2$ct_var <- factor(c("ct[1,1,1]", "ct[1,1,2]", "ct[1,1,3]", "ct[1,1,4]", "ct[1,1,5]", 
                              "ct[1,1,6]", "ct[1,1,7]", "ct[1,1,8]", "ct[1,1,9]", "ct[1,1,10]",
                              "ct[1,1,11]", "ct[1,1,12]", "ct[1,1,13]", "ct[1,1,14]", "ct[1,1,15]", 
                              "ct[1,1,16]", "ct[1,1,17]", "ct[1,1,18]", 
                              "ct[1,2,1]", "ct[1,2,2]", "ct[1,2,3]", "ct[1,2,4]", "ct[1,2,5]", 
                              "ct[1,2,6]", "ct[1,2,7]", "ct[1,2,8]", "ct[1,2,9]", "ct[1,2,10]",
                              "ct[1,2,11]", "ct[1,2,12]", "ct[1,2,13]", "ct[1,2,14]", "ct[1,2,15]", 
                              "ct[1,2,16]", "ct[1,2,17]", "ct[1,2,18]"))
ct_data2$ct_varm <- factor(c("ct[2,1,1]", "ct[2,1,2]", "ct[2,1,3]", "ct[2,1,4]", "ct[2,1,5]", 
                             "ct[2,1,6]", "ct[2,1,7]", "ct[2,1,8]", "ct[2,1,9]", "ct[2,1,10]",
                             "ct[2,1,11]", "ct[2,1,12]", "ct[2,1,13]", "ct[2,1,14]", "ct[2,1,15]", 
                             "ct[2,1,16]", "ct[2,1,17]", "ct[2,1,18]",
                             "ct[2,2,1]", "ct[2,2,2]", "ct[2,2,3]", "ct[2,2,4]", "ct[2,2,5]", 
                             "ct[2,2,6]", "ct[2,2,7]", "ct[2,2,8]", "ct[2,2,9]", "ct[2,2,10]",
                             "ct[2,2,11]", "ct[2,2,12]", "ct[2,2,13]", "ct[2,2,14]", "ct[2,2,15]", 
                             "ct[2,2,16]", "ct[2,2,17]", "ct[2,2,18]"))

ct_data2$d_var <- factor(c("d[1,1]", "d[1,2]", "d[1,3]", "d[1,4]", "d[1,5]", 
                    "d[1,6]", "d[1,7]", "d[1,8]", "d[1,9]", 
                    "d[2,1]", "d[2,2]", "d[2,3]", "d[2,4]", "d[2,5]", 
                    "d[2,6]", "d[2,7]", "d[2,8]", "d[2,9]"))

ct_data2$d_varm <- factor(c("dm[1,1]", "dm[1,2]", "dm[1,3]", "dm[1,4]", "dm[1,5]", 
                     "dm[1,6]", "dm[1,7]", "dm[1,8]", "dm[1,9]", 
                     "dm[2,1]", "dm[2,2]", "dm[2,3]", "dm[2,4]", "dm[2,5]", 
                     "dm[2,6]", "dm[2,7]", "dm[2,8]", "dm[2,9]"))


ct_data1$t_var <- factor(c("te[1,7]", "te[1,8]", "te[1,9]", "te[1,10]", 
                           "te[2,7]", "te[2,8]", "te[2,9]", "te[2,10]"))

ct_data1$t_varm <- factor(c("tem[1,7]", "tem[1,8]", "tem[1,9]", "tem[1,10]", 
                           "tem[2,7]", "tem[2,8]", "tem[2,9]", "tem[2,10]"))

###############

df_fit %>%
  select(starts_with("ct[1,")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
              ll = quantile(value, probs = 0.025),
              ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("ct[1,1,1]", "ct[1,1,2]", "ct[1,1,3]", "ct[1,1,4]", "ct[1,1,5]", 
                                         "ct[1,1,6]", "ct[1,1,7]", "ct[1,1,8]", "ct[1,1,9]", "ct[1,1,10]",
                                         "ct[1,1,11]", "ct[1,1,12]", "ct[1,1,13]", "ct[1,1,14]", "ct[1,1,15]", 
                                         "ct[1,1,16]", "ct[1,1,17]", "ct[1,1,18]", "ct[1,1,19]", "ct[1,1,20]","ct[1,1,21]",
                                         "ct[1,2,1]", "ct[1,2,2]", "ct[1,2,3]", "ct[1,2,4]", "ct[1,2,5]", 
                                         "ct[1,2,6]", "ct[1,2,7]", "ct[1,2,8]", "ct[1,2,9]", "ct[1,2,10]",
                                         "ct[1,2,11]", "ct[1,2,12]", "ct[1,2,13]", "ct[1,2,14]", "ct[1,2,15]", 
                                         "ct[1,2,16]", "ct[1,2,17]", "ct[1,2,18]", "ct[1,2,19]", "ct[1,2,20]", "ct[1,2,21]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data2, aes(x=ct_var, y=as.numeric(CTp), ymin = as.numeric(CTp)-1.96*as.numeric(CTse), 
                                       ymax = as.numeric(CTp)+1.96*as.numeric(CTse)), color="maroon", alpha =1) +
  geom_line(data = ct_data2, aes(x=ct_var, y=as.numeric(CTp), group = age),  size=0.7, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("prevalence") + xlab("year") +
  ylim(c(0, 0.08))

df_fit %>%
  select(starts_with("ct[2,")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("ct[2,1,1]", "ct[2,1,2]", "ct[2,1,3]", "ct[2,1,4]", "ct[2,1,5]", 
                                          "ct[2,1,6]", "ct[2,1,7]", "ct[2,1,8]", "ct[2,1,9]", "ct[2,1,10]",
                                          "ct[2,2,1]", "ct[2,2,2]", "ct[2,2,3]", "ct[2,2,4]", "ct[2,2,5]", 
                                          "ct[2,2,6]", "ct[2,2,7]", "ct[2,2,8]", "ct[2,2,9]", "ct[2,2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data2, aes(x=ct_varm, y=CTmp, ymin = CTmp-1.96*CTmse, 
                                       ymax = CTmp+1.96*CTmse), color="maroon", alpha =1) +
  geom_line(data = ct_data2, aes(x=ct_varm, y=CTmp, group = age),  size=0.7, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("prevalence") + xlab("year") +
  ylim(c(0, 0.08))


###############

df_fit %>%
  select(starts_with("d[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("d[1,1]", "d[1,2]", "d[1,3]", "d[1,4]", "d[1,5]", 
                                          "d[1,6]", "d[1,7]", "d[1,8]", "d[1,9]", "d[1,10]",
                                          "d[2,1]", "d[2,2]", "d[2,3]", "d[2,4]", "d[2,5]", 
                                          "d[2,6]", "d[2,7]", "d[2,8]", "d[2,9]", "d[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data2, aes(x=d_var, y=Dp, ymin = Dp-1.96*Dse, 
                                       ymax = Dp+1.96*Dse), color="maroon", alpha =1) +
  geom_line(data = ct_data2, aes(x=d_var, y=Dp, group = age),  size=0.7, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("CT diagnosis") + xlab("year") +
  ylim(c(0, 0.1))

df_fit %>%
  select(starts_with("dm[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("dm[1,1]", "dm[1,2]", "dm[1,3]", "dm[1,4]", "dm[1,5]", 
                                          "dm[1,6]", "dm[1,7]", "dm[1,8]", "dm[1,9]", "dm[1,10]", 
                                          "dm[2,1]", "dm[2,2]", "dm[2,3]", "dm[2,4]", "dm[2,5]", 
                                          "dm[2,6]", "dm[2,7]", "dm[2,8]", "dm[2,9]", "dm[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data2, aes(x=d_varm, y=Dmp, ymin = Dmp-1.96*Dmse, ymax = Dmp+1.96*Dmse), color="maroon", alpha =1) +
  geom_line(data = ct_data2, aes(x=d_varm, y=Dmp, group = age),  size=1, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("CT diagnosis") + xlab("year") +
  ylim(c(0, 0.1))

########################################

df_fit %>%
  select(starts_with("te[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels =  c("te[1,1]", "te[1,2]", "te[1,3]", "te[1,4]", "te[1,5]", 
                                           "te[1,6]", "te[1,7]", "te[1,8]", "te[1,9]", "te[1,10]", 
                                           "te[2,1]", "te[2,2]", "te[2,3]", "te[2,4]", "te[2,5]", 
                                           "te[2,6]", "te[2,7]", "te[2,8]", "te[2,9]", "te[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data1, aes(x=t_var, y=Tp, ymin = Tp-1.96*Tse, ymax = Tp+1.96*Tse), color="maroon", alpha =1) +
  geom_line(data = ct_data1, aes(x=t_var, y=Tp, group = age),  size=1, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("CT testing") + xlab("year") +
  ylim(c(0, 0.5))

df_fit %>%
  select(starts_with("tem[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))  %>%
  ungroup %>%
  mutate(group = factor(group, levels =  c("tem[1,1]", "tem[1,2]", "tem[1,3]", "tem[1,4]", "tem[1,5]", 
                                           "tem[1,6]", "tem[1,7]", "tem[1,8]", "tem[1,9]", "tem[1,10]", 
                                           "tem[2,1]", "tem[2,2]", "tem[2,3]", "tem[2,4]", "tem[2,5]", 
                                           "tem[2,6]", "tem[2,7]", "tem[2,8]", "tem[2,9]", "tem[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  geom_pointrange(data = ct_data1, aes(x=t_varm, y=Tmp, ymin = Tmp-1.96*Tmse, ymax = Tmp+1.96*Tmse), color="maroon", alpha =1) +
  geom_line(data = ct_data1, aes(x=t_varm, y=Tmp, group = age),  size=1, color="maroon") +
  theme_minimal() +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("CT testing") + xlab("year") +
  ylim(c(0, 0.5))

########################################


df_fit %>%
  select(starts_with("incid[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))   %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("incid[1,1]", "incid[1,2]", "incid[1,3]", "incid[1,4]", "incid[1,5]", 
                                          "incid[1,6]", "incid[1,7]", "incid[1,8]", "incid[1,9]", "incid[1,10]", 
                                          "incid[2,1]", "incid[2,2]", "incid[2,3]", "incid[2,4]", "incid[2,5]", 
                                          "incid[2,6]", "incid[2,7]", "incid[2,8]", "incid[2,9]", "incid[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("Predicted yearly incidence") + xlab("year") +
  theme_minimal() +
  ylim(c(0, 0.3))

df_fit %>%
  select(starts_with("incidm[")) %>%
  melt() %>%
  group_by(group = variable) %>% 
  summarise(mean = mean(value), 
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975)) %>%
  ungroup %>%
  mutate(group = factor(group, levels = c("incidm[1,1]", "incidm[1,2]", "incidm[1,3]", "incidm[1,4]", "incidm[1,5]", 
                                          "incidm[1,6]", "incidm[1,7]", "incidm[1,8]", "incidm[1,9]", "incidm[1,10]", 
                                          "incidm[2,1]", "incidm[2,2]", "incidm[2,3]", "incidm[2,4]", "incidm[2,5]", 
                                          "incidm[2,6]", "incidm[2,7]", "incidm[2,8]", "incidm[2,9]", "incidm[2,10]"))) %>%
  ggplot() + 
  geom_pointrange(aes(x=group, y=mean, ymin = ll, ymax = ul), shape = "-",  size=5, fatten =3, alpha =0.4) +
  scale_x_discrete(labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018",
                              "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018")) +
  ylab("Predicted yearly incidence") + xlab("year") +
  theme_minimal() +
  ylim(c(0, 0.3))




 
mcmc_areas(df_fit, pars = c("dur[1,1]", "dur[1,2]", "dur[1,3]", "dur[1,4]", "dur[1,5]", 
                            "dur[1,6]", "dur[1,7]", "dur[1,8]", "dur[1,9]", "dur[1,10]",
                            "dur[2,1]", "dur[2,2]", "dur[2,3]", "dur[2,4]", "dur[2,5]", 
                            "dur[2,6]", "dur[2,7]", "dur[2,8]", "dur[2,9]", "dur[2,10]"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("Average duration (Y) for women")

mcmc_areas(df_fit, pars =c("durm[1,1]", "durm[1,2]", "durm[1,3]", "durm[1,4]", "durm[1,5]", 
                           "durm[1,6]", "durm[1,7]", "durm[1,8]", "durm[1,9]", "durm[1,10]",
                           "durm[2,1]", "durm[2,2]", "durm[2,3]", "durm[2,4]", "durm[2,5]", 
                           "durm[2,6]", "durm[2,7]", "durm[2,8]", "durm[2,9]", "durm[2,10]"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("Average duration (Y) for men")


mcmc_areas(df_fit, pars = c("ct_t[1,1]", "ct_t[1,2]", "ct_t[1,3]", "ct_t[1,4]", "ct_t[1,5]",
                            "ct_t[1,6]", "ct_t[1,7]", "ct_t[1,8]", "ct_t[1,9]", "ct_t[1,10]",
                            "ct_t[2,1]", "ct_t[2,2]", "ct_t[2,3]", "ct_t[2,4]", "ct_t[2,5]",
                            "ct_t[2,6]", "ct_t[2,7]", "ct_t[2,8]", "ct_t[2,9]", "ct_t[2,10]"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("current CT given previous test, women")

mcmc_areas(df_fit, pars = c("ct_tm[1,1]", "ct_tm[1,2]", "ct_tm[1,3]", "ct_tm[1,4]", "ct_tm[1,5]",
                            "ct_tm[1,6]", "ct_tm[1,7]", "ct_tm[1,8]", "ct_tm[1,9]", "ct_tm[1,10]",
                            "ct_tm[2,1]", "ct_tm[2,2]", "ct_tm[2,3]", "ct_tm[2,4]", "ct_tm[2,5]",
                            "ct_tm[2,6]", "ct_tm[2,7]", "ct_tm[2,8]", "ct_tm[2,9]", "ct_tm[2,10]"),  
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("current CT given previous test, men")


mcmc_areas(df_fit, pars = c("t_ct[1,1]", "t_ct[1,2]", "t_ct[1,3]", "t_ct[1,4]", "t_ct[1,5]",
                            "t_ct[1,6]", "t_ct[1,7]", "t_ct[1,8]", "t_ct[1,9]", "t_ct[1,10]",
                            "t_ct[2,1]", "t_ct[2,2]", "t_ct[2,3]", "t_ct[2,4]", "t_ct[2,5]",
                            "t_ct[2,6]", "t_ct[2,7]", "t_ct[2,8]", "t_ct[2,9]", "t_ct[2,10]"), 
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("previous test given current CT, women")

mcmc_areas(df_fit, pars = c("t_ctm[1,1]", "t_ctm[1,2]", "t_ctm[1,3]", "t_ctm[1,4]", "t_ctm[1,5]",
                            "t_ctm[1,6]", "t_ctm[1,7]", "t_ctm[1,8]", "t_ctm[1,9]", "t_ctm[1,10]",
                            "t_ctm[2,1]", "t_ctm[2,2]", "t_ctm[2,3]", "t_ctm[2,4]", "t_ctm[2,5]",
                            "t_ctm[2,6]", "t_ctm[2,7]", "t_ctm[2,8]", "t_ctm[2,9]", "t_ctm[2,10]"),
           prob = 0.95, # 80% intervals
           prob_outer = 1, # 99%
           point_est = "median") +
  ggtitle("previous test given current CT, men")







out <- as.data.frame(rbind(fit_post[,"ct[1]"], fit_post[,2,], fit_post[,3,], fit_post[,4,])) 

# c("ct", "t", "ct_and_d", "t_and_d")

p1 <- figs("ct", "CTp", "CTse", 0.05, "Current CT")
p2 <- figs("t", "Tp", "Tse", 0.4, "CT Testing (12m)")
p3 <- figs("ct_and_d", "CTnDp", "CTnDse", 0.005, "Current CT & past CT diag(12m)")
p4 <- figs("t_and_d", "TnDp", "TnDse", 0.05, "Past CT test & CT diagn(12m)")
p5 <- figs("d", "Dp", "Dse", 0.05, "CT diagnosis (12m)")

plot_grid(p1, p2,  p5, p3, p4, labels = c('A)', 'B)', "C)", "D)" , "E)"), label_size = 12, nrow =2)

figs <- function(n1, n2, n3, ymax, name2){
  
  d <- data.frame(matrix(NA, ncol = 4, nrow = 2))
  x <- c("mean", "ll", "ul", "Estimate")
  colnames(d) <- x
  d$mean <- c(mean(out[, n1]), ct_data2[,n2])
  d$ll <- c(quantile(out[, n1], probs = 0.025)[[1]],  ct_data2[,n2] - 1.96*ct_data2[,n3] )
  d$ul <- c(quantile(out[, n1], probs = 0.975)[[1]],  ct_data2[,n2] + 1.96*ct_data2[,n3] )
  d$Estimate <- c("Model", "Data")
  
  print(d)
  
  d %>% 
    ggplot(aes(x=Estimate, y = mean, ymin = ll, ymax = ul, color=Estimate)) +
    geom_pointrange(fatten = 1) +
    ylim(c(0, ymax)) + ylab("") + xlab("") +
    theme_minimal_hgrid() +
    ggtitle(name2) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 8))  -> p
}