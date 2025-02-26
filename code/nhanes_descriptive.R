
library(cowplot)
library(here)
library(nhanesA)
library(survey)
library(tidyverse)
library(labelled)

source(here('code/nhanes_import_funs.R'))

#####################################
# crude logistic
library(lme4)
nhns %>%
 filter(gender == "female" & SDDSRVYR >6) %>%
  mutate(age2 = factor(age)) %>%
  mutate(centered_year = ifelse(SDDSRVYR == 7, -2, ifelse(SDDSRVYR== 8, 0, 2)) ) -> nhnsF

glmer(ct ~ age2 + centered_year + (1 | age2) , data = nhnsF, family = binomial)

#####################################

## NEED TO CONFIRM THIS IS CORRECT
nhns$mec18year[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2] <- 1/4.5 * nhns$WTMEC4YR[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2]
nhns$mec18year[nhns$SDDSRVYR > 2] <- 1/9 * nhns$WTMEC2YR[nhns$SDDSRVYR > 2]

nhns_all <- svydesign(id      = ~SDMVPSU,
                   strata  = ~SDMVSTRA,
                   weights = ~mec18year,
                   nest    = TRUE,
                   data    = nhns)

nhns_ct <- subset(nhns_all, !is.na(anyct))


# GENDER 
svyby(~ct, nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
 # arrange(., gender, SDDSRVYR) %>%
  ggplot() +
    geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=gender), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ct, color=gender), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 10)) +
 # ylab("CT prevalence (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() + theme(legend.position = "none") -> fig1.1

svyby(~ctd, nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., gender, SDDSRVYR) %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=gender), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ctd, color=gender), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 10)) +
 # ylab("CT diagnosis in 12m (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() -> fig1.2

comb_figs(fig1.1, fig1.2, "nhanes_sex")

# AGE
svyby(~ct, nhns_ct, by = ~ age+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., age, SDDSRVYR) %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=age), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ct, color=age), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 10)) +
  ylab("CT prevalence (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() -> fig2.1

svyby(~ctd, nhns_ct, by = ~ age+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., gender, SDDSRVYR) %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=age), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ctd, color=age), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 10)) +
  ylab("CT diagnosis in 12m (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() -> fig2.2

comb_figs(fig2.1, fig2.2, "nhanes_age")


# RACE / ETHNICTY 
svyby(~ct, nhns_ct, by = ~ race+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., age, SDDSRVYR) %>%
  filter(race != "other") %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=race), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ct, color=race), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 15)) +
  ylab("CT prevalence (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() -> fig3.1

svyby(~ctd, nhns_ct, by = ~ race+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., gender, SDDSRVYR) %>%
  filter(race != "other") %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=race), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*ctd, color=race), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 15)) +
  ylab("CT diagnosis in 12m (%)") +
  xlab("survey cycles (99-16)") +
  theme_minimal() -> fig3.2

comb_figs(fig3.1, fig3.2, "nhanes_race")

#####################

## JOINT

# this works, but hard to reframe
# test <- sapply( levels(nhns_ct$variables$anyct),
#                 function(x){ 
#                   form <- as.formula( substitute( ~I(anyct %in% x), list(x=x)))
#                   z <- arrange(svyby(form, nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit"), gender, SDDSRVYR)
#                 }  )
# 
# 
# cbind(test[[1]], test[[2]], test[[3]], test[[4]], test[[5]], test[[6]], test[[7]], test[[8]], test[[9]],
#       test[[10]], test[[11]], test[[12]], test[[13]], test[[14]], test[[15]], test[[16]],test[[17]],test[[18]]) %>%
#   as_tibble() %>%


## GENDER
svyby(~I(anyct %in% "pr&di CT"), nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., gender, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"pr&di CT\")` ) %>%
  mutate(ct_joint = "prev & diagn") -> tab1

svyby(~I(anyct %in% "prev. CT"), nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., gender, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"prev. CT\")` ) %>%
  mutate(ct_joint = "prev, no diagn") -> tab2

svyby(~I(anyct %in% "CT diag."), nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., gender, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"CT diag.\")` ) %>%
  mutate(ct_joint = "diagn, no prev") -> tab3

tab1 %>% 
  bind_rows(., tab2) %>%
  bind_rows(., tab3) -> alltabs

alltabs %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=ct_joint), position = position_dodge2(width = 0.2), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*est, color=ct_joint), position = position_dodge2(width = 0.2)) + 
  facet_wrap(~gender) +
  ylim(c(0, 4.5)) +
  ylab("%") +
  xlab("survey cycles (99-16)") +
  theme_cowplot() + theme(legend.position = "bottom")

ggsave(here(paste("figs/", "nhanes_ct_joint_gender", ".png", sep="")), width=9,height=4)


## AGE
## JOINT

# this works, but hard to reframe
# test <- sapply( levels(nhns_ct$variables$anyct),
#                 function(x){ 
#                   form <- as.formula( substitute( ~I(anyct %in% x), list(x=x)))
#                   z <- arrange(svyby(form, nhns_ct, by = ~ gender+SDDSRVYR, svyciprop, vartype="ci", method="logit"), gender, SDDSRVYR)
#                 }  )
# 
# 
# cbind(test[[1]], test[[2]], test[[3]], test[[4]], test[[5]], test[[6]], test[[7]], test[[8]], test[[9]],
#       test[[10]], test[[11]], test[[12]], test[[13]], test[[14]], test[[15]], test[[16]],test[[17]],test[[18]]) %>%
#   as_tibble() %>%

svyby(~I(anyct %in% "pr&di CT"), nhns_ct, by = ~ age+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., age, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"pr&di CT\")` ) %>%
  mutate(ct_joint = "prev & diagn") -> tab1

svyby(~I(anyct %in% "prev. CT"), nhns_ct, by = ~ age+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., age, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"prev. CT\")` ) %>%
  mutate(ct_joint = "prev, no diagn") -> tab2

svyby(~I(anyct %in% "CT diag."), nhns_ct, by = ~ age+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., age, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"CT diag.\")` ) %>%
  mutate(ct_joint = "diagn, no prev") -> tab3

tab1 %>% 
  bind_rows(., tab2) %>%
  bind_rows(., tab3) -> alltabs

alltabs %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=ct_joint), position = position_dodge2(width = 0.2), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*est, color=ct_joint), position = position_dodge2(width = 0.2)) + 
  facet_wrap(~age) +
  ylim(c(0, 8)) +
  ylab("%") +
  xlab("survey cycles (99-16)") +
  theme_cowplot() + theme(legend.position = "bottom")

ggsave(here(paste("figs/", "nhanes_ct_joint_age", ".png", sep="")), width=9,height=4)

## race 

svyby(~I(anyct %in% "pr&di CT"), nhns_ct, by = ~ race+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., race, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"pr&di CT\")` ) %>%
  mutate(ct_joint = "prev & diagn") -> tab1

svyby(~I(anyct %in% "prev. CT"), nhns_ct, by = ~ race+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., race, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"prev. CT\")` ) %>%
  mutate(ct_joint = "prev, no diagn") -> tab2

svyby(~I(anyct %in% "CT diag."), nhns_ct, by = ~ race+SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
  arrange(., race, SDDSRVYR) %>%
  as_tibble()  %>%
  mutate(est = `I(anyct %in% \"CT diag.\")` ) %>%
  mutate(ct_joint = "diagn, no prev") -> tab3

tab1 %>% 
  bind_rows(., tab2) %>%
  bind_rows(., tab3) -> alltabs

alltabs %>%
  filter(race != "other") %>%
  ggplot() +
  geom_linerange(aes(x=SDDSRVYR, ymin=100*ci_l, ymax=100*ci_u, color=ct_joint), position = position_dodge2(width = 0.2), size=1) +
  geom_point(aes(x=SDDSRVYR, y = 100*est, color=ct_joint), position = position_dodge2(width = 0.2)) + 
  facet_wrap(~race) +
  ylim(c(0, 12)) +
  ylab("%") +
  xlab("survey cycles (99-16)") +
  theme_cowplot() + theme(legend.position = "bottom")

ggsave(here(paste("figs/", "nhanes_ct_joint_race", ".png", sep="")), width=10,height=4)

