
library(ggpubr)
library(ggrepel) 

# total population 
fig_dat_f %>%
  group_by(NAME, state_2) %>%
  summarise(population = sum(population_ct)) %>%
  arrange(state_2) -> popsize

# testing from the model total in women
clb_num_test <- cbind( as.data.frame(summary(fit_main, pars = "num_test_ct", probs = c(0.025, 0.5, 0.975))$summary) , 
                     state = fig_dat_f$state_2[fig_dat_f$age_c=="15-24"], state2 = fig_dat_f$NAME[fig_dat_f$age_c=="15-24"],  
                    type="test num", population = popsize$population)
colnames(clb_num_test) <- make.names(colnames(clb_num_test)) 

read_excel("/Users/minttu/restricted_files/cg_screening_medicaid.xlsx", sheet = "counts") %>%
  filter(year==2019) %>%
  left_join(clb_num_test, by=c("state")) %>%
  filter(state!="MD") %>%
  ggplot(aes(y=100*X50./population, x = rate)) +
  geom_pointrange(aes(ymin=100*X2.5./population, ymax=100*X97.5./population), size = 0.5) +
  geom_text_repel(aes(label = state), size = 4) +  
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") + ylim(c(0,NA)) + xlim(c(0,NA)) +
  theme_bw()  + ylab("Model test coverage (%)")  + xlab("Medicaid test coverage (%)") -> p0


ggsave(filename = here("figs/valid-medicaid-comparison.png"),
       plot = p0,
       device = png(),
       scale = 1, 
       width = 20,
       height = 15, 
       units = "cm",
       dpi = 300)

dev.off()

