
library("bayesplot")


fit_post <- as.matrix(fit_ct)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(fit_post,
           pars = c("t_ct[1]", "t_ct[2]", "t_ct[3]", "t_ct[4]" , "t_ct[5]", "t_ct[6]",
                    "t_ctm[1]", "t_ctm[2]", "t_ctm[3]", "t_ctm[4]" , "t_ctm[5]", "t_ctm[6]"),
           prob = 0.95) + plot_title


mcmc_areas(fit_post,
           pars = c("ct_t[1]", "ct_t[2]", "ct_t[3]", "ct_t[4]" , "ct_t[5]", "ct_t[6]",
                    "ct_tm[1]", "ct_tm[2]", "ct_tm[3]", "ct_tm[4]" , "ct_tm[5]", "ct_tm[6]"),
           prob = 0.95) + plot_title

mcmc_areas(fit_post,
           pars = c("t[1]", "t[2]", "t[3]", "t[4]" , "t[5]", "t[6]",
                    "tm[1]", "tm[2]", "tm[3]", "tm[4]" , "tm[5]", "tm[6]"),
           prob = 0.95) + plot_title

mcmc_areas(fit_post,
           pars = c("ct_d[1]", "ct_d[2]", "ct_d[3]", "ct_d[4]" , "ct_d[5]", "ct_d[6]",
                    "ct_dm[1]", "ct_dm[2]", "ct_dm[3]", "ct_dm[4]" , "ct_dm[5]", "ct_dm[6]"),
           prob = 0.95) + plot_title

mcmc_areas(fit_post,
           pars = c("d_ct[1]", "d_ct[2]", "d_ct[3]", "d_ct[4]" , "d_ct[5]", "d_ct[6]",
                    "d_ctm[1]", "d_ctm[2]", "d_ctm[3]", "d_ctm[4]" , "d_ctm[5]", "d_ctm[6]"),
           prob = 0.95) + plot_title


mcmc_areas(fit_post,
           pars = c("ct_and_t[1]", "ct_and_t[2]", "ct_and_t[3]", "ct_and_t[4]" , "ct_and_t[5]", "ct_and_t[6]",
                    "ct_and_tm[1]", "ct_and_tm[2]", "ct_and_tm[3]", "ct_and_tm[4]" , "ct_and_tm[5]", "ct_and_tm[6]"),
           prob = 0.95) + plot_title

mcmc_areas(fit_post,
           pars = c("incid[1]", "incid[2]", "incid[3]", "incid[4]" , "incid[5]", "incid[6]",
                    "incidm[1]", "incidm[2]", "incidm[3]", "incidm[4]" , "incidm[5]", "incidm[6]"),
           prob = 0.95) + plot_title


mcmc_areas(fit_post,
           pars = c("dur[1]", "dur[2]", "dur[3]", "dur[4]" , "dur[5]", "dur[6]",
                    "durm[1]", "durm[2]", "durm[3]", "durm[4]" , "durm[5]", "durm[6]"),
           prob = 0.95) + plot_title

mcmc_areas(fit_post,
           pars = c("dur2[1]", "dur2[2]", "dur2[3]", "dur2[4]" , "dur2[5]", "dur2[6]",
                    "dur2m[1]", "dur2m[2]", "dur2m[3]", "dur2m[4]" , "dur2m[5]", "dur2m[6]"),
           prob = 0.95) + plot_title

