

cycle_impmerge_fun <- function(demonm, pregnm, sexnm, repronm, hivnm, addvar,  abvar, ctnm = NULL, hsvnm = NULL, trichnm=NULL,
                               ct_ab = NULL){
  
  #  "SSCTMBFI", "SSCTEIA", "SSCTEIOD", "SSCTMBA"
  
  
  demo <- nhanes(demonm)
  
  # lab -- all cycles have this
  pregn <- nhanes(pregnm)
  
  # questionnaire -- all cycles have this, variables vary
  sex <- nhanes(sexnm) 
 repro <- nhanes(repronm)
  
  hiv <- nhanes(hivnm)
  
  demo %>%
    full_join(hiv, by = "SEQN") %>%
    full_join(sex, by = "SEQN")  %>%
    full_join(repro, by = "SEQN")  -> dat 

 if  (addvar == TRUE) {
    
    ct <- nhanes(ctnm) 
    hsv <- nhanes(hsvnm)
    trich <- nhanes(trichnm)
    
    dat %>%
      full_join(ct, by = "SEQN") %>%
      full_join(hsv, by = "SEQN") %>%
      full_join(trich, by = "SEQN")  -> dat
 }

  if  (abvar == TRUE) {
    
    ab <- nhanes(ct_ab) 
    
    dat %>%
      full_join(ab, by = "SEQN")  -> dat
  }  
  
  lab <- var_label(dat)
  dat2 <- remove_attributes(dat, "label")
  
  return(list(dat2, lab))
}

comb_figs <- function(fig1, fig2, sname){
  
  legend1 <- get_legend(
    # create some space to the left of the legend
    fig1 +
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  combined_plot <- plot_grid(fig1 + theme(legend.position = "none"), 
                             fig2 + theme(legend.position = "none"),
                             labels = c('', ''), label_size = 12)
  
  
  plot_grid(combined_plot,
            legend1,
            ncol = 1, 
            rel_heights = c(1, .1))
  
  ggsave(here(paste("figs/", sname, ".png", sep="")), width=6,height=3)
  
}

cycle_figure_fun <- function(var1, var2){
  
  # cannot get this to work!
    svyby(~get(var1, nhns_ct), nhns_ct, by = ~ SDDSRVYR, svyciprop, vartype="ci", method="logit") %>%
      ggplot() +
      geom_linerange(aes(x=SDDSRVYR, ymin=ci_l, ymax=ci_u, color=get(var2)), position = position_dodge2(width = 0.5), size=1) +
      geom_point(aes(x=SDDSRVYR, y = ctd, color=get(var2)), position = position_dodge2(width = 0.5)) + 
      theme_minimal() -> fig1
  
  return(fig1)
  
}  