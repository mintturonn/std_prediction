
init_fun <- function() { list(
  b_year = c(runif(1,-0.05,0.05), runif(1,-0.05,0.05)),
  b0_age = matrix(runif(4,-5,-2), nrow = 2),
  b_age = c(runif(1,-1,1)),
  b_sex = c(runif(1,-1,1)),
  # incid = matrix(c(runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                  runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),runif(1, 0.01, 0.08),
  #                  runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                  runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                  runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                  runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08)), nrow = 2),
  # incidm = matrix(c(runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                   runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),runif(1, 0.01, 0.08),runif(1, 0.01, 0.08),
  #                   runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                   runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                   runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08),
  #                   runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08), runif(1, 0.01, 0.08)), nrow = 2),
  dursymptf = c(rbeta(1, 10.46, 136.01)),
  dursymptm = c(rbeta(1, 10.44, 65.49)),
  psymptf = c(rbeta(1, 25.35, 73.71)),
  psymptm = c(rbeta(1, 9.50, 48.36)),
  clear = c(rlnorm(1, 0.00504, 0.22206)),
  clearm = c(rlnorm(1, -0.0830, 0.07545)),
  # ct_t = runif(1, 0.1, 0.6),
  # ct_tm = runif(1, 0.1, 0.6),
  # t_ct = runif(1, 0.006, 0.01),
  # t_ctm = runif(1, 0.006, 0.01)
  
  ct_t =  matrix(runif(22*2, 0.5,1), nrow = 2),
  ct_tm = matrix(runif(22*2, 0.5,1), nrow = 2),
  t_ct =  matrix(rbeta(22*2, 1.2,6), nrow = 2),
  t_ctm = matrix(rbeta(22*2, 1.2,6), nrow = 2)
)
}
