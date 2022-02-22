## Wrapper function for conducting analysis in
## with respect to each of the collections of norms we consider

run_test <- function(vaccinees, num_mc_pval = 100, num_nrm_dst = 100 ) {
  ## Get starting time (both for test and overall)
  init_time <- start_time <- Sys.time()
  ## Running the adaptive lp test:
  cat("running lp test ... \n")
  lp_control <- amp::test.control(
    f_cv_summary = mean, more_info = TRUE,
    n_peld_mc_samples = num_nrm_dst, nrm_type = "lp", nrmlize = FALSE,
    perf_meas = "mag", pos_lp_norms = c(1, 2, 4, 6, 8, 10, "max"),
    test_stat_func = "mag", ld_est_meth = "par_boot",
    ts_ld_bs_samp = num_mc_pval
  )

  tst_res_lp <- amp::mv_pn_test(obs_data = vaccinees,
                                param_est = amp::ic.data.examp,
                                control = lp_control)
  cat("current run time : \n")
  lp_time <- Sys.time() - start_time
  print(lp_time)
  start_time <- Sys.time()

  ## Running the non-adaptive l2 test:
  cat("running l2 test ... \n")
  l2_control <- lp_control
  l2_control$pos_lp_norms <- 2
  tst_res_l2 <- amp::mv_pn_test(obs_data = vaccinees,
                                param_est = amp::ic.data.examp,
                                control = l2_control)

  cat("time for l2 test: \n")
  l2_time <- Sys.time() - start_time
  print(l2_time)
  start_time <- Sys.time()

  ## Running the non-adaptive l_infinity test:
  cat("running l_infinity test ... \n")
  linf_control <- lp_control
  linf_control$pos_lp_norms <- "max"
  tst_res_max <- amp::mv_pn_test(obs_data = vaccinees,
                                 param_est = amp::ic.data.examp,
                                 control = linf_control)
  cat("time for max test: \n")
  max_time <- Sys.time() - start_time
  print(max_time)
  start_time <- Sys.time()

  ## Running the adaptive sum of squares test:
  cat("running ssq test ... \n")
  ssq_control <- lp_control
  ssq_control$pos_lp_norms <-
    round(seq(from = 1, to = ncol(vaccinees) - 2, length.out = 6))
  ssq_control$nrm_type <- "ssq"
  tst_res_ssq <- amp::mv_pn_test(obs_data = vaccinees,
                                 param_est = amp::ic.data.examp,
                                 control = ssq_control)
  cat("time for ssq test: \n")
  ssq_time <- Sys.time() - start_time
  print(ssq_time)

  cat("Total time : \n")
  print(Sys.time() - init_time)
  obs_tst_res <- list("lp" = tst_res_lp, "l2" = tst_res_l2,
                      "max" = tst_res_max, "ssq" = tst_res_ssq)
  return(obs_tst_res)
}
