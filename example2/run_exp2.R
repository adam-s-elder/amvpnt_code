## This code provides a skeleton for users to
## re-run simulations.

suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(amp))
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(SuperLearner))
suppressPackageStartupMessages(library(dplyr))

source("exp2_funs.R")

## Multiple simulations can be run by creating
## a data-frame (instead of the list) and looping
## over rows. Each setting can be carried out
## multiple times by adding a "rep" variable.

set <- list(dim = 10, ld_meth = "par_boot",
            mod = 1, ss = 100, nrmtyp = "lp",
            gam_meas = "mag", num_mc_samp = 500,
            cor = 0.5)

if (set$nrmtyp == "lp") {
  norm_type <- "lp"
  norms_chosen <- c(1, 2, 4, 6, "max")
}else if (set$nrmtyp == "l2") {
  norm_type <- "lp"
  norms_chosen <- 2
}else if (set$nrmtyp == "max") {
  norm_type <- "lp"
  norms_chosen <- "max"
}else if (set$nrmtyp == "ssq") {
  norm_type <- "ssq"
  norms_chosen <- unique(round(seq(from = 1, to = set$dim, length.out = 6)))
}

print(data.frame(set))

this_control <- test.control(
  pos_lp_norms = norms_chosen, ld_est_meth = set$ld_meth,
  n_bs_smp = set$num_mc_samp, nrm_type = norm_type,
  ts_ld_bs_samp = set$num_mc_samp, more_info = "all",
  show_hist = FALSE, perf_meas = set$gam_meas
)

gen_data <- make_mod_dat_de2(ss = set$ss, dim = set$dim,
                             model = set$mod, cor = set$cor)

tst_res <- mv_pn_test(obs_data = gen_data, param_est = amp::ic.proj.rr,
                      control = this_control)

tst_res$test_settings <- set
tst_res <- amp::add_oth_pvals(tst_res)

saveRDS(tst_res, paste0(
  paste0(names(set), "of", do.call(c, set), collapse = "__"), ".rds"
))

