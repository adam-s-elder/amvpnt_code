# Read in Data ------------------------------------------------------------
all_data <- readRDS("../data_construction/all_data.RDS")

# Read in testing function ------------------------------------------------
source("helper_funs.R")

## Run the set of tests for each of the
## biomarkers of interest.  This could take
## some time.  It takes about 4 hours on a 2.2 GHz processor.

# The following code should be used if you are using
# multiple runs:
# if (!is.na(syst.envr)) {
#   j_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# }else{
#   warning("No Job ID found, choosing the answer to
#         life the universe and everything \n (1) \n")
#   j_id <- 1
# }
# rep <- j_id

rep <-  1

pval_count <- 30
ts_count <- 50

all_results <- purrr::map(
  all_data, run_test, num_mc_pval = pval_count,
  num_nrm_dst = ts_count
)

## Save results (in case R crashes)
saveRDS(all_results, file = paste0("all_results_pval_draws_",
                                pval_count, "_ts_count_",
                                ts_count, "_rep_", rep, ".rds"))

# save(Fx_ab_res, file = paste0("fxab_results_pval_draws_",
#                                 pval_count, "_ts_count_",
#                                 ts_count, ".rdata"))
