## Library the required packages:
library(tidyverse)
library(ggrepel)
library(kyotil)
library(amp)
library(HVTN505)

## Load in the needed functions:
source("helper_functions.R")

# Create plots for each of the biomarker groups
# all_res <- readRDS("combined_results.rds")
# all_res <- readRDS("../data_analysis/all_results.rds")
# just_FxAb <- readRDS("../data_analysis/FxAb_newer.rds")
all_res <- readRDS("../../cluster_helper_functions/data_example/all_results.rds")
just_FxAb <- readRDS("../../cluster_helper_functions/data_example/FxAb_large.rds")
all_res$`Fx Ab` <- just_FxAb$`Fx Ab`

## Construct a table of the p-values.
all_results <- all_res %>% purrr::map(
  .f = function(y) {
    p_est <- y$lp$param_ests
    p_se <- y$lp$param_sds / sqrt(150) # sample size
    pval <- min(pnorm(-1 * abs(p_est / p_se))) * length(p_se)
    y$bonf <- list("pvalue" = pval)
    return(y)
  }
)

pval_tab <- all_results %>%
  purrr::map(
  .f = function(y) {
    purrr::map(.x = y, .f = function(x){x$pvalue}) %>%
      do.call(what = data.frame)
  }
) %>% bind_rows() %>%
  round(4)

pval_tab %>% mutate(
  "Biomarker Group" = names(all_results)
) %>%
  relocate(
    `Biomarker Group`, l2, max, lp, ssq
  ) %>% select(-bonf) %>%
  knitr::kable(format = "latex", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "striped",
                            stripe_color = "#EEEEEE") %>%
  print()

# Create Combined Plot ----------------------------------------------------

igg_igga_plot <- make_de_plot(obs_tst_res = all_res$`IgG + IgA`,
                              plot_title = "IgG + IgA",
                              plot_pos = "top")

igg3_plot <- make_de_plot(
  obs_tst_res = all_res$`IgG3 (Immuno Globulin G3 Group)`,
  plot_title = "IgG3 (Immuno Globulin G3 Group)",
  plot_pos = "top")

Tcells_plot <- make_de_plot(obs_tst_res = all_res$`T Cells`,
             plot_title = "T Cells")

Fx_ab_plot <- make_de_plot(obs_tst_res = all_res$`Fx Ab`,
             plot_title = "Fx Ab")

igg_3_igga_plot <- make_de_plot(obs_tst_res = all_res$`IgG + IgA + IgG3`,
             plot_title = "IgG + IgA + IgG3")

igg_iga_t_plot <- make_de_plot(
  obs_tst_res = all_res$`IgG + IgA + T Cells`,
  plot_title = "IgG + IgA + T Cells")

iga_igg_3_t_plot <- make_de_plot(
  obs_tst_res = all_res$`IgG + IgA + IgG3 + T Cells`,
  plot_title = "IgG + IgA + IgG3 + T Cells")

igg_igga_pc_Fx_plot <-
  make_de_plot(obs_tst_res = all_res$`IgG + IgA + IgG3 + Fx Ab`,
             plot_title = "IgG + IgA + IgG3 + Fx Ab")

t_pc_Fx_plot <- make_de_plot(obs_tst_res = all_res$`T Cells + Fx Ab`,
             plot_title = "T Cells + Fx Ab",
             plot_pos = "bot")

all_plot <- make_de_plot(obs_tst_res = all_res$`All markers`,
                             plot_title = "All markers",
                             plot_pos = "bot")

## Combine all plots into a single long plot
## (for easier comparison) for supplement.
fin_plot <- cowplot::plot_grid(
  igg_igga_plot, igg3_plot, Tcells_plot,
  Fx_ab_plot, igg_3_igga_plot, igg_iga_t_plot, iga_igg_3_t_plot,
  igg_igga_pc_Fx_plot, t_pc_Fx_plot, all_plot, ncol = 2
)

quartz(type = 'pdf', file = 'all_examps.pdf',
       width = 20, height = 15)
print(fin_plot)
dev.off()

## Construct plot for paper section:
DE_plot <- make_de_plot(
  all_res$`Fx Ab`,
  plot_title = "Estimated Limiting Distribution of Normalized Test Statistic",
  plot_pos = "alone", num_bins = 60
)

quartz(type = 'pdf', file = 'data_examp_plot.pdf',
       width = 10, height = 5)
print(DE_plot)
dev.off()


