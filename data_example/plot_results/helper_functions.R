
make_de_plot <- function(obs_tst_res, plot_title,
                         plot_pos = "mid", num_bins = 30){
  ## Get test results from the first four tests (the Bonferroni
  ## correction based test does not have a single dimensional
  ## limiting distribution to understand the p-value).
  ld_df <- as.data.frame(sapply(obs_tst_res[1:4], "[[", "test_st_eld"))
  ## Create a tidy dataframe in which each row corresponds to a single
  ## draw from a single test type
  long_ld_df <- tidyr::pivot_longer(ld_df, cols = colnames(ld_df),
                                    names_to = "Nrm_type")
  ## Re-level factor so that the tests go non-adaptive to adaptive
  long_ld_df$Nrm_type_f <-
    factor(long_ld_df$Nrm_type, levels = c("l2", "max", "lp", "ssq"))
  ## Calculate the cutoff values based on the the estimated limiting
  ## distribution for each test type.
  ld_cut_vals <- long_ld_df %>% group_by(Nrm_type_f) %>%
    summarise("Cutoff" = quantile(value, 0.05))
  ## Extract the observed test statistics
  test_stats <- as.data.frame(t(mapply(
    obs_tst_res[1:4],
    # Here, we take the median across simulations as the
    # best guess for what the true test statistic is.
    # However, this choice should not change the
    # conclusion of the test since there should be very little
    # variability across simulations for the estiamted test
    # statistic
    FUN = function(x) as.numeric(median(x$test_stat))))
  ) %>% as.data.frame()
  ## Place the observed test statistics in a tidy dataset
  ## (one observation per row)
  long_test_stat <- tidyr::pivot_longer(test_stats[1, ],
                                        values_to = "test_stat",
                                        cols = colnames(test_stats),
                                        names_to = "Nrm_type")
  ## Re-level factor so that the tests go non-adaptive to adaptive
  long_test_stat$Nrm_type_f <-
    factor(long_test_stat$Nrm_type, levels = c("l2", "max", "lp", "ssq"))
  ## Extract p-values
  pvals <- as.data.frame(t(mapply(
    obs_tst_res[1:4], FUN = function(x) as.numeric(x$pvalue)))
  ) %>% as.data.frame()
  ## Make the data tidy
  long_pvalue <- tidyr::pivot_longer(pvals, values_to = "pvalue",
                                     cols = colnames(pvals),
                                     names_to = "Nrm_type")
  ## Re-level factor so that the tests go non-adaptive to adaptive
  long_pvalue$Nrm_type_f <-
    factor(long_pvalue$Nrm_type, levels = c("l2", "max", "lp", "ssq"))
  ## Set parameters so p-values can be displayed in the plot
  long_pvalue$xpos <- Inf
  long_pvalue$ypos <- Inf
  long_pvalue$hadj <- 1.5
  long_pvalue$vadj <- 4
  pvals <- round(long_pvalue$pvalue, 6)
  long_pvalue$pvalue <- ifelse(pvals > 0.001,
                               paste0("p-value = ",
                                      round(pvals, 4)),
                               "p < 0.001")
  ## If the plot is on the bottom (or alone) it will need
  ## a title for the x axis, otherwise it wont.
  if (plot_pos %in% c("alone", "bot")) {
    this_xlab <- "Normalized test statistic"
  }else{
    this_xlab <- ""
  }
  ## If the plot is on the top (or alone) it will need
  ## a title for the facets, otherwise it wont
  if (plot_pos %in% c("alone", "top")) {
    norm_names <- c(
      `l2` = "\u2113[2]",
      `max` = "\u2113[infinity]",
      `lp` = "\u2113[p]~(Adaptive)",
      `ssq` = "Adaptive~Sum~of~Squares"
    )
  }else{
    norm_names <- rep("", 4)
  }
  num_cols <- ifelse(plot_pos == "alone", 2, 4)
  ggplot(data = long_ld_df, aes(x = value)) +
    ## Histogram and density estimate of limiting distribution
    geom_histogram(alpha = 0.4,
                   aes(color = NULL, y = ..density..),
                   position = "identity", bins = num_bins) +
    geom_density(alpha = 0.4, aes(value, fill = NULL)) +
    ## Vertical lines for the cutoff value (black) and
    ## observed test statistic (red).
    geom_vline(data = ld_cut_vals, aes(xintercept = Cutoff)) +
    geom_vline(data = long_test_stat, aes(xintercept = test_stat),
               color = "red", linetype = 2) +
    ## Place the calculated p-value on the plot
    geom_text(data = long_pvalue,
              aes(x = xpos,y = ypos,
                  hjust = hadj, vjust = vadj ,
                  label = pvalue)) +
    facet_wrap( ~ Nrm_type_f,
                ncol = num_cols,
                labeller = labeller(
                  Nrm_type_f = as_labeller(norm_names, label_parsed)
                )) +
    ## Other formatting
    xlab(this_xlab) + ylab("Density") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1)),
          plot.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1.2), hjust = 0.5),
          axis.text = element_text(size = rel(1)),
          strip.text = element_text(size = rel(1.2))) +
    ggtitle(plot_title)
}

