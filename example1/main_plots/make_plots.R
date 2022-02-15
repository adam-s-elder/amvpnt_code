## Read data
res_tab <- read.csv("cleaned_res_examp1.csv")
## Plot settings
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

my_cols <- c("#FF5454", "#1E522B", "#60D171",
             "#803223", "#BF6639")
shape_vals <- c(1, 2, 6, 0, 5)

mod.labs <- c("Setting 1", "Setting 2",
              "Setting 3")
names(mod.labs) <- c("1", "2", "3")
norm.labs <- latex2exp::TeX(c(
  "Bonferroni test",
  "Bonferroni corrected linear regression",
  "Zhang and Laber",
  "Cauchy",
  "Adaptive $l_p$ (Parametric Bootstrap)",
  "Adaptive $l_p$ (Permutation)",
  "Adaptive Ssq (Parametric Bootstrap)",
  "Adaptive Ssq (Permutation)"
))

names(norm.labs) <- c("bonf", "simp", "ZL", "cauchy",
                      "pbotlp", "lp", "pbotssq", "ssq")
res_tab$nrm_typ <- factor(res_tab$nrm_typ, levels = names(norm.labs))
res_tab <- res_tab %>%
  filter(!(nrm_typ %in% c("pbotssq", "ssq"))) %>%
  filter(!nrm_typ == "simp")

ss_labs <- as.character(unique(res_tab$samp_size))
ss.labs <- paste0("n = ", ss_labs)
names(ss.labs) <- ss_labs
xcor_lab <- as.character(unique(res_tab$xcor))
xcor.labs <- paste0("xxcor = ", xcor_lab)
names(xcor.labs) <- xcor_lab


make_plot <- function(xxcor, null_mod, incl_bar) {
  if (null_mod) {
    mods <- 1
    line_wd <- 0.0
  }else{
    mods <- setdiff(unique(res_tab$model), 1)
    line_wd <- 0.25
  }
  new_plot_init <- ggplot(res_tab %>%
                            filter(xcor == xxcor, model %in% mods,
                                   samp_size != 500),
                          aes(x = as.factor(dim),
                              y = rej_rate,
                              col = nrm_typ,
                              fill = nrm_typ,
                              shape = nrm_typ)) +
    geom_abline(intercept = c(0, 0.25, 0.5, 0.75, 1),
                slope = 0, col = "gray",
                lty = 1, lwd = line_wd) +
    geom_abline(intercept = 0.05, slope = 0, col = "red", lty = 3)

  if (incl_bar) {
    new_plot_init <- new_plot_init +
      geom_point(position = position_dodge(width = 0.75), size = 0.31) +
      geom_pointrange(aes(ymin = lb, ymax = ub), size = 0.31,
                      position = position_dodge(width = 0.75))
  }else{
    new_plot_init <- new_plot_init +
      geom_point(position = position_dodge(width = 0.75),
                 size = 2, stroke = 0.85)
  }
  new_plot <- new_plot_init +
    geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), size = 0.1) +
    facet_grid(samp_size ~ model,
               labeller = labeller(model = mod.labs,
                                   samp_size = ss.labs,
                                   xcor = xcor.labs)) +
    theme_half_open(12) + panel_border() +
    theme(text = element_text(size = 20)) +
    xlab("Dimension") +  ylab("Rejection Rate") +
    labs(color = "Testing \n Procedure",
         shape = "Testing \n Procedure",
         fill = "Testing \n Procedure") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1)),
          plot.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1.2), hjust = 0.5),
          strip.text = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1.2)),
          axis.line = element_line(size = 0.25, colour = "black"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = 0.25, colour = "gray"),
          legend.position = "bottom") +
    scale_color_manual(values = my_cols,
                       labels = norm.labs) +
    scale_fill_manual(values = my_cols,
                      labels = norm.labs) +
    scale_shape_manual(values = shape_vals,
                       labels = norm.labs)
  if (null_mod) {
    return(new_plot + ylim(c(0, 0.25)) +
             theme(legend.position = "none",
                   strip.text.y = element_blank(),
                   legend.text.align = 0)
    )
  }else{
    return(new_plot + ylim(c(0, 1)) +
             theme(axis.title.y = element_blank(),
                   panel.grid = element_blank(),
                   legend.text.align = 0))
  }
}

fin_plot <- function(btw_x_cor, see_bar = FALSE){
  pnull_n_cor <- make_plot(xxcor = btw_x_cor, null_mod = TRUE, see_bar)
  palt_n_cor <- make_plot(xxcor = btw_x_cor, null_mod = FALSE, see_bar)

  p_ledge <- get_legend(palt_n_cor +
                          guides(color = guide_legend(nrow = 2)) +
                          theme(legend.position = "bottom"))
  no_ledge_plot <- plot_grid(
    pnull_n_cor,
    palt_n_cor + theme(legend.position = "none"),
    labels = c("", ""),
    rel_widths = c(1, 1.85))

  new_plot <- plot_grid(
    no_ledge_plot, p_ledge, ncol = 1, rel_heights = c(1, .1)
  )
  return(new_plot)
}

none_cor_plot <- fin_plot(0, see_bar = FALSE)
some_cor_plot <- fin_plot(0.5)
much_cor_plot <- fin_plot(0.8)

ggsave("none_cor_plot.pdf",
       none_cor_plot,
       width = 10, height = 6)

ggsave("some_cor_plot.pdf",
       some_cor_plot,
       width = 10, height = 6)

ggsave("much_cor_plot.pdf",
       much_cor_plot,
       width = 10, height = 6)