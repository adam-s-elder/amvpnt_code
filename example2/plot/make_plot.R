## Read Sims
library(tidyverse)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

res_tab <- read.csv("cleaned_res_examp2.csv")
# These colors are from wesanderson::wes_palette("Darjeeling1", 6, "continuous")
my_cols <- c("#FF0000", "#32806E", "#91A737",
             "#5BBCD6", "#D98F2A", "#F49C00")
shape_vals <- c(0, 1, 2, 6, 3, 4)

mod.labs <- c("Setting 1", "Setting 2",
              "Setting 3", "Setting 4")
names(mod.labs) <- c("1", "2", "3", "4")
ss_labs <- as.character(unique(res_tab$samp_size))
ss.labs <- paste0("n = ", ss_labs)
names(ss.labs) <- ss_labs
xcor_lab <- as.character(unique(res_tab$xcor))
xcor.labs <- paste0("xxcor = ", xcor_lab)
names(xcor.labs) <- xcor_lab
norm.labs <- latex2exp::TeX(c(
  "Bonferroni", "Cauchy", "$L_2$",
  "$L_\\infty$", "Adaptive $L_p$",
  "Adaptive sum of squares"))
names(norm.labs) <- c("bonf", "cauchy", "l2", "max", "lp", "ssq")
res_tab$nrm_typ <- factor(
  res_tab$nrm_typ, levels = names(norm.labs), labels = names(norm.labs))


make_plot <- function(xxcor, null_mod, incl_bar, sim_res_tab) {
  if (null_mod) {
    mods <- 1
  }else{
    mods <- setdiff(unique(sim_res_tab$model), 1)
  }
  new_plot_init <- ggplot(sim_res_tab %>%
                            filter(xcor == xxcor, model %in% mods),
                          aes(x = as.factor(dim),
                              y = rej_rate,
                              col = nrm_typ,
                              fill = nrm_typ,
                              shape = nrm_typ)) +
    geom_abline(intercept = c(0, 0.25, 0.5, 0.75, 1),
                slope = 0, col = "gray",
                lty = 1, size = 0.1)
  if (incl_bar) {
    new_plot_init <- new_plot_init +
      geom_point(position = position_dodge(width = 0.75), size = 0.31) +
      geom_pointrange(aes(ymin = lb, ymax = ub), size = 0.31,
                      position = position_dodge(width = 0.75))
  }else{
    new_plot_init <- new_plot_init +
      geom_point(position = position_dodge(width = 0.75),
                 size = 1.5, stroke = 0.75)
  }
  new_plot <- new_plot_init +
    geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5), col = "gray",
               size = 0.1) +
    facet_grid(samp_size ~ model,
               labeller = labeller(model = mod.labs,
                                   samp_size = ss.labs,
                                   xcor = xcor.labs)) +
    theme_half_open(12) + panel_border() +
    theme(text = element_text(size = 20)) +
    geom_abline(intercept = 0.05, slope = 0, col = "red", lty = 3) +
    xlab("Dimension") +  ylab("Rejection Rate") +
    labs(color = "Test Type") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1)),
          plot.title = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1.2), hjust = 0.5),
          strip.text = element_text(face = "bold", family = "Helvetica",
                                    size = rel(1.2)),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.line = element_line(size = 0.25, colour = "black"),
          panel.grid = element_blank(),
          legend.position = "bottom") +
    labs(color = "Testing Procedure",
         shape = "Testing Procedure",
         fill = "Testing Procedure") +
    scale_shape_manual(values = shape_vals,
                       labels = norm.labs) +
    scale_color_manual(values = my_cols,
                       labels = norm.labs) +
    scale_fill_manual(values = my_cols,
                      labels = norm.labs)
  if (null_mod) {
    return(new_plot + ylim(c(0, 0.25)) +
             theme(legend.position = "none",
                   strip.text.y = element_blank()) +
             geom_abline(intercept = seq(0.05, 0.2, 0.05),
                         slope = 0, col = "gray",
                         lty = 1, size = 0.1)
    )
  }else{
    return(new_plot + ylim(c(0, 1)) +
             theme(axis.title.y = element_blank(),
                   legend.text.align = 0))
  }
}

fin_plot <- function(btw_x_cor, sim_res_tab, inc_bar = FALSE){
  pnull_n_cor <- make_plot(xxcor = btw_x_cor, null_mod = TRUE,
                           inc_bar, sim_res_tab = sim_res_tab)
  palt_n_cor <- make_plot(xxcor = btw_x_cor, null_mod = FALSE,
                          inc_bar, sim_res_tab = sim_res_tab)

  p_ledge <- get_legend(palt_n_cor +
                          guides(color = guide_legend(nrow = 2)) +
                          theme(legend.position = "bottom"))
  no_ledge_plot <- plot_grid(
    pnull_n_cor,
    palt_n_cor + theme(legend.position = "none"),
    labels = c("", ""),
    rel_widths = c(1, 2.625))

  new_plot <- plot_grid(
    no_ledge_plot, p_ledge, ncol = 1, rel_heights = c(1, .07)
  )
  return(new_plot)
}

plot_0.5 <- fin_plot(0.5, sim_res_tab = res_tab, inc_bar = FALSE)

quartz(type = 'pdf', file = 'example_2_plot.pdf',
       width = 10, height = 6)
print(plot_0.5)
dev.off()

ggsave("example_2_plot.pdf",
       plot_0.5,
       width = 10, height = 6)
