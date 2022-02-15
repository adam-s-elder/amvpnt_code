## Testing what happens for p-values in example 1
library(tidyverse)
## Simulating the data
# get_one <- function(ss){
#   draw <- amp::ic.pearson(amp::make_data(ss, 1, 0, 1))
#   est <- draw$est
#   se <- (mean((draw$ic[,1]) ** 2) / sqrt(ss))
#   return(c(est = est, se = se))
# }
#
# many_draws_100 <- (replicate(get_one(100), n = 1000000))
# many_draws_200 <- (replicate(get_one(200), n = 1000000))
# all_dat <- data.frame(
#   est = c(many_draws_100[1, ], many_draws_200[1, ]),
#   "Sample Size" = paste0("n = ", rep(c(100, 200),
#                                      each = ncol(many_draws_100))),
#   se = c(many_draws_100[2, ], many_draws_200[2, ])) %>%
#   mutate(pval = 2 * (1 - pnorm(abs(est / se))))
# hist(many_draws_100[1, ])
# hist(many_draws_200[1, ])
# sd(many_draws_100[1, ])
# sd(many_draws_200[1, ])
# saveRDS(all_dat, "pval_dat_cmpr_se.rds")

all_dat <- readRDS("pval_dat_cmpr_se.rds")
pval_df <- expand.grid(dim = c(10, 50, 100),
                       Sample.Size = unique(all_dat$Sample.Size)) %>%
  dplyr::mutate("pco" = 0.05 / `dim`) %>%
  mutate(dim = factor(
    paste0("dim = ", dim), levels = paste0("dim = ", c(10, 50, 100))))

# Calculate the true standard errors
tr_var <- all_dat %>% group_by(Sample.Size) %>%
  summarise(tr_se = sqrt(var(est)))
all_dat <- all_dat %>% left_join(tr_var)

fin_dat <- all_dat %>% mutate(
  "Estimated SE" =  2 * (1 - pnorm(abs(est / se))),
  "True SE" =  2 * (1 - pnorm(abs(est / tr_se)))
) %>% select(Sample.Size, `Estimated SE`, `True SE`) %>%
  pivot_longer(- Sample.Size, names_to = "se_type",
               values_to = "p-value")

txt_size <- 18
zmd_out_cmpr <- fin_dat %>% ggplot(aes(x = `p-value`)) +
  geom_histogram(bins = 200, aes(y = ..density..)) +
  geom_hline(yintercept = 1, color = "blue") +
  facet_grid(se_type ~ Sample.Size) +
  cowplot::theme_minimal_grid(12) + xlab("p-value") +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = txt_size),
        axis.title = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1)),
        plot.title = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1.2), hjust = 0.5),
        strip.text = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1.2)),
        axis.line = element_line(size = 0.25, colour = "black"),
        panel.grid.major.y = element_line(size = 0.25, colour = "gray"),
  )

zmd_in_cmpr <- fin_dat %>% ggplot(aes(x = `p-value`)) +
  geom_histogram(bins = 5000, aes(y = ..density..)) +
  geom_vline(data = pval_df, aes(xintercept = pco), color = "red") +
  coord_cartesian(xlim = c(0, 0.01)) +
  facet_grid(se_type ~ Sample.Size, scales = "free_y") +
  geom_hline(yintercept = 1, color = "blue") +
  cowplot::theme_minimal_grid(16) + xlab("p-value") +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = txt_size),
        axis.title = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1)),
        plot.title = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1.2), hjust = 0.5),
        strip.text = element_text(face = "bold", family = "Helvetica",
                                  size = rel(1.2)),
        axis.line = element_line(size = 0.25, colour = "black"),
        panel.grid.major.y = element_line(size = 0.25, colour = "gray"),)

title <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Sampling Densitiy of p-values Under the Null",
    fontface = 'bold',
    size = 24, x = 0, hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cmbd_plots <-
  cowplot::plot_grid(zmd_out_cmpr, zmd_in_cmpr, nrow = 2,
                     labels = c("(A)", "(B)"), label_size = 22,
                     rel_heights = c(1, 1))

ggsave("pval_issues.pdf",
       cmbd_plots, width = 16, height = 16)
