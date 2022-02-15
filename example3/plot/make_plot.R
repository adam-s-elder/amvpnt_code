## The same functions are used to create example 2 and 3 plots
source("../../example2/plot/make_plot.R", chdir = TRUE)
res_tab_examp_3 <- read.csv("cleaned_res_examp3.csv")
plot_0.5 <- fin_plot(0.5, sim_res_tab = res_tab_examp_3, inc_bar = FALSE)

quartz(type = 'pdf', file = 'example_3_plot.pdf',
       width = 10, height = 6)
print(plot_0.5)
dev.off()

ggsave("example_3_plot.pdf",
       plot_0.5,
       width = 10, height = 6)
