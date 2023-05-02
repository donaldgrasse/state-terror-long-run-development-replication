
df <- readRDS('data/main_0310.rds')
df$ihs_light <- asinh(df$ntl_2km)

pov_plot <- noise_test(df, df$pov, df$dist_border, nsims = 1000, nonparametric = TRUE, plot_title = 'Poverty')
light_plot <- noise_test(df, df$ihs_light, df$dist_border, nsims = 1000, nonparametric = TRUE, plot_title = 'Night Lights')

falsification_test_chart <- cowplot::plot_grid(pov_plot, light_plot,
                                               labels = 'AUTO', ncol = 2)
falsification_test_chart

ggsave('figs-out/falsification_test_chart.pdf', width = 15)
