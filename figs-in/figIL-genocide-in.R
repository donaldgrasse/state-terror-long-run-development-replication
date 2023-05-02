
df <- readRDS("data/main_0310.rds")
genocide_plot <- rdplot_custom_theme(df$genocide2, df$dist_border, h = 10, ylab_rd = 'Genocide Intensity Index')
ggsave('figs-out/genocide_plot.pdf', genocide_plot)
