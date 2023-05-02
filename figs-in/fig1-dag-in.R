


dag1 <- dagify(pov2 ~ pov1,
               pov1 ~ repression ,
               labels = c("pov2" = "Poverty+t", 
                          "repression" = "Repression",
                          "pov1" = "Poverty"),
               latent = "pov1",
               exposure = "repression",
               outcome = "pov2") %>%
  ggdag(text = FALSE, use_labels = "label")+ 
  theme_dag_blank()

dag2 <- dagify(pov2 ~ pov1 + culture,
               pov1 ~  culture,
               culture ~ repression, 
               labels = c("pov2" = "Poverty+t", 
                          "repression" = "Repression",
                          "pov1" = "Poverty", 
                          'culture' = 'Culture'
               ),
               latent = "pov1",
               exposure = "repression",
               outcome = "pov2") %>%
  ggdag(text = FALSE, use_labels = "label") + 
  theme_dag_blank()

dag3 <- dagify(pov2 ~ pov1 + culture,
               pov1 ~  culture,
               culture ~ repression, 
               labels = c("pov2" = "Poverty+t", 
                          "repression" = "Repression",
                          "pov1" = "Poverty", 
                          'culture' = 'Institutions'
               ),
               latent = "pov1",
               exposure = "repression",
               outcome = "pov2")  %>% 
  ggdag(text = FALSE, use_labels = "label") + 
  theme_dag_blank()


ggsave('figs-out/dag1.pdf', dag1)
ggsave('figs-out/dag2.pdf', dag2)
ggsave('figs-out/dag3.pdf', dag3)

