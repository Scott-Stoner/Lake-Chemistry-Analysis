
phyto_rf[,-c(1)] %>%
  prcomp() -> test_pca

test_pca
summary(test_pca) -> pca_summary
pca_summary
biplot(test_pca)
test_pca$x %>%
  data.frame() %>%
  ggplot(aes(x = PC1, y = PC2, color = phyto_rf$PTL_RESULT)) +
  geom_point()



phyto_filtered %>% group_by(TAXA_ID) %>% count(TAXA_ID) -> test_guy


tb_histogram_panel(watershed_filter_result, PTL_RESULT, .cols)
