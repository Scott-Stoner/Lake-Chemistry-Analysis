
phyto_rf[,-c(1)] %>%
  prcomp() -> test_pca

test_pca
summary(test_pca) -> pca_summary

pca_summary
