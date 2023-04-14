
pca_scores <- waterchem_pca$x

waterchem_pca$rotation %>%
  data.frame() %>%
  dplyr::select(1:2)

biplot(waterchem_pca, arrow.len=0, cex = c(0.5,0.75))

fviz_pca_ind(waterchem_pca, labels = 2)

fviz_pca_biplot(waterchem_pca, labelsize = 3)


waterchem_pca$x %>%
  data.frame() %>%
  dplyr::select(1:2) -> waterchem_pc1_2

watershed_filter_result %>%
  mutate(PC1 = waterchem_pc1_2$PC1, PC2 = waterchem_pc1_2$PC2) -> testvar

newlistvar <- c("STATE", "ELEVATION", "AREA_HA", "PERIM_KM", "FS_EW","NA_L2CODE", "NA_L3CODE","URBAN", "LAKE_ORIGIN", 
    "DOMGEOL_BSN", "HOUSEDEN_BSN", "NLCD2006_ASSIGNEDAREA_BSN", "NLCD2006_DEVELOPEDPCT_BSN", "NLCD2006_FORESTPCT_BSN",
    "NLCD2006_IMPERVPCT_BSN", "NLCD2006_12PCT_BSN", "NLCD2006_AGRICPCT_BSN", "NLCD2006_WETLANDPCT_BSN", "POPDEN_BSN", 
    "PMEAN_BSN", "TMEANPY_BSN")


for (i in newlistvar) {
  waterchem_pca_metadata %>%
  ggplot(aes(x = PC1, y = PC2, color = "STATE")) +
    geom_point() -> plt
print(plt)
}


waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = STATE)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = ELEVATION)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = AREA_HA)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = PERIM_KM)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = FS_EW)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NA_L3CODE)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = URBAN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = LAKE_ORIGIN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = DOMGEOL_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = HOUSEDEN_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_ASSIGNEDAREA_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_DEVELOPEDPCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_FORESTPCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_IMPERVPCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_12PCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_AGRICPCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = NLCD2006_WETLANDPCT_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = POPDEN_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = PMEAN_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)
waterchem_pca_metadata %>% ggplot(aes(x = PC1, y = PC2, color = TMEANPY_BSN)) + geom_point() + ylim(-20,30) + xlim(-150,50)

waterchem_pca_metadata %>%
  dplyr::filter(NA_L3CODE == "6.2.5"| NA_L3CODE == "6.2.7") %>%
  ggplot(aes(x = TMEANPY_BSN, y = PC1)) + geom_point() + geom_smooth(method='lm', se=FALSE)

waterchem_pca_metadata %>% dplyr::filter(NA_L3CODE == "6.2.5"| NA_L3CODE == "6.2.7") %>% ggplot(aes(x = PC1, y = PC2, color = URBAN)) + geom_point() + ylim(-20,30) + xlim(-150,50)

set.seed(710)
phyto_taxa %>%
metaMDS(k=2,trace=T,autotransform =F) -> phyto_nmds

plot(phyto_nmds)

phyto_nmds$points %>%
  as_tibble(rownames="lake") %>%
  dplyr::inner_join(phyto_metadata, by = "lake") 
  
nmds_result %>%
  dplyr::select("NTL_RESULT", "PTL_RESULT", "PH_RESULT", "TURB_RESULT", "CHLORIDE_RESULT", 
                "NITRATE_N_RESULT", "NITRITE_N_RESULT", "SULFATE_RESULT", "CALCIUM_RESULT", 
                "AMMONIA_N_RESULT", "DOC_RESULT", "SODIUM_RESULT", "MAGNESIUM_RESULT", "POTASSIUM_RESULT")

envfit(ord = phyto_nmds, nmds_chem, perm = 999, na.rm = TRUE) %>%
  plot()

summary(waterchem_pca)

plot(testfit)
