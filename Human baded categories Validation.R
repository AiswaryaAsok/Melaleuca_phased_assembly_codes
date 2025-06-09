# === 1. Load Required Libraries ===
libraries <- c("readxl", "dplyr", "ggplot2", "factoextra", "cluster", "mclust", "officer", "rvg", "flextable")
lapply(libraries, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# === 2. Define BUSCO Reference Means (adjust as needed) ===
busco_altk_mean <- 0.9      # Example value
busco_selfk_mean <- 1.0     # Example value
busco_densk_mean <- 100     # Example value

# === 3. File Paths ===
hap_files <- list(
  Hap1 = "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx",
  Hap2 = "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"
)
datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")

# === 4. Helper Function to Load and Label Data ===
load_and_label_dataset <- function(filepath, sheet, hap) {
  df <- read_excel(filepath, sheet = sheet) %>%
    mutate(
      Dataset = sheet,
      Haplotype = hap,
      DensK_norm = DensK / busco_densk_mean,
      Human_Category = case_when(
        CN >= 1.5 & SelfK < busco_selfk_mean & AltK < busco_altk_mean ~ "Diploid Unique",
        CN >= 1.5 & (SelfK >= busco_selfk_mean | AltK >= busco_altk_mean) ~ "Diploid Repetitive",
        CN < 1.5 & SelfK < busco_selfk_mean & AltK < busco_altk_mean ~ "Haploid Unique",
        CN < 1.5 & (SelfK >= busco_selfk_mean | AltK >= busco_altk_mean) ~ "Haploid Repetitive",
        TRUE ~ "Unclassified"
      )
    )
  return(df)
}

# === 5. Combine All Datasets from Hap1 and Hap2 ===
all_data_list <- list()
for (hap in names(hap_files)) {
  for (ds in datasets) {
    temp <- tryCatch(load_and_label_dataset(hap_files[[hap]], ds, hap), error = function(e) NULL)
    if (!is.null(temp)) all_data_list[[paste(hap, ds, sep = "_")]] <- temp
  }
}
all_data <- bind_rows(all_data_list)

# === 6. Run PCA ===
pca_data <- all_data %>% select(CN, DensK_norm, AltK, SelfK) %>% scale()
pca_result <- prcomp(pca_data)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$Human_Category <- all_data$Human_Category
pca_df$Dataset <- all_data$Dataset

# === 7. Plot PCA ===
p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Human_Category)) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_wrap(~ Dataset) +
  theme_minimal() +
  labs(title = "PCA of Human-Defined Categories", x = "PC1", y = "PC2") +
  theme(plot.title = element_text(hjust = 0.5))

# === 8. Run K-means Clustering and Compare ===
set.seed(123)
kmeans_result <- kmeans(pca_data, centers = 4)
all_data$Kmeans_Cluster <- as.factor(kmeans_result$cluster)

# === 9. Run Adjusted Rand Index and Chi-square ===
ari <- adjustedRandIndex(all_data$Human_Category, all_data$Kmeans_Cluster)
chisq <- chisq.test(table(all_data$Human_Category, all_data$Kmeans_Cluster))

# === 10. Category Distribution Table ===
category_table <- as.data.frame(table(all_data$Dataset, all_data$Human_Category))
colnames(category_table) <- c("Dataset", "Human_Category", "Count")

# === 11. Save to Word Document ===
doc <- read_docx() %>%
  body_add_par("PCA Plot of Human-Defined Categories", style = "heading 1") %>%
  body_add_gg(p_pca, width = 6.5, height = 5.5) %>%
  body_add_par("Adjusted Rand Index", style = "heading 2") %>%
  body_add_par(paste("ARI Score:", round(ari, 4)), style = "Normal") %>%
  body_add_par("Chi-square Test Result", style = "heading 2") %>%
  body_add_par(paste("Chi-square Statistic:", round(chisq$statistic, 2)), style = "Normal") %>%
  body_add_par(paste("p-value:", signif(chisq$p.value, 3)), style = "Normal") %>%
  body_add_par("Human Category Distribution Table", style = "heading 2") %>%
  body_add_flextable(flextable(category_table))

print(doc, target = "C:/Dissertation (R Studio)/Human_Category_Validation.docx")
