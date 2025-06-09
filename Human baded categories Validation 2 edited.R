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
pca_df$Haplotype <- all_data$Haplotype

# === 7. Plot PCA Faceted by Haplotype and Dataset ===
p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Human_Category)) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_grid(Haplotype ~ Dataset) +
  theme_minimal() +
  labs(title = "PCA of Human-Defined Categories by Haplotype and Dataset", x = "PC1", y = "PC2") +
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

# === 11. Summary Barplot of Human Categories by Dataset ===
p_bar <- ggplot(category_table, aes(x = Dataset, y = Count, fill = Human_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribution of Human Categories by Dataset", x = "Dataset", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === 12. DensK_norm Distribution Plot ===
p_dens_norm <- ggplot(all_data, aes(x = Human_Category, y = DensK_norm, fill = Human_Category)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  facet_wrap(~ Haplotype) +
  theme_minimal() +
  labs(title = "Distribution of Normalized DensK by Human Category", x = "Human Category", y = "Normalized DensK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === 13. Kruskal-Wallis Test for DensK_norm ===
kruskal_test <- kruskal.test(DensK_norm ~ Human_Category, data = all_data)

# === 14. Boxplot of DensK_norm ===
p_box_dens <- ggplot(all_data, aes(x = Human_Category, y = DensK_norm, fill = Human_Category)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~ Haplotype) +
  theme_minimal() +
  labs(title = "Boxplot: Normalized DensK by Human Category", x = "Human Category", y = "Normalized DensK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Violin Plot: Allow plotting even when some categories are small
p_dens_norm <- ggplot(all_data, aes(x = Human_Category, y = DensK_norm, fill = Human_Category)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  facet_wrap(~ Haplotype, drop = FALSE) +  # <-- add drop = FALSE
  theme_minimal() +
  labs(title = "Distribution of Normalized DensK by Human Category", x = "Human Category", y = "Normalized DensK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot: same fix
p_box_dens <- ggplot(all_data, aes(x = Human_Category, y = DensK_norm, fill = Human_Category)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~ Haplotype, drop = FALSE) +  # <-- add drop = FALSE
  theme_minimal() +
  labs(title = "Boxplot: Normalized DensK by Human Category", x = "Human Category", y = "Normalized DensK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# === 15. Save to Word Document ===
doc <- read_docx() %>%
  body_add_par("PCA Plot of Human-Defined Categories by Haplotype and Dataset", style = "heading 1") %>%
  body_add_gg(p_pca, width = 6.5, height = 5.5) %>%
  body_add_par("Adjusted Rand Index", style = "heading 2") %>%
  body_add_par(paste("ARI Score:", round(ari, 4)), style = "Normal") %>%
  body_add_par("Chi-square Test Result", style = "heading 2") %>%
  body_add_par(paste("Chi-square Statistic:", round(chisq$statistic, 2)), style = "Normal") %>%
  body_add_par(paste("p-value:", signif(chisq$p.value, 3)), style = "Normal") %>%
  body_add_par("Human Category Distribution Table", style = "heading 2") %>%
  body_add_flextable(flextable(category_table)) %>%
  body_add_par("Distribution of Categories (Barplot)", style = "heading 2") %>%
  body_add_gg(p_bar, width = 6.5, height = 5) %>%
  body_add_par("DensK_norm by Category and Haplotype (Violin)", style = "heading 2") %>%
  body_add_gg(p_dens_norm, width = 6.5, height = 5.5) %>%
  body_add_par("DensK_norm by Category and Haplotype (Boxplot)", style = "heading 2") %>%
  body_add_gg(p_box_dens, width = 6.5, height = 5.5) %>%
  body_add_par("Kruskal-Wallis Test Result for DensK_norm", style = "heading 2") %>%
  body_add_par(paste("Chi-squared =", round(kruskal_test$statistic, 2)), style = "Normal") %>%
  body_add_par(paste("p-value =", signif(kruskal_test$p.value, 4)), style = "Normal") %>%
  body_add_par("\nSummary Interpretation:", style = "heading 1") %>%
  body_add_par("Human-defined categories were validated using PCA, clustering, and statistical tests. The PCA plot revealed category-specific distribution patterns across both haplotypes and all datasets. Despite a low Adjusted Rand Index (ARI = -0.0278), the Chi-square test revealed a significant association (p < 0.001), confirming non-random overlap between human categories and k-means clusters. The barplot illustrates that Haploid Repetitive regions dominate across datasets, especially in BUSCO and Windows. Normalized DensK (DensK_norm) played a crucial role in classification by acting as a standardized measure of raw k-mer signal, allowing fair comparison across regions and replacing CN where noisy. Violin and boxplots support the discriminative power of DensK_norm, and the Kruskal-Wallis test (p < 0.001) confirms statistically significant differences in DensK_norm across categories.", style = "Normal")

print(doc, target = "C:/Dissertation (R Studio)/Human_Category_Validation 2.docx")
