# Load necessary libraries
packages <- c("readxl", "dplyr", "ggplot2", "mclust", "officer", "rvg", "umap", "pheatmap", "cluster", "factoextra", "Rtsne", "dendextend")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# Load your datasets
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
busco_data <- read_excel(hap1_file, sheet = "BUSCO")
all_data <- read_excel(hap1_file, sheet = "Duplicated")

# Step 1: Calculate BUSCO baseline means
busco_mean_DensK <- mean(busco_data$DensK, na.rm = TRUE)
busco_mean_AltK <- mean(busco_data$AltK, na.rm = TRUE)
busco_mean_SelfK <- mean(busco_data$SelfK, na.rm = TRUE)

# Step 2: Normalize your DensK
all_data <- all_data %>%
  mutate(DensK_norm = DensK / busco_mean_DensK)

# Step 3: Assign human-defined categories using thresholds
all_data <- all_data %>%
  mutate(
    Human_Category = case_when(
      CN >= 1.5 & SelfK >= busco_mean_SelfK & AltK < busco_mean_AltK & DensK_norm <= 1.0 ~ "Diploid Unique",
      CN >= 1.5 & SelfK >= busco_mean_SelfK & AltK >= busco_mean_AltK & DensK_norm > 1.0 ~ "Diploid Repetitive",
      CN < 1.5 & SelfK < busco_mean_SelfK & AltK < busco_mean_AltK & DensK_norm <= 1.0 ~ "Haploid Unique",
      CN < 1.5 & SelfK >= busco_mean_SelfK & AltK >= busco_mean_AltK & DensK_norm > 1.0 ~ "Haploid Repetitive",
      TRUE ~ "Unclassified"
    )
  )

# Step 4: Silhouette method to determine optimal k
sil_plot <- fviz_nbclust(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")]), kmeans, method = "silhouette") +
  labs(title = "Silhouette Method - Optimal Number of Clusters")

# Step 5: Perform k-means clustering with k=4
set.seed(123)
k <- 4
kmeans_result <- kmeans(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")]), centers = k)
all_data$Kmeans_Cluster <- factor(kmeans_result$cluster)

# Step 6: ARI and Chi-square tests
ari_value <- adjustedRandIndex(all_data$Human_Category, all_data$Kmeans_Cluster)
chi_test <- chisq.test(table(all_data$Human_Category, all_data$Kmeans_Cluster))
chi_df <- data.frame(Statistic = chi_test$statistic, p_value = chi_test$p.value)

# Step 7: Silhouette plot for individual points
sil <- silhouette(kmeans_result$cluster, dist(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")])))
silhouette_plot <- fviz_silhouette(sil) + ggtitle("Silhouette Plot for K-means Clustering")

# Step 8: PCA plot (cleaned, no text labels)
pca_result <- prcomp(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")]), center = TRUE)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$Human_Category <- all_data$Human_Category
pca_df$Kmeans_Cluster <- all_data$Kmeans_Cluster

plot_pca_human <- ggplot(pca_df, aes(PC1, PC2, color = Human_Category, shape = Human_Category)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("PCA - Human-defined Categories") +
  theme_minimal()

plot_pca_kmeans <- ggplot(pca_df, aes(PC1, PC2, color = Kmeans_Cluster, shape = Kmeans_Cluster)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("PCA - K-means Clusters") +
  theme_minimal()

# Step 9: UMAP (cleaned)
umap_result <- umap(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")]))
umap_df <- data.frame(UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2],
                      Human_Category = all_data$Human_Category,
                      Kmeans_Cluster = all_data$Kmeans_Cluster)

plot_umap_human <- ggplot(umap_df, aes(UMAP1, UMAP2, color = Human_Category, shape = Human_Category)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("UMAP - Human-defined Categories") +
  theme_minimal()

plot_umap_kmeans <- ggplot(umap_df, aes(UMAP1, UMAP2, color = Kmeans_Cluster, shape = Kmeans_Cluster)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("UMAP - K-means Clusters") +
  theme_minimal()

# Step 10: t-SNE
tsne_result <- Rtsne(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")]), dims = 2, perplexity = 30)
tsne_df <- data.frame(D1 = tsne_result$Y[,1], D2 = tsne_result$Y[,2],
                      Human_Category = all_data$Human_Category,
                      Kmeans_Cluster = all_data$Kmeans_Cluster)

tsne_plot_human <- ggplot(tsne_df, aes(D1, D2, color = Human_Category, shape = Human_Category)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("t-SNE - Human-defined Categories") +
  theme_minimal()

tsne_plot_kmeans <- ggplot(tsne_df, aes(D1, D2, color = Kmeans_Cluster, shape = Kmeans_Cluster)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("t-SNE - K-means Clusters") +
  theme_minimal()

# Step 11: Dendrogram
hc <- hclust(dist(scale(all_data[, c("CN", "SelfK", "AltK", "DensK_norm")])), method = "ward.D2")
dend <- color_branches(as.dendrogram(hc), k = 4)

# Step 12: Confusion matrix heatmap
conf_matrix <- table(all_data$Human_Category, all_data$Kmeans_Cluster)

# Step 13: Export to Word
doc <- read_docx() %>%
  body_add_par("K-mer Clustering Analysis Report", style = "heading 1") %>%
  body_add_par("Adjusted Rand Index", style = "heading 2") %>%
  body_add_table(data.frame(ARI = ari_value), style = "table_template") %>%
  body_add_par("Chi-square Test", style = "heading 2") %>%
  body_add_table(chi_df, style = "table_template") %>%
  body_add_par("Silhouette Plot", style = "heading 2") %>%
  body_add_gg(silhouette_plot, width = 6, height = 5) %>%
  body_add_par("PCA - Human-defined Categories", style = "heading 2") %>%
  body_add_gg(plot_pca_human, width = 6, height = 5) %>%
  body_add_par("PCA - K-means Clusters", style = "heading 2") %>%
  body_add_gg(plot_pca_kmeans, width = 6, height = 5) %>%
  body_add_par("UMAP - Human-defined Categories", style = "heading 2") %>%
  body_add_gg(plot_umap_human, width = 6, height = 5) %>%
  body_add_par("UMAP - K-means Clusters", style = "heading 2") %>%
  body_add_gg(plot_umap_kmeans, width = 6, height = 5) %>%
  body_add_par("t-SNE - Human-defined Categories", style = "heading 2") %>%
  body_add_gg(tsne_plot_human, width = 6, height = 5) %>%
  body_add_par("t-SNE - K-means Clusters", style = "heading 2") %>%
  body_add_gg(tsne_plot_kmeans, width = 6, height = 5)

# Save Word document
print(doc, target = "Kmer_Clustering_Analysis_Report.docx")
