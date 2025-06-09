# === 1. Load Required Libraries ===
libraries <- c("readxl", "dplyr", "ggplot2", "factoextra", "cluster", "officer", "rvg")
lapply(libraries, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# === 2. File Paths for Haplotypes ===
hap_files <- list(
  Hap1 = "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx",
  Hap2 = "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"
)

# === 3. Initialize Word document ===
doc <- read_docx()

# === 4. Loop Over Haplotypes for Clustering Analysis ===
for (hap in names(hap_files)) {
  cat("\nProcessing:", hap, "\n")
  
  # Load and scale data
  data <- read_excel(hap_files[[hap]], sheet = "Duplicated")
  metrics <- data %>% select(CN, DensK, AltK, SelfK)
  scaled_data <- scale(metrics)
  
  # Silhouette scores for k = 2 to 6
  sil_scores <- sapply(2:6, function(k) {
    km <- kmeans(scaled_data, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(scaled_data))
    mean(sil[, 3])
  })
  
  # Create Silhouette Plot
  sil_df <- data.frame(k = 2:6, Silhouette = sil_scores)
  p_sil <- ggplot(sil_df, aes(x = k, y = Silhouette)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "darkred", size = 3) +
    labs(title = paste("Silhouette Scores for", hap), x = "Number of Clusters (k)", y = "Avg Silhouette Width") +
    theme_minimal()
  
  # Run K-means for k = 4
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = 4)
  pca_result <- prcomp(scaled_data)
  pca_df <- as.data.frame(pca_result$x[, 1:2])
  pca_df$Cluster <- factor(kmeans_result$cluster)
  
  # PCA Plot
  p_pca <- ggplot(pca_df, aes(PC1, PC2, color = Cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    labs(title = paste("PCA - K-means Clustering (k=4):", hap), x = "PC1", y = "PC2") +
    theme_minimal()
  
  # Format silhouette scores as string for Word doc
  sil_text <- paste(sprintf("k = %d: %.4f", 2:6, sil_scores), collapse = "\n")
  
  # Add to Word document
  doc <- doc %>%
    body_add_par(paste("Clustering Justification -", hap), style = "heading 1") %>%
    body_add_par("Silhouette Scores (k = 2 to 6):", style = "heading 2") %>%
    body_add_par(sil_text, style = "Normal") %>%
    body_add_gg(p_sil, width = 6.5, height = 4.5) %>%
    body_add_par("PCA Plot (k = 4):", style = "heading 2") %>%
    body_add_gg(p_pca, width = 6.5, height = 5)
}

# === 5. Save the Word Document ===
print(doc, target = "C:/Dissertation (R Studio)/KMeans_Clustering_Justification.docx")
