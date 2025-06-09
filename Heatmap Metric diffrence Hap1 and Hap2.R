# === Install required packages if not already installed ===
packages <- c("ggplot2", "dplyr", "readxl", "reshape2", "pheatmap")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# === File paths ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"

datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")
metrics <- c("MeanX", "CN", "DensX", "DensK", "AltK", "SelfK")

# === Initialize results table ===
heatmap_data <- data.frame(row.names = metrics)

# === Loop through datasets and calculate differences ===
for (ds in datasets) {
  # Load Hap1 and Hap2
  hap1 <- tryCatch({
    read_excel(hap1_file, sheet = ds) %>% select(all_of(metrics))
  }, error = function(e) NULL)
  
  hap2 <- tryCatch({
    read_excel(hap2_file, sheet = ds) %>% select(all_of(metrics))
  }, error = function(e) NULL)
  
  # Skip if either is missing
  if (is.null(hap1) || is.null(hap2)) next
  
  # Calculate mean of each metric for Hap1 and Hap2
  means_hap1 <- colMeans(hap1, na.rm = TRUE)
  means_hap2 <- colMeans(hap2, na.rm = TRUE)
  
  # Difference (Hap1 - Hap2)
  diff <- means_hap1 - means_hap2
  
  # Add column to heatmap data
  heatmap_data[[ds]] <- diff
}

# === Convert to matrix and plot heatmap ===
heatmap_matrix <- as.matrix(heatmap_data)

# Optional: Scale by row if needed
# heatmap_matrix <- t(scale(t(heatmap_matrix))) 

# === Plot heatmap ===
pheatmap(
  heatmap_matrix,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  fontsize_number = 10,
  color = colorRampPalette(c("yellow", "white", "lightblue"))(100),
  main = "Hap1 vs Hap2 Metric Differences Across Datasets",
  angle_col = 45
)
