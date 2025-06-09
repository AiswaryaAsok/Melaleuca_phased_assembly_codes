# === 1. Load Required Libraries ===
libraries <- c("readxl", "dplyr", "ggplot2", "factoextra", "cluster", "officer", "rvg", "flextable")
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
results_table <- data.frame(Haplotype = character(), Average_Silhouette_Width = numeric(), stringsAsFactors = FALSE)

# === 4. Loop Over Haplotypes ===
for (hap in names(hap_files)) {
  data <- read_excel(hap_files[[hap]], sheet = "Duplicated")
  metrics <- data %>% select(CN, DensK, AltK, SelfK)
  scaled_data <- scale(metrics)
  
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = 4)
  sil <- silhouette(kmeans_result$cluster, dist(scaled_data))
  avg_sil_width <- mean(sil[, 3])
  
  # Append result to table
  results_table <- rbind(results_table, data.frame(Haplotype = hap, Average_Silhouette_Width = round(avg_sil_width, 4)))
  
  p_sil <- fviz_silhouette(sil, print.summary = FALSE) +
    geom_hline(yintercept = avg_sil_width, color = "red", linetype = "dashed", linewidth = 0.7) +
    annotate("text", x = Inf, y = Inf, label = "\u2192 X: Observation index\n\u2191 Y: Silhouette width (Si)", hjust = 1.1, vjust = 1.3, size = 3.5, color = "gray30") +
    labs(
      title = paste("Silhouette Plot -", hap),
      subtitle = paste("Average silhouette width:", round(avg_sil_width, 4)),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  doc <- doc %>%
    body_add_par(paste("Silhouette Plot for", hap), style = "heading 1") %>%
    body_add_par(paste("Average Silhouette Width:", round(avg_sil_width, 4)), style = "Normal") %>%
    body_add_gg(p_sil, width = 6.5, height = 5)
}

# === 5. Add Summary Table ===
doc <- doc %>%
  body_add_par("Summary Table of Average Silhouette Widths", style = "heading 1") %>%
  body_add_flextable(flextable(results_table))
table(kmeans_result$cluster)
print("table(kmeans_result$cluster")


# === 6. Save the Word Document ===
print(doc, target = "C:/Dissertation (R Studio)/Silhouette_Separate_Hap_Plots.docx")
