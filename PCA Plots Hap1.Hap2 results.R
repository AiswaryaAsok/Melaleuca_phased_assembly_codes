# === Install and load necessary packages ===
packages <- c("readxl", "dplyr", "ggplot2", "factoextra", "tidyr", "purrr", "officer", "rvg", "broom")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# === File paths ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"

# === Datasets and metrics to analyze ===
datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")
key_metrics <- c("MeanX", "CN", "DensX", "DensK", "AltK", "SelfK")

# === PCA function ===
run_pca <- function(file_path, sheet, haplotype) {
  df <- tryCatch({
    read_excel(file_path, sheet = sheet)
  }, error = function(e) {
    message(paste("Sheet", sheet, "not found in", haplotype))
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  df <- df %>%
    mutate(Dataset = sheet, Haplotype = haplotype)
  
  pca_data <- df %>%
    select(any_of(key_metrics)) %>%
    scale() %>%
    as.data.frame()
  
  pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  
  loadings <- as.data.frame(pca$rotation[, 1:2])
  loadings$Metric <- rownames(loadings)
  loadings$Dataset <- sheet
  loadings$Haplotype <- haplotype
  
  pca_df <- as.data.frame(pca$x)
  pca_df$Haplotype <- haplotype
  pca_df$Dataset <- sheet
  
  scatter <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Haplotype)) +
    geom_point(alpha = 0.6, size = 2) +
    labs(title = paste("PCA -", sheet, "-", haplotype),
         x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "%)")) +
    theme_minimal()
  
  biplot <- fviz_pca_biplot(pca,
                            label = "var", repel = TRUE,
                            title = paste("Biplot -", sheet, "-", haplotype),
                            addEllipses = FALSE)
  
  return(list(loadings = loadings, scatter = scatter, biplot = biplot, pca_df = pca_df))
}

# === Run PCA and collect results ===
all_results <- list()

for (dataset in datasets) {
  hap1_res <- run_pca(hap1_file, dataset, "Hap1")
  hap2_res <- run_pca(hap2_file, dataset, "Hap2")
  
  if (!is.null(hap1_res)) all_results[[paste0(dataset, "_Hap1")]] <- hap1_res
  if (!is.null(hap2_res)) all_results[[paste0(dataset, "_Hap2")]] <- hap2_res
}

# === Combine PCA loadings and save ===
combined_loadings <- bind_rows(lapply(all_results, function(x) x$loadings))
write.csv(combined_loadings, "PCA_Loadings_All_Datasets_Hap1_Hap2.csv", row.names = FALSE)

# === Combine all PCA points for distribution plot ===
pca_points <- bind_rows(
  lapply(names(all_results), function(name) {
    pca_obj <- all_results[[name]]
    df <- pca_obj$pca_df
    df$Dataset <- strsplit(name, "_")[[1]][1]
    df$Haplotype <- strsplit(name, "_")[[1]][2]
    return(df)
  })
)

# === PCA Distribution Plot ===
dist_plot <- ggplot(pca_points, aes(x = PC1, y = PC2, color = Dataset, shape = Haplotype)) +
  geom_point(alpha = 0.5, size = 1.5) +
  facet_wrap(~ Dataset, scales = "free") +
  labs(title = "PCA Distribution Across All Datasets",
       x = "PC1", y = "PC2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold")
  )

# === Run MANOVA for each dataset ===
manova_results <- lapply(datasets, function(ds) {
  df_subset <- pca_points %>% filter(Dataset == ds)
  manova_out <- manova(cbind(PC1, PC2) ~ Haplotype, data = df_subset)
  summary_out <- summary(manova_out)
  tidy_out <- tidy(manova_out)
  return(list(dataset = ds, result = summary_out, table = tidy_out))
})

# === Create Word document ===
doc <- read_docx()

# Add individual PCA plots
for (key in names(all_results)) {
  res <- all_results[[key]]
  
  doc <- doc %>%
    body_add_par(paste("PCA Plots for", key), style = "heading 2") %>%
    body_add_gg(value = res$scatter, width = 6, height = 5) %>%
    body_add_par("Biplot", style = "heading 3") %>%
    body_add_gg(value = res$biplot, width = 6, height = 5)
}

# Add PCA distribution plot
doc <- doc %>%
  body_add_par("PCA Distribution Plot - All Datasets", style = "heading 2") %>%
  body_add_gg(value = dist_plot, width = 6.5, height = 5.5)

# Add MANOVA results
doc <- doc %>%
  body_add_par("MANOVA Summary", style = "heading 1")

for (res in manova_results) {
  manova_text <- paste(capture.output(res$result), collapse = "\n")
  doc <- doc %>%
    body_add_par(paste0("Dataset: ", res$dataset), style = "heading 2") %>%
    body_add_par(manova_text, style = "Normal")
}

# Save Word document
print(doc, target = "PCA_All_Datasets_Hap1_Hap2.docx")
