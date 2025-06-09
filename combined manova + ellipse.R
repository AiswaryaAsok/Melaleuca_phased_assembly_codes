# === 1. Load Required Packages ===
packages <- c("readxl", "dplyr", "ggplot2", "gridExtra", "officer", "rvg", "ggpubr", "patchwork")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# === 2. File Paths and Dataset Setup ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"
datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")
metrics <- c("CN", "AltK", "SelfK", "DensK")

# === 3. Prepare Storage for Plots ===
biplots_list <- list()
ellipses_list <- list()

# === 4. Loop through Datasets ===
for (ds in datasets) {
  # Load Hap1 and Hap2
  hap1 <- read_excel(hap1_file, sheet = ds) %>%
    select(all_of(metrics)) %>%
    mutate(Haplotype = "Hap1")
  
  hap2 <- read_excel(hap2_file, sheet = ds) %>%
    select(all_of(metrics)) %>%
    mutate(Haplotype = "Hap2")
  
  df <- bind_rows(hap1, hap2)
  
  # PCA
  pca <- prcomp(scale(df[, metrics]), center = TRUE, scale. = TRUE)
  scores <- as.data.frame(pca$x)
  scores$Haplotype <- df$Haplotype
  
  # Loadings
  loadings <- as.data.frame(pca$rotation[, 1:2])
  loadings$Metric <- rownames(loadings)
  loadings$Color <- ifelse(loadings$Metric == "DensK", "red", "grey40")
  
 
  # === 5. Biplot with colored arrows only (no labels) ===
  biplot <- ggplot(scores, aes(PC1, PC2)) +
    geom_point(aes(color = Haplotype), alpha = 0.6) +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4, color = Color),
                 arrow = arrow(length = unit(0.25, "cm")), linewidth = 1.5) +
    scale_color_manual(values = c("Hap1" = "#1f77b4", "Hap2" = "#ff7f0e",
                                  "red" = "red", "grey40" = "grey40")) +
    guides(color = "none") +
    ggtitle(paste0(ds, " - PCA Biplot")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  # === 6. MANOVA Ellipse Plot ===
  ellipse <- ggplot(scores, aes(PC1, PC2)) +
    geom_point(aes(color = Haplotype), alpha = 0.5) +
    stat_ellipse(aes(color = Haplotype), type = "t", size = 1.1) +
    scale_color_manual(values = c("Hap1" = "#1f77b4", "Hap2" = "#ff7f0e")) +
    ggtitle(paste0(ds, " - MANOVA Ellipse")) +
    theme_minimal()
  
  biplots_list[[ds]] <- biplot
  ellipses_list[[ds]] <- ellipse
}

# === 7. Combine All Plots into Grid Panels ===
biplot_panel <- wrap_plots(biplots_list, ncol = 3)
ellipse_panel <- wrap_plots(ellipses_list, ncol = 3)

# === 8. Save to Word Document ===
doc <- read_docx() %>%
  body_add_par("All Dataset PCA Biplots (Color Emphasis on DensK)", style = "heading 1") %>%
  body_add_gg(biplot_panel, width = 8, height = 6.5) %>%
  body_add_par("All Dataset MANOVA Ellipses", style = "heading 1") %>%
  body_add_gg(ellipse_panel, width = 8, height = 6.5)

# === 9. Export ===
print(doc, target = "C:/Dissertation (R Studio)/Combined_Biplots_And_Ellipses.docx")
