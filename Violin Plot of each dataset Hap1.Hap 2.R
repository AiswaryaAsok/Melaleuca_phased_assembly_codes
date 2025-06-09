# === Install and load required libraries ===
packages <- c("ggplot2", "dplyr", "readxl", "tidyr", "officer", "rvg")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# === File paths ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"

# === Datasets and metrics ===
datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")
metrics <- c("MeanX", "CN", "DensX", "DensK", "AltK", "SelfK")

# === Start a Word document ===
doc <- read_docx()

# === Loop through datasets ===
for (ds in datasets) {
  # Load Hap1
  hap1_data <- tryCatch({
    read_excel(hap1_file, sheet = ds) %>%
      select(all_of(metrics)) %>%
      mutate(Haplotype = "Hap1")
  }, error = function(e) NULL)
  
  # Load Hap2
  hap2_data <- tryCatch({
    read_excel(hap2_file, sheet = ds) %>%
      select(all_of(metrics)) %>%
      mutate(Haplotype = "Hap2")
  }, error = function(e) NULL)
  
  # Skip if either dataset is missing
  if (is.null(hap1_data) || is.null(hap2_data)) {
    message(paste("Skipping", ds, "- sheet not found."))
    next
  }
  
  # Combine and reshape data
  combined <- bind_rows(hap1_data, hap2_data)
  combined_long <- combined %>%
    pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value")
  
  # Create violin plot
  violin_plot <- ggplot(combined_long, aes(x = Haplotype, y = Value, fill = Haplotype)) +
    geom_violin(trim = FALSE, alpha = 0.6, color = "black") +
    facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
    labs(title = paste("Violin Plot -", ds, "Metrics"),
         x = "Haplotype", y = "Value") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 0, size = 10),
      legend.position = "none"
    )
  
  # Add to Word doc
  doc <- doc %>%
    body_add_par(paste("Violin Plot for", ds, "Metrics - Hap1 vs Hap2"), style = "heading 2") %>%
    body_add_gg(value = violin_plot, width = 6.5, height = 6.5)
}

# === Save Word file ===
print(doc, target = "Violin_Plots_Hap1_Hap2.docx")
