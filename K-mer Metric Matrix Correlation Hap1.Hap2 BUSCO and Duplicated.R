# === Install & Load Required Libraries ===
packages <- c("readxl", "dplyr", "GGally", "ggplot2", "officer", "rvg")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# === File paths ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"
metrics <- c("AltK", "DensK", "CN")
datasets <- c("BUSCO", "Duplicated")

# === Load and combine data ===
load_data <- function(file_path, hap) {
  data_list <- list()
  for (ds in datasets) {
    df <- tryCatch({
      read_excel(file_path, sheet = ds) %>%
        select(all_of(metrics)) %>%
        mutate(Dataset = ds, Haplotype = hap)
    }, error = function(e) NULL)
    if (!is.null(df)) {
      data_list[[ds]] <- df
    }
  }
  bind_rows(data_list)
}

hap1_df <- load_data(hap1_file, "Hap1")
hap2_df <- load_data(hap2_file, "Hap2")

# === Plotting Function ===
create_kmer_matrix_plot <- function(data, hap_label) {
  data$Dataset <- factor(data$Dataset, levels = c("BUSCO", "Duplicated"))
  ggpairs(
    data = data,
    columns = metrics,
    mapping = aes(color = Dataset),
    lower = list(continuous = wrap("points", alpha = 0.6, size = 1.5)),
    diag = list(continuous = wrap("densityDiag")),
    upper = list(continuous = wrap("cor", size = 4))
  ) +
    labs(title = paste("K-mer Metric Correlation Matrix -", hap_label)) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
}

# === Create plots ===
hap1_plot <- create_kmer_matrix_plot(hap1_df, "Hap1")
hap2_plot <- create_kmer_matrix_plot(hap2_df, "Hap2")

# === Export to Word document ===
doc <- read_docx() %>%
  body_add_par("K-mer Metric Matrix Correlation: BUSCO vs Duplicated", style = "heading 1") %>%
  body_add_par("Hap1", style = "heading 2") %>%
  body_add_gg(hap1_plot, width = 7.5, height = 6.5) %>%
  body_add_par("Hap2", style = "heading 2") %>%
  body_add_gg(hap2_plot, width = 7.5, height = 6.5)

# === Save the Word document ===
print(doc, target = "Kmer_Matrix_Correlation_BUSCO_vs_Duplicated.docx")
