# === 1. Load Required Packages ===
packages <- c("readxl", "dplyr", "ggplot2", "ggcorrplot", "patchwork")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# === 2. File Setup ===
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"
datasets <- c("BUSCO", "Duplicated", "Contigs", "Sequences", "Windows")
metrics <- c("CN", "DensK", "AltK", "SelfK")

# === 3. Generate Correlation Plots Function ===
create_corrplot_panel <- function(file_path, hap_name) {
  plot_list <- list()
  
  for (ds in datasets) {
    df <- tryCatch(read_excel(file_path, sheet = ds), error = function(e) NULL)
    if (!is.null(df)) {
      cor_matrix <- cor(df[, metrics], use = "complete.obs", method = "pearson")
      p <- ggcorrplot(cor_matrix,
                      type = "lower",
                      lab = TRUE,
                      lab_size = 3,
                      title = paste0(hap_name, " - ", ds),
                      colors = c("yellow", "white", "lightgreen"),
                      ggtheme = theme_minimal()) +
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
      plot_list[[ds]] <- p
    }
  }
  
  return(wrap_plots(plot_list, ncol = 3))
}

# === 4. Generate Panels ===
hap1_corr_panel <- create_corrplot_panel(hap1_file, "Hap1")
hap2_corr_panel <- create_corrplot_panel(hap2_file, "Hap2")

# === 5. Plot and Save (Optional) ===
# View in R
print(hap1_corr_panel)
print(hap2_corr_panel)

# === 6. Save to Word (optional) ===
library(officer)
doc <- read_docx() %>%
  body_add_par("Correlation Matrix - Hap1", style = "heading 1") %>%
  body_add_gg(hap1_corr_panel, width = 8, height = 6.5) %>%
  body_add_par("Correlation Matrix - Hap2", style = "heading 1") %>%
  body_add_gg(hap2_corr_panel, width = 8, height = 6.5)

print(doc, target = "C:/Dissertation (R Studio)/Violin_Metrics_Correlation_Panels.docx")
