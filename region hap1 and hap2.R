# 📦 Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(officer)
library(magrittr)

# 📂 Load dataset
windows_path <- "C:/Dissertation (R Studio)/Hap 2_Windows.csv"
windows <- read_csv(windows_path)

# 🧽 Clean dataset
windows_clean <- windows %>%
  filter(across(c(CN, DensK, SelfK, AltK, HomPC), ~ !is.na(.) & is.finite(.)))

# 🧠 Define metrics, thresholds, and classification logic
metrics <- list(
  CN = list(var = "CN", classify = function(df) {
    df %>% mutate(RegionType = case_when(
      CN < 0.75 & DensK > 20 ~ "Collapsed",
      CN > 2.5 & SelfK > 20 ~ "Repeat-Rich",
      HomPC < 0.3 & AltK > 10 ~ "Uncertain",
      DensK > 15 & SelfK < 5 & CN > 1 & CN < 2 ~ "Gene-Rich",
      TRUE ~ "Normal"
    ))
  }),
  DensK = list(var = "DensK", classify = function(df) {
    df %>% mutate(RegionType = case_when(
      DensK > 25 ~ "High DensK",
      DensK < 10 ~ "Low DensK",
      TRUE ~ "Moderate"
    ))
  }),
  AltK = list(var = "AltK", classify = function(df) {
    df %>% mutate(RegionType = case_when(
      AltK > 20 ~ "AltK-Rich",
      AltK < 5 ~ "AltK-Poor",
      TRUE ~ "Moderate"
    ))
  }),
  SelfK = list(var = "SelfK", classify = function(df) {
    df %>% mutate(RegionType = case_when(
      SelfK > 25 ~ "Self-repetitive",
      SelfK < 5 ~ "Unique",
      TRUE ~ "Moderate"
    ))
  })
)

# 📁 Create output folders if needed
dir.create("C:/Dissertation (R Studio)/Metric_Plots", showWarnings = FALSE)

# 📝 Create Word document
doc <- read_docx()

# 🔁 Loop over all metrics
for (metric_name in names(metrics)) {
  metric <- metrics[[metric_name]]
  var <- metric$var
  
  # 🧪 Classify
  classified <- metric$classify(windows_clean)
  
  # 🧬 Pick top 6 scaffolds for clarity
  top_scaffolds <- classified %>%
    count(SeqName, sort = TRUE) %>%
    top_n(6) %>%
    pull(SeqName)
  
  subset_classified <- classified %>%
    filter(SeqName %in% top_scaffolds)
  
  # 🎨 Plot
  p <- ggplot(subset_classified, aes_string(x = "Start", y = var, color = "RegionType")) +
    geom_point(alpha = 0.6, size = 1.5) +
    facet_wrap(~ SeqName, scales = "free_x", ncol = 2) +
    labs(
      title = paste("Region Classification by", var),
      x = "Genomic Start Position",
      y = var,
      color = "Region Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # 💾 Save PNG
  image_path <- paste0("C:/Dissertation (R Studio)/Metric_Plots/", metric_name, "_plot.png")
  ggsave(image_path, p, width = 10, height = 6, dpi = 300)
  
  # 📎 Add to Word
  doc <- doc %>%
    body_add_par(paste(metric_name, "Classification Plot"), style = "heading 2") %>%
    body_add_img(src = image_path, width = 6, height = 4.5) %>%
    body_add_par("")  # spacing
}

# 💾 Save Word file
print(doc, target = "C:/Dissertation (R Studio)/Region_Classification_Plots.docx")
