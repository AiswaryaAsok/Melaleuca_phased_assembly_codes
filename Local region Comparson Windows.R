# 📦 Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(officer)
library(magrittr)
library(flextable)

# 📂 Load Hap1 and Hap2 Windows data
hap1 <- read_csv("C:/Dissertation (R Studio)/Hap 1_Windows.csv")
hap2 <- read_csv("C:/Dissertation (R Studio)/Hap 2_Windows.csv")

# 🎯 Define region and scaffold
target_scaffold_hap1 <- "MQH1CHR06.01"
target_scaffold_hap2 <- "MQH2CHR06.01"
start_pos <- 1e7      # 10 Mb
end_pos <- 1.2e7      # 12 Mb

# 🧬 Subset data
hap1_local <- hap1 %>%
  filter(SeqName == target_scaffold_hap1 & Start >= start_pos & Start <= end_pos) %>%
  mutate(Haplotype = "Hap1")

hap2_local <- hap2 %>%
  filter(SeqName == target_scaffold_hap2 & Start >= start_pos & Start <= end_pos) %>%
  mutate(Haplotype = "Hap2")

combined_local <- bind_rows(hap1_local, hap2_local)

# 📈 1. Line plots of CN, DensK, SelfK, AltK
metrics_long <- combined_local %>%
  select(Start, Haplotype, CN, DensK, SelfK, AltK) %>%
  pivot_longer(cols = c(CN, DensK, SelfK, AltK), names_to = "Metric", values_to = "Value")

line_plot <- ggplot(metrics_long, aes(x = Start, y = Value, color = Haplotype)) +
  geom_line(size = 1, alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(
    title = paste0("Local Region Metrics: ", target_scaffold_hap1, " (", start_pos/1e6, "-", end_pos/1e6, " Mb)"),
    x = "Genomic Position (bp)", y = "Metric Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("C:/Dissertation (R Studio)/Line_Metrics_LocalRegion.png", line_plot, width = 8, height = 10, dpi = 300)

# 📉 2. Clean Scatter Plot (CN vs DensK)
scatter_plot <- ggplot(combined_local, aes(x = CN, y = DensK, color = Haplotype)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "CN vs DensK (Local Region)",
    x = "Copy Number (CN)", y = "K-mer Density (DensK)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("C:/Dissertation (R Studio)/Scatter_CN_vs_DensK_Clean.png", scatter_plot, width = 8, height = 6, dpi = 300)

# 📋 3. Create legend table for annotated points (CN < 0.75 & DensK > 20)
collapsed_points <- combined_local %>%
  filter(CN < 0.75 & DensK > 20) %>%
  mutate(
    Mb = round(Start / 1e6, 2),
    Start = format(Start, scientific = FALSE)
  ) %>%
  select(Haplotype, SeqName, Mb, CN, DensK)

write_csv(collapsed_points, "C:/Dissertation (R Studio)/Collapsed_Annotation_Table.csv")

# ✨ Optional: Create a flextable for the Word doc
ft <- flextable(collapsed_points) %>%
  set_header_labels(Mb = "Position (Mb)", CN = "Copy Number", DensK = "K-mer Density") %>%
  autofit()

# 📄 4. Save everything into a Word doc
doc <- read_docx() %>%
  body_add_par("Local Region Comparison: MQH1CHR06.01 vs MQH2CHR06.01", style = "heading 1") %>%
  body_add_par("Line Plot of CN, DensK, SelfK, AltK", style = "heading 2") %>%
  body_add_img("C:/Dissertation (R Studio)/Line_Metrics_LocalRegion.png", width = 6.5, height = 8) %>%
  body_add_par("Scatter Plot: CN vs DensK", style = "heading 2") %>%
  body_add_img("C:/Dissertation (R Studio)/Scatter_CN_vs_DensK_Clean.png", width = 6, height = 5) %>%
  body_add_par("Annotated Regions (CN < 0.75 & DensK > 20)", style = "heading 2") %>%
  body_add_flextable(ft)

print(doc, target = "C:/Dissertation (R Studio)/Local_Region_Comparison 1.docx")
