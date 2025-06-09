# 📦 Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(ggrepel)
library(officer)
library(magrittr)

# 📂 Load tweak files
hap1 <- read_excel("C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx") %>%
  mutate(Haplotype = "Hap1")
hap2 <- read_excel("C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx") %>%
  mutate(Haplotype = "Hap2")

# 📂 Load FPOP data
fpop <- read_csv("C:/Dissertation (R Studio)/FPOP_All_Segments.csv")

# 📍 Define comparison regions
regions <- list(
  list(tag = "10_12Mb", scaffold1 = "MQH1CHR06.01", scaffold2 = "MQH2CHR06.01", start = 10000000, end = 12000000),
  list(tag = "22_24Mb", scaffold1 = "MQH1CHR06.01", scaffold2 = "MQH2CHR06.01", start = 22000000, end = 24000000)
)

# 🎨 Vibrant color palette
region_colors <- c(
  "Collapsed"    = "#E41A1C",  # Red
  "Repeat-Rich"  = "#377EB8",  # Blue
  "Uncertain"    = "#984EA3",  # Purple
  "Gene-Rich"    = "#4DAF4A",  # Green
  "GC-Island"    = "#FF7F00",  # Orange
  "Normal"       = "#A65628"   # Brown
)

# 🧩 Merge region data
all_df <- list()
fpop_all <- list()
region_labels <- list()

for (r in regions) {
  h1 <- hap1 %>% filter(Contig == r$scaffold1, Start >= r$start, Start <= r$end)
  h2 <- hap2 %>% filter(Contig == r$scaffold2, Start >= r$start, Start <= r$end)
  
  combined <- bind_rows(h1, h2) %>%
    mutate(RegionType = case_when(
      CN < 0.75 & DensK > 20 ~ "Collapsed",
      CN > 2.5 & SelfK > 20 ~ "Repeat-Rich",
      AltK > 10 & SelfK < 5 ~ "Uncertain",
      DensK > 15 & SelfK < 5 & CN > 1 & CN < 2 ~ "Gene-Rich",
      DensK > 25 & AltK > 8 ~ "GC-Island",
      TRUE ~ "Normal"
    ))
  
  combined$Region <- r$tag
  all_df[[r$tag]] <- combined
  
  fpop_sub <- fpop %>% filter(
    Sheet == "Windows",
    SeqName %in% c(r$scaffold1, r$scaffold2),
    Start >= r$start, Start <= r$end
  )
  fpop_all[[r$tag]] <- fpop_sub
  
  region_labels[[r$tag]] <- data.frame(
    Start = mean(c(r$start, r$end)),
    Label = r$tag
  )
}

df <- bind_rows(all_df)
fpop_df <- bind_rows(fpop_all)
labels_df <- bind_rows(region_labels)

# 📈 Convert to long format for plotting
df_long <- df %>%
  pivot_longer(cols = c(CN, DensK, AltK, SelfK), names_to = "Metric", values_to = "Value")

# 📊 Enhanced line plot
line_plot <- ggplot(df_long, aes(x = Start, y = Value, color = RegionType, group = interaction(Haplotype, Metric))) +
  geom_line(size = 0.7) +
  facet_grid(Metric ~ Haplotype, scales = "free_y") +
  scale_color_manual(values = region_colors) +
  geom_vline(data = fpop_df, aes(xintercept = Start), linetype = "dotted", color = "black", size = 0.3) +
  geom_text_repel(data = labels_df, aes(x = Start, y = Inf, label = Label),
                  inherit.aes = FALSE, nudge_y = -1.5, segment.alpha = 0.4, size = 3.2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major = element_line(color = "grey90")
  ) +
  labs(
    title = "Line Plot: 10–12 Mb and 22–24 Mb Regions (Annotated)",
    x = "Genomic Position", y = "Metric Value", color = "Region Type"
  )

ggsave("Combined_10_12_22_24_LineFPOP.png", line_plot, width = 11, height = 7, dpi = 300)

# 📉 Enhanced scatter plot
scatter_plot <- ggplot(df_long, aes(x = Start, y = Value, color = RegionType)) +
  geom_point(size = 1.4, alpha = 0.8) +
  facet_grid(Metric ~ Haplotype, scales = "free_y") +
  scale_color_manual(values = region_colors) +
  geom_vline(data = fpop_df, aes(xintercept = Start), linetype = "dotted", color = "black", size = 0.3) +
  geom_text_repel(data = labels_df, aes(x = Start, y = Inf, label = Label),
                  inherit.aes = FALSE, nudge_y = -1.5, segment.alpha = 0.4, size = 3.2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major = element_line(color = "grey90")
  ) +
  labs(
    title = "Scatter Plot: 10–12 Mb and 22–24 Mb Regions (Annotated)",
    x = "Genomic Position", y = "Metric Value", color = "Region Type"
  )

ggsave("Combined_10_12_22_24_ScatterFPOP.png", scatter_plot, width = 11, height = 7, dpi = 300)

# 📄 Save to Word (Optional)
doc <- read_docx()
doc <- doc %>%
  body_add_par("Annotated Comparison of 10–12 Mb and 22–24 Mb Regions with FPOP Overlay", style = "heading 1") %>%
  body_add_par("Line Plot", style = "heading 2") %>%
  body_add_img(src = "Combined_10_12_22_24_LineFPOP.png", width = 6, height = 4) %>%
  body_add_par("Scatter Plot", style = "heading 2") %>%
  body_add_img(src = "Combined_10_12_22_24_ScatterFPOP.png", width = 6, height = 4)

print(doc, target = "C:/Dissertation (R Studio)/Comparison_10_12_vs_22_24_Regions.docx")
