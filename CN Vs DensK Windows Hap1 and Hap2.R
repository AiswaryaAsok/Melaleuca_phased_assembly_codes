# 📦 Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# 📂 Load Hap1 and Hap2
hap1 <- read_csv("C:/Dissertation (R Studio)/Hap 1_Windows.csv")
hap2 <- read_csv("C:/Dissertation (R Studio)/Hap 2_Windows.csv")

# 🎯 Define region
target_scaffold_hap1 <- "MQH1CHR06.01"
target_scaffold_hap2 <- "MQH2CHR06.01"
start_pos <- 1e7
end_pos <- 1.2e7

# 🧬 Subset local region
hap1_local <- hap1 %>%
  filter(SeqName == target_scaffold_hap1 & Start >= start_pos & Start <= end_pos) %>%
  mutate(Haplotype = "Hap1")

hap2_local <- hap2 %>%
  filter(SeqName == target_scaffold_hap2 & Start >= start_pos & Start <= end_pos) %>%
  mutate(Haplotype = "Hap2")

combined_local <- bind_rows(hap1_local, hap2_local)

# 🧠 Add RegionType classification
classified_local <- combined_local %>%
  mutate(RegionType = case_when(
    CN < 0.75 & DensK > 20 ~ "Collapsed",
    CN > 2.5 & SelfK > 20 ~ "Repeat-Rich",
    HomPC < 0.3 & AltK > 10 ~ "Uncertain",
    DensK > 15 & SelfK < 5 & CN > 1 & CN < 2 ~ "Gene-Rich",
    TRUE ~ "Normal"
  ))

# 📈 Scatter plot: CN vs DensK with RegionType
ggplot(classified_local, aes(x = CN, y = DensK, color = RegionType)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~Haplotype) +
  scale_color_manual(values = c(
    "Collapsed" = "#D55E00",
    "Repeat-Rich" = "#0072B2",
    "Uncertain" = "#999999",
    "Gene-Rich" = "#009E73",
    "Normal" = "grey80"
  )) +
  labs(
    title = "CN vs DensK (10–12 Mb region) Colored by Region Type",
    x = "Copy Number (CN)",
    y = "K-mer Density (DensK)",
    color = "Region Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# 💾 Save the plot
ggsave("C:/Dissertation (R Studio)/CN_vs_DensK_RegionType_Overlay.png", width = 10, height = 6, dpi = 300)
