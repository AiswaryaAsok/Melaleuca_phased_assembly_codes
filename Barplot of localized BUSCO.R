# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)

# === Step 1: Load tweak files ===
hap1 <- read_excel("C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx")
hap2 <- read_excel("C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx")

# === Step 2: Add haplotype labels ===
hap1 <- hap1 %>% mutate(Haplotype = "Hap1")
hap2 <- hap2 %>% mutate(Haplotype = "Hap2")

# === Step 3: Count rows per scaffold (Contig) to simulate BUSCO density ===
hap1_top6 <- hap1 %>%
  count(Contig, sort = TRUE) %>%
  slice_head(n = 6) %>%
  mutate(Haplotype = "Hap1")

hap2_top6 <- hap2 %>%
  count(Contig, sort = TRUE) %>%
  slice_head(n = 6) %>%
  mutate(Haplotype = "Hap2")

# === Step 4: Combine and view top scaffolds ===
top6_combined <- bind_rows(hap1_top6, hap2_top6)
print(top6_combined)

# === Optional: Plot the counts ===
ggplot(top6_combined, aes(x = reorder(Contig, -n), y = n, fill = Haplotype)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Top 6 BUSCO-Rich Scaffolds (based on window count)",
    x = "Scaffold", y = "Number of BUSCO-associated windows"
  ) +
  scale_fill_manual(values = c("Hap1" = "steelblue", "Hap2" = "darkorange")) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
