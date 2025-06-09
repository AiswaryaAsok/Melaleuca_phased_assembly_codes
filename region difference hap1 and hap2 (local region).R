# 📦 Load required packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# 📂 Load Hap1 and Hap2 Windows data
hap1 <- read_csv("C:/Dissertation (R Studio)/Hap 1_Windows.csv")
hap2 <- read_csv("C:/Dissertation (R Studio)/Hap 2_Windows.csv")

# 📊 Count top 6 scaffolds for each haplotype
top_hap1 <- hap1 %>%
  count(SeqName) %>%
  arrange(desc(n)) %>%
  slice_head(n = 6) %>%
  rename(Hap1 = n)

top_hap2 <- hap2 %>%
  count(SeqName) %>%
  arrange(desc(n)) %>%
  slice_head(n = 6) %>%
  rename(Hap2 = n)

# 🔁 Combine both sets of top scaffolds
top_combined <- full_join(top_hap1, top_hap2, by = "SeqName") %>%
  replace_na(list(Hap1 = 0, Hap2 = 0)) %>%
  pivot_longer(cols = c(Hap1, Hap2), names_to = "Haplotype", values_to = "Count")

# 🎨 Create the comparison plot
ggplot(top_combined, aes(x = SeqName, y = Count, fill = Haplotype)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Top 6 Scaffolds by Region Count: Hap1 vs Hap2",
    x = "Scaffold",
    y = "Number of Regions",
    fill = "Haplotype"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 💾 Save the plot
ggsave("C:/Dissertation (R Studio)/Top6_Scaffold_Comparison.png", width = 10, height = 6, dpi = 300)
