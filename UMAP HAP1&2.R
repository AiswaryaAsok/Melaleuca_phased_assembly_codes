library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(umap)

# 📂 Load dataset (choose one only)
hap1 <- read_csv("C:/Dissertation (R Studio)/Hap 2_Windows.csv") %>%
  drop_na(CN, DensK, SelfK, AltK)

# ✅ Filter to remove extreme values if needed
hap1 <- hap1 %>% filter(CN < 10)

# 🧪 Prepare input
umap_input <- hap1 %>%
  select(CN, DensK, SelfK, AltK)

# ❌ Remove constant columns
umap_input <- umap_input[, apply(umap_input, 2, function(x) var(x, na.rm = TRUE) > 0)]

# ⚙️ Run UMAP
set.seed(123)
umap_result <- umap(umap_input)
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$SeqName <- hap1$SeqName

# 📊 Plot UMAP
ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
  geom_point(alpha = 0.7) +
  labs(title = "UMAP of Hap2 Windows Metrics", x = "UMAP1", y = "UMAP2") +
  theme_minimal()
ggsave("C:/Dissertation (R Studio)/UMAP_Hap2_Windows.png", width = 8, height = 6)
