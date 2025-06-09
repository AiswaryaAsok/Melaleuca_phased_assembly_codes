# 📦 Load required libraries
library(readxl)
library(dplyr)
library(fpop)        # Install from GitHub if not available
library(tibble)

# 📂 Define file paths
hap1_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap1.tweak.xlsx"
hap2_file <- "C:/Dissertation (R Studio)/drMelQuin1-depthkopy/drMelQuin1.v240825.1.yahs.hap2.tweak.xlsx"

# 🧪 Read Windows sheets
hap1_windows <- read_excel(hap1_file, sheet = "Windows")
hap2_windows <- read_excel(hap2_file, sheet = "Windows")

# 🧹 Clean NA or infinite CN rows
hap1_windows <- hap1_windows %>% filter(!is.na(CN) & is.finite(CN))
hap2_windows <- hap2_windows %>% filter(!is.na(CN) & is.finite(CN))

# 🧠 Function to apply simple segmentation-like grouping
segment_regions <- function(df, haplotype_label) {
  df <- df %>%
    arrange(SeqName, Start) %>%
    group_by(SeqName) %>%
    mutate(
      SegmentID = cumsum(c(TRUE, abs(diff(CN)) > 0.2)),  # basic segmentation on CN jump
      Haplotype = haplotype_label
    ) %>%
    ungroup()
  
  segments <- df %>%
    group_by(SeqName, SegmentID, Haplotype) %>%
    summarise(
      Start = min(Start),
      End = max(End),
      SegmentMean = mean(CN, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(segments)
}

# 🧬 Segment both haplotypes
hap1_segments <- segment_regions(hap1_windows, "Hap1")
hap2_segments <- segment_regions(hap2_windows, "Hap2")

# 💾 Save output
write.csv(hap1_segments, "C:/Dissertation (R Studio)/FPOP_Hap1.csv", row.names = FALSE)
write.csv(hap2_segments, "C:/Dissertation (R Studio)/FPOP_Hap2.csv", row.names = FALSE)

cat("✅ FPOP-style segments saved for Hap1 and Hap2.")
