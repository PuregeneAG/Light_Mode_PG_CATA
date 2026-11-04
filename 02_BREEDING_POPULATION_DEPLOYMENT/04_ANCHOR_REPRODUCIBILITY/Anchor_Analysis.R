rm(list = ls())

############################################################
## Anchor Analysis v2 — Fixed Anchor GIDs Only
## Light Mode Deployment QC
##
## Anchors:
## - 17592858
## - 17592868
##
## Days:
## - Day01_AM
## - Day03_AM
## - Day04_AM
## These three sessions were defined a priori as the operational repeated-anchor
## series. The two genotypes were important to the breeding program and were
## therefore encountered again later in the deployment. The Day07_AM occurrence
## of 17592858 and Day08_AM occurrence of 17592868 were ordinary breeding-team
## deployment samples, remain in the 86-evaluation population analysis, and are
## not treated as anchor replicates.
##
## Input:
## - Pct_wide_All_Days.xlsx
##
## Outputs:
## - Supplementary anchor heatmap
## - Supplementary anchor line plot
## - Bray-Curtis pairwise similarity
## - Bray-Curtis summary table
## - Category-level stability table
############################################################

############################################################
## 0. Packages
############################################################
required_packages <- c(
        "rstudioapi",
        "readxl",
        "tidyverse",
        "vegan",
        "writexl"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
        if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
}

############################################################
## 1. Paths
############################################################
get_script_dir <- function() {
        args <- commandArgs(trailingOnly = FALSE)
        file_arg <- grep("^--file=", args, value = TRUE)
        if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
        if (rstudioapi::isAvailable()) return(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))
        normalizePath(getwd())
}
input_dir <- get_script_dir()
deployment_dir <- normalizePath(file.path(input_dir, ".."), mustWork = TRUE)
data_file <- file.path(deployment_dir, "03_COMBINED_DATA", "Pct_wide_All_Days.xlsx")

if (!file.exists(data_file)) {
        stop("Pct_wide_All_Days.xlsx not found in 03_COMBINED_DATA.")
}

output_dir <- file.path(input_dir, "OUTPUTS")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Load data
############################################################
df <- read_excel(data_file, sheet = "Category_Percent_Wide") %>%
        mutate(
                GID = as.character(GID),
                LM = as.character(LM),
                LMCode = as.character(LMCode),
                Day = as.character(Day)
        )

############################################################
## 3. Fixed anchor GIDs and days
############################################################
anchor_gids <- c("17592858", "17592868")

anchor_days <- c(
        "Day01_AM",
        "Day03_AM",
        "Day04_AM"
)

############################################################
## 4. Define 9 parental aroma categories
############################################################
aroma_cols <- c(
        "Cheesy/Sweaty",
        "Citrus/Terpentine",
        "Earthy/Musty/Moldy",
        "Floral",
        "Fruity",
        "Herbal/Spicy",
        "Resinous/Pine-like",
        "Skunky/Animalic",
        "Tropical/Exotic"
)

missing_cols <- setdiff(aroma_cols, names(df))

if (length(missing_cols) > 0) {
        stop(
                "Missing aroma category columns:\n",
                paste(missing_cols, collapse = "\n")
        )
}

############################################################
## 5. Filter only anchor GIDs and anchor days
############################################################
anchor_pct <- df %>%
        filter(
                GID %in% anchor_gids,
                Day %in% anchor_days
        ) %>%
        mutate(
                Day = factor(Day, levels = anchor_days),
                Sample_ID = paste(GID, Day, LM, sep = "_"),
                Sample_Label = paste0(GID, " | ", Day, " | ", LM)
        ) %>%
        select(Sample_ID, Sample_Label, GID, Day, LM, LMCode, all_of(aroma_cols)) %>%
        mutate(across(all_of(aroma_cols), ~ replace_na(as.numeric(.x), 0))) %>%
        arrange(GID, Day, LM)

if (nrow(anchor_pct) == 0) {
        stop("No rows found for the fixed anchor GIDs and days.")
}

message("Anchor rows retained: ", nrow(anchor_pct))
print(anchor_pct %>% select(GID, Day, LM, LMCode))

write_csv(
        anchor_pct,
        file.path(output_dir, "Anchor_Percentage_Fingerprints_Wide.csv")
)

############################################################
## 6. Long format
############################################################
anchor_long <- anchor_pct %>%
        pivot_longer(
                cols = all_of(aroma_cols),
                names_to = "Category",
                values_to = "Percent"
        ) %>%
        mutate(
                Category = factor(Category, levels = aroma_cols),
                Day = factor(Day, levels = anchor_days)
        ) %>%
        arrange(GID, Day, LM, Category) %>%
        mutate(
                Sample_Label = factor(
                        Sample_Label,
                        levels = rev(unique(Sample_Label))
                )
        )

write_csv(
        anchor_long,
        file.path(output_dir, "Anchor_Percentage_Fingerprints_Long.csv")
)

############################################################
## 7. Supplementary anchor heatmap
############################################################
p_heatmap <- ggplot(
        anchor_long,
        aes(
                x = Category,
                y = Sample_Label,
                fill = Percent
        )
) +
        geom_tile(color = "grey80", linewidth = 0.35) +
        geom_text(
                aes(label = round(Percent, 0)),
                size = 3.2,
                color = "black"
        ) +
        scale_fill_gradient(
                low = "white",
                high = "#00441b",
                limits = c(0, 100),
                name = "% panel\nagreement"
        ) +
        facet_grid(
                GID ~ .,
                scales = "free_y",
                space = "free_y"
        ) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Parental aroma category",
                y = "Repeated anchor evaluation"
        ) +
        theme_bw(base_size = 13) +
        theme(
                panel.grid = element_blank(),
                axis.text.x = element_text(
                        angle = 45,
                        hjust = 1,
                        face = "bold"
                ),
                axis.text.y = element_text(size = 10),
                axis.title = element_text(face = "bold"),
                strip.text.y = element_text(face = "bold", size = 12),
                legend.title = element_text(face = "bold"),
                plot.margin = margin(10, 10, 10, 10)
        )

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_Fingerprint_Heatmap_FIXED.pdf"),
        plot = p_heatmap,
        width = 13,
        height = 6,
        bg = "white"
)

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_Fingerprint_Heatmap_FIXED.png"),
        plot = p_heatmap,
        width = 13,
        height = 6,
        dpi = 300,
        bg = "white"
)

############################################################
## 8. Supplementary line plot
############################################################
p_lines <- ggplot(
        anchor_long,
        aes(
                x = Category,
                y = Percent,
                group = Sample_Label,
                color = Day
        )
) +
        geom_line(linewidth = 1.0, alpha = 0.8) +
        geom_point(size = 2.8, alpha = 0.9) +
        facet_wrap(~ GID, ncol = 1) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Parental aroma category",
                y = "% panel agreement",
                color = "Evaluation day"
        ) +
        theme_bw(base_size = 13) +
        theme(
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(
                        angle = 45,
                        hjust = 1,
                        face = "bold"
                ),
                axis.title = element_text(face = "bold"),
                strip.text = element_text(face = "bold"),
                legend.title = element_text(face = "bold")
        )

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_Fingerprint_LinePlot_FIXED.pdf"),
        plot = p_lines,
        width = 12,
        height = 8,
        bg = "white"
)

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_Fingerprint_LinePlot_FIXED.png"),
        plot = p_lines,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white"
)

############################################################
## 9. Bray-Curtis similarity between repeated anchor evaluations
############################################################
mat <- anchor_pct %>%
        select(all_of(aroma_cols)) %>%
        as.data.frame()

rownames(mat) <- anchor_pct$Sample_ID

bc_dist <- vegan::vegdist(mat, method = "bray")
bc_similarity <- 1 - as.matrix(bc_dist)

bc_table <- as.data.frame(as.table(bc_similarity)) %>%
        rename(
                Sample_1 = Var1,
                Sample_2 = Var2,
                BrayCurtis_Similarity = Freq
        ) %>%
        filter(as.character(Sample_1) < as.character(Sample_2)) %>%
        mutate(
                GID_1 = str_extract(as.character(Sample_1), "^[0-9]+"),
                GID_2 = str_extract(as.character(Sample_2), "^[0-9]+")
        ) %>%
        filter(GID_1 == GID_2) %>%
        mutate(
                GID = GID_1,
                Comparison = paste(Sample_1, "vs", Sample_2),
                BrayCurtis_Similarity = round(BrayCurtis_Similarity, 4)
        ) %>%
        select(GID, Sample_1, Sample_2, Comparison, BrayCurtis_Similarity) %>%
        arrange(GID, desc(BrayCurtis_Similarity))

bc_summary <- bc_table %>%
        group_by(GID) %>%
        summarise(
                Mean_BrayCurtis_Similarity = round(mean(BrayCurtis_Similarity), 4),
                Median_BrayCurtis_Similarity = round(median(BrayCurtis_Similarity), 4),
                Min_BrayCurtis_Similarity = round(min(BrayCurtis_Similarity), 4),
                Max_BrayCurtis_Similarity = round(max(BrayCurtis_Similarity), 4),
                N_Comparisons = n(),
                .groups = "drop"
        )

write_csv(
        bc_table,
        file.path(output_dir, "Anchor_BrayCurtis_Pairwise_FIXED.csv")
)

write_csv(
        bc_summary,
        file.path(output_dir, "Anchor_BrayCurtis_Summary_FIXED.csv")
)

############################################################
## 10. Bray-Curtis similarity plot
############################################################
bc_table_plot <- bc_table %>%
        group_by(GID) %>%
        mutate(
                Comparison_ID = paste0("C", row_number()),
                BrayCurtis_Label = sprintf("%.2f", BrayCurtis_Similarity)
        ) %>%
        ungroup() %>%
        left_join(
                bc_summary %>%
                        select(GID, Mean_BrayCurtis_Similarity),
                by = "GID"
        )

p_bc <- ggplot(
        bc_table_plot,
        aes(
                x = Comparison_ID,
                y = BrayCurtis_Similarity,
                fill = GID
        )
) +
        geom_col(
                width = 0.7,
                color = "black",
                alpha = 0.85
        ) +
        geom_text(
                aes(label = BrayCurtis_Label),
                vjust = -0.35,
                size = 3.8
        ) +
        geom_hline(
                aes(yintercept = Mean_BrayCurtis_Similarity),
                linetype = "dashed",
                linewidth = 0.7
        ) +
        facet_wrap(~ GID, scales = "free_x") +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Pairwise repeated-evaluation comparison",
                y = "Bray-Curtis similarity"
        ) +
        theme_bw(base_size = 13) +
        theme(
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(face = "bold"),
                axis.title = element_text(face = "bold"),
                strip.text = element_text(face = "bold"),
                legend.position = "none"
        )

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_BrayCurtis_Similarity_FIXED.pdf"),
        plot = p_bc,
        width = 10,
        height = 5.5,
        bg = "white"
)

ggsave(
        filename = file.path(output_dir, "Supp_Anchor_BrayCurtis_Similarity_FIXED.png"),
        plot = p_bc,
        width = 10,
        height = 5.5,
        dpi = 300,
        bg = "white"
)

############################################################
## 11. Category-level stability across anchor repeats
############################################################
category_stability <- anchor_long %>%
        group_by(GID, Category) %>%
        summarise(
                Mean_Percent = round(mean(Percent, na.rm = TRUE), 3),
                SD_Percent = round(sd(Percent, na.rm = TRUE), 3),
                Range_Percent = round(max(Percent, na.rm = TRUE) - min(Percent, na.rm = TRUE), 3),
                CV = ifelse(
                        Mean_Percent > 0,
                        round(SD_Percent / Mean_Percent, 3),
                        NA_real_
                ),
                N_Repeats = n(),
                .groups = "drop"
        ) %>%
        arrange(GID, desc(Mean_Percent))

write_csv(
        category_stability,
        file.path(output_dir, "Anchor_Category_Level_Stability_FIXED.csv")
)

############################################################
## 12. Export combined Excel workbook
############################################################
write_xlsx(
        list(
                Anchor_Percentage_Wide = anchor_pct,
                Anchor_Percentage_Long = anchor_long,
                BrayCurtis_Pairwise = bc_table,
                BrayCurtis_Summary = bc_summary,
                Category_Stability = category_stability
        ),
        path = file.path(output_dir, "Anchor_Analysis_v2_FIXED_Supplementary_Tables.xlsx")
)

############################################################
## 13. Done
############################################################
message("\nAnchor Analysis v2 FIXED complete.")
message("Anchor GIDs analyzed: ", paste(anchor_gids, collapse = ", "))
message("Anchor days analyzed: ", paste(anchor_days, collapse = ", "))
message("Outputs exported to: ", output_dir)

print(bc_summary)
