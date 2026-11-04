rm(list = ls())

############################################################
## 0. Libraries
############################################################
suppressPackageStartupMessages({
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(ggplot2)
        library(forcats)
        library(writexl)
})

############################################################
## 1. File paths
############################################################
get_script_dir <- function() {
        args <- commandArgs(trailingOnly = FALSE)
        file_arg <- grep("^--file=", args, value = TRUE)
        if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
        if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) return(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))
        normalizePath(getwd())
}
script_dir <- get_script_dir()
validation_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
t1_path <- file.path(validation_dir, "04_PROCESSED_DATA", "T1_OUTPUTS", "Aroma_Category_Percentages.xlsx")
t2_path <- file.path(validation_dir, "04_PROCESSED_DATA", "T2_OUTPUTS", "Aroma_Category_Percentages.xlsx")
output_dir <- file.path(script_dir, "OUTPUTS", "01_BRAY_CURTIS")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Bray-Curtis similarity function
############################################################
bray_curtis_similarity <- function(p, q) {
        p <- as.numeric(p)
        q <- as.numeric(q)
        
        num <- sum(abs(p - q), na.rm = TRUE)
        den <- sum(p + q, na.rm = TRUE)
        
        if (den == 0) return(NA_real_)
        
        1 - (num / den)
}

############################################################
## 3. Read percentage data
############################################################
t1 <- read_excel(t1_path, sheet = "Category_Percent_Wide")
t2 <- read_excel(t2_path, sheet = "Category_Percent_Wide")

id_cols <- c("LM", "LMCode", "GID")

missing_t1 <- setdiff(id_cols, names(t1))
missing_t2 <- setdiff(id_cols, names(t2))

if (length(missing_t1) > 0) stop("T1 missing ID columns: ", paste(missing_t1, collapse = ", "))
if (length(missing_t2) > 0) stop("T2 missing ID columns: ", paste(missing_t2, collapse = ", "))

category_cols <- setdiff(intersect(names(t1), names(t2)), id_cols)

if (length(category_cols) == 0) {
        stop("No shared aroma category columns found between T1 and T2.")
}

############################################################
## 4. Join T1 and T2
############################################################
joined <- t1 %>%
        select(all_of(id_cols), all_of(category_cols)) %>%
        inner_join(
                t2 %>% select(all_of(id_cols), all_of(category_cols)),
                by = id_cols,
                suffix = c("_T1", "_T2")
        )

if (nrow(joined) == 0) {
        stop("No matching LM/GID rows found between T1 and T2.")
}

############################################################
## 5. Calculate Bray-Curtis similarity per sample
############################################################
bc_by_lm <- joined %>%
        rowwise() %>%
        mutate(
                BrayCurtisSim = bray_curtis_similarity(
                        c_across(paste0(category_cols, "_T1")),
                        c_across(paste0(category_cols, "_T2"))
                ),
                LMLabel = paste0("GID ", GID, " (", LM, ")")
        ) %>%
        ungroup() %>%
        select(LM, LMCode, GID, LMLabel, BrayCurtisSim) %>%
        arrange(desc(BrayCurtisSim))

bc_mean <- mean(bc_by_lm$BrayCurtisSim, na.rm = TRUE)
bc_median <- median(bc_by_lm$BrayCurtisSim, na.rm = TRUE)

############################################################
## 6. Plot Figure 3 — clean manuscript version
############################################################
p_bc <- ggplot(
        bc_by_lm,
        aes(
                x = fct_reorder(LMLabel, BrayCurtisSim),
                y = BrayCurtisSim
        )
) +
        geom_col(fill = "#1565C0", width = 0.78) +
        geom_hline(
                yintercept = bc_mean,
                linetype = "dashed",
                linewidth = 0.7
        ) +
        geom_hline(
                yintercept = bc_median,
                linetype = "dotted",
                linewidth = 0.9
        ) +
        geom_text(
                aes(label = sprintf("%.2f", BrayCurtisSim)),
                hjust = -0.08,
                size = 4.5,
                fontface = "bold",
                color = "black"
        ) +
        coord_flip() +
        scale_y_continuous(
                limits = c(0, 1.05),
                breaks = seq(0, 1, by = 0.2),
                expand = expansion(mult = c(0.01, 0.05))
        ) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Sample",
                y = "Bray-Curtis similarity"
        ) +
        theme_minimal(base_size = 14) +
        theme(
                axis.text.y = element_text(face = "bold", size = 14),
                axis.text.x = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold", size = 15),
                axis.title.y = element_text(face = "bold", size = 15),
                panel.grid.minor = element_blank(),
                plot.margin = margin(10, 20, 10, 10)
        )

############################################################
## 7. Export figure and table
############################################################
ggsave(
        filename = file.path(output_dir, "Fig3_BrayCurtis_StructuralSimilarity_T1_vs_T2_clean.png"),
        plot = p_bc,
        width = 12,
        height = 8.5,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Fig3_BrayCurtis_StructuralSimilarity_T1_vs_T2_clean.pdf"),
        plot = p_bc,
        width = 12,
        height = 8.5,
        bg = "white",
        limitsize = FALSE
)

write_xlsx(
        list(
                BrayCurtis_BySample = bc_by_lm,
                Category_Columns_Used = tibble(Category = category_cols),
                Summary = tibble(
                        Mean_BrayCurtis = bc_mean,
                        Median_BrayCurtis = bc_median
                )
        ),
        path = file.path(output_dir, "Fig3_BrayCurtis_StructuralSimilarity_Tables.xlsx")
)

write.csv(
        bc_by_lm,
        file.path(output_dir, "Fig3_BrayCurtis_BySample.csv"),
        row.names = FALSE
)

message("Done. Clean Figure 3 exported to: ", output_dir)
message("Mean Bray-Curtis similarity: ", sprintf("%.3f", bc_mean))
message("Median Bray-Curtis similarity: ", sprintf("%.3f", bc_median))
