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
output_dir <- file.path(script_dir, "OUTPUTS", "02_CATEGORY_CHANGES")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Read percentage data
############################################################
t1 <- read_excel(t1_path, sheet = "Category_Percent_Wide")
t2 <- read_excel(t2_path, sheet = "Category_Percent_Wide")

id_cols <- c("LM", "LMCode", "GID")

missing_t1 <- setdiff(id_cols, names(t1))
missing_t2 <- setdiff(id_cols, names(t2))

if (length(missing_t1) > 0) {
        stop("T1 missing ID columns: ", paste(missing_t1, collapse = ", "))
}

if (length(missing_t2) > 0) {
        stop("T2 missing ID columns: ", paste(missing_t2, collapse = ", "))
}

category_cols <- setdiff(intersect(names(t1), names(t2)), id_cols)

if (length(category_cols) == 0) {
        stop("No shared aroma category columns found between T1 and T2.")
}

############################################################
## 3. Convert to long format
############################################################
t1_long <- t1 %>%
        select(all_of(id_cols), all_of(category_cols)) %>%
        pivot_longer(
                cols = all_of(category_cols),
                names_to = "Category",
                values_to = "T1"
        )

t2_long <- t2 %>%
        select(all_of(id_cols), all_of(category_cols)) %>%
        pivot_longer(
                cols = all_of(category_cols),
                names_to = "Category",
                values_to = "T2"
        )

delta_df <- t1_long %>%
        inner_join(t2_long, by = c(id_cols, "Category")) %>%
        mutate(
                T1 = replace_na(as.numeric(T1), 0),
                T2 = replace_na(as.numeric(T2), 0)
        )

############################################################
## 4. Convert percent units to proportions if needed
##    If values are 10, 20, 30 -> convert to 0.10, 0.20, 0.30
############################################################
max_val <- max(abs(c(delta_df$T1, delta_df$T2)), na.rm = TRUE)

if (max_val > 1) {
        delta_df <- delta_df %>%
                mutate(
                        T1 = T1 / 100,
                        T2 = T2 / 100
                )
}

delta_df <- delta_df %>%
        mutate(
                Delta = T2 - T1,
                SampleLabel = paste0("GID ", GID, " (", LM, ")"),
                DeltaLabel = ifelse(
                        abs(Delta) < 0.10,
                        "",
                        paste0(ifelse(Delta > 0, "+", ""), round(Delta * 100), "%")
                )
        )

############################################################
## 5. Order samples and categories like old L2A heatmap
############################################################
sample_order <- delta_df %>%
        group_by(SampleLabel) %>%
        summarise(
                TotalAbsChange = sum(abs(Delta), na.rm = TRUE),
                .groups = "drop"
        ) %>%
        arrange(desc(TotalAbsChange)) %>%
        pull(SampleLabel)

delta_df <- delta_df %>%
        mutate(
                SampleLabel = factor(SampleLabel, levels = rev(sample_order)),
                Category = factor(Category, levels = category_cols)
        )

############################################################
## 6. Plot Figure 4 heatmap
############################################################
p_heat <- ggplot(delta_df, aes(x = Category, y = SampleLabel, fill = Delta)) +
        geom_tile(
                color = "grey65",
                linewidth = 0.35
        ) +
        geom_text(
                aes(label = DeltaLabel),
                size = 4,
                fontface = "bold",
                color = "black"
        ) +
        scale_fill_gradient2(
                low = "#B71C1C",
                mid = "white",
                high = "#1B5E20",
                midpoint = 0,
                limits = c(-0.4, 0.3),
                breaks = c(-0.4, -0.2, 0, 0.2),
                name = expression(Delta~proportion)
        ) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Parental aroma category",
                y = "LM (GID)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
                panel.grid = element_blank(),
                
                axis.text.x = element_text(
                        angle = 45,
                        hjust = 1,
                        face = "bold",
                        size = 12
                ),
                
                axis.text.y = element_text(
                        face = "bold",
                        size = 15
                ),
                
                axis.title.x = element_text(
                        face = "bold",
                        size = 15
                ),
                
                axis.title.y = element_text(
                        face = "bold",
                        size = 15
                ),
                
                legend.title = element_text(
                        face = "bold",
                        size = 12
                ),
                
                legend.text = element_text(size = 11),
                
                plot.margin = margin(10, 20, 10, 10)
        )

############################################################
## 7. Export figure and tables
############################################################
ggsave(
        filename = file.path(output_dir, "Fig4_DeltaProportion_Heatmap_T2_minus_T1_clean.png"),
        plot = p_heat,
        width = 15,
        height = 10,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Fig4_DeltaProportion_Heatmap_T2_minus_T1_clean.pdf"),
        plot = p_heat,
        width = 15,
        height = 10,
        bg = "white",
        limitsize = FALSE
)

delta_export <- delta_df %>%
        mutate(
                T1_percent = round(T1 * 100, 1),
                T2_percent = round(T2 * 100, 1),
                Delta_percent = round(Delta * 100, 1)
        ) %>%
        select(
                LM, LMCode, GID, SampleLabel, Category,
                T1, T2, Delta,
                T1_percent, T2_percent, Delta_percent
        )

summary_export <- delta_df %>%
        group_by(SampleLabel) %>%
        summarise(
                TotalAbsChange = sum(abs(Delta), na.rm = TRUE),
                MaxAbsChange = max(abs(Delta), na.rm = TRUE),
                .groups = "drop"
        ) %>%
        arrange(desc(TotalAbsChange))

write_xlsx(
        list(
                Delta_BySampleCategory = delta_export,
                Sample_Change_Summary = summary_export,
                Category_Columns_Used = tibble(Category = category_cols)
        ),
        path = file.path(output_dir, "Fig4_DeltaProportion_Heatmap_Tables.xlsx")
)

write.csv(
        delta_export,
        file.path(output_dir, "Fig4_DeltaProportion_BySampleCategory.csv"),
        row.names = FALSE
)

message("Done. Clean Figure 4 heatmap exported to: ", output_dir)
