rm(list = ls())

############################################################
# Figure 7
# Correspondence Analysis of Light Mode Aroma Fingerprints
# 9 parental aroma categories
# Points colored by dominant aroma call
############################################################

############################################################
# 0. Packages
############################################################
required_packages <- c(
        "rstudioapi",
        "readxl",
        "tidyverse",
        "FactoMineR",
        "factoextra",
        "ggrepel",
        "grid"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
        if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
}

############################################################
# 1. Paths
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

output_dir <- file.path(input_dir, "OUTPUTS", "CORRESPONDENCE_ANALYSIS")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
# 2. Load data
############################################################
df <- read_excel(data_file, sheet = "Category_Percent_Wide")

############################################################
# 3. Metadata and 9 aroma categories
############################################################
meta <- df %>%
        select(any_of(c("Day", "LM", "LMCode", "GID")))

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

aroma <- df %>%
        select(all_of(aroma_cols)) %>%
        mutate(across(everything(), ~ replace_na(as.numeric(.x), 0))) %>%
        as.data.frame()

rownames(aroma) <- df$LM

############################################################
# 4. Highest-percentage category for display coloring only
############################################################
display_group <- aroma %>%
        rownames_to_column("LM") %>%
        pivot_longer(
                cols = -LM,
                names_to = "Category",
                values_to = "Percent"
        ) %>%
        group_by(LM) %>%
        arrange(factor(Category, levels = aroma_cols)) %>%
        slice_max(Percent, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(LM, Highest_Percentage_Category = Category)

############################################################
# 5. Correspondence Analysis
############################################################
ca_res <- CA(aroma, graph = FALSE)

eig <- get_eigenvalue(ca_res)

x_lab <- paste0("Dim1 (", round(eig[1, 2], 1), "%)")
y_lab <- paste0("Dim2 (", round(eig[2, 2], 1), "%)")

############################################################
# 6. Extract sample coordinates
############################################################
row_coords <- as.data.frame(ca_res$row$coord)
row_coords$LM <- rownames(row_coords)

row_coords <- row_coords %>%
        rename(
                CA1 = 1,
                CA2 = 2
        ) %>%
        left_join(meta, by = "LM") %>%
        left_join(display_group, by = "LM")

############################################################
# 7. Extract aroma-category coordinates
############################################################
col_coords <- as.data.frame(ca_res$col$coord)
col_coords$Category <- rownames(col_coords)

col_coords <- col_coords %>%
        rename(
                CA1 = 1,
                CA2 = 2
        )

############################################################
# 8. Plot CA
############################################################
set.seed(123)
p_ca <- ggplot() +
        geom_hline(
                yintercept = 0,
                linetype = "dashed",
                linewidth = 0.4
        ) +
        geom_vline(
                xintercept = 0,
                linetype = "dashed",
                linewidth = 0.4
        ) +
        geom_point(
                data = row_coords,
                aes(
                        x = CA1,
                        y = CA2,
                        color = Highest_Percentage_Category
                ),
                size = 3,
                alpha = 0.78
        ) +
        geom_point(
                data = col_coords,
                aes(
                        x = CA1,
                        y = CA2
                ),
                color = "black",
                size = 4,
                shape = 17
        ) +
        geom_text_repel(
                data = col_coords,
                aes(
                        x = CA1,
                        y = CA2,
                        label = Category
                ),
                color = "black",
                size = 4.5,
                fontface = "bold",
                max.overlaps = Inf,
                box.padding = 0.35,
                point.padding = 0.25,
                min.segment.length = 0
        ) +
        labs(
                title = NULL,
                subtitle = NULL,
                x = x_lab,
                y = y_lab,
                color = "Highest-percentage category"
        ) +
        theme_bw(base_size = 15) +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(linewidth = 0.25, color = "grey90"),
                axis.text = element_text(face = "bold", color = "black"),
                axis.title = element_text(face = "bold", size = 15),
                legend.position = "right",
                legend.title = element_text(face = "bold"),
                legend.text = element_text(size = 11),
                plot.margin = margin(10, 10, 10, 10)
        )

############################################################
# 9. Export figure and tables
############################################################
ggsave(
        filename = file.path(output_dir, "Figure_07_CA_LightMode_Aroma_Fingerprints_9Cat.pdf"),
        plot = p_ca,
        width = 12,
        height = 8,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Figure_07_CA_LightMode_Aroma_Fingerprints_9Cat.png"),
        plot = p_ca,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

write_csv(
        row_coords,
        file.path(output_dir, "Figure_07_CA_Sample_Coordinates_9Cat.csv")
)

write_csv(
        col_coords,
        file.path(output_dir, "Figure_07_CA_Aroma_Category_Coordinates_9Cat.csv")
)

write_csv(
        as.data.frame(eig),
        file.path(output_dir, "Figure_07_CA_Eigenvalues_9Cat.csv")
)

message("Done. Figure 7 CA outputs exported to: ", output_dir)
