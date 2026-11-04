rm(list = ls())

############################################################
## 0. Packages
############################################################
required_packages <- c(
        "rstudioapi", "readxl", "dplyr", "tidyr",
        "forcats", "ggplot2", "ggtext", "scales"
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

output_dir <- file.path(input_dir, "OUTPUTS", "HEATMAP")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Load data
############################################################
raw <- read_excel(data_file, sheet = "Category_Percent_Wide")

cat_order <- c(
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

day_short <- c(
        "Day01_AM" = "D1am",
        "Day01_PM" = "D1pm",
        "Day02_AM" = "D2am",
        "Day03_AM" = "D3am",
        "Day04_AM" = "D4am",
        "Day05_AM" = "D5am",
        "Day06_AM" = "D6am",
        "Day07_AM" = "D7am",
        "Day08_AM" = "D8am",
        "Day08_PM" = "D8pm"
)

############################################################
## 3. Fix missing day labels
##    Publication y-axis: LM only
############################################################
raw <- raw %>%
        mutate(
                DayShort = ifelse(
                        Day %in% names(day_short),
                        day_short[Day],
                        as.character(Day)
                ),
                RowLabel = LM
        )

############################################################
## 4. Long format
############################################################
df_long <- raw %>%
        pivot_longer(
                cols = all_of(cat_order),
                names_to = "Category",
                values_to = "Percent"
        ) %>%
        mutate(
                Percent = replace_na(Percent, 0),
                Category = factor(Category, levels = cat_order)
        )

############################################################
## 5. Sort rows by highest-percentage category for display
############################################################
row_order <- raw %>%
        rowwise() %>%
        mutate(
                # which.max resolves ties by the fixed category order.
                DisplayCat = cat_order[which.max(c_across(all_of(cat_order)))],
                DisplayVal = max(c_across(all_of(cat_order)), na.rm = TRUE),
                DisplayRank = match(DisplayCat, cat_order)
        ) %>%
        ungroup() %>%
        arrange(DisplayRank, desc(DisplayVal), LM) %>%
        pull(RowLabel)

df_long <- df_long %>%
        mutate(RowLabel = factor(RowLabel, levels = rev(row_order)))

############################################################
## 6. Cell labels and color threshold
############################################################
df_long <- df_long %>%
        mutate(
                CellLabel = ifelse(Percent >= 20, as.character(round(Percent)), ""),
                PercentPlot = ifelse(Percent < 20, NA_real_, Percent)
        )

green_ramp <- colorRampPalette(
        c("#BFDD96", "#639922", "#3B6D11", "#173404")
)(81)

############################################################
## 7. Plot
############################################################
n_rows <- length(unique(df_long$RowLabel))
page_h_mm <- max(297, 297 * (n_rows / 60))
page_w_mm <- 175

p <- ggplot(df_long, aes(x = Category, y = RowLabel, fill = PercentPlot)) +
        geom_tile(color = "grey80", linewidth = 0.25) +
        geom_text(
                aes(
                        label = CellLabel,
                        color = ifelse(Percent >= 45, "white", "dark")
                ),
                size = 1.9,
                fontface = ifelse(df_long$Percent >= 50, "bold", "plain"),
                show.legend = FALSE
        ) +
        scale_fill_gradientn(
                colours = green_ramp,
                limits = c(20, 100),
                na.value = "white",
                name = "% panel\nselecting\ncategory",
                breaks = seq(20, 100, 20),
                guide = guide_colorbar(
                        barheight = unit(60, "mm"),
                        barwidth = unit(3, "mm"),
                        ticks = TRUE,
                        title.position = "top"
                )
        ) +
        scale_color_manual(values = c("white" = "white", "dark" = "#333333")) +
        scale_x_discrete(position = "top") +
        labs(
                title = NULL,
                x = NULL,
                y = NULL
        ) +
        theme_minimal(base_size = 7) +
        theme(
                plot.background = element_rect(fill = "white", color = NA),
                panel.background = element_rect(fill = "white", color = NA),
                panel.grid = element_blank(),
                
                axis.text.x.top = element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0,
                        size = 6.8,
                        color = "#222222",
                        face = "bold"
                ),
                
                axis.text.y = element_text(
                        size = 5.6,
                        color = "#444444",
                        hjust = 1
                ),
                
                legend.position = "right",
                legend.title = element_text(size = 5.5),
                legend.text = element_text(size = 5),
                
                plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "mm")
        )

############################################################
## 8. Export
############################################################
out_file <- file.path(output_dir, "PG_Aroma_Heatmap_AllDays_LM_only.pdf")

ggsave(
        filename = out_file,
        plot = p,
        width = page_w_mm,
        height = page_h_mm,
        units = "mm",
        device = "pdf"
)

message("Heatmap saved to: ", out_file)
