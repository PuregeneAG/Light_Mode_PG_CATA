rm(list = ls())

############################################################
## Figure 5 + Table 2
## Panelist repeatability + entropy/Jaccard agreement table
############################################################

suppressPackageStartupMessages({
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(ggplot2)
        library(forcats)
        library(writexl)
        library(patchwork)
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
t1_raw_path <- file.path(validation_dir, "01_RAW_DATA", "Raw_Data_T1.xlsx")
t2_raw_path <- file.path(validation_dir, "01_RAW_DATA", "Raw_Data_T2.xlsx")
map_file <- file.path(validation_dir, "02_LM_TO_GID_MAPPING", "LM_to_GID_Master_Mapping.xlsx")
output_dir <- file.path(script_dir, "OUTPUTS", "03_JACCARD_ENTROPY")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Helpers
############################################################
parse_cata <- function(x) {
        if (length(x) != 1) x <- x[1]
        if (is.na(x)) return(character(0))
        
        x <- str_replace_all(as.character(x), "\u00A0", " ")
        x <- str_squish(x)
        
        if (x == "") return(character(0))
        if (tolower(x) %in% c("none", "na", "n/a")) return(character(0))
        
        v <- str_split(x, ";")[[1]] |> str_trim()
        v <- v[v != ""]
        v <- v[!tolower(v) %in% c("none", "na", "n/a")]
        
        unique(v)
}

q_type <- function(colname) {
        cn <- str_to_lower(colname)
        if (str_detect(cn, "ammonia")) return("Ammonia")
        if (str_detect(cn, "optional")) return("Optional")
        return("Parental")
}

extract_lm <- function(colname) {
        str_extract(colname, "LM_\\d+")
}

shannon_entropy <- function(p) {
        p <- p[!is.na(p) & p > 0]
        if (length(p) == 0) return(NA_real_)
        -sum(p * log2(p))
}

jaccard <- function(a, b) {
        a <- unique(a)
        b <- unique(b)
        inter <- length(intersect(a, b))
        uni <- length(union(a, b))
        if (uni == 0) return(NA_real_)
        inter / uni
}

mean_pairwise_jaccard <- function(sets_list) {
        n <- length(sets_list)
        if (n < 2) return(NA_real_)
        
        pairs <- combn(seq_len(n), 2)
        vals <- apply(
                pairs,
                2,
                function(k) jaccard(sets_list[[k[1]]], sets_list[[k[2]]])
        )
        
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) return(NA_real_)
        mean(vals)
}

collapse_set <- function(x) {
        if (is.null(x) || length(x) == 0) return("")
        paste(sort(unique(x)), collapse = "; ")
}

############################################################
## 3. Read LM mapping
############################################################
lm_map_raw <- read_excel(map_file)

col_lmcode <- names(lm_map_raw)[str_to_lower(names(lm_map_raw)) %in% c("lm_code", "lmcode", "lm code")]
col_gid <- names(lm_map_raw)[str_to_lower(names(lm_map_raw)) %in% c("gid")]

if (length(col_lmcode) != 1 || length(col_gid) != 1) {
        stop("Mapping file must contain LMCode/LM_Code and GID columns. Found: ",
             paste(names(lm_map_raw), collapse = ", "))
}

lm_map <- lm_map_raw %>%
        transmute(
                LMCode = as.character(.data[[col_lmcode]]),
                GID = as.character(.data[[col_gid]]),
                LM = paste0("LM_", LMCode)
        ) %>%
        distinct(LM, LMCode, GID)

############################################################
## 4. Read raw Microsoft Forms data
############################################################
read_and_long <- function(path, timepoint_label) {
        raw <- read_excel(path)
        
        if (!("Your name" %in% names(raw))) {
                stop("Missing column 'Your name' in: ", path)
        }
        
        lm_cols <- names(raw)[grepl("LM_\\d+", names(raw))]
        
        if (length(lm_cols) == 0) {
                stop("No LM_### columns detected in: ", path)
        }
        
        raw %>%
                rename(PanelistName = `Your name`) %>%
                mutate(
                        Timepoint = timepoint_label,
                        PanelistKey = str_to_lower(str_squish(as.character(PanelistName)))
                ) %>%
                select(Timepoint, PanelistName, PanelistKey, all_of(lm_cols)) %>%
                pivot_longer(
                        cols = all_of(lm_cols),
                        names_to = "Question",
                        values_to = "Response"
                ) %>%
                mutate(
                        LM = extract_lm(Question),
                        Type = vapply(Question, q_type, character(1)),
                        Response = as.character(Response)
                ) %>%
                filter(!is.na(LM))
}

t1_long <- read_and_long(t1_raw_path, "T1")
t2_long <- read_and_long(t2_raw_path, "T2")

parental_long <- bind_rows(t1_long, t2_long) %>%
        filter(Type == "Parental")

############################################################
## 5. Build panelist category sets
############################################################
panel_sets <- parental_long %>%
        rowwise() %>%
        mutate(Set = list(parse_cata(Response))) %>%
        ungroup() %>%
        select(Timepoint, LM, PanelistKey, Set)

panel_size <- panel_sets %>%
        group_by(Timepoint, LM) %>%
        summarise(
                PanelSize = n_distinct(PanelistKey),
                .groups = "drop"
        )

############################################################
## 6. Entropy + mean pairwise Jaccard table
############################################################
votes_expanded <- panel_sets %>%
        unnest(Set, keep_empty = TRUE) %>%
        rename(Category = Set) %>%
        mutate(Category = str_squish(as.character(Category))) %>%
        filter(!is.na(Category), Category != "")

freq <- votes_expanded %>%
        count(Timepoint, LM, Category, name = "Votes") %>%
        left_join(panel_size, by = c("Timepoint", "LM")) %>%
        group_by(Timepoint, LM) %>%
        mutate(
                PanelSelectionRate = Votes / PanelSize,
                # Normalize category counts within each sample-session before
                # calculating Shannon entropy. PanelSelectionRate remains the
                # CATA percentage-scale quantity.
                Prop = Votes / sum(Votes)
        ) %>%
        ungroup()

entropy_tbl <- freq %>%
        group_by(Timepoint, LM) %>%
        summarise(
                Entropy = shannon_entropy(Prop),
                .groups = "drop"
        )

mean_pairwise_tbl <- panel_sets %>%
        group_by(Timepoint, LM) %>%
        summarise(
                MeanPairwiseJaccard = mean_pairwise_jaccard(Set),
                .groups = "drop"
        )

agreement_tbl <- entropy_tbl %>%
        full_join(mean_pairwise_tbl, by = c("Timepoint", "LM")) %>%
        left_join(panel_size, by = c("Timepoint", "LM")) %>%
        left_join(lm_map, by = "LM") %>%
        select(
                Timepoint,
                LM,
                LMCode,
                GID,
                PanelSize,
                Entropy,
                MeanPairwiseJaccard
        )

thr_entropy <- median(agreement_tbl$Entropy, na.rm = TRUE)
thr_jaccard <- median(agreement_tbl$MeanPairwiseJaccard, na.rm = TRUE)

agreement_tbl <- agreement_tbl %>%
        mutate(
                AgreementClass = case_when(
                        Entropy <= thr_entropy & MeanPairwiseJaccard >= thr_jaccard ~ "High",
                        Entropy > thr_entropy & MeanPairwiseJaccard < thr_jaccard ~ "Low",
                        TRUE ~ "Intermediate"
                )
        ) %>%
        arrange(Timepoint, AgreementClass, LM)

############################################################
## 7. Panelist-level Jaccard by LM, T1 vs T2
############################################################
panel_pair <- panel_sets %>%
        pivot_wider(
                names_from = Timepoint,
                values_from = Set
        ) %>%
        mutate(
                Jaccard = mapply(
                        function(a, b) {
                                if (is.null(a) || is.null(b)) return(NA_real_)
                                jaccard(a, b)
                        },
                        T1, T2
                )
        ) %>%
        left_join(lm_map, by = "LM") %>%
        mutate(
                LMLabel = paste0("GID ", GID, " (", LM, ")"),
                T1_Set = vapply(T1, collapse_set, character(1)),
                T2_Set = vapply(T2, collapse_set, character(1))
        )

panel_pair_export <- panel_pair %>%
        select(
                PanelistKey,
                LM,
                LMCode,
                GID,
                LMLabel,
                T1_Set,
                T2_Set,
                Jaccard
        )

############################################################
## 8. Panel A: repeatability by sample
############################################################
lm_medians <- panel_pair %>%
        group_by(LMLabel) %>%
        summarise(
                Med = median(Jaccard, na.rm = TRUE),
                .groups = "drop"
        )

panel_pair_col <- panel_pair %>%
        left_join(lm_medians, by = "LMLabel")

p_panel_a <- ggplot(
        panel_pair_col,
        aes(
                x = fct_reorder(LMLabel, Jaccard, .fun = median, na.rm = TRUE),
                y = Jaccard,
                fill = Med
        )
) +
        geom_boxplot(
                outlier.shape = 16,
                outlier.size = 2,
                color = "black"
        ) +
        geom_point(
                data = lm_medians,
                aes(
                        x = fct_reorder(LMLabel, Med),
                        y = Med
                ),
                inherit.aes = FALSE,
                size = 2.7,
                color = "black"
        ) +
        geom_text(
                data = lm_medians,
                aes(
                        x = fct_reorder(LMLabel, Med),
                        y = Med,
                        label = sprintf("%.2f", Med)
                ),
                inherit.aes = FALSE,
                hjust = -0.25,
                size = 4,
                fontface = "bold",
                color = "black"
        ) +
        coord_flip() +
        scale_fill_gradient2(
                low = "#D32F2F",
                mid = "#FDD835",
                high = "#388E3C",
                midpoint = 0.5,
                limits = c(0, 1),
                name = "Median\nJaccard"
        ) +
        scale_y_continuous(
                limits = c(0, 1),
                breaks = seq(0, 1, by = 0.2),
                expand = expansion(mult = c(0.02, 0.12))
        ) +
        labs(
                title = "A. Panelist repeatability by sample",
                subtitle = "Distribution of within-panelist Jaccard similarity between T1 and T2.",
                x = NULL,
                y = "Jaccard similarity"
        ) +
        theme_minimal(base_size = 14) +
        theme(
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 17),
                plot.subtitle = element_text(size = 11),
                axis.text.y = element_text(face = "bold", size = 13),
                axis.text.x = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold", size = 14),
                legend.title = element_text(face = "bold", size = 11),
                legend.text = element_text(size = 10),
                plot.margin = margin(5, 20, 5, 10)
        )

############################################################
## 9. Panel B: repeatability by panelist
############################################################
panelist_map <- panel_pair_export %>%
        distinct(PanelistKey) %>%
        arrange(PanelistKey) %>%
        mutate(PanelistBlind = PanelistKey)

jac_panelist <- panel_pair_export %>%
        left_join(panelist_map, by = "PanelistKey")

lm_median_jaccard <- jac_panelist %>%
        group_by(LM) %>%
        summarise(
                LM_Median_Jaccard = median(Jaccard, na.rm = TRUE),
                .groups = "drop"
        )

panelist_summary <- jac_panelist %>%
        left_join(lm_median_jaccard, by = "LM") %>%
        mutate(
                Residual = Jaccard - LM_Median_Jaccard,
                GE_0_5 = Jaccard >= 0.5
        ) %>%
        group_by(PanelistBlind) %>%
        summarise(
                N_Samples = n(),
                Mean_Jaccard = mean(Jaccard, na.rm = TRUE),
                Median_Jaccard = median(Jaccard, na.rm = TRUE),
                Percent_GE_0_5 = mean(GE_0_5, na.rm = TRUE),
                Sample_Adjusted_Repeatability = mean(Residual, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        arrange(desc(Sample_Adjusted_Repeatability), desc(Median_Jaccard)) %>%
        mutate(PanelistOrd = reorder(PanelistBlind, Median_Jaccard))

p_panel_b <- ggplot(
        panelist_summary,
        aes(
                x = PanelistOrd,
                y = Median_Jaccard,
                fill = Sample_Adjusted_Repeatability
        )
) +
        geom_col() +
        coord_flip() +
        geom_text(
                aes(label = sprintf("%.2f", Median_Jaccard)),
                hjust = -0.10,
                size = 4,
                fontface = "bold",
                color = "black"
        ) +
        scale_y_continuous(
                limits = c(0, 1),
                breaks = seq(0, 0.9, by = 0.3),
                expand = expansion(mult = c(0.02, 0.15))
        ) +
        scale_fill_gradient2(
                low = "#D32F2F",
                mid = "white",
                high = "#388E3C",
                midpoint = 0,
                name = "Sample-adjusted\nrepeatability"
        ) +
        labs(
                title = "B. Panelist repeatability across samples",
                subtitle = "Bar height = median self-consistency; color = performance relative to sample difficulty.",
                x = NULL,
                y = "Median Jaccard"
        ) +
        theme_minimal(base_size = 14) +
        theme(
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 17),
                plot.subtitle = element_text(size = 11),
                axis.text.y = element_text(face = "bold", size = 12),
                axis.text.x = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold", size = 14),
                legend.title = element_text(face = "bold", size = 11),
                legend.text = element_text(size = 10),
                plot.margin = margin(5, 20, 5, 10)
        )

############################################################
## 10. Combine Figure 5
############################################################
fig5 <- p_panel_a / p_panel_b +
        plot_layout(heights = c(1.55, 1)) +
        plot_annotation(
                title = "Panelist repeatability across samples and individuals",
                theme = theme(
                        plot.title = element_text(
                                face = "bold",
                                size = 20,
                                hjust = 0.5
                        )
                )
        )

############################################################
## 11. Export figure and tables
############################################################
ggsave(
        filename = file.path(output_dir, "Figure_03_Within_Panelist_Jaccard_by_Sample.pdf"),
        plot = p_panel_a,
        width = 12,
        height = 8,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Figure_03_Within_Panelist_Jaccard_by_Sample.png"),
        plot = p_panel_a,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Supplementary_Figure_S5_Panelist_Repeatability.pdf"),
        plot = p_panel_b,
        width = 10,
        height = 6,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Supplementary_Figure_S5_Panelist_Repeatability.png"),
        plot = p_panel_b,
        width = 10,
        height = 6,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Figure_05.pdf"),
        plot = fig5,
        width = 13,
        height = 15,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Figure_05.png"),
        plot = fig5,
        width = 13,
        height = 15,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

write_xlsx(
        list(
                Table2_Entropy_Jaccard = agreement_tbl,
                Thresholds = tibble(
                        Entropy_Median_Threshold = thr_entropy,
                        Jaccard_Median_Threshold = thr_jaccard
                )
        ),
        path = file.path(output_dir, "Table2_WithinSession_Entropy_Jaccard.xlsx")
)

write_xlsx(
        list(
                Fig5_Jaccard_ByPanelistLM = panel_pair_export,
                Fig5_LM_Medians = lm_medians,
                Fig5_Panelist_Map = panelist_map,
                Fig5_Panelist_Summary = panelist_summary,
                Vote_Frequencies = freq
        ),
        path = file.path(output_dir, "Fig5_Jaccard_Tables.xlsx")
)

write.csv(
        agreement_tbl,
        file.path(output_dir, "Table2_WithinSession_Entropy_Jaccard.csv"),
        row.names = FALSE
)

write.csv(
        panel_pair_export,
        file.path(output_dir, "Fig5_Jaccard_ByPanelistLM.csv"),
        row.names = FALSE
)

write.csv(
        panelist_summary,
        file.path(output_dir, "Fig5_Panelist_Summary.csv"),
        row.names = FALSE
)

message("Done.")
message("Figure 5 PDF/PNG exported to: ", output_dir)
message("Entropy table exported to: ", output_dir)
