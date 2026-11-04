rm(list = ls())

############################################################
## Figure 6
## Category-level time effect forest plot
## Mixed model: selected ~ timepoint + (1|panelist) + (1|LM)
############################################################

suppressPackageStartupMessages({
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(purrr)
        library(ggplot2)
        library(lme4)
        library(broom.mixed)
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
t1_raw_path <- file.path(validation_dir, "01_RAW_DATA", "Raw_Data_T1.xlsx")
t2_raw_path <- file.path(validation_dir, "01_RAW_DATA", "Raw_Data_T2.xlsx")
map_file <- file.path(validation_dir, "02_LM_TO_GID_MAPPING", "LM_to_GID_Master_Mapping.xlsx")
output_dir <- file.path(script_dir, "OUTPUTS", "04_MIXED_MODEL_FOREST_PLOT")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Helper functions
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

############################################################
## 3. Read raw Microsoft Forms data
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

raw_long <- bind_rows(t1_long, t2_long) %>%
        filter(Type == "Parental")

############################################################
## 4. Build selected category table
############################################################
votes <- raw_long %>%
        rowwise() %>%
        mutate(Category = list(parse_cata(Response))) %>%
        ungroup() %>%
        unnest(Category) %>%
        mutate(Category = str_squish(Category)) %>%
        filter(!is.na(Category), Category != "") %>%
        distinct(Timepoint, PanelistKey, LM, Category) %>%
        mutate(selected = 1L)

all_panelists <- sort(unique(raw_long$PanelistKey))
all_lm <- sort(unique(raw_long$LM))
all_categories <- sort(unique(votes$Category))

dat_full <- votes %>%
        complete(
                Timepoint = c("T1", "T2"),
                PanelistKey = all_panelists,
                LM = all_lm,
                Category = all_categories,
                fill = list(selected = 0L)
        ) %>%
        mutate(
                Timepoint = factor(Timepoint, levels = c("T1", "T2")),
                PanelistKey = factor(PanelistKey),
                LM = factor(LM),
                Category = factor(Category),
                selected = as.integer(selected)
        )

############################################################
## 5. Fit mixed-effects model per category
############################################################
fit_one_category <- function(cat_name) {
        
        df_cat <- dat_full %>%
                filter(Category == cat_name)
        
        if (length(unique(df_cat$selected)) < 2) {
                return(tibble(
                        category = cat_name,
                        estimate = NA_real_,
                        std.error = NA_real_,
                        statistic = NA_real_,
                        p.value = NA_real_,
                        odds_ratio = NA_real_,
                        conf.low = NA_real_,
                        conf.high = NA_real_,
                        model_status = "Skipped: no variation"
                ))
        }
        
        fit <- tryCatch(
                glmer(
                        selected ~ Timepoint + (1 | PanelistKey) + (1 | LM),
                        data = df_cat,
                        family = binomial,
                        control = glmerControl(
                                optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e5)
                        )
                ),
                error = function(e) NULL
        )
        
        if (is.null(fit)) {
                return(tibble(
                        category = cat_name,
                        estimate = NA_real_,
                        std.error = NA_real_,
                        statistic = NA_real_,
                        p.value = NA_real_,
                        odds_ratio = NA_real_,
                        conf.low = NA_real_,
                        conf.high = NA_real_,
                        model_status = "Model failed"
                ))
        }
        
        tidy_fit <- broom.mixed::tidy(fit, effects = "fixed") %>%
                filter(term == "TimepointT2")
        
        if (nrow(tidy_fit) == 0) {
                return(tibble(
                        category = cat_name,
                        estimate = NA_real_,
                        std.error = NA_real_,
                        statistic = NA_real_,
                        p.value = NA_real_,
                        odds_ratio = NA_real_,
                        conf.low = NA_real_,
                        conf.high = NA_real_,
                        model_status = "No TimepointT2 coefficient"
                ))
        }
        
        tidy_fit %>%
                transmute(
                        category = cat_name,
                        estimate = estimate,
                        std.error = std.error,
                        statistic = statistic,
                        p.value = p.value,
                        odds_ratio = exp(estimate),
                        conf.low = exp(estimate - 1.96 * std.error),
                        conf.high = exp(estimate + 1.96 * std.error),
                        model_status = "OK"
                )
}

forest_df <- map_dfr(all_categories, fit_one_category) %>%
        mutate(
                p_adj = p.adjust(p.value, method = "BH")
        ) %>%
        arrange(p_adj, p.value)

############################################################
## 6. Prepare plot table
############################################################
plot_df <- forest_df %>%
        filter(!is.na(odds_ratio), !is.na(conf.low), !is.na(conf.high)) %>%
        mutate(
                direction = if_else(odds_ratio >= 1, "Higher at T2", "Lower at T2"),
                ci_crosses_1 = !(conf.low > 1 | conf.high < 1),
                alpha_level = if_else(ci_crosses_1, 0.55, 1),
                label = paste0(
                        "OR=", sprintf("%.2f", odds_ratio),
                        "  (FDR=", sprintf("%.3f", p_adj), ")"
                ),
                label_pos = conf.high * 1.12,
                category_ord = factor(category, levels = rev(category))
        )

############################################################
## 7. Plot Figure 6
############################################################
p_forest <- ggplot(plot_df, aes(x = category_ord, y = odds_ratio)) +
        geom_hline(
                yintercept = 1,
                linetype = 2,
                linewidth = 0.7
        ) +
        geom_errorbar(
                aes(
                        ymin = conf.low,
                        ymax = conf.high,
                        color = direction,
                        alpha = alpha_level
                ),
                width = 0.2,
                linewidth = 1
        ) +
        geom_point(
                aes(
                        color = direction,
                        alpha = alpha_level
                ),
                size = 3.2
        ) +
        geom_text(
                aes(
                        y = label_pos,
                        label = label
                ),
                hjust = 0,
                size = 3.5,
                color = "black"
        ) +
        coord_flip() +
        scale_y_log10(
                expand = expansion(mult = c(0.05, 0.25))
        ) +
        scale_alpha_identity() +
        labs(
                title = NULL,
                subtitle = NULL,
                x = "Parental aroma category",
                y = "Odds ratio (T2 vs T1, log scale)",
                color = "Direction"
        ) +
        theme_minimal(base_size = 16) +
        theme(
                panel.grid.minor = element_blank(),
                axis.text.y = element_text(face = "bold", size = 15),
                axis.text.x = element_text(face = "bold", size = 14),
                axis.title.x = element_text(face = "bold", size = 17),
                axis.title.y = element_text(face = "bold", size = 17),
                legend.position = "right",
                legend.title = element_text(face = "bold"),
                plot.margin = margin(10, 40, 10, 10)
        )

############################################################
## 8. Export outputs
############################################################
ggsave(
        filename = file.path(output_dir, "Figure_06_ForestPlot_MixedModel_OR.png"),
        plot = p_forest,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
)

ggsave(
        filename = file.path(output_dir, "Figure_06_ForestPlot_MixedModel_OR.pdf"),
        plot = p_forest,
        width = 12,
        height = 8,
        bg = "white",
        limitsize = FALSE
)

write_xlsx(
        list(
                ForestPlot_MixedModel_OR = forest_df,
                Binary_Model_Input = dat_full
        ),
        path = file.path(output_dir, "Figure_06_ForestPlot_MixedModel_OR_Tables.xlsx")
)

write.csv(
        forest_df,
        file.path(output_dir, "Forest_plot.xlsx_input_equivalent.csv"),
        row.names = FALSE
)

message("Done.")
message("Figure 6 forest plot exported to: ", output_dir)
message("Categories modeled: ", paste(all_categories, collapse = ", "))
