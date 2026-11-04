rm(list = ls())

############################################################
## Figure 6 / Table X
## Category-level timepoint effects using 9-category framework
## Mixed-effects logistic model:
## selected ~ Timepoint + (1 | PanelistKey) + (1 | LM)
############################################################

suppressPackageStartupMessages({
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(purrr)
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
output_dir <- file.path(script_dir, "OUTPUTS", "05_MIXED_MODEL_TABLE")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################
## 2. Final 9 parental aroma categories
############################################################
parental_categories <- c(
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

############################################################
## 3. Helper functions
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

raw_long <- bind_rows(t1_long, t2_long) %>%
        filter(Type == "Parental")

############################################################
## 5. Build binary selected/not-selected matrix
############################################################
votes <- raw_long %>%
        rowwise() %>%
        mutate(Category = list(parse_cata(Response))) %>%
        ungroup() %>%
        unnest(Category) %>%
        mutate(Category = str_squish(Category)) %>%
        filter(!is.na(Category), Category != "") %>%
        filter(Category %in% parental_categories) %>%
        distinct(Timepoint, PanelistKey, LM, Category) %>%
        mutate(selected = 1L)

all_panelists <- sort(unique(raw_long$PanelistKey))
all_lm <- sort(unique(raw_long$LM))

dat_full <- votes %>%
        complete(
                Timepoint = c("T1", "T2"),
                PanelistKey = all_panelists,
                LM = all_lm,
                Category = parental_categories,
                fill = list(selected = 0L)
        ) %>%
        mutate(
                Timepoint = factor(Timepoint, levels = c("T1", "T2")),
                PanelistKey = factor(PanelistKey),
                LM = factor(LM),
                Category = factor(Category, levels = parental_categories),
                selected = as.integer(selected)
        )

############################################################
## 6. Fit one mixed-effects logistic model per category
############################################################
fit_one_category <- function(cat_name) {
        
        df_cat <- dat_full %>%
                filter(Category == cat_name)
        
        if (length(unique(df_cat$selected)) < 2) {
                return(tibble(
                        `Parental Aroma Category` = cat_name,
                        Estimate = NA_real_,
                        `Std. Error` = NA_real_,
                        Statistic = NA_real_,
                        `p-value` = NA_real_,
                        OR = NA_real_,
                        `CI Low` = NA_real_,
                        `CI High` = NA_real_,
                        `q-value (BH-FDR)` = NA_real_,
                        Model_Status = "Skipped: no variation"
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
                        `Parental Aroma Category` = cat_name,
                        Estimate = NA_real_,
                        `Std. Error` = NA_real_,
                        Statistic = NA_real_,
                        `p-value` = NA_real_,
                        OR = NA_real_,
                        `CI Low` = NA_real_,
                        `CI High` = NA_real_,
                        `q-value (BH-FDR)` = NA_real_,
                        Model_Status = "Model failed"
                ))
        }
        
        tidy_fit <- broom.mixed::tidy(fit, effects = "fixed") %>%
                filter(term == "TimepointT2")
        
        if (nrow(tidy_fit) == 0) {
                return(tibble(
                        `Parental Aroma Category` = cat_name,
                        Estimate = NA_real_,
                        `Std. Error` = NA_real_,
                        Statistic = NA_real_,
                        `p-value` = NA_real_,
                        OR = NA_real_,
                        `CI Low` = NA_real_,
                        `CI High` = NA_real_,
                        `q-value (BH-FDR)` = NA_real_,
                        Model_Status = "No TimepointT2 coefficient"
                ))
        }
        
        tidy_fit %>%
                transmute(
                        `Parental Aroma Category` = cat_name,
                        Estimate = estimate,
                        `Std. Error` = std.error,
                        Statistic = statistic,
                        `p-value` = p.value,
                        OR = exp(estimate),
                        `CI Low` = exp(estimate - 1.96 * std.error),
                        `CI High` = exp(estimate + 1.96 * std.error),
                        `q-value (BH-FDR)` = NA_real_,
                        Model_Status = "OK"
                )
}

model_table <- map_dfr(parental_categories, fit_one_category)

############################################################
## 7. Add BH-FDR correction and formatted table
############################################################
model_table <- model_table %>%
        mutate(
                `q-value (BH-FDR)` = p.adjust(`p-value`, method = "BH")
        ) %>%
        arrange(`q-value (BH-FDR)`, `p-value`)

model_table_rounded <- model_table %>%
        mutate(
                Estimate = round(Estimate, 3),
                `Std. Error` = round(`Std. Error`, 3),
                Statistic = round(Statistic, 3),
                `p-value` = round(`p-value`, 3),
                OR = round(OR, 3),
                `CI Low` = round(`CI Low`, 3),
                `CI High` = round(`CI High`, 3),
                `q-value (BH-FDR)` = round(`q-value (BH-FDR)`, 3)
        )

############################################################
## 8. Export
############################################################
write_xlsx(
        list(
                Table3_MixedModel_CategoryEffects = model_table_rounded,
                Raw_Model_Output = model_table,
                Binary_Model_Input = dat_full
        ),
        path = file.path(output_dir, "Table3_Category_Timepoint_Effects.xlsx")
)

write.csv(
        model_table_rounded,
        file.path(output_dir, "Table3_Category_Timepoint_Effects.csv"),
        row.names = FALSE
)

message("Done.")
message("Table X exported to: ", output_dir)
message("Categories modeled: ", paste(parental_categories, collapse = ", "))
