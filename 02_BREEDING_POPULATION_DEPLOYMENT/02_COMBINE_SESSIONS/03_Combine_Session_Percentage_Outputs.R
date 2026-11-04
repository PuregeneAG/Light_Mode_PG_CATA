rm(list = ls())

suppressPackageStartupMessages({
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(writexl)
})

get_script_dir <- function() {
        args <- commandArgs(trailingOnly = FALSE)
        file_arg <- grep("^--file=", args, value = TRUE)
        if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
        normalizePath(getwd())
}

combined_dir <- get_script_dir()
deployment_dir <- normalizePath(file.path(combined_dir, ".."), mustWork = TRUE)
session_root <- file.path(deployment_dir, "01_SESSION_INPUTS")
output_file <- file.path(combined_dir, "Pct_wide_All_Days.xlsx")

categories <- c(
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

session_dirs <- sort(list.dirs(session_root, recursive = FALSE, full.names = TRUE))
if (length(session_dirs) != 9) stop("Expected nine deployment session folders; found ", length(session_dirs), ".")

read_session <- function(session_dir) {
        input <- file.path(session_dir, "OUTPUTS", "Aroma_Category_Percentages.xlsx")
        if (!file.exists(input)) stop("Missing session percentage output: ", input)
        day <- basename(session_dir)
        x <- read_excel(input)

        unexpected <- setdiff(unique(x$Category), categories)
        if (length(unexpected) > 0) {
                stop("Unexpected aroma categories in ", day, ": ", paste(unexpected, collapse = ", "))
        }

        x %>%
                select(LM, LMCode, GID, Category, Percent) %>%
                mutate(Category = factor(Category, levels = categories)) %>%
                complete(nesting(LM, LMCode, GID), Category, fill = list(Percent = 0)) %>%
                pivot_wider(names_from = Category, values_from = Percent) %>%
                mutate(Day = day, .before = 1) %>%
                select(Day, LM, LMCode, GID, all_of(categories))
}

combined <- bind_rows(lapply(session_dirs, read_session)) %>%
        arrange(factor(Day, levels = basename(session_dirs)), LM)

if (nrow(combined) != 86) stop("Expected 86 deployment evaluations; found ", nrow(combined), ".")
if (n_distinct(combined$GID) != 50) stop("Expected 50 unique GIDs; found ", n_distinct(combined$GID), ".")
if (any(combined[categories] < 0 | combined[categories] > 100, na.rm = TRUE)) {
        stop("All category percentages must be between 0 and 100.")
}

write_xlsx(list(Category_Percent_Wide = combined), output_file)
message("Combined nine sessions into: ", output_file)
message("Rows: ", nrow(combined), "; unique GIDs: ", n_distinct(combined$GID))
