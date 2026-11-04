rm(list = ls())

############################################################
## 0. Libraries
############################################################
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(ggtext)
library(writexl)

############################################################
## 1. File paths
############################################################
get_script_dir <- function() {
        args <- commandArgs(trailingOnly = FALSE)
        file_arg <- grep("^--file=", args, value = TRUE)
        if (length(file_arg) > 0) {
                return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
        }
        if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
                return(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))
        }
        normalizePath(getwd())
}

script_dir <- get_script_dir()
validation_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
data_file <- file.path(validation_dir, "01_RAW_DATA", "Raw_Data_T1.xlsx")
map_file <- file.path(validation_dir, "02_LM_TO_GID_MAPPING", "LM_to_GID_Master_Mapping.xlsx")
output_dir <- file.path(validation_dir, "04_PROCESSED_DATA", "T1_OUTPUTS")
dir.create(output_dir, showWarnings = FALSE)

percent_plot_dir <- file.path(output_dir, "LM_Percent_Profile_Plots")
dir.create(percent_plot_dir, showWarnings = FALSE)

############################################################
## Helpers
############################################################
shorten_list <- function(x, max_items = 3) {
        if (is.na(x) || str_trim(x) == "") return("")
        parts <- str_split(x, ",\\s*")[[1]]
        if (length(parts) <= max_items) paste(parts, collapse = ", ")
        else paste0(paste(parts[1:max_items], collapse = ", "), ", ...")
}

############################################################
## 2. Read LM <-> GID mapping
## ASSUMES mapping file has column: LM_Code
############################################################
lm_map <- read_excel(map_file) %>%
        rename(LMCode = LM_Code) %>%
        mutate(
                LMCode = as.character(LMCode),
                LM     = paste0("LM_", LMCode)
        ) %>%
        select(LM, LMCode, GID)

############################################################
## 3. Read CATA dataset and reshape
## ASSUMES form columns contain LM_### names
############################################################
raw <- read_excel(data_file)

lm_cols <- names(raw)[grepl("LM_", names(raw))]

clean <- raw %>%
        rename(PanelistName = `Your name`) %>%
        mutate(PanelistID = paste0("P", row_number())) %>%
        select(PanelistID, PanelistName, all_of(lm_cols)) %>%
        pivot_longer(
                cols      = -c(PanelistID, PanelistName),
                names_to  = "Question",
                values_to = "Response"
        ) %>%
        mutate(
                LM   = str_extract(Question, "LM_\\d+"),
                Type = case_when(
                        str_detect(Question, "Ammonia")            ~ "Ammonia",
                        str_detect(tolower(Question), "optional")  ~ "Optional",
                        TRUE                                       ~ "Parental"
                )
        )

############################################################
## 4. Expand parental selections (split on ;)
############################################################
parental <- clean %>%
        filter(Type == "Parental", !is.na(Response)) %>%
        mutate(Category = str_split(Response, ";")) %>%
        unnest(Category) %>%
        mutate(Category = str_trim(Category)) %>%
        filter(Category != "") %>%
        # CATA is binary at participant-sample-category level. Repeated text
        # within one exported response must not contribute an additional vote.
        distinct(PanelistID, LM, Category, .keep_all = TRUE)

panel_size <- parental %>%
        group_by(LM) %>%
        summarise(PanelSize = n_distinct(PanelistID), .groups = "drop")

freq <- parental %>%
        count(LM, Category, name = "Votes")

############################################################
## 5. Dominant / Secondary + tie logic
############################################################
consensus <- freq %>%
        group_by(LM) %>%
        summarise(
                DomVotes = max(Votes),
                DominantSet = paste(Category[Votes == DomVotes], collapse = ", "),
                Nmax = sum(Votes == DomVotes),
                
                SecVotes = ifelse(
                        any(Votes < DomVotes & Votes >= 2),
                        max(Votes[Votes < DomVotes & Votes >= 2]),
                        NA_integer_
                ),
                Nsec = ifelse(!is.na(SecVotes), sum(Votes == SecVotes & Votes < DomVotes), 0L),
                SecondarySet = ifelse(is.na(SecVotes), "", paste(Category[Votes == SecVotes], collapse = ", ")),
                .groups = "drop"
        ) %>%
        mutate(
                HasTieTop = Nmax >= 2,
                DominantCall = ifelse(HasTieTop, "None", DominantSet)
        )

############################################################
## 6. Ammonia QC
############################################################
ammonia <- clean %>%
        filter(Type == "Ammonia") %>%
        mutate(
                Response = str_to_lower(Response),
                IsYes    = Response == "yes"
        ) %>%
        group_by(LM) %>%
        summarise(AmmoniaYes = sum(IsYes), .groups = "drop") %>%
        mutate(
                AmmoniaFlag = case_when(
                        AmmoniaYes >= 3 ~ "FAIL",
                        AmmoniaYes == 2 ~ "WARN",
                        TRUE            ~ "PASS"
                )
        )

############################################################
## 7. Merge everything
############################################################
summary_table <- consensus %>%
        left_join(panel_size, by = "LM") %>%
        left_join(ammonia,    by = "LM") %>%
        left_join(lm_map,     by = "LM")

############################################################
## 8. Build summary for Max (Excel output)
############################################################
max_summary <- summary_table %>%
        rowwise() %>%
        mutate(
                SecondaryLabel = {
                        if (isTRUE(HasTieTop)) {
                                "N/A (not assessed due to dominant tie)"
                        } else if (!is.na(SecVotes) && SecondarySet != "" && Nsec > 1) {
                                paste0(
                                        "N/A (tie between: ",
                                        shorten_list(SecondarySet, 3),
                                        " — ",
                                        SecVotes, "/", PanelSize,
                                        " each)"
                                )
                        } else if (!is.na(SecVotes) && SecondarySet != "" && Nsec == 1) {
                                paste0(
                                        shorten_list(SecondarySet, 3),
                                        " (", SecVotes, "/", PanelSize, ")"
                                )
                        } else {
                                ""
                        }
                },
                
                ConsensusAroma = if (DomVotes < 3) {
                        "Not reached"
                } else if (isTRUE(HasTieTop)) {
                        "Mixed / No dominant"
                } else if (!is.na(SecVotes) && SecondarySet != "") {
                        if (Nsec > 1) paste0(DominantSet, " dominant; mixed secondary profile")
                        else paste0(DominantSet, " dominant with ", shorten_list(SecondarySet, 3), " secondary notes")
                } else {
                        paste0(DominantSet, " dominant")
                },
                
                DominantTieCategories = if (isTRUE(HasTieTop)) DominantSet else ""
        ) %>%
        ungroup() %>%
        select(
                LM, LMCode, GID,
                `Dominant call`        = DominantCall,
                `Dominant tie set`     = DominantTieCategories,
                `Secondary call`       = SecondaryLabel,
                `Consensus aroma call` = ConsensusAroma,
                `Ammonia call`         = AmmoniaFlag,
                AmmoniaYes,
                PanelSize
        ) %>%
        arrange(LM)

write_xlsx(
        max_summary,
        path = file.path(output_dir, "Max_Aroma_Summary.xlsx")
)

############################################################
## 9. Aroma categories in fixed order (9-category version)
############################################################
all_categories <- c(
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
## 10. Export category vote percentages per sample
##     (panel agreement, not intensity)
############################################################
category_percent_long <- expand_grid(
        LM = unique(summary_table$LM),
        Category = all_categories
) %>%
        left_join(freq, by = c("LM", "Category")) %>%
        left_join(panel_size, by = "LM") %>%
        left_join(lm_map, by = "LM") %>%
        mutate(
                Votes   = replace_na(Votes, 0L),
                Percent = ifelse(!is.na(PanelSize) & PanelSize > 0,
                                 round(100 * Votes / PanelSize, 1),
                                 NA_real_)
        ) %>%
        select(LM, LMCode, GID, Category, Votes, PanelSize, Percent) %>%
        arrange(LM, factor(Category, levels = all_categories))

category_percent_wide <- category_percent_long %>%
        select(LM, LMCode, GID, Category, Percent) %>%
        pivot_wider(
                names_from  = Category,
                values_from = Percent
        ) %>%
        arrange(LM)

write_xlsx(
        list(
                Category_Percent_Long = category_percent_long,
                Category_Percent_Wide = category_percent_wide
        ),
        path = file.path(output_dir, "Aroma_Category_Percentages.xlsx")
)

############################################################
## 11. Export panelist-level raw votes (with names)
############################################################
panel_votes <- parental %>%
        select(PanelistID, PanelistName, LM, Category) %>%
        arrange(LM, PanelistID, Category)

ammo_votes <- clean %>%
        filter(Type == "Ammonia") %>%
        mutate(IsYes = ifelse(str_to_lower(Response) == "yes", 1L, 0L)) %>%
        select(PanelistID, PanelistName, LM, IsYes) %>%
        arrange(LM, PanelistID)

write_xlsx(
        list(
                CATA_votes    = panel_votes,
                Ammonia_votes = ammo_votes
        ),
        path = file.path(output_dir, "Panelist_Raw_Votes.xlsx")
)

############################################################
## 12. Plot function (LM-level barplots)
############################################################
plot_lm <- function(lm_id) {
        
        lm_data <- freq %>% filter(LM == lm_id)
        
        lm_data_full <- tibble(Category = all_categories) %>%
                left_join(lm_data, by = "Category") %>%
                mutate(Votes = replace_na(Votes, 0L))
        
        lm_info <- summary_table %>% filter(LM == lm_id)
        
        dominant_set <- lm_info$DominantSet
        dvotes       <- lm_info$DomVotes
        secvotes     <- lm_info$SecVotes
        nsec         <- lm_info$Nsec
        panel_n      <- lm_info$PanelSize
        ammo_flag    <- lm_info$AmmoniaFlag
        ammo_yes     <- lm_info$AmmoniaYes
        gid          <- lm_info$GID
        lmcode       <- lm_info$LMCode
        has_tie_top  <- lm_info$HasTieTop
        
        secondary_short <- {
                if (isTRUE(has_tie_top)) {
                        ""
                } else if (is.na(lm_info$SecondarySet) || lm_info$SecondarySet == "") {
                        ""
                } else {
                        shorten_list(lm_info$SecondarySet, 3)
                }
        }
        
        if (dvotes < 3) {
                subtitle1  <- "<b>Dominant call:</b> None – Low agreement"
                subtitle2  <- "<b>Secondary call:</b> None"
                aroma_call <- "Not reached"
                
        } else if (isTRUE(has_tie_top)) {
                subtitle1 <- paste0(
                        "<b>Dominant call:</b> None – Mixed / No clear dominant (tie; ",
                        dvotes, " votes each)"
                )
                subtitle2 <- "<b>Secondary call:</b> N/A (not assessed due to dominant tie)"
                aroma_call <- "Mixed / No dominant"
                
        } else {
                subtitle1 <- paste0("<b>Dominant call:</b> ", dominant_set, " (", dvotes, "/", panel_n, ")")
                
                if (!is.na(secvotes) && secondary_short != "") {
                        if (nsec > 1) {
                                subtitle2 <- paste0(
                                        "<b>Secondary call:</b> N/A (tie between: ",
                                        secondary_short,
                                        " – ", secvotes, "/", panel_n, " each)"
                                )
                                aroma_call <- paste0(dominant_set, " dominant; mixed secondary profile")
                        } else {
                                subtitle2 <- paste0("<b>Secondary call:</b> ", secondary_short, " (", secvotes, "/", panel_n, ")")
                                aroma_call <- paste0(dominant_set, " dominant with ", secondary_short, " secondary notes")
                        }
                } else {
                        subtitle2  <- "<b>Secondary call:</b> None"
                        aroma_call <- paste0(dominant_set, " dominant")
                }
        }
        
        subtitle3 <- paste0("<b>Consensus Aroma call:</b> ", aroma_call)
        subtitle4 <- paste0("<b>Ammonia call:</b> ", ammo_flag, " (", ammo_yes, "/", panel_n, ")")
        subtitle_block <- paste(subtitle1, subtitle2, subtitle3, subtitle4, sep = "<br>")
        
        ggplot(
                lm_data_full,
                aes(
                        x = fct_relevel(Category, all_categories),
                        y = Votes,
                        fill = Category
                )
        ) +
                geom_col() +
                geom_text(aes(label = Votes), vjust = -0.3, size = 4) +
                scale_fill_hue() +
                scale_y_continuous(breaks = 0:panel_n, limits = c(0, panel_n)) +
                theme_minimal(base_size = 16) +
                theme(
                        plot.background  = element_rect(fill = "white", color = NA),
                        panel.background = element_rect(fill = "white", color = NA),
                        panel.grid.minor = element_blank(),
                        axis.text.x      = element_text(angle = 45, hjust = 1, size = 15),
                        legend.position  = "none",
                        plot.title       = element_text(face = "bold", size = 19),
                        plot.subtitle    = ggtext::element_markdown(size = 15, lineheight = 1.35)
                ) +
                labs(
                        title    = paste0(lm_id, " (LMCode: ", lmcode, ") – GID: ", gid),
                        subtitle = subtitle_block,
                        x        = "Parental Aroma Categories",
                        y        = "Number of Panelists"
                )
}

############################################################
## 13. Export LM-level plots
############################################################
unique_lms <- unique(freq$LM)

for (lm in unique_lms) {
        ggsave(
                filename = file.path(output_dir, paste0(lm, "_PG_CATA_plot.png")),
                plot     = plot_lm(lm),
                width    = 11,
                height   = 7,
                dpi      = 300,
                bg       = "white"
        )
}

############################################################
## 14. NEW: Breeder-friendly percent profile plot per LM
############################################################
plot_lm_percent_profile <- function(lm_id) {
        
        lm_percent_data <- category_percent_long %>%
                filter(LM == lm_id) %>%
                mutate(
                        Category = factor(Category, levels = all_categories)
                ) %>%
                arrange(desc(Percent), Category) %>%
                mutate(
                        Category = factor(Category, levels = rev(unique(as.character(Category))))
                )
        
        lm_info <- lm_percent_data %>%
                distinct(LM, LMCode, GID, PanelSize)
        
        top3 <- lm_percent_data %>%
                arrange(desc(Percent), Category) %>%
                slice_head(n = 3) %>%
                mutate(Label = paste0(Category, " ", Percent, "%")) %>%
                pull(Label) %>%
                paste(collapse = " | ")
        
        ggplot(
                lm_percent_data,
                aes(x = Category, y = Percent, fill = Category)
        ) +
                geom_col() +
                geom_text(aes(label = paste0(Percent, "%")), hjust = -0.15, size = 4) +
                coord_flip() +
                scale_fill_hue() +
                scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0.08))) +
                theme_minimal(base_size = 16) +
                theme(
                        plot.background  = element_rect(fill = "white", color = NA),
                        panel.background = element_rect(fill = "white", color = NA),
                        panel.grid.minor = element_blank(),
                        axis.text.y      = element_text(size = 14),
                        axis.text.x      = element_text(size = 13),
                        legend.position  = "none",
                        plot.title       = element_text(face = "bold", size = 19),
                        plot.subtitle    = element_text(size = 14, lineheight = 1.2)
                ) +
                labs(
                        title = paste0(lm_id, " (LMCode: ", lm_info$LMCode[1], ") – GID: ", lm_info$GID[1]),
                        subtitle = paste0(
                                "Panel agreement profile (n = ", lm_info$PanelSize[1], ")\n",
                                "Top profile: ", top3
                        ),
                        x = "Parental Aroma Categories",
                        y = "Panel Agreement (%)"
                )
}

############################################################
## 15. NEW: Export breeder-friendly percent profile plots
############################################################
for (lm in unique_lms) {
        ggsave(
                filename = file.path(percent_plot_dir, paste0(lm, "_Percent_Profile.png")),
                plot     = plot_lm_percent_profile(lm),
                width    = 10,
                height   = 7,
                dpi      = 300,
                bg       = "white"
        )
}

############################################################
## 16. Done
############################################################
message(
        "Done. Recomputed Max_Aroma_Summary.xlsx + Panelist_Raw_Votes.xlsx + ",
        "Aroma_Category_Percentages.xlsx + LM vote plots + breeder-friendly percent profile plots exported to: ",
        output_dir
)
