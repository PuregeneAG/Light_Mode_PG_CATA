rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)
library(scales)

get_script_dir <- function() {
        args <- commandArgs(trailingOnly = FALSE)
        file_arg <- grep("^--file=", args, value = TRUE)
        if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
        normalizePath(getwd())
}

script_dir <- get_script_dir()
validation_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
t1_path <- file.path(validation_dir, "04_PROCESSED_DATA", "T1_OUTPUTS", "Max_Aroma_Summary.xlsx")
t2_path <- file.path(validation_dir, "04_PROCESSED_DATA", "T2_OUTPUTS", "Max_Aroma_Summary.xlsx")
output_dir <- file.path(script_dir, "OUTPUTS", "06_CONSENSUS_STABILITY")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

t1 <- read_excel(t1_path)
t2 <- read_excel(t2_path)

req_cols <- c("LM", "LMCode", "GID", "Dominant call", "Consensus aroma call", "PanelSize")

missing_t1 <- setdiff(req_cols, names(t1))
missing_t2 <- setdiff(req_cols, names(t2))

if (length(missing_t1) > 0) stop(paste("T1 missing columns:", paste(missing_t1, collapse = ", ")))
if (length(missing_t2) > 0) stop(paste("T2 missing columns:", paste(missing_t2, collapse = ", ")))

comp <- t1 %>%
        inner_join(t2, by = c("LM", "LMCode", "GID"), suffix = c("_T1", "_T2")) %>%
        mutate(
                Dominant_T1 = str_squish(`Dominant call_T1`),
                Dominant_T2 = str_squish(`Dominant call_T2`),
                
                Consensus_T1 = str_squish(`Consensus aroma call_T1`),
                Consensus_T2 = str_squish(`Consensus aroma call_T2`),
                
                Dominant_T1 = ifelse(is.na(Dominant_T1) | Dominant_T1 == "", "None", Dominant_T1),
                Dominant_T2 = ifelse(is.na(Dominant_T2) | Dominant_T2 == "", "None", Dominant_T2),
                
                Consensus_T1 = ifelse(is.na(Consensus_T1) | Consensus_T1 == "", "None", Consensus_T1),
                Consensus_T2 = ifelse(is.na(Consensus_T2) | Consensus_T2 == "", "None", Consensus_T2),
                
                Dominant_same = Dominant_T1 == Dominant_T2,
                Consensus_same = Consensus_T1 == Consensus_T2,
                
                SampleLabel = paste0("GID ", GID, " (", LM, ")")
        )

headline <- tibble(
        Metric = "Layer 1 T1 vs T2 summary",
        Samples = nrow(comp),
        `Dominant stable count` = sum(comp$Dominant_same, na.rm = TRUE),
        `Dominant stable percent` = round(100 * mean(comp$Dominant_same, na.rm = TRUE), 1),
        `Consensus stable count` = sum(comp$Consensus_same, na.rm = TRUE),
        `Consensus stable percent` = round(100 * mean(comp$Consensus_same, na.rm = TRUE), 1)
)

what_changed <- comp %>%
        transmute(
                LM, LMCode, GID, SampleLabel,
                Dominant_T1,
                Dominant_T2,
                Dominant_changed = !Dominant_same,
                Consensus_T1,
                Consensus_T2,
                Consensus_changed = !Consensus_same
        ) %>%
        filter(Dominant_changed | Consensus_changed) %>%
        arrange(desc(Consensus_changed), desc(Dominant_changed), LM)

fig_data <- bind_rows(
        comp %>%
                transmute(
                        SampleLabel,
                        CallType = "Dominant call",
                        T1 = Dominant_T1,
                        T2 = Dominant_T2,
                        Stable = Dominant_same
                ),
        comp %>%
                transmute(
                        SampleLabel,
                        CallType = "Consensus aroma call",
                        T1 = Consensus_T1,
                        T2 = Consensus_T2,
                        Stable = Consensus_same
                )
) %>%
        mutate(
                Stability = ifelse(Stable, "Stable", "Changed"),
                
                TransitionLabel = ifelse(
                        CallType == "Consensus aroma call",
                        str_wrap(paste0("T1: ", T1, " → T2: ", T2), width = 60),
                        str_wrap(paste0("T1: ", T1, " → T2: ", T2), width = 45)
                ),
                
                CallType = factor(CallType, levels = c("Dominant call", "Consensus aroma call"))
        )

sample_order <- fig_data %>%
        group_by(SampleLabel) %>%
        summarise(
                AnyChanged = any(Stability == "Changed"),
                .groups = "drop"
        ) %>%
        arrange(desc(AnyChanged), SampleLabel) %>%
        pull(SampleLabel)

fig_data <- fig_data %>%
        mutate(SampleLabel = factor(SampleLabel, levels = rev(sample_order)))

p <- ggplot(fig_data, aes(x = CallType, y = SampleLabel, fill = Stability)) +
        geom_tile(
                color = "white",
                linewidth = 0.7,
                width = 0.95,
                height = 1.15
        ) +
        geom_text(
                aes(label = TransitionLabel),
                size = 3.2,
                lineheight = 0.9
        ) +
        scale_fill_manual(
                values = c(
                        "Changed" = "#F4CCCC",
                        "Stable" = "#D9EAD3"
                )
        ) +
        scale_x_discrete(position = "top") +
        labs(
                title = NULL,
                subtitle = NULL,
                x = NULL,
                y = "Samples",
                fill = NULL
        ) +
        theme_minimal(base_size = 14) +
        theme(
                panel.grid = element_blank(),
                
                axis.text.x.top = element_text(
                        face = "bold",
                        size = 15,
                        margin = margin(b = 14)
                ),
                
                axis.text.x.bottom = element_blank(),
                axis.ticks.x = element_blank(),
                
                axis.text.y = element_text(
                        face = "bold",
                        size = 11
                ),
                
                axis.title.y = element_text(
                        face = "bold",
                        size = 15
                ),
                
                legend.position = "top",
                legend.text = element_text(size = 12),
                
                plot.margin = margin(20, 20, 10, 10)
        )

ggsave(
        filename = file.path(output_dir, "Supplementary_Figure_S3_Consensus_Stability.pdf"),
        plot = p,
        width = 16,
        height = 10,
        units = "in"
)

ggsave(
        filename = file.path(output_dir, "Supplementary_Figure_S3_Consensus_Stability.png"),
        plot = p,
        width = 16,
        height = 10,
        units = "in",
        dpi = 300
)

write_xlsx(
        list(
                Headline = headline,
                WhatChanged = what_changed,
                FullJoin = comp,
                FigureData = fig_data
        ),
        path = file.path(output_dir, "Layer1_T1_vs_T2_Dominant_Consensus_Tables.xlsx")
)

write.csv(headline, file.path(output_dir, "Layer1_Headline.csv"), row.names = FALSE)
write.csv(what_changed, file.path(output_dir, "Layer1_WhatChanged.csv"), row.names = FALSE)

message("Done. Figure and tables exported to: ", output_dir)
