# Figures

#check stressors and correlations
summary(
    PSW_analysis |>
        select(stressor_factor1:stressor_factor4)
)
stressor_data <- PSW_analysis |>
    select(stressor_factor1:stressor_factor4) 

# Save the printed summary as a text file 
# minimum, median, mean, maximum, and missingness
writeLines(
    capture.output(summary(stressor_data)),
    "stressor_factor_summary.txt"
)

# Create and save the Spearman correlation matrix as a CSV and txt file
stressor_cor <- cor(
    stressor_data,
    use = "pairwise.complete.obs",
    method = "spearman"
)
write.csv(
    stressor_cor,
    "stressor_factor_correlations.csv",
    row.names = TRUE
)
write.table(
    stressor_cor,
    file = "stressor_factor_correlations.txt",
    sep = "\t",
    row.names = TRUE,
    col.names = NA
)
# descriptive trauma figure
ptsd_trauma_plot <- ggplot(
    ptsd_by_trauma,
    aes(
        x = trauma_cat_f,
        y = ptsd_pct
    )
) +
    geom_col() +
    labs(
        x = "Number of different traumatic-event types endorsed",
        y = "30-day DSM-5 PTSD (%)",
        title = "30-day DSM-5 PTSD prevalence by occupational trauma exposure"
    ) +
    theme_minimal()

print(ptsd_trauma_plot)
ggsave(
    filename = "PTSD_by_trauma_number.png",
    plot = ptsd_trauma_plot,
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
)



ptsd_by_trauma <- PSW_analysis |>
    group_by(trauma_cat_f) |>
    summarise(
        n = n(),
        ptsd_n = sum(ptsd_yes == 1),
        ptsd_pct = mean(ptsd_yes == 1) * 100,
        .groups = "drop"
    )

ptsd_trauma_plot <- ggplot(
    ptsd_by_trauma,
    aes(
        x = trauma_cat_f,
        y = ptsd_pct
    )
) +
    geom_col(
        fill = "#2C7FB8",
        width = 0.7
    ) +
    geom_text(
        aes(label = sprintf("%.1f%%", ptsd_pct)),
        vjust = -0.4,
        size = 4
    ) +
    scale_y_continuous(
        limits = c(0, 35),
        expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
        x = "Number of different traumatic-event types endorsed",
        y = "Participants with 30-day PTSD (%)",
        title = "30-day PTSD prevalence by number of traumatic-event types endorsed"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.major.x = element_blank()
    )

ptsd_trauma_plot
print(ptsd_trauma_plot)
ggsave(
    filename = "PTSD_trauma_plot.png",
    plot = ptsd_trauma_plot,
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
)