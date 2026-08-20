# ============================================================
# LOGISTIC REGRESSION
# Occupational traumatic exposure and current PTSD
# ============================================================

# ------------------------------------------------------------
# 1. SET REFERENCE GROUPS
# ------------------------------------------------------------

# Reference groups:
# Trauma: 0 traumatic events 
# Age: 18–24 
# Sex: Female 
# Race/ethnicity: Non-Hispanic White 
# Education: High school or GED 
# Marital relationship: Married/engaged/cohabit 
# Service sector: Police
# PSW status: Current 

PSW_analysis <- PSW_analysis |>
mutate(
    trauma_cat_f = fct_relevel(
        factor(trauma_cat_f),
        "0 traumatic events"
    ),
    age_4cat_f = fct_relevel(
        factor(age_4cat_f),
        "18–24"
    ),
    sex_2cat_f = fct_relevel(
        factor(sex_2cat_f),
        "Female"
    ),
    raceethnicity_cat_f = fct_relevel(
        factor(raceethnicity_cat_f),
        "Non-Hispanic White"
    ),
    education_cat_f = fct_relevel(
        factor(education_cat_f),
        "High school or GED"
    ),
    maritalrelat_new_f = fct_relevel(
        factor(maritalrelat_new_f),
        "Married/engaged/cohabit"
    ),
    psw_service4cat_f = fct_relevel(
        factor(psw_service4cat_f),
        "Police"
    ),
    psw_status3cat_f = fct_relevel(
        factor(psw_status3cat_f),
        "Current"
    )
)

# ------------------------------------------------------------
# 2. CHECK THE REFERENCE LEVELS
# ------------------------------------------------------------
lapply(
    PSW_analysis[c(
        "trauma_cat_f",
        "age_4cat_f",
        "sex_2cat_f",
        "raceethnicity_cat_f",
        "education_cat_f",
        "maritalrelat_new_f",
        "psw_service4cat_f",
        "psw_status3cat_f"
    )],
    levels
)
# ------------------------------------------------------------
# 3. CHECK THE ANALYTIC SAMPLE AND MISSINGNESS
# ------------------------------------------------------------

# Number of participants in the dataset
nrow(PSW_analysis)

# Number of missing observations for variables used
# in the primary adjusted model
PSW_analysis |>
    summarise(
        across(
            all_of(PSW_model_vars),
            ~ sum(is.na(.x))
        )
    )

# ------------------------------------------------------------
# 4. MODEL 1: UNADJUSTED
# ------------------------------------------------------------

# Examines the association between occupational traumatic
# exposure and current PTSD without adjustment for covariates.
model1_unadj <- glm(
    ptsd_yes ~ trauma_cat_f,
    data = PSW_analysis,
    family = binomial(link = "logit")
)

# ------------------------------------------------------------
# 5. MODEL 2: PRIMARY ADJUSTED MODEL
# ------------------------------------------------------------

# Adjusts for demographic and occupational characteristics.
model2_adj <- glm(
    ptsd_yes ~
        trauma_cat_f +
        age_4cat_f +
        sex_2cat_f +
        raceethnicity_cat_f +
        education_cat_f +
        maritalrelat_new_f +
        psw_service4cat_f +
        psw_status3cat_f,
    data = PSW_analysis,
    family = binomial(link = "logit")
)

# ------------------------------------------------------------
# 6. MODEL 3: ADJUSTED MODEL + WORK-RELATED STRESSORS
# ------------------------------------------------------------

# Adds the four work-related stressor factors to Model 2.
# This is a secondary/additional model.
model3_stress <- glm(
    ptsd_yes ~
        trauma_cat_f +
        age_4cat_f +
        sex_2cat_f +
        raceethnicity_cat_f +
        education_cat_f +
        maritalrelat_new_f +
        psw_service4cat_f +
        psw_status3cat_f + 
        stressor_factor1 +
        stressor_factor2 +
        stressor_factor3 +
        stressor_factor4,
    data = PSW_analysis,
    family = binomial(link = "logit")
)
# ------------------------------------------------------------
# 7. BASIC MODEL CHECKS
# ------------------------------------------------------------

# Check that all models converged
# Check that all models converged
model1_unadj$converged
model2_adj$converged
model3_stress$converged

# Number of observations used in each model
nobs(model1_unadj)
nobs(model2_adj)
nobs(model3_stress)

# Full model summaries if you need to inspect them
summary(model1_unadj)
summary(model2_adj)
summary(model3_stress)

# ------------------------------------------------------------
# 8. EXTRACT ODDS RATIOS AND 95% CIs
# ------------------------------------------------------------

# exponentiate = TRUE converts logistic regression coefficients
# from log-odds to odds ratios.
model1_output <- tidy(
    model1_unadj,
    conf.int = TRUE,
    exponentiate = TRUE
)
model2_output <- tidy(
    model2_adj,
    conf.int = TRUE,
    exponentiate = TRUE
)
model3_output <- tidy(
    model3_stress,
    conf.int = TRUE,
    exponentiate = TRUE
)
# ------------------------------------------------------------
# 9. CHECK FOR POTENTIAL MODEL INSTABILITY
# ------------------------------------------------------------

# Look for infinite estimates or unusually large/small
# odds ratios in the primary adjusted model.
model2_output |>
    filter(
        is.infinite(estimate) |
            is.infinite(conf.low) |
            is.infinite(conf.high) |
            estimate > 100 |
            estimate < 0.01
    )
# Look at the largest standard errors
# to identify coefficients that may be unstable.
tidy(model2_adj) |>
    arrange(desc(std.error))

# ------------------------------------------------------------
# 10. EXAMINE THE TRAUMA EXPOSURE RESULTS
# ------------------------------------------------------------

# Primary question:
# Is greater occupational traumatic exposure associated
# with current PTSD?
model2_output |>
    filter(grepl("^trauma_cat_f", term)) |>
    select(
        term,
        estimate,
        conf.low,
        conf.high,
        p.value
    )
# Unadjusted trauma results
model1_output |>
    filter(grepl("^trauma_cat_f", term))

# Adjusted + stressors trauma results
model3_output |>
    filter(grepl("^trauma_cat_f", term))

# ------------------------------------------------------------
# 11. CREATE A CLEAN TRAUMA RESULTS TABLE
# ------------------------------------------------------------

model2_output |>
    filter(grepl("^trauma_cat_f", term)) |>
    transmute(
        predictor = term,
        adjusted_OR = round(estimate, 2),
        CI_lower = round(conf.low, 2),
        CI_upper = round(conf.high, 2),
        p_value = format.pval(
            p.value,
            digits = 3,
            eps = 0.001
        )
    )
# ------------------------------------------------------------
# 12. CREATE THE MAIN REGRESSION TABLE
# ------------------------------------------------------------

# Displays the three logistic regression models side-by-side.
# Estimates are presented as odds ratios.
labels_model1 <- list(
    trauma_cat_f = "Number of different traumatic-event types endorsed"
)
labels_model2 <- list(
    trauma_cat_f = "Number of different traumatic-event types endorsed",
    age_4cat_f = "Age group",
    sex_2cat_f = "Sex",
    raceethnicity_cat_f = "Race/ethnicity",
    education_cat_f = "Education",
    maritalrelat_new_f = "Marital/relationship status",
    psw_service4cat_f = "Public safety service",
    psw_status3cat_f = "PSW employment status"
)
labels_model3 <- c(
    labels_model2,
    list(
        stressor_factor1 = "Organizational climate and supervisory stress",
        stressor_factor2 = "Operational resource inadequacy",
        stressor_factor3 = "Administrative and public-facing stress",
        stressor_factor4 = "Coworker competence concerns"
    )
)
tbl_1 <- tbl_regression(
    model1_unadj,
    exponentiate = TRUE,
    label = labels_model1
)
tbl_2 <- tbl_regression(
    model2_adj,
    exponentiate = TRUE,
    label = labels_model2
)
tbl_3 <- tbl_regression(
    model3_stress,
    exponentiate = TRUE,
    label = labels_model3
)
regression_table <- tbl_merge(
    tbls = list(tbl_1, tbl_2, tbl_3),
    tab_spanner = c(
        "**Model 1: Unadjusted**",
        "**Model 2: Adjusted**",
        "**Model 3: Adjusted + Stressors**"
    )
) |>
    bold_labels()
regression_table

regression_table_gt <- regression_table |>
    as_gt() |>
    tab_header(
        title = "Associations Between Occupational Traumatic Exposure and 30-day PTSD Among Pennsylvania Public Safety Workers"
    )
gtsave(
    data = regression_table_gt,
    filename = "regression_table.html"
)

# ------------------------------------------------------------
# 13. FIGURE: TRAUMA EXPOSURE AND PTSD
# ------------------------------------------------------------

# Plot the adjusted odds ratios for occupational traumatic
# exposure from the primary adjusted model.

trauma_or_plot <- model2_output |>
    filter(grepl("^trauma_cat_f", term)) |>
    mutate(
        trauma = sub("^trauma_cat_f", "", term),
        trauma = factor(
            trauma,
            levels = c("1-2", "3-5", "6-8", "9+ traumatic events")
        )
    ) |>
    ggplot(
        aes(
            x = estimate,
            y = trauma,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
     geom_errorbar(
    orientation = "y",
    height = 0.15
  ) +
    geom_errorbarh(height = 0.15) +
    geom_point(size = 3, color = "#D95F02") +
    geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "gray40"
    ) +
    scale_x_log10() +
    labs(
        x = "Adjusted odds ratio (95% CI)",
        y = "Number of different traumatic-event types endorsed",
        title = "Number of different traumatic-event types endorsed and 30-day DSM-5 PTSD"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
    )
trauma_or_plot
ggsave(
    filename = "trauma_adjusted_ORs.png",
    plot = trauma_or_plot,
    width = 8,
    height = 5,
    dpi = 300
)
