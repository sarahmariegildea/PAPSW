# descriptive statistics 

# List the outcome and all predictor variables I'm going to use
PSW_model_vars <- c(
    "age_4cat",
    "sex_2cat",
    "raceethnicity_cat",
    "education_cat",
    "maritalrelat_new",
    "psw_service4cat",
    "psw_status3cat",
    "safety_numppl_1",
    "dsm5_ptsd_recreated",
    "stressor_factor1",
    "stressor_factor2",
    "stressor_factor3",
    "stressor_factor4",
    "trauma_cat"
)
# Number of missing observations for variables used
# in the primary adjusted model
PSW_data |>
  summarise(
    across(
      all_of(PSW_model_vars),
      ~ sum(is.na(.x))
    )
  )
# Keep only observations complete on those variables
#PSW_analysis <- PSW_data |>
 #   drop_na(all_of(PSW_model_vars))

# check the n's before and after
#nrow(PSW_data)
#nrow(PSW_analysis)


# I need to create factor and numeric versions so it works in tables vs regression
PSW_analysis <- PSW_data |>
    mutate(
        # Explicit numeric version of PTSD outcome
        ptsd_yes = case_when(
            as.numeric(dsm5_ptsd_recreated) == 1 ~ 1L,
            as.numeric(dsm5_ptsd_recreated) == 0 ~ 0L,
            TRUE ~ NA_integer_
        ),
        
        # Factor version used for Table 1 
        ptsd_group = factor(
            ptsd_yes,
            levels = c(0, 1),
            labels = c("No 30-day DSM-5 PTSD", "30-day DSM-5 PTSD")
        ),
        # Factor versions used in tables/regression
        trauma_cat_f = as_factor(trauma_cat) |>
            forcats::fct_relevel("0 traumatic events"),
        
        age_4cat_f = as_factor(age_4cat),
        sex_2cat_f = as_factor(sex_2cat),
        raceethnicity_cat_f = as_factor(raceethnicity_cat),
        education_cat_f = as_factor(education_cat),
        maritalrelat_new_f = as_factor(maritalrelat_new),
        psw_service4cat_f = as_factor(psw_service4cat),
        psw_status3cat_f = as_factor(psw_status3cat)
    ) 
levels(PSW_analysis$trauma_cat_f)
table(PSW_analysis$ptsd_group, useNA = "ifany")
table(PSW_analysis$ptsd_yes, useNA = "ifany")
table(PSW_analysis$psw_status3cat_f, useNA = "ifany")
table(PSW_analysis$psw_service4cat_f, useNA = "ifany")

# now just some simple descriptive statistics 
# How prevalent is current PTSD among PSWs?
ptsd_prev <- PSW_analysis |>
    summarise(
        n = sum(!is.na(ptsd_yes)),
        ptsd_n = sum(ptsd_yes == 1, na.rm = TRUE),
        ptsd_pct = mean(ptsd_yes == 1, na.rm = TRUE) * 100
    )
ptsd_prev

# n     ptsd_n ptsd_pct
#  2985    596     20.0
# 20.0% of PSWs met criteria for 30-day DSM-5 PTSD

# Distribution of trauma event types
# number of different occupational traumatic-event types endorsed
table(as_factor(PSW_analysis$trauma_cat, levels = "labels"),
      useNA = "ifany")
# 0 traumatic events 1-2  3-5  6-8     9+ traumatic events 
# 264                 218   433  610          1460 

# PTSD prevalence within the trauma categories
ptsd_by_trauma <- PSW_analysis |>
    group_by(trauma_cat_f) |>
    summarise(
        n = n(),
        ptsd_n = sum(ptsd_yes == 1, na.rm = TRUE),
        ptsd_pct = mean(ptsd_yes == 1, na.rm = TRUE) * 100
    )
ptsd_by_trauma
# A tibble: 5 × 4
# trauma_cat                  n ptsd_n ptsd_pct
# <dbl+lbl>               <int>  <int>    <dbl>
# 0 [0 traumatic events]    264     46    17.4 
# 1 [1-2]                   218     16     7.34
# 2 [3-5]                   433     41     9.47
# 3 [6-8]                   610     75    12.3 
# 4 [9+ traumatic events]  1460    418    28.6 

# PTSD prevalence was highest among participants reporting nine or more occupational 
# traumatic-event types. However, the relationship was not strictly linear across categories, 
# because prevalence was lower among participants reporting one to eight event types than 
# among those reporting no occupational event types. 
# This pattern should be interpreted cautiously, particularly because the 
# 0 exposure and 1-2 exposure groups were relatively small and because occupational trauma 
# exposure does not necessarily capture trauma from other areas of life.


# now create the descriptive table 1
table1 <- PSW_analysis |>
    select(
         ptsd_group,
        trauma_cat_f,
        age_4cat_f,
        sex_2cat_f,
        raceethnicity_cat_f,
        education_cat_f,
        maritalrelat_new_f,
        psw_service4cat_f,
        psw_status3cat_f, 
        ptsd_yes
        ) |>
    tbl_summary(
        by = ptsd_group,
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "no",
        label = list(
            trauma_cat_f ~ "Number of different traumatic-event types endorsed",
            age_4cat_f ~ "Age group",
            sex_2cat_f ~ "Sex",
            raceethnicity_cat_f ~ "Race/ethnicity",
            education_cat_f ~ "Education",
            maritalrelat_new_f ~ "Marital/relationship status",
            psw_service4cat_f ~ "Public safety service",
            psw_status3cat_f ~ "PSW employment status",
            ptsd_yes ~ "30-day DSM-5 PTSD"
        )
    ) |>
    add_overall(last = TRUE) |>
    add_p() |>
    bold_labels()

table1

table1_gt <- table1 |>
    as_gt() |>
    tab_header(
        title = "Characteristics of Pennsylvania Public Safety Workers by 30-Day PTSD Status"
    )

gtsave(
    data = table1_gt,
    filename = "Table1.html"
)
