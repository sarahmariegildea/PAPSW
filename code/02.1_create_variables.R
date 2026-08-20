# ID529 
# Sarah Gildea
# July/August 2026

# data manipulation -------------------------------------------------------

#################### ------------------------ DSM-5 PTSD #################### 

# Count of Criterion B items endorsed extremely, a lot, or some
PSW_data$dsmcritb_count <- apply(
    PSW_data[,
                 c("ptsda_1", "ptsda_3", "ptsdb_1", "ptsdb_5", "ptsdc_3")],
    1,
    function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE)
)
# Yes/no for meeting Criterion B: 1 or more
PSW_data$dsmcritb <- ifelse(
    PSW_data$dsmcritb_count >= 1,
    1,
    0
)

# Count of Criterion C items endorsed extremely, a lot, or some
PSW_data$dsmcritc_count <- apply(
    PSW_data[,
                  c("ptsda_4", "ptsda_5")],
    1,
    function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE)
)
# Yes/no for meeting Criterion C: 1 or more
PSW_data$dsmcritc <- ifelse(
    PSW_data$dsmcritc_count >= 1,
    1,
    0
)

# Count of Criterion D items endorsed extremely, a lot, or some
PSW_data$dsmcritd_count <- apply(
    PSW_data[,
                  c("ptsdb_2", "ptsdc_1", "ptsdc_2", "ptsdc_5",
                    "ptsdd_2", "ptsdd_3", "ptsdd_4")],
    1,
    function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE)
)
# Yes/no for meeting Criterion D: 2 or more
PSW_data$dsmcritd <- ifelse(
    PSW_data$dsmcritd_count >= 2,
    1,
    0
)

# Count of Criterion E items endorsed extremely, a lot, or some
PSW_data$dsmcrite_count <- apply(
    PSW_data[,
                  c("ptsda_6", "ptsda_7", "ptsdb_3",
                    "ptsdb_4", "ptsdc_4", "ptsdd_1")],
    1,
    function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE)
)
# Yes/no for meeting Criterion E: 2 or more
PSW_data$dsmcrite <- ifelse(
    PSW_data$dsmcrite_count >= 2,
    1,
    0
)

# Count of Interference + Distress  items endorsed extremely, a lot, or some
PSW_data$dsmint_count <- apply(
    PSW_data[,
                  c("ptsdint_1", "ptsdint_2", "ptsdint_3", "ptsd_dist")],
    1,
    function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE)
)
# Yes/no for meeting interference
PSW_data$dsmint <- ifelse(
    PSW_data$dsmint_count >= 1,
    1,
    0
)

# Yes/no for meeting all DSM-5 Criterion with interference
PSW_data$dsm5_ptsd_recreated <- ifelse(
    PSW_data$dsmcritb == 1 &
        PSW_data$dsmcritc == 1 &
        PSW_data$dsmcritd == 1 &
        PSW_data$dsmcrite == 1 &
        PSW_data$dsmint == 1,
    1,
    0
)
PSW_data <- PSW_data |>
    mutate(dsm5_ptsd_recreated = labelled(
        dsm5_ptsd_recreated,
        c("No 30-day DSM-5 PTSD" = 0, "Yes 30-day DSM-5 PTSD" = 1)
    )
    )
# Yes/no for meeting DSM-5 Criterion without interference
PSW_data$dsm5_ptsd_noint <- ifelse(
    PSW_data$dsmcritb == 1 &
        PSW_data$dsmcritc == 1 &
        PSW_data$dsmcritd == 1 &
        PSW_data$dsmcrite == 1,
    1,
    0
)
table(PSW_data$dsm5_ptsd_recreated, useNA = "ifany")

# table comparing recreated and original variable
table(
    PSW_data$dsm5_ptsd_recreated,
    PSW_data$criteria_dsm5_ptsd,
    useNA = "ifany"
)

#################### ------------------------ Work stressors #################### 
# Spearman correlation to see if the items hang together to create a scale
stressors <- PSW_data |>
    select(safetystrsa_1_rev:safetystrsb_5_rev)
psych::corr.test(
    stressors,
    method = "spearman",
    use = "pairwise"
)
# most of the item correlations are .4 or higher 
# reliability
psych::alpha(stressors)
# the raw_alpha is .9 so they are probably redundant
# and now a factor anaylsis to see how many factors
psych::fa.parallel(
    stressors,
    fa = "fa",
    cor = "poly"
)
# the suggested number of factors is 4, components na
# so run the 4 factor solution
stressors <- PSW_data |>
    select(safetystrsa_1_rev:safetystrsb_5_rev)

fa_4 <- psych::fa(
    stressors,
    nfactors = 4,
    rotate = "oblimin",
    fm = "minres",
    cor = "poly"
)
print(fa_4$loadings, cutoff = .30)
# save this
saveRDS(fa_4, "fa_4_factor_analysis.rds")
fa_4_loadings <- as.data.frame(unclass(fa_4$loadings))

write.csv(
    fa_4_loadings,
    "fa_4_loadings.csv",
    row.names = TRUE
)
writeLines(
    capture.output(print(fa_4$loadings, cutoff = .30)),
    "fa_4_loadings.txt"
)
png(
    "stressors_parallel_analysis.png",
    width = 8,
    height = 8,
    units = "in",
    res = 300
)
psych::fa.parallel(
    stressors,
    fa = "fa",
    cor = "poly"
)
dev.off()
#Loadings:
#                   MR3    MR2    MR1    MR4   
# safetystrsa_1_rev         0.690              
# safetystrsa_2_rev         0.699              
# safetystrsa_3_rev  0.465  0.452              
# safetystrsa_4_rev  0.873                     
# safetystrsa_5_rev  0.695                     
# safetystrsb_1_rev  0.507                0.371
# safetystrsb_2_rev                       0.899
# safetystrsb_3_rev  0.674                     
# safetystrsb_4_rev                0.313       
# safetystrsb_5_rev                0.997       

#               MR3   MR2   MR1   MR4
# SS loadings    2.249 1.244 1.108 0.988
# Proportion Var 0.225 0.124 0.111 0.099
# Cumulative Var 0.225 0.349 0.460 0.559
# so i will create 4 scales with these items instead of 1 big one

#  How much ongoing stress do you experience in each of these other areas of your public safety work? 
# stressor_factor1 = Organizational climate and supervisory stress scale 
    # safetystrsa_3_rev: Conflicting demands or unclear expectations
    # safetystrsa_4_rev: Favoritism or unfair treatment of workers by line supervisors
    # safetystrsa_5_rev: Lack of input or voice in decisions that affect how you do your job
    # safetystrsb_1_rev: Incompetent, poorly trained, or unmotivated supervisors
    # safetystrsb_3_rev: A toxic work climate (e.g., gossip, disrespect, bullying, lack of trust)

# stressor_factor2 = Operational resource inadequacy scale
    # safetystrsa_1_rev: Inadequate staffing to do the job safely and effectively
    # safetystrsa_2_rev: Inadequate equipment or facilities
    
# stressor_factor3 = Administrative and public-facing stress scale
    # safetystrsb_4_rev: Excessive paperwork or administrative tasks that take time away from core duties
    # safetystrsb_5_rev: Negative reactions, criticism, or lack of support from the public or media
    
# stressor_factor4 = Coworker competence concerns scale
    # safetystrsb_2_rev: Incompetent, poorly trained, or unmotivated coworkers
# An exploratory factor analysis suggested four dimensions of work-related stress. 
# Because some factors contained only one or two items and some items cross-loaded, 
# these stressor measures were treated as exploratory secondary covariates rather than 
# validated scales.        
PSW_data <- PSW_data |>
    mutate(
        stressor_factor1 = rowSums(
            across(c(
                safetystrsa_3_rev,
                safetystrsa_4_rev,
                safetystrsa_5_rev,
                safetystrsb_1_rev,
                safetystrsb_3_rev
            )),
            na.rm = TRUE
        ),
        stressor_factor2 = rowSums(
            across(c(
                safetystrsa_1_rev,
                safetystrsa_2_rev
            )),
            na.rm = TRUE
        ), 
        stressor_factor3 = rowSums(
            across(c(
                safetystrsb_4_rev,
                safetystrsb_5_rev
            )),
            na.rm = TRUE
        ),
        stressor_factor4 = rowSums(
            across(c(
                safetystrsb_2_rev
            )),
            na.rm = TRUE
        )
    )
PSW_data |> 
    select(stressor_factor1:stressor_factor4) |> 
    lapply(table, useNA = "ifany")

#################### ------------------------ Traumatic events #################### 
# create dummy variables for if they experienced each trauma and then a summary of the number experienced 
PSW_data <- PSW_data |>
    mutate(
        across(
            telawenfa_1_rev:teotherb_5_rev,
            ~ case_when(
                .x == 0 ~ 0,
                .x >= 1 ~ 1,
                TRUE ~ NA_real_
            ),
            .names = "{.col}_bin"
        )
    )
PSW_data <- PSW_data |>
    mutate(
        trauma_count = rowSums(
            across(ends_with("_bin")),
            na.rm = TRUE
        )
    )
PSW_data |> 
    select( trauma_count) |> 
    lapply(table, useNA = "ifany")

#and now create categorical for number of trauma types
PSW_data <- PSW_data |>
    mutate(
        trauma_cat = case_when(
            trauma_count == 0 ~ 0,
            trauma_count <= 2 ~ 1,
            trauma_count <= 5 ~ 2,
            trauma_count <= 8 ~ 3,
            trauma_count >= 9 ~ 4,
            TRUE ~ NA_real_
        ),
        trauma_cat = labelled(
            trauma_cat,
            c(
                "0 traumatic events" = 0,
                "1-2" = 1,
                "3-5" = 2,
                "6-8" = 3,
                "9+ traumatic events" = 4
            )
        )
    )

table(PSW_data$trauma_count, useNA="ifany")
table(PSW_data$trauma_cat, useNA="ifany")
