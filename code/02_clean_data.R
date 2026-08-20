# ID529 
# Sarah Gildea
# July/August 2026

### data cleaning

#-------------------------------------------------------
#I am going to only use a subset of variables, so will recode those missings etc: 
# 1) demos variables: 
# responseid age sex:
# hispanic race_1 race_2 race_3 race_4 race_5 race_6 race_6_text
# education maritalstatus relatstatus

# 2) PSW basic variables: 
# psw_servicesector psw_status_4cat psw_location - constructed to use as control
# safety_wrkhrs_1 safety_fireemshrs_1 - these are just used for logical imputations
  #only answered if Safety_WrkStatus = 1 /// only answered if Safety_WrkStatus = 1 and Safety_WrkType = 1

# 3) # in locale
# safety_numppl_1 - predictor for 

# 4) work-related stress
# safetystrsa_1:safetystrsb_5 
  #only answered if Safety_WrkStatus = 1

# 5) Traumatic events
# the TEs were answered based on the type of PSW as indicated in the survey
# police: telawenfa_1:telawenfa_4 telawenfb_1:telawenfb_5 telawenfc_1:telawenfc_4
# fire: tefirea_1:tefirea_4 tefireb_1:tefireb_4 tefirec_1:tefirec_4
# fire+EMS: tefireemsa_1:tefireemsa_5 tefireemsb_1:tefireemsb_4 tefireemsc_1:tefireemsc_4
# EMS: teemsa_1:teemsa_5 teemsb_1:teemsb_4 teemsc_1:teemsc_4
# 911: te911a_1:te911a_5 te911b_1:te911b_4 te911c_1:te911c_3
# other: teothera_1:teothera_5 teotherb_1:teotherb_5

# 6) PTSD 
# ptsda_1:ptsda_7 ptsdb_1:ptsdb_5 ptsdc_1:ptsdc_5 ptsdd_1:ptsdd_4 ptsdint_1:ptsdint_3 ptsd_dist
# Full DSM-5 30-day PTSD criteria yes/no: criteria_dsm5_ptsd

#------------------------------ clean column names ------------------------

# I don't want to use the clean function because the variable names are already clean
# So I will just make them all lower case and look at it
names(PSW_df) <- tolower(names(PSW_df))
names(PSW_df)

#------------------------------turn into a tibble#------------------------------
PSW_data <- as_tibble(PSW_df)
print(PSW_data)

#  -----------------------------------------Standardize missing value codes -----------------------------------------
# -99 in qualtrics means seen but not answered, NA means not seen because of skips
# so I am going to replace -99 with NA

# first do some logical imputations/create new variables because of skips and then code -99 to NA
    
##############################------------------------ DEMOS------------------------############
# 1) age and sex
# See if any are -99 and turn to NA: age_cat sex_cat age
table(PSW_data$age, useNA="ifany")
table(PSW_data$sex, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
        age = ifelse(age == -99, NA_real_, age),
        age_4cat = case_when(
          age >= 18 & age <= 24 ~ 0,
          age >= 25 & age <= 34 ~ 1,
          age >= 35 & age <= 49 ~ 2,
          age >= 50 & age <= 64 ~ 3,
          age >= 65 ~ 4,
          TRUE ~ NA_real_
        ),
        age_4cat = labelled(
          age_4cat,
          c(
            "18–24" = 0,
            "25–34" = 1,
            "35–49" = 2,
            "50–64" = 3,
            "65+" = 4
          )
        ),
        sex = ifelse(sex == -99, NA_real_, sex),
        sex_2cat = case_when(
          sex == 1 ~ 0,
          sex == 2 ~ 1,
          TRUE ~ NA_real_
        ),
        sex_2cat = labelled(
          sex_2cat,
          c(
            "Female" = 0,
            "Male" = 1
          )
        )
  )
table(PSW_data$age_4cat, useNA="ifany")
table(PSW_data$age, useNA="ifany")
table(PSW_data$sex, useNA="ifany")
table(PSW_data$sex_2cat, useNA="ifany")

#  2) ethnicity and race
# Hispanic - answered by everyone
# 1	yes 2	no -99	Seen but unanswered
# race: -99	Seen but nothing selected
# race_1	1	White or Caucasian selected 0	White or Caucasian not selected
# race_2	1	Black or African American selected 0	Black or African American not selected
# race_3	1	American Indian or Alaskan Native selected 0	American Indian or Alaskan Native not selected
# race_4	1	Asian (e.g., Chinese, Filipino, Indian) selected 0	Asian (e.g., Chinese, Filipino, Indian) not selected
# race_5	1	Native Hawaiian or other Pacific Islander selected 0	Native Hawaiian or other Pacific Islander not selected
# race_6	1	My race is not listed above (Please briefly describe) selected 0	My race is not listed above (Please briefly describe) not selected
# and create a 4 category race
# i'm going to just ignore the text open-end, and create hispanic, nonhisp white, nonhisp black, other non-hispanic
# but first i'm goign to create new categorical variables so I can see the distribution of race better
# but a count variable first because you could check all 6, i just want to seevand then categorical

PSW_data <- PSW_data |>
  mutate(
    n_races = rowSums(across(race_1:race_6, ~ !is.na(.) & . == 1)),
    race_cat = case_when(
      hispanic == 1 ~ "Hispanic",
      n_races >= 2 ~ "More than one race",
      race_1 == 1 ~ "White",
      race_2 == 1 ~ "Black",
      race_3 == 1 ~ "American Indian/Alaskan Native",
      race_4 == 1 ~ "Asian",
      race_5 == 1  ~ "Native Hawaiian/Pacific Islander",
      race_6 == 1 ~ "Other",
      n_races == 0 ~ NA_character_,
      TRUE ~ NA_character_
      ),
    race_cat = factor(
      race_cat, levels = c(
        "Hispanic", "White", "Black", "American Indian/Alaskan Native", "Asian", 
        "Native Hawaiian/Pacific Islander", "More than one race", "Other"
))) 
table(PSW_data$race_cat, useNA="ifany")

# ok it looks good, so I am goign to use this variable and collapse to create 
# a new race/ethnicity categorical that is hispanic, nonhisp white, nonshisp black, nonhispanic other
# because the distribution of other races is small
PSW_data <- PSW_data |>
  mutate(
    raceethnicity_cat =  case_when(
      race_cat == "Hispanic" ~ 2,
      race_cat == "White"  ~ 0,
      race_cat == "Black" ~ 1,
      race_cat %in% c("American Indian/Alaskan Native", "Asian", 
        "Native Hawaiian/Pacific Islander", "More than one race", "Other") ~ 3,
      TRUE ~ NA_real_
    ),
    raceethnicity_cat = labelled(
      raceethnicity_cat,
      c("Hispanic" = 2, "Non-Hispanic White" = 0, "Non-Hispanic Black" = 1, 
        "Non-Hispanic Other" = 3
        )
      )
    ) 
table(PSW_data$raceethnicity_cat, useNA="ifany")

#  3) education
# Education
# coded 1	Some high school or less 2	GED, ARNG, or alternative education certificate 3	High school graduate
# 4	Some post high school education, no degree 5	Associate degree (academic, occupational, technical, or vocational program)
# 6	Bachelor’s degree (e.g., BA, AB, BS, BBA) 7	Master’s, Doctoral, or Professional degree (e.g., MA, MS, MBA, PhD, ScD, MD, JD)
# -99	Seen but unanswered
# just look to see the distribution, and then I Will create a categorical education 
# 
table(PSW_data$education, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
    education_cat = case_when(
      education %in% c(1, 2, 3)  ~ 0,
      education == 4             ~ 1,
      education == 5             ~ 2,
      education %in% c(6, 7)     ~ 3,
      TRUE ~ NA_real_      
    ), 
    education_cat = labelled(
      education_cat,
      c("High school or GED" = 0, "Some post high school, no degree" = 1, 
        "Associate degree" = 2, "Bachelor degree or higher" = 3
        )
      )
    ) 
table(PSW_data$education_cat, useNA="ifany")

# 4) marital status 
# maritalstatus: answered by everyone
# 1	Married 2	Separated 3	Divorced 4	Widowed 5	Never married -99	Seen but unanswered
# relatstatus: missing if maritalstatus = 1 "married"       
# 1	Living with someone in a marriage-like relationship or engaged to be married
# 2 Steadily dating one person, but not engaged
# 3	Dating one or more people, but not in one steady relationship 4	Not currently dating
# -99	Seen but unanswered

#look at a table of maritalstatus x relationstatus
table(PSW_data$maritalstatus, PSW_data$relatstatus, useNA="ifany")
table(PSW_data$relatstatus, useNA="ifany")

#collapse to married/engaged/cohabit, previously married, never married. there are
#some people who quit the survey before this point so there will be NA's
PSW_data <- PSW_data |>
    mutate(
      maritalrelat_new = case_when(
            maritalstatus == 1 | relatstatus == 1 ~ 0,
            maritalstatus %in% c(2, 3, 4) ~ 1,
            maritalstatus == 5 ~ 2,
            TRUE ~ NA_real_
        ),
        maritalrelat_new = labelled(
          maritalrelat_new,
          c(
            "Married/engaged/cohabit" = 0,
            "Previously married" = 1,
            "Never married" = 2
        )
    )
    )
table(PSW_data$maritalrelat_new, useNA="ifany")

##############################------------------------ PSW basics------------------------############
#1) service sector, status
# PSW service sector: psw_servicesector is a Qualtrics clean constructed of safety_wrkstatus
# PSW status: psw_status_4cat is a Qualtrics constructed of safety_wrktype
# look at distributions of these
table(PSW_data$psw_servicesector, PSW_data$safety_wrktype, useNA="ifany")
table(PSW_data$psw_status_4cat, PSW_data$safety_wrkstatus, useNA="ifany")
table(PSW_data$psw_servicesector, useNA="ifany")

#create updated 4-category, other will be NA
PSW_data <- PSW_data |>
  mutate(
    psw_service4cat = case_when(
      psw_servicesector == 1 ~ 1,
      psw_servicesector == 2 ~ 0,
      psw_servicesector == 3 ~ 2,
      psw_servicesector == 4 ~ 3,
      TRUE ~ NA_real_      
    ), 
    psw_service4cat = labelled(
      psw_service4cat,
      c("Police" = 0, "Fire" = 1, 
        "EMS" = 2, "911 dispatcher" = 3
      )
    )
  ) 

table(PSW_data$psw_service4cat, useNA="ifany")

#create updated 3-category, other will be NA
table(PSW_data$psw_status_4cat, useNA="ifany")
PSW_data <- PSW_data |>
  mutate(
    psw_status3cat = case_when(
      psw_status_4cat == 1 ~ 0,
      psw_status_4cat == 2 ~ 1,
      psw_status_4cat == 3 ~ 2,
      TRUE ~ NA_real_      
    ), 
    psw_status3cat = labelled(
      psw_status3cat,
      c("Current" = 0, "Retired" = 1, 
        "Disabled" = 2
      )
    )
  ) 
table(PSW_data$psw_status3cat, useNA="ifany")

# 2) number of people, look at missings for num ppl - i'm going to leave it continouous for now: 
table(PSW_data$safety_numppl_1, useNA="ifany")
#110 with -99, 
PSW_data <- PSW_data |>
    mutate(safety_numppl_1 = ifelse(safety_numppl_1 == -99, NA_real_, safety_numppl_1)    
           )
table(PSW_data$safety_numppl_1, useNA="ifany")

##############################------------------------ work stressors-------##############################
# these are only answered if Safety_WrkStatus = 1
# so i will recode all -99 to NA
# the stem question : How much ongoing stress do you experience in each of these other areas of your psw?

#safetystrsa_1 - Inadequate staffing to do the job safely and effectively
#safetystrsa_2	- Inadequate equipment or facilities
#safetystrsa_3	- Conflicting demands or unclear expectations
#safetystrsa_4	- Favoritism or unfair treatment of workers by line supervisors
#safetystrsa_5	- Lack of input or voice in decisions that affect how you do your job
#safetystrsb_1	- Incompetent, poorly trained, or unmotivated supervisors
#safetystrsb_2	- Incompetent, poorly trained, or unmotivated coworkers
#safetystrsb_3	- A toxic work climate (e.g., gossip, disrespect, bullying, lack of trust)
#safetystrsb_4	- Excessive paperwork or administrative tasks that take time away from core duties
#safetystrsb_5	- Negative reactions, criticism, or lack of support from the public or media
# response options are 1	Extreme 2	A lot 3	Some 4	A little 5	None -99	Seen but unanswered
# so i want to just create a 10-item scale. I will do that later because I Want to look to see if they are all correlated
# to decide if I should create a scale
# but first I will recode to make the items 0-4 so 0=none and 4= extreme and recode -99 to na
table(PSW_data$safetystrsa_1, useNA="ifany")
PSW_data <- PSW_data |>
  mutate(
    across(
      safetystrsa_1:safetystrsb_5, 
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$safetystrsb_5_rev, useNA="ifany")
# ------------------------Traumatic events ------------------------
# the TEs were answered based on the type of PSW as indicated in the survey
# so there are a lot of NA's. also these variabels are coded
    # 1	Very often (More than 10 times) 2	Many times (6-10 times) 3	A few times (3-5 times)
    # 4	Once or twice   5	Never   -99	Seen but unanswered
#so I want to reverse code, and also collapse and create count variables for everyone

# but reverse code and turn -99 to 0
PSW_data <- PSW_data |>
  mutate(
    across(telawenfa_1:telawenfc_4,
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$telawenfc_4_rev, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
    across(
      tefirea_1:tefirec_4,
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$tefirec_4, useNA="ifany")
table(PSW_data$tefirec_4_rev, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
    across(
      tefireemsa_1:tefireemsc_4,
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$tefireemsc_4, useNA="ifany")
table(PSW_data$tefireemsc_4_rev, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
    across(
      te911a_1:te911c_3,
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$te911c_3, useNA="ifany")
table(PSW_data$te911c_3_rev, useNA="ifany")

PSW_data <- PSW_data |>
  mutate(
    across(
      teothera_1:teotherb_5,
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$teotherb_5, useNA="ifany")
table(PSW_data$teotherb_5_rev, useNA="ifany")

#################### ------------------------PTSD ------------------------
# PTSD variables, recode to 0-4 instead of 1-5
PSW_data <- PSW_data |>
  mutate(
    across(
      c(ptsda_1:ptsd_dist,
      ),
      ~ 5 - na_if(., -99), 
      .names = "{.col}_rev"
    )
  )
table(PSW_data$ptsda_1, useNA="ifany")
table(PSW_data$ptsda_1_rev, useNA="ifany")

PSW_data |> 
  select(ptsda_1_rev:ptsd_dist_rev) |> 
  lapply(table, useNA = "ifany")
