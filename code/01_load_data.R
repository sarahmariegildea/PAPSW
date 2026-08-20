# ID529 
# Sarah Gildea
# July/August 2026

# read in data ------------------------------------------------------------
# use read_csv

# read in the PA csv dataset and view it:
#PSW_df <- read_csv(here("The 2985 with Critera_DSM5_PTSD = 0 or 1.csv"))
#I came back here and reread because it was not importing correctly
PSW_df <- read.csv(here("The 2985 with Critera_DSM5_PTSD = 0 or 1.csv"))
str(PSW_df)
# keep these as characters
keep_char <-c("ResponseId", "DistributionChannel", "UserLanguage",
              "NOTFINISHED", "LAST_SCREEN", "LastVariable")

PSW_df <- PSW_df |>
    mutate(across(
        -c(any_of(keep_char), ends_with("Fill"), ends_with("Text")),
        ~as.numeric(.x)
    ))

# Print and glimpse at it, because there are "warning" messages " problems(dat) "
# and it looks like some variables types are logic which is probably because of missing/NA
PSW_df 
glimpse(PSW_df) 
problems(PSW_df) 
#look at structure of dataframe
str(PSW_df)
# all of the TE variables are stored as col_logical
#  ..   Safety_FireEMSHrs_1 = col_logical(),


#so i will reread it and force it to be character which I think is safer

# So it's expecting some variables to be a double type/number but it's a character
# I will tell R to store as character and the other varibles with errors as double/numberic
#PSW_df <- read_csv(here("The 2985 with Critera_DSM5_PTSD = 0 or 1.csv"),
#               col_types = cols(Race_6_TEXT = col_character(), 
#                                Safety_FireEMSHrs_1 = col_double()
#                                )
#)
#glimpse(PSW_df)
# that worked but there are still errors with the TE variables so I will do it again
#PSW_df <- PSW_df %>%
#    mutate(
#        across(TEFirea_1:TEFirec_4, as.numeric),
#        across(TEFireEMSa_1:TEFireEMSc_4, as.numeric),
#        across(TEEMSa_1:TEEMSc_4, as.numeric),
#        across(TE911a_1:TE911c_3, as.numeric)
#    )
#view(PSW_df)
#glimpse(PSW_df)

#that worked
#
