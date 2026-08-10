# analyze a model -------------------------------------------------------------

model <- lm(flipper_length_mm ~ body_mass_g + species, penguins)

# use broom::tidy to extract the coefficients and their statistics
model_output <- broom::tidy(model, conf.int = TRUE)

# visualize model results
model_output |>
    filter(term != '(Intercept)') |>
    ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
    geom_pointrange()

# create a table of the results
gtsummary::tbl_regression(model)


# one example with multiple models --------------------------------------------

model1 <- lm(flipper_length_mm ~ species, penguins)
model2 <- lm(flipper_length_mm ~ species + body_mass_g, penguins)
model3 <- lm(flipper_length_mm ~ species + body_mass_g + island, penguins)

# extract tables of results
model_results <- list(
    bind_cols(model = 'model1', broom::tidy(model1, conf.int = TRUE)),
    bind_cols(model = 'model2', broom::tidy(model2, conf.int = TRUE)),
    bind_cols(model = 'model3', broom::tidy(model3, conf.int = TRUE)))

# make into one data frame
model_results <- bind_rows(model_results)

# create a plot of covariates from multiple models
model_results |>
    filter(term %in% c('speciesGentoo', 'speciesAdelie')) |>
    ggplot(
        aes(x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            color = model,
            shape = model)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    ggtitle("Coefficient estimates for species effect",
            stringr::str_wrap(
                paste(
                    "Model 1 includes no other covariates, model 2 includes body mass,",
                    "and model 3 includes body mass and island effects"
                )
            ))