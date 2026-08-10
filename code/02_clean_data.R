# data manipulation -------------------------------------------------------

# use group_by and summarize together to create summary statistics per-group
penguins_summarized <- penguins |>
    group_by(species) |>
    summarize(
        mean_flipper_length_mm = mean(flipper_length_mm, na.rm=TRUE))

# know how to use mutate to update columns (either creating new ones or updating
# existing ones):
# here, we'll just convert species to a character vector just for an example
# so then we can next practice making it a factor:
penguins <- penguins |>
    mutate(species = as.character(species))

# convert a variable to a factor:
#
# method 1: base R
# here, the levels will be assumed from the output of unique(penguins$species):
penguins$species <- factor(penguins$species)
#
# method 2: dplyr
penguins <- penguins |>
    mutate(species = factor(species))

# if I wanted to change the reference category, I could use relevel:
penguins$species <- relevel(penguins$species, 'Chinstrap')

# or the dplyr way:
penguins <- penguins |>
    mutate(species = relevel(species, 'Chinstrap'))
# you can also use forcats::fct_relevel