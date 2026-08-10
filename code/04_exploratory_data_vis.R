# data visualization ------------------------------------------------------

# use ggplot2 to make some graphics
# a scatter plot:
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
    geom_point() +
    ggtitle("Penguin Bill Lengths and Depths by Species")

# use ggsave to save your work
ggsave(here("output/penguins_scatterplot.png"), width = 7, height = 5)

# a histogram with facets:
ggplot(penguins, aes(x = flipper_length_mm)) +
    geom_histogram() +
    facet_wrap(~species) +
    ggtitle("Penguin Bill Lengths and Depths by Species")

# again use ggsave and here() to save it within your project
ggsave(here("output/penguins_faceted_histogram.png"), width = 8, height = 3)


# you might also want to plot regression lines in ggplot quickly so
# use geom_smooth:
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    ggtitle("Linear regression of flipper length on body mass")