# ID529 Core Concepts Script 

# This script covers the following skills:
#   - Installing packages 
#   - Reading csv data in with readr::read_csv
#   - Using dplyr for group_by and summarize
#   - Creating factors, setting reference levels
#   - Creating plots with ggplot2 and saving them 
#   - Fitting a model
#   - Reporting on a model with gtsummary
#   - Pulling out coefficient estimates with broom::tidy
#   - Comparing multiple model estimates using ggplot2

# know how to install packages:
# install.packages("tidyverse")

# set up a project so we could use the {here} package

# dependencies ------------------------------------------------------------
install.packages("devtools") 
install.packages("tidyverse")
install.packages("broom")
install.packages("here")
install.packages("palmerpenguins")
install.packages("gtsummary")
devtools::install_github("ID529/ID529tutorials")

library(tidyverse)
library(broom)
library(here)
library(palmerpenguins)
library(gtsummary)

