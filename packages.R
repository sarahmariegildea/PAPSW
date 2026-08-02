# install devtools if you haven't already
install.packages("devtools") 

# install our ID529tutorials package
devtools::install_github("ID529/ID529tutorials")

library(ID529tutorials)
available_tutorials('ID529tutorials')
run_tutorial('dataframes_and_matrices', 'ID529tutorials')

install.packages("library")
install.packages("renv")
installed.packages()
