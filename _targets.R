# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tidytext") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(institution_data, read_csv("data/raw/institutions.csv")),
  tar_target(course_data, read_csv("data/raw/Biology Curriculum Data Availability - course_descriptions_2026-09-22.csv")),
  tar_target(inst_cleaned, clean_institution_data(institution_data)),
  tar_target(courses_cleaned, clean_course_data(course_data)),
  tar_target(terms_temp, read_csv("data/raw/terms_list_temporary.csv")),
  tar_target(terms_vec, get_terms_vector(terms_temp))
)
