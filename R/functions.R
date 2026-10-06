# Functions

## Data cleaning
### Institutions
clean_institution_data <- function(institution_data){
  test_inst <- institution_data %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols")) %>%
    rename("level" = "acreditation_level",
           "region" = "accreditation_region") %>%
    filter(include != "NO") %>%
    select(institution, include, category, region, level) %>%
    mutate(across(where(is.character), ~ str_squish(.))) %>%
    mutate(institution_std = harmonize_inst(institution))
}

clean_course_data <- function(course_data){
  test_course <- course_data %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols")) %>%
    mutate(date_of_access = lubridate::mdy(date_of_access),
           major_requirement = case_when(major_requirement == "Major" ~ "major requirement",
                                         major_requirement == "Restricted elective" ~ "restricted elective",
                                         .default = major_requirement),
           division = case_when(division == "intro" ~ "lower",
                                division == "Lower" ~ "lower",
                                division == "Upper" ~ "upper",
                                .default = division),
           major_requirement = factor(major_requirement, levels = c("major requirement", "restricted elective", "free elective")),
           division = factor(division, levels = c("lower", "upper"))) %>%
    select(-c("res_elective_how_many", "res_elective_out_of",
              "x17", "notes", "special_topics_or_rare_offering")) %>%
    mutate(lab_course = lab_course == "Y",
           text_description = tolower(text_description)) %>%
    mutate(institution_std = harmonize_inst(institution))
}

crosswalk <- c(
  "Brigham Young University - Hawaii - Laie"  = "Brigham Young University - Hawaii",
  "Central Methodist University"              = "Central Methodist University - Fayette Campus",
  "Missouri State University-Springfield"     = "Missouri State University",
  "University of Washington-Tacoma"           = "University of Washington - Tacoma",
  "UMass - Amherst"                           = "University of Massachusetts - Amherst",
  "University of North Carolina, Wilmington"  = "University of North Carolina Wilmington",
  "Stilman College - Alabama"                 = "Stillman College",
  "bridgewater State University" = "Bridgewater State University",
  "Stilman College"                           = "Stillman College",
  "Denison Unviersity"                        = "Denison University",
  "Lagrange College"                          = "LaGrange College",
  "Vanguard University"                       = "Vanguard University of Southern California",
  "UC Santa Barbara"                          = "University of California, Santa Barbara"
)

harmonize_inst <- function(x) {
  x <- str_squish(x)
  
  # Only strip a location tail when a ", ST" state code is present
  has_state <- str_detect(x, ",\\s*[A-Z]{2}$")
  stripped  <- x %>%
    str_remove(",\\s*[A-Z]{2}$") %>%                 # drop ", SD"
    str_remove("(\\s+-\\s+|,\\s+)[^,-]+$")           # drop " - City" or ", City"
  x <- if_else(has_state, stripped, x)
  
  # Apply manual crosswalk; anything not listed passes through unchanged
  coalesce(unname(crosswalk[x]), x)
}
### Courses