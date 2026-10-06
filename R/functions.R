# Functions

## Data cleaning
### Institution data and course data
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
  return(test_inst)
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
    mutate(institution_std = harmonize_inst(institution)) %>%
    mutate(inst_code = case_when(institution_std == "Black Hills State University" ~ "BlackHillsState",
                                 institution_std == "Brandeis University" ~ "Brandeis",
                                 institution_std == "Bridgewater State University" ~ "BridgewaterState",
                                 institution_std == "Brigham Young University - Hawaii" ~ "BrighamYoungHI",
                                 institution_std == "California Baptist University" ~ "CABaptist",
                                 institution_std == "Central Methodist University - Fayette Campus" ~ "CentralMethodistFayette",
                                 institution_std == "Claflin University" ~ "Claflin",
                                 institution_std == "Colby-Sawyer College" ~ "ColbySawyer",
                                 institution_std == "Concordia University Irvine" ~ "ConcordiaIrvine",
                                 institution_std == "Coppin State University" ~ "CoppinState",
                                 institution_std == "Dean College" ~ "Dean",
                                 institution_std == "Eastern Oregon University" ~ "EasternOR",
                                 institution_std == "Florida International University" ~"FLInternational",
                                 institution_std == "Fordham University" ~ "Fordham",
                                 institution_std == "Georgia Institute of Technology" ~ "GATech",
                                 institution_std == "Georgia State University" ~ "GAState",
                                 institution_std == "Gonzaga University" ~ "Gonzaga",
                                 institution_std == "Hofstra University" ~ "Hofstra",
                                 institution_std == "Idaho State University" ~ "IDState",
                                 institution_std == "Jarvis Christian University" ~ "JarvisChristian",
                                 institution_std == "Missouri State University" ~ "MOState",
                                 institution_std == "Northern Arizona University" ~ "NorthernAZ",
                                 institution_std == "Pace University" ~ "Pace",
                                 institution_std == "Post University" ~ "Post",
                                 institution_std == "Roger Williams University" ~ "RogerWilliams",
                                 institution_std == "Saint Anselm College" ~ "SaintAnselm",
                                 institution_std == "San Diego State University" ~ "SanDiegoState",
                                 institution_std == "Schreiner University" ~ "Schreiner",
                                 institution_std == "Siena College" ~ "Siena",
                                 institution_std == "Simpson University" ~ "Simpson",
                                 institution_std == "Slippery Rock University of Pennsylvania" ~ "SlipperyRockPA",
                                 institution_std == "St. Catherine University" ~ "StCatherine",
                                 institution_std == "St. Thomas Aquinas College" ~ "StThomasAquinas",
                                 institution_std == "Stillman College" ~ "Stillman",
                                 institution_std == "The University of Montana Western" ~ "MTWestern",
                                 institution_std == "Union College" ~ "Union",
                                 institution_std == "University of California, Merced" ~ "CAMerced",
                                 institution_std == "University of California, Santa Barbara" ~ "CASantaBarbara",
                                 institution_std == "University of Cincinnati" ~ "Cincinnati",
                                 institution_std == "University of Detroit Mercy" ~ "DetroitMercy",
                                 institution_std == "University of Hartford" ~ "Hartford",
                                 institution_std == "University of Idaho" ~ "ID",
                                 institution_std == "University of Massachusetts - Amherst" ~ "MAAmherst",
                                 institution_std == "University of Montevallo" ~ "Montevallo",
                                 institution_std == "University of North Carolina Wilmington" ~ "NCWilmington",
                                 institution_std == "University of Portland" ~ "Portland",
                                 institution_std == "University of Richmond" ~ "Richmond",
                                 institution_std == "Vanguard University of Southern California" ~ "VanguardSouthernCA",
                                 institution_std == "Wisconsin Lutheran College" ~ "WILutheran",
                                 institution_std == "Denison University" ~ "Denison",
                                 institution_std == "Kean University" ~ "Kean",
                                 institution_std == "LaGrange College" ~ "LaGrange",
                                 institution_std == "Lehman College/CUNY" ~ "LehmanCUNY",
                                 institution_std == "Quinnipiac University" ~ "Quinnipiac",
                                 institution_std == "Rensselaer Polytechnic Institute" ~ "RensselaerPolytechnic",
                                 institution_std == "University of Tampa" ~ "Tampa",
                                 institution_std == "University of Washington - Tacoma" ~ "WATacoma",
                                 institution_std == "Husson University" ~ "Husson",
                                 institution_std == "University of Texas at El Paso" ~ "TXElPaso",
                                 institution_std == "Central Methodist University - Statewide Campus" ~ "CentralMethodistStatewide",
                                 institution_std == "Utah State University" ~ "UTState",
                                 .default = NA))
  return(test_course)
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

### Research terms
get_terms_vector <- function(terms_df){
  terms_df <- terms_df %>%
    rename("term" = "Term",
           "full_term" = "Full term") %>%
    select(term, full_term) %>%
    mutate(across(everything(), tolower)) %>%
    unnest_tokens("term", term)
  terms_vec <- sort(unique(terms_df$term))
  return(terms_vec)
}
