# Script for Kaija to play around with the data
library(tidyverse)

dat <- read_csv("data/raw/Biology Curriculum Data Availability - course_descriptions_2026-09-22.csv")
glimpse(dat)

# Cleaning
## Clean up column names
dat <- dat %>%
  rename("date_access" = `Date of access`,
         "institution" = Institution,
         "major_requirement" = `major requirement`,
         "special_topics_or_rare_offering" = `special topics or rare offering`) %>%
  mutate(date_access = lubridate::mdy(date_access))

## Clean and standardize column contents/factor levels
### Institution name
sort(unique(dat$institution))
dat$institution[dat$institution == "bridgewater State University"] <- "Bridgewater State University"

### degree
sort(unique(dat$degree))
table(dat$degree)
dat <- dat %>%
  mutate(degree = str_remove_all(degree, "\\."))

sort(unique(dat$department_name))
dat$department_name[dat$department_name == "Chemistry and Chemical Biology"] <- "Chemistry and Chemical Biology"
dat$department_name[dat$department_name == "Chemistry and CHemical Biology"] <- "Chemistry and Chemical Biology"
dat$department_name[dat$department_name == "Geography, Planning and Recreation"] <- "Geography, Planning, and Recreation"


