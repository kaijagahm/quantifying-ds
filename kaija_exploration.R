# Script for Kaija to play around with the data
library(tidyverse)
library(tidytext)

dat <- read_csv("data/raw/Biology Curriculum Data Availability - course_descriptions_2026-09-22.csv")
terms <- read_csv("data/raw/terms_list_temporary.csv")
terms <- terms %>%
  rename("term" = "Term",
         "full_term" = "Full term") %>%
  select(term, full_term) %>%
  mutate(across(everything(), tolower)) %>%
  unnest_tokens("term", term)
terms <- sort(unique(terms$term))
glimpse(dat)

# 00_Data Cleaning --------------------------------------------------------
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

# minimize data
dat_min <- dat %>%
  select(-c("date_access", "res_elective_how_many", "res_elective_out_of", "...17", "notes", "special_topics_or_rare_offering", "...20", "...21")) %>%
  mutate(mj_id = str_remove_all(paste(institution, major_department, major_concentration, degree, sep = "_"), " "),
         course_id = str_remove_all(paste(mj_id, department_name, home_department, dept_code, courseID, major_requirement, division, lab_course, course_name, sep = "_"), " "))

todetect <- dat_min %>%
  select(course_id, text_description)

detected <- todetect %>%
  cross_join(tibble(term = terms)) %>%
  mutate(included = str_detect(text_description, fixed(term, ignore_case = TRUE)))
nrow(dat_min)
length(terms)
nrow(detected) == length(terms)*nrow(dat_min) # should be TRUE

table(detected$included)

# Analysis --------------------------------------------------------------------
# 1. How does DS instruction vary across institutions?
# H1. We expect that on average, data science skills are not well-represented in biology curricula
# M1. summarize the presence/absence or frequency of data science terms found within curricula for each major. Use summary statistics to report min, max, median, distribution
summ_bycourse <- detected %>%
  group_by(course_id) %>%
  summarize(prop_terms = mean(included)) %>%
  left_join(dat_min, by = "course_id")

set.seed(3)
insts <- sample(unique(summ_bycourse$institution), 6)
summ_bycourse %>%
  filter(!is.na(institution), institution %in% insts) %>%
  ggplot(aes(x = paste(major_department, major_concentration), y = prop_terms))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.5, width = 0.1)+
  facet_wrap(~institution, scales = "free_x")+
  labs(y = "Prop terms included",
       x = "Department")+
  ggtitle("Proportion of terms by institution/major")

summ_bymajor <- summ_bycourse %>%
  select(institution, major_department, major_concentration, degree, prop_terms) %>%
  distinct()
  
nrow(summ_bycourse)
nrow(summ_bymajor)

summ_byinst <- summ_bycourse %>%
  group_by(institution) %>%
  summarize(min = min(prop_terms),
            med = median(prop_terms),
            mean = mean(prop_terms),
            max = max(prop_terms),
            sd = sd(prop_terms))

summ_byinst %>%
  ggplot(aes(x = institution))+
  geom_pointrange(aes(ymin = mean-sd, ymax = mean+sd, y = mean), size = 0.25)+
  coord_flip()+
  theme_minimal()
  
# H2. We expect that there are more data science terms in course descriptions for courses taught internally (within major department) than adjacent or external at smaller and Baccalaureate institutions (than at larger and Doctoral institutions)
# M2a. Compare frequency of data science terms found within internal courses vs. external & adjacent courses at baccalaureate vs. MS/PhD institutions. T-test and boxplots?
# M2b. Compare frequency of data science terms found within internal courses vs. external & adjacent courses at smaller vs larger institutions (what size cut-offs to use?). T-test or ANOVA and boxplots?
# M2c. Use term frequencies, course types, and institution characteristics in a PCA analysis (can include both continuous and categorical variables). Evaluate which variables are most loaded on PC1 and PC2, and variance explained, see if there is a pattern for how institutions/variables cluster in multivariate space.


# H3. We expect that there are more data science terms in course descriptions for major programs (including those taught within the dept or outside the dept) at larger and Doctoral institutions than at smaller and Baccalaureate institutions.

# M3a. Compare frequency of data science terms found within all major courses (major is unit of analysis) at baccalaureate vs. MS/PhD institutions. T-test and boxplots?
  
# M3b. Compare frequency of data science terms found within all major courses (major is unit of analysis) at smaller vs. larger i institutions (what is the cut-off for bins?). T-test and boxplots?





