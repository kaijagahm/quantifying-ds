# Script for Kaija to play around with the data
library(tidyverse)
library(tidytext)
library(lme4)

tar_load(inst_cleaned) # cleaned institution data
tar_load(courses_cleaned) # cleaned course data
tar_load(terms_vec) # vector of terms (temporary)

cleaned_all <- left_join(courses_cleaned, inst_cleaned, by = "institution_std") %>%
  mutate(course_id_full = paste(inst_code, dept_code, course_id, sep = ".")) %>%
  select(-c("institution.x", "institution.y"))

todetect <- cleaned_all %>%
  select(course_id_full, text_description)

detected <- todetect %>%
  cross_join(tibble(term = terms_vec)) %>%
  mutate(included = str_detect(text_description, fixed(term, ignore_case = TRUE)))

nrow(detected) == length(terms_vec)*nrow(todetect) # should be TRUE

table(detected$included)

# Analysis --------------------------------------------------------------------
# 1. How does DS instruction vary across institutions?
# H1. We expect that on average, data science skills are not well-represented in biology curricula
# M1. summarize the presence/absence or frequency of data science terms found within curricula for each major. Use summary statistics to report min, max, median, distribution
summ_bycourse <- detected %>%
  group_by(course_id_full) %>%
  summarize(prop_terms = mean(included)) %>%
  left_join(cleaned_all, by = "course_id_full")

set.seed(3)
codes <- sample(unique(summ_bycourse$inst_code), 6)
summ_bycourse %>%
  filter(!is.na(inst_code), inst_code %in% codes) %>%
  ggplot(aes(x = paste(major_department, major_concentration), y = prop_terms))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.5, width = 0.1)+
  facet_wrap(~inst_code, scales = "free_x")+
  labs(y = "Prop terms included",
       x = "Department")+
  ggtitle("Proportion of terms by institution/major")+
  theme(axis.text.x = element_blank()) # remove axis labels for now since majors are hard to read. Will need to come up with a summary.

summ_bymajor <- summ_bycourse %>%
  select(inst_code, major_department, major_concentration, degree, prop_terms) %>%
  distinct()
  
nrow(summ_bycourse)
nrow(summ_bymajor)

summ_byinst <- summ_bycourse %>%
  group_by(inst_code) %>%
  summarize(min = min(prop_terms),
            med = median(prop_terms),
            mean = mean(prop_terms),
            max = max(prop_terms),
            sd = sd(prop_terms))

summ_byinst %>%
  ggplot(aes(x = inst_code))+
  geom_pointrange(aes(ymin = mean-sd, ymax = mean+sd, y = mean), size = 0.25)+
  coord_flip()+
  theme_minimal()
  
# H2. We expect that there are more data science terms in course descriptions for courses taught internally (within major department) than adjacent or external at smaller and Baccalaureate institutions (than at larger and Doctoral institutions)
# M2a. Compare frequency of data science terms found within internal courses vs. external & adjacent courses at baccalaureate vs. MS/PhD institutions. T-test and boxplots?
summ_bycourse %>%
  filter(!is.na(inst_code), inst_code %in% codes) %>%
  ggplot(aes(x = home_department, y = prop_terms))+
  geom_boxplot(outlier.shape = NA, aes(fill = home_department))+
  geom_jitter(alpha = 0.5, width = 0.1)+
  facet_wrap(~inst_code, scales = "free_x")+
  labs(y = "Prop terms included",
       x = "Internal vs. external")+
  ggtitle("Proportion of terms by internal/external department")+
  theme(legend.position = "none") # no super obvious trends for this particular subset of institutions.

df <- summ_bycourse %>%
  select(inst_code, major_department, major_concentration, home_department, prop_terms) %>%
  mutate(major = paste(major_department, major_concentration)) %>%
  select(-c(major_department, major_concentration)) %>%
  mutate(dept_simplified = case_when(home_department == "Adjacent" ~ "External",
                                     .default = home_department)) %>%
  glimpse()

mod <- lmer(prop_terms ~ dept_simplified + (1|inst_code/major), data = df)
summary(mod)

# XXX need categories of institutions here

# M2b. Compare frequency of data science terms found within internal courses vs. external & adjacent courses at smaller vs larger institutions (what size cut-offs to use?). T-test or ANOVA and boxplots?
# M2c. Use term frequencies, course types, and institution characteristics in a PCA analysis (can include both continuous and categorical variables). Evaluate which variables are most loaded on PC1 and PC2, and variance explained, see if there is a pattern for how institutions/variables cluster in multivariate space.


# H3. We expect that there are more data science terms in course descriptions for major programs (including those taught within the dept or outside the dept) at larger and Doctoral institutions than at smaller and Baccalaureate institutions.

# M3a. Compare frequency of data science terms found within all major courses (major is unit of analysis) at baccalaureate vs. MS/PhD institutions. T-test and boxplots?
  
# M3b. Compare frequency of data science terms found within all major courses (major is unit of analysis) at smaller vs. larger i institutions (what is the cut-off for bins?). T-test and boxplots?





