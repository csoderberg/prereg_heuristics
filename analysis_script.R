# load libraries
library(tidyverse)
library(osfr)
library(brms)
library(bayestestR)
library(emmeans)

# download and read in data
osf_retrieve_file('hxbe5') %>%
  osf_download()

numeric_data <- read_csv(here::here('/raw_numeric_data.csv'))
                      

# clean data  
numeric_data <- numeric_data %>%  
                      filter(!is.na(quality)) %>% #only keep those who were randomized to a condition
                      filter(!is.na(study_prereg)) %>% #keep only those who filled out manipulation check
                      mutate(failed_manipulation_check = case_when((quality == 'high' | quality == 'low') & study_prereg == 1 ~ 'yes',
                                                                    quality == 'none' & study_prereg == 2 ~ 'yes',
                                                                    TRUE ~ 'no')) %>%
                      mutate(career_stage = case_when(career_level == 1 | career_level == 2 | career_level == 3 ~ 1,
                                                      career_level == 4 | career_level == 5 ~ 2,
                                                      career_level == 6 ~ 3)) %>%
                      # create variables for whether respondents got a pre-reg cue or read prereg
                      mutate(prereg_cue = case_when(quality == 'high' | quality == 'low' ~ 'yes',
                                                    quality == 'none' ~ 'no'),
                             read_prereg = case_when((quality == 'high' | quality == 'low') & prereg_read == 2 ~ 'yes',
                                                     (quality == 'high' | quality == 'low') & (prereg_read == 1 | is.na(prereg_read)) ~ 'no'))

cleaned_data <- numeric_data %>%
                  filter(failed_manipulation_check == 'no') %>%  # remove those who failed manipulation check
                  filter(participant_id != 24444 & participant_id != 27200) %>% # remove 2nd and 3rd time individual subject was erroneasly able ot take survey 3 times
                  mutate(quality = as.factor(quality),
                         quality = fct_relevel(quality, c('none', 'low', 'high')),
                         vignette = as.factor(vignette),
                         career_stage = as.factor(career_stage))

prereg_cued_data <- cleaned_data %>%
  filter(prereg_cue == 'yes')

write_csv(cleaned_data, 'cleaned_data.csv')
write_csv(cleaned_data %>% filter(prereg_cue == 'yes'), 'cleaned_data_no_none.csv')
                  
## basic descriptives

# how many failed manipulation check?
numeric_data %>%
  group_by(quality, failed_manipulation_check) %>%
  tally()

# how many in each group opened pre-reg link
cleaned_data %>%
  filter(quality == 'high' | quality == 'low') %>%
  group_by(quality, prereg_link) %>%
  tally()

# of those who opened, how many read?
cleaned_data %>%
  filter((quality == 'high' | quality == 'low') & prereg_link == 2) %>%
  group_by(quality, prereg_read) %>%
  tally()

## Pre-registered Analyses

contrasts(cleaned_data$quality) <- contr.bayes
contrasts(cleaned_data$vignette) <- contr.bayes
contrasts(cleaned_data$career_stage) <- contr.bayes

### Primary RQs

# set up overall model (with default priors from JASP)
m0 <- brm(study_credibility ~ vignette + career_stage, data = cleaned_data, save_all_pars = TRUE, prior = c(set_prior("normal(0,.354)", class = "b", coef = "career_stage1"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "career_stage2"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "vignette1"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "vignette2"),
                                                                                                            set_prior("normal(0, .5)", class = "Intercept"),
                                                                                                            set_prior("normal(0,1)", "sigma")))
m1 <- brm(study_credibility ~ vignette + career_stage + quality, data = cleaned_data, save_all_pars = TRUE, prior = c(set_prior("normal(0,.354)", "b", "career_stage1"),
                                                                                                                      set_prior("normal(0,.354)", "b", "career_stage2"),
                                                                                                                      set_prior("normal(0,.354)", "b", "vignette1"),
                                                                                                                      set_prior("normal(0,.354)", "b", "vignette2"),
                                                                                                                      set_prior("normal(0,.5)", "b", "quality1"),
                                                                                                                      set_prior("normal(0,.5)", "b", "quality2"),
                                                                                                                      set_prior("normal(0, .5)", class = "Intercept"),
                                                                                                                      set_prior("normal(0,1)", "sigma")))
comparison <- bayesfactor_models(m1, denominator = m0)
comparison

# set up pairwise contrasts
quality_levels <- emmeans(m1, ~quality)
quality_diff <- pairs(quality_levels)
quality_all <- rbind(quality_levels, quality_diff)

bayesfactor_parameters(quality_all, prior = m1, direction = "two-sided")

### Secondary RQ1

contrasts(prereg_cued_data$vignette) <- contr.bayes
contrasts(prereg_cued_data$career_stage) <- contr.bayes

# set up overall model (with default priors from JASP)
q2_m0 <- brm(prereg_quality ~ vignette + career_stage, data = prereg_cued_data, save_all_pars = TRUE, prior = c(set_prior("normal(0,.354)", class = "b", coef = "career_stage1"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "career_stage2"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "vignette1"),
                                                                                                            set_prior("normal(0,.354)", class = "b", coef = "vignette2"),
                                                                                                            set_prior("normal(0,.5)", "Intercept"),
                                                                                                            set_prior("normal(0,1)", "sigma")))
q2_m1 <- brm(prereg_quality ~ vignette + career_stage + quality, data = prereg_cued_data, save_all_pars = TRUE, prior = c(set_prior("normal(0,.354)", "b", "career_stage1"),
                                                                                                                      set_prior("normal(0,.354)", "b", "career_stage2"),
                                                                                                                      set_prior("normal(0,.354)", "b", "vignette1"),
                                                                                                                      set_prior("normal(0,.354)", "b", "vignette2"),
                                                                                                                      set_prior("normal(0,.5)", "b", "qualityhigh"),
                                                                                                                      set_prior("normal(0,.5)", "Intercept"),
                                                                                                                      set_prior("normal(0,1)", "sigma")))

q2_comparison <- bayesfactor_models(q2_m1, denominator = q2_m0)
q2_comparison
