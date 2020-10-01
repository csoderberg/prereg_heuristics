# load libraries
library(tidyverse)
library(osfr)
library(BayesFactor)

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
                  filter(participant_id != 24444 & participant_id != 27200) # remove 2nd and 3rd time individual subject was erroneasly able ot take survey 3 times
                  
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
  filter(prereg_link == 2) %>%
  group_by(prereg_read) %>%
  tally()

## Pre-registered Analyses

### Primary RQ1
test <- anovaBF(study_credibility ~ quality, data = cleaned_data %>% filter(!is.na(study_credibility)) %>% mutate(quality = as.factor(quality)))
