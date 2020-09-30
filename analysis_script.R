# load libraries
library(tidyverse)
library(osfr)

# download and read in data
osf_retrieve_file('hxbe5') %>%
  osf_download()

numeric_data <- read_csv(here::here('/raw_numeric_data.csv'))
                      

# clean data  
cleaned_data <- numeric_data %>%  
                      filter(!is.na(quality)) %>% #only keep those who were randomized to a condition
                      filter(!is.na(study_prereg)) %>% #keep only those who filled out manipulation check
                      mutate(failed_manipulation_check = case_when((quality == 'high' | quality == 'low') & study_prereg == 1 ~ 1,
                                                                    quality == 'none' & study_prereg == 2 ~ 1,
                                                                    TRUE ~ 0)) %>%
                      mutate(career_stage = case_when(career_level == 1 | career_level == 2 | career_level == 3 ~ 1,
                                                      career_level == 4 | career_level == 5 ~ 2,
                                                      career_level == 6 ~ 3)) %>%
                      filter(failed_manipulation_check == 0) # remove those who failed manipulation check


