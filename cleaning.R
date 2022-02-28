###########################
### Cleaning Pilot Data ###
###########################

##### Notes about original data #####

# 251 participants originally collected from Prolific Academic and 1 preview from me
# Removed 3 had empty data lines & 1 preview = 248 in imported data-set

#### Packages needed #####

#install.packages("magrittr")
#install.packages("tidyverse")
#install.packages("rio")

# Loading packages
library(magrittr)
library(tidyverse)
library(rio)

##### Importing the data #####

# This data has been manually reviewed for completion of PT task; filter will be applied at end of this .R document
# Some variables were removed from the original document in order to prevent identifiable data from being uploaded online: LocationLatitude, LocationLongitude, prolificID_typed
wide_data <- import("pilot_raw_removed4.csv")
clean_data <- wide_data %>% 
  mutate(subid = as.factor(subid),
         vote_check = as.factor(dplyr::recode(vote_check,
                                    `1` = "Trump",
                                    `2` = "Biden", # Prolific pre-screen worked and there is only one level of Biden supporters
                                    `3` = "Other")),
         consent = as.factor(dplyr::recode(consent,
                                           `1` ="Yes", # Pre-screen worked and only people who consented participated
                                           `2` = "No")),
         manip_check_politics = as.factor(dplyr::recode(manip_check_politics,
                                                        `1` = "No information",
                                                        `2` = "Jen voted for Trump",
                                                        `3` = "Jen voted for Biden",
                                                        `4` = "Don't remember")),
         manip_check_covid = as.factor(dplyr::recode(manip_check_covid,
                                                     `3` = "No information",
                                                     `1` = "Jen supports vaccination",
                                                     `2` = "Jen does not support vaccination",
                                                     `4` = "Don't remember")),
         manip_check_profess = as.factor(dplyr::recode(manip_check_profess,
                                                       `4` = "No information",
                                                       `1` = "Jen is a teacher",
                                                       `3` = "Jen is a business manager",
                                                       `5` = "Don't remember")),
         jen_check = as.factor(dplyr::recode(jen_check,
                                             `1` = "Jen", # Jen is correct answer
                                             `2` = "Myself/Participant",
                                             `3` = "Trump supporters",
                                             `4` = "Biden supporters")),
         gender = as.factor(dplyr::recode(gender,
                                          `1` = "Man",
                                          `2` = "Woman",
                                          `3` = "Other")),
         education = as.factor(dplyr::recode(education,
                                             `1` = "Less than high school",
                                             `2` = "High School",
                                             `3` = "Some college",
                                             `4` = "2 years/Associate's degree",
                                             `5` = "4 years/Bachelor's degree",
                                             `6` = "Some graduate school",
                                             `7` = "Master's degree",
                                             `8` = "Doctorate or Professional degree")),
         parent_education = as.factor(dplyr::recode(parent_education,
                                                    `20` = "Less than high school",
                                                    `21` = "High School",
                                                    `22` = "Some college",
                                                    `23` = "2 years/Associate's degree",
                                                    `24` = "4 years/Bachelor's degree",
                                                    `26` = "Some graduate school",
                                                    `1` = "Master's degree",
                                                    `2` = "Doctorate or Professional degree")),
         race = as.factor(dplyr::recode(race,
                                        `1` = "American Indian/Alaska Native",
                                        `2` = "Asian/Asian American",
                                        `3` = "Black/African American",
                                        `4` = "Latina/o or Hispanic or Chicano/a or Puerto Rican",
                                        `5` = "Middle Eastern or North African",
                                        `6` = "White or European American",
                                        `7` = "Multiracial",
                                        `8` = "Other")),
         country_birth = as.factor(dplyr::recode(country_birth,
                                                 `1` = "US is birth country",
                                                 `2` = "No, US is not birth country")),
         country_raised = as.factor(dplyr::recode(country_raised,
                                                  `1` = "Raised in US",
                                                  `2` = "Primarily raised in another country")),
         native_language = as.factor(dplyr::recode(native_language,
                                                   `1` = "English",
                                                   `2` = "Not English"))) %>% 
  mutate_at(c("distance_coffee", "distance_town", "realistic_q", "symbolic_q",
            "explicit_targ", "explicit_group", "pol_orient_1", "pol_orient_2",
            "pol_orient_3", "age"),
            list(~as.numeric(.)))

  

# Need to go to Qualtrics' and find the condition
## Remember to ADD condition info to updated study on Human Subjects Pool!!!! Embedded conditon info


