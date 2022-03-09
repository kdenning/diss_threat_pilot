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
library(lubridate)

##### Importing the data #####

# This data has been manually reviewed for completion of PT task; filter will be applied at end of this .R document
# Some variables were removed from the original document in order to prevent identifiable data from being uploaded online: LocationLatitude, LocationLongitude, prolificID_typed
# wide_data <- import(data/"pilot_raw_removed4.csv")

get_clean_data <- function(wide_df){
  wide_df %>% 
  # Making relevant variables categorical & dates correct
  mutate(subid = as.factor(subid), 
         recorded_date = mdy_hm(recorded_date),
         condition = as.factor(condition),
         condition = fct_relevel(condition, 
                                 "warm_targ", 
                                 "control_targ", 
                                 "covid_targ", 
                                 "comp_targ", 
                                 "loss_targ"),
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
         explicit_targ = as.factor(dplyr::recode(explicit_targ,
                                                 `1` = "1",
                                                 `2` = "2",
                                                 `4` = "3",
                                                 `5` = "4")),
         explicit_group = as.factor(dplyr::recode(explicit_group,
                                                 `1` = "1",
                                                 `2` = "2",
                                                 `4` = "3",
                                                 `5` = "4")),
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
  # Making relevant variables numeric
  mutate_at(c("distance_coffee", "distance_town", "realistic_q", "symbolic_q", 
            "explicit_targ", "explicit_group", "pol_orient_1", "pol_orient_2",
            "pol_orient_3", "age"),
            list(~as.numeric(.))) %>% 
  # Filtering out incorrect manipulation check responses
  ## Loss target - should say "yes" to Trump, no info about profession or covid
  mutate(q_check_politics = case_when(condition == "loss_targ" & manip_check_politics == "Jen voted for Trump" ~ "Correct",
                                     condition == "loss_targ" & manip_check_politics == "No information" ~ "Somewhat correct",
                                     condition == "loss_targ" & manip_check_politics == "Don't remember" ~ "Incorrect",
                                     condition == "loss_targ" & manip_check_politics == "Jen voted for Biden" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_politics == "Jen voted for Trump" ~ "Correct",
                                     condition == "comp_targ" & manip_check_politics == "No information" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_politics == "Don't remember" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_politics == "Jen voted for Biden" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_politics == "Jen voted for Trump" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_politics == "No information" ~ "Correct",
                                     condition == "covid_targ" & manip_check_politics == "Don't remember" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_politics == "Jen voted for Biden" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_politics == "Jen voted for Trump" ~ "Correct",
                                     condition == "warm_targ" & manip_check_politics == "No information" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_politics == "Don't remember" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_politics == "Jen voted for Biden" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_politics == "Jen voted for Trump" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_politics == "No information" ~ "Correct",
                                     condition == "control_targ" & manip_check_politics == "Don't remember" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_politics == "Jen voted for Biden" ~ "Incorrect"),
         q_check_covid = case_when(condition == "loss_targ" & manip_check_covid == "Jen does not support vaccination" ~ "Incorrect",
                                   condition == "loss_targ" & manip_check_covid == "No information" ~ "Correct",
                                   condition == "loss_targ" & manip_check_covid == "Don't remember" ~ "Incorrect",
                                   condition == "comp_targ" & manip_check_covid == "Jen does not support vaccination" ~ "Incorrect",
                                   condition == "comp_targ" & manip_check_covid == "No information" ~ "Correct",
                                   condition == "comp_targ" & manip_check_covid == "Don't remember" ~ "Incorrect",
                                   condition == "covid_targ" & manip_check_covid == "Jen does not support vaccination" ~ "Correct",
                                   condition == "covid_targ" & manip_check_covid == "No information" ~ "Incorrect",
                                   condition == "covid_targ" & manip_check_covid == "Don't remember" ~ "Incorrect",
                                   condition == "warm_targ" & manip_check_covid == "Jen does not support vaccination" ~ "Incorrect",
                                   condition == "warm_targ" & manip_check_covid == "No information" ~ "Correct",
                                   condition == "warm_targ" & manip_check_covid == "Don't remember" ~ "Incorrect",
                                   condition == "control_targ" & manip_check_covid == "Jen does not support vaccination" ~ "Incorrect",
                                   condition == "control_targ" & manip_check_covid == "No information" ~ "Correct",
                                   condition == "control_targ" & manip_check_covid == "Don't remember" ~ "Incorrect"),
         q_check_profess = case_when(condition == "loss_targ" & manip_check_profess == "Jen is a business manager" ~ "Incorrect",
                                     condition == "loss_targ" & manip_check_profess == "Jen is a teacher" ~ "Incorrect",
                                     condition == "loss_targ" & manip_check_profess == "No information" ~ "Correct",
                                     condition == "loss_targ" & manip_check_profess == "Don't remember" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_profess == "Jen is a business manager" ~ "Correct",
                                     condition == "comp_targ" & manip_check_profess == "Jen is a teacher" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_profess == "No information" ~ "Incorrect",
                                     condition == "comp_targ" & manip_check_profess == "Don't remember" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_profess == "Jen is a business manager" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_profess == "Jen is a teacher" ~ "Incorrect",
                                     condition == "covid_targ" & manip_check_profess == "No information" ~ "Correct",
                                     condition == "covid_targ" & manip_check_profess == "Don't remember" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_profess == "Jen is a business manager" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_profess == "Jen is a teacher" ~ "Correct",
                                     condition == "warm_targ" & manip_check_profess == "No information" ~ "Incorrect",
                                     condition == "warm_targ" & manip_check_profess == "Don't remember" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_profess == "Jen is a business manager" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_profess == "Jen is a teacher" ~ "Correct",
                                     condition == "control_targ" & manip_check_profess == "No information" ~ "Incorrect",
                                     condition == "control_targ" & manip_check_profess == "Don't remember" ~ "Incorrect")) %>% 
  filter(q_check_politics != "Incorrect" & q_check_covid == "Correct" & q_check_profess == "Correct") %>% 
    filter(jen_check == "Jen")
}
            
# Testing function
# clean_data <- get_clean_data(wide_data)  
  

# Notes
## Remember to ADD condition info to updated study on Human Subjects Pool!!!! Embedded conditon info
## Discuss political manipulation check q for loss targ with Sara


