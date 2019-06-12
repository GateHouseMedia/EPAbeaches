library(tidyverse)
library(janitor)
library(here)

beach_actions <- read_csv(here("beaches_code", "beach_actions_(advisories_and_closures).csv")) %>% 
  clean_names() 

beach_actions_clean <- beach_actions %>% 
  mutate(state = str_replace(state, "BR", "WI")) %>% 
  mutate(state = str_replace(state, "MK", "WA")) %>% 
  filter(state != "AS",
         state != "CN", 
         state != "GP",
         state != "GU",
         state != "MP",
         state != "VI")
  
beach_actions_clean2 <- beach_actions_clean %>% 
  separate_rows(action_reasons, sep = ",") %>% 
  separate_rows(action_indicator, sep = ",") %>% 
  separate_rows(action_possible_source, sep = ",") %>% 
  mutate(action_reasons = str_trim(action_reasons)) %>% 
  mutate(action_indicator = str_trim(action_indicator)) %>% 
  mutate(action_possible_source = str_trim(action_possible_source)) %>% 
  mutate(action_possible_source = str_replace(action_possible_source, "CSO", "COMBINED SEWER OVERFLOW")) %>% 
  mutate(action_possible_source = str_replace(action_possible_source, "SSO", "SANITARY SEWER OVERFLOW")) %>% 
  mutate(action_possible_source = str_replace(action_possible_source, "POTW", "PUBLICLY-OWNED TREATMENT WORKS")) %>% 
  mutate(action_possible_source = str_replace(action_possible_source, "SEWER_LINE", "SEWER LINE")) %>% 
  mutate(action_indicator = str_replace(action_indicator, "ENTERO", "ENTEROCOCCI")) %>% 
  mutate(action_indicator = str_replace(action_indicator, "PREEMPT", "PREEMPTORY ACTION")) %>% 
  mutate(action_indicator = str_replace(action_indicator, "FECAL_COL", "FECAL COLIFORM")) %>% 
  mutate(action_indicator = str_replace(action_indicator, "TOTAL_COL", "TOTAL COLIFORM")) %>% 
  mutate(action_indicator = str_replace(action_indicator, "RATIO", "FECAL-TO-TOTAL COLIFORM RATIO")) %>% 
  mutate(action_reasons = str_replace(action_reasons, "ELEV_BACT", "ELEVATED BACTERIA")) 
  
beach_actions_clean2 %>% 
  count(action_reasons) %>% 
  arrange(desc(n))

beach_actions_clean2 %>% 
  count(action_indicator) 
# Total coliforms include species that may inhabit the intestines of warm-blooded animals or occur naturally in soil, vegetation, and water. 

beach_actions_clean2 %>% 
  count(action_reasons) 

beach_actions_collapsed <- beach_actions_clean2 %>% 
  group_by(beach_id, beach_name, state, county) %>% 
  summarize(reasons = paste(toString(unique(action_reasons)), collapse = ","),
            indicators = paste(toString(unique(action_indicator)), collapse = ","),
            sources = paste(toString(unique(action_possible_source)), collapse = ","))

beach_numbers <- beach_actions_clean %>% 
  group_by(beach_id) %>% 
  summarize(no_of_beach_actions = n(),
            no_of_days_under_action = sum(action_duration_days),
            closure = sum(action_type == "Closure"),
            rain_advisory = sum(action_type == "Rain Advisory"),
            contamination_advisory = sum(action_type == "Contamination Advisory"))

all_beach_vars <- left_join(beach_actions_collapsed, beach_numbers, by = "beach_id")

write_csv(all_beach_vars, "all_beach_vars.csv")

### count number of actions by state
state_sums <- all_beach_vars 
  group_by(state) %>% 
  summarize_at(vars(no_of_beach_actions, no_of_days_under_action, closure, contamination_advisory, rain_advisory), sum, na.rm =T)

countofbeaches <- all_beach_vars %>% 
  ungroup() %>% 
  count(state)

state_sums_perbeach <- state_sums %>%
  left_join(countofbeaches, by = "state") %>% 
  mutate(actionsperbeach = no_of_beach_actions/n) %>% 
  mutate(daysperbeach = no_of_days_under_action/n) %>% 
  adorn_totals(where = "row")

# numbers i ran for the story
action_types %>% 
  filter(!is.na(closure))

beach_actions_clean2 %>%
  filter(action_type == "Closure") %>% 
  distinct(beach_id, .keep_all = T) %>% 
  count(action_reasons)
                          