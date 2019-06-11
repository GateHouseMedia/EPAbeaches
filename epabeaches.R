library(tidyverse)
library(janitor)
library(here)

action_counts <- read_csv(here("beaches_code", "action_duration.csv")) %>% 
  clean_names()

action_counts_clean <- action_counts %>% 
  mutate(state = str_replace(state, "BR", "WI")) %>% 
  mutate(state = str_replace(state, "MK", "WA")) %>% 
  filter(state != "AS",
         state != "CN", 
         state != "GP",
         state != "GU",
         state != "MP",
         state != "VI") %>% 
  filter(year == "2017" | year == "2018")

action_counts_sum <- action_counts_clean %>% 
  group_by(beach_id, beach_name, county, state) %>% 
  summarize_at(vars(no_of_beach_actions:no_of_actions_greater_than_30_day_duration), sum, na.rm =T)

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

beach_actions_clean2 %>% filter(action_reasons == "POLICY") %>% count(state)

beach_actions_clean2 %>% filter(action_reasons == "MODEL") %>% count(state)

beach_actions_clean2 %>% 
  count(action_indicator) 
# Total coliforms include species that may inhabit the intestines of warm-blooded animals or occur naturally in soil, vegetation, and water. 

beach_actions_clean2 %>% 
  count(action_reasons) 

beach_actions_source_collapsed <- beach_actions_clean2 %>%
  group_by(beach_id) %>% 
  select(beach_id, action_possible_source) %>% 
  distinct(beach_id, action_possible_source) %>% 
  mutate(sources = paste(action_possible_source, collapse = ", ")) %>% 
  distinct(beach_id, sources)
  # mutate(sources = str_replace(sources, "UNKNOWN, ", "")) %>% 
  # mutate(sources = str_replace(sources, "OTHER, ", "")) %>% 
  # mutate(sources = str_replace(sources, ", UNKNOWN", "")) %>% 
  # mutate(sources = str_replace(sources,", OTHER, ", "")) # should we remove unknown and other?

beach_actions_indicator_collapsed <- beach_actions_clean2 %>%
  group_by(beach_id) %>% 
  select(beach_id, action_indicator) %>% 
  distinct(beach_id, action_indicator) %>% 
  mutate(indicators = paste(action_indicator, collapse = ", ")) %>% 
  distinct(beach_id, indicators)
  # mutate(indicators = str_replace(indicators,"OTHER, ", "")) %>%
  # mutate(indicators = str_replace(indicators,", OTHER, ", "")) # same for this

both_collapsed <- beach_actions_source_collapsed %>% 
  full_join(beach_actions_indicator_collapsed, by = "beach_id")

sums_pollution_indicator <- action_counts_sum %>% 
  full_join(both_collapsed, by = "beach_id") %>% 
  select(beach_name, beach_id, county, state, no_of_beach_actions, no_of_days_under_action, sources, indicators)

action_types <- beach_actions_clean %>% #needed to use beach_actions_clean because numbers get duplicated for action types when i run separate_rows
  count(beach_id, action_type) %>% 
  spread(action_type, n) %>% 
  clean_names

action_reasons <- beach_actions_clean2 %>% 
  group_by(beach_id) %>% 
  select(beach_id, action_reasons) %>% 
  distinct(beach_id, action_reasons) %>% 
  mutate(reasons = paste(action_reasons, collapse = ", ")) %>% 
  distinct(beach_id, reasons)
  # mutate(reasons = str_replace(reasons, "UNKNOWN, ", "")) %>% 
  # mutate(reasons = str_replace(reasons, "OTHER, ", "")) %>% 
  # mutate(reasons = str_replace(reasons, ", UNKNOWN", "")) %>% 
  # mutate(reasons = str_replace(reasons,", OTHER, ", ""))

types_reasons <- action_types %>%
  full_join(action_reasons, by = "beach_id")

all_beach_vars <- sums_pollution_indicator %>% 
  full_join(types_reasons, by = "beach_id")

blankstate <- all_beach_vars %>% 
  filter(is.na(state))

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
# california counts don't match
# massachusetts is one number off

# numbers i ran for the story
action_types %>% 
  filter(!is.na(closure))

beach_actions_clean2 %>%
  filter(action_type == "Closure") %>% 
  distinct(beach_id, .keep_all = T) %>% 
  count(action_reasons)
                          