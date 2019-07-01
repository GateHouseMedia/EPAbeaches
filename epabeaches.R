library(tidyverse)
library(janitor)
library(lubridate)
library(jsonlite)
library(cdlTools)

beach_actions <- read_csv("EPAbeaches/beach_actions_(advisories_and_closures).csv") %>% 
  clean_names() 

# remove territories other than PR and convert tribes to states
beach_actions_clean <- beach_actions %>% 
  mutate(state = str_replace(state, "BR", "WI")) %>% 
  mutate(state = str_replace(state, "MK", "WA")) %>% 
  filter(state != "AS",
         state != "CN", 
         state != "GP",
         state != "GU",
         state != "MP",
         state != "VI")

write_csv(beach_actions_clean, "beach_actions_clean.csv")

# clean for analysis
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

beach_actions_clean2 %>% 
  count(action_possible_source) %>% 
  arrange(desc(n))

# collapse so there's only one row for each beach
beach_actions_collapsed <- beach_actions_clean2 %>% 
  group_by(beach_id, beach_name, state, county) %>% 
  summarize(reasons = paste(toString(unique(action_reasons)), collapse = ","),
            indicators = paste(toString(unique(action_indicator)), collapse = ","),
            sources = paste(toString(unique(action_possible_source)), collapse = ","))

# summarize so there's only one row for each beach
beach_numbers <- beach_actions_clean %>% 
  group_by(beach_id) %>% 
  summarize(no_of_beach_actions = n(),
            no_of_days_under_action = sum(action_duration_days),
            closure = sum(action_type == "Closure"),
            rain_advisory = sum(action_type == "Rain Advisory"),
            contamination_advisory = sum(action_type == "Contamination Advisory"))

# join
all_beach_vars <- left_join(beach_actions_collapsed, beach_numbers, by = "beach_id")

write_csv(all_beach_vars, "all_beach_vars.csv")

### count number of actions by state
state_sums <- all_beach_vars %>% 
  group_by(state) %>% 
  summarize_at(vars(no_of_beach_actions, no_of_days_under_action, closure, contamination_advisory, rain_advisory), sum, na.rm =T)

countofbeaches <- all_beach_vars %>% 
  ungroup() %>% 
  count(state)

# create state summaries
state_sums_perbeach <- state_sums %>%
  left_join(countofbeaches, by = "state") %>% 
  mutate(actionsperbeach = no_of_beach_actions/n) %>% 
  mutate(daysperbeach = no_of_days_under_action/n) %>% 
  adorn_totals(where = "row") %>% 
  rename("number_of_beaches_with_actions" = "n")

write_csv(state_sums_perbeach, "statesums.csv")

# states with most contamination advisories and closures (no rain)
state_sums_perbeach %>% 
  mutate(contamclose = closure + contamination_advisory) %>% 
  arrange(desc(contamclose)) %>% View()

### numbers for story ### 
beach_actions_clean2 %>%
  filter(action_type == "Closure") %>% 
  distinct(beach_id, .keep_all = T) %>% 
  count(action_reasons)

beach_actions_clean %>% 
  filter(beach_id == "NY569623") %>% 
  filter(action_type == "Closure") %>% 
  count(action_possible_source)

beach_actions_clean %>% 
  filter(beach_id == "NY626819") %>% 
  filter(action_type == "Closure") %>% 
  count(action_possible_source)

actions <- beach_actions_clean %>%
  count(action_reasons) %>% 
  arrange(desc(n))
  
actions$Pct <- actions$n / sum(actions$n) * 100

# elevated bacteria percent
43.479059515 + 13.776634827

# rain fall percent
38.262307127 + 13.776634827

# count beaches with closures
all_beach_vars %>% 
  filter(closure > 0) 

# contamination advisories
all_beach_vars %>% 
  filter(contamination_advisory > 0)

# rain advisories
all_beach_vars %>% 
  filter(rain_advisory > 0)

# count actions taken during summer months of 2017 and 2018
sumbyyear <- beach_actions_clean %>% 
  mutate(action_start_date = dmy(action_start_date)) %>% 
  mutate(action_end_date = dmy(action_end_date)) %>% 
  group_by(month = floor_date(action_start_date, "month")) %>% 
  filter(month == "2017-06-01" |
         month == "2017-07-01" |
         month == "2017-08-01" |
         month == "2017-09-01" |
         month == "2018-06-01" |
         month == "2018-07-01" |
         month == "2018-08-01" |
         month == "2018-09-01") %>% 
  count(month, action_type)

beach_actions_clean %>% 
  mutate(action_start_date = dmy(action_start_date)) %>% 
  mutate(action_end_date = dmy(action_end_date)) %>% 
  group_by(month = floor_date(action_start_date, "month")) %>% 
  filter(month == "2017-06-01" |
           month == "2017-07-01" |
           month == "2017-08-01" |
           month == "2017-09-01" |
           month == "2018-06-01" |
           month == "2018-07-01" |
           month == "2018-08-01" |
           month == "2018-09-01") %>% 
  filter(action_type != "Rain Advisory") %>% 
  count(month)

write_csv(sumbyyear, "summernumbers.csv")

beach_actions_clean %>% 
  count(action_type)

# import all beaches and their status for join
beachprofile <- read_csv("beaches/beach_profile_list.csv") %>% 
  clean_names()

# clean
beachprofile_clean <- beachprofile %>% 
  mutate(state_code = str_replace(state_code, "BR", "WI")) %>% 
  mutate(state_code = str_replace(state_code, "MK", "WA")) %>% 
  filter(state_code != "AS",
         state_code != "CN", 
         state_code != "GP",
         state_code != "GU",
         state_code != "MP",
         state_code != "VI") %>% 
  distinct(beach_id, beach_status, .keep_all = T) %>% 
  filter(year == "2018") %>% # only showing beach statuses as of 2018
  select(beach_id, beach_name, state_code, county, beach_status) %>% 
  rename(state = "state_code")

beachprofile_clean %>% # confirm that it matches EPA counts
  filter(beach_status != "Historical") %>% 
  distinct(beach_id, .keep_all = T) %>% 
  count(state) %>% View()

allbeaches <- full_join(all_beach_vars, beachprofile_clean, by = c("beach_id", "beach_name", "state", "county")) %>% 
  filter(!(beach_status == "Historical" & is.na(reasons))) %>% # keep beaches that are historic but have action data
  mutate(status = case_when(beach_status == "No advisory or closure" & !is.na(reasons) ~ "Active", # remove "no advisory or closure" language if a beach has had an advisory or closure
                            beach_status == "No advisory or closure" ~ "Active: No advisory or closure",
                            TRUE ~ paste0(beach_status))) %>% 
  select(-beach_status)

allbeaches %>%
  ungroup() %>% 
  count(state) 

allbeaches %>% 
  ungroup() %>% 
  count(beach_status) 

allbeaches %>% 
  ungroup() %>% 
  filter(status == "Dormant") %>% 
  filter(no_of_beach_actions > 0)

### graphics ###

closures_contaminations <- beach_actions_clean %>%
  filter(action_type != "Rain Advisory") %>%  
  count(action_type, year)

write_csv(closures_contaminations, "graphic1.csv")

pollution_sources <- beach_actions_clean2 %>%
  count(action_possible_source)

write_csv(pollution_sources, "graphic2.csv")

# headers for searchable database
allbeachesforpub <- allbeaches %>% 
  ungroup() %>% 
  mutate(state = fips(state, to = "Name")) %>% 
  rename("EPA beach ID" = "beach_id") %>% 
  rename("Beach name" = "beach_name") %>% 
  rename("State" = "state") %>% 
  rename("County" = "county") %>% 
  rename("Action reasons" = "reasons") %>% 
  rename("Bacteria indicators" = "indicators") %>% 
  rename("Possible pollution sources" = "sources") %>% 
  rename("Total number of beach actions" = "no_of_beach_actions") %>% 
  rename("Total number of days under action" = "no_of_days_under_action") %>% 
  rename("Total number of beach closures" = "closure") %>% 
  rename("Total number of rain advisories" = "rain_advisory") %>% 
  rename("Total number of contamination advisories" = "contamination_advisory") %>% 
  rename("2018 beach reporting status" = "status") 

write_json(allbeaches, "beaches.json")

allbeachesforpubjson <- toJSON(allbeachesforpub, na = "string")

write(allbeachesforpubjson, "allbeachesforpub.json")

### for gatehouse markets ###

state_summary <- read_csv("EPAbeaches/state_summary.csv") %>% 
  clean_names()

clean_state_summary <- state_summary %>% 
  mutate(state = str_replace(state, "BR", "WI")) %>% 
  mutate(state = str_replace(state, "MK", "WA")) %>% 
  filter(state != "AS",
         state != "CN", 
         state != "GP",
         state != "GU",
         state != "MP",
         state != "VI")

sum_states2018 <- clean_state_summary %>% 
  filter(year == "2018") %>% 
  group_by(state) %>% 
  summarize(total_beaches = sum(no_of_beach_act_beaches),
            monitored_beaches = sum(no_of_monitored_beaches),
            percent_monitored = (monitored_beaches/total_beaches * 100),
            beaches_with_actions = sum(no_of_monitored_beacheswith_actions),
            beaches_without_actions = sum(no_of_monitored_beacheswithout_actions),
            total_actions = sum(total_no_of_beach_actions),
            total_days_under_action = sum(no_of_days_undera_beach_action_monitored_beaches),
            total_days_not_under_action = sum(no_of_days_not_undera_beach_action_monitored_beaches))

write_csv(sum_states2018, "2018_state_summaries.csv")

sum_states2017 <- clean_state_summary %>% 
  filter(year == "2017") %>% 
  group_by(state) %>% 
  summarize(total_beaches = sum(no_of_beach_act_beaches),
            monitored_beaches = sum(no_of_monitored_beaches),
            percent_monitored = (monitored_beaches/total_beaches * 100),
            beaches_with_actions = sum(no_of_monitored_beacheswith_actions),
            beaches_without_actions = sum(no_of_monitored_beacheswithout_actions),
            total_actions = sum(total_no_of_beach_actions),
            total_days_under_action = sum(no_of_days_undera_beach_action_monitored_beaches),
            total_days_not_under_action = sum(no_of_days_not_undera_beach_action_monitored_beaches))

write_csv(sum_states2017, "2017_state_summaries.csv")
