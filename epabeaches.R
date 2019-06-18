library(tidyverse)
library(janitor)
library(lubridate)

beach_actions <- read_csv("EPAbeaches/beach_actions_(advisories_and_closures).csv") %>% 
  clean_names() 

# remove territories other than PR and clean some states named wrong
beach_actions_clean <- beach_actions %>% 
  mutate(state = str_replace(state, "BR", "WI")) %>% 
  mutate(state = str_replace(state, "MK", "WA")) %>% 
  filter(state != "AS",
         state != "CN", 
         state != "GP",
         state != "GU",
         state != "MP",
         state != "VI")
  
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
  adorn_totals(where = "row")

write_csv(state_sums_perbeach, "statesums.csv")

# states with most contamination advisories and closures (no rain)
state_sums_perbeach %>% 
  mutate(contamclose = closure + contamination_advisory) %>% 
  arrange(desc(contamclose))

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

### graphics ###

closures_contaminations <- beach_actions_clean %>%
  filter(action_type != "Rain Advisory") %>%  
  count(action_type, year)

write_csv(closures_contaminations, "graphic1.csv")

pollution_sources <- beach_actions_clean2 %>%
  count(action_possible_source)

write_csv(pollution_sources, "graphic2.csv")

sumbyyear <- beach_actions %>% 
  mutate(action_start_date = dmy(action_start_date)) %>% 
  mutate(action_end_date = dmy(action_end_date)) %>% 
  group_by(month = floor_date(action_start_date, "month")) %>% 
  count(month) %>% 
  filter(month == "2017-06-01" |
         month == "2017-07-01" |
         month == "2017-08-01" |
         month == "2017-09-01" |
         month == "2018-06-01" |
         month == "2018-07-01" |
         month == "2018-08-01" |
         month == "2018-09-01") %>% 
  adorn_totals(where = "row")
  
beaches_monitored <- read_csv("EPAbeaches/beach_monitoring_frequency.csv") %>% 
  clean_names()

latestyear <- beaches_monitored %>% 
  filter(year == "2018") %>% 
  select(beach_id, beach_name, county, state, swim_season_monitoring_frequency, swim_season_monitor_frequency_units, off_season_monitoring_frequency, off_season_monitor_frequency_units)

join_freq_and_all <- latestyear %>%
  left_join(all_beach_vars, by = "beach_id")

write_csv(join_freq_and_all, "frequency.csv")  
