library(tidyverse)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)



### LOAD DATA FROM SCRAPED CSVS ####

senate_raw <- read_csv("raw_data/broward_senate.csv")
senate <- senate_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage)) #removes two rows at the top that don't contain data

governor_raw <- read_csv("raw_data/broward_governor.csv")
governor <- governor_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))

CD20_raw <- read_csv("raw_data/broward_CD20.csv")
cd20 <- CD20_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))

CD22_raw <- read_csv("raw_data/broward_CD22.csv")
cd22 <- CD22_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))

CD23_raw <- read_csv("raw_data/broward_CD23.csv")
cd23 <- CD23_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))


#sum up total votes for the races
sum(senate$total_votes)
sum(governor$total_votes)

#combine all into one dataframe
combined <- rbind(senate, governor, cd20, cd22, cd23)


#### GROUP TOGETHER TO CREATE PRECINT-BY-PRECINCT TABLE ####
names(combined)

#by race totals
combined %>% 
  group_by(racename, choice) %>% 
  summarise(sum(total_votes))

#by precinct and race
combined_byprecinct <- combined %>% 
  group_by(precinctname, racename) %>% 
  summarise(totvotes = sum(total_votes))

#spread to wide format
combined_byprecinct_wide <- combined_byprecinct %>% 
  spread(key = racename, value = totvotes) %>% 
  clean_names()

#replace NAs with 0s
combined_byprecinct_wide[is.na(combined_byprecinct_wide)] <- 0

#sum up congressional races to create one column for congress
combined_byprecinct_wide$congress_combined <- rowSums(combined_byprecinct_wide[3:5], na.rm = TRUE)

#reorder columns and add calculated differences
combined_final <- combined_byprecinct_wide %>% 
  select(precinctname, governor = governor_and_lieutenant_governor, senate = united_states_senator,
         cd20 = representative_in_congress_district_20, cd22 = representative_in_congress_district_22,
         cd23 = representative_in_congress_district_23, congress_combined) %>% 
  mutate(diff_gov_vs_senate = governor-senate,
         diff_gov_vs_congress = governor-congress_combined,
         diff_senate_vs_congress = senate-congress_combined)

#export result to csv
write.csv(combined_final, "broward_precincts_combined.csv", row.names = FALSE)



### ANALYSIS ####

#where the undercount of senate and congress is identical
combined_final %>% 
  filter(diff_senate_vs_congress == 0) 

combined_final %>% 
  filter(diff_senate_vs_congress == 0) %>% 
  write.csv("exactmatches.csv", row.names = FALSE)


#where the undercount of senate and congress is nearly identical
combined_final %>% 
  filter(diff_senate_vs_congress >= -5 && diff_senate_vs_congress <= 5,
         diff_gov_vs_senate >= 5) %>% #filter out ones where the gov vs senate is more than 5 in first place
  write.csv("fiveorless.csv", row.names = FALSE)
