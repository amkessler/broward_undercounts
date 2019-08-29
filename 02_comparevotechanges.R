##comparing vote changes by precinct on Nov 13 vs. Nov 8

library(tidyverse)
library(janitor)
library(lubridate)



### LOAD DATA FROM SCRAPED CSVS FROM NOV 13 ####

senate_raw <- read_csv("raw_data/broward_senate.csv")
senate <- senate_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage)) #removes two rows at the top that don't contain data

governor_raw <- read_csv("raw_data/broward_governor.csv")
governor <- governor_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))


#combine all into one dataframe
combined <- rbind(senate, governor)


#by race totals
combined %>% 
  group_by(racename, choice) %>% 
  summarise(sum(total_votes))

#pull out just dems and reps
z1 <- combined %>%
  filter(str_detect(choice, "DEM"))
z2 <- combined %>%
  filter(str_detect(choice, "REP"))
z3 <- rbind(z1, z2)

#by precinct, race and candidate
combined_byprecinct <- z3 %>% 
  group_by(precinctname, racename, choice) %>% 
  summarise(totvotes = sum(total_votes)) %>% 
  ungroup()

#senate
byprecinct_senate <- combined_byprecinct %>% 
  filter(racename == "United States Senator") %>% 
  select(-racename)

#spread to wide format
byprecinct_senate_wide <- byprecinct_senate %>% 
  spread(key = choice, value = totvotes) %>% 
  clean_names() %>% 
  rename(nelson13 = bill_nelson_dem,
         scott13 = rick_scott_rep)


#governor
byprecinct_governor <- combined_byprecinct %>% 
  filter(racename == "Governor and Lieutenant Governor") %>% 
  select(-racename)

#spread to wide format
byprecinct_governor_wide <- byprecinct_governor %>% 
  spread(key = choice, value = totvotes) %>% 
  clean_names() %>% 
  rename(gillum13 = andrew_gillum_dem,
         desantis13 = ron_de_santis_rep)

#nov 13 tables
nov13_senate <- byprecinct_senate_wide
nov13_governor <- byprecinct_governor_wide





### LOAD DATA FROM SCRAPED CSVS FROM NOV 8 ####

senate_raw <- read_csv("raw_data/archived_nov8/broward_senate.csv")
senate <- senate_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage)) #removes two rows at the top that don't contain data

governor_raw <- read_csv("raw_data/archived_nov8/broward_governor.csv")
governor <- governor_raw %>% 
  clean_names() %>% 
  filter(!is.na(percentage))


#combine all into one dataframe
combined <- rbind(senate, governor)


#by race totals
combined %>% 
  group_by(racename, choice) %>% 
  summarise(sum(total_votes))

#pull out just dems and reps
z1 <- combined %>%
  filter(str_detect(choice, "DEM"))
z2 <- combined %>%
  filter(str_detect(choice, "REP"))
z3 <- rbind(z1, z2)

#by precinct, race and candidate
combined_byprecinct <- z3 %>% 
  group_by(precinctname, racename, choice) %>% 
  summarise(totvotes = sum(total_votes)) %>% 
  ungroup()

#senate
byprecinct_senate <- combined_byprecinct %>% 
  filter(racename == "United States Senator") %>% 
  select(-racename)

#spread to wide format
byprecinct_senate_wide <- byprecinct_senate %>% 
  spread(key = choice, value = totvotes) %>% 
  clean_names() %>% 
  rename(nelson8 = bill_nelson_dem,
         scott8 = rick_scott_rep)

#governor
byprecinct_governor <- combined_byprecinct %>% 
  filter(racename == "Governor and Lieutenant Governor") %>% 
  select(-racename)

#spread to wide format
byprecinct_governor_wide <- byprecinct_governor %>% 
  spread(key = choice, value = totvotes) %>% 
  clean_names() %>% 
  rename(gillum8 = andrew_gillum_dem,
         desantis8 = ron_de_santis_rep)

#nov 8 tables
nov8_senate <- byprecinct_senate_wide
nov8_governor <- byprecinct_governor_wide

#remove unneeded objects
removelist <- c("nov8_governor",
                "nov8_senate",
                "nov13_governor",
                "nov13_senate")

gdata::keep(list=removelist, sure = TRUE)


### join to make comparisons ####
compare_senate <- inner_join(nov8_senate, nov13_senate)
compare_governor <- inner_join(nov8_governor, nov13_governor)

#replace NAs with 0s
compare_senate[is.na(compare_senate)] <- 0
compare_governor[is.na(compare_governor)] <- 0


### CALCULATE MARGINS AND CHANGES ####

#senate
compare_senate <- compare_senate %>% 
  mutate(
    nelson_margin_8 = (nelson8-scott8),
    nelson_margin_13 = (nelson13-scott13),
    nelson_margin_CHANGE = (nelson_margin_13-nelson_margin_8)
  )

#total up how many votes nelson's gained/lost?
sum(compare_senate$nelson_margin_CHANGE)


#governor
compare_governor <- compare_governor %>% 
  mutate(
    gillum_margin_8 = (gillum8-desantis8),
    gillum_margin_13 = (gillum13-desantis13),
    gillum_margin_CHANGE = (gillum_margin_13-gillum_margin_8)
  )

#total up how many votes nelson's gained/lost?
sum(compare_governor$gillum_margin_CHANGE)


#export
write_csv(compare_senate, "compare_senate.csv")
write_csv(compare_governor, "compare_governor.csv")