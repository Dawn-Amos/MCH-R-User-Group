
#focuses of training
library(dplyr) 
library(stringr)

#other necessary packages
library(tidyverse) #NOTE: dplyr and stringr are part of tidyverse
library(lubridate)
library(stringdist)  

#clears environment
rm(list = ls())

#importing dataset 
data_raw = read.csv("dirty_data_done_dirt_cheap_DIRTY.csv") 

### exploring data #############################################################

#creating a new dataset to clean
data_adv = data_raw

#viewing the data
View(data_adv)
glimpse(data_adv)

### "Big Stuff" (capitalization, white spaces, blank cells) ####################

data_adv = data_adv %>% 
  mutate(
    #standardizing capitalization
    #title case with str_to_title
    across(c(musician_name, 
             band, 
             nationality), 
           ~ str_to_title(.)), 
    #lower case with str_to_lower
    across(c(instrument_role, 
             alive, 
             rock_hall_inductee), 
           ~ str_to_lower(.)), 
    #removing white space using str_squish function from stringr
    across(where(is.character), str_squish), 
    #turning blank cells into NA
    across(everything(), ~na_if(., ""))) 

#checking work 
glimpse(data_adv)

### instrument role ############################################################

#look at variable 
table(data_adv$instrument_role, useNA = "ifany")

#separating cells with two instruments into two separate columns 
#using str_split_fixed from stringr
data_adv[c("instrument_primary", "instrument_secondary")] =
  str_split_fixed(data_adv$instrument_role, "/", n = 2)

data_adv = data_adv %>%
    #cleaning up instrument labels for using str_detect() from stringr
    mutate(across(c(instrument_primary, 
                  instrument_secondary), 
                ~ case_when( 
                  str_detect(., "guitar") ~ "guitar", 
                  str_detect(., "vocal") | . == "singer" ~ "vocals", 
                  str_detect(., "drum") | . == "percussion" ~ "drums", 
                  str_detect(., "bass") ~ "bass", 
                  . == "keyboards" ~ "keyboard", 
                  . == "" ~ NA,
                  T ~ .))) %>% 
  #cleaning up the dataset 
  #moving the new cleaned instrument variables to be after "musician_name"
  relocate(c(instrument_primary, instrument_secondary), 
           .after = musician_name) %>% 
  #removing old instrument_role variable
  select(-instrument_role)
  
#checking work 
(check_instrument = data.frame(data_adv$instrument_primary,
                               data_adv$instrument_secondary,
                               data_raw$instrument_role))
table(data_adv$instrument_primary, useNA = "ifany")
table(data_adv$instrument_secondary, useNA = "ifany")


### years_in_band ##############################################################

#looking at variable 
table(data_adv$years_in_band, useNA = "ifany")

#cleaning up variable
data_adv = data_adv %>% 
  mutate( 
    #replacing any abbreviated year ('78) with full year (1978)
    years_in_band_clean = str_remove_all(str_replace_all(years_in_band, 
                                                         "'", "19"), "[^0-9]"),
    #creating a start year variable using str_sub() from stringr 
    #and making it numeric class
    years_in_band_start = as.numeric(str_sub(years_in_band_clean, 1, 4)), 
    #creating a end year variable using str_sub() from stringr and 
    #making it numeric class
    years_in_band_end = as.numeric(str_sub(years_in_band_clean, 5)))

#hunting down rock stars with missing band years
unique(data_adv[is.na(data_adv$years_in_band_start), 
                c("musician_name", "band")])

#manually entering the start and end year for rock stars with missing data
data_adv = data_adv %>% 
  mutate(
    #NOTE: specify name AND band! some rock stars are listed more than once in 
    #the dataset since they were in multiple bands
    years_in_band_start = case_when(
      musician_name == "Bon Scott." & band == "Ac/Dc" ~ 1974,
      musician_name == "Roger Glover." & band == "Deep Purple" ~ 1969, 
      T ~ years_in_band_start), 
    years_in_band_end = case_when(
      musician_name == "Bon Scott." & band == "Ac/Dc" ~ 1980,
      musician_name == "Roger Glover." & band == "Deep Purple" ~ NA, 
      T ~ years_in_band_end)) %>% 
  #removing old/unused band year variables
  select(-c(years_in_band, years_in_band_clean)) %>% 
  #relocating new band year variables 
  relocate(c(years_in_band_start, years_in_band_end), 
           .after = band)

#checking work 
(check_band_years = data.frame(data_adv$years_in_band_start,
                               data_adv$years_in_band_end,
                               data_raw$years_in_band))
table(data_adv$years_in_band_start, useNA = "ifany")
table(data_adv$years_in_band_end, useNA = "ifany")

### musician_name and nationality ##############################################

#look at variables 
table(data_adv$musician_name, useNA = "ifany")
table(data_adv$nationality, useNA = "ifany")

#before we remove symbols from musician names, let's check whether there are 
#any valid symbols (e.g., O'Brien, Smith-Jones)
data_adv[str_detect(data_adv$musician_name, "[^[:alnum:][:space:]]"), "musician_name"]
#NOTE: indexing format --> data[row, column]

#cleaning up variable
data_adv = data_adv %>% 
  mutate(across(c(musician_name, nationality), 
                #removing any symbols from "musician_name" and "nationality" 
                ~ str_remove_all(., "[^[:alpha:][:space:]]")), 
         #manually cleaning up any remaining discrepancies in the "nationality" variable
         nationality = case_when(
           nationality %in% c("America", "Us", "Usa") ~ "United States",
           nationality %in% c("Uk", "English", "England") ~ "United Kingdom", 
           T ~ nationality))

#checking work 
(check_nationality = data.frame(data_adv$nationality,
                                data_raw$nationality))
table(data_adv$nationality, useNA = "ifany")
(check_name = data.frame(data_adv$musician_name,
                         data_raw$musician_name))
table(data_adv$musician_name, useNA = "ifany")


### age ######################################################################## 

#look at variable
table(data_adv$age, useNA = "ifany")

#cleaning up age variable
data_adv = data_adv %>% 
  mutate(
    #converting age_clean into a numeric variable
    age_clean = as.numeric(case_when(
      #manually replacing the written number value with a numerical value
      #NOTE: R does have packages (numberize) to convert written numbers into numeric values
      age == "sixty-three" ~ "63", 
      #filtering out unreasonable age values
      !between(as.numeric(age), 18, 90) ~ NA, 
      T ~ age))) %>% 
  #relocating the new age variable to be after the years_in_band_end variable
  relocate(age_clean, .after = years_in_band_end)

#finding which rock stars are missing ages
unique(data_adv[which(is.na(data_adv$age_clean)), "musician_name"])
#manually entering the missing ages
data_adv = data_adv %>% 
  mutate( 
    age_clean = case_when( 
      musician_name == "Ritchie Blackmore" ~ 81, 
      musician_name == "James Hetfield" ~ 63, 
      musician_name == "Bruce Dickinson" ~ 68, 
      TRUE ~ age_clean)) %>% 
  #cleaning up dataset
  select(-age) %>% 
  rename(age = age_clean)

#checking work
(check_age = data.frame(data_adv$age, data_raw$age))
range(data_adv$age)

### alive and rock_hall_inductee ############################################### 

#look at variables 
table(data_adv$alive, useNA = "ifany")
table(data_adv$rock_hall_inductee, useNA = "ifany")

#cleaning up variables 
data_adv = data_adv %>% 
  mutate( 
    across(c(alive, 
             rock_hall_inductee), 
           ~ case_when( 
             #specifying conditions for logical vector
             . %in% c("0", "deceased", "n", "no", "false") ~ F, 
             . %in% c("1", "alive", "true", "y", "yes") ~ T), 
           .names = "{.col}_clean")) %>% 
  #relocating new variables
  relocate(alive_clean, .after = age) %>% 
  relocate(rock_hall_inductee_clean, .after = nationality)

#hunting down any rock stars with missing living or hall of fame status
unique(data_adv[is.na(data_adv$alive_clean), "musician_name"])
unique(data_adv[is.na(data_adv$rock_hall_inductee_clean), "musician_name"]) 
#manually entering the missing living status 
data_adv = data_adv %>% 
  mutate(
    alive_clean = case_when( 
      musician_name == "Bruce Dickinson" ~ T, 
      musician_name == "Ronnie James Dio" ~ F, 
      T ~ alive_clean)) %>% 
  #cleaning up dataset
  #removing old living and Rock Hall of Fame variables
  select(-c(alive, rock_hall_inductee)) %>% 
  #renaming new living and Rock Hall of Fame variables
  rename(alive = alive_clean, 
         rock_hall_inductee = rock_hall_inductee_clean) 

#checking work
(check_alive = data.frame(data_adv$alive, data_raw$alive))
table(data_adv$alive)
(check_hall = data.frame(data_adv$rock_hall_inductee, data_raw$rock_hall_inductee))
table(data_adv$rock_hall_inductee)

### band #######################################################################

#look at variable
table(data_adv$band,  useNA = "ifany")

#creating a vector of correctly spelt band names
band_names = c(
  "AC/DC", "Black Sabbath", "Deep Purple", "Dio", "Iron Maiden", 
  "Judas Priest", "Led Zeppelin", "Metallica", "Rainbow","Whitesnake")

#fuzzy matching band names in the dataset to our clean vector of band names 
#using amatch() from stringdist package
(match_bands = amatch(str_to_lower(data_adv$band), 
                      str_to_lower(band_names), maxDist = 2, method = "lv"))

#hunting down any un-matched bands to hand-match 
data_adv[which(is.na(match_bands)), "band"]

data_adv = data_adv %>%   
  mutate( 
    #making a new column that assigns band name based on fuzzy matching  
    band_clean = band_names[match_bands],
    #manually cleaning up the band names that were too messy to be fuzzy matched
    band_clean = case_when(
      band == "Ritchie Blackmore's Rainbow" ~ "Rainbow",
      T ~ band_clean)) %>%
  #cleaning up the dataset 
  relocate(band_clean, .after = musician_name) %>% 
  select(-band) %>% 
  rename(band = band_clean)

#checking work
(check_bands = data.frame(data_adv$band, data_raw$band))
table(data_adv$band)

### date_of_death (BONUS CHALLENGE) ############################################

#WARNING: This code won't work if you have date formats without clear 
#distinction between month and dates (e.g., 01/03/26 could be January 3rd or
#March 1st). That's not an issue with this dataset, but it could be in others.

#look at variable 
table(data_adv$date_of_death, useNA = "ifany") 

#creating a vector with all our date formats
date.format = c(
  "m/d/y",
  "d-m-y", 
  "d/m/y")

#converting "date_of_death" into Date class
data_adv = data_adv %>% 
  #using parse_date_time() from lubridate to account for different date format
  #using as.Date() to convert the variable into a Date class
  mutate(date_of_death_clean = as.Date(parse_date_time(date_of_death, orders = date.format)))

#hunting down any living rockstars with date of death present
unique(data_adv[which(data_adv$alive == T & !is.na(data_adv$date_of_death_clean)), "musician_name"])
#hunting down any deceased rockstars with missing death date 
unique(data_adv[which(data_adv$alive == F & is.na(data_adv$date_of_death_clean)), "musician_name"])

data_adv = data_adv %>% 
  mutate( 
    date_of_death_clean = case_when( 
      #manually removing Jimmy Page's death date (he's alive... thank god)
      musician_name == "Jimmy Page" ~ NA,  
      #manually entering Ozzy Osbourne's date of death
      musician_name == "Ozzy Osbourne" ~ as.Date("2025-07-22"),
      T ~ date_of_death_clean)) %>% 
  #relocating the cleaned death date variable
  relocate(date_of_death_clean, .after = alive) %>% 
  #cleaning up dataset
  select(-date_of_death) %>% 
  rename(date_of_death = date_of_death_clean)  

#check work 
(check_death_date = data.frame(data_adv$alive, data_adv$date_of_death,
                               data_raw$date_of_death))
table(data_adv$date_of_death, useNA = "ifany")           

### final touches ##############################################################

#using indexing and duplicated() function from base R to check for duplicates
data_adv[duplicated(data_adv), c("musician_name", "band")]

data_adv = unique(data_adv) 
data_adv = arrange(data_adv, band)

#export cleaned dataset
write.csv(data_adv, "Cleaned_adv_data.csv", row.names = F)

