
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
data_intro = data_raw

#viewing the data
View(data_intro)
glimpse(data_intro)

### "Big Stuff" (capitalization, white spaces, blank cells) ####################

#standardizing capitalization
#title case with str_to_title
data_intro$musician_name = str_to_title(data_intro$musician_name)
data_intro$band = str_to_title(data_intro$band)
data_intro$nationality = str_to_title(data_intro$nationality)
#lower case with str_to_lower 
data_intro$instrument_role = str_to_lower(data_intro$instrument_role)
data_intro$alive = str_to_lower(data_intro$alive)
data_intro$rock_hall_inductee = str_to_lower(data_intro$rock_hall_inductee)

#removing white space using str_squish function from stringr
#NOTE: str_squish will coerce all variables to be characters. 
#That's not a problem for us since all our variables are characters already.
data_intro$musician_name = str_squish(data_intro$musician_name)
data_intro$instrument_role = str_squish(data_intro$instrument_role)
data_intro$band = str_squish(data_intro$band)
data_intro$years_in_band = str_squish(data_intro$years_in_band)
data_intro$age = str_squish(data_intro$age)
data_intro$alive = str_squish(data_intro$alive)
data_intro$date_of_death = str_squish(data_intro$date_of_death)
data_intro$nationality = str_squish(data_intro$nationality)
data_intro$rock_hall_inductee = str_squish(data_intro$rock_hall_inductee)

#turning blank cells into NA
data_intro[data_intro == ""] = NA

#checking work 
glimpse(data_intro)

### instrument_role ############################################################

#look at variable 
table(data_intro$instrument_role, useNA = "ifany")

#separating cells with two instruments into two separate columns 
#using str_split_fixed from stringr
data_intro[c("instrument_primary", "instrument_secondary")] =
  str_split_fixed(data_intro$instrument_role, "/", n = 2)

#cleaning up instrument labels for using str_detect() from stringr
#primary instrument
table(data_intro$instrument_primary)
data_intro$instrument_primary = 
  case_when( 
    str_detect(data_intro$instrument_primary, "guitar") ~ "guitar", 
    str_detect(data_intro$instrument_primary, "vocal") | 
      data_intro$instrument_primary == "singer" ~ "vocals", 
    str_detect(data_intro$instrument_primary, "drum") | 
      data_intro$instrument_primary == "percussion" ~ "drums", 
    str_detect(data_intro$instrument_primary, "bass") ~ "bass", 
    data_intro$instrument_primary == "keyboards" ~ "keyboard", 
    TRUE ~ data_intro$instrument_primary)
#instrument secondary
table(data_intro$instrument_secondary)
data_intro$instrument_secondary = 
  case_when( 
    data_intro$instrument_secondary == "keyboards" ~ "keyboard", 
    data_intro$instrument_secondary == "" ~ NA,
    TRUE ~ data_intro$instrument_secondary)

#cleaning up the dataset 
#moving the new cleaned instrument variables to be after "musician_name"
data_intro = relocate(data_intro, c(instrument_primary, instrument_secondary), 
                      .after = musician_name)
#removing old instrument_role variable
data_intro = select(data_intro, -instrument_role)

#checking work 
(check_instrument = data.frame(data_intro$instrument_primary,
                              data_intro$instrument_secondary,
                              data_raw$instrument_role))
table(data_intro$instrument_primary, useNA = "ifany")
table(data_intro$instrument_secondary, useNA = "ifany")

### years_in_band ##############################################################

#looking at variable 
table(data_intro$years_in_band, useNA = "ifany")

#cleaning up variable  
#replacing any abbreviated year ('78) with full year (1978)
data_intro$years_in_band_clean = str_replace_all(data_intro$years_in_band, "'", "19")

#creating a START YEAR variable using str_sub() from stringr
data_intro$years_in_band_start = str_sub(data_intro$years_in_band_clean, 1, 4)
#removing any extra symbols and letters
data_intro$years_in_band_start = str_remove_all(data_intro$years_in_band_start, "[^0-9]")
#converting variable in to a numerical class 
data_intro$years_in_band_start = as.numeric(data_intro$years_in_band_start)

#creating an END YEAR variable using str_sub() from stringr
data_intro$years_in_band_end = str_sub(data_intro$years_in_band_clean, 5) 
#removing any extra symbols and letters
data_intro$years_in_band_end = str_remove_all(data_intro$years_in_band_end, "[^0-9]")
#converting variable in to a numerical class 
data_intro$years_in_band_end = as.numeric(data_intro$years_in_band_end)

#hunting down rock stars with missing band years
unique(data_intro[is.na(data_intro$years_in_band_start), c("musician_name", "band")])

#manually entering the start and end year for rock stars with missing data
data_intro$years_in_band_start =  
  case_when(
    #NOTE: specify name AND band! some rock stars are listed more than once in the 
    #dataset since they were in multiple bands
    data_intro$musician_name == "Bon Scott." & data_intro$band == "Ac/Dc" ~ 1974,
    data_intro$musician_name == "Roger Glover." & data_intro$band == "Deep Purple" ~ 1969, 
    TRUE ~ data_intro$years_in_band_start)
data_intro$years_in_band_end = 
  case_when( 
    data_intro$musician_name == "Bon Scott." & data_intro$band == "Ac/Dc" ~ 1980,
    data_intro$musician_name == "Roger Glover." & data_intro$band == "Deep Purple" ~ NA, 
    TRUE ~ data_intro$years_in_band_end)

#cleaning up dataset  
#removing old/unused band year variables
data_intro = select(data_intro, -c(years_in_band, years_in_band_clean)) 
#relocating new band year variables 
data_intro = relocate(data_intro, c(years_in_band_start, years_in_band_end),
                      .after = band)

#checking work 
(check_band_years = data.frame(data_intro$years_in_band_start,
                               data_intro$years_in_band_end,
                               data_raw$years_in_band))
table(data_intro$years_in_band_start, useNA = "ifany")
table(data_intro$years_in_band_end, useNA = "ifany")

### musician_name and nationality ##############################################

#look at variables
table(data_intro$musician_name, useNA = "ifany")
table(data_intro$nationality, useNA = "ifany")

#before we remove symbols from musician names, let's check whether there are 
#any valid symbols (e.g., O'Brien, Smith-Jones)
data_intro[str_detect(data_intro$musician_name, "[^[:alnum:][:space:]]"), "musician_name"]
#NOTE: indexing format --> data[row, column]

#removing any symbols from "musician_name" and "nationality" 
data_intro$musician_name = str_remove_all(data_intro$musician_name, "[^[:alpha:][:space:]]")
data_intro$nationality= str_remove_all(data_intro$nationality, "[^[:alpha:][:space:]]")

#manually cleaning up any remaining discrepancies in the "nationality" variable
table(data_intro$nationality, useNA = "ifany")
data_intro$nationality = case_when( 
  data_intro$nationality %in% 
    c("America", "Us", "Usa") ~ "United States", 
  data_intro$nationality %in% 
    c("England", "English", "Uk") ~ "United Kingdom", 
  TRUE ~ data_intro$nationality)

#checking work 
(check_nationality = data.frame(data_intro$nationality,
                               data_raw$nationality))
table(data_intro$nationality, useNA = "ifany")
(check_name = data.frame(data_intro$musician_name,
                                data_raw$musician_name))
table(data_intro$musician_name, useNA = "ifany")

### age ########################################################################

#look at variable
table(data_intro$age, useNA = "ifany")

#manually replacing the written number value with a numerical value
data_intro$age_clean = 
  case_when( 
    data_intro$age == "sixty-three" ~ "63", 
    TRUE ~ data_intro$age)
#NOTE: R does have packages (numberize) to convert written numbers into numeric values

#converting age_clean into a numeric variable
data_intro$age_clean = as.numeric(data_intro$age_clean)

#filtering out unreasonable age values 
data_intro$age_clean = 
  case_when(data_intro$age_clean < 18 | data_intro$age_clean > 90 ~ NA, 
            TRUE ~ data_intro$age_clean)

#finding which rock stars are missing ages
unique(data_intro[which(is.na(data_intro$age_clean)), "musician_name"])
#manually entering the missing ages
data_intro$age_clean = 
  case_when( 
    data_intro$musician_name == "Ritchie Blackmore" ~ 81, 
    data_intro$musician_name == "James Hetfield" ~ 63, 
    data_intro$musician_name == "Bruce Dickinson" ~ 68, 
    TRUE ~ data_intro$age_clean) 

#cleaning up dataset 
#relocating the new age variable to be after the years_in_band variable
data_intro = relocate(data_intro, age_clean, .after = years_in_band_end)
#removing the old age variable
data_intro = select(data_intro, -age)
#renaming the new age variable
data_intro = rename(data_intro, age = age_clean)

#checking work
(check_age = data.frame(data_intro$age, data_raw$age))
range(data_intro$age)

### alive and rock_hall_inductee ############################################### 

#look at variables 
table(data_intro$alive, useNA = "ifany")
table(data_intro$rock_hall_inductee, useNA = "ifany")

#cleaning up living status (alive) variable  
data_intro$alive_clean = 
  case_when(
    data_intro$alive %in% c("0", "deceased", "n", "no", "false") ~ FALSE, 
    data_intro$alive %in% c("1", "alive", "true", "y", "yes") ~ TRUE)

#cleaning up hall of fame (rock_hall_inductee) variable  
data_intro$rock_hall_inductee_clean = 
  case_when(
    data_intro$rock_hall_inductee %in% c("0", "n", "no", "false") ~ FALSE, 
    data_intro$rock_hall_inductee %in% c("1", "true", "y", "yes") ~ TRUE)

#hunting down any rock stars with missing living or hall of fame status
unique(data_intro[is.na(data_intro$alive_clean), "musician_name"])
unique(data_intro[is.na(data_intro$rock_hall_inductee_clean), "musician_name"]) 
#manually entering the missing living status 
data_intro$alive_clean = 
  case_when( 
    data_intro$musician_name == "Bruce Dickinson" ~ TRUE, 
    data_intro$musician_name == "Ronnie James Dio" ~ FALSE, 
    TRUE ~ data_intro$alive_clean) 

#cleaning up dataset
#relocating the new variables 
data_intro = relocate(data_intro, alive_clean, .after = age)
data_intro = relocate(data_intro, rock_hall_inductee_clean, .after = nationality) 
#removing old living and Rock Hall of Fame variables
data_intro = select(data_intro, -c(alive, rock_hall_inductee))
#renaming new living and Rock Hall of Fame variables
data_intro = rename(
  data_intro, 
  alive = alive_clean, 
  rock_hall_inductee = rock_hall_inductee_clean)

#checking work
(check_alive = data.frame(data_intro$alive, data_raw$alive))
table(data_intro$alive)
(check_hall = data.frame(data_intro$rock_hall_inductee, data_raw$rock_hall_inductee))
table(data_intro$rock_hall_inductee)

### band #######################################################################

#look at variable
table(data_intro$band, useNA = "ifany")

#creating a vector of correctly spelt band names
band_names = c(
  "AC/DC", "Black Sabbath", "Deep Purple", "Dio", "Iron Maiden", 
  "Judas Priest", "Led Zeppelin", "Metallica", "Rainbow","Whitesnake")

#fuzzy matching band names in the dataset to our clean vector of band names 
#using amatch() from stringdist package
(match_bands = amatch(str_to_lower(data_intro$band), 
                      str_to_lower(band_names), maxDist = 2, method = "lv"))

#making a new column that assigns band name based on fuzzy matching  
data_intro$band_clean = band_names[match_bands]

#hunting down any un-matched bands to hand-match 
data_intro[which(is.na(match_bands)), "band"]

#manually cleaning up the band names that were too messy to be fuzzy matched
data_intro$band_clean = 
  case_when( 
    data_intro$band == "Ritchie Blackmore's Rainbow" ~ "Rainbow", 
    TRUE ~ data_intro$band_clean)

#cleaning up the dataset 
#moving the new cleaned band variable to be after musician_name
data_intro = relocate(data_intro, band_clean, .after = musician_name)
#removing old band variable
data_intro = select(data_intro, -band)
#renaming new band variable 
data_intro = rename(data_intro, band = band_clean)

#check work

#checking work by comparing old variable to new variable
(check_bands = data.frame(data_intro$band, data_raw$band))
table(data_intro$band, useNA = "ifany")

### date_of_death (BONUS CHALLENGE) ##################################################

#WARNING: This code won't work if you have date formats without clear 
#distinction between month and dates (e.g., 01/03/26 could be January 3rd or
#March 1st). That's not an issue with this dataset, but it could be in others.

#look at variable 
table(data_intro$date_of_death, useNA = "ifany") 

#creating a vector with all our date formats
date.format = c(
  "m/d/y",
  "d-m-y", 
  "d/m/y")

#converting date_of_death into Date class
#using parse_date_time() from lubridate to account for different date formats
data_intro$date_of_death_clean = parse_date_time(data_intro$date_of_death, orders = date.format)
#using as.Date() to convert the variable into a Date class
data_intro$date_of_death_clean = as.Date(data_intro$date_of_death_clean)

#hunting down any living rock stars with date of death present
unique(data_intro[which(data_intro$alive == TRUE & !is.na(data_intro$date_of_death_clean)),
                "musician_name"])
#hunting down any deceased rock stars with missing death date 
unique(data_intro[which(data_intro$alive == FALSE & is.na(data_intro$date_of_death_clean)), 
                "musician_name"])

data_intro$date_of_death_clean = 
  case_when(
    #manually removing Jimmy Page's death date (he's alive... thank god)
    data_intro$musician_name == "Jimmy Page" ~ NA, 
    #manually entering Ozzy Osbourne's date of death
    data_intro$musician_name == "Ozzy Osbourne" ~ as.Date("2025-07-22"),
    TRUE ~ data_intro$date_of_death_clean)

#cleaning up datasets 
#relocating the cleaned death date variable 
data_intro = relocate(data_intro, date_of_death_clean, .after = alive)
#removing old death date variable 
data_intro = select(data_intro, -date_of_death)
#renaming new death date variable 
data_intro = rename(data_intro, date_of_death = date_of_death_clean)

#check work 
(check_death_date = data.frame(data_intro$alive, data_intro$date_of_death,
                               data_raw$date_of_death))
table(data_intro$date_of_death, useNA = "ifany") 

### final touches ##############################################################

#using indexing and duplicated() function from base R to check for duplicates
data_intro[duplicated(data_intro), c("musician_name", "band")]

data_intro = unique(data_intro) 
data_intro = arrange(data_intro, band)

#export cleaned dataset
write.csv(data_intro, "Cleaned_intro_data.csv", row.names = F)
