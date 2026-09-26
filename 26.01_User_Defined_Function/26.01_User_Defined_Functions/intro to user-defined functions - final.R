
###### AN INTRODUCTION TO USER-DEFINED FUNCTIONS IN R #######################################

## ANATOMY OF A FUNCTION 
function_name = function(argument_1, argument_2) {
  # code to be executed
  sum(argument_1, argument_2)
}
function_name(10, 10)


## ENTERING ARGUMENTS

# A function doesn’t have to have set arguments. 
# By not establishing set arguments, it allows us more flexibility with number of inputs. 
add_function = function(...) {
  # code to be executed
  sum(...)
}
add_function(20, 20, 20, 20)


# When providing arguments, you can set an argument as a constant. 
add_20_function = function(x, y = 20) {
  sum(x, y)
}
add_20_function(20) 
# You can also override that fixed argument. 
add_20_function(10, 10)


###### TEACHING EXAMPLE ########################################################

library(dplyr)
library(stringr)
library(srvyr) 
library(forcats)

# importing datasets
idaho_data = read.csv('Idaho data.csv')
montana_data = read.csv('Montana data.csv')
kansas_data = read.csv('Kansas data.csv')

# exploring our data 
summary(idaho_data) 

table(idaho_data$wt_tri1)
table(idaho_data$wt_tri2)
table(idaho_data$wt_birth)


# data cleaning --> what we want the function to do
idaho_data_ex = idaho_data %>% 
  mutate( 
    across(c(wt_tri1, wt_tri2, wt_birth), 
           ~ {
             . = as.numeric(str_remove_all(as.character(.), "[^0-9.-]"))
             . = round(., digits = 1)
             case_when( 
               . < 85 | . > 400 ~ NA_real_, 
               TRUE ~ .)
           }, 
           .names = "{.col}_clean"))
           

# creating our data cleaning function 
clean_weight = function(df, ...) {
  df %>% 
    mutate( 
      across(c(...), 
             ~ {
               . = as.numeric(str_remove_all(as.character(.), "[^0-9.-]"))
               . = round(., digits = 1)
               case_when( 
                 . < 85 | . > 400 ~ NA_real_, 
                 TRUE ~ .)
             }, 
             .names = "{.col}_clean"))
  }          
           
# applying our function          
idaho_data = clean_weight(idaho_data, wt_tri1, wt_tri2, wt_birth)
montana_data = clean_weight(montana_data, wt_tri1, wt_tri2, wt_birth)
kansas_data = clean_weight(kansas_data, wt_tri1, wt_tri2, wt_birth)

# looking at what the function did 
summary(idaho_data)


# survey design --> what we want the function to do
example = idaho_data %>%
  as_survey_design(weights = survey_weight, 
                   strata = county_type)

# creating our weighing function 
weigh_data = function(df, s_weight, s_strata) {
  df %>% 
    as_survey_design(weights = {{s_weight}}, 
                     strata = {{s_strata}})
}

#applying our function 
idaho_design = weigh_data(idaho_data, survey_weight, county_type)
montana_design = weigh_data(montana_data, survey_weight, county_type)
kansas_design = weigh_data(kansas_data, survey_weight, county_type) 

# looking at what the function did 
idaho_design %>%  
  summarise(wt_birth = survey_mean(wt_birth_clean, na.rm = T))


###### PRACTICE PROBLEM #1 #####################################################
 
# We want to know whether mothers who had gestational hypertension had a higher 
# average weight at the time of birth. 

# We use the following code to get the average weight at birth grouped by 
# gestational hypertension status. Using this code as a base, create a function
# to efficiently produce the same estimates for Montana and Kansas. 

idaho_design %>%  
  group_by(hypertension_group = hypertension) %>% 
  summarise(wt_birth = survey_mean(wt_birth_clean, na.rm = T))

###### PRACTICE SOLUTION #1 ####################################################
group_mean = function(df_design, group, wt) {
  df_design %>%  
    group_by(hypertension_group = {{group}}) %>% 
    summarise(wt = survey_mean({{wt}}, na.rm = T))
}

group_mean(montana_design, hypertension, wt_birth_clean)
group_mean(kansas_design, hypertension, wt_birth_clean)

###### PRACTICE PROBLEM #2 #####################################################

# We want to know the BMI of mothers at the end of the first trimester. We'd also 
# like to create a categorical variable classifying mothers' weight as underweight, 
# normal, overweight, or obese.

# We use the following code to produce the BMI and BMI category for Idaho mothers. 
# Using this code as a base, create a function to efficiently produce the same 
# variables for Montana and Kansas. 

# BMI formula = (lbs/(inches^2))*703

idaho_data = idaho_data %>%  
  mutate(bmi_tri1 = (wt_tri1_clean/(height_in^2))*703, 
         bmi_tri1_cat = case_when( 
           bmi_tri1 < 18.5 ~ "Underweight", 
           bmi_tri1 >= 18.5 & bmi_tri1 <= 24.9 ~ "Normal", 
           bmi_tri1 >= 25 & bmi_tri1 <= 29.9 ~ "Overweight", 
           bmi_tri1 >= 30 & bmi_tri1 <= 39.9 ~ "Obese"), 
         bmi_tri1_cat = fct_relevel(bmi_tri1_cat, 
                                    "Underweight", 
                                    "Normal", 
                                    "Overweight", 
                                    "Obese")) 
         
###### PRACTICE SOLUTION #2 #################################################### 
bmi = function(df, wt_1, height) {
  df %>%  
    mutate(bmi_tri1 = ({{wt_1}}/({{height}}^2))*703, 
           bmi_tri1_cat = case_when( 
             bmi_tri1 < 18.5 ~ "Underweight", 
             bmi_tri1 >= 18.5 & bmi_tri1 <= 24.9 ~ "Normal", 
             bmi_tri1 >= 25 & bmi_tri1 <= 29.9 ~ "Overweight", 
             bmi_tri1 >= 30 & bmi_tri1 <= 39.9 ~ "Obese"), 
           bmi_tri1_cat = fct_relevel(bmi_tri1_cat, 
                                      "Underweight", 
                                      "Normal", 
                                      "Overweight", 
                                      "Obese"))
    
}

montana_data = bmi(montana_data, wt_tri1_clean, height_in)
kansas_data = bmi(kansas_data, wt_tri1_clean, height_in)

# looking at what the function did 
mean(montana_data$bmi_tri1, na.rm = T)
table(montana_data$bmi_tri1_cat)

