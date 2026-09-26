

# import dataset 
solids = read.csv("solids_introduction.csv")

## SLIDE 11: Prepping Your Data ################################################

library(dplyr) 
library(forcats)

# creating our status and time variables for survival analysis
solids = solids %>% 
  mutate(  
    status = introduced_solids, 
    time = case_when( 
      status == 0 ~ infant_age_months, 
      status == 1 ~ age_solids_introduced_months), 
    # releveling our variables to make output cleaner 
    ethnicity = fct_relevel(ethnicity, "Non-Hispanic"), 
    insurance = fct_relevel(insurance, 
                            "Private", 
                            "Medicaid", 
                            "None"))

# checking our work
solids %>% 
  select(infant_age_months, 
         age_solids_introduced_months, 
         time, 
         status) %>% 
  head()

## SLIDE 13: Kaplan-Meier Curve ################################################

library(survival) 

# Kaplan-Meier Curve
km_plot = survfit(Surv(time, status) ~ 1, data = solids)

# using plot()
plot(km_plot,   
     main = "Surival Curve of Solids",
     xlab = "Time in Months", 
     ylab = "Proportion Fed Solids")

# using ggplot2
library(ggsurvfit)
km_plot %>% 
  ggsurvfit() + 
  add_confidence_interval() +
  labs(  
    title = "Survival Curve of Solids",
    x = "Time in Months", 
    y = "Proportion Not Fed Solids")


library(survey)
library(survminer)

# creating a survey design 
solids.design = svydesign(ids = ~1, 
                          strata = ~ethnicity, 
                          weights = ~survey_weight,  
                          fpc = ~N,
                          data = solids) 

# Kaplan-Meier Curve with a survey design (survey weights)
km_weighted_plot = svykm(Surv(time, status) ~ 1, design = solids.design)

# using plot()
plot(km_weighted_plot,   
     main = "Weighted Surival Curve of Solids",
     xlab = "Time in Months", 
     ylab = "Proportion Not Fed Solids")

library(jskm)

# using svyjskm with ggplot2
svyjskm(km_weighted_plot, 
        main = "Weighted Survival Curve of Solids",
        xlabs = "Time in Months", 
        ylabs = "Proportion Not Fed Solids")


### SLIDE 15: Log-Rank Test ####################################################

# Log-Rank Test 
log_rank = survdiff(Surv(time, status) ~ insurance, 
                    data = solids) %>% 
  print()

# weighted Log-Rank Test
log_rank_weighted = svylogrank(Surv(time, status) ~ insurance, 
                               design = solids.design) %>% 
  print()

### SLIDE 16: Log-Rank Test graph, un-weighted ##################################

# create fit first 
insurance_fit = survfit(Surv(time, status) ~ insurance, 
                        data = solids)

# using ggsurvplot()
ggsurvplot(insurance_fit, 
           title = "Survival Curve of Solids by Insurance",
           xlab = "Time in Months", 
           ylab = "Proportion Not Fed Solids")

### SLIDE 17: Log-Rank Test graph, weighted ####################################

# created weighted fit first 
insurance_fit_weighted = svykm(Surv(time, status) ~ insurance, 
                               design = solids.design)

# using plot()
plot(insurance_fit_weighted[["None"]], 
     main = "Survival Curve of Solids by Insurance", 
     xlab = "Time in Months", 
     ylab = "Proportion Not Fed Solids",
     col = "green", lwd = 2)
lines(insurance_fit_weighted[["Medicaid"]], col = "salmon", lwd = 2)
lines(insurance_fit_weighted[["Private"]], col = "blue", lwd = 2)
# Add a legend
legend("bottomright", 
       legend = c("None", "Medicaid", "Private"),
       col = c("green", "salmon", "blue"), 
       lty = 1, 
       lwd = 2)

### SLIDE 22: Cox Proportional Hazards Model, un-weighted ######################## 

cox_model = coxph(Surv(time, status) ~ insurance + ethnicity + maternal_age, 
                  data = solids) %>% 
  print()


### SLIDE 23: Cox Proportional Hazards Model, weighted ########################### 

cox_model_weighted = svycoxph(Surv(time, status) ~ insurance + ethnicity + maternal_age, 
                    design = solids.design) %>% 
  print()
