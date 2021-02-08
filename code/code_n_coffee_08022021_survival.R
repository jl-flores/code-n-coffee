# Presenter: Jorge Luis Flores
library(tidyverse)
library(RColorBrewer)

library(survival)
library(survminer)

library(broom)
 
# ---- Load data -------------------------------------------------------------------------
# load data
chkup_data <- read_csv("code_n_coffee_08022021_survival_data.csv")
covar_data <- read_csv("code_n_coffee_08022021_survival_covariates.csv")

# ---- Prepare for survival analysis -----------------------------------------------------
# data has to have one row per individual
# and an indicator of whether they experienced the event

## get idea of sample size
# count how many times each individual got Lyme disease
# and then how many individuals got the disease x times
chkup_data %>% 
  group_by(id) %>% 
  summarize(nb_diagnosis = sum(lyme_dis)) %>% 
  
  group_by(nb_diagnosis) %>% 
  summarize(nb = n())

## build survival data tibble
# get first instance of Lyme disease for each person
surv_data <- chkup_data %>% 
  group_by(id) %>% 
  filter(lyme_dis == 1) %>% 
  slice_head() %>% 
  ungroup()

# get last visit for individuals who didn't get disease
surv_data <- surv_data %>% 
  bind_rows(chkup_data %>% 
              filter(!id %in% surv_data$id) %>% 
              
              group_by(id) %>% 
              slice_tail() %>% 
              ungroup()
  ) %>% 
  arrange(id)

# ---- Plot KM curves --------------------------------------------------------------------
# now we make survival and incidence curves from our data

# first add covariates
surv_data <- surv_data %>% 
  left_join(covar_data %>% select(id, repellant, gear), by = "id")

# create Survival object and plot
surv_all <- surv_fit(Surv(followup_time, lyme_dis) ~ repellant, data = surv_data)

# from survminer, easier to interface with other ggplot objects
km_all <- ggsurvplot(surv_all, censor.shape = "|", conf.int = T, cumevents = T, 
                     legend.title = "Used repellant?")

km_all

# can also stratify by different variables
surv_strat <- surv_fit(Surv(followup_time, lyme_dis) ~ repellant + gear, data = surv_data)
km_strat <- ggsurvplot(surv_strat, censor.shape = "|", conf.int = T, 
                       legend.title = "Used repellant?",
                       palette = rep(brewer.pal(3, "Dark2")[1:2], each = 3))

km_strat$plot + facet_wrap(~gear)

# ---- Cox regression --------------------------------------------------------------------
# fit a Cox model and output coefficients

## run regressions
# crude model
cox_crude <- coxph(Surv(followup_time, lyme_dis) ~ repellant, data = surv_data)
summary(cox_crude)

# adjusted model
cox_adjust <- coxph(Surv(followup_time, lyme_dis) ~ repellant + gear, data = surv_data)

## present results
# create table
cox_tbl <- tidy(cox_crude, conf.int = T) %>% mutate(nb_event = cox_crude$nevent)

# add "title" for second regression and its coefficients
cox_tbl <- cox_tbl %>% add_row(term = "adjusted")

# add adjusted coefficients
cox_tbl <- cox_tbl %>% 
  bind_rows(tidy(cox_adjust, conf.int = T) %>% mutate(nb_event = cox_adjust$nevent))

# exponentiate the coefficient and bounds of the confidence interval
cox_tbl <- cox_tbl %>% 
  mutate_at(vars("estimate", "conf.low", "conf.high"), exp)

cox_tbl
