# Presenter: Marshall Lloyd
library(tidyverse)
library(broom)

####How I generated the data
# x1 <- runif(100, 0, 50)
# x2 <- runif(100, -10, 10)
# x3 <- rnorm(100, 0, 5)
# x4 <- rbinom(100, 1, 0.1)
# x5 <- runif(100, 0, 25)
# x6 <- runif(100, -5, 20)
# x7 <- rnorm(100, 20, 10)
# x8 <- rbinom(100, 1, 0.5) 
# y <- 20 + 0.5*x6 + 0.9*x7 + 10*x8 + rnorm(100, 0, 5)    #there is an association with x6, x7, and x8
# obs_number <- 1:100

#cnc_df_wide <- data.frame(obs_number, y, x1, x2, x3, x4, x5, x6, x7, x8)

#write.csv(cnc_df_wide, "code_n_coffee_16092020_broom_data.csv")

cnc_df_wide <- read.csv("code_n_coffee_16092020_broom_data.csv")

#different forms of linear regression model outputs
cnc_df_wide %>% lm(data = ., y ~ x6)
cnc_df_wide %>% lm(data = ., y ~ x6) %>% summary()
cnc_df_wide %>% lm(data = ., y ~ x6) %>% tidy()
cnc_df_wide %>% lm(data = ., y ~ x6) %>% glance()
#tidy and glance can be easier to use

#make the wide data long
cnc_df_long <- cnc_df_wide %>% gather(key = "predictor_variable", value = "predictor_variable_value", x1:x8)

#inspect and compare to the wide data
dim(cnc_df_wide)
head(cnc_df_wide)

dim(cnc_df_long)
head(cnc_df_long)

#can also do this using the pivot_longer() function, which is actually a better, more up to date function
#cnc_df_wide %>% pivot_longer(cols = x1:x8, names_to = "predictor_variable", values_to = "value")

cnc_uni_reg_results <- cnc_df_long %>% 
  nest(-predictor_variable) %>%         #nest puts entire tables into a cell, it is part of the tidyr
  mutate(fit = map(data, ~ lm(y ~ predictor_variable_value, data = .)),
         t_results = map(fit, tidy),    #tidy and glance give different outputs, I want something from each
         g_results = map(fit, glance)) %>% 
  unnest(g_results) %>% dplyr::select(-statistic, -p.value) %>%   #unest each of them
  unnest(t_results) %>%
  transmute(predictor_variable, term, Beta = estimate, SE = std.error,            #take the information that i want
            LL = (estimate - (1.96*std.error)), UL = (estimate + (1.96*std.error)),
            RMSE = sqrt(deviance/sum(cnc_df_long$y)),
            R2 = r.squared, PVal = p.value)

cnc_uni_reg_results %>% filter(term == "predictor_variable_value")   #this gives me the coefficients, it removed the intercept outputs
