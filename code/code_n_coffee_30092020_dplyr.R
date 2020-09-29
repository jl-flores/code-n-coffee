# Presenter: Jorge Luis Flores
library(tidyverse)
library(lubridate)

# ---- Clean data ------------------------------------------------------------------------
# load data
survey_data <- read_csv("code_n_coffee_30092020_dplyr_data.csv")

# extract year of response from survey date
survey_data <- survey_data %>% 
  mutate(survey_date = year(dmy(survey_date)))

# get column names
flavour_names <- grep(x = names(survey_data), pattern = "^flavour_", value = T)

# alternatively
survey_data %>% 
  select(starts_with("flavour_")) %>% 
  names()

# ---- Proportion reporting each flavour -------------------------------------------------
# compute raw counts
tbl_flvr <- survey_data %>% 
  summarize_at(flavour_names, sum, na.rm = T) %>% 
  pivot_longer(cols = flavour_vanilla:flavour_greentea, 
               names_to = "flavour", values_to = "nb_reporting", 
               names_prefix = "flavour_") %>% 
  arrange(desc(nb_reporting))

# compute frequency/proportion
tbl_flvr <- tbl_flvr %>% 
  mutate(freq = nb_reporting / nrow(survey_data) * 100)

# add code for colour
tbl_flvr <- tbl_flvr %>% 
  mutate(
    flvr_type = case_when(flavour %in% c("hazelnut", "pistachio") ~ "nuts",
                          flavour %in% c("vanilla", "chocolate", "strawberry") ~ "classic",
                          T ~ "special")
  )

# bar plot
ggplot(tbl_flvr, aes(x = reorder(flavour, -freq), y = freq, fill = flvr_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(..y.., 1)), vjust = -0.25) +
  labs(title = "Proportion of people who like each ice cream flavour", 
       x = "Ice cream flavour", y = "Proportion (%)")

# ---- Proportion reporting x number of flavours -----------------------------------------
# compute number of flavours liked by each
flvr_numbers <- survey_data %>% 
  select(flavour_vanilla:flavour_greentea) %>% 
  mutate(sum = rowSums(.[1:8], na.rm = T)) %>% 
  pull(sum)

#survey_data$flvr_numbers <- rowSums(survey_data[, 5:12], na.rm = T)

survey_data <- add_column(survey_data, flvr_nb = flvr_numbers,
                          .before = "flavour_vanilla")

# compute proportion reporting each number of ice creams
tbl_cont <- survey_data %>% 
  group_by(flvr_nb) %>% 
  summarize(count = n()) %>% 
  mutate(freq = count / sum(count) * 100)

# bar plot - categories
ggplot(tbl_cont, aes(x = factor(flvr_nb), y = freq)) +
  geom_bar(stat = "identity", fill = "dodgerblue3") +
  geom_text(aes(label = round(..y.., 1)), vjust = -0.25) +
  scale_y_continuous(limits = c(0, 35)) + 
  labs(title = "Reported number of ice cream flavours liked",
       x = "Number of flavours", y = "Proportion (%)")

# compute proportion into three categories
tbl_cat <- survey_data %>% 
  mutate(flvr_nb = case_when(flvr_nb == 2 ~ "2 flavours",
                             flvr_nb <= 5 ~ "3 - 5 flavours",
                             flvr_nb >  5 ~ "6 or more flavours")
         ) %>% 
  
  group_by(flvr_nb) %>% 
  summarize(count = n()) %>% 
  mutate(freq = count / sum(count) * 100)

# bar plot - categories
ggplot(tbl_cat, aes(x = factor(flvr_nb), y = freq)) +
 geom_bar(stat = "identity", fill = "dodgerblue3") +
 geom_text(aes(label = round(..y.., 1)), vjust = -0.25) +
 labs(title = "Reported number of ice cream flavours liked (categorized)",
      x = "Number of flavours", y = "Proportion (%)")
