# Presenter: Chinchin Wang
#### WORKING WITH LONGITUDINAL DATA - RLE FUNCTION

# load tidyverse package
library(tidyverse)

### EXAMPLE 1 (SIMPLE EXAMPLE)

# create binary vector
binary_vector <- sample(c(0,1), size=100, replace = T)

# apply rle function
binary_vector %>% rle()

# show class - RLE
binary_vector %>% rle() %>% class()

# apply rle function then unclass
binary_vector %>% rle() %>% unclass()

# apply rle function then unclass and change into dataframe
binary_vector %>% rle() %>% unclass() %>% as.data.frame()


### EXAMPLE 2

# load beavers data (built in R dataset; body temperature of 2 beavers)
data("beavers")

# view data
head(beaver1)

# round temperature to one decimal place
beaver1_round <- beaver1 %>% 
  mutate(temp = round(temp, 1))

# view data
head(beaver1_round)

# apply rle function to temperature
beaver_rle <- rle(beaver1_round$temp) %>% unclass() %>% as.data.frame()

# view output
beaver_rle

## let's say we only want to keep the entries in the original dataframe where the temperature changes
# create index column based on lengths
beaver_rle <- beaver_rle %>% mutate(index = cumsum(lengths)-lengths+1)

# store these indices in an object
beaver_indices <- beaver_rle %>% select(index) %>% pull()

# only keep these indices in original dataset
beaver1_round[beaver_indices,]

### EXAMPLE 3 (CENSORING RUNS OF MISSING DATA)
# create id variables for beaver1 and beaver2 datasets
beaver1 <- beaver1 %>% mutate(beaver = 1)
beaver2 <- beaver2 %>% mutate(beaver = 2)

# combine beaver1 and beaver2 datasets
beaver <- rbind(beaver1, beaver2)

## create missing data
# set seed
set.seed(1)

# randomly sample indices from 1 to 214
missing_indices <- runif(100, min=1, max=214) %>% round()

# make missing temp data
beaver[missing_indices, 3] <- NA

# RLE function treats NAs as different values
rle(beaver$temp) %>% unclass() %>% as.data.frame()

# get around this by creating a character value for NA
beaver <- beaver %>% mutate(temp = ifelse(is.na(temp), "NA", temp))

# run RLE
beaver_missing_rle <- rle(beaver$temp) %>% unclass() %>% as.data.frame()

# add indices for first entry of run and last entry
beaver_missing_rle <- beaver_missing_rle %>% mutate(first_index = cumsum(lengths)-lengths+1,
                              last_index = lead(first_index)-1)

# filter to entries with 2+ missing entries in a row
beaver_missing_rle_indices <- beaver_missing_rle %>% filter(first_index != last_index & values == "NA")

# add a censored column to the beaver dataframe
beaver$censored <- F

## change censored to TRUE where there are 2+ missing entries in a row
# create a function that takes the first and last indices as arguments
beaver_censor <- function(first_index, last_index){
  beaver[first_index:last_index, "censored"] <<- T
  return(beaver)
}

# apply function to indices
mapply(beaver_censor, beaver_missing_rle_indices$first_index, beaver_missing_rle_indices$last_index)

# look at dataframe
beaver

### EXAMPLE 4 (RUNS WITH MULTIPLE VARIABLES)
# combine beaver id and temp into one variable
beaver <- beaver %>% mutate(beaver_temp = paste(beaver, temp, sep = " "))

# run RLE on new variable
beaver_temp_rle <- rle(beaver$beaver_temp) %>% unclass() %>% as.data.frame()

# separate values into separate beaver and temp variables
beaver_temp_rle <- beaver_temp_rle %>% separate(values, into=c("beaver", "temp"), sep = " ")

# show rle output
beaver_temp_rle
