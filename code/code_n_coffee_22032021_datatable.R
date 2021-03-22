
library(data.table)
library(tidyverse)
library(bench)
library(nycflights13)
suppressMessages(library(dplyr))

setwd("Documents/McGill Docs/Fifth Year/Thesis/Workshops/Code and Coffee")

nrow(flights)
names(flights)



# things need to be in a data.table object
dt_flights <- as.data.table(flights)

names(dt_flights)



# syntax for data.table follows a very simple format:

# dt[i, j, by]


# some simple data tidying

dt_flights[month == 3, ] # only month 3
dt_flights[, new_col := hour + minute]

# can use the pipe
dt_flights[, new_col := hour + minute] %>% 
    .[month == 3,]

dt_flights[, .(mean_delay = mean(dep_delay, na.rm = T)), by = carrier]


dt_flights[month == 3, .(mean_delay = mean(dep_delay, na.rm = T)), by = carrier]

# the equivalent in tidyverse would be
flights %>% 
    filter(month == 3) %>% 
    group_by(carrier) %>% 
    summarize(mean_delay = mean(dep_delay, na.rm = T))

benchmark <- microbenchmark::microbenchmark(
    datatable = dt_flights[month == 3, .(mean_delay = mean(dep_delay, na.rm = T)), by = carrier],
    dplyr = flights %>% 
        filter(month == 3) %>% 
        group_by(carrier) %>% 
        summarize(mean_delay = mean(dep_delay, na.rm = T))
)
autoplot(benchmark)




# making our own data.table -----------------------------------------------

# exactly the same as making a data.frame
size = 1e5
dt1 <- data.table(x1 = sample(1:size), 
                 x2 = rnorm(size, 100, 12),
                 x3 = runif(size, min = 0, max = 22),
                 x4 = rbinom(size, 1, 0.4)
                 )

dt2 <- data.table(x1 = sample(1:size),
                  x5 = rnorm(size, 45, 4), 
                  x6 = rbinom(size, 1, 0.8),
                  x7 = runif(size, min = 100, max = 1000))


# an introduction to joining
# joining is super easy. for a full join we can:
x  = dt1[dt2, on = "x1"]


# turn into a tibble
df1 <- as_tibble(dt1)
df2 <- as_tibble(dt2)
full_join(df1, df2, by = "x1")

all_equal(dt1[dt2, on = "x1"], full_join(df1, df2, by = "x1"))


# now we can compare the speed at which they join
benchmark <- microbenchmark::microbenchmark(
    data.table = dt1[dt2, on = "x1"],
    dplyr = full_join(df1, df2, by = "x1"),
    times = 50
    )
autoplot(benchmark)


# reading and writing -- this is the best bit

fread("Data/data.csv")
read_csv("Data/data.csv")
read.csv("Data/data.csv")

load_data <- microbenchmark::microbenchmark(
    readr      = read_csv("Data/data.csv", col_types = cols(), progress = F),
    base_R     = read.csv("Data/data.csv"),
    data.table = fread("Data/data.csv"),
    times = 10
)

autoplot(load_data)
