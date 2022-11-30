library(tidyverse)
library(tidyquant)
library(OECD)
library(zoo)

dataset_list <- get_datasets()
search_dataset("mei", data = dataset_list)

## Compile export values by country (do I need self-exports?)


## Compile price index data by country CPI & PPI

time <- as.yearmon(seq(from=as.Date("1995-01-01"),to=as.Date("2018-12-01"), by="month"))
length(time)*67
