# LIBRARIES ####

library(readr) # to import tabular data (e.g. csv)
library(dplyr) # to manipulate (tabular) data
library(ggplot2) # to visualize data
library(sf) # to handle spatial vector data
library(terra) # to handle raster data
library(lubridate) # to handle dates and times
library(purrr) # to apply functions
library(zoo) # moving window function
library(tidyr) # tidy data

# DATA IMPORT ####

caro60 <- read_delim("./data/caro60.txt", ",")

caro60

# TASK 1: SEGMENTATION ####

# measure the distance from every point to every other point within temporal window ####
# time window: 6 minutes

# calculate the euclidian distance  ####
caro60 <- caro60 %>% 
  mutate(
    nMinus3 =  sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2),
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)
  )

# calculate mean distance ####

caro60 <- caro60 %>% 
  rowwise() %>% 
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1, nPlus1, nPlus2, nPlus3))
  ) %>% 
  ungroup()


