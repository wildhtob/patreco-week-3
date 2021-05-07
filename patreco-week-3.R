# LIBRARIES ####

library(readr) # to import tabular data (e.g. csv)
library(dplyr) # to manipulate (tabular) data
library(ggplot2) # to visualize data
library(lubridate) # to handle dates and times
library(purrr) # to apply functions
library(zoo) # moving window function
library(tidyr) # tidy data
library(SimilarityMeasures) # similarity measures

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

# TASK 2: SPECIFY AND APPLY THRESHOLD D ####

# plot unfiltered trajectories ####
caro60 %>% 
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# visual exploration
summary(caro60$stepMean) # mean at ~7, median at ~4

caro60 %>% 
  ggplot(aes(stepMean)) +
  geom_histogram(binwidth = 1)

caro60 %>% 
  ggplot(aes(stepMean)) +
  geom_boxplot()


# remove static points ####

caro60 <- caro60 %>% 
  ungroup() %>% 
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE)) # threshold defined as mean

caro_filter <- caro60 %>% 
  filter(!static)

# plot filtered trajectories ####
caro_filter %>% 
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

## TASK 3: VISUALIZE SEGMENTED TRAJECTORIES ####

caro60 %>% 
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point(aes(color = static)) +
  coord_fixed() +
  theme(legend.position = "right")

# TASK 4: SEGMENT-BASED ANALYSIS ####

# define function to assign unique IDs ####
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

# apply function to data, based on static ####

caro60 <- caro60 %>% 
  mutate(segment_id = rle_id(static))

# plot moving segments coloured by segment ID ####
caro60 %>%
  filter(!static) %>% 
  ggplot(aes(E, N, color = segment_id)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "right")

# add count column per segment ####
caro60 <- caro60 %>% 
  group_by(segment_id) %>%
  mutate(n = n()) 

# plot moving segments >5 min ####
caro60 %>% 
  filter(!static & n >5) %>% 
  ggplot(aes(E, N, color = segment_id)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "right")

# TASK 5: SIMILARITY MEASURES ####

ped <- read_delim("data/pedestrian.txt", ",")

ped$TrajID <- ped$TrajID %>% as.factor # define TrajID as a factor

ped

# data exploration ####
ped %>% 
  ggplot(aes(E, N)) +
  geom_point(dplyr::select(ped, -TrajID), alpha = 0.05) + # plot all trajectories in the background
  geom_path(aes(E, N), alpha = 0.2) +
  geom_point(aes(color = TrajID)) +
  facet_wrap(~ TrajID, nrow = 2, labeller = label_both) +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "none")

