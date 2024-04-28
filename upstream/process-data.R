# Load packages ============================= 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(highcharter)
library(reactablefmtr)
library(flextable)

# Load data ================================
data <- read.csv("data/2017-18_NBA_salary.csv")

# Data cleaning ======================================
data2 <- data %>% # Create copy with id
  mutate(id = row_number())

## Change column names =====================
names(data2) <- str_to_lower(
  gsub("[.]", "", names(data2))
)

## All character columns - Make all lower case
data2 <- data2 %>% 
  mutate(across(
    where(is.character), 
    ~str_to_lower(.x)
  ))

## All character columns - Remove spaces
data2 <- data2 %>% 
  mutate(across(
    where(is.character), 
    ~gsub(" ", "", .x)
  ))

## Check for duplicates ====================
players <- data2 %>% 
  count(player)
# kayfelder - 3 occurences

kay <- data2 %>% # Detroit, Chicago, Total.. Use combined "tot"
  filter(player == "kayfelder")

data2 <- data2 %>% 
  filter(!id %in% c(225, 226))

## Explore NA values ========
data2 %>% 
  filter(if_any(everything(), ~is.na(.x))) %>% 
  View() # Can replace NA values with 0

data2 <- data2 %>% 
  replace(is.na(.), 0)

colSums(is.na(data2))

# Save cleaned up data
saveRDS(data2, "data/processed-data.RDS")
