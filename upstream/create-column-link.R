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

## Change column names =====================
data2 <- data

names(data2) <- str_to_lower(
  gsub("[.]", "", names(data2))
)

# Columns Full Names 
dataCols <- c(
  "player" = "Player",
  "salary" = "Salary",
  "nba_country" = "Origin Country",
  "nba_draftnumber" = "Draft Number",
  "age" = "Age",
  "tm" = "Team",
  "g" = "Ganes Played",
  "mp" = "Minutes Played",
  "per" = "Player Efficiency Rating",
  "ts" = "True Sgooting Percentage",
  "x3par" = "3-Point Attempt Rate",
  "ftr" = "Free Throw Rate",
  "orb" = "Offensive Rebound Percentage",
  "drb" = "Defensive Rebound Percentage",
  "trb" = "Total Rebound Percentage",
  "ast" = "Assist Percentage",
  "stl" = "Steal Percentage",
  "blk" = "Block Percentage",
  "tov" = "Turnover Percentage",
  "usg" = "Usage Rate",
  "ows" = "Offensive Win Shares",
  "dws" = "Defensive Win Shares",
  "ws" = "Total Win Shares",
  "ws48" = "Total Win Shares per 48 min",
  "obpm" = "Offensive Box Plus or Minus",
  "dbpm" = "Defensive Box Plus or Minus",
  "bpm" = "Total Box Plus or Minus",
  "vorp" = "Value over Replacement Player"
)

columnLink <- tibble(colShort = names(dataCols), 
                     colFull = unname(dataCols))

write.csv(columnLink, file = "info/columnLink.csv", row.names = F)
