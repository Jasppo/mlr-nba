# Load packages ============================= 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(highcharter)
library(reactablefmtr)
library(flextable)
library(scales)
library(purrr)
library(skimr) # For calculating skewness



# Load data and info ================================
data <- readRDS("data/processed-data.RDS")
columnLink <- read.csv("info/columnLink.csv")

# Exploratory data analysis ===================

## Check summary stats ========
data %>% 
  dplyr::select(-player, -id, -nba_country, -tm) %>% 
  pivot_longer(cols = everything(), names_to = "measure", values_to = "value") %>% 
  left_join(columnLink, by = c("measure" = "colShort")) %>% 
  dplyr::select(-measure, measure = colFull, value) %>% 
  continuous_summary(by = "measure", digits = 2)

# Frequencies
plotDensity <- function(myCol) {
  
  dens <- ifelse(myCol %in% c("nba_country", "tm"), F, T)
  
  tName <- filter(columnLink, colShort == myCol) %>% pull(colFull)
  tDat <- data %>% rename(x = !!as.symbol(myCol))
  tType <- ifelse(dens, "area", "column")
  myX <- if (dens) density(tDat$x) else tDat$x
  
  hchart(myX, type = tType, color = "#B71C1C", name = tName)
}

tCols <- names(data)[!names(data) %in% c("player", "id")]

highcharter::hw_grid(lapply(tCols[1:9], plotDensity), ncol = 3)
highcharter::hw_grid(lapply(tCols[10:18], plotDensity), ncol = 3)
highcharter::hw_grid(lapply(tCols[19:27], plotDensity), ncol = 3)

# Observtions:
# Nba Country: Change to USA vs International
# Team: Change total to traded

# Right-Skewed - salary, age, per, true shooting %, freethrow rate, offensive rebound percentage
# defensive rebound %, total rebound %, Assist %, steal %, block %, TO %, usage rate, offensive win shares, defensive win shares, total win shares, 
# total win shares per 48, offfensive box plus or minus, value over replacement player


# Left-Skewed - defensive box plus or minus

# Normal - total box plus or minus

# bimodal - games played, minutes played, 3pt attempt rate, nba draft number

# Unknown - NBA Draft Number


# Correlation matrix before any transformations
tDat <- data %>% 
  dplyr::select(-nba_country, -tm, -player, -id)

hchart(cor(tDat))

# Identify large correlations
cors <- cor(tDat) %>% 
  as.data.frame()

strongCors <- cors %>% 
  mutate(across(everything(), ~ifelse(abs(.x) > 0.75, round(.x, 2), NA))
         )
diag(strongCors) <- NA

strongCors <- strongCors %>% 
  tibble::rownames_to_column(var = "Variable")

strongCors %>% 
  flextable() %>% 
  bg(i = ~ !is.na(1:25), j = 2:26, bg = "green")

myft = bg(myft, i = ~ `2020-03-30`  > 50, 
          j = 2,
          bg="red")

colourer <- col_numeric(
  palette = c("red", "orange", "lightgreen", "darkgreen"),
  domain = c(-1, 1),
  na.color = "white"
)

strongCors %>% 
  flextable() %>%
  theme_box() %>% 
  bg(j = 2:26,
     bg = colourer, part = "body")


# Constant Transformation - Make everything positive from 1-
cTrans <- function(x) {
  xMin <- min(x)
  if (xMin < 1) {
    x - xMin + 1
  } else {
    x
  }
}

# One-hot encoding
data2 <- data %>% 
  mutate(nba_country = ifelse(nba_country == "usa", "usa", "international"), 
         tm = ifelse(tm == "tot", "traded", tm)
         ) %>% 
  pivot_wider(names_from = nba_country, values_from = nba_country) %>% 
  pivot_wider(names_from = tm, values_from = tm, names_prefix = "team_") %>% 
  mutate(across(starts_with("team_"), ~ifelse(!is.na(.x), 1, 0)), 
         across(c("usa", "international"), ~ifelse(!is.na(.x), 1, 0))
  )


# Right-Skewed - salary, age, per, true shooting %, freethrow rate, offensive rebound percentage
# defensive rebound %, total rebound %, Assist %, steal %, block %, TO %, usage rate, offensive win shares, defensive win shares, total win shares, 
# total win shares per 48, offfensive box plus or minus, value over replacement player


# Left-Skewed - defensive box plus or minus

# Normal - total box plus or minus

# bimodal - games played, minutes played, 3pt attempt rate, nba draft number

# Unknown - NBA Draft Number


# Add Constants
data2_trans <- data2 %>% 
  mutate(across(c("salary", "nba_draftnumber", "age", "g", "mp", "per", "ts", "x3par", 
                  "ftr", "orb", "drb", "trb", "ast", "stl", "blk", "tov", "usg", 
                  "ows", "dws", "ws", "ws48", "obpm", "dbpm", "bpm", "vorp"), 
                ~cTrans(.x)))


data2_trans2 <- data2_trans %>% dplyr::select(!contains("team_"), -usa, -international, -player, -id)

# Calculate skewness before transformation
skewness_before <- data2_trans2 %>% 
  skim() %>% 
  dplyr::select(-skim_type, -n_missing, -complete_rate) %>% 
  mutate(type = "before")

# Explore various transformations
transformations <- list(
  minus_one = function(x) x^(-1),
  minus_three4th = function(x) x^(-3/4),
  minus_two3rd = function(x) x^(-2/3),
  minus_sqrt = function(x) x^(-1/2),
  minus_cube_root = function(x) x^(-1/3),
  minus_one4th = function(x) x^(-1/4),
  log = log,
  one4th = function(x) x^(1/4),
  cube_root = function(x) x^(1/3),
  sqrt = sqrt,
  two3rd = function(x) x^(2/3),
  three4th = function(x) x^(3/4),
  one = function(x) x^(1)
  # Add more transformations as needed
)

# Function to apply transformation and calculate skewness
apply_transformation <- function(data, transform_func) {
  data %>%
    mutate(across(where(is.numeric), ~ transformations[[transform_func]](.))) %>%
    skim() %>% 
    dplyr::select(-skim_type, -n_missing, -complete_rate) %>% 
    mutate(type = transform_func)
}

transformations2 <- list(
  salary = function(x) x^(1/4),
  nba_draftnumber = sqrt,
  age = function(x) x^(-1/2),
  g = function(x) x^(1), # Unresolved
  mp = function(x) x^(2/3),
  per = sqrt,
  ts = log,
  x3par = function(x) x^(1/4),
  ftr = function(x) x^(1), # unresolved
  orb= sqrt,
  drb = sqrt,
  trb = function(x) x^(3/4),
  ast = function(x) x^(1/3), 
  stl = function(x) x^(-1),
  blk = function(x) x^(-3/4),
  tov = function(x) x^(-2/3),
  usg = function(x) x^(-1/2),
  ows = function(x) x^(-1/3),
  dws = function(x) x^(-1/3),
  ws = log,
  ws48 = function(x) x^(1/4),
  obpm = function(x) x^(1/3),
  dbpm = function(x) x^(1), # unresolved
  bpm = function(x) x^(3/4),
  vorp = function(x) x^(3/4),
)

# Apply transformations and calculate skewness
skewness_after <- map_df(names(transformations), ~ apply_transformation(data2_trans2, .x))
# salary: one4th
# draftnumber: sqrt
# age: -sqrt
# mp: 2/3
# per: sqrt
# ast: 1/3
# blk: -3/4
# bpm: 3/4
# dbpm: unresolved
# drb: sqrt
# dws: -1/3
# ftr: unresolved
# g: unresolves
# obpm: sqrt
# orb: -1/4
# ows: log
# stl: -1/2
# tov: 1/4
# trb: sqrt
# ts: log
# usg: 3/4
# vorp: -1/3
# ws: -1/3
# ws48: log
# x3par: -3/4





skewness_comparison <- cbind(before = skewness_before, after = skewness_after) %>% 
  relocate(before.numeric.hist, .before = 1) %>% 
  relocate(after.numeric.hist, .before = 2) %>% 
  relocate(before.type, .before = 1) %>% 
  relocate(after.type, .before = 2) %>% 
  arrange(before.skim_variable)

 



# Frequencies - Transformed
plotDensity <- function(myCol) {
  
  dens <- ifelse(myCol %in% c("usa", "international") | grepl("team_", myCol), F, T)
  
  # tName <- filter(columnLink, colShort == myCol) %>% pull(colFull)
  tDat <- data2_trans %>% rename(x = !!as.symbol(myCol))
  tType <- ifelse(dens, "area", "column")
  myX <- if (dens) density(tDat$x) else tDat$x
  
  hchart(myX, type = tType, color = "#B71C1C", name = myCol)
}

tCols <- names(data2_trans)[!names(data2_trans) %in% c("player", "id")]

highcharter::hw_grid(lapply(tCols[1:9], plotDensity), ncol = 3)
highcharter::hw_grid(lapply(tCols[10:18], plotDensity), ncol = 3)
highcharter::hw_grid(lapply(tCols[19:27], plotDensity), ncol = 3)

# Right-Skewed - salary, age, per, true shooting %, freethrow rate, offensive rebound percentage
# defensive rebound %, total rebound %, Assist %, steal %, block %, TO %, usage rate, offensive win shares, defensive win shares, total win shares, 
# total win shares per 48, offfensive box plus or minus, value over replacement player


# Left-Skewed - defensive box plus or minus

# Normal - total box plus or minus

# bimodal - games played, minutes played, 3pt attempt rate, nba draft number

# Unknown - NBA Draft Number

