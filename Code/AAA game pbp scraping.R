library(baseballr)
library(tidyverse)
library(progress)
library(lubridate)
library(arrow)




# nl_teams <- mlb_teams(season = 2024, league_ids = 104)
# al_teams <- mlb_teams(2024, league_ids = 104)

#### 2024 AAA PBP ####
AAA_pks <- function(date){
  date <- as.character(date)
  df <- get_game_pks_mlb(date, level_ids = 11);if (is.data.frame(df) == FALSE){
  return(NULL)
} else{
  return(df)
}
}

AAA_pks_df <- map_df(seq.Date(as.Date('2024-04-01'), as.Date('2024-09-30'), by = 'day'), AAA_pks, .progress = TRUE)


AAA_pbp <- function(game_pk){
  mlb_pbp(game_pk)
}

AAA_pbp <- map_df(AAA_pks_df$game_pk, ~tryCatch({
  # Try to scrape data for each game_pk
  mlb_pbp(.x)
}, error = function(e) {
  # Print the game_pk that caused the error and skip it
  cat("No PBP found for:", .x, "\n")
  return(NULL)  # Return NULL to skip this entry
}), .progress = TRUE)


#### 2023 AAA PBP ####
AAA_pks_2023 <- function(date){
  date <- as.character(date)
  df <- get_game_pks_mlb(date, level_ids = 11);if (is.data.frame(df) == FALSE){
    return(NULL)
  } else{
    return(df)
  }
}

AAA_pks_2023_df <- map_df(seq.Date(as.Date('2023-04-01'), as.Date('2023-09-30'), by = 'day'), AAA_pks_2023, .progress = TRUE)


AAA_pbp_2023 <- map_df(AAA_pks_2023_df$game_pk, ~tryCatch({
  # Try to scrape data for each game_pk
  mlb_pbp(.x)
}, error = function(e) {
  # Print the game_pk that caused the error and skip it
  cat("No PBP found for:", .x, "\n")
  return(NULL)  # Return NULL to skip this entry
}), .progress = TRUE)


#### 2022 AAA PBP ####
AAA_pks_2022 <- function(date){
  date <- as.character(date)
  df <- get_game_pks_mlb(date, level_ids = 11);if (is.data.frame(df) == FALSE){
    return(NULL)
  } else{
    return(df)
  }
}

AAA_pks_2022_df <- map_df(seq.Date(as.Date('2022-04-01'), as.Date('2022-09-30'), by = 'day'), AAA_pks_2022, .progress = TRUE)


AAA_pbp_2022 <- map_df(AAA_pks_2022_df$game_pk, ~tryCatch({
  # Try to scrape data for each game_pk
  mlb_pbp(.x)
}, error = function(e) {
  # Print the game_pk that caused the error and skip it
  cat("No PBP found for:", .x, "\n")
  return(NULL)  # Return NULL to skip this entry
}), .progress = TRUE)




#### 2021 AAA PBP ####
AAA_pks_2021 <- function(date){
  date <- as.character(date)
  df <- get_game_pks_mlb(date, level_ids = 11);if (is.data.frame(df) == FALSE){
    return(NULL)
  } else{
    return(df)
  }
}

AAA_pks_2021_df <- map_df(seq.Date(as.Date('2021-04-01'), as.Date('2021-09-30'), by = 'day'), AAA_pks_2021, .progress = TRUE)


AAA_pbp_2021 <- map_df(AAA_pks_2021_df$game_pk, ~tryCatch({
  # Try to scrape data for each game_pk
  mlb_pbp(.x)
}, error = function(e) {
  # Print the game_pk that caused the error and skip it
  cat("No PBP found for:", .x, "\n")
  return(NULL)  # Return NULL to skip this entry
}), .progress = TRUE)
