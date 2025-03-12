
library(googlesheets4)
library(tidyverse)

# One time, it should create a .httr-oauth file in the project
# it looks like it's already ignored in by github, but still check
# the .gitignore after running the command for sanity sake
gs4_auth()

simple_split <- function(string, pattern){
  st <- 1
  result <- NULL
  for(letter in 1:nchar(string)){
    current <- substr(string, letter, letter)
    if(current == pattern){
      result <- c(result, substr(string, st, letter - 1))
      st <- letter + 1
    }
  }
  result <- c(result, substr(string, st, nchar(string)))
  result
}

get_responses <- function(survey_url, sheet_name){

  #survey <- googlesheets4::url(survey_url, verbose = FALSE)
  responses <- read_sheet(survey_url, sheet_name, .name_repair = "minimal")

  na_cols <- responses |>
    map_lgl(\(x) all(is.na(x)))

  valid_responses <- responses[, !na_cols]

  valid_responses |>
    pivot_longer(
      cols = !Timestamp,
      names_to = "question",
      values_to = "response",
      values_drop_na = TRUE
    ) |>
    select(-Timestamp) |>
    group_by(question, response) |>
    count(name = "total") |>
    mutate(question = substr(question, 31, nchar(question) - 1))
}


