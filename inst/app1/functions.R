
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
  responses <- read_sheet(survey_url, sheet_name)

  tr <- responses %>%
    select(-Timestamp)

  tidy_responses <- 1:length(tr) %>%
    map_df(~{
      headers <- simple_split(colnames(tr[.x]), "[")
      if(length(headers) == 2){
        section <- trimws(headers[1])
        question <- str_replace_all(headers[2], "\\]", "")
      } else {
        section <- ""
        question <- headers[1]
      }
      tibble(
        section,
        question,
        response = pull(tr, .x)
      )
    }) %>%
    filter(!is.na(response))

  tidy_responses
}


