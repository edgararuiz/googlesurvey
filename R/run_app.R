.pkg_env <- environment()
.pkg_env$url <- NULL
.pkg_env$sheet <- NULL

#' @export
run_app <- function(url = NULL, sheet = NULL, survey = "how-comfortable") {
  url <- url %||% .pkg_env$url %||% Sys.getenv("SURVEY_GS_URL", unset = NA)
  if(is.na(url)) {
    url <- readline("Please provide a valid Google Sheet URL: ")
  }

  sheet <- sheet %||% .pkg_env$sheet %||% Sys.getenv("SURVEY_GS_SHEET", unset = NA)
  if(is.na(sheet)) {
    sheets <- googlesheets4::sheet_names(url)
    res <- menu(sheets)
    sheet <- sheets[res]
    print(paste("Selecting:", sheet))
  }


}
