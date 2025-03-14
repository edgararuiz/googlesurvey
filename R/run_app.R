.pkg_env <- environment()
.pkg_env$url <- NULL

#' @export
run_app <- function(url = NULL, sheet = NULL, survey = "how-comfortable") {
  url <- url %||% .pkg_env$url %||% Sys.getenv("SURVEY_GS_URL", unset = NA)
  if (is.na(url)) {
    url <- readline("Please provide a valid Google Sheet URL: ")
  }

  sheet <- sheet %||% Sys.getenv("SURVEY_GS_SHEET", unset = NA)
  if (is.na(sheet)) {
    sheets <- googlesheets4::sheet_names(url)
    res <- menu(sheets)
    sheet <- sheets[res]
    print(paste("Selecting:", sheet))
  }
  survey_path <- system.file(survey, package = "googlesurvey")

  withr::with_envvar(
    c("SURVEY_GS_URL" = url, "SURVEY_GS_SHEET" = sheet),
    shiny::runApp(survey_path)
  )
}
