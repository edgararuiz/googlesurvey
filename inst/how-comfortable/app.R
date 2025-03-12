library(shiny)
library(ggiraph)
library(googlesheets4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

get_responses <- function(survey_url, sheet_name) {

}

ui <- basicPage(
  girafeOutput("distPlot", height = "800px")
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(10000)

  output$distPlot <- renderGirafe({
    autoInvalidate()

    url <- Sys.getenv("SURVEY_GS_URL", unset = NA)
    sheet <- Sys.getenv("SURVEY_GS_SHEET", unset = NA)

    if(is.na(url) | is.na(sheet)) {
      stop("No URL or sheet provided")
    }

    responses <- read_sheet(
      ss = url,
      sheet = sheet,
      .name_repair = "minimal"
      )
    na_cols <- map_lgl(responses, \(x) all(is.na(x)))
    valid_responses <- responses[, !na_cols]
    results <- valid_responses |>
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

    p1 <- results |>
      ggplot() +
      geom_bar_interactive(
        aes(
          x = total,
          y = response,
          tooltip = total,
          fill = response
        ),
        stat = "identity"
      ) +
      facet_wrap(vars(question), ncol = 1, strip.position = "right") +
      labs(x = "", y = "") +
      theme(
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        strip.text.y = element_text(angle = 0, hjust = 0),
        strip.text = element_text(size = 7),
        strip.background = element_rect("white"),
        panel.grid.major.x = element_line("#ddd", linewidth = 0.1),
        panel.background = element_rect("white")
      )

    x <- girafe(ggobj = p1)
    girafe_options(
      x = x,
      opts_tooltip(css = "font-family: monospace; background-color: lightblue; padding: 5px;")
      )
  })
}

shinyApp(ui, server)
