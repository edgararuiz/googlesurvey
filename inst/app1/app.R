library(shiny)

source("functions.R")

ui <- basicPage(
  plotOutput("distPlot", height = "800px")
)

server <- function(input, output) {

  autoInvalidate <- reactiveTimer(10000)

  output$distPlot <- renderPlot({

    autoInvalidate()

    results <- get_responses(
      "https://docs.google.com/spreadsheets/d/1YPFPC9GqMqt5EUWgWGe2aXsfbB50ql_wkQDlgxbR3xw/edit?gid=1663697578#gid=1663697578",
      "Form Responses 2"
    )

    results %>%
      ggplot() +
      geom_col(aes(x = response, y = total, fill = response)) +
      coord_flip() +
      facet_grid(question ~ .) +
      labs(x = "", y  = "")


  })
}

shinyApp(ui, server)
