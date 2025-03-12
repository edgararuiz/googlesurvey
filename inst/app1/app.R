library(shiny)
library(ggiraph)

source("functions.R")

ui <- basicPage(
  girafeOutput("distPlot", height = "800px")
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(10000)

  output$distPlot <- renderGirafe({
    autoInvalidate()

    results <- get_responses(
      "https://docs.google.com/spreadsheets/d/1YPFPC9GqMqt5EUWgWGe2aXsfbB50ql_wkQDlgxbR3xw/edit?gid=1663697578#gid=1663697578",
      "Form Responses 2"
    )




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
      #theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(size = 6),
        #plot.background = element_rect(fill = "#ddd"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line("#ddd"),
        panel.background = element_rect("white", colour = "#ddd")
      )

    girafe(
      ggobj = p1,
      options = list(opts_sizing(rescale = TRUE))
    )
  })
}

shinyApp(ui, server)
