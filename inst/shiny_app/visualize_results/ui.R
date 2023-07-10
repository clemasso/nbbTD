ui <- fluidPage(
  titlePanel("NBB Temporal disaggregation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series", "Select series", choices = d_series_names)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Growth chart', plotOutput("growth_chart")),
        tabPanel('Scaled annual BI ratio', plotOutput("bi_scaled_line")),
        tabPanel('Annual BI ratio', plotOutput("bi_line")),
        tabPanel('Annual BI ratio AF', plotOutput("bi_altfcst_line")),
        tabPanel('Fit', tableOutput("ols_fit"))
      ),
      tabsetPanel(
        tabPanel('TD series', plotOutput("td_series")),
        tabPanel('TD BI ratio', plotOutput("td_bi")),
        tabPanel('Decomposition', plotOutput("decomposition"))
      )
    )
  )
)
