# UI
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
        tabPanel('Annual BI ratio AF', plotOutput("bi_altfcst_line"))
      ),
      tabsetPanel(
        tabPanel('TD series', plotOutput("td_series")),
        tabPanel('Infra BI ratio', plotOutput("bi_infra")),
        tabPanel('(e)Denton CI', plotOutput("denton_ci")),
        tabPanel('(e)Denton BI analysis', plotOutput("denton_bi_analysis")),
        tabPanel('(e)Denton TD analysis', plotOutput("denton_td_analysis")),
        tabPanel('Chow-Lin analysis', plotOutput("chowlin_analysis"))
      )
    )
  )
)
