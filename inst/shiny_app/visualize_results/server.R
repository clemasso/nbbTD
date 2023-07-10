server <- function(input, output){
  output$growth_chart <-  renderPlot({
    plot_growth_chart(d_benchmarks, d_indicators, series_name = input$series, call.conversion = d_conversion)
  })
  output$bi_scaled_line <- renderPlot({
    plot_annual_bi(d_bi_annual, d_bi_annual_f, series_name = input$series, scaled = TRUE, main = "Annual BI ratio scaled")
  })
  output$bi_line <- renderPlot({
    plot_annual_bi(d_bi_annual, d_bi_annual_f, series_name = input$series, main = "Annual BI ratio")
  })
  output$bi_altfcst_line <- renderPlot({
    plot_annual_bi(d_bi_annual, d_bi_annual_falt, series_name = input$series, main = "Annual BI ratio with alternative forecast")
  })
  output$ols_fit <- renderTable({d_fit[input$series,]}, digits = 3)
  output$td_series <- renderPlot({
    plot_td_series(d_td_series, d_td_series_stderr, series_name = input$series)
  })
  output$td_bi <- renderPlot({
    plot_td_bi(d_td_bi, d_td_bi_stderr, series_name = input$series)
  })
  output$decomposition <- renderPlot({
    plot_decomposition_td_series(d_td_series, d_decomposition, series_name = input$series)
  })
}
