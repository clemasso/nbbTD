server <- function(input, output){
  output$growth_chart <-  renderPlot({
    plot_growth_chart(d_benchmarks, d_indicators, series_name = input$series, call.conversion = d_call["conversion"])
  })
  output$bi_scaled_line <- renderPlot({
    PlotAnnualBIratio(d_bi_annual, d_bi_annual_f, series_name = input$series, scaled = TRUE, main = "Annual BI ratio scaled")
  })
  output$bi_line <- renderPlot({
    PlotAnnualBIratio(d_bi_annual, d_bi_annual_f, series_name = input$series, main = "Annual BI ratio")
  })
  output$bi_altfcst_line <- renderPlot({
    PlotAnnualBIratio(d_bi_annual, d_bi_annual_falt, series_name = input$series, main = "Annual BI ratio with alternative forecast")
  })
  output$td_series <- renderPlot({
    plot_td_series(d_td_series, series_name = input$series)
  })
  output$bi_infra <- renderPlot({
    if(input$series %in% colnames(d_edenton$td.series)){
      plot_bi_infra(d_bi_infra, series_name = input$series, bi_infra_f = d_edenton$bi.infra.f)
    } else if (input$series %in% colnames(d_denton$td.series)){
      plot_bi_infra(d_bi_infra, series_name = input$series, bi_infra_f = d_denton$bi.infra.f)
    } else{
      plot_bi_infra(d_bi_infra, series_name = input$series, bi_infra_f = NULL)
    }
  })
  output$denton_ci <- renderPlot({
    if(input$series %in% colnames(d_edenton$td.series)){
      plot_td_series_ci(d_edenton, series_name = input$series)
    } else if (input$series %in% colnames(d_denton$td.series)){
      plot_td_series_ci(d_denton, series_name = input$series)
    }
  })
  output$denton_bi_analysis <- renderPlot({
    if(input$series %in% colnames(d_edenton$td.series)){
      plot_bi_compare_manual(d_edenton, series_name = input$series)
    } else if (input$series %in% colnames(d_denton$td.series)){
      plot_bi_compare_manual(d_denton, series_name = input$series)
    }
  })
  output$denton_td_analysis <- renderPlot({
    if(input$series %in% colnames(d_edenton$td.series)){
      plot_td_compare_manual(d_edenton, series_name = input$series, d_indicators)
    } else if (input$series %in% colnames(d_denton$td.series)){
      plot_td_compare_manual(d_denton, series_name = input$series, d_indicators)
    }
  })
  output$chowlin_analysis <- renderPlot({
    if(input$series %in% colnames(d_clvar$td.series)){
      plot_td_series_decomp(d_clvar, series_name = input$series)
    }
  })
}


